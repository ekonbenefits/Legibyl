namespace Legibyl.Validation

open System
open System.IO
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open org.verapdf.core
open org.verapdf.gf.foundry
open org.verapdf.gf.model
open org.verapdf.pdfa
open org.verapdf.pdfa.flavours
open org.verapdf.pdfa.results
open org.verapdf.pdfa.validation.profiles
open Legibyl.Validation.Interop

module internal VeraPdf =
    let private initLock = obj ()
    let mutable private initialised = false

    /// Each profile's rules' descriptions, by specification, clause and test (read once).
    let ruleDescriptions = Collections.Concurrent.ConcurrentDictionary<PdfProfile, Map<string * string * int, string>>()

    let ensureInitialised () =
        if not initialised then
            lock initLock (fun () ->
                if not initialised then
                    // Before veraPDF logs anything: what it logs is kept, not printed.
                    ValidatorLog.install ()
                    // WCAG's contrast check draws pages with PDFium (see java/README.md).
                    legibyl.wcag.PageRasterizers.set ContrastRasterizer.rasterizer
                    // Nothing draws through Java's AWT now (its BufferedImage holds PDFium's
                    // pixels), but its native graphics crash the process under IKVM unless it's
                    // headless, so it stays so.
                    java.lang.System.setProperty("java.awt.headless", "true") |> ignore
                    VeraGreenfieldFoundryProvider.initialise ()
                    // Each validation profile is shared process-wide and builds its rule lookup
                    // tables on first use, without locking: a rebuild clears the table while
                    // another thread may be reading it, which surfaces as a NullPointerException
                    // ("FixError") in the metadata fixer. Build them once here, on one thread.
                    for profile in javaSeq (Profiles.getVeraProfileDirectory().getValidationProfiles ()) do
                        let profile = profile :?> ValidationProfile
                        match Seq.tryHead (javaSeq (profile.getRules ())) with
                        | Some rule -> profile.getRuleByRuleId((rule :?> Rule).getRuleId ()) |> ignore
                        | None -> ()
                    initialised <- true)

    let ensureExists (pdfPath: string) =
        if not (File.Exists pdfPath) then
            raise (VeraPdfValidationException $"File not found: {pdfPath}")

    /// Runs f, rethrowing veraPDF/Java failures as VeraPdfValidationException.
    let wrapErrors (failedTo: string) f =
        try
            f ()
        with
        | :? VeraPDFException as ex -> raise (VeraPdfValidationException($"{failedTo}: {ex.Message}", ex))
        | :? java.lang.Exception as ex -> raise (VeraPdfValidationException($"{failedTo}: {ex.getMessage ()}", ex))

    /// veraPDF's WCAG 2.2 machine checks, as its v1.28.1 release has them (Profiles/
    /// WCAG-2-2-Machine.xml), less its copy of PDF/UA-1's rules: PDF/UA is checked on its own, by
    /// the standard the file's PDF version calls for. What's left (contrast, text size, repeated
    /// characters, page orientation, figures' boxes, links' descriptions, redundant tags) checks
    /// PDF 1.x and PDF 2.0 files alike.
    let wcagProfile: Lazy<ValidationProfile> =
        lazy
            (let assembly = typeof<PdfProfile>.Assembly
             use stream = assembly.GetManifestResourceStream "WCAG-2-2-Machine.xml" |> nonNull
             let document = Xml.Linq.XDocument.Load stream
             let ns = document.Root |> nonNull |> _.Name.Namespace
             for rule in List.ofSeq (document.Descendants(ns + "rule")) do
                 let specification =
                     match rule.Element(ns + "id") with
                     | NonNull id ->
                         match id.Attribute(Xml.Linq.XName.Get "specification") with
                         | NonNull attribute -> attribute.Value
                         | Null -> ""
                     | Null -> ""
                 if specification <> "WCAG_2_2" then rule.Remove()
             use bytes = new java.io.ByteArrayInputStream(Text.Encoding.UTF8.GetBytes(document.ToString()))
             let profile = Profiles.profileFromXml bytes
             // Its rule lookup tables built now, on one thread (see ensureInitialised).
             match Seq.tryHead (javaSeq (profile.getRules ())) with
             | Some rule -> profile.getRuleByRuleId((rule :?> Rule).getRuleId ()) |> ignore
             | None -> ()
             profile)

    /// WCAG 2.2's rules veraPDF tags "cosmetic" (characters repeated to lay text out, 1.3.1): a
    /// guess at a problem, found often in PDFs that are fine (a contents page's dot leaders, a
    /// form's underscores), and without a place on the page. Their failures are warnings.
    let wcagCosmetic: Lazy<Set<string * int>> =
        lazy
            (set [ for item in javaSeq (wcagProfile.Value.getRules ()) do
                       let rule = item :?> Rule
                       // Its tags, as the profile has them: "cosmetic,machine,text".
                       let tags = match rule.getTags () with NonNull tags -> tags.Split ',' | Null -> [||]
                       if tags |> Array.exists (fun tag -> tag.Trim() = "cosmetic") then
                           rule.getRuleId().getClause (), rule.getRuleId().getTestNumber () ])

    /// Legibyl's name for a specification veraPDF names, for a rule of clause: WCAG 2.2's
    /// "WCAG2.2" is "WCAG 2.2" and the criterion's level ("WCAG 2.2 AA"; see WcagSpecification);
    /// PDF/UA's are as veraPDF gives them.
    let specificationName (specification: string) (clause: string) =
        if specification.StartsWith("WCAG", StringComparison.Ordinal) then WcagSpecification.ofCriterion clause else specification

    let toJavaFlavour profile =
        match profile with
        | PdfProfile.PdfUa1 -> PDFAFlavour.PDFUA_1
        | PdfProfile.PdfUa2 -> PDFAFlavour.PDFUA_2
        | _ -> raise (ArgumentOutOfRangeException(nameof profile, profile, "Unsupported PDF profile"))

    /// What a rule of profile's says, by its specification, clause and test; None for a rule it
    /// doesn't have.
    let ruleDescription (profile: PdfProfile) (specification: string) (clause: string) (testNumber: int) =
        ensureInitialised ()
        if WcagSpecification.isWcag specification then
            javaSeq (wcagProfile.Value.getRules ())
            |> Seq.map (fun item -> item :?> Rule)
            |> Seq.tryFind (fun rule -> rule.getRuleId().getClause () = clause && rule.getRuleId().getTestNumber () = testNumber)
            |> Option.map _.getDescription()
        else
        let table =
            ruleDescriptions.GetOrAdd(
                profile,
                fun profile ->
                    let validationProfile = Profiles.getVeraProfileDirectory().getValidationProfileByFlavour (toJavaFlavour profile)
                    [ for item in javaSeq (validationProfile.getRules ()) do
                          let rule = item :?> Rule
                          let id = rule.getRuleId ()
                          yield (id.getSpecification().ToString(), id.getClause (), id.getTestNumber ()), rule.getDescription () ]
                    |> Map.ofList
            )
        table |> Map.tryFind (specification, clause, testNumber)

    let private repairStatuses =
        [ MetadataFixerResult.RepairStatus.SUCCESS, RepairStatus.Success
          MetadataFixerResult.RepairStatus.FIX_ERROR, RepairStatus.FixError
          MetadataFixerResult.RepairStatus.WONT_FIX, RepairStatus.WontFix
          MetadataFixerResult.RepairStatus.NO_ACTION, RepairStatus.NoAction
          MetadataFixerResult.RepairStatus.ID_REMOVED, RepairStatus.IdRemoved ]

    let toRepairStatus (status: MetadataFixerResult.RepairStatus) =
        match repairStatuses |> List.tryFind (fun (javaStatus, _) -> javaStatus = status) with
        | Some(_, repairStatus) -> repairStatus
        | None -> raise (ArgumentOutOfRangeException(nameof status, string status, "Unknown veraPDF repair status"))

    /// What fixes each rule's failure, by standard and rule ("clause-test", as veraPDF numbers
    /// them). PDF/UA-1 and PDF/UA-2 number the same requirements differently. Anything else has
    /// no fix here (though withLegibylAnalysis adds some, from what's in the file).
    let private fixesByRule: Map<PdfProfile * string, PdfFixOptions> =
        let ua1 rule options = (PdfProfile.PdfUa1, rule), options
        let ua2 rule options = (PdfProfile.PdfUa2, rule), options
        let metadataAndTitle = PdfFixOptions.Metadata ||| PdfFixOptions.Title
        Map.ofList
            [ // The PDF/UA identifier in the metadata (pdfuaid:part, and PDF/UA-1's amd and
              // corr or PDF/UA-2's rev).
              ua1 "5-1" PdfFixOptions.Metadata
              ua1 "5-3" PdfFixOptions.Metadata
              ua1 "5-4" PdfFixOptions.Metadata
              ua1 "5-5" PdfFixOptions.Metadata
              ua2 "5-1" PdfFixOptions.Metadata
              ua2 "5-2" PdfFixOptions.Metadata
              ua2 "5-3" PdfFixOptions.Metadata
              ua2 "5-4" PdfFixOptions.Metadata
              ua2 "5-5" PdfFixOptions.Metadata
              // No metadata stream; no dc:title in it; DisplayDocTitle not set.
              ua1 "7.1-8" PdfFixOptions.Metadata
              ua1 "7.1-9" metadataAndTitle
              ua1 "7.1-10" PdfFixOptions.Title
              ua2 "8.11.1-2" PdfFixOptions.Metadata
              ua2 "8.11.1-1" metadataAndTitle
              ua2 "8.11.2-1" PdfFixOptions.Title
              // The metadata's language (PDF/UA-2 has no such rule).
              ua1 "7.2-33" PdfFixOptions.Metadata
              // Annotations' descriptions (PDF/UA-2 doesn't require one on links).
              ua1 "7.18.1-2" PdfFixOptions.LinkContents
              ua1 "7.18.5-2" PdfFixOptions.LinkContents
              // Tab order on pages with annotations.
              ua1 "7.18.3-1" PdfFixOptions.Tabs
              ua2 "8.9.3.3-1" PdfFixOptions.Tabs
              // Fonts not embedded.
              ua1 "7.21.4.1-1" PdfFixOptions.EmbedFonts
              ua2 "8.4.5.5.1-1" PdfFixOptions.EmbedFonts
              // CIDToGIDMap on embedded Type 2 CIDFonts.
              ua1 "7.21.3.2-1" PdfFixOptions.FontGlyphs
              ua2 "8.4.5.3.2-1" PdfFixOptions.FontGlyphs
              ua1 "7.21.4.2-1" PdfFixOptions.FontDictionaries
              ua1 "7.21.4.2-2" PdfFixOptions.FontDictionaries
              ua1 "7.21.3.1-1" PdfFixOptions.FontDictionaries
              ua2 "8.4.5.3.1-1" PdfFixOptions.FontDictionaries
              ua1 "7.21.3.3-2" PdfFixOptions.FontDictionaries
              ua2 "8.4.5.4-2" PdfFixOptions.FontDictionaries
              ua1 "7.21.7-1" PdfFixOptions.FontUnicode
              ua1 "7.21.7-2" PdfFixOptions.FontUnicode
              ua2 "8.4.5.8-1" PdfFixOptions.FontUnicode
              ua2 "8.4.5.8-2" PdfFixOptions.FontUnicode
              // The role map: names not mapped, circles, standard tags mapped.
              ua1 "7.1-5" PdfFixOptions.RoleMap
              ua1 "7.1-6" PdfFixOptions.RoleMap
              ua1 "7.1-7" PdfFixOptions.RoleMap
              ua2 "8.2.4-1" PdfFixOptions.RoleMap
              ua2 "8.2.4-2" PdfFixOptions.RoleMap
              ua2 "8.2.4-3" PdfFixOptions.RoleMap
              ua2 "8.2.4-4" PdfFixOptions.RoleMap
              // A single Document tag at the top of the structure, in PDF 2.0's namespace.
              ua2 "8.2.5.2-1" PdfFixOptions.DocumentRoot
              ua2 "8.2.5.2-2" PdfFixOptions.DocumentRoot
              ua1 "7.5-1" PdfFixOptions.TableHeaderScope
              ua2 "8.2.5.26-5" PdfFixOptions.TableHeaderScope ]

    /// The rules for a missing metadata stream, and (by standard) the clause and test of the rule
    /// for its dc:title, which veraPDF can only check once there's metadata.
    let private noMetadataRules = set [ PdfProfile.PdfUa1, "7.1-8"; PdfProfile.PdfUa2, "8.11.1-2" ]

    let private dcTitleRule standard = if standard = PdfProfile.PdfUa2 then "8.11.1", 1 else "7.1", 9

    let private isDcTitleRule (assertion: RuleViolation) =
        match PdfUaTarget.ofSpecification assertion.Specification with
        | Some standard -> dcTitleRule standard = (assertion.Clause, assertion.TestNumber)
        | None -> false

    /// What fixes a WCAG 2.2 rule's failure, by rule: a link with no description (or one that's
    /// its address), described from its text or its page's title. Its redundant paragraphs and
    /// contents entries aren't offered the fixes for empty ones: veraPDF calls a paragraph whose
    /// text couldn't be read empty, which Legibyl's own checks don't (they offer those fixes
    /// where they apply). Its others (contrast, text size…) have no fix: they're for where the PDF
    /// was made.
    let private wcagFixes: Map<string, PdfFixOptions> =
        Map.ofList [ "2.4.9-1", PdfFixOptions.LinkContents ||| PdfFixOptions.LinkTitlesFromWeb ]

    let private determineFixOptions (assertion: RuleViolation) =
        match PdfUaTarget.ofSpecification assertion.Specification with
        | None when WcagSpecification.isWcag assertion.Specification ->
            wcagFixes |> Map.tryFind assertion.RuleId |> Option.defaultValue PdfFixOptions.None
        | Some standard -> fixesByRule |> Map.tryFind (standard, assertion.RuleId) |> Option.defaultValue PdfFixOptions.None
        // ISO 32005's rows for what the structure tree root may hold ("Table 5. StructTreeRoot-Part"):
        // a single Document at the top is what they ask for.
        | None when
            assertion.Specification.StartsWith("ISO 32005", StringComparison.Ordinal)
            && assertion.Clause.StartsWith("Table 5. StructTreeRoot-", StringComparison.Ordinal)
            ->
            PdfFixOptions.DocumentRoot
        | None -> PdfFixOptions.None

    /// Legibyl's own read of the file (iText and PdfPig): its structure-tree warnings, plus
    /// the open document and context needed to place veraPDF's findings on the page.
    type LegibylAnalysis =
        | Analysed of document: iText.Kernel.Pdf.PdfDocument * context: StructureTreeObjects.Context * warnings: RuleViolation list
        | CouldNotInspect of warning: RuleViolation

    /// Starts Legibyl's analysis on a background thread. It doesn't need veraPDF's results,
    /// so Validate runs it alongside veraPDF's validation rather than after it.
    let startLegibylAnalysis (pdfPath: string) : Task<LegibylAnalysis> =
        Task.Run(fun () ->
            try
                let document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfReader(pdfPath))
                try
                    let context = StructureTreeObjects.createContext document pdfPath
                    Analysed(document, context, LegibylChecks.run document context)
                with _ ->
                    document.Close()
                    reraise ()
            with
            | :? iText.Kernel.Exceptions.PdfException
            | :? IOException as ex -> CouldNotInspect(LegibylChecks.couldNotInspect ex.Message))

    /// Waits for the analysis to finish and closes its document, whatever happened to it.
    let closeLegibylAnalysis (analysis: Task<LegibylAnalysis>) =
        try
            match analysis.GetAwaiter().GetResult() with
            | Analysed(document, _, _) -> document.Close()
            | CouldNotInspect _ -> ()
        with _ ->
            ()

    /// The rules an untagged document fails that tagging it from its layout fixes: no MarkInfo
    /// (6.2-1 in both), no structure tree (PDF/UA-1's 7.1-11, PDF/UA-2's 8.2.1-1), untagged content
    /// (7.1-3; 8.2.2-1), untagged annotations (7.18.1-2; 8.9.2.3-1) and links (7.18.5-1;
    /// 8.2.5.20-1), and pages with links but no tab order (7.18.3-1; 8.9.3.3-1).
    let private tagFromLayoutRules =
        set
            [ for rule in [ "6.2-1"; "7.1-3"; "7.1-11"; "7.18.1-2"; "7.18.3-1"; "7.18.5-1" ] -> PdfProfile.PdfUa1, rule
              for rule in [ "6.2-1"; "8.2.1-1"; "8.2.2-1"; "8.9.2.3-1"; "8.2.5.20-1"; "8.9.3.3-1" ] -> PdfProfile.PdfUa2, rule ]

    let private tagFromLayoutFixes (assertion: RuleViolation) =
        match PdfUaTarget.ofSpecification assertion.Specification with
        | Some standard -> tagFromLayoutRules.Contains((standard, assertion.RuleId))
        | None -> false

    let private linkAnnotationPattern = Regex(@"\((\d+) \d+ obj PDLinkAnnot\)", RegexOptions.Compiled)

    let private annotationPattern = Regex(@"\((\d+) \d+ obj PD\w*Annot\)", RegexOptions.Compiled)

    let private mcidPattern = Regex(@"\{mcid:(\d+)\}", RegexOptions.Compiled)

    /// PDF/UA's rules for content, and annotations, the tags don't hold: which a missing parent
    /// tree makes fail even where the tags do hold them.
    let private untaggedContentRules = set [ PdfProfile.PdfUa1, "7.1-3"; PdfProfile.PdfUa2, "8.2.2-1" ]

    let private untaggedAnnotationRules =
        set
            [ PdfProfile.PdfUa1, "7.18.1-2"
              PdfProfile.PdfUa1, "7.18.5-1"
              PdfProfile.PdfUa2, "8.2.5.20-1"
              PdfProfile.PdfUa2, "8.9.2.3-1" ]

    /// The link annotations a finding's location names, by object number.
    /// A WCAG 2.2 finding's link: its page (0-based) and its place among the page's annotations.
    let private wcagAnnotationPattern = Regex(@"^root/doc\[0\]/pages\[(\d+)\]/annots\[(\d+)\]", RegexOptions.Compiled)

    let private linkAnnotationNumbers (locationContext: string | null) =
        match locationContext with
        | NonNull context -> set [ for m in linkAnnotationPattern.Matches context -> int m.Groups[1].Value ]
        | Null -> Set.empty

    /// Adds page regions to veraPDF's failures and appends Legibyl's own warnings.
    let withLegibylAnalysis (analysis: LegibylAnalysis) (assertions: RuleViolation list) =
        match analysis with
        | CouldNotInspect warning -> assertions @ [ warning ]
        | Analysed(document, context, warnings) ->
            try
                // The link fix only describes links in a table of contents, so a missing link
                // description elsewhere isn't fixable (see StructureFixes.describeLinks).
                let tocLinkAnnotations = lazy (StructureFixes.tocLinkAnnotationNumbers document context)
                // Links to web pages can be described by their pages' titles, wherever they are.
                let webLinkAnnotations = lazy (StructureFixes.webLinkAnnotationNumbers document)
                let hasTitlePage = lazy (StructureFixes.titlePageText document |> Option.isSome)
                let untagged = isNull (document.GetCatalog().GetPdfObject().Get iText.Kernel.Pdf.PdfName.StructTreeRoot)
                // PDF/UA-2's links within the document that don't point to structure (8.8): those
                // LinkTargets can place, and table-of-contents links whose headings the
                // HeadingsFromToc fix tags (it points them at the headings).
                let linkTargets = lazy (LinkTargets.find document context)
                // What the tags hold, where the parent tree doesn't say so: rebuilding it fixes those.
                let unresolvedTags = lazy (ParentTree.find document context |> Option.map (fun _ -> ParentTree.tagged document context))
                let tocHeadingLinks =
                    lazy
                        (match TocHeadings.analyse document context with
                         | TocHeadings.Recoverable(headings, _) ->
                             set
                                 [ for heading in headings do
                                       for annotation in heading.Entry.Links do
                                           match annotation.GetIndirectReference() with
                                           | null -> ()
                                           | reference -> yield reference.GetObjNumber() ]
                         | _ -> Set.empty)
                let languageUndeclared = not (DocumentLanguage.isDeclared document)
                // No title in Info or XMP: the fixes that copy one across have none to copy.
                let untitled =
                    String.IsNullOrWhiteSpace(document.GetDocumentInfo().GetTitle()) && (StructureFixes.xmpTitle document).IsNone
                let fixOptionsFor (assertion: RuleViolation) =
                    let isIn (numbers: Lazy<Set<int>>) =
                        match assertion.Location with
                        | NonNull location when location.ObjectNumber.HasValue -> numbers.Value.Contains location.ObjectNumber.Value
                        | _ -> false
                    let options =
                        if assertion.FixOptions.HasFlag PdfFixOptions.LinkContents && not (isIn tocLinkAnnotations) then
                            assertion.FixOptions &&& ~~~PdfFixOptions.LinkContents
                        else
                            assertion.FixOptions
                    let options =
                        if assertion.FixOptions.HasFlag PdfFixOptions.LinkContents && isIn webLinkAnnotations then
                            options ||| PdfFixOptions.LinkTitlesFromWeb
                        else
                            options
                    // Titling from the first page does what DocumentTitle does, and can only
                    // add a title where the document has a title page to take it from.
                    let options =
                        if options.HasFlag PdfFixOptions.Title && hasTitlePage.Value then
                            options ||| PdfFixOptions.TitleFromFirstPage
                        else
                            options
                    // What tagging an untagged document from its layout addresses.
                    let options =
                        if untagged && tagFromLayoutFixes assertion then
                            options ||| PdfFixOptions.TagFromLayout
                        else
                            options
                    let options =
                        match PdfUaTarget.ofSpecification assertion.Specification with
                        | Some PdfProfile.PdfUa2 when assertion.RuleId = "8.8-1" || assertion.RuleId = "8.8-2" ->
                            let links = linkAnnotationNumbers assertion.LocationContext
                            let placed =
                                if links.IsEmpty then linkTargets.Value |> List.exists (fun t -> match t.Holder with LinkTargets.Link _ -> false | _ -> true)
                                else not (Set.intersect links (LinkTargets.linkNumbers linkTargets.Value)).IsEmpty
                            let options = if placed then options ||| PdfFixOptions.LinkTargets else options
                            if not (Set.intersect links tocHeadingLinks.Value).IsEmpty then options ||| PdfFixOptions.HeadingsFromToc else options
                        | _ -> options
                    let options =
                        match PdfUaTarget.ofSpecification assertion.Specification with
                        | Some standard when untaggedContentRules.Contains((standard, assertion.RuleId)) ->
                            match unresolvedTags.Value, assertion.Location, assertion.LocationContext with
                            | Some tags, NonNull location, NonNull locationContext when location.PageNumber.HasValue ->
                                let mcids = mcidPattern.Matches locationContext
                                if mcids.Count > 0 && tags.MarkedContent.ContainsKey((location.PageNumber.Value, int mcids[mcids.Count - 1].Groups[1].Value)) then
                                    options ||| PdfFixOptions.ParentTree
                                else
                                    options
                            | _ -> options
                        | Some standard when untaggedAnnotationRules.Contains((standard, assertion.RuleId)) ->
                            match unresolvedTags.Value, assertion.LocationContext with
                            | Some tags, NonNull locationContext when
                                annotationPattern.Matches locationContext |> Seq.exists (fun m -> tags.Objects.ContainsKey(int m.Groups[1].Value))
                                ->
                                options ||| PdfFixOptions.ParentTree
                            | _ -> options
                        | _ -> options
                    // Every "natural language ... shall be determined" rule falls back on /Lang.
                    let options =
                        if languageUndeclared && assertion.Message.Contains("Natural language", StringComparison.OrdinalIgnoreCase) then
                            options ||| PdfFixOptions.Language
                        else
                            options
                    // Without a title to copy, only titling from the first page (or by hand) gives
                    // the metadata its dc:title.
                    if untitled && isDcTitleRule assertion then
                        options &&& ~~~(PdfFixOptions.Metadata ||| PdfFixOptions.Title)
                    else
                        options
                // With no metadata stream, veraPDF can't check its dc:title, so an untitled
                // document only fails that once the metadata fix has written one (and then veraPDF
                // won't add the PDF/UA identifier). Report it now, so it's fixed first.
                let missingTitle =
                    if not untitled then []
                    else
                        assertions
                        |> List.tryPick (fun assertion ->
                            match PdfUaTarget.ofSpecification assertion.Specification with
                            | Some standard when not assertion.Passed && noMetadataRules.Contains((standard, assertion.RuleId)) ->
                                let clause, testNumber = dcTitleRule standard
                                if assertions |> List.exists (fun a -> a.Specification = assertion.Specification && a.Clause = clause && a.TestNumber = testNumber && not a.Passed) then
                                    None
                                else
                                    Some
                                        { assertion with
                                            Clause = clause
                                            TestNumber = testNumber
                                            Message = ruleDescription standard assertion.Specification clause testNumber |> Option.defaultValue "The metadata shall contain a dc:title entry."
                                            ErrorMessage =
                                                "The document has no title (in its Info dictionary or XMP metadata). veraPDF can't check dc:title until there's metadata, so this would fail once the metadata fix writes it, and then the PDF/UA identifier isn't added. Give it a title (Structure…'s Document title field, or a review's title with fix --edits): that's written before the metadata fix, in the same pass."
                                            FixOptions = PdfFixOptions.Title }
                            | _ -> None)
                        |> Option.toList
                // A WCAG 2.2 finding about a link names it by page and position
                // ("root/doc[0]/pages[0]/annots[2]"), not by object: the annotation it is.
                let wcagAnnotation (assertion: RuleViolation) =
                    match assertion.LocationContext with
                    | NonNull locationContext when WcagSpecification.isWcag assertion.Specification ->
                        let found = wcagAnnotationPattern.Match locationContext
                        if not found.Success then None
                        else
                            let page = int found.Groups[1].Value + 1
                            let index = int found.Groups[2].Value
                            if page > document.GetNumberOfPages() then None
                            else
                                let annotations = document.GetPage(page).GetAnnotations()
                                if index < annotations.Count then Some(page, annotations[index].GetPdfObject()) else None
                    | _ -> None
                // A link whose 2.4.9 finding (above AA: a warning) is because it has no description
                // at all, not one that's only its address: that fails 2.4.4 (level A) too.
                let undescribed (annotation: iText.Kernel.Pdf.PdfDictionary) =
                    let said (text: string | null) = not (String.IsNullOrWhiteSpace text)
                    let contents = annotation.GetAsString iText.Kernel.Pdf.PdfName.Contents |?> _.ToUnicodeString()
                    let element =
                        match annotation.GetIndirectReference() with
                        | null -> None
                        | reference ->
                            match (ParentTree.tagged document context).Objects.TryGetValue(reference.GetObjNumber()) with
                            | true, element -> Some element
                            | _ -> None
                    not (said contents)
                    && (match element with
                        | Some element ->
                            not (said (element.GetAsString iText.Kernel.Pdf.PdfName.Alt |?> _.ToUnicodeString()))
                            && not (said (StructureTreeObjects.extractText element context (StructureTreeObjects.ancestorPage element context)))
                        | None -> true)
                let located =
                    assertions @ missingTitle
                    |> List.map (fun assertion ->
                        // Failures, and veraPDF's findings that are warnings (WCAG 2.2's above AA).
                        if not assertion.IsFinding then
                            assertion
                        else
                            let annotation = wcagAnnotation assertion
                            // Named by the annotation's object, as PDF/UA's findings about one are.
                            let assertion =
                                match annotation with
                                | Some(page, dictionary) ->
                                    match dictionary.GetIndirectReference() with
                                    | NonNull reference ->
                                        { assertion with LocationContext = $"{assertion.LocationContext}({reference.GetObjNumber()} 0 obj PDLinkAnnot)" }
                                    | Null -> assertion
                                | None -> assertion
                            let assertion =
                                match annotation with
                                | Some(_, dictionary) when assertion.RuleId = "2.4.9-1" && assertion.Severity = FindingSeverity.Warning && undescribed dictionary ->
                                    { assertion with
                                        Specification = $"{WcagSpecification.Name} A"
                                        Passed = false
                                        Severity = FindingSeverity.Failure
                                        ErrorMessage = "This link has no description at all (no Contents, alt text or text of its own), so it fails 2.4.4 Link Purpose (In Context), level A, too." }
                                | _ -> assertion
                            { assertion with
                                Geometry =
                                    FindingGeometry.locate context document assertion
                                    |> Option.orElseWith (fun () -> annotation |> Option.bind (fun (page, dictionary) -> StructureTreeObjects.rectGeometry page dictionary))
                                    |> Option.toObj
                                Element = FindingGeometry.describe context document assertion |> Option.toObj
                                FixOptions = fixOptionsFor assertion })
                // Legibyl's own warnings name their elements the same way.
                let warnings = warnings |> List.map (fun w -> { w with Element = FindingGeometry.describe context document w |> Option.toObj })
                located @ warnings
            with
            | :? iText.Kernel.Exceptions.PdfException
            | :? IOException as ex -> assertions @ [ LegibylChecks.couldNotInspect ex.Message ]

    /// veraPDF only exposes its running counters as text ("Progress: 12 checks, 0 failed, 3
    /// processed objects, 9 in stack.", where "failed" counts rules, not checks); its own
    /// progress output goes to stdout.
    let private progressPattern =
        Regex(@"(\d+) checks, (\d+) failed, (\d+) processed objects, (\d+) in stack", RegexOptions.Compiled)

    /// Reads the path of the object veraPDF is checking, e.g. "root/document[0]/pages[57](…)/
    /// contentStream[0]/…", from the top of BaseValidator's private objectsContext stack.
    /// veraPDF has no public way to say where it is; if a later version renames the field,
    /// the reader returns "" and progress just stays on Preparing.
    let private contextReader (validator: PDFAValidator) : unit -> string =
        let rec findField (t: Type | null) : Reflection.FieldInfo | null =
            match t with
            | null -> null
            | t ->
                match t.GetField("objectsContext", Reflection.BindingFlags.Instance ||| Reflection.BindingFlags.NonPublic) with
                | null -> findField t.BaseType
                | field -> field
        match findField (validator.GetType()) with
        | null -> fun () -> ""
        | field ->
            fun () ->
                match field.GetValue validator with
                | :? java.util.Deque as stack -> string (stack.peekFirst ())
                | _ -> ""

    let private pagePattern = Regex(@"^root/document\[0\]/pages\[(\d+)\]", RegexOptions.Compiled)

    /// Runs validate, reporting the validator's counters and position to progress every
    /// quarter second and once more when it finishes.
    /// Runs validate, reporting the validator's counters and position to progress every
    /// quarter second and once more when it finishes, as stage when given (a pass that says
    /// nothing of its pages, as WCAG's doesn't), else the page it's on.
    let withStageProgress (stage: ValidationStage option) (validator: PDFAValidator) (pageCount: int) (progress: IProgress<ValidationProgress>) (validate: unit -> 'T) =
        let stopwatch = Diagnostics.Stopwatch.StartNew()
        // veraPDF checks the pages in order, then the rest (e.g. the structure tree). Keep the
        // furthest page seen: while reading, and briefly between pages, there's no current
        // object at all. Any other object after the pages have started means they're done.
        let mutable page = 0
        let mutable pastPages = false
        let currentContext = contextReader validator
        let report finished =
            let found = progressPattern.Match(validator.getValidationProgressString ())
            if found.Success then
                let context = currentContext ()
                let onPage = pagePattern.Match context
                if finished || (page > 0 && context <> "" && not onPage.Success) then
                    pastPages <- true
                    page <- max page pageCount
                elif onPage.Success && not pastPages then
                    page <- max page (int onPage.Groups[1].Value + 1)
                let stage =
                    match stage with
                    | Some stage -> stage
                    | None when pastPages -> ValidationStage.Finishing
                    | None when page = 0 -> ValidationStage.Preparing
                    | None -> ValidationStage.CheckingPages
                let count (group: int) = int found.Groups[group].Value
                progress.Report
                    { Stage = stage
                      Page = (if stage = ValidationStage.CheckingWcag then pageCount else page)
                      PageCount = pageCount
                      Checks = count 1
                      FailedRules = count 2
                      ProcessedObjects = count 3
                      QueuedObjects = count 4
                      Elapsed = stopwatch.Elapsed }
        // An exception escaping a timer callback would take down the process; progress is
        // best-effort, so a failed read just skips that update.
        let timer = new Timer((fun _ -> try report false with _ -> ()), null, 0, 250)
        let result =
            try
                validate ()
            finally
                // Wait for any in-flight callback, so no stale report lands after the final one.
                use stopped = new ManualResetEvent(false)
                if timer.Dispose stopped then
                    stopped.WaitOne() |> ignore
        report true
        result

    /// withStageProgress for PDF/UA's pass: the stage from the page it's on.
    let withProgress validator pageCount progress validate = withStageProgress None validator pageCount progress validate

    /// Runs validate, a pass of validator's, stopping it (between the objects it checks) if
    /// cancel is cancelled, which then throws OperationCanceledException.
    let cancellable (cancel: CancellationToken) (validator: PDFAValidator) (validate: unit -> 'T) =
        cancel.ThrowIfCancellationRequested()
        use _ = cancel.Register(fun () -> validator.cancelValidation org.verapdf.processor.reports.enums.JobEndStatus.CANCELLED)
        let result =
            try
                validate ()
            with _ when cancel.IsCancellationRequested ->
                // (However veraPDF ends a pass it's stopped.)
                raise (OperationCanceledException cancel)
        cancel.ThrowIfCancellationRequested()
        result

    /// WCAG 2.2's machine checks of pdfPath (see wcagProfile), reporting the CheckingWcag stage to
    /// progress if given; stopped if cancel is.
    let validateWcag (foundry: VeraPDFFoundry) (pdfPath: string) (pageCount: int) (progress: IProgress<ValidationProgress> | null) (cancel: CancellationToken) =
        let profile = wcagProfile.Value
        use parser = foundry.createParser (java.io.File pdfPath, profile.getPDFAFlavour ())
        // Its error messages kept: they say the measured contrast ratio, the text size, how many
        // characters repeat.
        let validator = org.verapdf.pdfa.validation.validators.ValidatorFactory.createValidator (profile, -1, false, true, false)
        cancellable cancel validator (fun () ->
            match progress with
            | null -> validator.validate parser
            | progress -> withStageProgress (Some ValidationStage.CheckingWcag) validator pageCount progress (fun () -> validator.validate parser))

    /// veraPDF's detail for a finding, in words where it's a measurement: WCAG 2.2's contrast
    /// ("contrastRatio = 1.6059…") as the ratio it is, and what's needed.
    let private wcagDetail (specification: string) (clause: string) (detail: string | null) : string | null =
        match detail with
        | NonNull detail when specification.StartsWith("WCAG", StringComparison.Ordinal) && clause = "1.4.3" ->
            let ratio = Regex.Match(detail, @"contrastRatio\s*=\s*([0-9.]+)")
            match Double.TryParse(ratio.Groups[1].Value, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, value when ratio.Success ->
                let shown = value.ToString("0.0#", Globalization.CultureInfo.InvariantCulture)
                $"Contrast {shown} to 1, where at least 4.5 to 1 (3 to 1 for large text: 18 pt, or 14 pt bold) is needed."
            | _ -> detail
        | detail -> detail

    let toReport (pdfPath: string) profile (result: ValidationResult) (wcag: ValidationResult option) (analysis: LegibylAnalysis) : ValidationReport =
        let assertions =
            [ for result in result :: Option.toList wcag do
              for item in javaSeq (result.getTestAssertions ()) do
                  let assertion = item :?> TestAssertion
                  let ruleId = assertion.getRuleId ()
                  // NB: assertion.getLocationContext() is a rarely-populated, model-object-specific
                  // "extra context" field (almost always null). The real per-failure tree-walk
                  // path (e.g. "root/Catalog[0]/StructTreeRoot[0]/.../TH[3]") — the same value
                  // veraPDF's own report writer uses — lives on assertion.getLocation().getContext().
                  // A failure of WCAG 2.2 above level AA, which Legibyl aims at, is a warning, as is
                  // one of its cosmetic rules (wcagCosmetic).
                  let aboveTarget =
                      ruleId.getSpecification().ToString().StartsWith("WCAG", StringComparison.Ordinal)
                      && (WcagSpecification.isAboveTarget (ruleId.getClause ())
                          || wcagCosmetic.Value.Contains(ruleId.getClause (), ruleId.getTestNumber ()))
                      && assertion.getStatus () <> TestAssertion.Status.PASSED
                  let violation =
                      { Specification = specificationName (ruleId.getSpecification().ToString()) (ruleId.getClause ())
                        Clause = ruleId.getClause ()
                        TestNumber = ruleId.getTestNumber ()
                        Passed = aboveTarget || (assertion.getStatus () = TestAssertion.Status.PASSED)
                        Message = assertion.getMessage ()
                        ErrorMessage = wcagDetail (ruleId.getSpecification().ToString()) (ruleId.getClause ()) (assertion.getErrorMessage ())
                        LocationContext = assertion.getLocation () |?> _.getContext()
                        Severity = (if aboveTarget then FindingSeverity.Warning else FindingSeverity.Failure)
                        FixOptions = PdfFixOptions.None
                        Geometry = null
                        Element = null }
                  { violation with FixOptions = determineFixOptions violation } ]

        { FilePath = pdfPath
          Profile = profile
          FlavourId = result.getPDFAFlavour().getId()
          IsCompliant = result.isCompliant ()
          PdfVersion = PdfUaTarget.pdfVersion pdfPath |> Option.toObj
          // The caller chose it; one that detected it (PdfUaTarget.detect) says so.
          ProfileDetected = false
          TotalAssertions = result.getTotalAssertions () + (wcag |> Option.map _.getTotalAssertions() |> Option.defaultValue 0)
          Assertions = List.toArray (withLegibylAnalysis analysis assertions)
          Notes = [||]
          WcagError = null }

    let createTemporaryPdfPath (outputPath: string) =
        let prefix =
            match Path.GetFileNameWithoutExtension outputPath with
            | NonNull prefix when not (String.IsNullOrWhiteSpace prefix) -> prefix
            | _ -> "legibyl"
        Path.Combine(Path.GetTempPath(), $"{prefix}.{Guid.NewGuid():N}.tmp.pdf")

    let deleteIfExists (path: string | null) =
        match path with
        | NonNull path when File.Exists path -> File.Delete path
        | _ -> ()

    /// Deletes a temporary file, retrying briefly in case another handle is still closing.
    let tryDeleteTemporary (path: string | null) =
        let rec attempt n (path: string) =
            try
                File.Delete path
            with
            | :? IOException
            | :? UnauthorizedAccessException when n < 4 ->
                Thread.Sleep 100
                attempt (n + 1) path
            | :? IOException
            | :? UnauthorizedAccessException -> ()

        match path with
        | NonNull path when File.Exists path -> attempt 0 path
        | _ -> ()

    type MetadataFixOutcome =
        { Changed: bool
          Status: RepairStatus
          AppliedFixes: string list }

    let fixMetadataOnly (pdfPath: string) profile (outputPath: string) =
        ensureExists pdfPath
        let flavour = toJavaFlavour profile
        let foundry = Foundries.defaultInstance ()
        // veraPDF's own output is an incremental update iText can't read reliably, so it only
        // goes to a scratch file; MetadataTransfer copies the fix into outputPath.
        let fixedPath = createTemporaryPdfPath outputPath
        try
            let status, appliedFixes =
                wrapErrors $"veraPDF failed to fix '{pdfPath}'" (fun () ->
                    use parser = foundry.createParser (java.io.File pdfPath, flavour)
                    let result = foundry.createValidator(flavour, false).validate parser
                    let fixResult =
                        use outputStream = new java.io.FileOutputStream(fixedPath)
                        foundry.createMetadataFixer().fixMetadata (parser, outputStream, result)

                    toRepairStatus (fixResult.getRepairStatus ()),
                    [ for fix in javaSeq (fixResult.getAppliedFixes ()) -> fix :?> string ])

            // Only Success/IdRemoved reflect the fixer actually changing something; NoAction,
            // WontFix and FixError leave a fixed copy that's either empty or just a useless
            // byte-for-byte (or broken) duplicate of the original. Don't produce a file the user
            // didn't ask for and can't use.
            let changed =
                (status = RepairStatus.Success || status = RepairStatus.IdRemoved)
                && File.Exists fixedPath
                && FileInfo(fixedPath).Length > 0L
            let revised =
                changed
                && wrapErrors $"veraPDF failed to read its fixed copy of '{pdfPath}'" (fun () ->
                    MetadataTransfer.apply pdfPath fixedPath outputPath)
            // Say the revision it has, not the one veraPDF wrote.
            let revision = $"'rev' with value '{MetadataTransfer.PdfUa2Revision}'"
            let appliedFixes =
                if not revised then appliedFixes
                elif appliedFixes |> List.exists (fun fix -> fix.Contains "'rev' with value") then
                    appliedFixes |> List.map (fun fix -> Regex.Replace(fix, @"'rev' with value '[^']*'", revision))
                else
                    appliedFixes @ [ $"Set property {revision} in Identification schema" ]

            { Changed = changed
              Status = status
              AppliedFixes = appliedFixes }
        finally
            tryDeleteTemporary fixedPath

    /// Moves the last fix step's temporary file to the requested output path.
    ///
    /// veraPDF's parser can still be holding the file it read when the metadata fixer
    /// decided not to change anything: its Java handle is released on its own schedule
    /// under IKVM, not necessarily by the time we get here. So retry briefly, as
    /// tryDeleteTemporary does for the same reason, and fall back to copying — the
    /// source is a temp file the caller deletes afterwards either way.
    let moveToOutput (temporaryPath: string) (outputPath: string) =
        Directory.CreateDirectory(nonNull (Path.GetDirectoryName(Path.GetFullPath outputPath))) |> ignore

        let rec attempt n =
            try
                File.Move(temporaryPath, outputPath, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException when n < 4 ->
                Thread.Sleep 100
                attempt (n + 1)
            | :? IOException
            | :? UnauthorizedAccessException -> File.Copy(temporaryPath, outputPath, true)

        attempt 0

open VeraPdf

/// IPdfValidator implementation backed by veraPDF's Greenfield Java validation core,
/// executed in-process via IKVM. All Java/IKVM types stay inside this assembly;
/// callers only see the plain .NET types in this namespace (ValidationReport,
/// RuleViolation, etc).
[<Sealed>]
type VeraPdfValidator() =
    // A plain field: an auto-property (member val) of a nullable type reaches other assemblies
    // as non-nullable.
    let mutable review: Func<Collections.Generic.IReadOnlyList<TaggedRegion>, string | null> | null = null
    let mutable title: string | null = null
    let mutable language: string | null = null
    // veraPDF's one-time setup (a second or two) happens on first use, not here, so a validator
    // can be made on a UI thread; Warm does it ahead of time.

    /// Where the TagFromLayout and JoinSplitParagraphs fixes get each page's regions (see
    /// Legibyl.Layout). Without one, those fixes do nothing.
    member val LayoutSource: IPageLayoutSource | null = null with get, set

    /// Builds the review to carry in the fixed copy (see EmbeddedReview), as JSON (null for none),
    /// given where tagging from the layout put each region (none if it didn't tag), so edits made
    /// for regions can be moved to the elements they became. Null for no review.
    member _.Review
        with get (): Func<Collections.Generic.IReadOnlyList<TaggedRegion>, string | null> | null = review
        and set (value: Func<Collections.Generic.IReadOnlyList<TaggedRegion>, string | null> | null) = review <- value

    /// The edits the ApplyEdits fix writes (see ElementEdits).
    member val Edits: Collections.Generic.IReadOnlyList<ElementEdit> = [] with get, set

    /// The title a person gave the document, which the ApplyEdits fix writes (see
    /// ElementEdits.applyTitle); null for none.
    member _.Title
        with get (): string | null = title
        and set (value: string | null) = title <- value

    /// The language a person chose for the document (Structure…'s Document language, a review's
    /// "language", or the CLI's --lang): Fix declares it (the Language fix, which it runs then),
    /// in place of any it says or its text reads as; null for none.
    member _.Language
        with get (): string | null = language
        and set (value: string | null) = language <- value

    /// A report of the file Fix is given, as it is now, against the standard asked for (one the
    /// caller has already, e.g. the window it's shown in): Fix uses it rather than validating the
    /// file again. Only for a file unchanged since (see SavedResults.hashOf); None to validate it.
    member val Validated: ValidationReport option = None with get, set

    /// The standard tag a person chose for each of the PDF's own tag names (Structure…'s Role
    /// map, or a review's "roleMap"), which the ApplyEdits fix writes to its role map before the
    /// RoleMap fix maps the others by what they say.
    member val RoleMap: Collections.Generic.IReadOnlyDictionary<string, string> = Collections.Generic.Dictionary<string, string>() with get, set

    /// What a person gave the PDF's attached files and media clips (Structure…'s Attachments, or a
    /// review's "attachments"), by key (see Attachments), which the ApplyEdits fix writes.
    member val Attachments: Collections.Generic.IReadOnlyDictionary<string, Attachments.AttachmentEdit> =
        Collections.Generic.Dictionary<string, Attachments.AttachmentEdit>() with get, set

    /// Where the LinkTitlesFromWeb fix reads the titles of the web pages links go to: the web
    /// itself, safely (see LinkTitles), unless replaced.
    member val PageTitleSource: IPageTitleSource = LinkTitles.WebPageTitles() with get, set

    member _.Validate
        (
            pdfPath: string,
            profile: PdfProfile,
            [<Optional; DefaultParameterValue(null: IProgress<ValidationProgress> | null)>] progress: IProgress<ValidationProgress> | null,
            /// Whether to run WCAG 2.2's machine checks too, as every validation does but Append's,
            /// which is about PDF/UA's tags.
            [<Optional; DefaultParameterValue(true)>] checkWcag: bool,
            /// Stops it (OperationCanceledException), between the objects veraPDF checks.
            [<Optional>] cancel: CancellationToken
        ) : ValidationReport =
        ensureExists pdfPath
        ensureInitialised ()
        // (Not started, Legibyl's own checks included, if it's cancelled already.)
        cancel.ThrowIfCancellationRequested()
        let flavour = toJavaFlavour profile
        let foundry = Foundries.defaultInstance ()
        let analysis = startLegibylAnalysis pdfPath
        try
            // What veraPDF logs while it reads the file goes on the report, not the console.
            let report, notes =
              ValidatorLog.collecting (fun () ->
                wrapErrors $"veraPDF failed to validate '{pdfPath}'" (fun () ->
                use parser = foundry.createParser (java.io.File pdfPath, flavour)
                // Report every failure (-1: no per-rule limit) and don't log passed checks.
                // Once veraPDF has recorded 10,000 results it keeps only the first failure of
                // each rule, so logging passed checks hides repeated failures on any document
                // of more than a few pages.
                let validator = foundry.createValidator (flavour, -1, false, false, false)
                let pageCount =
                    match parser with
                    | :? GFModelParser as parser -> try parser.getPDDocument().getNumberOfPages () with _ -> 0
                    | _ -> 0
                // The last progress said: PDF/UA's pass's, for WCAG's to count on from; then WCAG's, to say
                // again as Legibyl's own checks finish.
                let mutable last: ValidationProgress option = None
                let result =
                  cancellable cancel validator (fun () ->
                    match progress with
                    | null -> validator.validate parser
                    | progress ->
                        let tracked =
                            { new IProgress<ValidationProgress> with
                                member _.Report p =
                                    last <- Some p
                                    progress.Report p }
                        withProgress validator pageCount tracked (fun () -> validator.validate parser))
                // Then WCAG 2.2's machine checks, with every validation. What veraPDF logs while it
                // reads the file for them is PDF/UA's notes again, and its WCAG heuristics' workings
                // ("Semantic span contains child of unexpected semantic type"): left out.
                let continued: IProgress<ValidationProgress> | null =
                    match progress, last with
                    | NonNull progress, Some before ->
                        { new IProgress<ValidationProgress> with
                            member _.Report p =
                                let p =
                                    { p with
                                        Checks = before.Checks + p.Checks
                                        FailedRules = before.FailedRules + p.FailedRules
                                        ProcessedObjects = before.ProcessedObjects + p.ProcessedObjects
                                        Elapsed = before.Elapsed + p.Elapsed }
                                last <- Some p
                                progress.Report p }
                    | progress, _ -> progress
                // A WCAG pass that fails (a page veraPDF's heuristics can't take, say) doesn't fail
                // the validation: PDF/UA's result stands, and WCAG 2.2 is said not checked.
                let wcag, wcagError =
                    if not checkWcag then None, null
                    else
                        try
                            Some(ValidatorLog.quietly (fun () -> validateWcag foundry pdfPath pageCount continued cancel)), null
                        with
                        | :? OperationCanceledException
                        | :? OutOfMemoryException -> reraise ()
                        | e -> None, (if e.Message.Trim() = "" then e.GetType().Name else e.Message)
                // veraPDF's done; Legibyl's own checks, run beside it, may not be.
                match progress, last with
                | NonNull progress, Some last when not analysis.IsCompleted -> progress.Report { last with Stage = ValidationStage.CheckingLegibyl }
                | _ -> ()
                // (Waited for until cancelled.)
                Task.WaitAny([| analysis :> Task |], cancel) |> ignore
                { toReport pdfPath profile result wcag (analysis.GetAwaiter().GetResult()) with WcagError = wcagError }))
            let wcagNote =
                match report.WcagError with
                | NonNull error ->
                    [ { Level = "SEVERE"
                        Logger = "Legibyl"
                        Message = $"{WcagSpecification.Name}'s checks couldn't run ({error}), so this PDF isn't counted as passing them. Validate it again; if this stays, report it with the PDF if you can share it."
                        Count = 1 } ]
                | Null -> []
            { report with Notes = Array.ofList (notes @ wcagNote) }
        finally
            // Cancelled: its document is closed once it's done, not waited for.
            if cancel.IsCancellationRequested then
                analysis.ContinueWith(fun (analysis: Task<LegibylAnalysis>) -> closeLegibylAnalysis analysis) |> ignore
            else
                closeLegibylAnalysis analysis

    member this.Fix
        (
            pdfPath: string,
            profile: PdfProfile,
            outputPath: string,
            [<Optional; DefaultParameterValue(PdfFixOptions.Metadata)>] options: PdfFixOptions,
            [<Optional; DefaultParameterValue(null: IProgress<FixProgress> | null)>] progress: IProgress<FixProgress> | null,
            /// Stops it (OperationCanceledException): between pages where it goes page by page,
            /// and between steps; nothing is written to outputPath.
            [<Optional>] cancel: CancellationToken
        ) : FixResult =
        ensureExists pdfPath
        ensureInitialised ()
        // A language a person chose is declared, whatever else is asked for.
        let options = if String.IsNullOrWhiteSpace this.Language then options else options ||| PdfFixOptions.Language
        if options = PdfFixOptions.None then
            raise (VeraPdfValidationException "At least one fix option must be selected.")

        let structuralOptions =
            StructureFixes.orderedFixes this.PageTitleSource None [] None None Map.empty Map.empty |> List.fold (fun all (option, _) -> all ||| option) PdfFixOptions.None
        let tagging = options.HasFlag PdfFixOptions.TagFromLayout && not (isNull this.LayoutSource)
        // Reading web pages' titles (up to two minutes) stops at once if it's cancelled: what it's
        // read by then isn't used.
        let titleSource =
            let titles = this.PageTitleSource
            if not cancel.CanBeCanceled then titles
            else
                { new IPageTitleSource with
                    member _.PageTitles addresses =
                        let reading = Task.Run(fun () -> titles.PageTitles addresses)
                        reading.Wait cancel
                        reading.GetAwaiter().GetResult() }
        // A report of the file as it is, given: it isn't validated again.
        let known = this.Validated |> Option.filter (fun report -> report.FilePath = pdfPath && report.Profile = profile)
        // The steps this fix runs, in order, so progress can say "step 2 of 4".
        let steps =
            [ if known.IsNone then yield FixStage.CheckingOriginal
              if tagging then yield FixStage.Tagging
              if options &&& structuralOptions <> PdfFixOptions.None then yield FixStage.Fixing
              if options.HasFlag PdfFixOptions.Metadata then yield FixStage.FixingMetadata ]
        // Said as each step and page starts, which is where it stops if it's cancelled.
        let report stage page pageCount =
            cancel.ThrowIfCancellationRequested()
            match progress with
            | NonNull progress ->
                progress.Report
                    { Stage = stage
                      // Checking the original against WCAG 2.2 is part of checking it.
                      Step = 1 + List.findIndex ((=) (if stage = FixStage.CheckingWcag then FixStage.CheckingOriginal else stage)) steps
                      Steps = steps.Length
                      Page = page
                      PageCount = pageCount }
            | Null -> ()

        let originalReport =
            match known with
            | Some report -> report
            | None ->
                report FixStage.CheckingOriginal 0 0
                let validationProgress =
                    { new IProgress<ValidationProgress> with
                        member _.Report p =
                            // Pages as they're checked, and all of them once they're done (a short
                            // document can finish before the first page is ever sampled).
                            if p.Stage = ValidationStage.CheckingPages || p.Stage = ValidationStage.Finishing then
                                report FixStage.CheckingOriginal p.Page p.PageCount
                            elif p.Stage = ValidationStage.CheckingWcag then
                                report FixStage.CheckingWcag 0 0 }
                this.Validate(pdfPath, profile, validationProgress, cancel = cancel)
        let appliedFixes = ResizeArray<string>()
        let notApplied = ResizeArray<string>()
        let mutable currentInput = pdfPath
        let mutable tempMetadataOutput: string | null = null
        let mutable tempStructuralOutput: string | null = null
        let mutable tempTaggedOutput: string | null = null
        let mutable tempReviewOutput: string | null = null
        // Where tagging put each region, for the review the copy carries.
        let mutable taggedRegions: TaggedRegion list = []
        let mutable changed = false
        let mutable status = RepairStatus.NoAction
        // Legibyl's own fixes asked for that found nothing to change.
        let mutable unneeded: PdfFixOptions list = []

        try
            try
                // Tagging an untagged document comes first, so the structural fixes can work
                // on its tags; then Legibyl's own fixes, then veraPDF's metadata fix.
                match this.LayoutSource with
                | NonNull source when tagging ->
                    report FixStage.Tagging 0 0
                    let tempOutput = createTemporaryPdfPath outputPath
                    tempTaggedOutput <- tempOutput
                    match
                        LayoutTagging.tag
                            currentInput
                            tempOutput
                            source
                            (options.HasFlag PdfFixOptions.QrLinks)
                            (if options.HasFlag PdfFixOptions.LinkTitlesFromWeb then Some titleSource else None)
                            (Option.ofObj this.Language |> Option.filter (String.IsNullOrWhiteSpace >> not) |> Option.map _.Trim())
                            (report FixStage.Tagging)
                    with
                    | [], _ ->
                        tryDeleteTemporary tempOutput
                        tempTaggedOutput <- null
                    | lines, regions ->
                        taggedRegions <- regions
                        appliedFixes.AddRange lines
                        currentInput <- tempOutput
                        changed <- true
                        status <- RepairStatus.Success
                | _ -> ()

                if options &&& structuralOptions <> PdfFixOptions.None then
                    report FixStage.Fixing 0 0
                    let tempOutput = createTemporaryPdfPath outputPath
                    tempStructuralOutput <- tempOutput
                    // Joining paragraphs asks the layout detector about pages, which takes a while.
                    let layout =
                        match this.LayoutSource with
                        | NonNull source -> Some { StructureFixes.Source = source; StructureFixes.PdfPath = pdfPath; StructureFixes.Report = report FixStage.Fixing }
                        | Null -> None
                    // Edits of elements (not regions, which tagging takes) that won't change anything, and why.
                    if options.HasFlag PdfFixOptions.ApplyEdits && not (options.HasFlag PdfFixOptions.TagFromLayout) then
                        notApplied.AddRange(ElementEdits.unapplied currentInput (List.ofSeq this.Edits))
                    let structuralFixes, nothingToDo, notes =
                        StructureFixes.applyFixes
                            currentInput
                            tempOutput
                            options
                            titleSource
                            layout
                            (List.ofSeq this.Edits)
                            (Option.ofObj this.Title)
                            (Option.ofObj this.Language |> Option.filter (String.IsNullOrWhiteSpace >> not) |> Option.map _.Trim())
                            (this.RoleMap |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq)
                            (this.Attachments |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq)
                            cancel
                    unneeded <- nothingToDo
                    notApplied.AddRange notes
                    match structuralFixes with
                    | [] ->
                        tryDeleteTemporary tempOutput
                        tempStructuralOutput <- null
                    | structuralFixes ->
                        appliedFixes.AddRange structuralFixes
                        currentInput <- tempOutput
                        changed <- true
                        status <- RepairStatus.Success

                if options.HasFlag PdfFixOptions.Metadata then
                    report FixStage.FixingMetadata 0 0
                    let tempOutput = createTemporaryPdfPath outputPath
                    tempMetadataOutput <- tempOutput
                    let metadataFix = fixMetadataOnly currentInput profile tempOutput
                    if metadataFix.Changed then
                        appliedFixes.AddRange metadataFix.AppliedFixes
                        currentInput <- tempOutput
                        changed <- true
                        status <- metadataFix.Status
                    else
                        // Its changes aren't kept, so they aren't applied fixes; say why, so a
                        // fixed copy saved for other fixes doesn't look like it fixed metadata too.
                        match metadataFix.Status with
                        | RepairStatus.WontFix ->
                            notApplied.Add
                                "The metadata wasn't fixed: veraPDF's metadata fixer found no way to make it conform (Won't fix), so its changes weren't kept."
                        | RepairStatus.FixError ->
                            notApplied.Add "The metadata wasn't fixed: veraPDF's metadata fixer failed, so its changes weren't kept."
                        | _ -> ()
                        if not changed then
                            status <- metadataFix.Status
                        tryDeleteTemporary tempOutput
                        tempMetadataOutput <- null

                let result =
                    { FilePath = pdfPath
                      Profile = profile
                      RequestedOptions = options
                      OutputPath = null
                      Status = status
                      AppliedFixes = appliedFixes
                      NotApplied = notApplied
                      OriginalReport = originalReport }

                if not changed then
                    deleteIfExists outputPath
                    // Nothing was written: say why for each fix asked for (veraPDF's metadata fix,
                    // and edits, say so themselves).
                    for option in unneeded do
                        if option <> PdfFixOptions.Metadata && option <> PdfFixOptions.ApplyEdits then
                            match PdfFixDescriptions.Describe option with
                            | described :: _ -> notApplied.Add $"{described}: nothing in this PDF needed it."
                            | [] -> ()
                    result
                else
                    match this.Review with
                    | NonNull review ->
                        match review.Invoke taggedRegions with
                        | NonNull review ->
                            let tempOutput = createTemporaryPdfPath outputPath
                            tempReviewOutput <- tempOutput
                            EmbeddedReview.write currentInput tempOutput review
                            currentInput <- tempOutput
                        | Null -> ()
                    | Null -> ()
                    cancel.ThrowIfCancellationRequested()
                    moveToOutput currentInput outputPath
                    { result with OutputPath = outputPath }
            with
            // Cancelled: however it ended (closing a half-written copy can fail too), it was stopped.
            | ex when cancel.IsCancellationRequested && not (ex :? OperationCanceledException) ->
                raise (OperationCanceledException("The fix was cancelled.", ex, cancel))
            | :? iText.Kernel.Exceptions.PdfException as ex ->
                raise (VeraPdfValidationException($"iText failed to fix '{pdfPath}': {ex.Message}", ex))
            | :? IOException as ex ->
                // The path in ex is the one that was actually locked or unwritable, which
                // may be a temporary file rather than outputPath; keep both.
                raise (VeraPdfValidationException($"Could not write fixed PDF for '{outputPath}': {ex.Message}", ex))
        finally
            for tempOutput in [ tempMetadataOutput; tempStructuralOutput; tempTaggedOutput; tempReviewOutput ] do
                if not (String.Equals(tempOutput, outputPath, StringComparison.OrdinalIgnoreCase)) then
                    tryDeleteTemporary tempOutput

    /// Reads the structure tree by opening the PDF a second time through veraPDF's
    /// plain PD model (org.verapdf.pd.PDDocument) — the same model the validator
    /// itself uses internally. This is intentionally independent of Validate / Fix
    /// (no shared parser/session state), since reading the tagged structure is a
    /// distinct concern from PDF/UA rule validation and callers may want it without
    /// running a full validation pass.
    member _.ReadStructureTree
        (
            pdfPath: string,
            [<Optional; DefaultParameterValue(null: IProgress<StructureReadProgress> | null)>] progress: IProgress<StructureReadProgress> | null,
            /// Stops it (OperationCanceledException), as it says where it's got.
            [<Optional>] cancel: CancellationToken
        ) : StructureNode | null =
        ensureExists pdfPath
        ensureInitialised ()
        cancel.ThrowIfCancellationRequested()
        let progress: IProgress<StructureReadProgress> | null =
            if not cancel.CanBeCanceled then progress
            else
                { new IProgress<StructureReadProgress> with
                    member _.Report p =
                        cancel.ThrowIfCancellationRequested()
                        match progress with
                        | NonNull progress -> progress.Report p
                        | Null -> () }
        let tree = wrapErrors $"veraPDF failed to read the structure tree of '{pdfPath}'" (fun () -> StructureTreeWalk.read pdfPath progress)
        cancel.ThrowIfCancellationRequested()
        tree

    /// Each record veraPDF logs (WARNING and above) as it comes, for a caller to show as it goes
    /// (the CLI's --verbose prints them); the report's Notes keep them too, counted. Nothing veraPDF
    /// logs reaches the console.
    static member Recorded: IEvent<ValidatorNote> = ValidatorLog.recorded.Publish

    /// The veraPDF version its results come from (stamped from VeraPdfVersion in
    /// Directory.Build.props).
    static member EngineVersion =
        typeof<VeraPdfValidator>.Assembly.GetCustomAttributes(typeof<Reflection.AssemblyMetadataAttribute>, false)
        |> Seq.cast<Reflection.AssemblyMetadataAttribute>
        |> Seq.tryFind (fun a -> a.Key = "VeraPdfVersion")
        |> Option.bind (fun a -> Option.ofObj a.Value)
        |> Option.defaultValue "unknown"

    /// What a rule of profile's says (its description, as a failure's message has it), by its
    /// specification ("ISO 14289-1:2014"), clause and test; None for a rule it doesn't have.
    static member RuleDescription(profile: PdfProfile, specification: string, clause: string, testNumber: int) : string option =
        ruleDescription profile specification clause testNumber

    /// Every rule of profile's: its specification ("ISO 14289-1:2014"), clause and test, and what
    /// it says.
    static member Rules(profile: PdfProfile) : (string * string * int * string) list =
        ensureInitialised ()
        let validationProfile = Profiles.getVeraProfileDirectory().getValidationProfileByFlavour (toJavaFlavour profile)
        [ for item in javaSeq (validationProfile.getRules ()) do
              let rule = item :?> Rule
              let id = rule.getRuleId ()
              yield id.getSpecification().ToString(), id.getClause (), id.getTestNumber (), rule.getDescription () ]

    /// Gets veraPDF ready ahead of time, e.g. on a background thread as an app starts: its
    /// one-time setup (a second or two), and a validation of a tiny PDF, so the code the first
    /// real validation runs is compiled already (most of a second more). Safe to run alongside
    /// validations; they wait for the setup if it's still under way.
    static member Warm() =
        ensureInitialised ()
        let pdf = Path.Combine(Path.GetTempPath(), $"legibyl-warm-{Guid.NewGuid():N}.pdf")
        try
            // Tagged, as most PDFs worth checking are, so tagged content's checks are compiled too.
            do
                use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfWriter(pdf))
                document.SetTagged() |> ignore
                use layout = new iText.Layout.Document(document)
                layout.Add(iText.Layout.Element.Paragraph "Legibyl") |> ignore
            VeraPdfValidator().Validate(pdf, PdfProfile.PdfUa1) |> ignore
        finally
            try File.Delete pdf with _ -> ()

    interface IPdfValidator with
        member this.Validate(pdfPath, profile, progress) = this.Validate(pdfPath, profile, progress)

    interface IPdfFixer with
        member this.Fix(pdfPath, profile, outputPath, options, progress) = this.Fix(pdfPath, profile, outputPath, options, progress)

    interface IStructureTreeReader with
        member this.ReadStructureTree(pdfPath, progress) = this.ReadStructureTree(pdfPath, progress)
