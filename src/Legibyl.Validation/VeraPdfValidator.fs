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

    let ensureInitialised () =
        if not initialised then
            lock initLock (fun () ->
                if not initialised then
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

    let toJavaFlavour profile =
        match profile with
        | PdfProfile.PdfUa1 -> PDFAFlavour.PDFUA_1
        | PdfProfile.PdfA1B -> PDFAFlavour.PDFA_1_B
        | PdfProfile.PdfA2B -> PDFAFlavour.PDFA_2_B
        | PdfProfile.PdfA2U -> PDFAFlavour.PDFA_2_U
        | PdfProfile.PdfA3B -> PDFAFlavour.PDFA_3_B
        | PdfProfile.PdfA4 -> PDFAFlavour.PDFA_4
        | _ -> raise (ArgumentOutOfRangeException(nameof profile, profile, "Unsupported PDF profile"))

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

    let private determineFixOptions (message: string | null) =
        let text = Option.ofObj message |> Option.defaultValue ""
        let contains (value: string) = text.Contains(value, StringComparison.OrdinalIgnoreCase)
        if contains "Tabs" && contains "annotation" && contains "S" then
            PdfFixOptions.AnnotationTabs
        elif contains "Contents key" then
            PdfFixOptions.LinkContents
        elif contains "dc:title" then
            PdfFixOptions.Metadata ||| PdfFixOptions.DocumentTitle
        elif
            contains "metadata"
            || contains "PDF/UA Identification"
            || contains "PDF/A Identification"
            || contains "conformance level"
            || contains "XMP"
        then
            PdfFixOptions.Metadata
        else
            PdfFixOptions.None

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

    /// Adds page regions to veraPDF's failures and appends Legibyl's own warnings.
    let withLegibylAnalysis (analysis: LegibylAnalysis) (assertions: RuleViolation list) =
        match analysis with
        | CouldNotInspect warning -> assertions @ [ warning ]
        | Analysed(document, context, warnings) ->
            try
                let located =
                    assertions
                    |> List.map (fun assertion ->
                        if assertion.Passed then
                            assertion
                        else
                            { assertion with Geometry = FindingGeometry.locate context document assertion |> Option.toObj })
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
    let withProgress (validator: PDFAValidator) (pageCount: int) (progress: IProgress<ValidationProgress>) (validate: unit -> 'T) =
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
                    if pastPages then ValidationStage.Finishing
                    elif page = 0 then ValidationStage.Preparing
                    else ValidationStage.CheckingPages
                let count (group: int) = int found.Groups[group].Value
                progress.Report
                    { Stage = stage
                      Page = page
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

    let toReport (pdfPath: string) profile (result: ValidationResult) (analysis: LegibylAnalysis) : ValidationReport =
        let assertions =
            [ for item in javaSeq (result.getTestAssertions ()) do
                  let assertion = item :?> TestAssertion
                  let ruleId = assertion.getRuleId ()
                  // NB: assertion.getLocationContext() is a rarely-populated, model-object-specific
                  // "extra context" field (almost always null). The real per-failure tree-walk
                  // path (e.g. "root/Catalog[0]/StructTreeRoot[0]/.../TH[3]") — the same value
                  // veraPDF's own report writer uses — lives on assertion.getLocation().getContext().
                  { Specification = ruleId.getSpecification().ToString()
                    Clause = ruleId.getClause ()
                    TestNumber = ruleId.getTestNumber ()
                    Passed = (assertion.getStatus () = TestAssertion.Status.PASSED)
                    Message = assertion.getMessage ()
                    ErrorMessage = assertion.getErrorMessage ()
                    LocationContext = assertion.getLocation () |?> _.getContext()
                    Severity = FindingSeverity.Failure
                    FixOptions = determineFixOptions (assertion.getMessage ())
                    Geometry = null } ]

        { FilePath = pdfPath
          Profile = profile
          FlavourId = result.getPDFAFlavour().getId()
          IsCompliant = result.isCompliant ()
          TotalAssertions = result.getTotalAssertions ()
          Assertions = List.toArray (withLegibylAnalysis analysis assertions) }

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
        // goes to a scratch file; MetadataTransfer copies the repair into outputPath.
        let repairedPath = createTemporaryPdfPath outputPath
        try
            let status, appliedFixes =
                wrapErrors $"veraPDF failed to fix '{pdfPath}'" (fun () ->
                    use parser = foundry.createParser (java.io.File pdfPath, flavour)
                    let result = foundry.createValidator(flavour, false).validate parser
                    let fixResult =
                        use outputStream = new java.io.FileOutputStream(repairedPath)
                        foundry.createMetadataFixer().fixMetadata (parser, outputStream, result)

                    toRepairStatus (fixResult.getRepairStatus ()),
                    [ for fix in javaSeq (fixResult.getAppliedFixes ()) -> fix :?> string ])

            // Only Success/IdRemoved reflect the fixer actually changing something; NoAction,
            // WontFix and FixError leave a repaired copy that's either empty or just a useless
            // byte-for-byte (or broken) duplicate of the original. Don't produce a file the user
            // didn't ask for and can't use.
            let changed =
                (status = RepairStatus.Success || status = RepairStatus.IdRemoved)
                && File.Exists repairedPath
                && FileInfo(repairedPath).Length > 0L
            if changed then
                wrapErrors $"veraPDF failed to read its repaired copy of '{pdfPath}'" (fun () ->
                    MetadataTransfer.apply pdfPath repairedPath outputPath)

            { Changed = changed
              Status = status
              AppliedFixes = appliedFixes }
        finally
            tryDeleteTemporary repairedPath

    /// Moves the last repair step's temporary file to the requested output path.
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
    do ensureInitialised ()

    member _.Validate
        (
            pdfPath: string,
            profile: PdfProfile,
            [<Optional; DefaultParameterValue(null: IProgress<ValidationProgress> | null)>] progress: IProgress<ValidationProgress> | null
        ) : ValidationReport =
        ensureExists pdfPath
        let flavour = toJavaFlavour profile
        let foundry = Foundries.defaultInstance ()
        let analysis = startLegibylAnalysis pdfPath
        try
            wrapErrors $"veraPDF failed to validate '{pdfPath}'" (fun () ->
                use parser = foundry.createParser (java.io.File pdfPath, flavour)
                // Report every failure (-1: no per-rule limit) and don't log passed checks.
                // Once veraPDF has recorded 10,000 results it keeps only the first failure of
                // each rule, so logging passed checks hides repeated failures on any document
                // of more than a few pages.
                let validator = foundry.createValidator (flavour, -1, false, false, false)
                let result =
                    match progress with
                    | null -> validator.validate parser
                    | progress ->
                        let pageCount =
                            match parser with
                            | :? GFModelParser as parser -> try parser.getPDDocument().getNumberOfPages () with _ -> 0
                            | _ -> 0
                        withProgress validator pageCount progress (fun () -> validator.validate parser)
                toReport pdfPath profile result (analysis.GetAwaiter().GetResult()))
        finally
            closeLegibylAnalysis analysis

    member this.Fix
        (
            pdfPath: string,
            profile: PdfProfile,
            outputPath: string,
            [<Optional; DefaultParameterValue(PdfFixOptions.Metadata)>] options: PdfFixOptions
        ) : FixResult =
        ensureExists pdfPath
        if options = PdfFixOptions.None then
            raise (VeraPdfValidationException "At least one fix option must be selected.")

        let originalReport = this.Validate(pdfPath, profile)
        let appliedFixes = ResizeArray<string>()
        let mutable currentInput = pdfPath
        let mutable tempMetadataOutput: string | null = null
        let mutable tempStructuralOutput: string | null = null
        let mutable changed = false
        let mutable status = RepairStatus.NoAction

        try
            try
                let structuralOptions =
                    PdfFixOptions.AnnotationTabs
                    ||| PdfFixOptions.RemoveEmptyParagraphs
                    ||| PdfFixOptions.LinkContents
                    ||| PdfFixOptions.DocumentTitle

                if options &&& structuralOptions <> PdfFixOptions.None then
                    let tempOutput = createTemporaryPdfPath outputPath
                    tempStructuralOutput <- tempOutput
                    match StructureFixes.applyFixes currentInput tempOutput options with
                    | [] ->
                        tryDeleteTemporary tempOutput
                        tempStructuralOutput <- null
                    | structuralFixes ->
                        appliedFixes.AddRange structuralFixes
                        currentInput <- tempOutput
                        changed <- true
                        status <- RepairStatus.Success

                if options.HasFlag PdfFixOptions.Metadata then
                    let tempOutput = createTemporaryPdfPath outputPath
                    tempMetadataOutput <- tempOutput
                    let metadataFix = fixMetadataOnly currentInput profile tempOutput
                    appliedFixes.AddRange metadataFix.AppliedFixes
                    if metadataFix.Changed then
                        currentInput <- tempOutput
                        changed <- true
                        status <- metadataFix.Status
                    else
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
                      OriginalReport = originalReport }

                if not changed then
                    deleteIfExists outputPath
                    result
                else
                    moveToOutput currentInput outputPath
                    { result with OutputPath = outputPath }
            with
            | :? iText.Kernel.Exceptions.PdfException as ex ->
                raise (VeraPdfValidationException($"iText failed to fix '{pdfPath}': {ex.Message}", ex))
            | :? IOException as ex ->
                // The path in ex is the one that was actually locked or unwritable, which
                // may be a temporary file rather than outputPath; keep both.
                raise (VeraPdfValidationException($"Could not write fixed PDF for '{outputPath}': {ex.Message}", ex))
        finally
            for tempOutput in [ tempMetadataOutput; tempStructuralOutput ] do
                if not (String.Equals(tempOutput, outputPath, StringComparison.OrdinalIgnoreCase)) then
                    tryDeleteTemporary tempOutput

    /// Reads the structure tree by opening the PDF a second time through veraPDF's
    /// plain PD model (org.verapdf.pd.PDDocument) — the same model the validator
    /// itself uses internally. This is intentionally independent of Validate / Fix
    /// (no shared parser/session state), since reading the tagged structure is a
    /// distinct concern from PDF/A|UA rule validation and callers may want it without
    /// running a full validation pass.
    member _.ReadStructureTree
        (
            pdfPath: string,
            [<Optional; DefaultParameterValue(null: IProgress<StructureReadProgress> | null)>] progress: IProgress<StructureReadProgress> | null
        ) : StructureNode | null =
        ensureExists pdfPath
        wrapErrors $"veraPDF failed to read the structure tree of '{pdfPath}'" (fun () -> StructureTreeWalk.read pdfPath progress)

    interface IPdfValidator with
        member this.Validate(pdfPath, profile, progress) = this.Validate(pdfPath, profile, progress)

    interface IPdfFixer with
        member this.Fix(pdfPath, profile, outputPath, options) = this.Fix(pdfPath, profile, outputPath, options)

    interface IStructureTreeReader with
        member this.ReadStructureTree(pdfPath, progress) = this.ReadStructureTree(pdfPath, progress)
