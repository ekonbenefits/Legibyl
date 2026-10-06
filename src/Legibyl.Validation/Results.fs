namespace Legibyl.Validation

open System
open System.Collections.Generic
open System.Runtime.InteropServices

/// Plain-language descriptions of the fixes, worded like the GUI's Auto Fixes dialog.
module PdfFixDescriptions =
    /// One description per option set in options, e.g. "Remove empty /P structure elements".
    let Describe (options: PdfFixOptions) : string list =
        [ PdfFixOptions.TagFromLayout, "Tag an untagged document from each page's layout"
          PdfFixOptions.QrLinks, "Link QR codes to the web addresses they encode"
          PdfFixOptions.ParentTree, "Rebuild the parent tree, so the tags are found for the content they hold"
          PdfFixOptions.RoleMap, "Map the PDF's own tag names to standard tags, and fix a broken role map"
          PdfFixOptions.DocumentRoot, "Put the tags under a single Document tag at the top of the structure"
          PdfFixOptions.Language, "Declare the document's language, detected from its text"
          PdfFixOptions.Metadata, "Write the metadata PDF/UA asks for, and its PDF/UA identifier (XMP)"
          PdfFixOptions.EmbedFonts, "Embed fonts that aren't embedded, from open fonts made to the same measure, or this computer's own copy"
          PdfFixOptions.FontGlyphs, "Write down how embedded fonts map their character codes to glyphs (/CIDToGIDMap)"
          PdfFixOptions.FontDictionaries, "Make embedded fonts' dictionaries agree with their font programs and CMaps"
          PdfFixOptions.FontUnicode, "Write which characters fonts draw (ToUnicode), from their own glyph names and Unicode tables"
          PdfFixOptions.Tabs, "Make the Tab key follow the reading order on pages with links or other annotations"
          PdfFixOptions.EmptyParagraphs, "Remove empty paragraph tags"
          PdfFixOptions.EmptyTocEntries, "Remove empty table-of-contents entries"
          PdfFixOptions.EmptyFigures, "Remove figure tags that draw nothing"
          PdfFixOptions.LinkContents, "Describe table-of-contents links with their own text"
          PdfFixOptions.LinkTitlesFromWeb, "Describe web links and QR codes by the titles of the pages they go to"
          PdfFixOptions.HeadingsFromToc, "Tag headings the table of contents lists (H1–H6) and point its links at them"
          PdfFixOptions.Lists, "Tag paragraphs that start with list markers as a list"
          PdfFixOptions.TableHeaderScope, "Say which cells each table header cell heads (its Scope: column or row)"
          PdfFixOptions.Bookmarks, "Add bookmarks for the H1 and H2 headings"
          PdfFixOptions.LinkTargets, "Point links and bookmarks within a PDF 2.0 document at the tags they go to"
          PdfFixOptions.Title, "Set a document title and show it in the title bar"
          PdfFixOptions.TitleFromFirstPage, "Title an untitled document from its title page's text"
          PdfFixOptions.JoinSplitParagraphs, "Join paragraphs tagged line by line where the page layout shows one block of text"
          PdfFixOptions.JoinSplitWords, "Give text blocks replacement text where adjacent Span tags split words"
          PdfFixOptions.ApplyEdits, "Write the edits a person made (alt text, replacement text, merged paragraphs)" ]
        |> List.filter (fun (option, _) -> options.HasFlag option)
        |> List.map snd

/// The outcome of a single validation rule check (org.verapdf.pdfa.results.TestAssertion).
type RuleViolation =
    { /// The specification identifier, e.g. "ISO 14289-1:2014".
      Specification: string
      /// The specification clause, e.g. "7.1".
      Clause: string
      /// The test number within the clause.
      TestNumber: int
      /// Whether this individual assertion passed.
      Passed: bool
      /// The human-readable rule description/message.
      Message: string
      /// Additional error detail, if any.
      ErrorMessage: string | null
      /// The context/path within the PDF where the assertion was checked.
      LocationContext: string | null
      /// Whether this is a failing validation assertion or a Legibyl warning.
      Severity: FindingSeverity
      /// Best-effort fix options that can address this finding, if any.
      FixOptions: PdfFixOptions
      /// The page region to highlight for this finding, if known. veraPDF findings leave
      /// this null (annotation regions come from Location instead); Legibyl's structure
      /// warnings set it from the offending element's content.
      Geometry: StructureGeometry | null
      /// What its location holds, named, where that's found: the element that owns the content
      /// (e.g. paragraph (P) "The garden in spring…"), or "content not in the tags". Null when it
      /// isn't known.
      Element: string | null }

    /// A short "clause.testNumber" identifier, e.g. "7.1-2".
    member this.RuleId = $"{this.Clause}-{this.TestNumber}"

    /// Best-effort, human-readable interpretation of LocationContext (page number +
    /// plain-English breadcrumb), ending with what's there when that's known (Element), in place
    /// of veraPDF's content streams and items. Null if there's no location context to parse.
    member this.Location: PdfLocation | null =
        match PdfLocationParser.Parse this.LocationContext, this.Element with
        | NonNull location, NonNull element ->
            let within = if location.PageNumber.HasValue then $"Document → Page {location.PageNumber.Value}" else "Structure tree"
            { location with Breadcrumb = $"{within} → {element}" }
        | location, _ -> location

    member this.IsFinding = not this.Passed || this.Severity = FindingSeverity.Warning

    member this.IsFixable = this.FixOptions <> PdfFixOptions.None

    /// What the available fixes do, e.g. "Remove empty /P structure elements"; empty if none.
    member this.FixDescription = String.Join("; ", PdfFixDescriptions.Describe this.FixOptions)

    override this.ToString() = $"[{this.Specification} {this.RuleId}] {this.Message}"

/// Something veraPDF logged while it read the file (see ValidatorLog): not a finding (no rule, not
/// counted), but worth keeping, like a font it couldn't read or marked content nested in marked content.
type ValidatorNote =
    { /// "WARNING" or "SEVERE".
      Level: string
      /// The logger's short name, e.g. "OperatorParser".
      Logger: string
      /// What it said, without the number a repeat ends with ("Content stream (object 16 0 obj): Nested MCID").
      Message: string
      /// How many times it said it.
      Count: int }

/// The result of validating a single PDF document against a PdfProfile.
type ValidationReport =
    { FilePath: string
      Profile: PdfProfile
      /// The flavour id actually reported by veraPDF, e.g. "ua1".
      FlavourId: string
      /// The PDF version the file declares, e.g. "1.7" or "2.0" (see PdfUaTarget); null if it
      /// couldn't be read.
      PdfVersion: string | null
      /// Whether Profile was worked out from the file's PDF version (see PdfUaTarget.detect), or
      /// asked for. Validate reports false; a caller that detected it says so.
      ProfileDetected: bool
      /// True if the document complies with the requested profile.
      IsCompliant: bool
      /// Total number of individual rule checks performed.
      TotalAssertions: int
      /// veraPDF's failed checks, followed by Legibyl's own warnings. Passed veraPDF checks
      /// are only counted (TotalAssertions), not recorded.
      Assertions: IReadOnlyList<RuleViolation>
      /// What veraPDF logged while it read the file, WARNING and above; empty for none.
      Notes: IReadOnlyList<ValidatorNote> }

    /// Convenience accessor for only the failed assertions.
    member this.FailedAssertions: RuleViolation seq = this.Assertions |> Seq.filter (fun a -> not a.Passed)

    /// Failed assertions plus non-failing Legibyl warnings worth showing to the user.
    member this.Findings: RuleViolation seq = this.Assertions |> Seq.filter (fun a -> a.IsFinding)

    member this.FailedCount = this.Assertions |> Seq.filter (fun a -> not a.Passed) |> Seq.length

    /// How many findings (failures and warnings) a fix addresses.
    member this.FixableCount(option: PdfFixOptions) =
        this.Findings |> Seq.filter (fun a -> a.FixOptions.HasFlag option) |> Seq.length

    member this.WarningCount =
        this.Assertions |> Seq.filter (fun a -> a.Severity = FindingSeverity.Warning) |> Seq.length

/// The outcome of running selected best-effort fixes against a PDF.
type FixResult =
    { FilePath: string
      Profile: PdfProfile
      RequestedOptions: PdfFixOptions
      /// Path to the fixed copy, or null if no output file was produced
      /// (nothing needed fixing, or veraPDF determined it couldn't fix anything).
      OutputPath: string | null
      Status: RepairStatus
      AppliedFixes: IReadOnlyList<string>
      /// Requested fixes that were attempted but not kept, and why: e.g. veraPDF's metadata
      /// fixer answering Won't fix. Empty when every attempt either applied or had nothing to do.
      NotApplied: IReadOnlyList<string>
      /// The validation report for the original (unfixed) file, for reference.
      OriginalReport: ValidationReport }

    member this.WasFixed =
        not (isNull this.OutputPath)
        && (this.Status = RepairStatus.Success || this.Status = RepairStatus.IdRemoved)

/// Which part of the document a running validation is working through.
type ValidationStage =
    /// Reading the file and preparing, before the first page.
    | Preparing = 0
    /// Checking the pages, in order: see ValidationProgress.Page and PageCount.
    | CheckingPages = 1
    /// Past the pages: the structure tree and other document-wide objects.
    | Finishing = 2

/// How far a running validation has got. veraPDF checks the pages in order, which is
/// where nearly all the time goes, so Page of PageCount is a fair measure of progress.
/// It finds the other objects it checks as it goes, so QueuedObjects isn't a total.
type ValidationProgress =
    { Stage: ValidationStage
      /// The 1-based page being checked; 0 before the first page.
      Page: int
      PageCount: int
      /// Rule checks run so far.
      Checks: int
      /// Distinct rules that have failed at least once so far.
      FailedRules: int
      /// PDF objects checked so far.
      ProcessedObjects: int
      /// Objects found but not checked yet.
      QueuedObjects: int
      /// Time since validation started.
      Elapsed: TimeSpan }

/// Validates PDF documents against PDF/UA-1 or PDF/UA-2. Implementations
/// wrap the veraPDF Java validation engine (loaded via IKVM); callers never
/// need to reference IKVM or Java types directly.
type IPdfValidator =
    /// Validates the PDF at pdfPath against profile, reporting to progress (if given)
    /// several times a second while it runs. Throws VeraPdfValidationException if the
    /// file cannot be parsed (corrupt/invalid PDF, encrypted, IO error, etc).
    abstract Validate:
        pdfPath: string *
        profile: PdfProfile *
        [<Optional; DefaultParameterValue(null: IProgress<ValidationProgress> | null)>] progress: IProgress<ValidationProgress> | null ->
            ValidationReport

/// Which step of a fix is running.
type FixStage =
    /// Validating the original, page by page (see FixProgress.Page), to know what to fix.
    | CheckingOriginal = 0
    /// Tagging an untagged document from its layout, page by page.
    | Tagging = 1
    /// Legibyl's own structural fixes.
    | Fixing = 2
    /// veraPDF's metadata fix, which validates the document again as it goes.
    | FixingMetadata = 3

/// How far a fix has got.
type FixProgress =
    { Stage: FixStage
      /// This step's position among the steps this fix runs (1-based), and how many there are.
      Step: int
      Steps: int
      /// Checking the original or tagging: the 1-based page, and the page count; 0 otherwise.
      Page: int
      PageCount: int }

/// Fixes selected PDF/UA issues. Implementations keep veraPDF/IKVM
/// and PDF writer details behind this façade.
type IPdfFixer =
    /// Validates the PDF at pdfPath against profile, then attempts the selected fixes,
    /// writing a fixed copy to outputPath only if at least one fix changed the file.
    /// Throws VeraPdfValidationException if the file cannot be parsed (corrupt/invalid
    /// PDF, encrypted, IO error, etc).
    abstract Fix:
        pdfPath: string *
        profile: PdfProfile *
        outputPath: string *
        [<Optional; DefaultParameterValue(PdfFixOptions.Metadata)>] options: PdfFixOptions *
        [<Optional; DefaultParameterValue(null: IProgress<FixProgress> | null)>] progress: IProgress<FixProgress> | null ->
            FixResult

/// Which part of reading a structure tree is running.
type StructureReadStage =
    /// Extracting each page's tagged text, page by page: see StructureReadProgress.Page.
    | ReadingText = 0
    /// Finding where link annotations point.
    | ReadingLinks = 1
    /// Walking the structure tree itself.
    | BuildingTree = 2

/// How far reading a structure tree has got. Reading the pages' text is most of the time.
type StructureReadProgress =
    { Stage: StructureReadStage
      /// The 1-based page whose text is being read; 0 outside ReadingText.
      Page: int
      PageCount: int
      /// Structure elements walked so far (BuildingTree).
      Elements: int
      /// Time since reading started.
      Elapsed: TimeSpan }

/// Reads a tagged PDF's logical structure tree — the semantic hierarchy (headings,
/// paragraphs, lists, tables, links, etc.) that screen readers use to present the
/// document. Implementations wrap veraPDF's Java PD model (loaded via IKVM).
type IStructureTreeReader =
    /// Reads the structure tree of the PDF at pdfPath, rooted at its StructTreeRoot, or
    /// returns null if the document has no StructTreeRoot at all (i.e. it is completely
    /// untagged), reporting to progress (if given) as it goes. Throws
    /// VeraPdfValidationException if the file cannot be parsed.
    abstract ReadStructureTree:
        pdfPath: string *
        [<Optional; DefaultParameterValue(null: IProgress<StructureReadProgress> | null)>] progress: IProgress<StructureReadProgress> | null ->
            StructureNode | null
