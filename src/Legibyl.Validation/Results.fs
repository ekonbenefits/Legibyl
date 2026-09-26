namespace Legibyl.Validation

open System
open System.Collections.Generic
open System.Runtime.InteropServices

/// Plain-language descriptions of the repairs, worded like the GUI's Fix dialog.
module PdfFixDescriptions =
    /// One description per option set in options, e.g. "Remove empty /P structure elements".
    let Describe (options: PdfFixOptions) : string list =
        [ PdfFixOptions.TagFromLayout, "Tag an untagged document from each page's layout"
          PdfFixOptions.Metadata, "Fix XMP metadata"
          PdfFixOptions.AnnotationTabs, "Set /Tabs /S on pages that contain annotations"
          PdfFixOptions.RemoveEmptyParagraphs, "Remove empty /P structure elements"
          PdfFixOptions.RemoveEmptyTocEntries, "Remove empty table-of-contents entries (TOCI)"
          PdfFixOptions.LinkContents, "Describe table-of-contents links from their own text (/Contents)"
          PdfFixOptions.HeadingsFromToc, "Tag headings the table of contents lists (H1–H6) and point its links at them"
          PdfFixOptions.ListsFromParagraphs, "Tag paragraphs that start with list markers as a list (L/LI)"
          PdfFixOptions.BookmarksFromHeadings, "Add bookmarks for the H1 and H2 headings"
          PdfFixOptions.DocumentTitle, "Set a document title and show it in the title bar"
          PdfFixOptions.TitleFromFirstPage, "Title an untitled document from its title page's text" ]
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
      /// Best-effort repair options that can address this finding, if any.
      FixOptions: PdfFixOptions
      /// The page region to highlight for this finding, if known. veraPDF findings leave
      /// this null (annotation regions come from Location instead); Legibyl's structure
      /// warnings set it from the offending element's content.
      Geometry: StructureGeometry | null }

    /// A short "clause.testNumber" identifier, e.g. "7.1-2".
    member this.RuleId = $"{this.Clause}-{this.TestNumber}"

    /// Best-effort, human-readable interpretation of LocationContext (page number +
    /// plain-English breadcrumb). Null if there's no location context to parse.
    member this.Location = PdfLocationParser.Parse this.LocationContext

    member this.IsFinding = not this.Passed || this.Severity = FindingSeverity.Warning

    member this.IsFixable = this.FixOptions <> PdfFixOptions.None

    /// What the available repairs do, e.g. "Remove empty /P structure elements"; empty if none.
    member this.FixDescription = String.Join("; ", PdfFixDescriptions.Describe this.FixOptions)

    override this.ToString() = $"[{this.Specification} {this.RuleId}] {this.Message}"

/// The result of validating a single PDF document against a PdfProfile.
type ValidationReport =
    { FilePath: string
      Profile: PdfProfile
      /// The flavour id actually reported by veraPDF, e.g. "ua1".
      FlavourId: string
      /// True if the document complies with the requested profile.
      IsCompliant: bool
      /// Total number of individual rule checks performed.
      TotalAssertions: int
      /// veraPDF's failed checks, followed by Legibyl's own warnings. Passed veraPDF checks
      /// are only counted (TotalAssertions), not recorded.
      Assertions: IReadOnlyList<RuleViolation> }

    /// Convenience accessor for only the failed assertions.
    member this.FailedAssertions: RuleViolation seq = this.Assertions |> Seq.filter (fun a -> not a.Passed)

    /// Failed assertions plus non-failing Legibyl warnings worth showing to the user.
    member this.Findings: RuleViolation seq = this.Assertions |> Seq.filter (fun a -> a.IsFinding)

    member this.FailedCount = this.Assertions |> Seq.filter (fun a -> not a.Passed) |> Seq.length

    /// How many findings (failures and warnings) a repair addresses.
    member this.FixableCount(option: PdfFixOptions) =
        this.Findings |> Seq.filter (fun a -> a.FixOptions.HasFlag option) |> Seq.length

    member this.WarningCount =
        this.Assertions |> Seq.filter (fun a -> a.Severity = FindingSeverity.Warning) |> Seq.length

/// The outcome of running selected best-effort repairs against a PDF.
type FixResult =
    { FilePath: string
      Profile: PdfProfile
      RequestedOptions: PdfFixOptions
      /// Path to the repaired copy, or null if no output file was produced
      /// (nothing needed fixing, or veraPDF determined it couldn't fix anything).
      OutputPath: string | null
      Status: RepairStatus
      AppliedFixes: IReadOnlyList<string>
      /// Requested repairs that were attempted but not kept, and why: e.g. veraPDF's metadata
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

/// Validates PDF documents against PDF/A or PDF/UA profiles. Implementations
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

/// Repairs selected PDF/A or PDF/UA issues. Implementations keep veraPDF/IKVM
/// and PDF writer details behind this façade.
type IPdfFixer =
    /// Validates the PDF at pdfPath against profile, then attempts the selected fixes,
    /// writing a repaired copy to outputPath only if at least one fix changed the file.
    /// Throws VeraPdfValidationException if the file cannot be parsed (corrupt/invalid
    /// PDF, encrypted, IO error, etc).
    abstract Fix:
        pdfPath: string *
        profile: PdfProfile *
        outputPath: string *
        [<Optional; DefaultParameterValue(PdfFixOptions.Metadata)>] options: PdfFixOptions ->
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
