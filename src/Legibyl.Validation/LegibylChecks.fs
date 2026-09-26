/// Legibyl's own accessibility checks — problems veraPDF doesn't report, surfaced as
/// warnings alongside its findings (Specification "Legibyl", Clause "Structure").
///
/// To add a check:
///   1. Write an `inspect` function that looks at one structure element (a
///      StructElemVisit: the element, its path, page, and next sibling) and returns
///      Some finding when something's wrong. Helpers for text, emptiness, and geometry
///      are in StructureTreeObjects.
///   2. Add it to `all` below with the next unused TestNumber (numbers are shown to
///      users as "Structure-N", so never reuse or renumber one).
///   3. If the Fix dialog can repair it, set FixOptions and implement the repair in
///      StructureFixes.
///   4. Cover it in tests/Legibyl.Validation.Tests/PdfFixerTests.fs.
///
/// Checks that look at the document as a whole rather than one element at a time (like
/// the table-of-contents heading checks, Structure-3 to 5, lists tagged as paragraphs,
/// Structure-6, and missing bookmarks, Structure-7) are added in `run` directly.
module internal Legibyl.Validation.LegibylChecks

open System
open System.Text.RegularExpressions
open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

/// Where a check found a problem.
type Finding =
    { /// Structure-tree path, e.g. "StructTreeRoot/K[0]/K[3]".
      Location: string
      /// Page region to highlight, if the offending content has one.
      Geometry: StructureGeometry option }

type Check =
    { TestNumber: int
      Message: string
      /// The Fix dialog option that repairs this finding, or None if it's warning-only.
      FixOptions: PdfFixOptions
      Inspect: Context -> StructElemVisit -> Finding option }

// ---- Structure-1: empty /P ----

let private emptyParagraph context (visit: StructElemVisit) =
    if isParagraph visit.Element context && isSubtreeEmpty visit.Element context visit.Page then
        Some
            { Location = visit.Path
              Geometry = subtreeGeometry visit.Element context visit.Page }
    else
        None

// ---- Structure-8: empty TOCI ----

let private emptyTocEntry context (visit: StructElemVisit) =
    if isTocItem visit.Element context && isSubtreeEmpty visit.Element context visit.Page then
        Some
            { Location = visit.Path
              Geometry = subtreeGeometry visit.Element context visit.Page }
    else
        None

// ---- Structure-2: likely split paragraph ----

let private splitParagraph context (visit: StructElemVisit) =
    match visit.NextSibling with
    | Some next when isParagraph visit.Element context && isParagraph next.Element context ->
        let onDifferentPages = visit.Page.IsSome && next.Page.IsSome && visit.Page <> next.Page
        let currentText = ParagraphJoins.normalizeText (extractText visit.Element context visit.Page)
        let nextText = ParagraphJoins.normalizeText (extractText next.Element context next.Page)
        if not onDifferentPages && ParagraphJoins.looksSplit currentText nextText then
            Some
                { Location = $"{visit.Path} + {next.Path}"
                  Geometry =
                    [ subtreeGeometry visit.Element context visit.Page
                      subtreeGeometry next.Element context next.Page ]
                    |> List.choose id
                    |> unionGeometry }
        else
            None
    | _ -> None

// ---- The checks ----

let all: Check list =
    [ { TestNumber = 1
        Message =
          "Empty semantic /P structure element. It can add meaningless stops to the screen-reader structure tree and can be removed by the Fix dialog."
        FixOptions = PdfFixOptions.RemoveEmptyParagraphs
        Inspect = emptyParagraph }
      { TestNumber = 2
        Message =
          "Likely split paragraph: adjacent /P structure elements look like visual line fragments from the same sentence. This is best fixed in the source document if possible; the Fix dialog can join them where the page layout shows one block of text."
        FixOptions = PdfFixOptions.JoinSplitParagraphs
        Inspect = splitParagraph }
      { TestNumber = 8
        Message =
          "Empty table-of-contents entry (TOCI). It adds a meaningless stop when a screen reader moves through the table of contents and can be removed by the Fix dialog."
        FixOptions = PdfFixOptions.RemoveEmptyTocEntries
        Inspect = emptyTocEntry } ]

let private warning message (errorMessage: string | null) location testNumber fixOptions geometry =
    { Specification = "Legibyl"
      Clause = "Structure"
      TestNumber = testNumber
      Passed = true
      Message = message
      ErrorMessage = errorMessage
      LocationContext = location
      Severity = FindingSeverity.Warning
      FixOptions = fixOptions
      Geometry = geometry }

// ---- Structure-3 to 5: headings from the table of contents (see TocHeadings) ----

[<Literal>]
let private TocHeadingMessage =
    "Paragraph that the table of contents lists as a heading. Tagged as a heading, screen reader users could jump to it; the Fix dialog can retag it."

[<Literal>]
let private TocEntryUnmatchedMessage =
    "Table-of-contents entry that couldn't be matched to exactly one paragraph on its page, so its heading wasn't recovered."

[<Literal>]
let private TocHeadingsUnrecoverableMessage = "Headings weren't recovered from the table of contents."

let private tocHeadingFindings (document: PdfDocument) context =
    let unmatchedWarning (unmatched: TocHeadings.Unmatched) =
        let found = if unmatched.Matches = 0 then "no matching paragraph" else $"{unmatched.Matches} matching paragraphs"
        warning
            TocEntryUnmatchedMessage
            $"\"{unmatched.Entry.Label}\" goes to page {unmatched.Entry.Page}, which has {found}."
            unmatched.Entry.Path
            4
            PdfFixOptions.None
            (Option.toObj unmatched.Entry.Geometry)
    match TocHeadings.analyse document context with
    | TocHeadings.NotApplicable -> []
    | TocHeadings.Recoverable(headings, unmatched) ->
        [ for heading in headings ->
              warning
                  TocHeadingMessage
                  $"\"{heading.Entry.Label}\" is a level-{heading.Level} entry, so this would become H{heading.Level}."
                  heading.ParagraphPath
                  3
                  PdfFixOptions.HeadingsFromToc
                  (Option.toObj heading.Geometry)
          yield! List.map unmatchedWarning unmatched ]
    | TocHeadings.Unrecoverable(tocPath, tocGeometry, reason, unmatched) ->
        [ warning TocHeadingsUnrecoverableMessage $"Not recovered because {reason}." tocPath 5 PdfFixOptions.None (Option.toObj tocGeometry)
          yield! List.map unmatchedWarning unmatched ]

// ---- Structure-6: lists tagged as plain paragraphs (see ParagraphLists) ----

[<Literal>]
let private ParagraphListMessage =
    "Looks like a list tagged as plain paragraphs: consecutive paragraphs start with list markers, so screen readers won't announce them as a list. The Fix dialog can tag them as one."

let private paragraphListFindings (document: PdfDocument) context =
    [ for run in ParagraphLists.find document context ->
          warning
              ParagraphListMessage
              $"{run.Items.Length} paragraphs on page {run.Page}, marked \"{run.FirstMarker}\" to \"{run.LastMarker}\"."
              run.Path
              6
              PdfFixOptions.ListsFromParagraphs
              (Option.toObj run.Geometry) ]

// ---- Structure-7: headings but no bookmarks (see Bookmarks) ----

[<Literal>]
let private MissingBookmarksMessage =
    "The document has headings but no bookmarks, so readers can't jump between its sections from the navigation pane. The Fix dialog can add them for the H1 and H2 headings."

let private missingBookmarkFindings (document: PdfDocument) context =
    match Bookmarks.find document context with
    | None -> []
    | Some missing ->
        let bookmarkable = missing.Bookmarkable.Length
        let detail = $"{missing.HeadingCount} heading(s), {bookmarkable} of them H1 or H2 with text to bookmark."
        let fixable = bookmarkable > 0 && missing.LanguageDeclared
        [ warning
              MissingBookmarksMessage
              (if bookmarkable > 0 && not missing.LanguageDeclared then
                   $"{detail} Declare the document's language (/Lang) first: bookmark text needs one."
               else
                   detail)
              missing.FirstHeadingPath
              7
              (if fixable then PdfFixOptions.BookmarksFromHeadings else PdfFixOptions.None)
              null ]

/// Runs every check in `all` over the document's structure tree, then the document-wide ones.
let run (document: PdfDocument) context : RuleViolation list =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> []
    | structTreeRoot ->
        let visits = walk structTreeRoot context |> List.ofSeq
        [ for check in all do
              for visit in visits do
                  match check.Inspect context visit with
                  | Some finding ->
                      warning check.Message null finding.Location check.TestNumber check.FixOptions (Option.toObj finding.Geometry)
                  | None -> ()
          yield! tocHeadingFindings document context
          yield! paragraphListFindings document context
          yield! missingBookmarkFindings document context ]

/// Reported instead of the checks' results when the PDF can't be opened to run them.
let couldNotInspect (errorMessage: string) =
    warning "Could not inspect the structure tree for Legibyl's structure checks." errorMessage "StructTreeRoot" 0 PdfFixOptions.None null
