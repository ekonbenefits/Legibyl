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
///   4. Cover it in tests/Legibyl.Validation.Tests/PdfFixerTests.cs.
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

// ---- Structure-2: likely split paragraph ----

let private normalizeText (text: string) = Regex.Replace(text, @"\s+", " ").Trim()

let private isLikelyStandaloneParagraph (text: string) =
    text.Length <= 3
    || Regex.IsMatch(text, @"^(SECTION|ARTICLE)\b", RegexOptions.IgnoreCase)
    || Regex.IsMatch(text, @"^\d+(\.\d+)*\s+\S+")
    || Regex.IsMatch(text, @"^[a-z]\.\s+\S+", RegexOptions.IgnoreCase)

let private isLikelySplitParagraph (currentText: string) (nextText: string) =
    let startsWith (prefix: string) = nextText.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)

    currentText.Length >= 20
    && nextText.Length >= 10
    && not (isLikelyStandaloneParagraph currentText || isLikelyStandaloneParagraph nextText)
    && not (".:;!?…".Contains currentText[currentText.Length - 1])
    && (Char.IsLower nextText[0]
        || startsWith "and "
        || startsWith "or "
        || startsWith "to "
        || startsWith "of "
        || startsWith "the ")

let private splitParagraph context (visit: StructElemVisit) =
    match visit.NextSibling with
    | Some next when isParagraph visit.Element context && isParagraph next.Element context ->
        let onDifferentPages = visit.Page.IsSome && next.Page.IsSome && visit.Page <> next.Page
        let currentText = normalizeText (extractText visit.Element context visit.Page)
        let nextText = normalizeText (extractText next.Element context next.Page)
        if not onDifferentPages && isLikelySplitParagraph currentText nextText then
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
          "Likely split paragraph: adjacent /P structure elements look like visual line fragments from the same sentence. This is best fixed in the source document if possible."
        FixOptions = PdfFixOptions.None
        Inspect = splitParagraph } ]

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

/// Runs every check in `all` over the document's structure tree.
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
                  | None -> () ]

/// Reported instead of the checks' results when the PDF can't be opened to run them.
let couldNotInspect (errorMessage: string) =
    warning "Could not inspect the structure tree for Legibyl's structure checks." errorMessage "StructTreeRoot" 0 PdfFixOptions.None null
