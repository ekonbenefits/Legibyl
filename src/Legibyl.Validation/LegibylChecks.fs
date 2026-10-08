/// Legibyl's own accessibility checks — problems veraPDF doesn't report, surfaced as
/// warnings alongside its findings (Specification "Legibyl", Clause "Structure").
///
/// To add a check:
///   1. Write an `inspect` function that looks at one structure element (a
///      StructElemVisit: the element, its path, page, and next sibling) and returns
///      Some finding when something's wrong. Helpers for text, emptiness, and geometry
///      are in StructureTreeObjects.
///   2. Add it to `all` below with the next unused TestNumber (numbers are shown to
///      users as "Structure-N", so never reuse or renumber one). Its message is fixed (what
///      varies goes in the warning's detail, errorMessage), so a saved report reads it back.
///   3. If an auto fix can fix it, set FixOptions and implement the fix (in
///      StructureFixes, or a module of its own that StructureFixes.orderedFixes runs).
///   4. Cover it in a test file of its own in tests/Legibyl.Validation.Tests (as
///      EmptyTocEntriesTests.fs, ParentTreeTests.fs and EmptyFiguresTests.fs do), and add it to
///      the README's table of checks.
///
/// Checks that look at the document as a whole rather than one element at a time (the
/// table-of-contents heading checks, Structure-3 to 5; lists tagged as paragraphs, Structure-6;
/// missing bookmarks, Structure-7; the parent tree, Structure-9; figures in pieces, Structure-10;
/// figures that draw nothing, Structure-11; pages read out of order, Structure-12; pages whose
/// text couldn't be read, Structure-13; words split across Span boundaries, Structure-14; tables
/// tagged as aligned paragraphs, Structure-15; no single Document tag at the top, Structure-16;
/// and content tied to a tag that's gone, Structure-17) are added in `run` directly, and their
/// messages to messageFor.
///
/// The whole list, for a check and for a fix (every place each is wired in, and the tests), is
/// the add-check skill: .claude/skills/add-check/SKILL.md.
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
      /// The auto fix that fixes this finding, or None if it's warning-only.
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

// ---- Structure-14: a word split across adjacent Span tags ----

[<Literal>]
let private SplitWordSpansMessage =
    "Adjacent Span tags split a word inside this text block. This is not a PDF/UA failure, but structure-based text extraction may present fragments separately. An auto fix can set the block's replacement text when every span's text is recoverable and none carries a language, alt text or other attributes it would hide."

// ---- Structure-15: paragraphs arranged like table cells ----

[<Literal>]
let private ParagraphTableMessage =
    "These paragraphs appear arranged in repeated rows and columns. They may be a table tagged as paragraphs, so screen reader users may not hear the relationships between values. Check the page and tag it as a table if appropriate; this layout-based warning can also match aligned text."

// ---- The checks ----

let all: Check list =
    [ { TestNumber = 1
        Message =
          "Empty paragraph tag. A screen reader stops on it with nothing to read; an auto fix can remove it."
        FixOptions = PdfFixOptions.EmptyParagraphs
        Inspect = emptyParagraph }
      { TestNumber = 2
        Message =
          "Likely split paragraph: neighbouring paragraph tags look like lines of the same sentence, so a screen reader reads one sentence as several paragraphs. Best fixed where the PDF was made; an auto fix can join them where the page shows one block of text, or in Structure…, shift-click the paragraphs and Merge them."
        FixOptions = PdfFixOptions.JoinSplitParagraphs
        Inspect = splitParagraph }
      { TestNumber = 8
        Message =
          "Empty table-of-contents entry (TOCI). It adds a meaningless stop when a screen reader moves through the table of contents and can be removed as an auto fix."
        FixOptions = PdfFixOptions.EmptyTocEntries
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
      Geometry = geometry
      Element = null }

// ---- Structure-3 to 5: headings from the table of contents (see TocHeadings) ----

[<Literal>]
let private TocHeadingMessage =
    "Paragraph that the table of contents lists as a heading. Tagged as a heading, screen reader users could jump to it; an auto fix can retag it."

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
    "Looks like a list tagged as plain paragraphs: consecutive paragraphs start with list markers, so screen readers won't announce them as a list. An auto fix can tag them as one."

let private paragraphListFindings (document: PdfDocument) context =
    [ for run in ParagraphLists.find document context ->
          warning
              ParagraphListMessage
              $"{run.Items.Length} paragraphs on page {run.Page}, marked \"{run.FirstMarker}\" to \"{run.LastMarker}\"."
              run.Path
              6
              PdfFixOptions.Lists
              (Option.toObj run.Geometry) ]

// ---- Structure-7: headings but no bookmarks (see Bookmarks) ----

[<Literal>]
let private MissingBookmarksMessage =
    "The document has headings but no bookmarks, so readers can't jump between its sections from the navigation pane. An auto fix can add them for the H1 and H2 headings."

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
              (if fixable then PdfFixOptions.Bookmarks else PdfFixOptions.None)
              null ]

// ---- Structure-9: the parent tree is missing or incomplete (see ParentTree) ----

[<Literal>]
let private ParentTreeMessage =
    "The tags exist but the PDF doesn't say which page content each covers (its parent tree is missing or incomplete), so the tags read as untagged. An auto fix can rebuild it from the tags."

let private parentTreeFindings (document: PdfDocument) context =
    match ParentTree.find document context with
    | None -> []
    | Some missing ->
        let pages = if missing.Pages = 1 then "1 page with tagged content has" else $"{missing.Pages} pages with tagged content have"
        let objects = if missing.Objects = 1 then "1 tagged annotation has" else $"{missing.Objects} tagged annotations have"
        // Content tied to a tag that's gone is Structure-17's, not this one's.
        let wrong =
            ParentTree.inconsistent document context |> List.filter (fun (_, _, _, expected) -> expected.IsSome) |> List.length
        let detail =
            [ if missing.TreeMissing then "The parent tree is missing."
              if missing.Pages > 0 then $"{pages} missing or incorrect entries in it."
              if missing.Objects > 0 then $"{objects} missing or incorrect entries in it."
              if wrong > 0 then $"{wrong} piece(s) of content are tied to the wrong tag." ]
            |> String.concat " "
        if missing.TreeMissing || missing.Pages > 0 || missing.Objects > 0 || wrong > 0 then
            [ warning ParentTreeMessage detail "StructTreeRoot" 9 PdfFixOptions.ParentTree null ]
        else
            []

// ---- Structure-17: content tied to a tag that isn't in the tags any more ----

[<Literal>]
let private DisconnectedMessage =
    "Content still tied to a tag that isn't in the tags any more (one taken out without its parent tree entries), so a screen reader skips it or reads it out of place. Structure… shows it on its own, under Tied to tags that are gone."

/// Each page's content tied to a tag that's gone, by that tag: the fix offered only where it acts.
let private disconnectedFindings (document: PdfDocument) context =
    [ for gone in ParentTree.disconnected document context do
          let geometry =
              gone.Mcids
              |> Seq.choose (fun mcid ->
                  match context.MarkedContent.TryGetValue((gone.Page, mcid)) with
                  | true, content -> content.Geometry
                  | _ -> None)
              |> unionGeometry
              |> Option.toObj
          let tag = match gone.Owner.GetAsName PdfName.S with null -> "a tag" | name -> $"<{name.GetValue()}>"
          let what =
              if gone.Artifact then
                  "Its tag said it was decoration, so Rebuild the parent tree unties it and marks it as not read."
              elif gone.Untied then
                  "It draws nothing, so Rebuild the parent tree unties it and marks it as not read."
              else
                  "It draws something or has a description, so it's kept: its tag needs putting back where the PDF was made, or in a PDF editor."
          warning
              DisconnectedMessage
              $"Page {gone.Page}: content tied to {tag}, which isn't in the tags. {what}"
              "StructTreeRoot"
              17
              (if gone.Untied then PdfFixOptions.ParentTree else PdfFixOptions.None)
              geometry ]

// ---- Structure-10: a picture tagged in pieces, as many small figures (see SplitFigures) ----

[<Literal>]
let private SplitFiguresMessage =
    "Small figures side by side look like one picture drawn in pieces: a screen reader announces each, and each needs alt text. In Structure…, shift-click them and Merge N Figures, then describe it once, or give it the tag Artifact (not read) if it's decoration."

let private splitFigureFindings (document: PdfDocument) context =
    [ for run in SplitFigures.find document context ->
          warning
              SplitFiguresMessage
              $"{run.Join.Indexes.Length} figures side by side on page {run.Join.Page}."
              run.Path
              10
              PdfFixOptions.None
              run.Box ]

// ---- Structure-11: a figure that draws nothing (see EmptyFigures) ----

[<Literal>]
let private EmptyFigureMessage =
    "Figure tag around nothing on the page: a screen reader announces an image that isn't there, and it fails for having no alt text though there's nothing to describe. An auto fix can remove it."

let private emptyFigureFindings (document: PdfDocument) context =
    [ for visit in EmptyFigures.visits document context ->
          let detail = visit.Page |> Option.map (fun page -> $"On page {page}.") |> Option.toObj
          warning EmptyFigureMessage detail visit.Path 11 PdfFixOptions.EmptyFigures null ]

// ---- Structure-12: a page read out of order, by a big jump ----

[<Literal>]
let private ReadingJumpMessage =
    "A page's tags aren't in the order the page reads: something that spans the page (a masthead, a heading over two columns) is read after content below it. In Structure…, select a block on that page: under Page n, Order Page n by Layout puts it in its layout's order, or Move Up and Move Down (Alt+Up, Alt+Down) change it by hand."

/// The document's top-level elements: those in its one Document (or Part, Art, Sect, Div), else
/// the structure tree's own.
let private topLevel (structTreeRoot: PdfDictionary) context =
    let elementsIn (kids: PdfObject | null) =
        match resolve kids with
        | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do match resolve (array.Get i) with :? PdfDictionary as d when not (isNull (d.GetAsName PdfName.S)) -> yield d | _ -> () ]
        | :? PdfDictionary as d when not (isNull (d.GetAsName PdfName.S)) -> [ d ]
        | _ -> []
    let wrappers = set [ "Document"; "Part"; "Art"; "Sect"; "Div" ]
    match elementsIn (structTreeRoot.Get PdfName.K) with
    | [ only ] when wrappers.Contains(standardTag only context |> Option.ofObj |> Option.map _.GetValue() |> Option.defaultValue "") ->
        elementsIn (only.Get PdfName.K)
    | elements -> elements

/// For each page, the first element that spans columns (it's across two elements side by side,
/// above them) but is read after something below it, under it, with the first such thing. Reading
/// the right column after the left isn't one; running headers and footers (short, in the top or
/// bottom tenth of the page) aren't counted, either way.
let private readingJumps (document: PdfDocument) context =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> []
    | structTreeRoot ->
        let elements =
            [ for element in topLevel structTreeRoot context do
                  match subtreeGeometry element context (ancestorPage element context) with
                  | Some box -> yield element, box
                  | None -> () ]
        [ for page, onPage in elements |> List.groupBy (fun (_, box) -> box.PageNumber) do
              let size = document.GetPage(page).GetPageSize()
              let bottom, top, height = float (size.GetBottom()), float (size.GetTop()), float (size.GetHeight())
              let furniture (box: StructureGeometry) =
                  box.Top - box.Bottom < 0.1 * height && (box.Bottom > top - 0.1 * height || box.Top < bottom + 0.1 * height)
              let content = onPage |> List.filter (snd >> furniture >> not)
              let sideBySide (a: StructureGeometry) (b: StructureGeometry) =
                  (a.Right <= b.Left || b.Right <= a.Left) && min a.Top b.Top > max a.Bottom b.Bottom
              let across (wide: StructureGeometry) (narrow: StructureGeometry) = wide.Left < narrow.Right && narrow.Left < wide.Right
              let spans (index: int) (box: StructureGeometry) =
                  let others = content |> List.indexed |> List.filter (fun (i, _) -> i <> index) |> List.map (snd >> snd)
                  others
                  |> List.exists (fun a -> others |> List.exists (fun b -> sideBySide a b && across box a && across box b && box.Top >= max a.Top b.Top))
              let jump =
                  content
                  |> List.indexed
                  |> List.tryPick (fun (index, (element, box)) ->
                      if spans index box then
                          content
                          |> List.take index
                          |> List.tryFind (fun (_, earlier) -> earlier.Top <= box.Bottom && across box earlier)
                          |> Option.map (fun (earlier, _) -> element, box, earlier)
                      else None)
              match jump with
              | Some jump -> yield page, jump
              | None -> () ]

let private readingJumpFindings (document: PdfDocument) context =
    let quoted (element: PdfDictionary) (page: int) =
        match (extractText element context (Some page)).Trim() |> fun t -> Regex.Replace(t, @"\s+", " ") with
        | "" ->
            let tag = standardTag element context |> Option.ofObj |> Option.map _.GetValue() |> Option.defaultValue "element"
            $"the <{tag}>"
        | text when text.Length > 40 -> $"\"{text.Substring(0, 40).TrimEnd()}…\""
        | text -> $"\"{text}\""
    let pathOf =
        match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
        | null -> fun _ -> None
        | root ->
            let visits = walk root context |> List.ofSeq
            fun (element: PdfDictionary) -> visits |> List.tryFind (fun v -> obj.ReferenceEquals(v.Element, element)) |> Option.map _.Path
    [ for page, (element, box, earlier) in readingJumps document context ->
          warning
              ReadingJumpMessage
              $"Page {page}: {quoted element page} spans the page but comes after {quoted earlier page}, below it."
              (pathOf element |> Option.defaultValue "StructTreeRoot")
              12
              PdfFixOptions.None
              box ]

// ---- Structure-16: no single Document tag at the top ----

[<Literal>]
let private DocumentRootMessage =
    "The tags don't start with a single Document tag, a screen reader's way into them, which checkers like PAC look for; an auto fix can put them under one."

/// A PDF before 2.0 whose structure doesn't start with a single Document tag. (A PDF 2.0 file's is a
/// failure veraPDF reports: PDF/UA-2's 8.2.5.2.)
let private documentRootFindings (document: PdfDocument) context =
    if document.GetPdfVersion().CompareTo(PdfVersion.PDF_2_0) >= 0 then
        []
    else
        match DocumentRoot.find document context with
        | Some(DocumentRoot.Wrap tags) ->
            [ warning DocumentRootMessage $"""At the top: {String.Join(", ", tags)}.""" "StructTreeRoot" 16 PdfFixOptions.DocumentRoot null ]
        | Some(DocumentRoot.Left reason) ->
            [ warning DocumentRootMessage $"Not fixable here: {reason}." "StructTreeRoot" 16 PdfFixOptions.None null ]
        | _ -> []

// ---- Structure-13: pages whose text couldn't be read ----

[<Literal>]
let private UnreadableTextMessage = "The text of some pages couldn't be read, so Legibyl's checks of what they say skipped them."

/// Pages whose text couldn't be read: the checks that go by what a page says skipped them.
let private unreadableTextFindings context =
    match context.MarkedContent.Unreadable with
    | pages when pages.Count > 0 ->
        let reason = context.MarkedContent.UnreadableReason |> Option.map (fun r -> $": {r}") |> Option.defaultValue ""
        [ warning
              UnreadableTextMessage
              $"Page(s) {pageList pages}{reason}"
              "StructTreeRoot"
              13
              PdfFixOptions.None
              null ]
    | _ -> []

/// Runs every check in `all` over the document's structure tree, then the document-wide ones.
let run (document: PdfDocument) context : RuleViolation list =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> []
    | structTreeRoot ->
        let visits = walk structTreeRoot context |> List.ofSeq
        // An empty tag the fix would keep (it draws something, or has a description) is still
        // warned of, but without offering the fix, which would do nothing.
        let discardable = lazy (ParentTree.discardable document context)
        let pruned = PdfFixOptions.EmptyParagraphs ||| PdfFixOptions.EmptyTocEntries
        [ for check in all do
              for visit in visits do
                  match check.Inspect context visit with
                  | Some finding when pruned.HasFlag check.FixOptions && check.FixOptions <> PdfFixOptions.None
                                      && not (discardable.Value visit.Element visit.Page) ->
                      warning
                          check.Message
                          "The fix leaves this one: it draws something, has a description, or its content isn't on the page: check it in Structure…"
                          finding.Location
                          check.TestNumber
                          PdfFixOptions.None
                          (Option.toObj finding.Geometry)
                  | Some finding ->
                      warning check.Message null finding.Location check.TestNumber check.FixOptions (Option.toObj finding.Geometry)
                  | None -> ()
          yield! parentTreeFindings document context
          yield! disconnectedFindings document context
          yield! tocHeadingFindings document context
          yield! paragraphListFindings document context
          yield! splitFigureFindings document context
          yield! emptyFigureFindings document context
          yield! readingJumpFindings document context
          yield! missingBookmarkFindings document context
          yield! unreadableTextFindings context
          for candidate in SplitWordSpans.find context visits do
              let page = candidate.Page |> Option.map string |> Option.defaultValue "unknown"
              warning
                  SplitWordSpansMessage
                  $"Page {page}: \"{candidate.Text}\"."
                  candidate.Path
                  14
                  (if candidate.Fixable then PdfFixOptions.JoinSplitWords else PdfFixOptions.None)
                  (Option.toObj candidate.Geometry)
          for candidate in ParagraphTables.find context visits do
              warning
                  ParagraphTableMessage
                  $"Page {candidate.Page}: {candidate.ParagraphCount} paragraphs in a repeated-row layout."
                  candidate.Path
                  15
                  PdfFixOptions.None
                  (Option.toObj candidate.Geometry)
          yield! documentRootFindings document context ]

/// Each check's message, by its TestNumber (as a saved validation reads it back).
let messageFor (testNumber: int) : string option =
    match all |> List.tryFind (fun check -> check.TestNumber = testNumber) with
    | Some check -> Some check.Message
    | None ->
        match testNumber with
        | 0 -> Some "Could not inspect the structure tree for Legibyl's structure checks."
        | 3 -> Some TocHeadingMessage
        | 4 -> Some TocEntryUnmatchedMessage
        | 5 -> Some TocHeadingsUnrecoverableMessage
        | 6 -> Some ParagraphListMessage
        | 7 -> Some MissingBookmarksMessage
        | 9 -> Some ParentTreeMessage
        | 10 -> Some SplitFiguresMessage
        | 11 -> Some EmptyFigureMessage
        | 12 -> Some ReadingJumpMessage
        | 13 -> Some UnreadableTextMessage
        | 14 -> Some SplitWordSpansMessage
        | 15 -> Some ParagraphTableMessage
        | 16 -> Some DocumentRootMessage
        | 17 -> Some DisconnectedMessage
        | _ -> None

/// Reported instead of the checks' results when the PDF can't be opened to run them.
let couldNotInspect (errorMessage: string) =
    warning "Could not inspect the structure tree for Legibyl's structure checks." errorMessage "StructTreeRoot" 0 PdfFixOptions.None null
