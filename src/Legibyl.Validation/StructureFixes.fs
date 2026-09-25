/// Structural repairs that work on the raw PDF objects via iText: removing empty /P
/// elements, setting annotation tab order, describing links, and naming the document.
/// The warnings that point users at these problems live in LegibylChecks.
module internal Legibyl.Validation.StructureFixes

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open iText.IO.Font
open iText.Kernel.Pdf
open iText.Kernel.XMP
open Legibyl.Validation.Interop
open Legibyl.Validation.StructureTreeObjects

/// Removes, below dictionary, every element isTarget accepts that has nothing visible below it
/// (empty /P, empty TOCI); returns how many were removed.
let rec private removeEmptyInside isTarget (dictionary: PdfDictionary) context inheritedPage : int =
    match dictionary.Get(PdfName.K) with
    | null -> 0
    | kids ->
        let pageNumber = structElemPage dictionary context inheritedPage
        match resolve kids with
        // A single child: shouldRemoveEmpty has already cleaned up inside it, so keep its
        // count (the array case below counts its own children instead).
        | :? PdfDictionary ->
            match shouldRemoveEmpty isTarget kids context pageNumber with
            | true, removedFromChild ->
                dictionary.Remove(PdfName.K) |> ignore
                removedFromChild + 1
            | false, removedFromChild -> removedFromChild
        | _ -> removeEmptyFromKids isTarget kids context pageNumber

and private removeEmptyFromKids isTarget (kids: PdfObject) context inheritedPage : int =
    match resolve kids with
    | :? PdfArray as array ->
        let mutable removed = 0
        for i in array.Size() - 1 .. -1 .. 0 do
            let remove, childRemoved = shouldRemoveEmpty isTarget (array.Get i) context inheritedPage
            if remove then
                array.Remove i
                removed <- removed + 1
            removed <- removed + childRemoved
        removed
    | :? PdfDictionary as dictionary -> removeEmptyInside isTarget dictionary context inheritedPage
    | _ -> 0

/// Cleans up obj's own children first, then reports whether obj itself is an empty element
/// isTarget accepts (plus how many were removed below it).
and private shouldRemoveEmpty isTarget (obj: PdfObject | null) context inheritedPage : bool * int =
    match resolve obj with
    | :? PdfDictionary as dictionary ->
        let pageNumber = structElemPage dictionary context inheritedPage
        let removedFromChildren = removeEmptyInside isTarget dictionary context pageNumber
        isTarget dictionary context && isSubtreeEmpty dictionary context pageNumber, removedFromChildren
    | _ -> false, 0

// ---- Link /Contents ----

/// A PdfString that survives non-ASCII text.
let private textString (value: string) =
    if value |> Seq.forall (fun c -> c < '\u0080') then PdfString value
    else PdfString(value, PdfEncodings.UNICODE_BIG)

let private isBlankString (value: PdfString | null) =
    match value with
    | null -> true
    | value -> String.IsNullOrWhiteSpace(value.ToUnicodeString())

/// The /Link elements inside a table of contents: anywhere below a TOC or TOCI, including
/// a nested entry's links and links wrapped in a Reference or Span. walk visits parents
/// before children (depth first), and each visit's Path extends its parent's, so the TOC
/// elements whose subtree we're in are exactly those on a stack of paths that prefix this one.
let private tocLinks (structTreeRoot: PdfDictionary) context =
    let openTocs = Stack<string>()
    [ for visit in walk structTreeRoot context do
          while openTocs.Count > 0 && not (visit.Path.StartsWith(openTocs.Peek() + "/", StringComparison.Ordinal)) do
              openTocs.Pop() |> ignore
          let tag = standardTag visit.Element context
          if PdfName.TOC.Equals tag || PdfName.TOCI.Equals tag then
              openTocs.Push visit.Path
          elif PdfName.Link.Equals tag && openTocs.Count > 0 then
              yield visit ]

/// Object numbers of the link annotations describeLinks would give a /Contents: those of
/// links in a table of contents. Lets validation mark only those findings as fixable.
let tocLinkAnnotationNumbers (document: PdfDocument) context : Set<int> =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> Set.empty
    | structTreeRoot ->
        set
            [ for visit in tocLinks structTreeRoot context do
                  for annotation in ownAnnotations visit.Element do
                      match annotation.GetIndirectReference() with
                      | null -> ()
                      | reference -> yield reference.GetObjNumber() ]

/// Gives the annotation of each link in a table of contents a /Contents taken from the
/// link's own text, so screen readers can announce where it goes. Returns how many were set.
///
/// Only there is a link's text known to name its destination ("Scope of this document
/// ..... 3"). Elsewhere it's often "click here" or "read more": writing that into /Contents
/// would make the failure stop reporting while screen reader users still hear nothing
/// useful, so links outside a table of contents are left failing for a person to describe.
let private describeLinks (structTreeRoot: PdfDictionary) context =
    let mutable described = 0
    for visit in tocLinks structTreeRoot context do
        match ownAnnotations visit.Element |> List.filter (fun a -> isBlankString (a.GetAsString PdfName.Contents)) with
        | [] -> ()
        | annotations ->
            match tocEntryText (extractText visit.Element context visit.Page) with
            | None -> ()
            | Some description ->
                for annotation in annotations do
                    annotation.Put(PdfName.Contents, textString description) |> ignore
                    described <- described + 1
    described

// ---- Document title ----

/// The document's XMP `dc:title`, if it has one. Best-effort: a malformed XMP packet
/// should not stop the rest of the repairs.
let private xmpTitle (document: PdfDocument) =
    try
        match document.GetXmpMetadata() with
        | null -> None
        | xmp ->
            match xmp.GetLocalizedText(XMPConst.NS_DC, "title", null, XMPConst.X_DEFAULT) with
            | null -> None
            | property ->
                match property.GetValue() with
                | NonNull value when not (String.IsNullOrWhiteSpace value) -> Some value
                | _ -> None
    with _ ->
        None

/// The most a first page can hold and still read as a title page rather than content.
let private titlePageMaxWords = 30
let private titlePageMaxLength = 200

/// The first page's text on one line, if the document has more pages than that and the
/// first is short enough to be a title page: a title, and perhaps a subtitle, author and
/// date. A one-page document's only page is its content, not a title page.
let titlePageText (document: PdfDocument) =
    if document.GetNumberOfPages() < 2 then
        None
    else
        let text =
            try
                iText.Kernel.Pdf.Canvas.Parser.PdfTextExtractor.GetTextFromPage(document.GetPage 1)
            with _ ->
                ""
        let words = text.Split((null: char array | null), StringSplitOptions.RemoveEmptyEntries)
        let title = String.Join(' ', words)
        if words.Length = 0 || words.Length > titlePageMaxWords || title.Length > titlePageMaxLength then
            None
        else
            Some title

/// Gives the document a /Title and asks readers to show it instead of the file name.
/// A title the document already carries is kept — in the Info dictionary, or in XMP,
/// where it only needs copying across.
///
/// Otherwise a title is only taken from the page when fromFirstPage asks for it, and
/// then only from a short first page (titlePageText). It is never guessed from the file
/// name, which would satisfy dc:title while telling a screen reader user something
/// untrue about the document, so a document with no title keeps reporting that failure.
let private nameDocument (document: PdfDocument) (fromFirstPage: bool) =
    let info = document.GetDocumentInfo()
    let titleSet =
        if not (String.IsNullOrWhiteSpace(info.GetTitle())) then
            None
        else
            xmpTitle document
            |> Option.orElseWith (fun () -> if fromFirstPage then titlePageText document else None)
            |> Option.map (fun title ->
                info.SetTitle title |> ignore
                title)

    let catalog = document.GetCatalog().GetPdfObject()
    let preferences =
        match catalog.GetAsDictionary(PdfName.ViewerPreferences) with
        | null ->
            let preferences = PdfDictionary()
            catalog.Put(PdfName.ViewerPreferences, preferences) |> ignore
            preferences
        | preferences -> preferences

    let displayDocTitleSet =
        if PdfBoolean.TRUE.Equals(preferences.Get(PdfName.DisplayDocTitle)) then
            false
        else
            preferences.Put(PdfName.DisplayDocTitle, PdfBoolean.TRUE) |> ignore
            true

    titleSet, displayDocTitleSet

// ---- Running the fixes ----

/// Points a recovered heading's table-of-contents links at the heading itself: the explicit
/// destination (/D) at its position on the page, a little above it so it isn't cut off at the
/// top of the window, rather than wherever the link went before; and a PDF 2.0 structure
/// destination (/SD) at the heading element, which readers that support it prefer and others
/// ignore. Returns how many links were changed.
let private pointLinksAtHeading (document: PdfDocument) (heading: TocHeadings.Heading) =
    if isNull (heading.Paragraph.GetIndirectReference()) then
        heading.Paragraph.MakeIndirect document |> ignore
    let position: PdfObject list =
        match heading.Geometry with
        | Some geometry -> [ PdfNumber geometry.Left; PdfNumber(geometry.Top + 12.0) ]
        | None -> [ PdfNull.PDF_NULL; PdfNull.PDF_NULL ]
    let destination (target: PdfObject) =
        let array = PdfArray()
        array.Add target
        array.Add PdfName.XYZ
        position |> List.iter array.Add
        // A null zoom keeps the reader's current zoom.
        array.Add PdfNull.PDF_NULL
        array
    for annotation in heading.Entry.Links do
        let action =
            match annotation.GetAsDictionary(PdfName.A) with
            | NonNull action when PdfName.GoTo.Equals(action.GetAsName(PdfName.S)) -> action
            | _ ->
                let action = PdfDictionary()
                action.Put(PdfName.S, PdfName.GoTo) |> ignore
                action
        action.Put(PdfName.D, destination (document.GetPage(heading.Entry.Page).GetPdfObject())) |> ignore
        action.Put(PdfName "SD", destination heading.Paragraph) |> ignore
        annotation.Remove(PdfName.Dest) |> ignore
        annotation.Put(PdfName.A, action) |> ignore
    heading.Entry.Links.Length

let private tagHeadingsFromToc (document: PdfDocument) context =
    match TocHeadings.analyse document context with
    | TocHeadings.Recoverable(headings, _) ->
        let mutable links = 0
        for heading in headings do
            heading.Paragraph.Put(PdfName.S, PdfName $"H{heading.Level}") |> ignore
            links <- links + pointLinksAtHeading document heading
        [ $"Tagged {headings.Length} heading(s) listed in the table of contents."
          if links > 0 then $"Pointed {links} table-of-contents link(s) at their headings." ]
    | _ -> []

let private removeEmpty isTarget (describe: int -> string) (document: PdfDocument) context =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> []
    | structTreeRoot ->
        match removeEmptyInside isTarget structTreeRoot context None with
        | 0 -> []
        | removed -> [ describe removed ]

let private removeEmptyParagraphs =
    removeEmpty isParagraph (fun removed -> $"Removed {removed} empty /P structure element(s).")

let private removeEmptyTocEntries =
    removeEmpty isTocItem (fun removed -> $"Removed {removed} empty table-of-contents entr(ies) (TOCI).")

let private tagParagraphLists (document: PdfDocument) context =
    match ParagraphLists.find document context with
    | [] -> []
    | runs ->
        // Later runs first: tagging one shortens its parent's /K, which would move the ones after it.
        for run in List.rev runs do
            ParagraphLists.tagAsList document run
        [ $"Tagged {runs.Length} run(s) of marked paragraphs as lists." ]

let private addBookmarks (document: PdfDocument) context =
    match Bookmarks.find document context with
    | Some missing when not missing.Bookmarkable.IsEmpty && missing.LanguageDeclared ->
        [ $"Added {Bookmarks.add document missing.Bookmarkable} bookmark(s) for the H1 and H2 headings." ]
    | _ -> []

let private setAnnotationTabOrder (document: PdfDocument) _ =
    let mutable fixedPages = 0
    for pageNumber in 1 .. document.GetNumberOfPages() do
        let page = document.GetPage(pageNumber).GetPdfObject()
        match page.GetAsArray(PdfName.Annots) with
        | null -> ()
        | annotations when annotations.Size() = 0 -> ()
        | _ ->
            if not (PdfName.S.Equals(page.GetAsName(PdfName.Tabs))) then
                page.Put(PdfName.Tabs, PdfName.S) |> ignore
                fixedPages <- fixedPages + 1
    if fixedPages > 0 then [ $"Set /Tabs /S on {fixedPages} page(s) containing annotations." ] else []

let private describeTocLinks (document: PdfDocument) context =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> []
    | structTreeRoot ->
        match describeLinks structTreeRoot context with
        | 0 -> []
        | described -> [ $"Described {described} link annotation(s) from their own text." ]

let private setDocumentTitle fromFirstPage (document: PdfDocument) _ =
    let titleSet, displayDocTitleSet = nameDocument document fromFirstPage
    [ match titleSet with
      | Some title -> $"Set the document title to \"{title}\"."
      | None -> ()
      if displayDocTitleSet then "Set /DisplayDocTitle so readers show the document title rather than the file name." ]

/// Legibyl's own repairs, in the order they run. Each takes the open document and returns a
/// line per change it made. The order matters where one repair changes what another sees:
///
///   1. Headings from the table of contents first. Its analysis has to see the tree the
///      warnings were computed from, and a numbered heading ("1. Scope") has to become a
///      heading before the list repair could take it for a list item.
///   2. Empty paragraphs and table-of-contents entries next, so blank lines between list items
///      are gone before lists are tagged. (The list repair copes with them either way, keeping
///      them in the item.)
///   3. Lists.
///   4. Bookmarks, after headings are recovered, so those get bookmarks too.
///   5. The rest touch pages, annotations and the Info dictionary rather than paragraphs, so
///      their order among themselves doesn't matter.
///
/// veraPDF's metadata fix runs after all of these (in VeraPdfValidator.Fix): it validates the
/// result, and syncs XMP with the Info dictionary the title repair may have just set.
let orderedFixes: (PdfFixOptions * (PdfDocument -> Context -> string list)) list =
    [ PdfFixOptions.HeadingsFromToc, tagHeadingsFromToc
      PdfFixOptions.RemoveEmptyParagraphs, removeEmptyParagraphs
      PdfFixOptions.RemoveEmptyTocEntries, removeEmptyTocEntries
      PdfFixOptions.ListsFromParagraphs, tagParagraphLists
      PdfFixOptions.BookmarksFromHeadings, addBookmarks
      PdfFixOptions.AnnotationTabs, setAnnotationTabOrder
      PdfFixOptions.LinkContents, describeTocLinks
      PdfFixOptions.DocumentTitle, setDocumentTitle false
      PdfFixOptions.TitleFromFirstPage, setDocumentTitle true ]

/// The structural repairs Legibyl makes itself, writing the result to outputPath.
/// Returns one plain-language line per repair that actually changed something, so a
/// document that already had a given fix applied reports nothing for it.
let applyFixes (inputPath: string) (outputPath: string) (options: PdfFixOptions) : string list =
    use reader = new PdfReader(inputPath)
    use writer = new PdfWriter(outputPath)
    use document = new PdfDocument(reader, writer)
    let context = createContext document inputPath
    [ for option, apply in orderedFixes do
          if options.HasFlag option then
              yield! apply document context ]
