/// Structural fixes that work on the raw PDF objects via iText: removing empty /P
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

// ---- Web link /Contents ----

/// The web address a link annotation goes to, if its action is a URI to one.
let private webLinkAddress (annotation: PdfDictionary) =
    match annotation.GetAsDictionary PdfName.A with
    | NonNull action when PdfName.Link.Equals(annotation.GetAsName PdfName.Subtype) && PdfName.URI.Equals(action.GetAsName PdfName.S) ->
        match action.GetAsString PdfName.URI with
        | NonNull uri -> LinkTitles.webAddress (uri.ToUnicodeString())
        | Null -> None
    | _ -> None

/// Every link annotation without a description that goes to a web address, with the address.
let private undescribedWebLinks (document: PdfDocument) =
    [ for pageNumber in 1 .. document.GetNumberOfPages() do
          match document.GetPage(pageNumber).GetPdfObject().GetAsArray PdfName.Annots with
          | null -> ()
          | annotations ->
              for i in 0 .. annotations.Size() - 1 do
                  match annotations.GetAsDictionary i with
                  | NonNull annotation when isBlankString (annotation.GetAsString PdfName.Contents) ->
                      match webLinkAddress annotation with
                      | Some address -> yield annotation, address
                      | None -> ()
                  | _ -> () ]

/// Object numbers of the link annotations describeWebLinks could give a /Contents: those going
/// to web addresses. Lets validation mark those findings as fixable by it.
let webLinkAnnotationNumbers (document: PdfDocument) : Set<int> =
    set
        [ for annotation, _ in undescribedWebLinks document do
              match annotation.GetIndirectReference() with
              | null -> ()
              | reference -> yield reference.GetObjNumber() ]

/// Gives each link to a web address that has no description one from the title of the page it
/// goes to, with its host ("Apply for benefits (example.com)"), from titles. Wherever the link
/// is: unlike its visible text ("click here"), the page's title does say where it goes. A link
/// whose page has no title that can be read is left for a person to describe.
let describeWebLinks (titles: IPageTitleSource) (document: PdfDocument) (_: Context) =
    match undescribedWebLinks document with
    | [] -> []
    | links ->
        let found = titles.PageTitles(links |> List.map snd |> List.distinct)
        let mutable described = 0
        for annotation, address in links do
            match LinkTitles.linkDescription found address with
            | Some description ->
                annotation.Put(PdfName.Contents, textString description) |> ignore
                described <- described + 1
            | None -> ()
        match described with
        | 0 -> []
        | 1 -> [ "Described 1 web link by the title of the page it goes to, read from the web: to check." ]
        | n -> [ $"Described {n} web links by the titles of the pages they go to, read from the web: to check." ]

// ---- Document title ----

/// The document's XMP `dc:title`, if it has one. Best-effort: a malformed XMP packet
/// should not stop the rest of the fixes.
let xmpTitle (document: PdfDocument) =
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
/// top of the window, rather than wherever the link went before; and, in a PDF 2.0 file, a
/// structure destination (/SD) at the heading element. /SD is a PDF 2.0 key, so a PDF 1.x file
/// doesn't get it: its readers would ignore it anyway. Returns how many links were changed.
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
        if document.GetPdfVersion().CompareTo(PdfVersion.PDF_2_0) >= 0 then
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

/// The table attributes (attribute dictionaries with /O /Table) that apply to a structure element:
/// its own (/A, one dictionary or an array of them, with revision numbers between), then its
/// classes' (/C, through the StructTreeRoot's /ClassMap).
let private tableAttributes (classMap: PdfDictionary | null) (element: PdfDictionary) : PdfDictionary list =
    let isTable (attributes: PdfDictionary) = PdfName.Table.Equals(attributes.GetAsName PdfName.O)
    let tablesIn (value: PdfObject | null) =
        match value with
        | :? PdfDictionary as attributes when isTable attributes -> [ attributes ]
        | :? PdfArray as array ->
            [ for i in 0 .. array.Size() - 1 do
                  match array.Get(i, true) with
                  | :? PdfDictionary as attributes when isTable attributes -> attributes
                  | _ -> () ]
        | _ -> []
    let classes =
        match classMap, element.Get(PdfName.C, true) with
        | NonNull map, (:? PdfName as name) -> tablesIn (map.Get(name, true))
        | NonNull map, (:? PdfArray as names) ->
            [ for i in 0 .. names.Size() - 1 do
                  match names.Get(i, true) with
                  | :? PdfName as name -> yield! tablesIn (map.Get(name, true))
                  | _ -> () ]
        | _ -> []
    tablesIn (element.Get(PdfName.A, true)) @ classes

/// The most a table can span, in rows or columns: more is taken for a malformed value.
[<Literal>]
let private MostSpan = 1000

/// Tagged tables' header cells (TH) without a Scope given one (PDF/UA 7.5; 8.2.5.26), so a screen
/// reader can tell which cells each heads: Column in a header row (in THead, or a row whose cells,
/// those spanning down into it included, are all header cells), Row for one with only header cells
/// before it in its row and a data cell after it, Column otherwise. Cells are placed in the table's
/// grid by their RowSpan and ColSpan (a row span no further than the table's last row, either at
/// most MostSpan). Attributes are read from the cell and its classes; a new Scope is written on the
/// cell alone (its /A copied, so attributes another cell shares aren't changed). A table whose cells
/// name their headers (/Headers) is left as it is.
/// A structure element's own first table attributes (in its /A), if it has some.
let private ownTableAttributes (cell: PdfDictionary) =
    match tableAttributes null cell with
    | own :: _ -> Some own
    | [] -> None

let private scopeTableHeaders (document: PdfDocument) context =
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> []
    | root ->
        let classMap = root.GetAsDictionary(PdfName "ClassMap")
        let attributesOf = tableAttributes classMap
        let tag (element: PdfDictionary) = standardTag element context
        let kids (element: PdfDictionary) : PdfDictionary list =
            match element.Get(PdfName.K, true) with
            | :? PdfDictionary as kid -> [ kid ]
            | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do match array.Get(i, true) with :? PdfDictionary as kid -> kid | _ -> () ]
            | _ -> []
        let span (cell: PdfDictionary) (name: PdfName) =
            attributesOf cell |> List.tryPick (fun a -> a.GetAsNumber name |> Option.ofObj |> Option.map _.IntValue()) |> Option.defaultValue 1 |> max 1 |> min MostSpan
        let mutable scoped = 0
        let mutable tables = 0
        for visit in walk root context do
            if PdfName.Table.Equals(tag visit.Element) then
                // Its rows, and whether each is in the table's head.
                let rows =
                    [ for kid in kids visit.Element do
                          match tag kid with
                          | t when PdfName.TR.Equals t -> kid, false
                          | t when PdfName.THead.Equals t -> for row in kids kid do if PdfName.TR.Equals(tag row) then row, true
                          | t when PdfName.TBody.Equals t || PdfName.TFoot.Equals t -> for row in kids kid do if PdfName.TR.Equals(tag row) then row, false
                          | _ -> () ]
                let cells = [| for row, head in rows -> head, [ for cell in kids row do if PdfName.TH.Equals(tag cell) || PdfName.TD.Equals(tag cell) then cell ] |]
                let named = cells |> Array.exists (fun (_, row) -> row |> List.exists (fun cell -> attributesOf cell |> List.exists (fun a -> not (isNull (a.Get PdfName.Headers)))))
                if not named then
                    // Which cell occupies each place in the grid, filling each row left to right past
                    // the cells spanning down into it; and where each cell starts.
                    let occupant = Dictionary<int * int, PdfDictionary>()
                    let starts = Dictionary<PdfDictionary, int * int>(HashIdentity.Reference)
                    for r in 0 .. cells.Length - 1 do
                        let mutable column = 0
                        for cell in snd cells[r] do
                            while occupant.ContainsKey((r, column)) do column <- column + 1
                            let rowSpan = min (span cell PdfName.RowSpan) (cells.Length - r)
                            let colSpan = span cell PdfName.ColSpan
                            starts[cell] <- (r, column)
                            for dr in 0 .. rowSpan - 1 do
                                for dc in 0 .. colSpan - 1 do occupant[(r + dr, column + dc)] <- cell
                            column <- column + colSpan
                    let isHeader (cell: PdfDictionary) = PdfName.TH.Equals(tag cell)
                    // A row's cells, those spanning down into it included, by the column each covers.
                    let inRow (r: int) =
                        occupant |> Seq.filter (fun kv -> fst kv.Key = r) |> Seq.map (fun kv -> snd kv.Key, kv.Value) |> List.ofSeq
                    let mutable changed = false
                    for r in 0 .. cells.Length - 1 do
                        let head, row = cells[r]
                        let covering = inRow r
                        let headerRow = head || covering |> List.forall (snd >> isHeader)
                        for cell in row do
                            let hasScope = attributesOf cell |> List.exists (fun a -> not (isNull (a.Get PdfName.Scope)))
                            if isHeader cell && not hasScope then
                                let _, column = starts[cell]
                                let before = covering |> List.filter (fun (c, _) -> c < column)
                                let after = covering |> List.filter (fun (c, other) -> c > column && not (obj.ReferenceEquals(other, cell)))
                                let scope =
                                    if headerRow then PdfName.Column
                                    elif before |> List.forall (snd >> isHeader) && after |> List.exists (not << isHeader << snd) then PdfName.Row
                                    else PdfName.Column
                                // On the cell alone: its own table attributes copied with the scope
                                // (another cell may share them), in a copy of its /A.
                                let withScope = PdfDictionary()
                                match ownTableAttributes cell with
                                | Some own -> withScope.PutAll own
                                | None -> withScope.Put(PdfName.O, PdfName.Table) |> ignore
                                withScope.Put(PdfName.Scope, scope) |> ignore
                                let replacement =
                                    match cell.Get(PdfName.A, true) with
                                    | null -> withScope :> PdfObject
                                    | :? PdfDictionary as only when PdfName.Table.Equals(only.GetAsName PdfName.O) -> withScope :> PdfObject
                                    | :? PdfArray as array ->
                                        let copy = PdfArray()
                                        let mutable replaced = false
                                        for i in 0 .. array.Size() - 1 do
                                            match array.Get(i, true) with
                                            | :? PdfDictionary as a when not replaced && PdfName.Table.Equals(a.GetAsName PdfName.O) ->
                                                copy.Add withScope
                                                replaced <- true
                                            | _ -> copy.Add(array.Get(i, false))
                                        if not replaced then copy.Add withScope
                                        copy :> PdfObject
                                    | other -> PdfArray([ other; withScope :> PdfObject ] |> Collections.Generic.List) :> PdfObject
                                cell.Put(PdfName.A, replacement) |> ignore
                                scoped <- scoped + 1
                                changed <- true
                    if changed then tables <- tables + 1
        if scoped > 0 then
            [ $"Gave {scoped} table header cell(s) in {tables} table(s) a Scope (Column in a header row, Row for one heading its row), so a screen reader can tell which cells each heads: to check." ]
        else
            []

/// Where JoinSplitParagraphs finds each page's regions: the source, the document to ask it about
/// (the original: tagging doesn't change how its pages look), and what to tell as it asks for
/// each page (its number, and the document's page count).
type PageLayout =
    { Source: IPageLayoutSource
      PdfPath: string
      Report: int -> int -> unit }

let private joinSplitParagraphs (layout: PageLayout option) (document: PdfDocument) context =
    match layout with
    | None -> []
    | Some layout ->
        let pageCount = document.GetNumberOfPages()
        let regions page =
            layout.Report page pageCount
            layout.Source.PageRegions(layout.PdfPath, page)
        match ParagraphJoins.find document context regions with
        | [] -> []
        | runs ->
            let joined = ParagraphJoins.join runs
            [ $"Joined {joined + runs.Length} paragraphs tagged line by line into {runs.Length}, where the page layout shows one block of text." ]

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

/// Embedded TrueType CID fonts without a /CIDToGIDMap get /Identity: the mapping the PDF spec
/// says readers use when it's missing, so nothing draws differently, but PDF/UA (7.21.3.2-1)
/// wants it written down. macOS Quartz, for one, leaves it out.
let private setIdentityCidToGidMaps (document: PdfDocument) _ =
    let mutable fixedFonts = 0
    let fix (font: PdfDictionary) =
        if PdfName.CIDFontType2.Equals(font.GetAsName PdfName.Subtype) && isNull (font.Get PdfName.CIDToGIDMap) then
            match font.GetAsDictionary PdfName.FontDescriptor with
            | null -> ()
            | descriptor when isNull (descriptor.Get PdfName.FontFile2) -> ()
            | _ ->
                font.Put(PdfName.CIDToGIDMap, PdfName.Identity) |> ignore
                fixedFonts <- fixedFonts + 1
    for number in 1 .. document.GetNumberOfPdfObjects() - 1 do
        match document.GetPdfObject number with
        | :? PdfDictionary as font when PdfName.Font.Equals(font.GetAsName PdfName.Type) ->
            fix font
            // A Type0 font's descendant can also be a direct object, which the loop won't reach.
            match font.GetAsArray PdfName.DescendantFonts with
            | null -> ()
            | descendants ->
                for i in 0 .. descendants.Size() - 1 do
                    match descendants.Get(i, false) with
                    | :? PdfDictionary as descendant -> fix descendant
                    | _ -> ()
        | _ -> ()
    if fixedFonts > 0 then
        [ $"Set /CIDToGIDMap /Identity on {fixedFonts} embedded TrueType CID font(s), the mapping readers already use when it's missing." ]
    else
        []

/// For a document with no XMP metadata stream, writes one from its Info dictionary, so
/// veraPDF's metadata fixer (which runs next) has metadata to fix: for a document without
/// any, it answers Won't fix.
let private addMissingMetadata (document: PdfDocument) _ =
    if not (isNull (document.GetCatalog().GetPdfObject().Get PdfName.Metadata)) then
        []
    else
        let info = document.GetDocumentInfo()
        let value (text: string | null) =
            match text with
            | NonNull text when not (String.IsNullOrWhiteSpace text) -> Some text
            | _ -> None
        let xmp = XMPMetaFactory.Create()
        value (info.GetTitle()) |> Option.iter (fun title -> xmp.SetLocalizedText(XMPConst.NS_DC, "title", null, XMPConst.X_DEFAULT, title))
        value (info.GetAuthor())
        |> Option.iter (fun author ->
            xmp.AppendArrayItem(XMPConst.NS_DC, "creator", iText.Kernel.XMP.Options.PropertyOptions(iText.Kernel.XMP.Options.PropertyOptions.ARRAY_ORDERED), author, null))
        value (info.GetSubject()) |> Option.iter (fun subject -> xmp.SetLocalizedText(XMPConst.NS_DC, "description", null, XMPConst.X_DEFAULT, subject))
        value (info.GetKeywords()) |> Option.iter (fun keywords -> xmp.SetProperty(XMPConst.NS_PDF, "Keywords", keywords))
        value (info.GetCreator()) |> Option.iter (fun creator -> xmp.SetProperty(XMPConst.NS_XMP, "CreatorTool", creator))
        value (info.GetProducer()) |> Option.iter (fun producer -> xmp.SetProperty(XMPConst.NS_PDF, "Producer", producer))
        for key, property in [ PdfName.CreationDate, "CreateDate"; PdfName.ModDate, "ModifyDate" ] do
            match document.GetTrailer().GetAsDictionary PdfName.Info |?> _.GetAsString(key) with
            | null -> ()
            | date ->
                match PdfDate.GetW3CDate(date.GetValue()) with
                | NonNull w3c when w3c <> "" -> xmp.SetProperty(XMPConst.NS_XMP, property, w3c)
                | _ -> ()
        document.SetXmpMetadata xmp
        [ "Added an XMP metadata stream, from the document information dictionary." ]

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

let private setLanguageFromText (document: PdfDocument) context =
    if DocumentLanguage.isDeclared document then
        []
    else
        match DocumentLanguage.detect (DocumentLanguage.sample document context) with
        | Some language ->
            document.GetCatalog().GetPdfObject().Put(PdfName.Lang, PdfString language) |> ignore
            [ $"Declared the document's language as {language} ({DocumentLanguage.nameOf language}), detected from its text." ]
        | None -> []

/// Legibyl's own fixes, in the order they run. Each takes the open document and returns a
/// line per change it made. The order matters where one fix changes what another sees:
///
///   0. The parent tree first, where it's missing: until it's rebuilt, every tag reads as
///      untagged. Then the document's language: it only sets /Lang, and bookmarks need it.
///   1. Headings from the table of contents next. Its analysis has to see the tree the
///      warnings were computed from, and a numbered heading ("1. Scope") has to become a
///      heading before the list fix could take it for a list item.
///   2. Empty paragraphs, table-of-contents entries and figures next, so blank lines between list items
///      are gone before lists are tagged. (The list fix copes with them either way, keeping
///      them in the item.)
///   3. Paragraphs tagged line by line are joined, so a list item's second line is part of
///      the item before lists are found (a line starting with a marker is never joined on).
///   4. Lists.
///   5. Bookmarks, after headings are recovered, so those get bookmarks too. Then your edits,
///      once nothing else will move the elements they're for. Then links' and bookmarks'
///      targets (PDF 2.0 only), once the elements they land on won't move or merge.
///   6. The rest touch pages, annotations, fonts and the Info dictionary rather than
///      paragraphs, so their order among themselves doesn't matter (except that a contents
///      link is described from its own text before any is from a web page's title)...
///   7. ...except that a missing XMP stream is written last, from the Info dictionary the
///      title fixes may have just filled in.
///
/// veraPDF's metadata fix runs after all of these (in VeraPdfValidator.Fix): it validates the
/// result, and syncs XMP with the Info dictionary the title fix may have just set.
/// titles is where LinkTitlesFromWeb reads the titles of the pages links go to, layout where
/// JoinSplitParagraphs finds the pages' regions (without one, it does nothing), and edits and title
/// what ApplyEdits writes (the title before the title fixes, which then leave it).
/// A CMap program's text without its PostScript comments, strings left as they are: so a
/// comment that mentions /WMode isn't taken for its definition.
let private withoutComments (program: string) =
    let out = Text.StringBuilder(program.Length)
    let mutable depth = 0 // inside a string literal ( ... ), which can nest
    let mutable i = 0
    while i < program.Length do
        let c = program[i]
        if depth > 0 then
            out.Append c |> ignore
            if c = '\\' && i + 1 < program.Length then
                out.Append program[i + 1] |> ignore
                i <- i + 1
            elif c = '(' then depth <- depth + 1
            elif c = ')' then depth <- depth - 1
        elif c = '(' then
            depth <- 1
            out.Append c |> ignore
        elif c = '%' then
            // To the end of the line.
            while i < program.Length && program[i] <> '\n' && program[i] <> '\r' do
                i <- i + 1
            out.Append ' ' |> ignore
            i <- i - 1
        else
            out.Append c |> ignore
        i <- i + 1
    out.ToString()

/// The glyphs an embedded TrueType font program draws (those with an outline in its glyf table,
/// by its loca table), or None if it can't be read.
let private trueTypeGlyphs (program: byte array) : Set<int> option =
    try
        let u16 (at: int) = (int program[at] <<< 8) ||| int program[at + 1]
        let u32 (at: int) = (int64 (u16 at) <<< 16) ||| int64 (u16 (at + 2))
        let tables =
            dict [ for i in 0 .. u16 4 - 1 ->
                       let entry = 12 + 16 * i
                       Text.Encoding.ASCII.GetString(program, entry, 4), int (u32 (entry + 8)) ]
        let glyphs = u16 (tables["maxp"] + 4)
        let longOffsets = u16 (tables["head"] + 50) = 1
        let loca = tables["loca"]
        let offset (glyph: int) = if longOffsets then u32 (loca + 4 * glyph) else int64 (u16 (loca + 2 * glyph)) * 2L
        Some(set [ for g in 0 .. glyphs - 1 do if offset (g + 1) > offset g then g ])
    with _ -> None

/// Embedded fonts' dictionaries made to agree with their font programs and CMaps, as PDF/UA asks,
/// without changing what's drawn: a subset font's glyph list (a Type 1 font's CharSet, a CID
/// font's CIDSet: optional, and not used to draw) is removed, since it has to list every glyph
/// to be right (7.21.4.2); an embedded composite font's CIDSystemInfo is set to its CMap's, which
/// it has to match (7.21.3.1; it only matters for choosing a substitute, and the font is
/// embedded); and an embedded CMap's /WMode is set to the one in its program, which readers use
/// (7.21.3.3-2).
let private repairFontDictionaries (document: PdfDocument) _ =
    let mutable glyphLists = 0
    let mutable uncheckedLists = 0
    let mutable collections = 0
    let mutable writingModes = 0
    let subset (name: PdfName | null) =
        match name with
        | NonNull name -> Regex.IsMatch(name.GetValue(), "^[A-Z]{6}\\+")
        | Null -> false
    let embedded (descriptor: PdfDictionary) =
        not (isNull (descriptor.Get PdfName.FontFile) && isNull (descriptor.Get PdfName.FontFile2) && isNull (descriptor.Get PdfName.FontFile3))
    let descriptorOf (font: PdfDictionary) =
        match font.GetAsDictionary PdfName.FontDescriptor with
        | null -> None
        | descriptor -> Some descriptor
    // A subset font's glyph list.
    let glyphList (font: PdfDictionary) =
        match descriptorOf font with
        | Some descriptor when embedded descriptor && subset (font.GetAsName PdfName.BaseFont) ->
            let subtype = font.GetAsName PdfName.Subtype
            if PdfName.Type1.Equals subtype && not (isNull (descriptor.Get PdfName.CharSet)) then
                // A Type 1 font's glyph names can't be read here to check it against: it goes.
                descriptor.Remove PdfName.CharSet |> ignore
                uncheckedLists <- uncheckedLists + 1
            elif (PdfName.CIDFontType0.Equals subtype || PdfName.CIDFontType2.Equals subtype) && not (isNull (descriptor.Get PdfName.CIDSet)) then
                // A TrueType font's (with CIDs as its glyph numbers) is checked: kept if it lists
                // every glyph the program draws. Another kind's can't be, and goes: it's optional.
                let identity = PdfName.CIDFontType2.Equals subtype && (match font.Get PdfName.CIDToGIDMap with :? PdfName as m -> PdfName.Identity.Equals m | null -> true | _ -> false)
                let complete =
                    match descriptor.GetAsStream PdfName.FontFile2, descriptor.GetAsStream PdfName.CIDSet with
                    | NonNull program, NonNull cidSet when identity ->
                        trueTypeGlyphs (program.GetBytes())
                        |> Option.map (fun glyphs ->
                            let bits = cidSet.GetBytes()
                            glyphs |> Set.forall (fun g -> g / 8 < bits.Length && (bits[g / 8] &&& (0x80uy >>> (g % 8))) <> 0uy))
                    | _ -> None
                match complete with
                | Some true -> ()
                | Some false ->
                    descriptor.Remove PdfName.CIDSet |> ignore
                    glyphLists <- glyphLists + 1
                | None ->
                    descriptor.Remove PdfName.CIDSet |> ignore
                    uncheckedLists <- uncheckedLists + 1
        | _ -> ()
    // A composite font's CIDSystemInfo, and its embedded CMap's /WMode.
    let composite (font: PdfDictionary) =
        let descendant =
            match font.GetAsArray PdfName.DescendantFonts with
            | null -> None
            | descendants when descendants.Size() > 0 ->
                match descendants.Get(0, true) with
                | :? PdfDictionary as descendant -> Some descendant
                | _ -> None
            | _ -> None
        let encoding = font.Get(PdfName.Encoding, true)
        // The CMap's registry, ordering and supplement, if it says (or its name does).
        let cmapCollection =
            match encoding with
            | :? PdfStream as cmap ->
                // Its /WMode as its program has it, which readers use.
                let program = withoutComments (Text.Encoding.Latin1.GetString(cmap.GetBytes()))
                // The last definition is the one in force.
                let definitions = Regex.Matches(program, @"/WMode\s+(\d)\s+def")
                if definitions.Count > 0 then
                    let wanted = int definitions[definitions.Count - 1].Groups[1].Value
                    let stated = match cmap.GetAsNumber (PdfName "WMode") with null -> 0 | n -> n.IntValue()
                    if stated <> wanted then
                        cmap.Put((PdfName "WMode"), PdfNumber wanted) |> ignore
                        writingModes <- writingModes + 1
                match cmap.GetAsDictionary PdfName.CIDSystemInfo with
                | null -> None
                | info ->
                    match info.GetAsString PdfName.Registry, info.GetAsString PdfName.Ordering with
                    | NonNull registry, NonNull ordering ->
                        Some(registry.ToUnicodeString(), ordering.ToUnicodeString(), (match info.GetAsNumber PdfName.Supplement with null -> 0 | n -> n.IntValue()))
                    | _ -> None
            | :? PdfName as name ->
                PredefinedCMaps.ordering (name.GetValue()) |> Option.map (fun ordering -> "Adobe", ordering, 0)
            | _ -> None
        match descendant, cmapCollection with
        | Some cidFont, Some(registry, ordering, supplement) ->
            match descriptorOf cidFont with
            | Some descriptor when embedded descriptor ->
                let info = cidFont.GetAsDictionary PdfName.CIDSystemInfo
                let same =
                    not (isNull info)
                    && (match info.GetAsString PdfName.Registry with NonNull r -> r.ToUnicodeString() = registry | Null -> false)
                    && (match info.GetAsString PdfName.Ordering with NonNull o -> o.ToUnicodeString() = ordering | Null -> false)
                    && (match info.GetAsNumber PdfName.Supplement with null -> false | n -> n.IntValue() <= supplement)
                if not same then
                    let replacement = PdfDictionary()
                    replacement.Put(PdfName.Registry, PdfString registry) |> ignore
                    replacement.Put(PdfName.Ordering, PdfString ordering) |> ignore
                    // No more than the CMap's: for a predefined CMap, 0, the first supplement.
                    let current = match info with null -> 0 | info -> (match info.GetAsNumber PdfName.Supplement with null -> 0 | n -> n.IntValue())
                    replacement.Put(PdfName.Supplement, PdfNumber(min current supplement)) |> ignore
                    cidFont.Put(PdfName.CIDSystemInfo, replacement) |> ignore
                    collections <- collections + 1
            | _ -> ()
        | _ -> ()
    let rec fix (font: PdfDictionary) =
        glyphList font
        if PdfName.Type0.Equals(font.GetAsName PdfName.Subtype) then
            composite font
            // A descendant can be a direct object, which the loop over objects won't reach.
            match font.GetAsArray PdfName.DescendantFonts with
            | null -> ()
            | descendants ->
                for i in 0 .. descendants.Size() - 1 do
                    match descendants.Get(i, false) with
                    | :? PdfDictionary as descendant -> glyphList descendant
                    | _ -> ()
    let seen = Collections.Generic.HashSet<PdfDictionary>(HashIdentity.Reference)
    for number in 1 .. document.GetNumberOfPdfObjects() - 1 do
        match document.GetPdfObject number with
        | :? PdfDictionary as font when PdfName.Font.Equals(font.GetAsName PdfName.Type) && seen.Add font -> fix font
        | _ -> ()
    [ if glyphLists > 0 then
          $"Removed the incomplete glyph list (CharSet or CIDSet) of {glyphLists} subset font(s): it's optional, and isn't used to draw."
      if uncheckedLists > 0 then
          $"Removed the glyph list (CharSet or CIDSet) of {uncheckedLists} subset font(s) whose list couldn't be checked against the font: it's optional, and isn't used to draw."
      if collections > 0 then
          $"Set the character collection (CIDSystemInfo) of {collections} embedded composite font(s) to their CMap's."
      if writingModes > 0 then
          $"Set the writing direction (/WMode) of {writingModes} embedded CMap(s) to the one in their program, which readers use." ]

/// The character codes each font draws, by its dictionary: what the pages' text uses (and the
/// forms they draw, and their annotations' appearances), each code as many bytes as the font's
/// codes are. And the pages that couldn't be read.
let private usedCodes (document: PdfDocument) =
    let used = Dictionary<PdfDictionary, HashSet<int>>(HashIdentity.Reference)
    let listener =
        { new iText.Kernel.Pdf.Canvas.Parser.Listener.IEventListener with
            member _.EventOccurred(data, kind) =
                match data with
                | :? iText.Kernel.Pdf.Canvas.Parser.Data.TextRenderInfo as info ->
                    let font = info.GetFont()
                    let dictionary = font.GetPdfObject()
                    let bytes = info.GetPdfString().GetValueBytes()
                    let codes =
                        match font with
                        | :? iText.Kernel.Font.PdfType0Font -> [ for i in 0 .. 2 .. bytes.Length - 2 -> (int bytes[i] <<< 8) ||| int bytes[i + 1] ]
                        | _ -> [ for b in bytes -> int b ]
                    match used.TryGetValue dictionary with
                    | true, set -> set.UnionWith codes
                    | _ -> used[dictionary] <- HashSet<int>(codes)
                | _ -> ()
            member _.GetSupportedEvents() =
                HashSet<iText.Kernel.Pdf.Canvas.Parser.EventType>([ iText.Kernel.Pdf.Canvas.Parser.EventType.RENDER_TEXT ]) }
    let unread = ResizeArray<int>()
    for number in 1 .. document.GetNumberOfPages() do
        let page = document.GetPage number
        try
            iText.Kernel.Pdf.Canvas.Parser.PdfCanvasProcessor(listener).ProcessPageContent page
            // And what its annotations' appearances draw (a form field's text, say).
            for annotation in page.GetAnnotations() do
                let appearances =
                    match annotation.GetNormalAppearanceObject() with
                    | null -> []
                    | :? PdfStream as stream -> [ stream ]
                    | states -> [ for key in states.KeySet() do match states.GetAsStream key with null -> () | stream -> stream ]
                for appearance in appearances do
                    let resources = match appearance.GetAsDictionary PdfName.Resources with null -> PdfDictionary() | r -> r
                    iText.Kernel.Pdf.Canvas.Parser.PdfCanvasProcessor(listener).ProcessContent(appearance.GetBytes(), PdfResources resources)
        with _ ->
            unread.Add number
    used, List.ofSeq unread

/// Fonts made to say which characters their codes draw (a ToUnicode map), so a screen reader can
/// read their text (7.21.7; 8.4.5.8), where some code the pages use says none: a composite
/// Identity font with no map, a map that leaves codes out or gives U+0000, U+FEFF or U+FFFE, or a
/// simple font whose glyph names aren't the Glyph List's. What each code draws comes from what the
/// font says (see ToUnicodeMaps): its glyph names, or its embedded TrueType program's Unicode
/// cmap. A code nothing says is left out, and counted: its text still can't be read.
let private writeFontUnicode (document: PdfDocument) _ =
    let mutable fonts = 0
    let mutable added = 0
    let mutable missing = 0
    let symbolic (descriptor: PdfDictionary | null) =
        match descriptor with
        | NonNull d -> (match d.GetAsNumber PdfName.Flags with null -> false | n -> (n.IntValue() &&& 4) <> 0)
        | Null -> false
    let trueTypeProgram (descriptor: PdfDictionary | null) =
        match descriptor with
        | NonNull d ->
            match d.GetAsStream PdfName.FontFile2 with
            | null -> None
            | program -> Some(program.GetBytes())
        | Null -> None
    let used, unread = usedCodes document
    for KeyValue(font, codes) in used do
        let existing =
            match font.GetAsStream PdfName.ToUnicode with
            | null -> Map.empty
            | stream -> try ToUnicodeMaps.parse (Text.Encoding.Latin1.GetString(stream.GetBytes())) with _ -> Map.empty
        let subtype = font.GetAsName PdfName.Subtype
        // What each code draws, as the font says it, and whether a reader already knows it
        // without a map (a simple font's standard names).
        let worked =
            if PdfName.Type0.Equals subtype then
                match font.GetAsName PdfName.Encoding, font.GetAsArray PdfName.DescendantFonts with
                | NonNull encoding, NonNull descendants when (encoding.GetValue() = "Identity-H" || encoding.GetValue() = "Identity-V") && descendants.Size() > 0 ->
                    let cidFont = descendants.GetAsDictionary 0
                    let descriptor = cidFont.GetAsDictionary PdfName.FontDescriptor
                    let glyphOf =
                        match cidFont.Get(PdfName.CIDToGIDMap, true) with
                        | :? PdfStream as map ->
                            let table = map.GetBytes()
                            fun (cid: int) -> if 2 * cid + 1 < table.Length then (int table[2 * cid] <<< 8) ||| int table[2 * cid + 1] else 0
                        | _ -> id
                    let glyphText =
                        if PdfName.CIDFontType2.Equals(cidFont.GetAsName PdfName.Subtype) then
                            trueTypeProgram descriptor |> Option.bind ToUnicodeMaps.trueTypeGlyphText
                        else
                            None
                    Some((fun cid -> glyphText |> Option.bind (fun text -> text (glyphOf cid))), (fun _ -> None), 2)
                // Another CMap (CJK; its codes aren't two bytes each): left as it is, map and all.
                | _ -> None
            else
                let descriptor = font.GetAsDictionary PdfName.FontDescriptor
                let baseEncoding, differences =
                    match font.Get(PdfName.Encoding, true) with
                    | :? PdfName as name -> Some(name.GetValue()), Map.empty
                    | :? PdfDictionary as encoding ->
                        let names = Dictionary<int, string>()
                        match encoding.GetAsArray PdfName.Differences with
                        | null -> ()
                        | array ->
                            let mutable code = 0
                            for i in 0 .. array.Size() - 1 do
                                match array.Get i with
                                | :? PdfNumber as n -> code <- n.IntValue()
                                | :? PdfName as name ->
                                    names[code] <- name.GetValue()
                                    code <- code + 1
                                | _ -> ()
                        (encoding.GetAsName PdfName.BaseEncoding |> Option.ofObj |> Option.map _.GetValue()),
                        (names |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq)
                    | _ -> None, Map.empty
                // Codes /Differences doesn't name use the base encoding: the one named, else, for a
                // symbolic font, the font's built-in one (not known here), or Standard.
                let standard =
                    match baseEncoding with
                    | Some name -> ToUnicodeMaps.baseEncoding name
                    | None when symbolic descriptor -> None
                    | None -> ToUnicodeMaps.baseEncoding "StandardEncoding"
                // A name's text, if the Glyph List has it exactly (as readers take it).
                let exact (name: string) =
                    let unicode = AdobeGlyphList.NameToUnicode name
                    if unicode > 0 && unicode <= 0x10FFFF && (unicode < 0xD800 || unicode > 0xDFFF) then Some(Char.ConvertFromUtf32 unicode) else None
                let program =
                    if PdfName.TrueType.Equals subtype then
                        trueTypeProgram descriptor
                        |> Option.bind (fun bytes ->
                            try Some(UglyToad.PdfPig.Fonts.TrueType.Parser.TrueTypeFontParser.Parse(UglyToad.PdfPig.Fonts.TrueType.TrueTypeDataBytes bytes), bytes)
                            with _ -> None)
                    else
                        None
                // Worked out once for the font, if a code needs it.
                let glyphText = lazy (program |> Option.bind (fun (_, bytes) -> ToUnicodeMaps.trueTypeGlyphText bytes))
                let glyphNamed = lazy (program |> Option.bind (fun (_, bytes) -> ToUnicodeMaps.trueTypeGlyphNamed bytes))
                // A glyph /Differences names, by that name in the program (never by the code: the
                // name chose another glyph).
                let fromProgramName (name: string) =
                    match glyphNamed.Value, glyphText.Value with
                    | Some named, Some text -> named name |> Option.bind text
                    | _ -> None
                let fromProgram (code: int) =
                    match program with
                    | Some(parsed, _) ->
                        match glyphText.Value with
                        | Some glyphText ->
                            // (PdfPig says these are never null; a font without one gives null.)
                            let viaSymbol =
                                match Option.ofObj (box parsed.WindowsSymbolCMap) with
                                | Some table ->
                                    let table = table :?> UglyToad.PdfPig.Fonts.TrueType.Tables.CMapSubTables.ICMapSubTable
                                    max (table.CharacterCodeToGlyphIndex(0xF000 + code)) (table.CharacterCodeToGlyphIndex code)
                                | None -> 0
                            let viaMac =
                                match Option.ofObj (box parsed.MacRomanCMap) with
                                | Some table -> (table :?> UglyToad.PdfPig.Fonts.TrueType.Tables.CMapSubTables.ICMapSubTable).CharacterCodeToGlyphIndex code
                                | None -> 0
                            let glyph = if viaSymbol > 0 then viaSymbol else viaMac
                            if glyph > 0 then glyphText glyph else None
                        | None -> None
                    | None -> None
                let says (code: int) =
                    match Map.tryFind code differences with
                    | Some name -> ToUnicodeMaps.glyphNameText name |> Option.orElse (fromProgramName name)
                    | None ->
                        match standard with
                        | Some unicode when unicode code > 0 -> Some(Char.ConvertFromUtf32(unicode code))
                        | _ -> fromProgram code
                let known (code: int) =
                    match Map.tryFind code differences with
                    | Some name -> exact name
                    | None ->
                        match standard with
                        | Some unicode when unicode code > 0 -> Some(Char.ConvertFromUtf32(unicode code))
                        | _ -> None
                Some(says, known, 1)
        match worked with
        | None -> ()
        | Some(says, known, bytes) ->
        let readable (code: int) =
            match Map.tryFind code existing with
            | Some text -> ToUnicodeMaps.isReadable text
            | None -> (known code).IsSome
        let invalid = existing |> Map.exists (fun _ text -> not (ToUnicodeMaps.isReadable text))
        if invalid || codes |> Seq.exists (not << readable) then
            let mutable newly = 0
            let mutable still = 0
            let entries =
                [ for code in codes |> Seq.sort do
                      match Map.tryFind code existing with
                      | Some text when ToUnicodeMaps.isReadable text -> yield code, text
                      | _ ->
                          match known code |> Option.filter ToUnicodeMaps.isReadable |> Option.orElse (says code) with
                          | Some text ->
                              newly <- newly + 1
                              yield code, text
                          | None -> still <- still + 1 ]
                // And what the old map gave codes these pages don't use, if it gave it right.
                @ [ for KeyValue(code, text) in existing do if not (codes.Contains code) && ToUnicodeMaps.isReadable text then code, text ]
                |> Map.ofList
            if newly > 0 || invalid then
                let stream = PdfStream(Text.Encoding.ASCII.GetBytes(ToUnicodeMaps.write bytes entries))
                stream.SetCompressionLevel CompressionConstants.DEFAULT_COMPRESSION
                font.Put(PdfName.ToUnicode, stream.MakeIndirect document) |> ignore
                fonts <- fonts + 1
                added <- added + newly
            missing <- missing + still
    [ if not unread.IsEmpty then
          let pages = String.Join(", ", unread)
          $"The text of page(s) {pages} couldn't be read, so their fonts weren't checked for what they draw."
      if fonts > 0 then
          $"Wrote which characters {fonts} font(s) draw (ToUnicode), from their own glyph names and Unicode tables, so a screen reader can read {added} more character code(s): to check."
      if missing > 0 then
          $"{missing} character code(s) the pages use say nothing a reader can map to text: they need fixing where the PDF was made." ]

/// Fonts the pages draw with but don't embed, embedded from the open fonts Legibyl brings, where
/// one is made to the font's measure (see FontEmbedding): only the glyphs the pages draw, unless a
/// form's fields fill in with the font or some page couldn't be read, when it's the whole font.
/// The others are left, and reported.
let private embedFonts (document: PdfDocument) _ =
    let used, unread = usedCodes document
    let formFonts = HashSet<PdfDictionary>(HashIdentity.Reference)
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.AcroForm with
    | null -> ()
    | form ->
        match form.GetAsDictionary PdfName.DR with
        | null -> ()
        | resources ->
            match resources.GetAsDictionary PdfName.Font with
            | null -> ()
            | fonts -> for key in fonts.KeySet() do match fonts.GetAsDictionary key with null -> () | font -> formFonts.Add font |> ignore
    let outcomes =
        [ for KeyValue(font, codes) in used do
              match FontEmbedding.embed document font codes (not unread.IsEmpty || formFonts.Contains font) with
              | Some outcome -> outcome
              | None -> () ]
        |> List.distinct
    [ for name, outcome in outcomes do
          match outcome with
          | FontEmbedding.Embedded shown -> $"Embedded {shown} in place of {name}, which wasn't embedded: an open font with its glyphs, each as wide."
          | FontEmbedding.Left reason -> $"{name} isn't embedded, and was left as it is ({reason}): export the PDF again with fonts embedded." ]

let orderedFixes
    (titles: IPageTitleSource)
    (layout: PageLayout option)
    (edits: ElementEdit list)
    (title: string option)
    : (PdfFixOptions * (PdfDocument -> Context -> string list)) list =
    [ PdfFixOptions.ParentTree, ParentTree.rebuild
      PdfFixOptions.Language, setLanguageFromText
      PdfFixOptions.HeadingsFromToc, tagHeadingsFromToc
      PdfFixOptions.EmptyParagraphs, removeEmptyParagraphs
      PdfFixOptions.EmptyTocEntries, removeEmptyTocEntries
      PdfFixOptions.EmptyFigures, EmptyFigures.remove
      PdfFixOptions.JoinSplitParagraphs, joinSplitParagraphs layout
      PdfFixOptions.Lists, tagParagraphLists
      PdfFixOptions.TableHeaderScope, scopeTableHeaders
      PdfFixOptions.Bookmarks, addBookmarks
      PdfFixOptions.ApplyEdits,
      (fun document context ->
          ElementEdits.apply document context edits
          @ (title |> Option.map (ElementEdits.applyTitle document) |> Option.defaultValue []))
      PdfFixOptions.LinkTargets, (fun document context -> LinkTargets.apply document (LinkTargets.find document context))
      PdfFixOptions.Tabs, setAnnotationTabOrder
      PdfFixOptions.LinkContents, describeTocLinks
      PdfFixOptions.LinkTitlesFromWeb, describeWebLinks titles
      PdfFixOptions.EmbedFonts, embedFonts
      PdfFixOptions.FontGlyphs, setIdentityCidToGidMaps
      PdfFixOptions.FontDictionaries, repairFontDictionaries
      PdfFixOptions.FontUnicode, writeFontUnicode
      PdfFixOptions.Title, setDocumentTitle false
      PdfFixOptions.TitleFromFirstPage, setDocumentTitle true
      PdfFixOptions.Metadata, addMissingMetadata ]

/// The structural fixes Legibyl makes itself, writing the result to outputPath.
/// Returns one plain-language line per fix that actually changed something, so a
/// document that already had a given fix applied reports nothing for it; and the fixes asked
/// for that found nothing to change.
let applyFixes
    (inputPath: string)
    (outputPath: string)
    (options: PdfFixOptions)
    (titles: IPageTitleSource)
    (layout: PageLayout option)
    (edits: ElementEdit list)
    (title: string option)
    : string list * PdfFixOptions list =
    use reader = new PdfReader(inputPath)
    use writer = new PdfWriter(outputPath)
    use document = new PdfDocument(reader, writer)
    let context = createContext document inputPath
    let results =
        [ for option, apply in orderedFixes titles layout edits title do
              if options.HasFlag option then
                  yield option, apply document context ]
    // What each did, and those asked for that found nothing to change.
    List.collect snd results, [ for option, lines in results do if lines.IsEmpty then option ]
