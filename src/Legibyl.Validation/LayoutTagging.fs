/// Tags an untagged document from its pages' layout: regions (a heading, a paragraph, a
/// figure...) found by a layout detector, in reading order. Each piece of page content goes
/// to the region it's drawn in, and is marked in the page's content stream as that region's
/// content, or as an artifact (page furniture, decoration). The content stream is rewritten
/// by copying every original operator's bytes unchanged and inserting only the marked-content
/// operators (BDC/BMC/EMC) between them, so what the page draws doesn't change. Then the
/// structure tree is built: a Document of the regions' elements, page by page. A Table region
/// with cell regions (TH, TD) inside it is written as a table: its cells in rows (see TableGrid),
/// each cell's content its own; without cells, a table is written as a paragraph.
module internal Legibyl.Validation.LayoutTagging

open System
open System.Collections.Generic
open System.IO
open System.Text
open iText.IO.Font
open iText.IO.Source
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Pdf.Canvas.Parser.Listener
open iText.Kernel.Pdf.Canvas.Parser.Util
open iText.Kernel.Pdf.Annot

// The content stream's operators, and what each draws: see ContentOperations.
open Legibyl.Validation.ContentOperations

// ---- Assigning content to regions ----

/// Who an operator's content belongs to.
type private Owner =
    | Region of int
    | Artifact

let private area (r: Rectangle) = float (r.GetWidth()) * float (r.GetHeight())

let private rectangleOf (geometry: StructureGeometry) =
    Rectangle(float32 geometry.Left, float32 geometry.Bottom, float32 (geometry.Right - geometry.Left), float32 (geometry.Top - geometry.Bottom))

let private centre (r: Rectangle) = r.GetX() + r.GetWidth() / 2f, r.GetY() + r.GetHeight() / 2f

let private containsPoint (r: Rectangle) (x: float32, y: float32) =
    x >= r.GetLeft() && x <= r.GetRight() && y >= r.GetBottom() && y <= r.GetTop()

let private distance (r: Rectangle) (x: float32, y: float32) =
    let dx = max 0f (max (r.GetLeft() - x) (x - r.GetRight()))
    let dy = max 0f (max (r.GetBottom() - y) (y - r.GetTop()))
    sqrt (dx * dx + dy * dy)

/// Where a page's leftover text goes when it has no text regions at all.
[<Literal>]
let private LeftoverText = -1

/// Where a page's images go when it has no text (a scan): a figure, not an empty paragraph.
[<Literal>]
let private PageFigure = -2

let private isTextRegion (tag: string) = tag <> Tags.Artifact && tag <> Tags.Figure

/// The owner of one piece of content: the smallest region it's centred in. Text outside
/// every region still reads as content, in the nearest text region; images and drawings
/// outside every region are decoration. In a table with cells (cellsOf, by its region), what's
/// drawn between them goes to the nearest cell, and its lines and fills are decoration (a
/// table's rules).
let private ownerOf (regions: (LayoutRegion * Rectangle) array) (cellsOf: int -> int list) (drawn: Drawn) =
    let point = centre drawn.Box
    let inside =
        regions
        |> Array.indexed
        |> Array.filter (fun (_, (_, r)) -> containsPoint r point)
        |> Array.sortBy (fun (_, (_, r)) -> area r)
        |> Array.tryHead
    match drawn.Kind, inside with
    | Path, Some(index, (region, _)) when region.Tag = Tags.Table && not (cellsOf index).IsEmpty -> Artifact
    | _, Some(index, (region, _)) when region.Tag = Tags.Table && not (cellsOf index).IsEmpty ->
        cellsOf index |> List.minBy (fun cell -> distance (snd regions[cell]) point) |> Region
    | Text, Some(index, (region, _))
    | Image, Some(index, (region, _)) -> if region.Tag = Tags.Artifact then Artifact else Region index
    // Lines and fills are decoration, except in a figure or table (a chart, a table's rules),
    // or where they draw a region's text (outlined text, read by OCR into its ActualText).
    | Path, Some(index, (region, _)) when region.Tag = Tags.Figure || region.Tag = Tags.Table || not (isNull region.ActualText) ->
        Region index
    | Path, _ -> Artifact
    | Image, None -> Artifact
    | Text, None ->
        regions
        |> Array.indexed
        |> Array.filter (fun (_, (region, _)) -> isTextRegion region.Tag)
        |> Array.sortBy (fun (_, (_, r)) -> distance r point)
        |> Array.tryHead
        |> Option.map (fun (index, _) -> Region index)
        |> Option.defaultValue (Region LeftoverText)

/// The owner of an operator's content: the owner most of its pieces have.
let private operatorOwner regions cellsOf pageHasText (drawn: List<Drawn>) =
    drawn
    |> Seq.map (fun piece ->
        match piece.Kind, ownerOf regions cellsOf piece with
        | Image, Region _ when not pageHasText -> Region PageFigure
        | _, owner -> owner)
    |> Seq.countBy id
    |> Seq.maxBy snd
    |> fst


// ---- Building the structure ----

type private Element =
    { Dictionary: PdfDictionary
      Kids: PdfArray }

let private newElement (document: PdfDocument) (tag: string) (parent: PdfDictionary) (page: PdfPage option) =
    let dictionary = PdfDictionary()
    dictionary.Put(PdfName.Type, PdfName.StructElem) |> ignore
    dictionary.Put(PdfName.S, PdfName tag) |> ignore
    dictionary.Put(PdfName.P, parent) |> ignore
    page |> Option.iter (fun page -> dictionary.Put(PdfName.Pg, page.GetPdfObject()) |> ignore)
    let kids = PdfArray()
    dictionary.Put(PdfName.K, kids) |> ignore
    dictionary.MakeIndirect document |> ignore
    { Dictionary = dictionary; Kids = kids }

/// What was tagged, for the fix's report.
type private Tally =
    { mutable Pages: int
      mutable Headings: int
      mutable Paragraphs: int
      mutable ListItems: int
      mutable Figures: int
      mutable Tables: int
      mutable Links: int
      mutable UntaggedAnnotations: int
      mutable PagesWithoutText: int
      /// Elements whose text is drawn as outlines or pixels, given an OCR'd /ActualText.
      mutable Recognized: int
      /// Figures that are QR codes, described by what they encode.
      mutable QrCodes: int
      /// Of those, the ones given a link to the address they encode.
      mutable QrLinks: int
      /// QR codes described with the title of the page they go to.
      mutable QrTitles: int
      /// Figures given alt text a person wrote.
      mutable WrittenAlt: int
      /// Links without a description given one by the source.
      mutable LinksDescribed: int
      /// Tables written with their rows and cells (those without cells count in Tables).
      mutable TablesWithCells: int }

/// The tag written for a region, given whether the document has a title (H1) already.
let private writtenTag (region: LayoutRegion) hasTitle =
    match region.Tag with
    // A level a person gave (a retag edit) is kept.
    | Tags.H1
    | Tags.H2
    | Tags.H3
    | Tags.H4
    | Tags.H5
    | Tags.H6 -> region.Tag
    // Levels below the title aren't known from the layout; one level under it keeps the
    // outline from skipping levels.
    | Tags.H -> if hasTitle then Tags.H2 else Tags.H1
    | Tags.Figure -> Tags.Figure
    | Tags.Note -> Tags.Note
    | Tags.LI -> Tags.LI
    // A table's rows and cells can't be found from its outline; its text reads as text, and
    // the report says it needs tagging by hand.
    | _ -> Tags.P

/// Tags pdfPath's pages from source's regions, writing the result to outputPath. Returns
/// the report lines; empty (and nothing worth keeping written) if the document was already
/// tagged. With linkQrCodes, a QR code that encodes a web address also gets a link going
/// there; with titles, its description names the title of the page it goes to. onPage hears
/// (page, page count) as each page's regions are asked for.
let tag
    (pdfPath: string)
    (outputPath: string)
    (source: IPageLayoutSource)
    (linkQrCodes: bool)
    (titles: IPageTitleSource option)
    (onPage: int -> int -> unit)
    : string list * TaggedRegion list =
    use reader = new PdfReader(pdfPath)
    use writer = new PdfWriter(outputPath)
    use document = new PdfDocument(reader, writer)
    let catalog = document.GetCatalog().GetPdfObject()
    if not (isNull (catalog.Get PdfName.StructTreeRoot)) then
        [], []
    else
        let root = PdfDictionary()
        root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
        root.MakeIndirect document |> ignore
        let documentElement = newElement document Tags.Document root None
        root.Put(PdfName.K, documentElement.Dictionary) |> ignore
        // PDF/UA-2 (for PDF 2.0) wants the Document in PDF 2.0's namespace. The elements under
        // it are the same in PDF 1.7's, which is the default and is allowed there too.
        if document.GetPdfVersion().CompareTo PdfVersion.PDF_2_0 >= 0 then
            let pdf20 = PdfDictionary()
            pdf20.Put(PdfName.Type, PdfName.Namespace) |> ignore
            pdf20.Put(PdfName.NS, PdfString "http://iso.org/pdf2/ssn") |> ignore
            pdf20.MakeIndirect document |> ignore
            documentElement.Dictionary.Put(PdfName.NS, pdf20) |> ignore
            root.Put(PdfName.Namespaces, PdfArray(pdf20 :> PdfObject)) |> ignore

        let parentTree = PdfArray()
        let tally =
            { Pages = 0; Headings = 0; Paragraphs = 0; ListItems = 0; Figures = 0; Tables = 0; Links = 0; UntaggedAnnotations = 0; PagesWithoutText = 0; Recognized = 0; QrCodes = 0; QrLinks = 0; QrTitles = 0; WrittenAlt = 0; LinksDescribed = 0; TablesWithCells = 0 }
        let pageCount = document.GetNumberOfPages()
        let regionsByPage =
            [| for page in 1 .. pageCount ->
                   onPage page pageCount
                   List.ofSeq (source.PageRegions(pdfPath, page)) |]
        let hasTitle = regionsByPage |> Array.exists (List.exists (fun region -> region.Tag = Tags.H1))
        // The titles of the pages QR codes go to, when asked for.
        let pageTitles =
            match titles with
            | Some titles ->
                titles.PageTitles
                    [ for regions in regionsByPage do
                          for region in regions do
                              match region.QrText with
                              | NonNull text -> yield! Option.toList (LinkTitles.webAddress text)
                              | Null -> () ]
            | None ->
                // Titles the source already has (read in Suggest Structure), if any.
                match source.PageTitles with
                | NonNull titles -> titles
                | Null -> Dictionary<string, string>()
        let pageTitle address = LinkTitles.titleFor pageTitles address
        let mutable noteIds = 0
        // Annotations' /StructParent keys come after every page's.
        let objectReferences = List<PdfDictionary * PdfDictionary>()
        let tagged = List<TaggedRegion>()

        for pageNumber in 1 .. pageCount do
            let page = document.GetPage pageNumber
            let resources = page.GetResources()
            let bytes = page.GetContentBytes()
            let regions = [| for region in regionsByPage[pageNumber - 1] -> region, rectangleOf region.Geometry |]
            // Each cell's table: the smallest Table region its centre is in. A cell in none is
            // written as a paragraph.
            let tableOf = Dictionary<int, int>()
            for i, (region, box) in Array.indexed regions do
                if TableGrid.isCell region.Tag then
                    regions
                    |> Array.indexed
                    |> Array.filter (fun (_, (table, tableBox)) -> table.Tag = Tags.Table && containsPoint tableBox (centre box))
                    |> Array.sortBy (fun (_, (_, tableBox)) -> area tableBox)
                    |> Array.tryHead
                    |> Option.iter (fun (table, _) -> tableOf[i] <- table)
            let cellsOf (table: int) =
                [ for KeyValue(cell, owner) in tableOf do if owner = table then cell ] |> List.sort
            let drawn = drawnByOperator bytes resources
            let operations, length = operationsOf bytes resources

            // Each operator's owner; a path's construction goes with the operator that paints it.
            let pageHasText = drawn.Values |> Seq.exists (Seq.exists (fun piece -> piece.Kind = Text))
            let owners = Array.create operations.Count None
            for i in 0 .. operations.Count - 1 do
                match drawn.TryGetValue i with
                | true, pieces when operations[i].Name <> ContentOperators.n -> owners[i] <- Some(operatorOwner regions cellsOf pageHasText pieces)
                | _ when operations[i].Name = ContentOperators.sh -> owners[i] <- Some Artifact
                | _ -> ()
            let mutable painter = None
            for i in operations.Count - 1 .. -1 .. 0 do
                let name = operations[i].Name
                if pathPainting.Contains name then painter <- owners[i]
                elif pathConstruction.Contains name then owners[i] <- painter
                else painter <- None

            // The page's elements, made as content turns up for them, and added to the
            // document in reading order once the page is done.
            let elements = Dictionary<int, Element>()
            // QR code figures to link: the region, the address, and the Link element holding it.
            let qrLinks = List<int * string * Element>()
            let listItems = Dictionary<int, Element * string>()
            let mcids = List<PdfDictionary>()
            let regionAt index =
                match index with
                | LeftoverText -> { Tag = Tags.P; Geometry = Unchecked.defaultof<_>; ListNumbering = null; ActualText = null; QrText = null; Alt = null }
                | PageFigure -> { Tag = Tags.Figure; Geometry = Unchecked.defaultof<_>; ListNumbering = null; ActualText = null; QrText = null; Alt = null }
                | index -> fst regions[index]
            let markRecognized (element: Element) (region: LayoutRegion) =
                match region.ActualText with
                | NonNull text ->
                    element.Dictionary.Put(PdfName.ActualText, PdfString(text, PdfEncodings.UNICODE_BIG)) |> ignore
                    // OCR reads with its English model, so the text it gives is English.
                    element.Dictionary.Put(PdfName.Lang, PdfString "en") |> ignore
                    tally.Recognized <- tally.Recognized + 1
                | Null -> ()
            // A table with cells, all at once, when its first content turns up: a TR for each row,
            // and in each its cells (TH or TD, with their spans), each cell's content its own.
            let tableElement (table: int) =
                match elements.TryGetValue table with
                | true, element -> element
                | _ ->
                    let element = newElement document Tags.Table documentElement.Dictionary (Some page)
                    elements[table] <- element
                    tally.TablesWithCells <- tally.TablesWithCells + 1
                    let cells = cellsOf table
                    let places = TableGrid.place [ for cell in cells -> (fst regions[cell]).Geometry ]
                    let rowCount = places |> List.map (fun p -> p.Row + p.RowSpan) |> List.max
                    let rows = [| for _ in 1..rowCount -> newElement document Tags.TR element.Dictionary (Some page) |]
                    for row in rows do element.Kids.Add row.Dictionary
                    for cell, place in List.zip cells places |> List.sortBy (fun (_, p) -> p.Row, p.Column) do
                        let region = fst regions[cell]
                        let row = rows[place.Row]
                        let cellElement = newElement document region.Tag row.Dictionary (Some page)
                        let attributes = PdfDictionary()
                        attributes.Put(PdfName.O, PdfName.Table) |> ignore
                        if region.Tag = Tags.TH then attributes.Put(PdfName.Scope, PdfName.Column) |> ignore
                        if place.RowSpan > 1 then attributes.Put(PdfName.RowSpan, PdfNumber place.RowSpan) |> ignore
                        if place.ColSpan > 1 then attributes.Put(PdfName.ColSpan, PdfNumber place.ColSpan) |> ignore
                        cellElement.Dictionary.Put(PdfName.A, attributes) |> ignore
                        row.Kids.Add cellElement.Dictionary
                        markRecognized cellElement region
                        elements[cell] <- cellElement
                    element
            let elementFor (index: int) =
                match elements.TryGetValue index with
                | true, element -> element
                | _ when tableOf.ContainsKey index -> tableElement tableOf[index] |> ignore; elements[index]
                | _ when index >= 0 && not (cellsOf index).IsEmpty -> tableElement index
                | _ ->
                    let region = regionAt index
                    let element =
                        match writtenTag region hasTitle with
                        | Tags.LI ->
                            // Its list is found when the page's elements are put in order.
                            let item = newElement document Tags.LI documentElement.Dictionary None
                            let body = newElement document Tags.LBody item.Dictionary (Some page)
                            item.Kids.Add body.Dictionary
                            listItems[index] <- (item, (match region.ListNumbering with NonNull numbering -> numbering | Null -> ""))
                            tally.ListItems <- tally.ListItems + 1
                            body
                        | tag ->
                            let element = newElement document tag documentElement.Dictionary (Some page)
                            match tag with
                            | Tags.H1
                            | Tags.H2
                            | Tags.H3
                            | Tags.H4
                            | Tags.H5
                            | Tags.H6 -> tally.Headings <- tally.Headings + 1
                            | Tags.Figure ->
                                tally.Figures <- tally.Figures + 1
                                // Alt text a person wrote comes first; the QR code's own is kept
                                // for anything else it's used for (its link).
                                let written =
                                    match region.Alt with
                                    | NonNull alt when not (String.IsNullOrWhiteSpace alt) ->
                                        element.Dictionary.Put(PdfName.Alt, PdfString(alt.Trim(), PdfEncodings.UNICODE_BIG)) |> ignore
                                        tally.WrittenAlt <- tally.WrittenAlt + 1
                                        true
                                    | _ -> false
                                // A QR code says what it encodes, which is what its alt text should say.
                                match region.QrText with
                                | NonNull text ->
                                    let address = LinkTitles.webAddress text
                                    let alt = LinkTitles.qrAltText pageTitles text
                                    if not written then
                                        if address |> Option.bind pageTitle |> Option.isSome then
                                            tally.QrTitles <- tally.QrTitles + 1
                                        element.Dictionary.Put(PdfName.Alt, PdfString(alt, PdfEncodings.UNICODE_BIG)) |> ignore
                                        // Legibyl writes these descriptions in English.
                                        element.Dictionary.Put(PdfName.Lang, PdfString "en") |> ignore
                                        tally.QrCodes <- tally.QrCodes + 1
                                    // Unless the page already has a link over it.
                                    let alreadyLinked () =
                                        let point = centre (rectangleOf region.Geometry)
                                        page.GetAnnotations()
                                        |> Seq.exists (fun annotation ->
                                            PdfName.Link.Equals(annotation.GetSubtype())
                                            && not (isNull (annotation.GetRectangle()))
                                            && containsPoint (annotation.GetRectangle().ToRectangle()) point)
                                    match address with
                                    | Some address when linkQrCodes && not (alreadyLinked ()) ->
                                        // A Link holding the figure; its annotation is added with the page's.
                                        let wrapper = newElement document Tags.Link documentElement.Dictionary (Some page)
                                        // So is its annotation's /Contents.
                                        wrapper.Dictionary.Put(PdfName.Lang, PdfString "en") |> ignore
                                        element.Dictionary.Put(PdfName.P, wrapper.Dictionary) |> ignore
                                        wrapper.Kids.Add element.Dictionary
                                        qrLinks.Add((index, address, wrapper))
                                    | _ -> ()
                                | Null -> ()
                            | Tags.Note ->
                                // PDF/UA wants every Note to have an ID.
                                noteIds <- noteIds + 1
                                element.Dictionary.Put(PdfName.ID, PdfString $"note-{noteIds}") |> ignore
                            | _ ->
                                if region.Tag = Tags.Table then tally.Tables <- tally.Tables + 1
                                tally.Paragraphs <- tally.Paragraphs + 1
                            element
                    markRecognized element region
                    elements[index] <- element
                    element

            // Text objects (BT..ET) and saved graphics states (q..Q) whose content all has one
            // owner, or none: a marked sequence can hold the whole block, so it needn't close
            // around it. Writers like Quartz draw a line in many such blocks; closing at each
            // would split a heading into pieces that read with spaces between them.
            let uniformBlocks = Dictionary<int, int * Owner option>()
            // Each block's end, in one pass: q..Q nest, and a BT..ET sits inside one level.
            let ends = Dictionary<int, int>()
            let saves = Stack<int>()
            let mutable textStart = -1
            for k in 0 .. operations.Count - 1 do
                match operations[k].Name with
                | ContentOperators.q -> saves.Push k
                | ContentOperators.Q when saves.Count > 0 -> ends[saves.Pop()] <- k
                | ContentOperators.BT -> textStart <- k
                | ContentOperators.ET when textStart >= 0 ->
                    ends[textStart] <- k
                    textStart <- -1
                | _ -> ()
            // Where each operator's owner run and marked-content count stand, to test a block
            // in constant time.
            let markedBefore = Array.zeroCreate (operations.Count + 1)
            for k in 0 .. operations.Count - 1 do
                let name = operations[k].Name
                markedBefore[k + 1] <- markedBefore[k] + (if name = ContentOperators.BMC || name = ContentOperators.BDC || name = ContentOperators.EMC then 1 else 0)
            for KeyValue(first, last) in ends do
                if markedBefore[last + 1] = markedBefore[first] then
                    let inner = seq { for k in first .. last -> owners[k] } |> Seq.choose id |> Seq.distinct |> Seq.truncate 2 |> List.ofSeq
                    match inner with
                    | [] -> uniformBlocks[first] <- (last, None)
                    | [ owner ] -> uniformBlocks[first] <- (last, Some owner)
                    | _ -> ()

            // Copy the stream, opening and closing marked content around each owner's operators.
            let output = new MemoryStream()
            let write (text: string) = output.Write(Encoding.ASCII.GetBytes text, 0, text.Length)
            let copy (first: int) (last: int) =
                output.Write(bytes, int operations[first].Start, int (operations[last].End - operations[first].Start))
            let mutable current: Owner option = None
            let close () =
                if current.IsSome then
                    write "\nEMC\n"
                    current <- None
            let openFor owner =
                close ()
                match owner with
                | Artifact -> write "\n/Artifact BMC\n"
                | Region index ->
                    let element = elementFor index
                    let mcid = mcids.Count
                    mcids.Add element.Dictionary
                    element.Kids.Add(PdfNumber mcid)
                    let tag = (element.Dictionary.GetAsName PdfName.S).GetValue()
                    write $"\n/{tag} <</MCID {mcid}>> BDC\n"
                current <- Some owner
            let mutable insideArtifact = 0
            let mutable i = 0
            while i < operations.Count do
                match uniformBlocks.TryGetValue i with
                | true, (last, owner) when insideArtifact = 0 ->
                    // The whole block, in the sequence that's open or one opened for it.
                    match owner with
                    | Some owner when current <> Some owner -> openFor owner
                    | _ -> ()
                    copy i last
                    i <- last + 1
                | _ ->
                    let operation = operations[i]
                    if boundaries.Contains operation.Name then close ()
                    // Content already marked as an artifact stays as it is.
                    match operation.Name with
                    | ContentOperators.BMC
                    | ContentOperators.BDC ->
                        let text = Encoding.Latin1.GetString(bytes, int operation.Start, int (operation.End - operation.Start))
                        if insideArtifact > 0 || text.Contains "/Artifact" then insideArtifact <- insideArtifact + 1
                    | ContentOperators.EMC ->
                        if insideArtifact > 0 then insideArtifact <- insideArtifact - 1
                    | _ -> ()
                    match owners[i] with
                    | Some owner when insideArtifact = 0 && current <> Some owner -> openFor owner
                    | _ -> ()
                    copy i i
                    i <- i + 1
            close ()
            // Anything after the last operator (trailing whitespace).
            let last = if operations.Count > 0 then operations[operations.Count - 1].End else 0L
            output.Write(bytes, int last, int (length - last))

            // Each region's element, with the box of what it holds.
            let held = Dictionary<int, Rectangle>()
            for k in 0 .. operations.Count - 1 do
                match owners[k], drawn.TryGetValue k with
                | Some(Region index), (true, pieces) when index >= 0 ->
                    for piece in pieces do
                        held[index] <-
                            match held.TryGetValue index with
                            | true, box -> Rectangle.GetCommonRectangle(box, piece.Box)
                            | _ -> piece.Box
                | _ -> ()
            for KeyValue(index, element) in elements do
                if index >= 0 then
                    tagged.Add
                        { Region = fst regions[index]
                          Tag = (element.Dictionary.GetAsName PdfName.S).GetValue()
                          Box =
                            match held.TryGetValue index with
                            | true, box ->
                                { PageNumber = pageNumber
                                  Left = float (box.GetLeft())
                                  Bottom = float (box.GetBottom())
                                  Right = float (box.GetRight())
                                  Top = float (box.GetTop()) }
                            | _ -> null }

            // The page's elements in reading order (leftover text last), consecutive list
            // items with the same numbering in one list.
            let mutable currentList: (Element * string) option = None
            // (A table's cells are in its rows already.)
            for index in elements.Keys |> Seq.filter (not << tableOf.ContainsKey) |> Seq.sortBy (fun i -> if i < 0 then Int32.MaxValue else i) |> List.ofSeq do
                match listItems.TryGetValue index with
                | true, (item, numbering) ->
                    let list =
                        match currentList with
                        | Some(list, current) when current = numbering -> list
                        | _ ->
                            let list = newElement document Tags.L documentElement.Dictionary None
                            documentElement.Kids.Add list.Dictionary
                            if numbering <> "" then
                                let attributes = PdfDictionary()
                                attributes.Put(PdfName.O, PdfName.List) |> ignore
                                attributes.Put(PdfName.ListNumbering, PdfName numbering) |> ignore
                                list.Dictionary.Put(PdfName.A, attributes) |> ignore
                            currentList <- Some(list, numbering)
                            list
                    item.Dictionary.Put(PdfName.P, list.Dictionary) |> ignore
                    list.Kids.Add item.Dictionary
                | _ ->
                    currentList <- None
                    match qrLinks |> Seq.tryFind (fun (i, _, _) -> i = index) with
                    | Some(_, _, wrapper) -> documentElement.Kids.Add wrapper.Dictionary
                    | None -> documentElement.Kids.Add elements[index].Dictionary

            let stream = PdfStream(output.ToArray())
            stream.SetCompressionLevel CompressionConstants.DEFAULT_COMPRESSION
            page.GetPdfObject().Put(PdfName.Contents, stream.MakeIndirect document) |> ignore
            page.GetPdfObject().Put(PdfName.StructParents, PdfNumber(pageNumber - 1)) |> ignore
            let pageElements = PdfArray()
            for element in mcids do pageElements.Add element
            parentTree.Add(PdfNumber(pageNumber - 1))
            parentTree.Add pageElements
            tally.Pages <- tally.Pages + 1
            if not pageHasText then
                tally.PagesWithoutText <- tally.PagesWithoutText + 1

            // Links: a Link element in the region the link is over, holding the annotation.
            let annotations = page.GetAnnotations()
            let mutable hasLinks = false
            for annotation in annotations do
                let dictionary = annotation.GetPdfObject()
                if PdfName.Link.Equals(annotation.GetSubtype()) && not (isNull (annotation.GetRectangle())) then
                    let rectangle = annotation.GetRectangle().ToRectangle()
                    let point = centre rectangle
                    let parent =
                        regions
                        |> Array.indexed
                        |> Array.filter (fun (i, (_, r)) -> containsPoint r point && elements.ContainsKey i)
                        |> Array.sortBy (fun (_, (_, r)) -> area r)
                        |> Array.tryHead
                        |> Option.map (fun (i, _) -> elements[i])
                        |> Option.defaultValue documentElement
                    let link = newElement document Tags.Link parent.Dictionary (Some page)
                    parent.Kids.Add link.Dictionary
                    let reference = PdfDictionary()
                    reference.Put(PdfName.Type, PdfName.OBJR) |> ignore
                    reference.Put(PdfName.Obj, dictionary) |> ignore
                    reference.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
                    link.Kids.Add reference
                    objectReferences.Add((dictionary, link.Dictionary))
                    tally.Links <- tally.Links + 1
                    hasLinks <- true
                    // A description for a link without one, if the source has it.
                    if isNull (dictionary.Get PdfName.Contents) then
                        let geometry: StructureGeometry =
                            { PageNumber = pageNumber
                              Left = float (rectangle.GetLeft())
                              Bottom = float (rectangle.GetBottom())
                              Right = float (rectangle.GetRight())
                              Top = float (rectangle.GetTop()) }
                        match source.LinkDescription(pdfPath, pageNumber, geometry) with
                        | NonNull description when not (String.IsNullOrWhiteSpace description) ->
                            dictionary.Put(PdfName.Contents, PdfString(description.Trim(), PdfEncodings.UNICODE_BIG)) |> ignore
                            tally.LinksDescribed <- tally.LinksDescribed + 1
                        | _ -> ()
                elif not (PdfName.Popup.Equals(annotation.GetSubtype())) then
                    tally.UntaggedAnnotations <- tally.UntaggedAnnotations + 1
            // Links over QR codes, added after the page's own were tagged. No border, so the
            // page looks the same.
            for index, address, wrapper in qrLinks do
                let annotation = PdfLinkAnnotation(rectangleOf (fst regions[index]).Geometry)
                annotation.SetAction(iText.Kernel.Pdf.Action.PdfAction.CreateURI address) |> ignore
                annotation.SetBorder(PdfArray([| 0.0; 0.0; 0.0 |])) |> ignore
                annotation.SetFlags PdfAnnotation.PRINT |> ignore
                // What a person wrote for the figure says where it goes better than its address.
                let contents =
                    match (fst regions[index]).Alt, pageTitle address with
                    | NonNull alt, _ when not (String.IsNullOrWhiteSpace alt) -> alt.Trim()
                    | _, Some title -> $"QR code link to {title} ({address})"
                    | _, None -> $"QR code link to {address}"
                annotation.SetContents(PdfString(contents, PdfEncodings.UNICODE_BIG)) |> ignore
                page.AddAnnotation annotation |> ignore
                let dictionary = annotation.GetPdfObject()
                let reference = PdfDictionary()
                reference.Put(PdfName.Type, PdfName.OBJR) |> ignore
                reference.Put(PdfName.Obj, dictionary) |> ignore
                reference.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
                wrapper.Kids.Add reference
                objectReferences.Add((dictionary, wrapper.Dictionary))
                tally.QrLinks <- tally.QrLinks + 1
                hasLinks <- true
            if hasLinks then
                page.GetPdfObject().Put(PdfName.Tabs, PdfName.S) |> ignore

        for i, (annotation, element) in Seq.indexed objectReferences do
            let key = pageCount + i
            annotation.Put(PdfName.StructParent, PdfNumber key) |> ignore
            parentTree.Add(PdfNumber key)
            parentTree.Add element

        let parentTreeDictionary = PdfDictionary()
        parentTreeDictionary.Put(PdfName.Nums, parentTree) |> ignore
        root.Put(PdfName.ParentTree, parentTreeDictionary.MakeIndirect document) |> ignore
        root.Put(PdfName.ParentTreeNextKey, PdfNumber(pageCount + objectReferences.Count)) |> ignore
        catalog.Put(PdfName.StructTreeRoot, root) |> ignore
        let markInfo = PdfDictionary()
        markInfo.Put(PdfName.Marked, PdfBoolean.TRUE) |> ignore
        catalog.Put(PdfName.MarkInfo, markInfo) |> ignore

        // A title from the source, shown in place of the file name.
        let titled =
            match source.DocumentTitle with
            | NonNull title when not (String.IsNullOrWhiteSpace title) && title.Trim() <> (document.GetDocumentInfo().GetTitle() |> Option.ofObj |> Option.defaultValue "") ->
                document.GetDocumentInfo().SetTitle(title.Trim()) |> ignore
                let preferences =
                    match catalog.GetAsDictionary PdfName.ViewerPreferences with
                    | null ->
                        let preferences = PdfDictionary()
                        catalog.Put(PdfName.ViewerPreferences, preferences) |> ignore
                        preferences
                    | preferences -> preferences
                preferences.Put(PdfName.DisplayDocTitle, PdfBoolean.TRUE) |> ignore
                Some(title.Trim())
            | _ -> None

        let plural n (one: string) (many: string) = if n = 1 then $"1 {one}" else $"{n} {many}"
        let lines =
            let headings = plural tally.Headings "heading" "headings"
            let paragraphs = plural tally.Paragraphs "paragraph" "paragraphs"
            let items = plural tally.ListItems "list item" "list items"
            let figures = plural tally.Figures "figure" "figures"
            let links = plural tally.Links "link" "links"
            let pages = plural tally.Pages "page" "pages"
            [ yield
                  $"Tagged {pages} from their layout: {headings}, {paragraphs}, {items}, {figures} "
                  + $"and {links}; page furniture marked as artifacts"
              if tally.Recognized > 0 then
                  let recognized = plural tally.Recognized "element's text is" "elements' text is"
                  yield $"{recognized} drawn as outlines or pixels: read with OCR into /ActualText, to check"
              if tally.QrCodes > 0 then
                  let described = plural tally.QrCodes "QR code" "QR codes"
                  yield $"Described {described} by what they encode"
              if tally.QrTitles > 0 then
                  let named = plural tally.QrTitles "QR code's description names" "QR codes' descriptions name"
                  yield $"{named} the title of the web page it goes to, read from the web: to check"
              if tally.QrLinks > 0 then
                  let linked = plural tally.QrLinks "QR code" "QR codes"
                  yield $"Linked {linked} to the web address they encode"
              match titled with
              | Some title -> yield $"Titled the document \"{title}\", as reviewed"
              | None -> ()
              if tally.LinksDescribed > 0 then
                  let described = plural tally.LinksDescribed "link" "links"
                  yield $"Described {described} as reviewed"
              if tally.WrittenAlt > 0 then
                  let given = plural tally.WrittenAlt "figure" "figures"
                  yield $"Gave {given} the alt text a person wrote"
              if tally.Figures > tally.QrCodes + tally.WrittenAlt then
                  let need = plural (tally.Figures - tally.QrCodes - tally.WrittenAlt) "figure needs" "figures need"
                  yield $"{need} alt text, written by a person"
              if tally.TablesWithCells > 0 then
                  let tables = plural tally.TablesWithCells "table" "tables"
                  yield $"Tagged {tables} with rows and cells, from their layout: to check, the header row above all"
              if tally.Tables > 0 then
                  let were = plural tally.Tables "table was" "tables were"
                  yield $"{were} tagged as text: rows and cells need tagging by hand"
              if tally.UntaggedAnnotations > 0 then
                  let annotations = plural tally.UntaggedAnnotations "annotation" "annotations"
                  yield $"{annotations} other than links left untagged"
              if tally.PagesWithoutText > 0 then
                  let scans = plural tally.PagesWithoutText "page has" "pages have"
                  yield $"{scans} no text (a scan?): it needs OCR before it can be read" ]
        lines, List.ofSeq tagged
