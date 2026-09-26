/// Tags an untagged document from its pages' layout: regions (a heading, a paragraph, a
/// figure...) found by a layout detector, in reading order. Each piece of page content goes
/// to the region it's drawn in, and is marked in the page's content stream as that region's
/// content, or as an artifact (page furniture, decoration). The content stream is rewritten
/// by copying every original operator's bytes unchanged and inserting only the marked-content
/// operators (BDC/BMC/EMC) between them, so what the page draws doesn't change. Then the
/// structure tree is built: a Document of the regions' elements, page by page.
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

// ---- Where each piece of content is drawn ----

type private ContentKind =
    | Text
    | Image
    | Path

/// Something an operator draws, and its bounding box in user space.
type private Drawn = { Kind: ContentKind; Box: Rectangle }

let private boxOf (points: (float32 * float32) seq) =
    let points = Array.ofSeq points
    if points.Length = 0 then
        None
    else
        let xs = points |> Array.map fst
        let ys = points |> Array.map snd
        let left, bottom = Array.min xs, Array.min ys
        Some(Rectangle(left, bottom, Array.max xs - left, Array.max ys - bottom))

let private transform (matrix: Matrix) (x: float32, y: float32) =
    let v = Vector(x, y, 1f).Cross matrix
    v.Get Vector.I1, v.Get Vector.I2

/// Records what each top-level operator of a content stream draws. Content inside a form
/// XObject counts as its Do operator's.
type private DrawnByOperator() =
    let drawn = Dictionary<int, List<Drawn>>()
    let mutable current = -1

    member val Depth = 0 with get, set
    member _.Current with set value = current <- value
    member _.Drawn = drawn

    member private _.Add(kind, box: Rectangle option) =
        match box with
        | Some box when current >= 0 ->
            match drawn.TryGetValue current with
            | true, list -> list.Add { Kind = kind; Box = box }
            | _ -> drawn[current] <- List [ { Kind = kind; Box = box } ]
        | _ -> ()

    interface IEventListener with
        member this.EventOccurred(data, eventType) =
            match eventType, data with
            | EventType.RENDER_TEXT, (:? TextRenderInfo as text) ->
                let points =
                    [ for line in [ text.GetAscentLine(); text.GetDescentLine() ] do
                          for point in [ line.GetStartPoint(); line.GetEndPoint() ] -> point.Get Vector.I1, point.Get Vector.I2 ]
                this.Add(Text, boxOf points)
            | EventType.RENDER_IMAGE, (:? ImageRenderInfo as image) ->
                let ctm = image.GetImageCtm()
                this.Add(Image, boxOf [ for corner in [ 0f, 0f; 1f, 0f; 0f, 1f; 1f, 1f ] -> transform ctm corner ])
            | EventType.RENDER_PATH, (:? PathRenderInfo as path) when path.GetOperation() <> PathRenderInfo.NO_OP ->
                let ctm = path.GetCtm()
                let points =
                    [ for subpath in path.GetPath().GetSubpaths() do
                          for segment in subpath.GetSegments() do
                              for point in segment.GetBasePoints() -> transform ctm (float32 (point.GetX()), float32 (point.GetY())) ]
                this.Add(Path, boxOf points)
            | _ -> ()

        member _.GetSupportedEvents() = null

/// Wraps an operator so the recorder knows which top-level operator is running.
type private CountingOperator(inner: IContentOperator, recorder: DrawnByOperator, next: unit -> int) =
    interface IContentOperator with
        member _.Invoke(processor, operator, operands) =
            if recorder.Depth = 0 then
                recorder.Current <- next ()
            recorder.Depth <- recorder.Depth + 1
            try
                inner.Invoke(processor, operator, operands)
            finally
                recorder.Depth <- recorder.Depth - 1

let private drawnByOperator (bytes: byte array) (resources: PdfResources) =
    let recorder = DrawnByOperator()
    let processor = PdfCanvasProcessor(recorder)
    let mutable index = -1
    let next () =
        index <- index + 1
        index
    for name in List.ofSeq (processor.GetRegisteredOperatorStrings()) do
        match processor.RegisterContentOperator(name, null) with
        | null -> ()
        | inner -> processor.RegisterContentOperator(name, CountingOperator(inner, recorder, next)) |> ignore
    processor.ProcessContent(bytes, resources)
    recorder.Drawn

// ---- The content stream's operators ----

type private Operation =
    { Name: string
      /// The operator's bytes in the original stream, operands included.
      Start: int64
      End: int64 }

/// Splits a content stream into its operators, the way PdfCanvasProcessor reads it (so the
/// indexes line up), keeping each one's position in the bytes.
let private operationsOf (bytes: byte array) (resources: PdfResources) =
    use tokenizer = new PdfTokenizer(RandomAccessFileOrArray(RandomAccessSourceFactory().CreateSource bytes))
    let parser = PdfCanvasParser(tokenizer, resources)
    let operands = List<PdfObject>()
    let operations = List<Operation>()
    let mutable start = 0L
    parser.Parse operands |> ignore
    while operands.Count > 0 do
        let name = string operands[operands.Count - 1]
        if name = "BI" then
            // The inline image's dictionary and data, through EI.
            InlineImageParsingUtils.Parse(parser, resources.GetResource PdfName.ColorSpace) |> ignore
        operations.Add { Name = name; Start = start; End = tokenizer.GetPosition() }
        start <- tokenizer.GetPosition()
        parser.Parse operands |> ignore
    operations, bytes.LongLength

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

let private isTextRegion (tag: string) = tag <> "Artifact" && tag <> "Figure"

/// The owner of one piece of content: the smallest region it's centred in. Text outside
/// every region still reads as content, in the nearest text region; images and drawings
/// outside every region are decoration.
let private ownerOf (regions: (LayoutRegion * Rectangle) array) (drawn: Drawn) =
    let point = centre drawn.Box
    let inside =
        regions
        |> Array.indexed
        |> Array.filter (fun (_, (_, r)) -> containsPoint r point)
        |> Array.sortBy (fun (_, (_, r)) -> area r)
        |> Array.tryHead
    match drawn.Kind, inside with
    | Text, Some(index, (region, _))
    | Image, Some(index, (region, _)) -> if region.Tag = "Artifact" then Artifact else Region index
    // Lines and fills are decoration, except in a figure or table (a chart, a table's rules),
    // or where they draw a region's text (outlined text, read by OCR into its ActualText).
    | Path, Some(index, (region, _)) when region.Tag = "Figure" || region.Tag = "Table" || not (isNull region.ActualText) ->
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
let private operatorOwner regions pageHasText (drawn: List<Drawn>) =
    drawn
    |> Seq.map (fun piece ->
        match piece.Kind, ownerOf regions piece with
        | Image, Region _ when not pageHasText -> Region PageFigure
        | _, owner -> owner)
    |> Seq.countBy id
    |> Seq.maxBy snd
    |> fst

let private pathConstruction = set [ "m"; "l"; "c"; "v"; "y"; "h"; "re"; "W"; "W*" ]
let private pathPainting = set [ "S"; "s"; "f"; "F"; "f*"; "B"; "B*"; "b"; "b*"; "n" ]
/// Operators a marked-content sequence mustn't straddle.
let private boundaries = set [ "BT"; "ET"; "q"; "Q"; "BMC"; "BDC"; "EMC"; "BX"; "EX" ]

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
      mutable LinksDescribed: int }

/// The tag written for a region, given whether the document has a title (H1) already.
let private writtenTag (region: LayoutRegion) hasTitle =
    match region.Tag with
    | "H1" -> "H1"
    // Levels below the title aren't known from the layout; one level under it keeps the
    // outline from skipping levels.
    | "H" -> if hasTitle then "H2" else "H1"
    | "Figure" -> "Figure"
    | "Note" -> "Note"
    | "LI" -> "LI"
    // A table's rows and cells can't be found from its outline; its text reads as text, and
    // the report says it needs tagging by hand.
    | _ -> "P"

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
    : string list =
    use reader = new PdfReader(pdfPath)
    use writer = new PdfWriter(outputPath)
    use document = new PdfDocument(reader, writer)
    let catalog = document.GetCatalog().GetPdfObject()
    if not (isNull (catalog.Get PdfName.StructTreeRoot)) then
        []
    else
        let root = PdfDictionary()
        root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
        root.MakeIndirect document |> ignore
        let documentElement = newElement document "Document" root None
        root.Put(PdfName.K, documentElement.Dictionary) |> ignore

        let parentTree = PdfArray()
        let tally =
            { Pages = 0; Headings = 0; Paragraphs = 0; ListItems = 0; Figures = 0; Tables = 0; Links = 0; UntaggedAnnotations = 0; PagesWithoutText = 0; Recognized = 0; QrCodes = 0; QrLinks = 0; QrTitles = 0; WrittenAlt = 0; LinksDescribed = 0 }
        let pageCount = document.GetNumberOfPages()
        let regionsByPage =
            [| for page in 1 .. pageCount ->
                   onPage page pageCount
                   List.ofSeq (source.PageRegions(pdfPath, page)) |]
        let hasTitle = regionsByPage |> Array.exists (List.exists (fun region -> region.Tag = "H1"))
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

        for pageNumber in 1 .. pageCount do
            let page = document.GetPage pageNumber
            let resources = page.GetResources()
            let bytes = page.GetContentBytes()
            let regions = [| for region in regionsByPage[pageNumber - 1] -> region, rectangleOf region.Geometry |]
            let drawn = drawnByOperator bytes resources
            let operations, length = operationsOf bytes resources

            // Each operator's owner; a path's construction goes with the operator that paints it.
            let pageHasText = drawn.Values |> Seq.exists (Seq.exists (fun piece -> piece.Kind = Text))
            let owners = Array.create operations.Count None
            for i in 0 .. operations.Count - 1 do
                match drawn.TryGetValue i with
                | true, pieces when operations[i].Name <> "n" -> owners[i] <- Some(operatorOwner regions pageHasText pieces)
                | _ when operations[i].Name = "sh" -> owners[i] <- Some Artifact
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
                | LeftoverText -> { Tag = "P"; Geometry = Unchecked.defaultof<_>; ListNumbering = null; ActualText = null; QrText = null; Alt = null }
                | PageFigure -> { Tag = "Figure"; Geometry = Unchecked.defaultof<_>; ListNumbering = null; ActualText = null; QrText = null; Alt = null }
                | index -> fst regions[index]
            let elementFor (index: int) =
                match elements.TryGetValue index with
                | true, element -> element
                | _ ->
                    let region = regionAt index
                    let element =
                        match writtenTag region hasTitle with
                        | "LI" ->
                            // Its list is found when the page's elements are put in order.
                            let item = newElement document "LI" documentElement.Dictionary None
                            let body = newElement document "LBody" item.Dictionary (Some page)
                            item.Kids.Add body.Dictionary
                            listItems[index] <- (item, (match region.ListNumbering with NonNull numbering -> numbering | Null -> ""))
                            tally.ListItems <- tally.ListItems + 1
                            body
                        | tag ->
                            let element = newElement document tag documentElement.Dictionary (Some page)
                            match tag with
                            | "H1"
                            | "H2" -> tally.Headings <- tally.Headings + 1
                            | "Figure" ->
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
                                        let wrapper = newElement document "Link" documentElement.Dictionary (Some page)
                                        // So is its annotation's /Contents.
                                        wrapper.Dictionary.Put(PdfName.Lang, PdfString "en") |> ignore
                                        element.Dictionary.Put(PdfName.P, wrapper.Dictionary) |> ignore
                                        wrapper.Kids.Add element.Dictionary
                                        qrLinks.Add((index, address, wrapper))
                                    | _ -> ()
                                | Null -> ()
                            | "Note" ->
                                // PDF/UA wants every Note to have an ID.
                                noteIds <- noteIds + 1
                                element.Dictionary.Put(PdfName.ID, PdfString $"note-{noteIds}") |> ignore
                            | _ ->
                                if region.Tag = "Table" then tally.Tables <- tally.Tables + 1
                                tally.Paragraphs <- tally.Paragraphs + 1
                            element
                    match region.ActualText with
                    | NonNull text ->
                        element.Dictionary.Put(PdfName.ActualText, PdfString(text, PdfEncodings.UNICODE_BIG)) |> ignore
                        // OCR reads with its English model, so the text it gives is English.
                        element.Dictionary.Put(PdfName.Lang, PdfString "en") |> ignore
                        tally.Recognized <- tally.Recognized + 1
                    | Null -> ()
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
                | "q" -> saves.Push k
                | "Q" when saves.Count > 0 -> ends[saves.Pop()] <- k
                | "BT" -> textStart <- k
                | "ET" when textStart >= 0 ->
                    ends[textStart] <- k
                    textStart <- -1
                | _ -> ()
            // Where each operator's owner run and marked-content count stand, to test a block
            // in constant time.
            let markedBefore = Array.zeroCreate (operations.Count + 1)
            for k in 0 .. operations.Count - 1 do
                let name = operations[k].Name
                markedBefore[k + 1] <- markedBefore[k] + (if name = "BMC" || name = "BDC" || name = "EMC" then 1 else 0)
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
                    | "BMC"
                    | "BDC" ->
                        let text = Encoding.Latin1.GetString(bytes, int operation.Start, int (operation.End - operation.Start))
                        if insideArtifact > 0 || text.Contains "/Artifact" then insideArtifact <- insideArtifact + 1
                    | "EMC" ->
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

            // The page's elements in reading order (leftover text last), consecutive list
            // items with the same numbering in one list.
            let mutable currentList: (Element * string) option = None
            for index in elements.Keys |> Seq.sortBy (fun i -> if i < 0 then Int32.MaxValue else i) |> List.ofSeq do
                match listItems.TryGetValue index with
                | true, (item, numbering) ->
                    let list =
                        match currentList with
                        | Some(list, current) when current = numbering -> list
                        | _ ->
                            let list = newElement document "L" documentElement.Dictionary None
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
                    let link = newElement document "Link" parent.Dictionary (Some page)
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
                let contents =
                    match pageTitle address with
                    | Some title -> $"QR code link to {title} ({address})"
                    | None -> $"QR code link to {address}"
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
          | Some title -> yield $"Titled the document \"{title}\", as set in Suggest Structure"
          | None -> ()
          if tally.LinksDescribed > 0 then
              let described = plural tally.LinksDescribed "link" "links"
              yield $"Described {described} as reviewed in Suggest Structure"
          if tally.WrittenAlt > 0 then
              let given = plural tally.WrittenAlt "figure" "figures"
              yield $"Gave {given} the alt text written in Suggest Structure"
          if tally.Figures > tally.QrCodes + tally.WrittenAlt then
              let need = plural (tally.Figures - tally.QrCodes - tally.WrittenAlt) "figure needs" "figures need"
              yield $"{need} alt text, written by a person"
          if tally.Tables > 0 then
              let were = plural tally.Tables "table was" "tables were"
              yield $"{were} tagged as text: rows and cells need tagging by hand"
          if tally.UntaggedAnnotations > 0 then
              let annotations = plural tally.UntaggedAnnotations "annotation" "annotations"
              yield $"{annotations} other than links left untagged"
          if tally.PagesWithoutText > 0 then
              let scans = plural tally.PagesWithoutText "page has" "pages have"
              yield $"{scans} no text (a scan?): it needs OCR before it can be read" ]
