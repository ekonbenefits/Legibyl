/// Content a tagged page draws outside every tag: a background fill, rules under headings, a
/// border, stray text. PDF/UA wants all of it either tagged or marked as an artifact (decoration,
/// not read), and only a person can tell a background from a chart, so nothing here decides:
/// this finds the pieces (strokes drawn together, grouped), shows them with the structure (a
/// "NotInTags" group per page, see nodes), and writes what a person chose for each (see apply):
/// not read (an artifact), or tagged as a figure or a paragraph.
module internal Legibyl.Validation.UntaggedContent

open System
open System.Collections.Generic
open System.IO
open System.Text
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Tagging
open Legibyl.Validation.ContentOperations
open Legibyl.Validation.StructureTreeObjects

/// What a piece is.
type Kind =
    | Fill
    | Line
    | Image
    | Text
    /// Several kinds drawn together.
    | Drawing

    member kind.Name =
        match kind with
        | Fill -> "fill"
        | Line -> "line"
        | Image -> "image"
        | Text -> "text"
        | Drawing -> "drawing"

/// Operators drawing one thing outside every tag, first to last (as operationsOf numbers them).
type Unit = { First: int; Last: int; Kind: Kind; Box: Rectangle }

/// Things drawn together outside every tag, on a page.
type Piece =
    { Page: int
      Kind: Kind
      Box: StructureGeometry
      Units: Unit list }

/// The most the gap between two things drawn one after the other can be, in points, for them to
/// be one piece (a border in many strokes).
[<Literal>]
let private MaxGap = 6.0

let private geometry (page: int) (box: Rectangle) : StructureGeometry =
    { PageNumber = page
      Left = float (box.GetLeft())
      Bottom = float (box.GetBottom())
      Right = float (box.GetRight())
      Top = float (box.GetTop()) }

let private gap (a: Rectangle) (b: Rectangle) =
    let dx = max 0f (max (a.GetLeft() - b.GetRight()) (b.GetLeft() - a.GetRight()))
    let dy = max 0f (max (a.GetBottom() - b.GetTop()) (b.GetBottom() - a.GetTop()))
    float (max dx dy)

let private union (a: Rectangle) (b: Rectangle) = Rectangle.GetCommonRectangle(a, b)

/// At least 2 pt each way (a hairline's box has no height), so it can be seen and found again.
let private visible (box: Rectangle) =
    let grow (size: float32) = max 0f (2f - size) / 2f
    let dx, dy = grow (box.GetWidth()), grow (box.GetHeight())
    Rectangle(box.GetX() - dx, box.GetY() - dy, box.GetWidth() + 2f * dx, box.GetHeight() + 2f * dy)

/// The box around all of boxes, if there are any.
let private around (boxes: Rectangle list) = match boxes with [] -> None | boxes -> Some(visible (List.reduce union boxes))

let private fills = set [ "f"; "F"; "f*"; "B"; "B*"; "b"; "b*" ]

/// Each operator's place: how deep in marked content (0: outside every tag).
let private depths (operations: List<Operation>) =
    let depth = Array.zeroCreate operations.Count
    let mutable d = 0
    for i in 0 .. operations.Count - 1 do
        depth[i] <- d
        match operations[i].Name with
        | "BMC"
        | "BDC" -> d <- d + 1
        | "EMC" when d > 0 -> d <- d - 1
        | _ -> ()
    depth

/// The units a page's content draws outside every tag, in order, each with the run of content
/// between two top-level marked-content sequences it's in.
let private units (operations: List<Operation>) (drawn: Dictionary<int, List<Drawn>>) (pageBox: Rectangle) =
    let depth = depths operations
    let boxOf (first: int) (last: int) =
        [ for i in first .. last do
              match drawn.TryGetValue i with
              | true, pieces -> yield! pieces
              | _ -> () ]
    let found = List<int * Unit>()
    let mutable run = 0
    let mutable i = 0
    while i < operations.Count do
        let name = operations[i].Name
        if depth[i] = 0 && (name = "BMC" || name = "BDC") then
            run <- run + 1
            i <- i + 1
        elif depth[i] > 0 then
            i <- i + 1
        elif name = "BT" then
            // A text object outside every tag, if nothing in it is marked.
            let mutable last = i
            while last + 1 < operations.Count && operations[last].Name <> "ET" do
                last <- last + 1
            let marked = seq { i .. last } |> Seq.exists (fun k -> operations[k].Name = "BMC" || operations[k].Name = "BDC")
            let pieces = boxOf i last |> List.filter (fun d -> d.Kind = ContentKind.Text)
            if not marked && not pieces.IsEmpty then
                found.Add(run, { First = i; Last = last; Kind = Text; Box = pieces |> List.map _.Box |> List.reduce union })
            i <- last + 1
        else
            let add first kind (box: Rectangle option) =
                match box with
                | Some box -> found.Add(run, { First = first; Last = i; Kind = kind; Box = box })
                | None -> ()
            if pathPainting.Contains name && name <> "n" then
                let mutable first = i
                while first > 0 && pathConstruction.Contains operations[first - 1].Name do
                    first <- first - 1
                let box = boxOf i i |> List.map _.Box |> around
                add first (if fills.Contains name then Fill else Line) box
            elif name = "sh" then
                // A shading fills what's clipped, often the page.
                add i Fill (Some pageBox)
            elif name = "BI" || name = "Do" then
                let pieces = boxOf i i
                let kind =
                    if pieces |> List.exists (fun d -> d.Kind = ContentKind.Text) then Text
                    elif pieces |> List.forall (fun d -> d.Kind = ContentKind.Image) then Image
                    else Drawing
                add i kind (pieces |> List.map _.Box |> around)
            i <- i + 1
    found

/// A page's pieces: units drawn one after another in the same run, near each other, and all
/// text or none of it.
let piecesOn (page: PdfPage) (pageNumber: int) : Piece list =
    let bytes = page.GetContentBytes()
    let resources = page.GetResources()
    let operations, _ = operationsOf bytes resources
    let drawn = drawnByOperator bytes resources
    let pieces = List<Piece>()
    let mutable current: (int * Kind * Rectangle * Unit list) option = None
    let finish () =
        match current with
        | Some(_, kind, box, found) -> pieces.Add { Page = pageNumber; Kind = kind; Box = geometry pageNumber box; Units = List.rev found }
        | None -> ()
        current <- None
    for run, unit in units operations drawn (page.GetCropBox()) do
        match current with
        | Some(currentRun, kind, box, found) when
            currentRun = run && (kind = Text) = (unit.Kind = Text) && gap box unit.Box <= MaxGap
            ->
            current <- Some(run, (if kind = unit.Kind then kind else Drawing), union box unit.Box, unit :: found)
        | _ ->
            finish ()
            current <- Some(run, unit.Kind, unit.Box, [ unit ])
    finish ()
    List.ofSeq pieces

/// Whether a document is one whose content can be outside its tags: tagged.
let private tagged (document: PdfDocument) =
    not (isNull (document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot))

/// Every page's pieces, for a tagged document.
let pieces (document: PdfDocument) : Piece list =
    if not (tagged document) then []
    else [ for n in 1 .. document.GetNumberOfPages() do yield! piecesOn (document.GetPage n) n ]

// ---- veraPDF's findings about them ----

/// Each of a page's content items outside every tag as veraPDF numbers them in its findings
/// ("content[N]/contentItem[k]": each top-level marked-content sequence is one content[N], and
/// so is each run of content between them; in a run, each text-showing operator, painted path,
/// shading, inline image or XObject is an item), with the box of what it draws.
let private items (page: PdfPage) : IReadOnlyDictionary<int * int, Rectangle> =
    let bytes = page.GetContentBytes()
    let resources = page.GetResources()
    let operations, _ = operationsOf bytes resources
    let drawn = drawnByOperator bytes resources
    let depth = depths operations
    let boxes = Dictionary<int * int, Rectangle>()
    let mutable chunk = -1
    let mutable item = 0
    let mutable inRun = false
    for i in 0 .. operations.Count - 1 do
        let name = operations[i].Name
        if depth[i] = 0 && (name = "BMC" || name = "BDC") then
            chunk <- chunk + 1
            inRun <- false
        elif depth[i] = 0 then
            let isItem =
                textShowing.Contains name || (pathPainting.Contains name && name <> "n") || name = "sh" || name = "BI" || name = "Do"
            if isItem then
                if not inRun then
                    chunk <- chunk + 1
                    item <- 0
                    inRun <- true
                match drawn.TryGetValue i with
                | true, pieces when pieces.Count > 0 -> boxes[(chunk, item)] <- pieces |> Seq.map _.Box |> Seq.reduce union
                | _ when name = "sh" -> boxes[(chunk, item)] <- page.GetCropBox()
                | _ -> ()
                item <- item + 1
    boxes

let private itemsByDocument = Runtime.CompilerServices.ConditionalWeakTable<PdfDocument, Dictionary<int, IReadOnlyDictionary<int * int, Rectangle>>>()

/// Where a content item outside every tag that veraPDF names ("content[N]/contentItem[k]" on
/// page) is drawn, if it's known.
let itemGeometry (document: PdfDocument) (page: int) (content: int) (contentItem: int) : StructureGeometry option =
    if page < 1 || page > document.GetNumberOfPages() then
        None
    else
        let pages = itemsByDocument.GetValue(document, fun _ -> Dictionary())
        let pageItems =
            match pages.TryGetValue page with
            | true, found -> found
            | _ ->
                let found = try items (document.GetPage page) with _ -> Dictionary() :> IReadOnlyDictionary<_, _>
                pages[page] <- found
                found
        match pageItems.TryGetValue((content, contentItem)) with
        | true, box -> Some(geometry page box)
        | _ -> None

// ---- As the structure shows them ----

let private node (tag: string) (content: string | null) (geometry: StructureGeometry | null) (page: int) (children: StructureNode list) : StructureNode =
    { Tag = tag
      Role = null
      AlternateDescription = null
      ActualText = null
      Content = content
      FormFieldName = null
      FormFieldDescription = null
      FormFieldType = null
      FormFieldValue = null
      LinkUri = null
      LinkDestinationPageNumber = Nullable()
      LinkDestinationDescription = null
      LinkDestination = null
      LinkDescription = null
      Geometry = geometry
      Language = null
      PageNumber = Nullable page
      ContentLeafCount = (if children.IsEmpty then 1 else 0)
      Children = List.toArray children }

/// A piece as a person reads it: "a fill, 612 × 792 pt (the whole page)", or its text.
let describe (piece: Piece) (text: string option) (pageBox: Rectangle) =
    let n (value: float) = Math.Round(value).ToString(Globalization.CultureInfo.InvariantCulture)
    let width, height = piece.Box.Right - piece.Box.Left, piece.Box.Top - piece.Box.Bottom
    let whole =
        width >= float (pageBox.GetWidth()) * 0.9 && height >= float (pageBox.GetHeight()) * 0.9
    let size = $"{n width} × {n height} pt" + (if whole then " (the whole page)" else "")
    match piece.Kind, text with
    | Text, Some text when not (String.IsNullOrWhiteSpace text) -> $"text \"{text}\""
    | Image, _ -> $"an image, {size}"
    | kind, _ -> $"a {kind.Name}, {size}"

/// The text drawn in a region of a page.
let private textIn (page: PdfPage) (box: StructureGeometry) =
    try
        let region = Rectangle(float32 box.Left, float32 box.Bottom, float32 (box.Right - box.Left), float32 (box.Top - box.Bottom))
        let filter = iText.Kernel.Pdf.Canvas.Parser.Filter.TextRegionEventFilter region
        let strategy =
            iText.Kernel.Pdf.Canvas.Parser.Listener.FilteredTextEventListener(
                iText.Kernel.Pdf.Canvas.Parser.Listener.LocationTextExtractionStrategy(),
                [| filter :> iText.Kernel.Pdf.Canvas.Parser.Filter.IEventFilter |]
            )
        let text = iText.Kernel.Pdf.Canvas.Parser.PdfTextExtractor.GetTextFromPage(page, strategy)
        Some(Text.RegularExpressions.Regex.Replace(text, @"\s+", " ").Trim())
    with _ ->
        None

/// A tagged PDF's content outside its tags, as the structure shows it: for each page that has
/// some, a "NotInTags" group of "Untagged" nodes, one per piece (its box, and what it is in
/// Content). None for an untagged PDF (Suggest Structure tags it all).
let nodes (pdfPath: string) : StructureNode list =
    use document = new PdfDocument(new PdfReader(pdfPath))
    [ for n in 1 .. (if tagged document then document.GetNumberOfPages() else 0) do
          let page = document.GetPage n
          match piecesOn page n with
          | [] -> ()
          | found ->
              let pieces =
                  [ for piece in found ->
                        let text = if piece.Kind = Text then textIn page piece.Box else None
                        node "Untagged" (describe piece text (page.GetCropBox())) piece.Box n [] ]
              node "NotInTags" $"Page {n}: not in the tags" null n pieces ]

// ---- What a person chose for them ----

/// What to make a piece: not read (an artifact), or tagged as a figure (with its alt text, if
/// given) or a paragraph.
type Choice =
    | NotRead
    | TagAs of tag: string * alt: (string | null)

/// The piece on page in about box (its intersection over union at least SameOverlap), if any.
let find (document: PdfDocument) (page: int) (box: StructureGeometry) (sameOverlap: float) : Piece option =
    if page < 1 || page > document.GetNumberOfPages() then
        None
    else
        piecesOn (document.GetPage page) page
        |> List.map (fun piece -> piece, Boxes.overlap piece.Box box)
        |> List.filter (fun (_, o) -> o >= sameOverlap)
        |> List.sortByDescending snd
        |> List.tryHead
        |> Option.map fst

/// The element whose marked content a page draws last before operator `before`, and where, if
/// any: where a piece tagged there goes in reading order (after it).
let private elementBefore (document: PdfDocument) context (pageNumber: int) (operations: List<Operation>) (resources: PdfResources) (before: int) =
    let owners = (ParentTree.tagged document context).MarkedContent
    seq { before - 1 .. -1 .. 0 }
    |> Seq.tryPick (fun i ->
        match mcidOf resources operations[i] with
        | Some mcid ->
            match owners.TryGetValue((pageNumber, mcid)) with
            | true, element -> Some(i, element)
            | _ -> None
        | None -> None)

/// Writes what was chosen for each piece: its drawing wrapped as an artifact (/Artifact BMC …
/// EMC), or as marked content of a new element (a Figure or P) placed in reading order after
/// the tagged content drawn just before it. Text is never made an artifact. Returns how many
/// were written, and why any weren't.
let apply (document: PdfDocument) context (chosen: (Piece * Choice) list) : int * string list =
    let mutable written = 0
    let notWritten = List<string>()
    let root = document.GetStructTreeRoot()
    for pageNumber, onPage in chosen |> List.groupBy (fun (piece, _) -> piece.Page) do
        let page = document.GetPage pageNumber
        let bytes = page.GetContentBytes()
        let resources = page.GetResources()
        let operations, length = operationsOf bytes resources
        // Text before an operator, and after one.
        let before = Dictionary<int, StringBuilder>()
        let after = Dictionary<int, StringBuilder>()
        let add (at: Dictionary<int, StringBuilder>) i (text: string) =
            match at.TryGetValue i with
            | true, builder -> builder.Append text |> ignore
            | _ -> at[i] <- StringBuilder text
        // In the order they're drawn, so those tagged read in that order too: each after what's
        // tagged before it, already or here.
        let mutable taggedHere: (int * PdfDictionary) option = None
        for piece, choice in onPage |> List.sortBy (fun (piece, _) -> piece.Units.Head.First) do
            match choice with
            | NotRead when piece.Kind = Text ->
                notWritten.Add $"Text on page {pageNumber} wasn't made decoration: text isn't hidden. Tag it as a paragraph instead."
            | NotRead ->
                for unit in piece.Units do
                    add before unit.First "\n/Artifact BMC\n"
                    add after unit.Last "\nEMC\n"
                written <- written + 1
            | TagAs(tag, alt) ->
                let element = PdfStructElem(document, PdfName tag, page)
                // After the tagged content drawn just before it; else at the end of the document.
                let first = piece.Units.Head.First
                let previous =
                    match elementBefore document context pageNumber operations resources first, taggedHere with
                    | Some(at, element), Some(hereAt, here) -> Some(if hereAt > at then here else element)
                    | Some(_, element), None -> Some element
                    | None, Some(_, here) -> Some here
                    | None, None -> None
                let placed =
                    match previous with
                    | Some previous ->
                        match PdfStructElem(previous).GetParent() with
                        | :? PdfStructElem as parent ->
                            let index =
                                parent.GetKids()
                                |> Seq.tryFindIndex (fun kid -> match kid with :? PdfStructElem as k -> obj.ReferenceEquals(k.GetPdfObject(), previous) | _ -> false)
                            match index with
                            | Some index ->
                                parent.AddKid(index + 1, element) |> ignore
                                true
                            | None -> false
                        | _ -> false
                    | None -> false
                if not placed then
                    match root.GetKids() |> Seq.tryHead with
                    | Some(:? PdfStructElem as top) -> top.AddKid element |> ignore
                    | _ -> root.AddKid element |> ignore
                match alt with
                | NonNull alt when not (String.IsNullOrWhiteSpace alt) -> element.SetAlt(PdfString(alt, iText.IO.Font.PdfEncodings.UNICODE_BIG)) |> ignore
                | _ -> ()
                taggedHere <- Some((piece.Units |> List.last).Last, element.GetPdfObject())
                for unit in piece.Units do
                    let reference = PdfMcrNumber(page, element)
                    element.AddKid reference |> ignore
                    add before unit.First $"\n/{tag} <</MCID {reference.GetMcid()}>> BDC\n"
                    add after unit.Last "\nEMC\n"
                written <- written + 1
        if before.Count > 0 then
            use output = new MemoryStream()
            let write (text: string) =
                let b = Encoding.ASCII.GetBytes text
                output.Write(b, 0, b.Length)
            for i in 0 .. operations.Count - 1 do
                match before.TryGetValue i with
                | true, text -> write (string text)
                | _ -> ()
                output.Write(bytes, int operations[i].Start, int (operations[i].End - operations[i].Start))
                match after.TryGetValue i with
                | true, text -> write (string text)
                | _ -> ()
            let last = if operations.Count > 0 then operations[operations.Count - 1].End else 0L
            output.Write(bytes, int last, int (length - last))
            page.GetPdfObject().Put(PdfName.Contents, PdfStream(output.ToArray()).MakeIndirect document) |> ignore
    written, List.ofSeq notWritten
