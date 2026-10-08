/// A tagged PDF's table rebuilt from its page: its rows and cells written again as the cells a
/// person checked (found by the table-structure model from how it looks, see TableStructure),
/// and what it holds put in the cell it's drawn in. For a table tagged wrongly: cells outside
/// rows, rows of different lengths, a table that's only paragraphs (PDF/UA's 7.2 table rules;
/// 8.2.5.26). What the page draws doesn't change, only the tags.
module internal Legibyl.Validation.TableRebuild

open System.Collections.Generic
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Tagging
open Legibyl.Validation.Interop
open Legibyl.Validation.StructureTreeObjects

/// The tags that make a table's shape (not what its cells hold).
let private tableTags = set [ Tags.TR; Tags.TH; Tags.TD; Tags.THead; Tags.TBody; Tags.TFoot ]

/// How deep a table's shape (its rows, row groups and cells) is followed; one deeper is refused.
[<Literal>]
let private MostDepth = 64

let private tagOf context (element: PdfStructElem) =
    match standardTag (element.GetPdfObject()) context with
    | NonNull tag -> tag.GetValue()
    | Null -> ""

/// What an element holds, in reading order, below its table's shape (rows, row groups, cells;
/// not a table's own caption): each piece (a marked-content reference, an annotation's, or an
/// element its cells hold, like a paragraph or a link), its parent and place there (iText makes
/// new objects for an element's kids each time it's asked), and where it's drawn (on another page:
/// that page, nowhere on it). And whether it's all of it (not too deep to follow).
let private piecesOf (element: PdfStructElem) context (page: int) =
    let pieces = ResizeArray<PdfStructElem * int * IStructureNode * StructureGeometry option>()
    let mutable complete = true
    let elsewhere (other: int) = Some { PageNumber = other; Left = 0.0; Bottom = 0.0; Right = 0.0; Top = 0.0 }
    let pageOf (pageObject: PdfDictionary | null) =
        match pageObject |> Option.ofObj |> Option.bind (fun p -> Option.ofObj (p.GetIndirectReference())) with
        | Some reference -> lookup context.PageNumberByObject (reference.GetObjNumber()) |> Option.defaultValue page
        | None -> page
    let rec walk (element: PdfStructElem) (depth: int) =
        let kids = List.ofSeq (element.GetKids())
        if depth >= MostDepth then
            if not kids.IsEmpty then complete <- false
        else
            for index, kid in List.indexed kids do
                let add (node: IStructureNode) (geometry: StructureGeometry option) = pieces.Add(element, index, node, geometry)
                match kid with
                | :? PdfStructElem as child ->
                    let tag = tagOf context child
                    if tableTags.Contains tag then walk child (depth + 1)
                    // (A table's caption is the table's, not a cell's: it stays where it is.)
                    elif tag = Tags.Caption && depth = 0 then ()
                    else add child (subtreeGeometry (child.GetPdfObject()) context (Some page))
                | :? PdfObjRef as reference ->
                    match (reference.GetPdfObject() :?> PdfDictionary).GetAsDictionary PdfName.Obj with
                    | NonNull annotation ->
                        // Its page: its own (/P), else the table's.
                        match pageOf (annotation.GetAsDictionary PdfName.P) with
                        | other when other <> page -> add reference (elsewhere other)
                        | _ ->
                            match annotation.GetAsArray PdfName.Rect with
                            | NonNull r when r.Size() = 4 ->
                                let n i = r.GetAsNumber(i).DoubleValue()
                                add reference (Some { PageNumber = page; Left = min (n 0) (n 2); Bottom = min (n 1) (n 3); Right = max (n 0) (n 2); Top = max (n 1) (n 3) })
                            | _ -> add reference None
                    | Null -> add reference None
                | :? PdfMcr as mcr ->
                    // Its page (its own /Pg, else the table's), and where it's drawn there.
                    match pageOf (mcr.GetPageObject()) with
                    | other when other <> page -> add mcr (elsewhere other)
                    | _ -> add mcr (lookup context.MarkedContent (page, mcr.GetMcid()) |> Option.bind _.Geometry)
                | _ -> ()
    walk element 0
    List.ofSeq pieces, complete

/// cells in the order a rebuild writes them (rows down, then across), each with its place.
let private ordered (cells: TableCell list) =
    List.zip cells (TableGrid.place [ for cell in cells -> cell.Box ]) |> List.sortBy (fun (_, p) -> p.Row, p.Column)

/// A cell's table attributes, as a rebuild writes them: its row and column spans, and a header
/// cell's scope.
let private spansOf (element: PdfStructElem) =
    let attributes =
        match element.GetPdfObject().Get(PdfName.A, true) with
        | :? PdfDictionary as a -> Some a
        | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do match array.Get(i, true) with :? PdfDictionary as a -> a | _ -> () ] |> List.tryHead
        | _ -> None
    let number (key: PdfName) = attributes |> Option.bind (fun a -> Option.ofObj (a.GetAsNumber key)) |> Option.map _.IntValue() |> Option.defaultValue 1
    let scope = attributes |> Option.bind (fun a -> Option.ofObj (a.GetAsName PdfName.Scope)) |> Option.map _.GetValue()
    number PdfName.RowSpan, number PdfName.ColSpan, scope

/// Whether a table is already as a rebuild with cells would write it: its rows (but a caption)
/// those rows of those cells, in order, each with its tag, spans and scope, and what each holds
/// drawn where the rebuild would put it.
let rebuilt (tableDictionary: PdfDictionary) context (page: int) (cells: TableCell list) =
    let table = PdfStructElem tableDictionary
    let order = ordered cells
    let expected =
        [ for cell, place in order ->
              place.Row, cell.Tag, place.RowSpan, place.ColSpan, (if cell.Tag = Tags.TH then Some(if place.Row = 0 then "Column" else "Row") else None) ]
    let kids = [ for kid in table.GetKids() do match kid with :? PdfStructElem as e when tagOf context e <> Tags.Caption -> Some e | :? PdfStructElem -> () | _ -> None ]
    if kids |> List.exists (fun k -> match k with Some e -> tagOf context e <> Tags.TR | None -> true) then
        false
    else
        let rows = kids |> List.choose id
        let present =
            [ for i, row in List.indexed rows do
                  for kid in row.GetKids() do
                      match kid with
                      | :? PdfStructElem as cell when TableGrid.isCell (tagOf context cell) -> Some(i, cell)
                      | _ -> None ]
        if present |> List.exists Option.isNone then
            false
        else
            let present = present |> List.choose id
            let shape = [ for i, cell in present -> let rowSpan, colSpan, scope = spansOf cell in i, tagOf context cell, rowSpan, colSpan, scope ]
            let boxes = [ for cell, _ in order -> cell.Box ]
            shape = expected
            && present
               |> List.indexed
               |> List.forall (fun (k, (_, cell)) ->
                   let pieces, complete = piecesOf cell context page
                   complete && pieces |> List.forall (fun (_, _, _, geometry) -> TableGrid.cellFor boxes geometry = k))

/// Why a table can't be rebuilt, if it can't: it holds something on another page, or its shape
/// is too deep to follow.
let refusal (tableDictionary: PdfDictionary) context (page: int) =
    let pieces, complete = piecesOf (PdfStructElem tableDictionary) context page
    if not complete then Some "its rows and cells are nested too deeply to follow"
    elif pieces |> List.exists (fun (_, _, _, g) -> match g with Some g -> g.PageNumber <> page | None -> false) then Some "it goes on to another page"
    else None

/// Rebuilds a table's rows and cells on page as cells (see TableGrid for their rows, columns and
/// spans): what it holds is taken out (through iText's tags, so the parent tree follows), its
/// old rows removed, rows of cells written (a header cell scoped to its column in the first row,
/// else to its row), and each piece put in the cell it's drawn in, in its reading order.
let rebuild (document: PdfDocument) context (tableDictionary: PdfDictionary) (page: int) (cells: TableCell list) =
    let table = PdfStructElem tableDictionary
    let pdfPage = document.GetPage page
    let pieces, _ = piecesOf table context page
    // Out of where they are (last first, so the earlier ones' places hold).
    let taken =
        [ for parent, index, _, geometry in List.rev pieces -> parent.RemoveKid index, geometry ]
        |> List.rev
    // The old rows, empty now; its captions kept, before the rows or after them as they were.
    let tagOf = tagOf context
    let kids = List.ofSeq (table.GetKids())
    let firstRow = kids |> List.tryFindIndex (function :? PdfStructElem as e -> tableTags.Contains(tagOf e) | _ -> false) |> Option.defaultValue kids.Length
    let captions =
        [ for i, kid in List.indexed kids do
              match kid with
              | :? PdfStructElem as e when tagOf e = Tags.Caption -> (i < firstRow), e.GetPdfObject()
              | _ -> () ]
    while table.GetKids().Count > 0 do
        table.RemoveKid 0 |> ignore
    for before, caption in captions do
        if before then table.AddKid(PdfStructElem caption) |> ignore
    let order = ordered cells
    let rowCount = order |> List.map (fun (_, p) -> p.Row + p.RowSpan) |> List.max
    let rows =
        [| for _ in 1..rowCount ->
               let row = PdfStructElem(document, PdfName.TR)
               table.AddKid row |> ignore
               row |]
    let cellElements = Array.zeroCreate<PdfStructElem> cells.Length
    for index, (cell, place) in List.indexed order do
        let element = PdfStructElem(document, PdfName cell.Tag, pdfPage)
        let attributes = PdfDictionary()
        attributes.Put(PdfName.O, PdfName.Table) |> ignore
        if cell.Tag = Tags.TH then attributes.Put(PdfName.Scope, (if place.Row = 0 then PdfName.Column else PdfName.Row)) |> ignore
        if place.RowSpan > 1 then attributes.Put(PdfName.RowSpan, PdfNumber place.RowSpan) |> ignore
        if place.ColSpan > 1 then attributes.Put(PdfName.ColSpan, PdfNumber place.ColSpan) |> ignore
        element.GetPdfObject().Put(PdfName.A, attributes) |> ignore
        rows[place.Row].AddKid element |> ignore
        cellElements[index] <- element
    for before, caption in captions do
        if not before then table.AddKid(PdfStructElem caption) |> ignore
    // Each piece in its cell, the parent tree pointed at the cell (as a new reference to it: the
    // one taken out still names the cell it came from, and iText writes the parent tree from that).
    for node, geometry in taken do
        let cell = cellElements[TableGrid.cellFor [ for cell, _ in order -> cell.Box ] geometry]
        match node with
        | :? PdfStructElem as element -> cell.AddKid element |> ignore
        | :? PdfObjRef as reference -> cell.AddKid(PdfObjRef(reference.GetPdfObject() :?> PdfDictionary, cell)) |> ignore
        | :? PdfMcrNumber as mcid -> cell.AddKid(PdfMcrNumber(mcid.GetPdfObject() :?> PdfNumber, cell)) |> ignore
        | :? PdfMcrDictionary as reference -> cell.AddKid(PdfMcrDictionary(reference.GetPdfObject() :?> PdfDictionary, cell)) |> ignore
        | _ -> ()
    pieces.Length
