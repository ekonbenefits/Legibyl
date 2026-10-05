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
let private tableTags = set [ "TR"; "TH"; "TD"; "THead"; "TBody"; "TFoot" ]

/// What a table holds, in reading order: each piece (a marked-content reference, an annotation's,
/// or an element its cells hold, like a paragraph or a link), its parent and place there (iText
/// makes new objects for an element's kids each time it's asked), and where it's drawn.
let private piecesOf (table: PdfStructElem) context (page: int) =
    let pieces = ResizeArray<PdfStructElem * int * IStructureNode * StructureGeometry option>()
    let rec walk (element: PdfStructElem) (depth: int) =
        if depth < 64 then
            for index, kid in List.indexed (List.ofSeq (element.GetKids())) do
                let add (node: IStructureNode) (geometry: StructureGeometry option) = pieces.Add(element, index, node, geometry)
                match kid with
                | :? PdfStructElem as child ->
                    let tag = match standardTag (child.GetPdfObject()) context with NonNull tag -> tag.GetValue() | Null -> ""
                    if tableTags.Contains tag then walk child (depth + 1)
                    else add child (subtreeGeometry (child.GetPdfObject()) context (Some page))
                | :? PdfObjRef as reference ->
                    let rect =
                        match (reference.GetPdfObject() :?> PdfDictionary).GetAsDictionary PdfName.Obj with
                        | NonNull annotation ->
                            match annotation.GetAsArray PdfName.Rect with
                            | NonNull r when r.Size() = 4 ->
                                let n i = r.GetAsNumber(i).DoubleValue()
                                Some { PageNumber = page; Left = min (n 0) (n 2); Bottom = min (n 1) (n 3); Right = max (n 0) (n 2); Top = max (n 1) (n 3) }
                            | _ -> None
                        | Null -> None
                    add reference rect
                | :? PdfMcr as mcr ->
                    // Its page (its own /Pg, else the table's), and where it's drawn there.
                    let onPage =
                        match mcr.GetPageObject() |> Option.ofObj |> Option.bind (fun p -> Option.ofObj (p.GetIndirectReference())) with
                        | Some reference -> lookup context.PageNumberByObject (reference.GetObjNumber()) |> Option.defaultValue page
                        | None -> page
                    let geometry =
                        if onPage <> page then Some { PageNumber = onPage; Left = 0.0; Bottom = 0.0; Right = 0.0; Top = 0.0 }
                        else lookup context.MarkedContent (onPage, mcr.GetMcid()) |> Option.bind _.Geometry
                    add mcr geometry
                | _ -> ()
    walk table 0
    List.ofSeq pieces

/// The middle of a box.
let private middle (box: StructureGeometry) = (box.Left + box.Right) / 2.0, (box.Bottom + box.Top) / 2.0

/// Whether a point is in a box.
let private inside (box: StructureGeometry) (x, y) = x >= box.Left && x <= box.Right && y >= box.Bottom && y <= box.Top

/// Whether a table's rows and cells are those already: as many cells, each a TH or TD of a row of
/// the table, and for each of cells one with its tag whose content is drawn in it (or is empty).
let rebuilt (tableDictionary: PdfDictionary) context (page: int) (cells: TableCell list) =
    let table = PdfStructElem tableDictionary
    let tagOf (element: PdfStructElem) = match standardTag (element.GetPdfObject()) context with NonNull tag -> tag.GetValue() | Null -> ""
    let rows = [ for kid in table.GetKids() do match kid with :? PdfStructElem as row when tagOf row = "TR" -> row | _ -> () ]
    let present =
        [ for row in rows do
              for kid in row.GetKids() do
                  match kid with
                  | :? PdfStructElem as cell when tagOf cell = "TH" || tagOf cell = "TD" ->
                      tagOf cell, subtreeGeometry (cell.GetPdfObject()) context (Some page)
                  | _ -> () ]
    present.Length = cells.Length
    && rows.Length = (table.GetKids() |> Seq.length)
    && cells
       |> List.forall (fun cell ->
           present |> List.exists (fun (tag, box) -> tag = cell.Tag && (match box with Some box -> inside cell.Box (middle box) | None -> true)))

/// The cell a piece goes in: the one its middle is in, else the nearest; the first without a place.
let private cellFor (cells: TableCell list) (geometry: StructureGeometry option) =
    match geometry with
    | Some box ->
        let x, y = middle box
        match cells |> List.tryFindIndex (fun c -> inside c.Box (x, y)) with
        | Some index -> index
        | None ->
            cells
            |> List.indexed
            |> List.minBy (fun (_, c) ->
                let dx = max 0.0 (max (c.Box.Left - x) (x - c.Box.Right))
                let dy = max 0.0 (max (c.Box.Bottom - y) (y - c.Box.Top))
                dx * dx + dy * dy)
            |> fst
    | None -> 0

/// Why a table can't be rebuilt, if it can't: it holds something on another page.
let refusal (tableDictionary: PdfDictionary) context (page: int) =
    let table = PdfStructElem tableDictionary
    if piecesOf table context page |> List.exists (fun (_, _, _, g) -> match g with Some g -> g.PageNumber <> page | None -> false) then
        Some "it goes on to another page"
    else
        None

/// Rebuilds a table's rows and cells on page as cells (see TableGrid for their rows, columns and
/// spans): what it holds is taken out (through iText's tags, so the parent tree follows), its
/// old rows removed, rows of cells written (a header cell scoped to its column in the first row,
/// else to its row), and each piece put in the cell it's drawn in, in its reading order.
let rebuild (document: PdfDocument) context (tableDictionary: PdfDictionary) (page: int) (cells: TableCell list) =
    let table = PdfStructElem tableDictionary
    let pdfPage = document.GetPage page
    let pieces = piecesOf table context page
    // Out of where they are (last first, so the earlier ones' places hold).
    let taken =
        [ for parent, index, node, geometry in List.rev pieces ->
              let removed =
                  match node with
                  | :? PdfMcr -> parent.RemoveKid(index, true)
                  | _ -> parent.RemoveKid index
              removed, geometry ]
        |> List.rev
    // The old rows, empty now.
    while table.GetKids().Count > 0 do
        table.RemoveKid 0 |> ignore
    let places = TableGrid.place [ for cell in cells -> cell.Box ]
    let rowCount = places |> List.map (fun p -> p.Row + p.RowSpan) |> List.max
    let rows =
        [| for _ in 1..rowCount ->
               let row = PdfStructElem(document, PdfName.TR)
               table.AddKid row |> ignore
               row |]
    let cellElements = Array.zeroCreate<PdfStructElem> cells.Length
    for index, (cell, place) in List.indexed (List.zip cells places) |> List.sortBy (fun (_, (_, p)) -> p.Row, p.Column) do
        let element = PdfStructElem(document, PdfName cell.Tag, pdfPage)
        let attributes = PdfDictionary()
        attributes.Put(PdfName.O, PdfName.Table) |> ignore
        if cell.Tag = "TH" then attributes.Put(PdfName.Scope, (if place.Row = 0 then PdfName.Column else PdfName.Row)) |> ignore
        if place.RowSpan > 1 then attributes.Put(PdfName.RowSpan, PdfNumber place.RowSpan) |> ignore
        if place.ColSpan > 1 then attributes.Put(PdfName.ColSpan, PdfNumber place.ColSpan) |> ignore
        element.GetPdfObject().Put(PdfName.A, attributes) |> ignore
        rows[place.Row].AddKid element |> ignore
        cellElements[index] <- element
    for node, geometry in taken do
        let cell = cellElements[cellFor cells geometry]
        match node with
        | :? PdfStructElem as element -> cell.AddKid element |> ignore
        | :? PdfMcr as mcr -> cell.AddKid mcr |> ignore
        | _ -> ()
    pieces.Length
