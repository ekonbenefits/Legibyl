/// Converts a run of sibling paragraphs into a table after a person has checked the cells the
/// table-structure model found. Each paragraph becomes its detected row; its content is split
/// among that row's cells by where it is drawn.
module internal Legibyl.Validation.ParagraphTableRebuild

open System
open System.Collections.Generic
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Tagging
open Legibyl.Validation.Interop
open Legibyl.Validation.StructureTreeObjects

type private Conversion =
    { Parent: PdfStructElem
      Paragraphs: PdfStructElem list
      OrderedCells: (TableCell * TableGrid.Place) list
      RowCount: int
      Contents: (int * PdfStructElem * (IStructureNode * StructureGeometry) list) list }

let private standaloneKeys = [ PdfName.ActualText; PdfName.Alt; PdfName.E; PdfName.ID ]

let private spaced (text: string) = Text.RegularExpressions.Regex.Replace(text, @"\s+", " ").Trim()

/// Whether paragraph's replacement text (/ActualText) says only what its content draws, spacing
/// aside: then it adds nothing, and nothing is lost when the paragraph's content is split among
/// cells and the paragraph is gone.
let private redundantActualText context page (paragraph: PdfStructElem) =
    match paragraph.GetPdfObject().GetAsString PdfName.ActualText with
    | null -> false
    | text ->
        let drawn = spaced (extractText (paragraph.GetPdfObject().Get PdfName.K) context (Some page))
        drawn <> "" && spaced (text.ToUnicodeString()) = drawn

/// Whether paragraph says something of its own the cells it's split among couldn't: replacement
/// text other than what it draws, alt text, an expansion, or an ID something may refer to.
let private standalone context page (paragraph: PdfStructElem) =
    standaloneKeys
    |> List.exists (fun key ->
        paragraph.GetPdfObject().ContainsKey key
        && not (key.Equals PdfName.ActualText && redundantActualText context page paragraph))

let private kidsOf (element: PdfStructElem) = List.ofSeq (element.GetKids())

let private geometryOf context page (node: IStructureNode) =
    match node with
    | :? PdfStructElem as element -> subtreeGeometry (element.GetPdfObject()) context (Some page)
    | :? PdfMcr as mcr ->
        lookup context.MarkedContent (page, mcr.GetMcid()) |> Option.bind _.Geometry
    | _ -> None

let private plan context (run: ParagraphJoins.JoinRun) (cells: TableCell list) =
    let validBox (box: StructureGeometry) =
        box.PageNumber = run.Page
        && Double.IsFinite box.Left
        && Double.IsFinite box.Bottom
        && Double.IsFinite box.Right
        && Double.IsFinite box.Top
        && box.Right > box.Left
        && box.Top > box.Bottom
    if cells.IsEmpty || (cells |> List.exists (fun cell -> not (TableGrid.isCell cell.Tag) || not (validBox cell.Box))) then
        Error "the checked cells aren't table cells on their page"
    else
      let parent = PdfStructElem run.Parent
      let paragraphs =
          run.Indexes
          |> List.map (fun index ->
              match parent.GetKids().[index] with
              | :? PdfStructElem as paragraph -> paragraph
              | _ -> invalidOp "A paragraph run no longer contains a structure element.")
      let placements = TableGrid.place [ for cell in cells -> cell.Box ]
      let ordered = List.zip cells placements |> List.sortBy (fun (_, place) -> place.Row, place.Column)
      let rowCount = ordered |> List.map (fun (_, place) -> place.Row + place.RowSpan) |> List.fold max 0
      let rowBoxes =
          [ for row in 0 .. rowCount - 1 ->
                row,
                (ordered
                 |> List.choose (fun (cell, place) ->
                     if place.Row <= row && row < place.Row + place.RowSpan then Some cell.Box else None)) ]
      let rowOfGeometry (geometry: StructureGeometry) =
          rowBoxes
          |> List.minBy (fun (_, boxes) ->
              let top = boxes |> List.map _.Top |> List.max
              let bottom = boxes |> List.map _.Bottom |> List.min
              abs ((top + bottom) / 2.0 - (geometry.Top + geometry.Bottom) / 2.0))
          |> fst
      let rec leafGeometries (node: IStructureNode) : StructureGeometry list =
          match node with
          | :? PdfStructElem as element ->
              match kidsOf element with
              | [] -> geometryOf context run.Page node |> Option.toList
              | children -> children |> List.collect leafGeometries
          | _ -> geometryOf context run.Page node |> Option.toList
      // An element holding no content (an empty Span, say) is nowhere on the page: it goes with
      // the piece before it (or after, first), so it can't stop the conversion.
      let rec holdsContent (node: IStructureNode | null) =
          match node with
          | null -> false
          | :? PdfStructElem as element -> kidsOf element |> List.exists holdsContent
          | _ -> true
      let piecesOf (paragraph: PdfStructElem) =
          let raw =
              kidsOf paragraph
              |> List.map (fun kid -> kid, geometryOf context run.Page kid, leafGeometries kid)
          raw
          |> List.mapi (fun index ((kid, _, _) as piece) ->
              if holdsContent kid then
                  piece
              else
                  let before = raw |> List.take index |> List.rev
                  let after = raw |> List.skip (index + 1)
                  match before @ after |> List.tryFind (fun (other, _, _) -> holdsContent other) with
                  | Some(_, geometry, leaves) -> kid, geometry, leaves
                  | None -> piece)
      let contents =
          paragraphs
          |> List.map (fun paragraph ->
              let box = subtreeGeometry (paragraph.GetPdfObject()) context (Some run.Page)
              let row =
                  box
                  |> Option.map rowOfGeometry
              row, paragraph, piecesOf paragraph)
      let assignedRows = contents |> List.choose (fun (row, _, _) -> row) |> List.sort
      let misplaced () =
          contents
          |> List.exists (fun (row, _, pieces) ->
                 match row with
                 | None -> true
                 | Some row ->
                     let rowCells =
                         ordered
                         |> List.filter (fun (_, place) -> place.Row <= row && row < place.Row + place.RowSpan)
                     let cellBoxes = rowCells |> List.map (fun (cell, _) -> cell.Box)
                     pieces.IsEmpty
                     || (pieces
                        |> List.exists (fun (_, geometry, leaves) ->
                            match geometry with
                            | None -> true
                            | Some geometry when geometry.PageNumber <> run.Page -> true
                            | Some geometry ->
                                let assigned = TableGrid.cellFor cellBoxes (Some geometry)
                                leaves.IsEmpty
                                || leaves
                                   |> List.exists (fun (leaf: StructureGeometry) ->
                                       leaf.PageNumber <> run.Page
                                       || rowOfGeometry leaf <> row
                                       || TableGrid.cellFor cellBoxes (Some leaf) <> assigned))))
      if paragraphs.Length < 2 then
          Error "a table needs at least two paragraphs"
      elif assignedRows <> [ 0 .. rowCount - 1 ] || (rowBoxes |> List.exists (fun (_, boxes) -> boxes.IsEmpty)) then
          Error $"the checked grid doesn't have a row for each paragraph ({rowCount} row(s) for {paragraphs.Length} paragraphs)"
      elif paragraphs |> List.exists (standalone context run.Page) then
          Error "a paragraph has its own replacement text (other than the text it draws), alt text, expansion or ID (/ActualText, /Alt, /E or /ID), which can't be split among cells"
      elif misplaced () then
          Error "some of their content isn't drawn within one cell of its row"
      else
          Ok
              { Parent = parent
                Paragraphs = paragraphs
                OrderedCells = ordered
                RowCount = rowCount
                Contents =
                  [ for row, paragraph, pieces in contents ->
                        row.Value, paragraph, [ for node, geometry, _ in pieces -> node, geometry.Value ] ] }

/// Why the paragraphs in run can't be converted to a table of cells, or None if they can.
let refusal context run cells =
    match plan context run cells with
    | Ok _ -> None
    | Error why -> Some why

let canConvert context run cells = (refusal context run cells).IsNone

let private moveKid (parent: PdfStructElem) (node: IStructureNode) =
    match node with
    | :? PdfStructElem as element -> parent.AddKid element |> ignore
    | :? PdfObjRef as reference -> parent.AddKid(PdfObjRef(reference.GetPdfObject() :?> PdfDictionary, parent)) |> ignore
    | :? PdfMcrNumber as mcid -> parent.AddKid(PdfMcrNumber(mcid.GetPdfObject() :?> PdfNumber, parent)) |> ignore
    | :? PdfMcrDictionary as reference -> parent.AddKid(PdfMcrDictionary(reference.GetPdfObject() :?> PdfDictionary, parent)) |> ignore
    | _ -> ()

/// Converts paragraphs from a validated consecutive run. None means the edit is unsafe to apply.
let convert (document: PdfDocument) context (run: ParagraphJoins.JoinRun) (cells: TableCell list) =
    match plan context run cells with
    | Error _ -> None
    | Ok conversion ->
        let page = document.GetPage run.Page
        for _, paragraph, pieces in conversion.Contents do
            for _ in pieces do paragraph.RemoveKid 0 |> ignore
        let table = PdfStructElem(document, PdfName.Table, page)
        let insertion = run.Indexes.Head
        for index in List.rev run.Indexes do conversion.Parent.RemoveKid index |> ignore
        conversion.Parent.AddKid(insertion, table) |> ignore
        let rows =
            [| for _ in 0 .. conversion.RowCount - 1 ->
                   let row = PdfStructElem(document, PdfName.TR)
                   table.AddKid row |> ignore
                   row |]
        let cellElements =
            conversion.OrderedCells
            |> List.map (fun (cell, place) ->
                let element = PdfStructElem(document, PdfName cell.Tag, page)
                let attributes = PdfDictionary()
                attributes.Put(PdfName.O, PdfName.Table) |> ignore
                if cell.Tag = "TH" then attributes.Put(PdfName.Scope, if place.Row = 0 then PdfName.Column else PdfName.Row) |> ignore
                if place.RowSpan > 1 then attributes.Put(PdfName.RowSpan, PdfNumber place.RowSpan) |> ignore
                if place.ColSpan > 1 then attributes.Put(PdfName.ColSpan, PdfNumber place.ColSpan) |> ignore
                element.GetPdfObject().Put(PdfName.A, attributes) |> ignore
                rows[place.Row].AddKid element |> ignore
                cell, place, element)
        for row, paragraph, pieces in conversion.Contents do
            let rowCells = cellElements |> List.filter (fun (_, place, _) -> place.Row <= row && row < place.Row + place.RowSpan)
            let cellBoxes = rowCells |> List.map (fun (cell, _, _) -> cell.Box)
            let groups = Array.init rowCells.Length (fun _ -> ResizeArray<IStructureNode>())
            for node, geometry in pieces do
                groups[TableGrid.cellFor cellBoxes (Some geometry)].Add node
            for index in 0 .. groups.Length - 1 do
                if groups[index].Count > 0 then
                    let cell = rowCells[index] |> fun (_, _, element) -> element
                    let paragraphElement = PdfStructElem(document, PdfName.P, page)
                    match paragraph.GetPdfObject().Get(PdfName.Lang, true) with
                    | NonNull language -> paragraphElement.GetPdfObject().Put(PdfName.Lang, language) |> ignore
                    | Null -> ()
                    cell.AddKid paragraphElement |> ignore
                    for node in groups[index] do moveKid paragraphElement node
        Some conversion.Paragraphs.Length
