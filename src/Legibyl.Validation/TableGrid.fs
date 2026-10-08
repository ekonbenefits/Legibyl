/// A table's cells as a grid: each cell's row and column, and how many of each it spans, from
/// where its edges fall among the table's cells' edges. A table's cells are regions (TH or TD)
/// inside its Table region (see LayoutTagging), found by a table-structure model as one grid, so
/// their edges line up; a cell's span is the rows (or columns) whose edges fall inside it.
module Legibyl.Validation.TableGrid

/// A cell's place in its table's grid, counted from 0.
type Place =
    { Row: int
      Column: int
      RowSpan: int
      ColSpan: int }

/// How far apart two edges can be and still be one grid line (a saved layout's boxes are rounded
/// to a tenth of a point).
[<Literal>]
let private Tolerance = 0.5

/// The distinct values in values, each within Tolerance of no other, in order.
let private lines (values: float seq) =
    values
    |> Seq.sort
    |> Seq.fold
        (fun (found: float list) value ->
            match found with
            | last :: _ when value - last <= Tolerance -> found
            | _ -> value :: found)
        []
    |> List.rev

/// Each of cells' place in their grid, in the order given. Rows count down the page from the
/// top; columns, left to right.
let place (cells: StructureGeometry list) : Place list =
    // Rows go by each cell's top, highest first; columns by each's left.
    let tops = lines [ for cell in cells -> -cell.Top ] |> List.map (fun top -> -top)
    let lefts = lines [ for cell in cells -> cell.Left ]
    let indexOf (found: float list) (value: float) =
        found |> List.findIndex (fun line -> abs (line - value) <= Tolerance)
    [ for cell in cells ->
          let row = indexOf tops cell.Top
          let column = indexOf lefts cell.Left
          { Row = row
            Column = column
            // The rows whose tops fall inside it (its own included), and the same for columns.
            RowSpan = max 1 (tops |> List.filter (fun top -> top <= cell.Top + Tolerance && top > cell.Bottom + Tolerance) |> List.length)
            ColSpan = max 1 (lefts |> List.filter (fun left -> left >= cell.Left - Tolerance && left < cell.Right - Tolerance) |> List.length) } ]

/// The grid for paragraphs (rows, their boxes, top first) becoming a table's rows, from cells
/// the table-structure model found in their region: cells as found when each paragraph is in a
/// row of its own; else a grid of the paragraphs' rows and the model's columns (it misses rows,
/// a header row especially, more often than columns), a row a header row (TH) where the model
/// put a header cell. Spanning cells aren't kept in that grid.
let fitRows (rows: StructureGeometry list) (cells: TableCell list) : TableCell list =
    if rows.IsEmpty || cells.IsEmpty then
        cells
    else
        let places = place [ for cell in cells -> cell.Box ]
        let rowCount = places |> List.map (fun p -> p.Row + p.RowSpan) |> List.fold max 0
        let middle (box: StructureGeometry) = (box.Top + box.Bottom) / 2.0
        // The model's row nearest each paragraph's middle.
        let rowOf (box: StructureGeometry) =
            [ 0 .. rowCount - 1 ]
            |> List.minBy (fun row ->
                let boxes = List.zip cells places |> List.filter (fun (_, p) -> p.Row <= row && row < p.Row + p.RowSpan) |> List.map (fun (c, _) -> c.Box)
                if boxes.IsEmpty then infinity
                else abs ((List.max [ for b in boxes -> b.Top ] + List.min [ for b in boxes -> b.Bottom ]) / 2.0 - middle box))
        if rowCount = rows.Length && (rows |> List.map rowOf |> List.sort) = [ 0 .. rowCount - 1 ] then
            cells
        else
            let rows = rows |> List.sortByDescending _.Top |> Array.ofList
            let lefts = lines [ for cell in cells -> cell.Box.Left ] |> Array.ofList
            let left = min lefts[0] (rows |> Array.map _.Left |> Array.min)
            let right = max (cells |> List.map _.Box.Right |> List.max) (rows |> Array.map _.Right |> Array.max)
            let columns =
                [ for c in 0 .. lefts.Length - 1 ->
                      (if c = 0 then left else lefts[c]), (if c = lefts.Length - 1 then right else lefts[c + 1]) ]
            // Each row reaches halfway to the next.
            let bands =
                [ for r in 0 .. rows.Length - 1 ->
                      (if r = 0 then rows[r].Top else (rows[r - 1].Bottom + rows[r].Top) / 2.0),
                      (if r = rows.Length - 1 then rows[r].Bottom else (rows[r].Bottom + rows[r + 1].Top) / 2.0) ]
            let header (top, bottom) =
                let y = (top + bottom) / 2.0
                cells |> List.exists (fun cell -> cell.Tag = Tags.TH && y <= cell.Box.Top && y >= cell.Box.Bottom)
            [ for top, bottom as band in bands do
                  let tag = if header band then Tags.TH else Tags.TD
                  for l, r in columns ->
                      { Tag = tag; Box = { rows[0] with Left = l; Right = r; Top = top; Bottom = bottom } } ]

/// Whether a tag is a table cell's: a header cell (TH) or a data cell (TD).
let isCell (tag: string) = tag = Tags.TH || tag = Tags.TD

/// The cell of cells (by index) a piece drawn at geometry belongs in: the one its middle is in,
/// else the nearest; the first, for one with no place. As a table's rebuild puts it (see
/// TableRebuild), and Structure… shows it before.
let cellFor (cells: StructureGeometry list) (geometry: StructureGeometry option) =
    match geometry with
    | Some box ->
        let x, y = (box.Left + box.Right) / 2.0, (box.Bottom + box.Top) / 2.0
        let inside (c: StructureGeometry) = x >= c.Left && x <= c.Right && y >= c.Bottom && y <= c.Top
        match cells |> List.tryFindIndex inside with
        | Some index -> index
        | None ->
            cells
            |> List.indexed
            |> List.minBy (fun (_, c) ->
                let dx = max 0.0 (max (c.Left - x) (x - c.Right))
                let dy = max 0.0 (max (c.Bottom - y) (y - c.Top))
                dx * dx + dy * dy)
            |> fst
    | None -> 0
