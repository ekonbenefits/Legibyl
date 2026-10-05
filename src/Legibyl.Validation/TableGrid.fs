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

/// Whether a tag is a table cell's: a header cell (TH) or a data cell (TD).
let isCell (tag: string) = tag = "TH" || tag = "TD"

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
