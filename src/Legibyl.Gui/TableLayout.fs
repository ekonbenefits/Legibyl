/// Where a table's cells go in a grid, as HTML lays out a table: each in the first column the
/// cells before it in its row, and those spanning down from the rows above, leave free, across
/// its column span and down its row span.
module Legibyl.Gui.TableLayout

/// A cell, where it goes: its first row and column, and how many of each it covers.
type Placed<'Cell> =
    { Cell: 'Cell
      Row: int
      Column: int
      RowSpan: int
      ColumnSpan: int }

/// Places the rows' cells (each with its column span and row span, 1 or more) in a grid. A row
/// span is cut off at the last row, as HTML does.
let place (rows: ('Cell * int * int) list list) : Placed<'Cell> list =
    let rowCount = rows.Length
    let taken = System.Collections.Generic.HashSet<int * int>()
    [ for rowIndex, cells in List.indexed rows do
          let mutable column = 0
          for cell, columnSpan, rowSpan in cells do
              let columnSpan = max 1 columnSpan
              let rowSpan = min (max 1 rowSpan) (rowCount - rowIndex)
              while taken.Contains((rowIndex, column)) do
                  column <- column + 1
              for r in rowIndex .. rowIndex + rowSpan - 1 do
                  for c in column .. column + columnSpan - 1 do
                      taken.Add((r, c)) |> ignore
              yield
                  { Cell = cell
                    Row = rowIndex
                    Column = column
                    RowSpan = rowSpan
                    ColumnSpan = columnSpan }
              column <- column + columnSpan ]
