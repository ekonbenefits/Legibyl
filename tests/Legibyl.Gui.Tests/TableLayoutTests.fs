module Legibyl.Gui.Tests.TableLayoutTests

open Xunit
open FsUnit.Xunit
open Legibyl.Gui

let private cell name = name, 1, 1
let private wide name columns = name, columns, 1
let private tall name rows = name, 1, rows

/// Each cell's name, first row and column, and spans.
let private layout rows =
    TableLayout.place rows |> List.map (fun p -> p.Cell, p.Row, p.Column, p.RowSpan, p.ColumnSpan)

[<Fact>]
let ``cells without spans go one to a column`` () =
    layout [ [ cell "a"; cell "b" ]; [ cell "c"; cell "d" ] ]
    |> should equal [ "a", 0, 0, 1, 1; "b", 0, 1, 1, 1; "c", 1, 0, 1, 1; "d", 1, 1, 1, 1 ]

[<Fact>]
let ``a cell spanning columns pushes the next along, so the rows line up`` () =
    // As on page 3 of the participant statement: a title across the table, and row headers
    // across the first two columns of each row.
    layout [ [ wide "title" 3 ]; [ wide "header" 2; cell "x" ]; [ cell "h"; cell "y"; cell "z" ] ]
    |> should
        equal
        [ "title", 0, 0, 1, 3
          "header", 1, 0, 1, 2
          "x", 1, 2, 1, 1
          "h", 2, 0, 1, 1
          "y", 2, 1, 1, 1
          "z", 2, 2, 1, 1 ]

[<Fact>]
let ``a cell spanning rows keeps its column free in the rows below`` () =
    layout [ [ tall "side" 2; cell "a"; cell "b" ]; [ cell "c"; cell "d" ] ]
    |> should equal [ "side", 0, 0, 2, 1; "a", 0, 1, 1, 1; "b", 0, 2, 1, 1; "c", 1, 1, 1, 1; "d", 1, 2, 1, 1 ]

[<Fact>]
let ``a row span past the last row stops there, and a span under 1 is 1`` () =
    layout [ [ tall "side" 5; ("zero", 0, 0) ] ]
    |> should equal [ "side", 0, 0, 1, 1; "zero", 0, 1, 1, 1 ]
