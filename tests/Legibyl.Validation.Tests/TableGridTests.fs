module Legibyl.Validation.Tests.TableGridTests

open Xunit
open FsUnit.Xunit
open Legibyl.Validation

let private box (left, bottom, right, top) : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

let private places cells =
    TableGrid.place (List.map box cells) |> List.map (fun p -> p.Row, p.Column, p.RowSpan, p.ColSpan)

[<Fact>]
let ``a plain grid's cells are in rows down the page and columns across it`` () =
    places [ (0.0, 80.0, 50.0, 100.0); (50.0, 80.0, 100.0, 100.0); (0.0, 60.0, 50.0, 80.0); (50.0, 60.0, 100.0, 80.0) ]
    |> should equal [ 0, 0, 1, 1; 0, 1, 1, 1; 1, 0, 1, 1; 1, 1, 1, 1 ]

[<Fact>]
let ``a cell over two columns, or two rows, spans them`` () =
    // "Bed" over two header rows; "Garden" over two columns; then the second header row.
    places
        [ (0.0, 60.0, 50.0, 100.0)
          (50.0, 80.0, 150.0, 100.0)
          (50.0, 60.0, 100.0, 80.0)
          (100.0, 60.0, 150.0, 80.0) ]
    |> should equal [ 0, 0, 2, 1; 0, 1, 1, 2; 1, 1, 1, 1; 1, 2, 1, 1 ]

[<Fact>]
let ``edges a little apart, as a saved layout rounds them, are one grid line`` () =
    places [ (0.0, 80.0, 50.04, 100.0); (50.0, 80.0, 100.0, 99.96) ]
    |> should equal [ 0, 0, 1, 1; 0, 1, 1, 1 ]

let private cell tag bounds : TableCell = { Tag = tag; Box = box bounds }

[<Fact>]
let ``cells with a row for each paragraph are kept as found`` () =
    let cells = [ cell "TH" (0.0, 80.0, 50.0, 100.0); cell "TH" (50.0, 80.0, 100.0, 100.0); cell "TD" (0.0, 60.0, 50.0, 80.0); cell "TD" (50.0, 60.0, 100.0, 80.0) ]
    TableGrid.fitRows [ box (2.0, 82.0, 98.0, 98.0); box (2.0, 62.0, 98.0, 78.0) ] cells |> should equal cells

[<Fact>]
let ``rows the model missed are made from the paragraphs, with its columns and header row`` () =
    // Four paragraph rows; the model found only the bottom two rows, the first a header row.
    let rows = [ box (72.0, 704.6, 362.7, 716.0); box (72.0, 693.2, 325.3, 704.6); box (72.0, 681.8, 251.1, 693.2); box (72.0, 670.5, 202.8, 681.9) ]
    let found = [ cell "TH" (72.5, 682.5, 170.2, 693.1); cell "TH" (170.2, 682.5, 360.2, 693.1); cell "TD" (72.5, 673.1, 170.2, 682.5); cell "TD" (170.2, 673.1, 360.2, 682.5) ]
    let fitted = TableGrid.fitRows rows found
    fitted |> List.map _.Tag |> should equal [ "TD"; "TD"; "TD"; "TD"; "TH"; "TH"; "TD"; "TD" ]
    let places = TableGrid.place [ for c in fitted -> c.Box ]
    places |> List.map (fun p -> p.Row, p.Column) |> should equal [ 0, 0; 0, 1; 1, 0; 1, 1; 2, 0; 2, 1; 3, 0; 3, 1 ]
    // Each paragraph's middle is in its own row's cells.
    for r, row in List.indexed rows do
        let y = (row.Top + row.Bottom) / 2.0
        fitted[r * 2].Box.Top |> should be (greaterThanOrEqualTo y)
        fitted[r * 2].Box.Bottom |> should be (lessThanOrEqualTo y)
    fitted.Head.Box.Left |> should equal 72.0
    fitted[1].Box.Right |> should equal 362.7
