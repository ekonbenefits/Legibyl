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
