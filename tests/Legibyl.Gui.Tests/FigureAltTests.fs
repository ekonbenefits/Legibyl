module Legibyl.Gui.Tests.FigureAltTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation

let private box left bottom right top : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

let private logo = box 100.0 500.0 200.0 600.0

/// A path for a PDF in a fresh temporary folder (the PDF itself needn't exist).
let private withPdfPath (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try test (Path.Combine(folder, "flyer.pdf")) finally Directory.Delete(folder, true)

[<Fact>]
let ``a figure is found again by its page, tag and about the same box`` () =
    let figures = FigureAlt.set [] 1 "Figure" logo "The pack's wolf logo"
    // Moved a little, as in a regenerated PDF: still the same figure.
    FigureAlt.find figures 1 "Figure" (box 103.0 497.0 204.0 598.0) |> Option.map _.Alt |> should equal (Some "The pack's wolf logo")
    FigureAlt.find figures 2 "Figure" logo |> should equal None
    FigureAlt.find figures 1 "Table" logo |> should equal None
    FigureAlt.find figures 1 "Figure" (box 300.0 500.0 400.0 600.0) |> should equal None

[<Fact>]
let ``setting alt text replaces the figure's, and blank alt text removes it`` () =
    let figures = FigureAlt.set [] 1 "Figure" logo "Old"
    let replaced = FigureAlt.set figures 1 "Figure" (box 101.0 501.0 201.0 601.0) "New"
    replaced |> List.map _.Alt |> should equal [ "New" ]
    FigureAlt.set replaced 1 "Figure" logo "  " |> should be Empty

[<Fact>]
let ``importing adds only the figures that aren't here already`` () =
    let here = FigureAlt.set [] 1 "Figure" logo "Written here"
    let imported = FigureAlt.set (FigureAlt.set [] 1 "Figure" logo "From the old file") 2 "Figure" logo "Page two's"
    let merged, added = FigureAlt.merge here imported
    added |> should equal 1
    merged |> List.map _.Alt |> should equal [ "Written here"; "Page two's" ]

[<Fact>]
let ``alt text and dismissals share the file next to the PDF without losing each other`` () =
    withPdfPath (fun pdf ->
        let figures = FigureAlt.set [] 1 "Figure" logo "The pack's wolf logo"
        FigureAlt.save pdf figures
        // A file with only alt text doesn't mean dismissals are remembered.
        Dismissals.load pdf |> should equal None

        let dismissed = Map [ { Dismissals.Rule = "Legibyl Structure-1"; Dismissals.Location = "StructTreeRoot/K[0]" }, "Empty paragraph" ]
        Dismissals.save pdf dismissed
        FigureAlt.load pdf |> List.map _.Alt |> should equal [ "The pack's wolf logo" ]

        FigureAlt.save pdf (FigureAlt.set figures 1 "Figure" logo "A wolf's head in a yellow diamond")
        Dismissals.load pdf |> should equal (Some dismissed)

        // Forgetting dismissals keeps the file for the alt text...
        Dismissals.forget pdf
        File.Exists(Dismissals.filePath pdf) |> should be True
        FigureAlt.load pdf |> List.map _.Alt |> should equal [ "A wolf's head in a yellow diamond" ]
        // ...and removing the alt text too leaves nothing to keep.
        FigureAlt.save pdf []
        File.Exists(Dismissals.filePath pdf) |> should be False)

[<Fact>]
let ``alt text can be read from a file saved for another PDF`` () =
    withPdfPath (fun pdf ->
        FigureAlt.save pdf (FigureAlt.set [] 1 "Figure" logo "The pack's wolf logo")
        let renamed = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "old-name.pdf.legibyl.json")
        File.Move(Dismissals.filePath pdf, renamed)
        FigureAlt.fromFile renamed |> List.map _.Alt |> should equal [ "The pack's wolf logo" ])
