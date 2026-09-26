module Legibyl.Layout.Tests.LayoutSuggestionsTests

open System
open Xunit
open FsUnit.Xunit
open PaddleOcrSharp.Models.Layout
open Legibyl.Layout
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private region left bottom right top : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

[<Fact>]
let ``detector labels suggest tags`` () =
    [ "doc_title"; "paragraph_title"; "text"; "image"; "table"; "figure_title"; "footer"; "number"; "footnote" ]
    |> List.map LayoutSuggestions.tagFor
    |> should equal [ "H1"; "H"; "P"; "Figure"; "Table"; "Caption"; "Artifact"; "Artifact"; "Note" ]

[<Fact>]
let ``boxes in rendered pixels become PDF points from the bottom of the page`` () =
    // A 792pt page rendered at 2 pixels per point.
    let box = LayoutBox(0, "text", 0.9f, 100f, 200f, 300f, 260f, 0)
    LayoutSuggestions.toGeometry 3 (0.0, 792.0) 2.0 box |> should equal { PageNumber = 3; Left = 50.0; Right = 150.0; Top = 692.0; Bottom = 662.0 }

[<Fact>]
let ``a crop box that doesn't start at the origin offsets the boxes`` () =
    let box = LayoutBox(0, "text", 0.9f, 100f, 200f, 300f, 260f, 0)
    LayoutSuggestions.toGeometry 1 (20.0, 800.0) 2.0 box |> should equal { PageNumber = 1; Left = 70.0; Right = 170.0; Top = 700.0; Bottom = 670.0 }

[<Fact>]
let ``each word goes to the smallest region it's in, once`` () =
    let page = region 0.0 0.0 600.0 800.0
    let title = region 50.0 700.0 550.0 740.0
    let words = [ "Title", (100.0, 720.0); "body", (100.0, 400.0); "outside", (700.0, 900.0) ]
    LayoutSuggestions.assignText [ page; title ] words |> should equal [ "body"; "Title" ]

/// The detector itself, when its model has been downloaded (it isn't part of the build).
let private withModel test =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    test ()

[<Fact>]
let ``the detector finds the page's text regions and their words`` () =
    withModel (fun () ->
        use suggester = new LayoutSuggester(corpusFile "pdfua1-pass.pdf")
        let page = suggester.Suggest 1
        page.HasText |> should be True
        page.Blocks |> should not' (be Empty)
        page.Blocks |> List.map _.Order |> should equal [ 1 .. page.Blocks.Length ]
        page.Blocks |> List.exists (fun block -> block.Tag = "P" && block.Text <> "") |> should be True)

let private block order tag left text : SuggestedBlock =
    { Order = order
      Label = "text"
      Tag = tag
      Score = 0.9
      Geometry = { PageNumber = 1; Left = left; Bottom = 0.0; Right = 500.0; Top = 10.0 }
      Text = text
      ListNumbering = None }

[<Fact>]
let ``runs of paragraphs starting with list markers become list items, like the lists fix`` () =
    let blocks =
        [ block 1 "P" 70.0 "It is expected that:"
          block 2 "P" 86.0 "• all car construction will be done by the Scout"
          block 3 "P" 86.0 "• No prebuilt cars will be used."
          block 4 "P" 87.0 "• Cars will be tuned only by the Scout."
          block 5 "H" 70.0 "1. General Rules"
          block 6 "P" 70.0 "1. A lone numbered paragraph"
          block 7 "P" 70.0 "1. Steps that don't count"
          block 8 "P" 70.0 "3. up by one" ]

    LayoutSuggestions.findLists blocks
    |> List.map (fun b -> b.Order, b.Tag, b.ListNumbering)
    |> should
        equal
        [ 1, "P", None
          2, "LI", Some "Disc"
          3, "LI", Some "Disc"
          4, "LI", Some "Disc"
          5, "H", None
          6, "P", None
          7, "P", None
          8, "P", None ]
