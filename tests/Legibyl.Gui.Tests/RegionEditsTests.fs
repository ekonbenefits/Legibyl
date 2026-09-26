module Legibyl.Gui.Tests.RegionEditsTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Layout
open Legibyl.Validation

let private box left bottom right top : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

let private logo = box 100.0 500.0 200.0 600.0
let private blurb = box 100.0 300.0 400.0 360.0

let private block tag geometry (text: string) recognized : SuggestedBlock =
    { Order = 1
      Label = "text"
      Tag = tag
      Score = 0.9
      Geometry = geometry
      Text = text
      ListNumbering = None
      Recognized = recognized
      QrText = None }

let private region tag geometry actualText : LayoutRegion =
    { Tag = tag; Geometry = geometry; ListNumbering = null; ActualText = actualText; QrText = null; Alt = null }

/// A path for a PDF in a fresh temporary folder (the PDF itself needn't exist).
let private withPdfPath (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try test (Path.Combine(folder, "flyer.pdf")) finally Directory.Delete(folder, true)

[<Fact>]
let ``a region is found again by its page, tag and about the same box`` () =
    let edits = RegionEdits.setAlt [] 1 "Figure" logo "The pack's wolf logo"
    // Moved a little, as in a regenerated PDF: still the same region.
    RegionEdits.find edits 1 "Figure" (box 103.0 497.0 204.0 598.0) |> Option.bind _.Alt |> should equal (Some "The pack's wolf logo")
    RegionEdits.find edits 2 "Figure" logo |> should equal None
    RegionEdits.find edits 1 "Table" logo |> should equal None
    RegionEdits.find edits 1 "Figure" (box 300.0 500.0 400.0 600.0) |> should equal None

[<Fact>]
let ``setting alt text replaces the figure's, and blank alt text removes it`` () =
    let edits = RegionEdits.setAlt [] 1 "Figure" logo "Old"
    let replaced = RegionEdits.setAlt edits 1 "Figure" (box 101.0 501.0 201.0 601.0) "New"
    replaced |> List.map _.Alt |> should equal [ Some "New" ]
    RegionEdits.setAlt replaced 1 "Figure" logo "  " |> should be Empty

[<Fact>]
let ``corrected OCR text is recorded only when it differs from what OCR read`` () =
    let ocr = "Campouts, cavlng, climbing"
    RegionEdits.setText [] 1 "P" blurb ocr ocr |> should be Empty
    let corrected = RegionEdits.setText [] 1 "P" blurb ocr "Campouts, caving, climbing"
    corrected |> List.map _.Text |> should equal [ Some "Campouts, caving, climbing" ]
    // Putting back what OCR read drops the correction.
    RegionEdits.setText corrected 1 "P" blurb ocr ocr |> should be Empty
    // Clearing it is a correction: nothing to read here.
    RegionEdits.setText [] 1 "P" blurb ocr "" |> List.map _.Text |> should equal [ Some "" ]

[<Fact>]
let ``edits apply to what's tagged: alt text to figures, corrected text to what OCR read`` () =
    let edits =
        RegionEdits.setAlt [] 1 "Figure" logo "The pack's wolf logo"
        |> fun e -> RegionEdits.setText e 1 "P" blurb "Campouts, cavlng" "Campouts, caving"
    let figure = block "Figure" logo "" false
    let ocrText = block "P" blurb "Campouts, cavlng" true
    RegionEdits.apply edits 1 figure (region "Figure" logo null) |> _.Alt |> should equal "The pack's wolf logo"
    RegionEdits.apply edits 1 ocrText (region "P" blurb "Campouts, cavlng") |> _.ActualText |> should equal "Campouts, caving"
    // Cleared: nothing to read, so no ActualText.
    let cleared = RegionEdits.setText [] 1 "P" blurb "Campouts, cavlng" ""
    RegionEdits.apply cleared 1 ocrText (region "P" blurb "Campouts, cavlng") |> _.ActualText |> should be Null
    // Text that was real text, not read by OCR, isn't changed.
    RegionEdits.apply edits 1 (block "P" blurb "Campouts, caving" false) (region "P" blurb null) |> _.ActualText |> should be Null

[<Fact>]
let ``importing adds only the regions that have no edits here`` () =
    let here = RegionEdits.setAlt [] 1 "Figure" logo "Written here"
    let imported = RegionEdits.setAlt (RegionEdits.setAlt [] 1 "Figure" logo "From the old file") 2 "Figure" logo "Page two's"
    let merged, added = RegionEdits.merge here imported
    added |> should equal 1
    merged |> List.map _.Alt |> should equal [ Some "Written here"; Some "Page two's" ]

[<Fact>]
let ``edits and dismissals share the file next to the PDF without losing each other`` () =
    withPdfPath (fun pdf ->
        let edits = RegionEdits.setAlt [] 1 "Figure" logo "The pack's wolf logo"
        RegionEdits.save pdf edits
        // A file with only edits doesn't mean dismissals are remembered...
        Dismissals.load pdf |> should equal None
        // ...and an entry has only the fields it changes, in readable text.
        let json = File.ReadAllText(Dismissals.filePath pdf)
        json |> should haveSubstring "\"alt\": \"The pack's wolf logo\""
        json |> should not' (haveSubstring "\"text\"")

        let dismissed = Map [ { Dismissals.Rule = "Legibyl Structure-1"; Dismissals.Location = "StructTreeRoot/K[0]" }, "Empty paragraph" ]
        Dismissals.save pdf dismissed
        RegionEdits.load pdf |> List.map _.Alt |> should equal [ Some "The pack's wolf logo" ]

        RegionEdits.save pdf (RegionEdits.setText edits 1 "P" blurb "cavlng" "caving")
        Dismissals.load pdf |> should equal (Some dismissed)
        RegionEdits.load pdf |> List.map (fun e -> e.Alt, e.Text) |> should equal [ Some "The pack's wolf logo", None; None, Some "caving" ]

        // Forgetting dismissals keeps the file for the edits...
        Dismissals.forget pdf
        File.Exists(Dismissals.filePath pdf) |> should be True
        // ...and removing the edits too leaves nothing to keep.
        RegionEdits.save pdf []
        File.Exists(Dismissals.filePath pdf) |> should be False)

[<Fact>]
let ``edits can be read from a file saved for another PDF`` () =
    withPdfPath (fun pdf ->
        RegionEdits.save pdf (RegionEdits.setAlt [] 1 "Figure" logo "The pack's wolf logo")
        let renamed = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "old-name.pdf.legibyl.json")
        File.Move(Dismissals.filePath pdf, renamed)
        RegionEdits.fromFile renamed |> List.map _.Alt |> should equal [ Some "The pack's wolf logo" ])
