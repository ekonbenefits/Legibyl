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
    let only (regions: LayoutRegion list) = List.exactlyOne regions
    (RegionEdits.onRegions edits [ figure ] |> only).Alt |> should equal "The pack's wolf logo"
    (RegionEdits.onRegions edits [ ocrText ] |> only).ActualText |> should equal "Campouts, caving"
    // Cleared: nothing to read, so no ActualText.
    let cleared = RegionEdits.setText [] 1 "P" blurb "Campouts, cavlng" ""
    (RegionEdits.onRegions cleared [ ocrText ] |> only).ActualText |> should be Null
    // Text that was real text, not read by OCR, isn't changed.
    (RegionEdits.onRegions edits [ block "P" blurb "Campouts, caving" false ] |> only).ActualText |> should be Null

[<Fact>]
let ``leaving out and retagging are edits on the suggestion, which stays as found`` () =
    let heading = block "H" blurb "Campouts" false
    let figure = block "Figure" logo "" false
    let edits =
        RegionEdits.setRetag [] 1 "H" blurb "H1"
        |> fun e -> RegionEdits.setExcluded e 1 "Figure" logo true

    RegionEdits.onRegions edits [ heading; figure ] |> List.map _.Tag |> should equal [ "H1" ]
    RegionEdits.tagOf edits 1 heading |> should equal "H1"
    RegionEdits.isExcluded edits 1 figure |> should be True
    heading.Tag |> should equal "H"
    // Its own tag again, and kept again: no edits left.
    let undone = RegionEdits.setRetag edits 1 "H" blurb "H" |> fun e -> RegionEdits.setExcluded e 1 "Figure" logo false
    undone |> should be Empty

[<Fact>]
let ``importing adds only the regions that have no edits here`` () =
    let here = RegionEdits.setAlt [] 1 "Figure" logo "Written here"
    let imported = RegionEdits.setAlt (RegionEdits.setAlt [] 1 "Figure" logo "From the old file") 2 "Figure" logo "Page two's"
    let merged, added = RegionEdits.merge here imported
    added |> should equal 1
    merged |> List.map _.Alt |> should equal [ Some "Written here"; Some "Page two's" ]

[<Fact>]
let ``edits and dismissals share the file next to the PDF`` () =
    withPdfPath (fun pdf ->
        let edits = RegionEdits.setAlt [] 1 "Figure" logo "The pack's wolf logo"
        Reviews.saveEdits pdf edits
        // An entry has only the fields it changes, in readable text.
        let json = File.ReadAllText(Dismissals.filePath pdf)
        json |> should haveSubstring "\"alt\": \"The pack's wolf logo\""
        json |> should not' (haveSubstring "\"text\"")

        let dismissed = Map [ { Dismissals.Rule = "Legibyl Structure-1"; Dismissals.Location = "StructTreeRoot/K[0]" }, "Empty paragraph" ]
        Reviews.save pdf (fun review -> review.Dismissed <- dismissed)
        (Reviews.read pdf).Edits |> List.map _.Alt |> should equal [ Some "The pack's wolf logo" ]

        Reviews.saveEdits pdf (RegionEdits.setText edits 1 "P" blurb "cavlng" "caving")
        (Reviews.read pdf).Dismissed |> should equal dismissed
        (Reviews.read pdf).Edits |> List.map (fun e -> e.Alt, e.Text) |> should equal [ Some "The pack's wolf logo", None; None, Some "caving" ]

        // Without the dismissals the file stays for the edits...
        Reviews.save pdf (fun review -> review.Dismissed <- Map.empty)
        File.Exists(Dismissals.filePath pdf) |> should be True
        // ...and without the edits too there's nothing to keep.
        Reviews.saveEdits pdf []
        File.Exists(Dismissals.filePath pdf) |> should be False)

[<Fact>]
let ``edits can be read from a file saved for another PDF`` () =
    withPdfPath (fun pdf ->
        Reviews.saveEdits pdf (RegionEdits.setAlt [] 1 "Figure" logo "The pack's wolf logo")
        let renamed = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "old-name.pdf.legibyl.json")
        File.Move(Dismissals.filePath pdf, renamed)
        RegionEdits.fromFile renamed |> List.map _.Alt |> should equal [ Some "The pack's wolf logo" ])

[<Fact>]
let ``the document title is saved with the rest of the file, and removed without losing it`` () =
    withPdfPath (fun pdf ->
        Reviews.saveEdits pdf (RegionEdits.setAlt [] 1 "Figure" logo "The pack's wolf logo")
        Reviews.save pdf (fun review -> review.Title <- Some "Cub Scout Pack 721")
        (Reviews.read pdf).Title |> should equal (Some "Cub Scout Pack 721")
        (Reviews.read pdf).Edits |> List.map _.Alt |> should equal [ Some "The pack's wolf logo" ]
        Reviews.save pdf (fun review -> review.Title <- None)
        (Reviews.read pdf).Title |> should equal None
        (Reviews.read pdf).Edits |> should haveLength 1)

/// A one-page PDF with text, in a fresh folder.
let private withPdf (text: string) (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "flyer.pdf")
    let write (text: string) =
        use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfWriter(pdf))
        let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas(document.AddNewPage())
        let font = iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA
        canvas.BeginText().SetFontAndSize(font, 12f).MoveText(100.0, 330.0).ShowText(text).EndText() |> ignore
    write text
    try test pdf finally Directory.Delete(folder, true)

let private found: PageSuggestion =
    { PageNumber = 1
      HasText = true
      Rotated = false
      Blocks = [ { block "P" blurb "Caving this spring" false with Order = 1 }; { block "Figure" logo "" false with Order = 2 } ] }

[<Fact>]
let ``a review's layout is recorded next to the PDF as found, with the edits on it, and found again when it's reopened`` () =
    withPdf "Caving this spring" (fun pdf ->
        let review = DocumentReview(File.GetLastWriteTimeUtc pdf)
        review.Pages[1] <- found
        review.Edits <- RegionEdits.setExcluded [] 1 "Figure" logo true
        review.Save pdf

        let reopened = DocumentReviews().For pdf

        reopened.Pages.Keys |> List.ofSeq |> should equal [ 1 ]
        reopened.Pages[1].Blocks |> List.map (fun b -> b.Order, b.Tag, b.Text) |> should equal [ 1, "P", "Caving this spring"; 2, "Figure", "" ]
        reopened.Edits |> List.map (fun e -> e.Tag, e.Exclude) |> should equal [ "Figure", true ])

[<Fact>]
let ``a page that has changed since isn't taken from the saved layout`` () =
    withPdf "Caving this spring" (fun pdf ->
        let review = DocumentReview(File.GetLastWriteTimeUtc pdf)
        review.Pages[1] <- found
        // A layout is recorded once something's decided: here, the title.
        review.Title <- Some "Caving"
        review.Save pdf
        let saved = File.ReadAllText(Dismissals.filePath pdf)
        // The PDF regenerated with other text (its review file kept).
        do
            use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfWriter(pdf))
            let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas(document.AddNewPage())
            let font = iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA
            canvas.BeginText().SetFontAndSize(font, 12f).MoveText(100.0, 330.0).ShowText("Hiking this summer").EndText() |> ignore
        File.WriteAllText(Dismissals.filePath pdf, saved)

        (DocumentReviews().For pdf).Pages.IsEmpty |> should be True)

[<Fact>]
let ``the auto fixes chosen are saved by name, and choosing none is kept too`` () =
    withPdfPath (fun pdf ->
        Reviews.save pdf (fun review -> review.AutoFixes <- Some(PdfFixOptions.Metadata ||| PdfFixOptions.AnnotationTabs))

        File.ReadAllText(Dismissals.filePath pdf) |> should haveSubstring "\"fixes\": [\n    \"tabs\",\n    \"metadata\"\n  ]"
        (Reviews.read pdf).AutoFixes |> should equal (Some(PdfFixOptions.Metadata ||| PdfFixOptions.AnnotationTabs))

        Reviews.save pdf (fun review -> review.AutoFixes <- Some PdfFixOptions.None)
        (Reviews.read pdf).AutoFixes |> should equal (Some PdfFixOptions.None)
        (Reviews.read pdf).IsEdited |> should be False)
