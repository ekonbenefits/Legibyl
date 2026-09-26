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
      ListNumbering = None
      Recognized = false
      QrText = None }

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

[<Fact>]
let ``OCR reads text in an image`` () =
    match TextRecognition.createEngine () with
    | None -> Assert.Skip "Tesseract has no native library for this platform."
    | Some engine ->
        use engine = engine
        use bitmap = new SkiaSharp.SKBitmap(900, 120)
        use canvas = new SkiaSharp.SKCanvas(bitmap)
        canvas.Clear SkiaSharp.SKColors.White
        use font = new SkiaSharp.SKFont(SkiaSharp.SKTypeface.Default, 56f)
        use paint = new SkiaSharp.SKPaint(Color = SkiaSharp.SKColors.Black, IsAntialias = true)
        canvas.DrawText("Scouting for Food", 20f, 80f, font, paint)
        use image = SkiaSharp.SKImage.FromBitmap bitmap
        use png = image.Encode(SkiaSharp.SKEncodedImageFormat.Png, 100)
        TextRecognition.recognize engine (png.ToArray()) |> should equal "Scouting for Food"

/// Draws a QR code for text into the canvas, size pixels square at (x, y).
let private drawQrCodeIn (canvas: SkiaSharp.SKCanvas) (text: string) (x: float32) (y: float32) (size: int) (dark: SkiaSharp.SKColor) (light: SkiaSharp.SKColor) =
    let matrix = ZXing.QrCode.QRCodeWriter().encode(text, ZXing.BarcodeFormat.QR_CODE, size, size)
    use darkPaint = new SkiaSharp.SKPaint(Color = dark)
    use lightPaint = new SkiaSharp.SKPaint(Color = light)
    for row in 0 .. matrix.Height - 1 do
        for column in 0 .. matrix.Width - 1 do
            canvas.DrawRect(x + float32 column, y + float32 row, 1f, 1f, (if matrix[column, row] then darkPaint else lightPaint))

let private drawQrCode canvas text x y size =
    drawQrCodeIn canvas text x y size SkiaSharp.SKColors.Black SkiaSharp.SKColors.White

[<Fact>]
let ``QR codes large and small are found anywhere on a page`` () =
    // A letter page at about 300 dpi, with a large code and a small one.
    use page = new SkiaSharp.SKBitmap(2448, 3168)
    use canvas = new SkiaSharp.SKCanvas(page)
    canvas.Clear SkiaSharp.SKColors.White
    drawQrCode canvas "https://example.com/large" 200f 300f 900
    drawQrCode canvas "https://example.com/small" 1900f 2700f 140
    // Light modules on a dark background, as in a brand-coloured box.
    drawQrCodeIn canvas "https://example.com/inverted" 1600f 1400f 400 (SkiaSharp.SKColor(0xF0uy, 0xF0uy, 0xE0uy)) (SkiaSharp.SKColor(0x1Fuy, 0x4Duy, 0x2Cuy))

    let found = QrCodes.decodeAll page

    found |> List.map fst |> List.sort |> should equal [ "https://example.com/inverted"; "https://example.com/large"; "https://example.com/small" ]
    let _, (left, top, right, bottom) = found |> List.find (fun (text, _) -> text = "https://example.com/small")
    // Its bounds cover where it was drawn.
    (left <= 1930.0 && top <= 2730.0 && right >= 2010.0 && bottom >= 2810.0) |> should be True

[<Fact>]
let ``a QR code describes the figure it's in, or becomes a figure where the layout missed it`` () =
    let figure = { block 1 "Figure" 100.0 ""  with Geometry = { PageNumber = 1; Left = 100.0; Bottom = 500.0; Right = 200.0; Top = 600.0 } }
    let heading = { block 2 "H" 100.0 "Scan to apply" with Geometry = { PageNumber = 1; Left = 100.0; Bottom = 300.0; Right = 400.0; Top = 320.0 } }
    let paragraph = { block 3 "P" 100.0 "More below." with Geometry = { PageNumber = 1; Left = 100.0; Bottom = 100.0; Right = 400.0; Top = 120.0 } }
    let codes =
        [ "https://example.com/in-figure", { PageNumber = 1; Left = 120.0; Bottom = 520.0; Right = 180.0; Top = 580.0 }
          "https://example.com/missed", { PageNumber = 1; Left = 100.0; Bottom = 200.0; Right = 160.0; Top = 260.0 } ]

    LayoutSuggestions.placeQrCodes codes [ figure; heading; paragraph ]
    |> List.map (fun b -> b.Order, b.Tag, b.QrText)
    |> should
        equal
        [ 1, "Figure", Some "https://example.com/in-figure"
          2, "H", None
          3, "Figure", Some "https://example.com/missed"
          4, "P", None ]

[<Fact>]
let ``a QR code with coloured finder patterns is found`` () =
    // Green modules with yellow finder rings, as on a styled flyer. Yellow is nearly as light
    // as white in ordinary luminance, which hides the rings the detector looks for.
    let code = ZXing.QrCode.Internal.Encoder.encode("https://example.com/styled", ZXing.QrCode.Internal.ErrorCorrectionLevel.H)
    let modules = code.Matrix
    let cell = 12
    let quiet = 4
    let size = (modules.Width + 2 * quiet) * cell
    use page = new SkiaSharp.SKBitmap(size + 400, size + 400)
    use canvas = new SkiaSharp.SKCanvas(page)
    canvas.Clear SkiaSharp.SKColors.White
    use green = new SkiaSharp.SKPaint(Color = SkiaSharp.SKColor(0x1Fuy, 0x5Auy, 0x2Auy))
    use yellow = new SkiaSharp.SKPaint(Color = SkiaSharp.SKColor(0xF5uy, 0xC5uy, 0x2Auy))
    let inFinderRing x y =
        let n = modules.Width
        [ 0, 0; n - 7, 0; 0, n - 7 ]
        |> List.exists (fun (fx, fy) ->
            let dx, dy = x - fx, y - fy
            dx >= 0 && dx < 7 && dy >= 0 && dy < 7 && (dx = 0 || dx = 6 || dy = 0 || dy = 6))
    for y in 0 .. modules.Height - 1 do
        for x in 0 .. modules.Width - 1 do
            if modules[x, y] = 1 then
                let paint = if inFinderRing x y then yellow else green
                canvas.DrawRect(float32 (200 + (quiet + x) * cell), float32 (200 + (quiet + y) * cell), float32 cell, float32 cell, paint)

    QrCodes.decodeAll page |> List.map fst |> should equal [ "https://example.com/styled" ]

[<Fact>]
let ``a QR code drawn as a soft mask, as Canva exports it, is rendered and found`` () =
    // A flat light image whose soft mask holds the code, drawn flipped in a form XObject with a
    // transparency group, over a dark box.
    use input = new TempPdf()
    do
        use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfWriter(input.Path))
        let page = document.AddNewPage(iText.Kernel.Geom.PageSize.LETTER)
        let matrix = ZXing.QrCode.QRCodeWriter().encode("https://example.com/canva", ZXing.BarcodeFormat.QR_CODE, 300, 300)
        let size = matrix.Width
        let stream (bytes: byte array) (colorSpace: iText.Kernel.Pdf.PdfName) =
            let image = iText.Kernel.Pdf.PdfStream(bytes)
            image.Put(iText.Kernel.Pdf.PdfName.Type, iText.Kernel.Pdf.PdfName.XObject) |> ignore
            image.Put(iText.Kernel.Pdf.PdfName.Subtype, iText.Kernel.Pdf.PdfName.Image) |> ignore
            image.Put(iText.Kernel.Pdf.PdfName.Width, iText.Kernel.Pdf.PdfNumber size) |> ignore
            image.Put(iText.Kernel.Pdf.PdfName.Height, iText.Kernel.Pdf.PdfNumber size) |> ignore
            image.Put(iText.Kernel.Pdf.PdfName.ColorSpace, colorSpace) |> ignore
            image.Put(iText.Kernel.Pdf.PdfName.BitsPerComponent, iText.Kernel.Pdf.PdfNumber 8) |> ignore
            image
        // Light modules where the code is dark: the mask is opaque on the code's dark modules,
        // so the light colour shows there, on the dark box.
        let mask = stream [| for y in 0 .. size - 1 do for x in 0 .. size - 1 -> if matrix[x, size - 1 - y] then 255uy else 0uy |] iText.Kernel.Pdf.PdfName.DeviceGray
        let image = stream (Array.create (size * size * 3) 240uy) iText.Kernel.Pdf.PdfName.DeviceRGB
        image.Put(iText.Kernel.Pdf.PdfName.SMask, mask.MakeIndirect document) |> ignore
        let xobject = iText.Kernel.Pdf.Xobject.PdfImageXObject(image.MakeIndirect document :?> iText.Kernel.Pdf.PdfStream)
        let form = iText.Kernel.Pdf.Xobject.PdfFormXObject(iText.Kernel.Geom.Rectangle(0f, 0f, 200f, 200f))
        let group = iText.Kernel.Pdf.PdfDictionary()
        group.Put(iText.Kernel.Pdf.PdfName.S, iText.Kernel.Pdf.PdfName.Transparency) |> ignore
        form.GetPdfObject().Put(iText.Kernel.Pdf.PdfName.Group, group) |> ignore
        let formCanvas = iText.Kernel.Pdf.Canvas.PdfCanvas(form, document)
        formCanvas.AddXObjectWithTransformationMatrix(xobject, 200f, 0f, 0f, -200f, 0f, 200f) |> ignore
        let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas(page)
        canvas.SetFillColorRgb(0.05f, 0.25f, 0.05f).Rectangle(150.0, 250.0, 300.0, 300.0).Fill() |> ignore
        canvas.AddXObjectAt(form, 200f, 300f) |> ignore

    use document = new Legibyl.Rendering.PdfiumDocument(input.Path)
    use rendered = document.Render(1, float32 (400.0 / 72.0))

    QrCodes.decodeAll rendered |> List.map fst |> should equal [ "https://example.com/canva" ]

[<Fact>]
let ``the link scanner finds undescribed web links and QR codes, without the layout model`` () =
    use input = new TempPdf()
    do
        use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfWriter(input.Path))
        let page = document.AddNewPage(iText.Kernel.Geom.PageSize.LETTER)
        let link (uri: string) (x: float32) (contents: string option) =
            let annotation = iText.Kernel.Pdf.Annot.PdfLinkAnnotation(iText.Kernel.Geom.Rectangle(x, 700f, 100f, 14f))
            annotation.SetAction(iText.Kernel.Pdf.Action.PdfAction.CreateURI uri) |> ignore
            contents |> Option.iter (fun c -> annotation.SetContents c |> ignore)
            page.AddAnnotation annotation |> ignore
        link "https://example.com/apply" 72f None
        link "https://example.com/described" 300f (Some "Already described")
        let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas(page)
        let matrix = ZXing.QrCode.QRCodeWriter().encode("https://example.com/scan", ZXing.BarcodeFormat.QR_CODE, 144, 144)
        for row in 0 .. matrix.Height - 1 do
            for column in 0 .. matrix.Width - 1 do
                if matrix[column, row] then canvas.Rectangle(72.0 + float column, 500.0 - float row, 1.0, 1.0) |> ignore
        canvas.Fill() |> ignore

    use scanner = new LinkScanner(input.Path)

    scanner.WebLinks() |> List.map (fun l -> l.Page, l.Address) |> should equal [ 1, "https://example.com/apply" ]
    scanner.QrCodes 1 |> List.map fst |> should equal [ "https://example.com/scan" ]
