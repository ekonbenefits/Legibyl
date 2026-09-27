module Legibyl.Rendering.Tests.PdfRegionRendererTests

open System
open Xunit
open FsUnit.Xunit
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

[<Literal>]
let private PngSignatureHex = "89504E47"

let private shouldBePng (bytes: byte array) =
    bytes.Length |> should be (greaterThan 8)
    Convert.ToHexString(bytes, 0, 4) |> should equal PngSignatureHex

[<Fact>]
let ``RenderPageWithHighlight content item failure renders full page without precise region`` () =
    // The known-bad corpus PDF's failures are content-marking issues (Artifact/tagged content),
    // which have no attached geometry in veraPDF's model - only annotations do currently.
    use renderer = new PdfRegionRenderer()
    let location =
        PdfLocationParser.Parse
            "root/document[0]/pages[0](11 0 obj PDPage)/contentStream[0](21 0 obj PDSemanticContentStream)/content[10]/contentItem[0]"
        |> notNull

    let result = renderer.RenderPageWithHighlight(corpusFile "pdfua1-fail.pdf", location)

    result.PageNumber |> should equal 1
    result.HasPreciseRegion |> should be False
    result.Note |> should not' (be EmptyString)
    shouldBePng result.PngBytes

[<Fact>]
let ``RenderPageWithHighlight location without page number throws`` () =
    use renderer = new PdfRegionRenderer()
    let location = PdfLocationParser.Parse "root/document[0]" |> notNull

    (fun () -> renderer.RenderPageWithHighlight(corpusFile "pdfua1-fail.pdf", location) |> ignore)
    |> should throw typeof<InvalidOperationException>

[<Fact>]
let ``RenderPageWithHighlight page out of range throws`` () =
    use renderer = new PdfRegionRenderer()
    let location = PdfLocationParser.Parse "root/document[0]/pages[999](11 0 obj PDPage)" |> notNull

    (fun () -> renderer.RenderPageWithHighlight(corpusFile "pdfua1-fail.pdf", location) |> ignore)
    |> should throw typeof<InvalidOperationException>

let private region left bottom right top =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

[<Fact>]
let ``RenderPageWithHighlight on a cached page doesn't keep earlier highlights`` () =
    use cached = new PdfRegionRenderer()
    use fresh = new PdfRegionRenderer()
    let pdf = corpusFile "pdfua1-fail.pdf"

    cached.RenderPageWithHighlight(pdf, region 50.0 600.0 200.0 700.0) |> ignore
    let second = cached.RenderPageWithHighlight(pdf, region 300.0 100.0 450.0 200.0)

    second.PngBytes |> should equal (fresh.RenderPageWithHighlight(pdf, region 300.0 100.0 450.0 200.0)).PngBytes

[<Fact>]
let ``RenderPageWithHighlight rereads a file that changed on disk`` () =
    use renderer = new PdfRegionRenderer()
    use input = new TempPdf()
    IO.File.Copy(corpusFile "pdfua1-fail.pdf", input.Path, true)
    let before = renderer.RenderPageWithHighlight(input.Path, region 50.0 600.0 200.0 700.0)

    // A visibly different page. (pdfua1-pass.pdf draws the same page as pdfua1-fail.pdf; they
    // differ only in their tags.)
    IO.File.Copy(corpusFile "libreoffice-empty-paragraph.pdf", input.Path, true)
    IO.File.SetLastWriteTimeUtc(input.Path, DateTime.UtcNow.AddMinutes 1.0)
    let after = renderer.RenderPageWithHighlight(input.Path, region 50.0 600.0 200.0 700.0)

    after.PngBytes |> should not' (equal before.PngBytes)

/// The color of the rendered image at a point given in PDF coordinates (scale 2).
let private colorAt (png: byte array) (x: float) (y: float) =
    use bitmap = SkiaSharp.SKBitmap.Decode png
    bitmap.GetPixel(int (x * 2.0), bitmap.Height - int (y * 2.0))

[<Fact>]
let ``RenderPageWithTarget marks where a link goes in blue`` () =
    use renderer = new PdfRegionRenderer()
    let destination =
        { PageNumber = 1
          Kind = "XYZ"
          Left = Nullable 0.0
          Top = Nullable 400.0
          Right = Nullable()
          Bottom = Nullable() }

    let result = renderer.RenderPageWithTarget(corpusFile "pdfua1-fail.pdf", destination)

    result.PageNumber |> should equal 1
    result.HasPreciseRegion |> should be True
    // Inside the band just below the view's top edge: tinted blue, unlike a finding's red.
    let inside = colorAt result.PngBytes 300.0 390.0
    inside.Blue |> should be (greaterThan inside.Red)
    // Well outside it, the page is untouched.
    let outside = colorAt result.PngBytes 300.0 200.0
    colorAt (renderer.RenderPageWithHighlight(corpusFile "pdfua1-fail.pdf", region 0.0 0.0 1.0 1.0).PngBytes) 300.0 200.0
    |> should equal outside

[<Fact>]
let ``RenderPageWithTarget outlines the page when the link gives no position`` () =
    use renderer = new PdfRegionRenderer()
    let destination =
        { PageNumber = 1
          Kind = "Fit"
          Left = Nullable()
          Top = Nullable()
          Right = Nullable()
          Bottom = Nullable() }

    let result = renderer.RenderPageWithTarget(corpusFile "pdfua1-fail.pdf", destination)

    result.HasPreciseRegion |> should be False
    result.Note |> should haveSubstring "without a position"

[<Fact>]
let ``RenderPageWithRegions outlines each region in its color, filling the emphasised one`` () =
    use renderer = new PdfRegionRenderer()
    let region left bottom right top color emphasised =
        { Geometry = { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }
          Label = "1 P"
          Color = color
          Emphasised = emphasised
          Marker = None }

    let png =
        renderer.RenderPageWithRegions(
            corpusFile "pdfua1-fail.pdf",
            1,
            [ region 100.0 100.0 200.0 200.0 0x00C000u false; region 300.0 100.0 400.0 200.0 0xE00000u true ]
        )

    shouldBePng png
    let outline = colorAt png 100.0 150.0
    outline.Green |> should be (greaterThan outline.Red)
    let filled = colorAt png 350.0 150.0
    filled.Red |> should be (greaterThan filled.Green)

[<Fact>]
let ``RenderPageWithRegions draws a region's marker at its top-right corner`` () =
    use renderer = new PdfRegionRenderer()
    let region =
        { Geometry = { PageNumber = 1; Left = 100.0; Bottom = 100.0; Right = 200.0; Top = 200.0 }
          Label = "1 Figure"
          Color = 0x00C000u
          Emphasised = false
          Marker = Some(0xE53935u, "2") }

    let png = renderer.RenderPageWithRegions(corpusFile "pdfua1-fail.pdf", 1, [ region ])

    // Just inside the corner, off the marker's text.
    let marker = colorAt png 196.0 196.0
    marker.Red |> should be (greaterThan 200uy)
    marker.Green |> should be (lessThan 100uy)

[<Fact>]
let ``PDFium draws each page at the requested scale, and refuses pages that don't exist`` () =
    use document = new PdfiumDocument(corpusFile "pdfua1-fail.pdf")
    document.PageCount |> should equal 1
    use page = document.Render(1, 2f)
    (page.Width, page.Height) |> should equal (1224, 1584)
    page.Pixels |> Array.exists (fun c -> c.Red < 128uy) |> should be True
    (fun () -> document.Render(2, 1f) |> ignore) |> should throw typeof<InvalidOperationException>

let private area (left, bottom, right, top) : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

[<Fact>]
let ``RenderRegion renders just the region, with a margin, at twice its size in points`` () =
    use renderer = new PdfRegionRenderer()

    let png = renderer.RenderRegion(corpusFile "pdfua1-pass.pdf", area (100.0, 500.0, 200.0, 550.0), 10.0)

    shouldBePng png
    use image = SkiaSharp.SKBitmap.Decode png
    // 100 x 50 points, 10 points of margin each side, at 2 pixels a point.
    (image.Width, image.Height) |> should equal (240, 140)

[<Fact>]
let ``RenderRegion keeps a large region's longest side to the size asked for`` () =
    use renderer = new PdfRegionRenderer()

    let png = renderer.RenderRegion(corpusFile "pdfua1-pass.pdf", area (0.0, 0.0, 600.0, 300.0), 0.0, 400)

    use image = SkiaSharp.SKBitmap.Decode png
    (image.Width, image.Height) |> should equal (400, 200)
