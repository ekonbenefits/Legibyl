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

    IO.File.Copy(corpusFile "pdfua1-pass.pdf", input.Path, true)
    IO.File.SetLastWriteTimeUtc(input.Path, DateTime.UtcNow.AddMinutes 1.0)
    let after = renderer.RenderPageWithHighlight(input.Path, region 50.0 600.0 200.0 700.0)

    after.PngBytes |> should not' (equal before.PngBytes)
