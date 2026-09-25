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
    let renderer = PdfRegionRenderer()
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
    let renderer = PdfRegionRenderer()
    let location = PdfLocationParser.Parse "root/document[0]" |> notNull

    (fun () -> renderer.RenderPageWithHighlight(corpusFile "pdfua1-fail.pdf", location) |> ignore)
    |> should throw typeof<InvalidOperationException>

[<Fact>]
let ``RenderPageWithHighlight page out of range throws`` () =
    let renderer = PdfRegionRenderer()
    let location = PdfLocationParser.Parse "root/document[0]/pages[999](11 0 obj PDPage)" |> notNull

    (fun () -> renderer.RenderPageWithHighlight(corpusFile "pdfua1-fail.pdf", location) |> ignore)
    |> should throw typeof<InvalidOperationException>
