module Legibyl.Validation.Tests.VeraPdfValidatorTests

open System
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

[<Fact>]
let ``Validate known good PDF/UA-1 document reports compliant`` () =
    let validator = VeraPdfValidator()

    let report = validator.Validate(corpusFile "pdfua1-pass.pdf", PdfProfile.PdfUa1)

    report.IsCompliant |> should be True
    report.FlavourId |> should equal "ua1"
    report.TotalAssertions |> should be (greaterThan 0)
    report.FailedAssertions |> should be Empty

[<Fact>]
let ``Validate known bad PDF/UA-1 document reports non-compliant with failed rules`` () =
    let validator = VeraPdfValidator()

    let report = validator.Validate(corpusFile "pdfua1-fail.pdf", PdfProfile.PdfUa1)

    report.IsCompliant |> should be False
    report.FailedAssertions |> should not' (be Empty)
    report.FailedAssertions |> Seq.map (fun a -> a.Clause) |> should contain "7.1"

[<Fact>]
let ``Validate missing file throws VeraPdfValidationException`` () =
    let validator = VeraPdfValidator()

    (fun () -> validator.Validate(corpusFile "does-not-exist.pdf", PdfProfile.PdfUa1) |> ignore)
    |> should throw typeof<VeraPdfValidationException>

[<Fact>]
let ``Validate clipping path inside marked content does not inflate region`` () =
    let validator = VeraPdfValidator()

    let report = validator.Validate(corpusFile "pages-clipped-paragraphs.pdf", PdfProfile.PdfUa1)

    // The first paragraph (MCID 1) is a single line near the top of the page, drawn
    // inside a page-sized clipping rectangle.
    let firstParagraph =
        report.FailedAssertions
        |> Seq.find (fun a ->
            match a.LocationContext with
            | NonNull context -> context.Contains("{mcid:1}", StringComparison.Ordinal)
            | Null -> false)
    let geometry = firstParagraph.Geometry |> notNull
    geometry.Top - geometry.Bottom |> should be (inRange 5.0 20.0)
    geometry.Bottom |> should be (inRange 700.0 720.0)
