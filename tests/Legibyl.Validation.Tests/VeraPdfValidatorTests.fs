module Legibyl.Validation.Tests.VeraPdfValidatorTests

open System
open iText.IO.Image
open iText.Kernel.Pdf
open iText.Layout
open iText.Layout.Element
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

/// A tagged PDF with `figures` sections, each a page or more of text followed by a figure
/// that has no alt text.
let private createPdfWithUntitledFigures (path: string) (figures: int) =
    use pdf = new PdfDocument(new PdfWriter(path))
    pdf.SetTagged() |> ignore
    use document = new Document(pdf)
    let pixel = ImageDataFactory.Create(2, 2, 3, 8, Array.create 12 128uy, null)
    for section in 1 .. figures do
        for line in 1 .. 12 do
            document.Add(Paragraph(String.replicate 6 $"Section {section}, paragraph {line}: text for veraPDF to check. ")) |> ignore
        document.Add(Image(pixel).SetWidth(20f)) |> ignore
        if section < figures then
            document.Add(AreaBreak()) |> ignore

[<Fact>]
let ``Validate reports every occurrence of a repeated failure`` () =
    // veraPDF keeps only the first failure of each rule once it has recorded 10,000 results,
    // which logging passed checks reached within a few pages.
    use input = new TempPdf()
    createPdfWithUntitledFigures input.Path 4

    let report = VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1)

    report.TotalAssertions |> should be (greaterThan 10000)
    // One missing-alt-text failure per figure, each placed on its own page.
    report.FailedAssertions
    |> Seq.filter (fun a -> a.RuleId = "7.3-1")
    |> Seq.map (fun a -> (a.Geometry |> notNull).PageNumber)
    |> Seq.distinct
    |> Seq.length
    |> should equal 4

[<Fact>]
let ``Validate reports progress, ending with the final counts`` () =
    use input = new TempPdf()
    createPdfWithUntitledFigures input.Path 10
    let reports = Collections.Concurrent.ConcurrentQueue<ValidationProgress>()
    let progress = { new IProgress<ValidationProgress> with member _.Report value = reports.Enqueue value }

    let report = VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1, progress)

    reports |> should not' (be Empty)
    let last = Seq.last reports
    last.Checks |> should equal report.TotalAssertions
    last.FailedRules |> should equal (report.FailedAssertions |> Seq.distinctBy _.RuleId |> Seq.length)
    last.ProcessedObjects |> should be (greaterThan 0)
    last.QueuedObjects |> should equal 0
    last.Stage |> should equal ValidationStage.Finishing
    last.PageCount |> should be (greaterThanOrEqualTo 10)
    last.Page |> should equal last.PageCount
    // Seeing a page mid-run shows Legibyl can still read where veraPDF is (a private field).
    reports |> Seq.exists (fun r -> r.Stage = ValidationStage.CheckingPages && r.Page > 0) |> should be True
    // Pages are checked in order, so neither the page nor the stage ever goes backwards.
    let inOrder (values: int list) = values |> List.pairwise |> List.forall (fun (a, b) -> a <= b)
    reports |> Seq.map _.Page |> List.ofSeq |> inOrder |> should be True
    reports |> Seq.map (_.Stage >> int) |> List.ofSeq |> inOrder |> should be True

[<Fact>]
let ``ReadStructureTree reports its stages in order, page by page`` () =
    use input = new TempPdf()
    createPdfWithUntitledFigures input.Path 3
    let reports = Collections.Concurrent.ConcurrentQueue<StructureReadProgress>()
    let progress = { new IProgress<StructureReadProgress> with member _.Report value = reports.Enqueue value }

    VeraPdfValidator().ReadStructureTree(input.Path, progress) |> notNull |> ignore

    let stages = reports |> Seq.map (_.Stage >> int) |> List.ofSeq
    stages |> List.pairwise |> List.forall (fun (a, b) -> a <= b) |> should be True
    stages |> List.distinct |> should equal [ 0; 1; 2 ]
    let textReports = reports |> Seq.filter (fun r -> r.Stage = StructureReadStage.ReadingText) |> List.ofSeq
    textReports.Head.Page |> should equal 1
    textReports |> List.forall (fun r -> r.PageCount >= 3 && r.Page <= r.PageCount) |> should be True
