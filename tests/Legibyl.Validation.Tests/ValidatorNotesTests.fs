module Legibyl.Validation.Tests.ValidatorNotesTests

open System
open System.IO
open System.Threading.Tasks
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A page whose paragraphs each hold marked content inside marked content (as a Pages export's
/// lists do), which veraPDF logs as it reads the page: once per paragraph.
let createNestedPdf (path: string) (paragraphs: int) =
    use document = new PdfDocument(new PdfWriter(path))
    let canvas = PdfCanvas(document.AddNewPage())
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let marked (tag: string) (mcid: int) =
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
        canvas.BeginMarkedContent(PdfName tag, properties) |> ignore
    for i in 0 .. paragraphs - 1 do
        marked "P" (2 * i)
        marked "Span" (2 * i + 1)
        canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 700.0 - 20.0 * float i).ShowText($"Item {i + 1}").EndText() |> ignore
        canvas.EndMarkedContent().EndMarkedContent() |> ignore

/// What's written to Console.Error while f runs, and what f gives.
let private withErrorCaptured (f: unit -> 'T) =
    let original = Console.Error
    use writer = new StringWriter()
    Console.SetError writer
    try
        let result = f ()
        result, writer.ToString()
    finally
        Console.SetError original

[<Fact>]
let ``what veraPDF logs while it reads a file is on the report, counted, not on the console`` () =
    use input = new TempPdf()
    createNestedPdf input.Path 3

    let report, written = withErrorCaptured (fun () -> VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1))

    let note = report.Notes |> single (fun n -> n.Message.Contains "Nested MCID")
    note.Count |> should equal 3
    note.Level |> should equal "WARNING"
    note.Logger |> should equal "OperatorParser"
    // Without the number each repeat ends with.
    note.Message |> should endWith "Nested MCID"
    written |> should not' (haveSubstring "Nested MCID")

[<Fact>]
let ``a clean file has no notes`` () =
    let report = VeraPdfValidator().Validate(corpusFile "pdfua1-pass.pdf", PdfProfile.PdfUa1)

    report.Notes |> should be Empty

[<Fact>]
let ``two validations at once each get only their own notes`` () =
    use nested = new TempPdf()
    createNestedPdf nested.Path 5

    let validate path = Task.Run(fun () -> VeraPdfValidator().Validate(path, PdfProfile.PdfUa1))
    let reports = Task.WhenAll([ for _ in 1..3 do validate nested.Path; validate (corpusFile "pdfua1-pass.pdf") ]).Result

    for report in reports do
        if report.FilePath = nested.Path then
            (report.Notes |> single (fun n -> n.Message.Contains "Nested MCID")).Count |> should equal 5
        else
            report.Notes |> should be Empty

[<Fact>]
let ``a saved result keeps its notes`` () =
    use input = new TempPdf()
    createNestedPdf input.Path 2
    let report = VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1)

    let saved = SavedResults.ofReport report (SavedResults.hashOf input.Path) DateTime.UtcNow
    let back = SavedResults.toReport saved input.Path

    back.Notes |> should equal report.Notes
