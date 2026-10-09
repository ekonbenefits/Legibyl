module Legibyl.Cli.Tests.WcagCommandTests

open System
open System.IO
open System.Text.Json
open Xunit
open FsUnit.Xunit
open iText.Kernel.Colors
open iText.Kernel.Pdf
open iText.Layout.Element
open iText.Pdfua

/// A PDF/UA-1 page (iText's PDF/UA support makes it so) whose one line is light grey on white:
/// it complies with PDF/UA, and fails WCAG 2.2's contrast.
let private createLowContrast (path: string) =
    use document = new PdfUADocument(new PdfWriter(path), PdfUAConfig(PdfUAConformance.PDF_UA_1, "Garden Colours", "en-US"))
    let font =
        iText.Kernel.Font.PdfFontFactory.CreateFont(
            Path.Combine(__SOURCE_DIRECTORY__, "..", "Legibyl.Validation.Tests", "SampleCorpus", "fonts", "Lato-Regular.ttf"),
            iText.IO.Font.PdfEncodings.WINANSI,
            iText.Kernel.Font.PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED
        )
    use layout = new iText.Layout.Document(document)
    layout.SetFont font |> ignore
    layout.Add(Paragraph("Light grey on white is hard to read.").SetFontColor(DeviceRgb(0xCC, 0xCC, 0xCC))) |> ignore

[<Fact>]
let ``validate fails a PDF that passes PDF/UA but not WCAG 2.2, and says which`` () =
    let pdf = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}.pdf")
    let json = Path.GetTempFileName()
    let text = Path.GetTempFileName()
    try
        createLowContrast pdf
        let code = Legibyl.Cli.Program.main [| "validate"; pdf; "--output"; json |]
        Legibyl.Cli.Program.main [| "validate"; pdf; "--format"; "text"; "--output"; text |] |> ignore
        let result = JsonDocument.Parse(File.ReadAllText json).RootElement[0]
        let firstLine = (File.ReadAllLines text)[0]

        code |> should equal 1
        result.GetProperty("isCompliant").GetBoolean() |> should be True
        result.GetProperty("meetsWcag").GetBoolean() |> should be False
        result.GetProperty("wcagFailedCount").GetInt32() |> should be (greaterThanOrEqualTo 1)
        [ for f in result.GetProperty("failures").EnumerateArray() -> f.GetProperty("specification").GetString(), f.GetProperty("clause").GetString() ]
        |> should contain ("WCAG 2.2 AA", "1.4.3")
        firstLine |> should haveSubstring ": FAIL (PDF/UA-1 passes; WCAG 2.2: "
    finally
        for file in [ pdf; json; text ] do
            if File.Exists file then File.Delete file
