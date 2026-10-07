module Legibyl.Cli.Tests.AppendCommandTests

open System
open System.IO
open System.Text.Json
open Xunit
open FsUnit.Xunit
open iText.Kernel.Pdf
open iText.Layout.Element
open iText.Pdfua

/// A one-page PDF/UA-1 file (iText's PDF/UA support makes it so): a heading and a paragraph.
let private createPassing (path: string) (title: string) (language: string) =
    use document = new PdfUADocument(new PdfWriter(path), PdfUAConfig(PdfUAConformance.PDF_UA_1, title, language))
    let font =
        iText.Kernel.Font.PdfFontFactory.CreateFont(
            Path.Combine(__SOURCE_DIRECTORY__, "..", "Legibyl.Validation.Tests", "SampleCorpus", "fonts", "Lato-Regular.ttf"),
            iText.IO.Font.PdfEncodings.WINANSI,
            iText.Kernel.Font.PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED
        )
    use layout = new iText.Layout.Document(document)
    layout.SetFont font |> ignore
    let heading = Paragraph title
    heading.GetAccessibilityProperties().SetRole(iText.Kernel.Pdf.Tagging.StandardRoles.H1) |> ignore
    layout.Add heading |> ignore
    layout.Add(Paragraph $"Everything about {title}.") |> ignore

/// Runs test with a folder holding garden.pdf and jardin.pdf, both passing.
let private withInputs (test: string -> string -> string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try
        let garden, jardin = Path.Combine(folder, "garden.pdf"), Path.Combine(folder, "jardin.pdf")
        createPassing garden "Garden" "en-US"
        createPassing jardin "Jardin" "es-ES"
        test folder garden jardin
    finally
        Directory.Delete(folder, true)

/// Runs `append` with args, returning its exit code and its JSON report.
let private append (args: string list) =
    let report = Path.GetTempFileName()
    try
        let code = Legibyl.Cli.Program.main (Array.ofList ("append" :: args @ [ "--report"; report ]))
        let json = if FileInfo(report).Length > 0L then Some(JsonDocument.Parse(File.ReadAllText report).RootElement.Clone()) else None
        code, json
    finally
        File.Delete report

[<Fact>]
let ``append joins the files into the first one's next version, and exits 0 when it passes`` () =
    withInputs (fun folder garden jardin ->
        let code, report = append [ garden; jardin ]
        let report = report.Value
        code |> should equal 0
        report.GetProperty("status").GetString() |> should equal "joined"
        report.GetProperty("outputPath").GetString() |> should equal (Path.Combine(folder, "garden.v2.pdf"))
        report.GetProperty("standard").GetString() |> should equal "PDF/UA-1"
        report.GetProperty("title").GetString() |> should equal "Garden"
        report.GetProperty("language").GetString() |> should equal "en-US"
        report.GetProperty("after").GetProperty("isCompliant").GetBoolean() |> should equal true
        [ for input in report.GetProperty("inputs").EnumerateArray() -> input.GetProperty("isCompliant").GetBoolean() ] |> should equal [ true; true ]
        report.GetProperty("structureDifferences").GetArrayLength() |> should equal 0
        File.Exists(Path.Combine(folder, "garden.v2.pdf")) |> should equal true)

[<Fact>]
let ``append refuses a file that fails, writes nothing, and exits 2`` () =
    withInputs (fun folder garden _ ->
        let failing = Path.Combine(folder, "failing.pdf")
        do
            use document = new PdfDocument(new PdfWriter(failing))
            document.AddNewPage() |> ignore
        let code, report = append [ garden; failing; "--output"; Path.Combine(folder, "packet.pdf") ]
        let report = report.Value
        code |> should equal 2
        report.GetProperty("status").GetString() |> should equal "refused"
        report.GetProperty("outputPath").ValueKind |> should equal JsonValueKind.Null
        [ for refusal in report.GetProperty("refusals").EnumerateArray() -> refusal.GetProperty("reason").GetString() ]
        |> should equal [ "It isn't tagged: it has no structure tree to keep." ]
        File.Exists(Path.Combine(folder, "packet.pdf")) |> should equal false)

[<Fact>]
let ``append takes two files or more, a part tag its standard has, and a language tag`` () =
    withInputs (fun _ garden jardin ->
        append [ garden ] |> fst |> should equal 64
        append [ garden; jardin; "--part-tag"; "DocumentFragment" ] |> fst |> should equal 64
        append [ garden; jardin; "--lang"; "English (US)" ] |> fst |> should equal 64)
