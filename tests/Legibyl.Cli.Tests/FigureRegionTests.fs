module Legibyl.Cli.Tests.FigureRegionTests

open System
open System.IO
open System.Text.Json
open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Tagging
open Legibyl.Validation

type private Folder() =
    let path = Path.GetFullPath($"figure-cli-{Guid.NewGuid():N}")
    do Directory.CreateDirectory path |> ignore
    member _.File name = Path.Combine(path, name)
    interface IDisposable with
        member _.Dispose() = Directory.Delete(path, true)

let private create (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString "en-US") |> ignore
    let page = document.AddNewPage()
    let top = PdfStructElem(document, PdfName.Document)
    document.GetStructTreeRoot().AddKid top |> ignore
    let paragraph = PdfStructElem(document, PdfName.P, page)
    top.AddKid paragraph |> ignore
    let mcr = PdfMcrNumber(page, paragraph)
    paragraph.AddKid mcr |> ignore
    let properties = PdfDictionary()
    properties.Put(PdfName.MCID, PdfNumber(mcr.GetMcid())) |> ignore
    let canvas = PdfCanvas page
    canvas.BeginMarkedContent(PdfName.P, properties).BeginText()
        .SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f)
        .MoveText(75.0, 610.0).ShowText("Chart label").EndText().EndMarkedContent() |> ignore
    canvas.BeginMarkedContent(PdfName.Artifact).Rectangle(75.0, 500.0, 80.0, 80.0).Fill().EndMarkedContent() |> ignore

let private review box =
    $"""{{"version":1,"edits":[{{"page":1,"tag":"FigureRegion","retag":"Figure","box":{box},"alt":"A rising bar."}}]}}"""

let private run input output report edits =
    Legibyl.Cli.Program.main (Array.ofList (["fix"; input; "--output"; output; "--report"; report; "--fixes"; "none"] @ edits)) |> ignore
    use json = JsonDocument.Parse(File.ReadAllText report)
    json.RootElement[0].Clone()

let rec private nodes (node: StructureNode) =
    node :: (node.Children |> Seq.collect nodes |> List.ofSeq)

[<Fact>]
let ``CLI edits and edits-from write the same region figure and carry its review`` () =
    use folder = new Folder()
    let input = folder.File "chart.pdf"
    create input
    let source = File.ReadAllBytes input
    let saved = ReviewFile.filePath input
    File.WriteAllText(saved, review "[60,480,180,650]")
    for index, flags in [0, ["--edits"]; 1, ["--edits-from"; saved]] do
        let output = folder.File $"copy-{index}.pdf"
        let report = folder.File $"report-{index}.json"
        let result = run input output report flags
        let applied = result.GetProperty("applied").EnumerateArray() |> Seq.map (fun s -> string (s.GetString())) |> List.ofSeq
        applied |> should equal ["Replaced structure in 1 selected region(s) with a Figure and your alt text, preserving the artwork."]
        let tree = VeraPdfValidator().ReadStructureTree output |> nonNull |> nodes
        tree |> List.filter (fun n -> n.Tag = "Figure") |> List.map _.AlternateDescription |> should equal ["A rising bar."]
        tree |> List.exists (fun n -> n.Tag = "P") |> should be False
        let carried = EmbeddedReview.read output |> Option.bind ReviewFile.parse |> Option.get |> ReviewFile.edits
        carried.Head.Tag |> should equal "FigureRegion"
        File.ReadAllBytes input |> should equal source

[<Fact>]
let ``CLI reports boundary refusal and writes no figure copy`` () =
    use folder = new Folder()
    let input = folder.File "chart.pdf"
    create input
    File.WriteAllText(ReviewFile.filePath input, review "[60,550,180,650]")
    let output = folder.File "refused.pdf"
    let result = run input output (folder.File "report.json") ["--edits"]
    File.Exists output |> should be False
    result.GetProperty("notApplied").EnumerateArray()
    |> Seq.exists (fun s -> (string (s.GetString())).Contains "crosses the region boundary") |> should be True
