module Legibyl.Validation.Tests.TableStructureTests

open System
open Xunit
open FsUnit.Xunit
open SkiaSharp
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Layout
open Legibyl.Validation.Tests.TestSupport

let private found label (left, top, right, bottom) : TableStructure.Found =
    { Label = label; Score = 0.9f; Box = SKRect(float32 left, float32 top, float32 right, float32 bottom) }

/// Two rows (the first a header) and three columns, as the model would find them.
let private plain =
    [ found "row" (10, 10, 310, 40)
      found "row" (10, 42, 310, 72)
      found "column" (10, 10, 100, 72)
      found "column" (105, 10, 200, 72)
      found "column" (205, 10, 310, 72)
      found "column header" (10, 10, 310, 40) ]

let private shape (cells: TableStructure.Cell list) =
    cells |> List.map (fun c -> c.Header, int c.Box.Left, int c.Box.Top, int c.Box.Right, int c.Box.Bottom) |> List.sort

[<Fact>]
let ``rows and columns make a cell for each, edge to edge, header cells in the header's rows`` () =
    TableStructure.grid plain
    |> shape
    // Each band reaches halfway to the next: rows split at 41, columns at 102 and 202.
    |> should
        equal
        ([ true, 10, 10, 102, 41; true, 102, 10, 202, 41; true, 202, 10, 310, 41
           false, 10, 41, 102, 72; false, 102, 41, 202, 72; false, 202, 41, 310, 72 ]
         |> List.sort)

[<Fact>]
let ``a spanning cell takes the cells it covers, and a projected row header the whole row`` () =
    let spanning = plain @ [ found "spanning cell" (105, 10, 310, 40) ]
    TableStructure.grid spanning |> shape |> should equal ([ true, 10, 10, 102, 41; true, 102, 10, 310, 41; false, 10, 41, 102, 72; false, 102, 41, 202, 72; false, 202, 41, 310, 72 ] |> List.sort)
    let section = plain @ [ found "projected row header" (10, 42, 120, 72) ]
    TableStructure.grid section |> shape |> should equal ([ true, 10, 10, 102, 41; true, 102, 10, 202, 41; true, 202, 10, 310, 41; false, 10, 41, 310, 72 ] |> List.sort)

[<Fact>]
let ``the same row found twice is one, and one cell is no table`` () =
    let twice = plain @ [ { found "row" (10, 12, 310, 39) with Score = 0.6f } ]
    TableStructure.grid twice |> List.length |> should equal 6
    TableStructure.grid [ found "row" (0, 0, 10, 10); found "column" (0, 0, 10, 10) ] |> should be Empty

// ---- The model ----

/// An untagged page with a ruled table: a header row and four rows of three columns.
let private createTable (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage PageSize.LETTER
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let bold = PdfFontFactory.CreateFont StandardFonts.HELVETICA_BOLD
    let text (f: PdfFont) (x: float) (y: float) (value: string) =
        canvas.BeginText().SetFontAndSize(f, 11f).MoveText(x, y).ShowText(value).EndText() |> ignore
    canvas.BeginText().SetFontAndSize(bold, 16f).MoveText(72.0, 740.0).ShowText("Planting times").EndText() |> ignore
    text font 72.0 715.0 "When to sow and harvest what grows in the community garden."
    let rows =
        [ [ "Plant"; "Sow"; "Harvest" ]
          [ "Tomato"; "March"; "July" ]
          [ "Lettuce"; "April"; "June" ]
          [ "Carrot"; "April"; "August" ]
          [ "Bean"; "May"; "August" ] ]
    for r, row in List.indexed rows do
        for x, value in List.zip [ 80.0; 230.0; 380.0 ] row do
            text (if r = 0 then bold else font) x (680.0 - float r * 24.0) value
    for r in 0..5 do canvas.MoveTo(72.0, 696.0 - float r * 24.0).LineTo(520.0, 696.0 - float r * 24.0).Stroke() |> ignore
    for x in [ 72.0; 220.0; 370.0; 520.0 ] do canvas.MoveTo(x, 576.0).LineTo(x, 696.0).Stroke() |> ignore
    text font 72.0 540.0 "Water the beds in the morning."

let private withTableModel test =
    if not (TableModel.isDownloaded ()) then
        Assert.Skip "The table model isn't downloaded; open Suggest Structure in the app to fetch it."
    test ()

[<Fact>]
let ``the model finds a ruled table's rows, columns and header`` () =
    withTableModel (fun () ->
        use pdf = new TempPdf()
        createTable pdf.Path
        use pages = new Legibyl.Rendering.PdfiumDocument(pdf.Path)
        let scale = 4.0f
        use page = pages.Render(1, scale)
        // The table, with a little margin, in pixels from the page's top-left (792 points tall).
        let area = SKRectI(int (66f * scale), int ((792f - 702f) * scale), int (526f * scale), int ((792f - 570f) * scale))
        use table = new SKBitmap()
        page.ExtractSubset(table, area) |> should be True
        use session = new Microsoft.ML.OnnxRuntime.InferenceSession(TableModel.path ())
        let cells = TableStructure.grid (TableStructure.find session table)

        cells |> List.length |> should equal 15
        cells |> List.filter _.Header |> List.length |> should equal 3)

[<Fact>]
let ``suggesting a page's structure gives its table's cells after it, each with its words`` () =
    withTableModel (fun () ->
        if not (LayoutModel.isDownloaded ()) then
            Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
        use pdf = new TempPdf()
        createTable pdf.Path
        use suggester = new LayoutSuggester(pdf.Path)
        let blocks = (suggester.Suggest 1).Blocks
        let table = blocks |> List.findIndex (fun b -> b.Tag = "Table")
        let cells = blocks |> List.skip (table + 1) |> List.takeWhile (fun b -> b.Tag = "TH" || b.Tag = "TD")

        [ for cell in cells -> cell.Tag, cell.Text ]
        |> should
            equal
            [ "TH", "Plant"; "TH", "Sow"; "TH", "Harvest"
              "TD", "Tomato"; "TD", "March"; "TD", "July"
              "TD", "Lettuce"; "TD", "April"; "TD", "June"
              "TD", "Carrot"; "TD", "April"; "TD", "August"
              "TD", "Bean"; "TD", "May"; "TD", "August" ]
        blocks |> List.map _.Order |> should equal [ 1 .. blocks.Length ])

[<Fact>]
let ``tagging an untagged page from its layout tags its table with rows and cells`` () =
    withTableModel (fun () ->
        if not (LayoutModel.isDownloaded ()) then
            Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
        use input = new TempPdf()
        use output = new TempPdf()
        createTable input.Path
        use source = new LayoutRegionSource(null)
        let validator = Legibyl.Validation.VeraPdfValidator(LayoutSource = source)
        let result = validator.Fix(input.Path, Legibyl.Validation.PdfProfile.PdfUa1, output.Path, Legibyl.Validation.PdfFixOptions.TagFromLayout)

        result.AppliedFixes |> should contain "Tagged 1 table with rows and cells, from their layout: to check, the header row above all"
        let root = Legibyl.Validation.VeraPdfValidator().ReadStructureTree output.Path |> notNull
        let rec find (node: Legibyl.Validation.StructureNode) : Legibyl.Validation.StructureNode list =
            (if node.Tag = "Table" then [ node ] else []) @ [ for child in node.Children do yield! find child ]
        let table = find root |> List.exactlyOne
        [ for row in table.Children -> [ for cell in row.Children -> cell.Tag, cell.Content ] ]
        |> List.head
        |> should equal [ "TH", "Plant"; "TH", "Sow"; "TH", "Harvest" ]
        table.Children |> Seq.length |> should equal 5
        let failed =
            Legibyl.Validation.VeraPdfValidator().Validate(output.Path, Legibyl.Validation.PdfProfile.PdfUa1).Findings
            |> Seq.filter (fun f -> not f.Passed && f.Severity = Legibyl.Validation.FindingSeverity.Failure)
            |> Seq.map _.RuleId
            |> Set.ofSeq
        failed |> Set.filter (fun rule -> rule.StartsWith "7.5" || rule = "7.1-3") |> should be Empty)
