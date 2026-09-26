module Legibyl.Cli.Tests.LayoutFileTests

open System
open System.IO
open System.Text.Json
open Xunit
open FsUnit.Xunit
open iText.IO.Image
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Layout
open Legibyl.Validation

let private block order tag (l, b, r, t) text : SuggestedBlock =
    { Order = order
      Label = "text"
      Tag = tag
      Score = 0.9
      Geometry = { PageNumber = 1; Left = l; Bottom = b; Right = r; Top = t }
      Text = text
      ListNumbering = None
      Recognized = false
      QrText = None }

let private suggestion: PageSuggestion =
    { PageNumber = 1
      HasText = true
      Rotated = false
      Blocks =
        [ block 1 "H" (70.0, 695.0, 300.0, 715.0) "The garden"
          { block 2 "Figure" (70.0, 548.0, 175.0, 652.0) "" with Label = "image" } ] }

/// An untagged page, a heading and an image, in a fresh folder.
let private withUntaggedPdf (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try
        let pdf = Path.Combine(folder, "garden.pdf")
        do
            use document = new PdfDocument(new PdfWriter(pdf))
            let canvas = PdfCanvas(document.AddNewPage())
            canvas.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA_BOLD, 16f).MoveText(72.0, 700.0).ShowText("The garden").EndText()
            |> ignore
            let pixels = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 255uy; 255uy; 0uy |]
            canvas.AddImageWithTransformationMatrix(ImageDataFactory.Create(2, 2, 3, 8, pixels, null), 100f, 0f, 0f, 100f, 72f, 550f, true) |> ignore
        test pdf
    finally
        Directory.Delete(folder, true)

let private run (args: string list) =
    let output = Path.GetTempFileName()
    try
        let code = Legibyl.Cli.Program.main (Array.ofList (args @ [ "--output"; output ]))
        let json = if File.Exists output && FileInfo(output).Length > 0L then Some(JsonDocument.Parse(File.ReadAllText output)) else None
        code, json |> Option.map (fun json -> json.RootElement[0])
    finally
        File.Delete output

let rec private nodes (node: StructureNode) : StructureNode list =
    node :: (node.Children |> Seq.collect nodes |> List.ofSeq)

[<Fact>]
let ``a suggested layout reads back as the regions it suggested`` () =
    let path = Path.GetTempFileName()
    try
        File.WriteAllText(path, LayoutFile.toJson (LayoutFile.ofSuggestions "garden.pdf" [ suggestion ] (Some "The garden")))
        let layout =
            match LayoutFile.read path with
            | Ok layout -> layout
            | Error message -> failwith message
        let regions = (LayoutFile.FileRegions layout :> IPageLayoutSource).PageRegions("garden.pdf", 1)

        layout.Title |> should equal "The garden"
        regions |> Seq.map (fun r -> r.Tag, r.Geometry.Left, r.Geometry.Top) |> List.ofSeq |> should equal [ "H", 70.0, 715.0; "Figure", 70.0, 652.0 ]
    finally
        File.Delete path

[<Fact>]
let ``a layout with an unknown tag or a bad box says what's wrong`` () =
    let path = Path.GetTempFileName()
    try
        let layout = LayoutFile.ofSuggestions "garden.pdf" [ suggestion ] None
        let broken =
            { layout with
                Pages =
                    [| { layout.Pages[0] with
                           Regions = [| { layout.Pages[0].Regions[0] with Tag = "Heading" }; { layout.Pages[0].Regions[1] with Box = [| 1.0 |] } |] } |] }
        File.WriteAllText(path, LayoutFile.toJson broken)

        match LayoutFile.read path with
        | Ok _ -> failwith "Expected problems."
        | Error message ->
            message |> should haveSubstring "Page 1, region 1: unknown tag \"Heading\""
            message |> should haveSubstring "Page 1, region 2: \"box\" needs four numbers"
    finally
        File.Delete path

[<Fact>]
let ``--apply-layout tags the file from the layout, as changed`` () =
    withUntaggedPdf (fun pdf ->
        let layout = LayoutFile.ofSuggestions pdf [ suggestion ] (Some "The garden")
        let changed =
            { layout with
                Pages =
                    [| { layout.Pages[0] with
                           Regions =
                             [| { layout.Pages[0].Regions[0] with Tag = "H1" }
                                { layout.Pages[0].Regions[1] with Alt = "Tomato seedlings" } |] } |] }
        let path = pdf + ".layout.json"
        File.WriteAllText(path, LayoutFile.toJson changed)

        let _, result = run [ pdf; "--apply-layout"; path ]

        let copy = result.Value.GetProperty("fixOutputPath").GetString() |> nonNull
        let tagged = VeraPdfValidator().ReadStructureTree copy |> nonNull |> nodes
        tagged |> List.map _.Tag |> List.filter (fun t -> t = "H1" || t = "Figure") |> should equal [ "H1"; "Figure" ]
        (tagged |> List.find (fun n -> n.Tag = "Figure")).AlternateDescription |> should equal "Tomato seedlings"
        use document = new PdfDocument(new PdfReader(copy))
        document.GetDocumentInfo().GetTitle() |> should equal "The garden")

[<Fact>]
let ``--apply-layout on a tagged file says the layout wasn't applied`` () =
    withUntaggedPdf (fun pdf ->
        let path = pdf + ".layout.json"
        File.WriteAllText(path, LayoutFile.toJson (LayoutFile.ofSuggestions pdf [ suggestion ] None))
        let _, first = run [ pdf; "--apply-layout"; path ]
        let copy = first.Value.GetProperty("fixOutputPath").GetString() |> nonNull

        let _, again = run [ copy; "--apply-layout"; path ]

        let notApplied = again.Value.GetProperty("fixNotApplied")
        notApplied[0].GetString() |> should haveSubstring "the file already has a structure tree")

[<Fact>]
let ``a layout that can't be used, or layout options used wrongly, are usage errors`` () =
    withUntaggedPdf (fun pdf ->
        let path = pdf + ".layout.json"
        File.WriteAllText(path, "{ not json")
        fst (run [ pdf; "--apply-layout"; path ]) |> should equal 64
        fst (run [ pdf; "--suggest-layout-output"; path ]) |> should equal 64
        fst (run [ pdf; pdf; "--apply-layout"; path ]) |> should equal 64)

[<Fact>]
let ``--suggest-layout returns each page's regions, ready to apply`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    withUntaggedPdf (fun pdf ->
        let path = pdf + ".layout.json"

        let _, result = run [ pdf; "--suggest-layout"; "--suggest-layout-output"; path ]

        let pages = result.Value.GetProperty("suggestedLayout").GetProperty("pages")
        pages.GetArrayLength() |> should equal 1
        pages[0].GetProperty("regions").GetArrayLength() |> should be (greaterThan 0)
        LayoutFile.read path |> Result.isOk |> should be True)
