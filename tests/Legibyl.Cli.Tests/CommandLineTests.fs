module Legibyl.Cli.Tests.CommandLineTests

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
open Legibyl.Validation

let private rect (l, b, r, t) : StructureGeometry = { PageNumber = 1; Left = l; Bottom = b; Right = r; Top = t }

type private FixedLayout(regions: LayoutRegion list) =
    interface IPageLayoutSource with
        member _.PageRegions(_, _) = regions
        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = null

/// A tagged page (a paragraph long enough for its language to be told, and a figure without alt
/// text) in a fresh folder.
let private withTaggedPdf (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try
        let untagged = Path.Combine(folder, "untagged.pdf")
        do
            use document = new PdfDocument(new PdfWriter(untagged))
            let canvas = PdfCanvas(document.AddNewPage())
            let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
            for y, text in
                [ 706.0, "The community garden is open to every resident of the neighbourhood, whether"
                  698.0, "you have grown vegetables for years or have never planted a seed before." ] do
                canvas.BeginText().SetFontAndSize(font, 6f).MoveText(72.0, y).ShowText(text).EndText() |> ignore
            let pixels = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 255uy; 255uy; 0uy |]
            canvas.AddImageWithTransformationMatrix(ImageDataFactory.Create(2, 2, 3, 8, pixels, null), 100f, 0f, 0f, 100f, 72f, 550f, true) |> ignore
        let region tag bounds : LayoutRegion =
            { Tag = tag; Geometry = rect bounds; ListNumbering = null; ActualText = null; QrText = null; Alt = null }
        let pdf = Path.Combine(folder, "garden.pdf")
        VeraPdfValidator(LayoutSource = FixedLayout [ region "P" (70.0, 695.0, 300.0, 715.0); region "Figure" (70.0, 548.0, 175.0, 652.0) ])
            .Fix(untagged, PdfProfile.PdfUa1, pdf, PdfFixOptions.TagFromLayout)
        |> ignore
        test pdf
    finally
        Directory.Delete(folder, true)

/// Runs the CLI with args, returning its exit code and its JSON output's first file.
let private run (args: string list) =
    let output = Path.GetTempFileName()
    try
        let code = Legibyl.Cli.Program.main (Array.ofList (args @ [ "--output"; output ]))
        let json = if File.Exists output && FileInfo(output).Length > 0L then Some(JsonDocument.Parse(File.ReadAllText output)) else None
        code, json |> Option.map (fun json -> json.RootElement[0])
    finally
        File.Delete output

let private strings (element: JsonElement) (name: string) =
    match element.GetProperty name with
    | e when e.ValueKind = JsonValueKind.Array -> [ for item in e.EnumerateArray() -> item.GetString() |> string ]
    | _ -> []

let private figureAlt (pdf: string) =
    let rec find (node: StructureNode) : StructureNode option =
        if node.Tag = "Figure" then Some node else node.Children |> Seq.tryPick find
    VeraPdfValidator().ReadStructureTree pdf |> nonNull |> find |> Option.bind (fun n -> Option.ofObj n.AlternateDescription)

let private writeReview (path: string) (alt: string) =
    let review: ReviewFile.SavedFile =
        { Version = 1
          DismissedWarnings = null
          Edits = [| { Page = 1; Tag = "Figure"; Box = [| 72.0; 550.0; 172.0; 650.0 |]; Alt = alt; Text = null; MergeWith = null; Retag = null; Exclude = false } |]
          Title = null
          Layout = null }
    File.WriteAllText(path, ReviewFile.toJson review)

[<Fact>]
let ``--edits writes the review next to the PDF into the repaired copy, and carries it`` () =
    withTaggedPdf (fun pdf ->
        writeReview (ReviewFile.filePath pdf) "Tomato seedlings"
        let copy = Path.ChangeExtension(pdf, ".v2.pdf") |> nonNull

        let _, result = run [ pdf; "--edits" ]

        strings result.Value "fixAppliedFixes" |> should contain "Applied 1 of your edits (alt text, replacement text, tags and merged paragraphs)."
        figureAlt copy |> should equal (Some "Tomato seedlings")
        // Only the edits: no other repair was asked for.
        strings result.Value "fixAppliedFixes" |> should haveLength 1
        EmbeddedReview.read copy |> Option.isSome |> should be True)

[<Fact>]
let ``--edits-from reads another review, such as an earlier version's`` () =
    withTaggedPdf (fun pdf ->
        let other = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "earlier.legibyl.json")
        writeReview other "Seedlings in a tray"
        let copy = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "out.pdf")

        run [ pdf; "--edits-from"; other; "--fix-output"; copy ] |> ignore

        figureAlt copy |> should equal (Some "Seedlings in a tray"))

[<Fact>]
let ``--edits without a review says there's nothing to apply`` () =
    withTaggedPdf (fun pdf ->
        let _, result = run [ pdf; "--edits" ]

        result.Value.GetProperty("fixStatus").GetString() |> should equal "NoAction"
        strings result.Value "fixNotApplied" |> List.head |> should haveSubstring "No edits were applied: there's no garden.pdf.legibyl.json next to it")

[<Fact>]
let ``--fixes runs the repairs named`` () =
    withTaggedPdf (fun pdf ->
        let _, result = run [ pdf; "--fixes"; "language" ]

        strings result.Value "fixAppliedFixes" |> should equal [ "Declared the document's language as en (English), detected from its text." ])

[<Fact>]
let ``an unknown repair is a usage error`` () =
    withTaggedPdf (fun pdf ->
        let code, _ = run [ pdf; "--fixes"; "safe,nope" ]
        code |> should equal 64)

[<Fact>]
let ``structure nodes say where they are as edits do: page and box`` () =
    withTaggedPdf (fun pdf ->
        let _, result = run [ pdf; "--structure" ]

        let rec withBox (node: JsonElement) : JsonElement list =
            [ if node.GetProperty("box").ValueKind = JsonValueKind.Array then node
              for child in node.GetProperty("children").EnumerateArray() do
                  yield! withBox child ]
        let figure = withBox (result.Value.GetProperty "structure") |> List.find (fun n -> n.GetProperty("tag").GetString() = "Figure")
        figure.GetProperty("page").GetInt32() |> should equal 1
        [ for v in figure.GetProperty("box").EnumerateArray() -> v.GetDouble() ] |> should haveLength 4)
