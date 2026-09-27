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
        // Where each command writes its report: fix and layout apply take --report, validate and
        // structure --output; layout suggest writes the layout to its own --output.
        let report =
            match args with
            | "fix" :: _
            | "layout" :: "apply" :: _ -> [ "--report"; output ]
            | "layout" :: _ -> []
            | _ -> [ "--output"; output ]
        let code = Legibyl.Cli.Program.main (Array.ofList (args @ report))
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
        File.WriteAllText(path, LayoutFile.toJson (LayoutSuggestions.toLayoutFile "garden.pdf" [ suggestion ] (Some "The garden")))
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
        let layout = LayoutSuggestions.toLayoutFile "garden.pdf" [ suggestion ] None
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

/// A review with edits on regions (by the page, tag and box the layout found them with).
let private edits (title: string | null) (edits: ReviewFile.SavedEdit list) : ReviewFile.SavedFile =
    { Version = 1; DismissedWarnings = null; Edits = Array.ofList edits; Title = title; Layout = null; Fixes = null; Validation = null; Checklist = null }

let private editOn tag box : ReviewFile.SavedEdit =
    { Page = 1; Tag = tag; Box = box; Alt = null; Text = null; MergeWith = null; Retag = null; Exclude = false; MoveAfter = null; MoveAfterPage = 0; MoveFirst = false }

let private heading = [| 70.0; 695.0; 300.0; 715.0 |]
let private figure = [| 70.0; 548.0; 175.0; 652.0 |]

[<Fact>]
let ``layout apply tags the file from the layout as found, with the edits layered on`` () =
    withUntaggedPdf (fun pdf ->
        let path = pdf + ".layout.json"
        File.WriteAllText(path, LayoutFile.toJson (LayoutSuggestions.toLayoutFile pdf [ suggestion ] None))
        let review = pdf + ".edits.json"
        File.WriteAllText(
            review,
            ReviewFile.toJson (edits "The garden" [ { editOn "H" heading with Retag = "H1" }; { editOn "Figure" figure with Alt = "Tomato seedlings" } ])
        )

        let _, result = run [ "layout"; "apply"; pdf; "--layout"; path; "--edits-from"; review ]

        let copy = result.Value.GetProperty("outputPath").GetString() |> nonNull
        let tagged = VeraPdfValidator().ReadStructureTree copy |> nonNull |> nodes
        tagged |> List.map _.Tag |> List.filter (fun t -> t = "H1" || t = "Figure") |> should equal [ "H1"; "Figure" ]
        (tagged |> List.find (fun n -> n.Tag = "Figure")).AlternateDescription |> should equal "Tomato seedlings"
        use document = new PdfDocument(new PdfReader(copy))
        document.GetDocumentInfo().GetTitle() |> should equal "The garden")

[<Fact>]
let ``layout apply keeps a heading level an edit gives, and names edits that match no region`` () =
    withUntaggedPdf (fun pdf ->
        let path = pdf + ".layout.json"
        File.WriteAllText(path, LayoutFile.toJson (LayoutSuggestions.toLayoutFile pdf [ suggestion ] None))
        let review = pdf + ".edits.json"
        let nowhere = [| 300.0; 100.0; 400.0; 200.0 |]
        File.WriteAllText(
            review,
            ReviewFile.toJson (edits null [ { editOn "H" heading with Retag = "H2" }; { editOn "Figure" nowhere with Alt = "Not here" } ])
        )

        let _, result = run [ "layout"; "apply"; pdf; "--layout"; path; "--edits-from"; review ]

        let copy = result.Value.GetProperty("outputPath").GetString() |> nonNull
        VeraPdfValidator().ReadStructureTree copy |> nonNull |> nodes |> List.map _.Tag |> should contain "H2"
        [ for line in result.Value.GetProperty("notApplied").EnumerateArray() -> string (line.GetString()) ]
        |> should
            contain
            "Your edit for the <Figure> on page 1 at [300, 100, 400, 200] wasn't applied: no region of the layout with that tag is in about that place (its box overlapping at least 60%). An edit names a region by the tag it was suggested with, not one it's given.")

[<Fact>]
let ``edits with a field an edit doesn't have, or a tag that isn't one, are usage errors`` () =
    withUntaggedPdf (fun pdf ->
        let review = pdf + ".edits.json"
        let check (edit: string) =
            File.WriteAllText(review, $"""{{ "version": 1, "edits": [ {edit} ] }}""")
            fst (run [ "fix"; pdf; "--edits-from"; review ])
        check """{ "page": 1, "tag": "Link", "box": [ 70, 695, 300, 715 ], "description": "Apply" }""" |> should equal 64
        check """{ "page": 1, "tag": "H", "box": [ 70, 695, 300, 715 ], "retag": "Heading2" }""" |> should equal 64
        // --edits checks the review next to the PDF the same way.
        File.WriteAllText(pdf + ".legibyl.json", """{ "version": 1, "edits": [ { "page": 1, "tag": "P", "box": [ 1, 2, 3, 4 ], "contents": "x" } ] }""")
        fst (run [ "fix"; pdf; "--edits" ]) |> should equal 64
        ReviewFile.problems (File.ReadAllText(pdf + ".legibyl.json"))
        |> should equal [ "edits[0] has \"contents\", which isn't a field of an edit (a link's description is its \"alt\"). An edit has page, tag and box, and any of alt, text, retag, exclude, mergeWith, and moveAfter (with moveAfterPage) or moveFirst." ])

[<Fact>]
let ``layout apply on a tagged file says it's already tagged`` () =
    withUntaggedPdf (fun pdf ->
        let path = pdf + ".layout.json"
        File.WriteAllText(path, LayoutFile.toJson (LayoutSuggestions.toLayoutFile pdf [ suggestion ] None))
        let _, first = run [ "layout"; "apply"; pdf; "--layout"; path ]
        let copy = first.Value.GetProperty("outputPath").GetString() |> nonNull

        let code, again = run [ "layout"; "apply"; copy; "--layout"; path ]

        again.Value.GetProperty("status").GetString() |> should equal "already-tagged"
        code |> should equal 1)

[<Fact>]
let ``a layout or review that can't be used, or commands used wrongly, are usage errors`` () =
    withUntaggedPdf (fun pdf ->
        let path = pdf + ".layout.json"
        File.WriteAllText(path, "{ not json")
        fst (run [ "layout"; "apply"; pdf; "--layout"; path ]) |> should equal 64
        fst (run [ "layout"; "apply"; pdf ]) |> should equal 64
        fst (run [ "layout"; "apply"; pdf; pdf; "--layout"; path ]) |> should equal 64
        fst (run [ "fix"; pdf; "--fixes" ]) |> should equal 64
        fst (run [ "fix"; pdf; "--edits-from"; pdf + ".missing.json" ]) |> should equal 64
        fst (run [ "fix"; pdf; pdf; "--output"; path ]) |> should equal 64
        fst (run [ "fix"; pdf; "--edits"; "--edits-from"; path ]) |> should equal 64
        fst (run []) |> should equal 64)

[<Fact>]
let ``layout suggest writes each page's regions, ready to apply`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    withUntaggedPdf (fun pdf ->
        let path = pdf + ".layout.json"

        let code, _ = run [ "layout"; "suggest"; pdf; "--output"; path ]

        code |> should equal 0
        let layout = match LayoutFile.read path with Ok layout -> layout | Error message -> failwith message
        layout.Pages |> should haveLength 1
        layout.Pages[0].Regions.Length |> should be (greaterThan 0))

[<Fact>]
let ``--figure-images writes a picture of each figure the model finds`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    withUntaggedPdf (fun pdf ->
        let path = pdf + ".layout.json"

        run [ "layout"; "suggest"; pdf; "--output"; path; "--figure-images" ] |> ignore

        let layout = match LayoutFile.read path with Ok layout -> layout | Error message -> failwith message
        let figures = layout.Pages |> Array.collect _.Regions |> Array.filter (fun r -> r.Tag = "Figure")
        figures |> should not' (be Empty)
        for figure in figures do
            let image = Path.Combine(Path.GetDirectoryName path |> nonNull, figure.Image |> nonNull)
            File.Exists image |> should be True)

[<Fact>]
let ``figure images go in a folder beside the layout, replacing the last run's`` () =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try
        let layoutPath = Path.Combine(folder, "garden.layout.json")
        let images = LayoutFile.imagesFolder layoutPath
        Directory.CreateDirectory images |> ignore
        File.WriteAllText(Path.Combine(images, "p9-r9.png"), "stale")
        File.WriteAllText(Path.Combine(images, "notes.txt"), "someone's")
        let rendered = ResizeArray<StructureGeometry>()

        let layout =
            LayoutSuggestions.toLayoutFile "garden.pdf" [ suggestion ] None
            |> LayoutFile.withFigureImages layoutPath (fun geometry -> rendered.Add geometry; [| 1uy |])

        images |> should equal (Path.Combine(folder, "garden.layout.images"))
        [ for region in layout.Pages[0].Regions -> region.Tag, Option.ofObj region.Image ]
        |> should equal [ "H", None; "Figure", Some "garden.layout.images/p1-r2.png" ]
        [ for g in rendered -> g.Left, g.Top ] |> should equal [ 70.0, 652.0 ]
        Directory.GetFiles images |> Array.map Path.GetFileName |> Array.sort |> should equal [| "notes.txt"; "p1-r2.png" |]
    finally
        Directory.Delete(folder, true)

[<Fact>]
let ``a figure is given the text just before and after it`` () =
    let blocks =
        { suggestion with
            Blocks =
                [ block 1 "H" (70.0, 695.0, 300.0, 715.0) "The garden"
                  block 2 "Artifact" (70.0, 680.0, 300.0, 690.0) "Page 1"
                  { block 3 "Figure" (70.0, 548.0, 175.0, 652.0) "" with Label = "image" }
                  block 4 "Caption" (70.0, 530.0, 175.0, 545.0) "Seedlings in a tray" ] }

    let figure = (LayoutSuggestions.toLayoutFile "garden.pdf" [ blocks ] None).Pages[0].Regions[2]

    (figure.TextBefore, figure.TextAfter) |> should equal ("The garden", "Seedlings in a tray")

[<Fact>]
let ``figure images need a layout file to go beside`` () =
    withUntaggedPdf (fun pdf -> fst (run [ "layout"; "suggest"; pdf; "--figure-images" ]) |> should equal 64)

let private fixedSource (regions: LayoutRegion list) =
    { new IPageLayoutSource with
        member _.PageRegions(_, _) = regions
        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = "From the model" }

[<Fact>]
let ``pages a layout doesn't have come from the fallback`` () =
    let layout = LayoutSuggestions.toLayoutFile "garden.pdf" [ suggestion ] None
    let fromModel: LayoutRegion =
        { Tag = "P"; Geometry = { PageNumber = 2; Left = 1.0; Bottom = 1.0; Right = 2.0; Top = 2.0 }; ListNumbering = null; ActualText = null; QrText = null; Alt = null }
    let source = LayoutFile.FileRegions(layout, Some(fixedSource [ fromModel ])) :> IPageLayoutSource

    source.PageRegions("garden.pdf", 1) |> Seq.map _.Tag |> List.ofSeq |> should equal [ "H"; "Figure" ]
    source.PageRegions("garden.pdf", 2) |> Seq.map _.Tag |> List.ofSeq |> should equal [ "P" ]
    // No title of its own: the fallback's.
    source.DocumentTitle |> should equal "From the model"
    LayoutFile.covers layout 1 |> should be True
    LayoutFile.covers layout 2 |> should be False

[<Fact>]
let ``a review records an untagged PDF's layout as found, and --edits tags from it with its edits`` () =
    withUntaggedPdf (fun pdf ->
        let layout = LayoutSuggestions.toLayoutFile pdf [ suggestion ] None
        ReviewFile.writeFile
            pdf
            { edits "The garden" [ { editOn "H" heading with Retag = "H1" }; { editOn "Figure" figure with Exclude = true } ] with
                Layout = layout }

        let _, result = run [ "fix"; pdf; "--edits"; "--fixes"; "none" ]

        let copy = result.Value.GetProperty("outputPath").GetString() |> nonNull
        let tagged = VeraPdfValidator().ReadStructureTree copy |> nonNull |> nodes
        tagged |> List.map _.Tag |> List.filter (fun t -> t = "H1" || t = "H2" || t = "Figure") |> should equal [ "H1" ]
        use document = new PdfDocument(new PdfReader(copy))
        document.GetDocumentInfo().GetTitle() |> should equal "The garden"
        // The copy carries the review, but not the layout it was tagged from (it has elements
        // now), and the edits are in its elements: a retag is just its tag, an exclusion is gone.
        let carried = ReviewFile.readFor copy |> Option.get
        carried.Layout |> should be Null
        carried.Edits |> should be Null)
