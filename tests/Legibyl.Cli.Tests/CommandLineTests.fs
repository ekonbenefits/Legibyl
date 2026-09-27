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
          Layout = null
          Fixes = null
          Validation = null
          Checklist = null }
    File.WriteAllText(path, ReviewFile.toJson review)

[<Fact>]
let ``--edits writes the review next to the PDF into the repaired copy, and carries it`` () =
    withTaggedPdf (fun pdf ->
        writeReview (ReviewFile.filePath pdf) "Tomato seedlings"
        let copy = Path.ChangeExtension(pdf, ".v2.pdf") |> nonNull

        let _, result = run [ "fix"; pdf; "--edits"; "--fixes"; "none" ]

        strings result.Value "applied" |> should contain "Applied 1 of your edits (alt text, replacement text, tags, merges and decoration not read)."
        figureAlt copy |> should equal (Some "Tomato seedlings")
        // Only the edits: no other repair was asked for.
        strings result.Value "applied" |> should haveLength 1
        EmbeddedReview.read copy |> Option.isSome |> should be True)

[<Fact>]
let ``--edits with a figure retagged Artifact makes it decoration`` () =
    withTaggedPdf (fun pdf ->
        let review: ReviewFile.SavedFile =
            { Version = 1
              DismissedWarnings = null
              Edits = [| { Page = 1; Tag = "Figure"; Box = [| 72.0; 550.0; 172.0; 650.0 |]; Alt = null; Text = null; MergeWith = null; Retag = "Artifact"; Exclude = false } |]
              Title = null
              Layout = null
              Fixes = null
              Validation = null
              Checklist = null }
        File.WriteAllText(ReviewFile.filePath pdf, ReviewFile.toJson review)
        let copy = Path.ChangeExtension(pdf, ".v2.pdf") |> nonNull

        let _, result = run [ "fix"; pdf; "--edits"; "--fixes"; "none" ]

        strings result.Value "applied" |> should contain "Applied 1 of your edits (alt text, replacement text, tags, merges and decoration not read)."
        let rec tags (node: StructureNode) : string list = node.Tag :: [ for child in node.Children do yield! tags child ]
        VeraPdfValidator().ReadStructureTree copy |> nonNull |> tags |> should not' (contain "Figure"))

[<Fact>]
let ``--edits gives a tagged PDF the review's title`` () =
    withTaggedPdf (fun pdf ->
        let review: ReviewFile.SavedFile =
            { Version = 1
              DismissedWarnings = null
              Edits = null
              Title = "The Community Garden"
              Layout = null
              Fixes = null
              Validation = null
              Checklist = null }
        File.WriteAllText(ReviewFile.filePath pdf, ReviewFile.toJson review)
        let copy = Path.ChangeExtension(pdf, ".v2.pdf") |> nonNull

        let _, result = run [ "fix"; pdf; "--edits"; "--fixes"; "none" ]

        strings result.Value "applied"
        |> should contain "Titled the document \"The Community Garden\", as you gave it, shown in place of the file name."
        use document = new PdfDocument(new PdfReader(copy))
        document.GetDocumentInfo().GetTitle() |> should equal "The Community Garden")

[<Fact>]
let ``--edits-from reads another review, such as an earlier version's`` () =
    withTaggedPdf (fun pdf ->
        let other = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "earlier.legibyl.json")
        writeReview other "Seedlings in a tray"
        let copy = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "out.pdf")

        run [ "fix"; pdf; "--edits-from"; other; "--fixes"; "none"; "--output"; copy ] |> ignore

        figureAlt copy |> should equal (Some "Seedlings in a tray"))

[<Fact>]
let ``--edits without a review says there's nothing to apply`` () =
    withTaggedPdf (fun pdf ->
        let _, result = run [ "fix"; pdf; "--edits"; "--fixes"; "none" ]

        result.Value.GetProperty("status").GetString() |> should equal "nothing-to-do"
        strings result.Value "notApplied" |> List.head |> should haveSubstring "No edits were applied: there's no garden.pdf.legibyl.json next to it")

[<Fact>]
let ``--fixes runs the repairs named`` () =
    withTaggedPdf (fun pdf ->
        let _, result = run [ "fix"; pdf; "--fixes"; "language" ]

        strings result.Value "applied" |> should equal [ "Declared the document's language as en (English), detected from its text." ])

[<Fact>]
let ``an unknown repair is a usage error`` () =
    withTaggedPdf (fun pdf ->
        let code, _ = run [ "fix"; pdf; "--fixes"; "safe,nope" ]
        code |> should equal 64)

[<Fact>]
let ``structure nodes say where they are as edits do: page and box`` () =
    withTaggedPdf (fun pdf ->
        let _, result = run [ "structure"; pdf ]

        let rec withBox (node: JsonElement) : JsonElement list =
            [ if node.GetProperty("box").ValueKind = JsonValueKind.Array then node
              for child in node.GetProperty("children").EnumerateArray() do
                  yield! withBox child ]
        let figure = withBox (result.Value.GetProperty "structure") |> List.find (fun n -> n.GetProperty("tag").GetString() = "Figure")
        figure.GetProperty("page").GetInt32() |> should equal 1
        [ for v in figure.GetProperty("box").EnumerateArray() -> v.GetDouble() ] |> should haveLength 4)

[<Fact>]
let ``validate reports compliance, with nothing about repairs`` () =
    withTaggedPdf (fun pdf ->
        let code, result = run [ "validate"; pdf ]

        code |> should equal 1
        result.Value.GetProperty("isCompliant").GetBoolean() |> should be False
        [ for property in result.Value.EnumerateObject() -> property.Name ] |> should not' (contain "status"))

[<Fact>]
let ``fix validates the copy it writes, and its exit code is about that copy`` () =
    withTaggedPdf (fun pdf ->
        let code, result = run [ "fix"; pdf; "--fixes"; "language" ]

        result.Value.GetProperty("status").GetString() |> should equal "repaired"
        let before = result.Value.GetProperty "before"
        let after = result.Value.GetProperty "after"
        after.GetProperty("failedCount").GetInt32() |> should be (lessThan (before.GetProperty("failedCount").GetInt32()))
        code |> should equal (if after.GetProperty("isCompliant").GetBoolean() then 0 else 1))

[<Fact>]
let ``structure reads a tagged file, and says an untagged one has none`` () =
    withTaggedPdf (fun pdf ->
        let untagged = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "untagged.pdf")

        let tagged, _ = run [ "structure"; pdf ]
        let none, result = run [ "structure"; untagged ]

        tagged |> should equal 0
        none |> should equal 1
        result.Value.GetProperty("structure").ValueKind |> should equal JsonValueKind.Null)

[<Fact>]
let ``the lists come as JSON too`` () =
    let original = Console.Out
    use writer = new StringWriter()
    Console.SetOut writer
    let code =
        try
            Legibyl.Cli.Program.main [| "list"; "fixes"; "--format"; "json" |]
        finally
            Console.SetOut original
    let fixes = JsonDocument.Parse(writer.ToString()).RootElement

    code |> should equal 0
    [ for fix in fixes.EnumerateArray() -> fix.GetProperty("name").GetString() ] |> should contain "lists"
    (fixes.EnumerateArray() |> Seq.find (fun f -> f.GetProperty("name").GetString() = "metadata")).GetProperty("safe").GetBoolean()
    |> should be True

[<Fact>]
let ``validate explains common failures, and names their fixes as --fixes does`` () =
    withTaggedPdf (fun pdf ->
        let _, result = run [ "validate"; pdf ]

        let failures = [ for f in result.Value.GetProperty("failures").EnumerateArray() -> f ]
        let language = failures |> List.find (fun f -> f.GetProperty("clause").GetString() = "7.2" && f.GetProperty("testNumber").GetInt32() = 34)
        language.GetProperty("meaning").GetString() |> should haveSubstring "language of some text isn't declared"
        [ for name in language.GetProperty("fixes").EnumerateArray() -> string (name.GetString()) ] |> should equal [ "language" ]
        // Where it is, readably: the paragraph the text is in.
        language.GetProperty("location").GetString() |> should startWith "Document → Page 1 → paragraph (P) \"The community garden")

/// validate's results for all files, with its exit code.
let private validateAll (args: string list) =
    let output = Path.GetTempFileName()
    try
        let code = Legibyl.Cli.Program.main (Array.ofList ("validate" :: args @ [ "--output"; output ]))
        let results = [ for r in JsonDocument.Parse(File.ReadAllText output).RootElement.EnumerateArray() -> r.Clone() ]
        code, results
    finally
        File.Delete output

[<Fact>]
let ``each file is checked against the PDF/UA its PDF version calls for, unless --ua says which`` () =
    withTaggedPdf (fun pdf17 ->
        let pdf20 = Path.Combine(Path.GetDirectoryName pdf17 |> nonNull, "pdf20.pdf")
        do
            use document = new PdfDocument(new PdfWriter(pdf20, WriterProperties().SetPdfVersion(PdfVersion.PDF_2_0)))
            PdfCanvas(document.AddNewPage()).BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f)
                .MoveText(72.0, 720.0).ShowText("A PDF 2.0 page.").EndText()
            |> ignore

        let _, detected = validateAll [ pdf17; pdf20 ]
        let code, asked = validateAll [ pdf17; "--ua"; "2" ]

        let summary (r: JsonElement) =
            r.GetProperty("standard").GetString(), r.GetProperty("flavourId").GetString(), r.GetProperty("pdfVersion").GetString(), r.GetProperty("standardFrom").GetString()
        detected |> List.map summary |> should equal [ ("PDF/UA-1", "ua1", "1.7", "file"); ("PDF/UA-2", "ua2", "2.0", "file") ]
        // Measured against PDF/UA-2 before upgrading it: it fails.
        asked |> List.map summary |> should equal [ ("PDF/UA-2", "ua2", "1.7", "--ua") ]
        asked.Head.GetProperty("isCompliant").GetBoolean() |> should be False
        code |> should equal 1)

[<Fact>]
let ``--profile and list profiles are gone, and --ua takes 1 or 2`` () =
    withTaggedPdf (fun pdf ->
        Legibyl.Cli.Program.main [| "validate"; pdf; "--profile"; "pdfua1" |] |> should equal 64
        Legibyl.Cli.Program.main [| "list"; "profiles" |] |> should equal 64
        Legibyl.Cli.Program.main [| "validate"; pdf; "--ua"; "3" |] |> should equal 64)
