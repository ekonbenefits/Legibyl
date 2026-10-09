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
          Edits = [| { Page = 1; Tag = "Figure"; Box = [| 72.0; 550.0; 172.0; 650.0 |]; Alt = alt; Text = null; MergeWith = null; Retag = null; Exclude = false; MoveAfter = null; MoveAfterPage = 0; MoveFirst = false ; Cells = null} |]
          Title = null
          Language = null
          RoleMap = null; Attachments = null
          Layout = null
          Fixes = null
          Validation = null
          Checklist = null }
    File.WriteAllText(path, ReviewFile.toJson review)

[<Fact>]
let ``--edits writes the review next to the PDF into the fixed copy, and carries it`` () =
    withTaggedPdf (fun pdf ->
        writeReview (ReviewFile.filePath pdf) "Tomato seedlings"
        let copy = Path.ChangeExtension(pdf, ".v2.pdf") |> nonNull

        let _, result = run [ "fix"; pdf; "--edits"; "--fixes"; "none" ]

        strings result.Value "applied" |> should contain "Applied 1 of your edits (alt text, replacement text, tags, merges and decoration not read)."
        figureAlt copy |> should equal (Some "Tomato seedlings")
        // Only the edits: no other fix was asked for.
        strings result.Value "applied" |> should haveLength 1
        EmbeddedReview.read copy |> Option.isSome |> should be True)

[<Fact>]
let ``--edits with a figure retagged Artifact makes it decoration`` () =
    withTaggedPdf (fun pdf ->
        let review: ReviewFile.SavedFile =
            { Version = 1
              DismissedWarnings = null
              Edits = [| { Page = 1; Tag = "Figure"; Box = [| 72.0; 550.0; 172.0; 650.0 |]; Alt = null; Text = null; MergeWith = null; Retag = "Artifact"; Exclude = false; MoveAfter = null; MoveAfterPage = 0; MoveFirst = false ; Cells = null} |]
              Title = null
              Language = null
              RoleMap = null; Attachments = null
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
              Language = null
              RoleMap = null; Attachments = null
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
let ``--edits writes a review that only gives an attached file a description`` () =
    withTaggedPdf (fun pdf ->
        // The tagged PDF with notes.txt attached, without a description.
        let attached = Path.ChangeExtension(pdf, ".attached.pdf") |> nonNull
        do
            use document = new PdfDocument(new PdfReader(pdf), new PdfWriter(attached))
            let spec = iText.Kernel.Pdf.Filespec.PdfFileSpec.CreateEmbeddedFileSpec(document, Text.Encoding.UTF8.GetBytes "Bring gloves.", null, "notes.txt", null, null)
            document.AddFileAttachment("notes.txt", spec)
        File.Copy(attached, pdf, true)
        let edits = Collections.Generic.Dictionary<string, Attachments.AttachmentEdit>()
        edits["the document: notes.txt"] <- { FileName = null; Description = "What to bring"; ContentType = null }
        let review: ReviewFile.SavedFile =
            { Version = 1; DismissedWarnings = null; Edits = null; Title = null; Language = null; RoleMap = null; Attachments = edits
              Layout = null; Fixes = null; Validation = null; Checklist = null }
        File.WriteAllText(ReviewFile.filePath pdf, ReviewFile.toJson review)
        let copy = Path.ChangeExtension(pdf, ".v2.pdf") |> nonNull

        let _, result = run [ "fix"; pdf; "--edits"; "--fixes"; "none" ]

        strings result.Value "applied" |> should contain "Wrote the attached file \"notes.txt\"'s Desc, as given."
        Attachments.list copy |> List.map _.Description |> should equal [ Some "What to bring" ])

[<Fact>]
let ``--edits-from reads another review, such as an earlier version's`` () =
    withTaggedPdf (fun pdf ->
        let other = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "earlier.legibyl.json")
        writeReview other "Seedlings in a tray"
        let copy = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "out.pdf")

        run [ "fix"; pdf; "--edits-from"; other; "--fixes"; "none"; "--output"; copy ] |> ignore

        figureAlt copy |> should equal (Some "Seedlings in a tray"))

[<Fact>]
let ``--lang declares the language given, with no other fixes, and a tag that isn't one is a usage error`` () =
    withTaggedPdf (fun pdf ->
        let copy = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "out.pdf")

        let _, result = run [ "fix"; pdf; "--lang"; "es-MX"; "--fixes"; "none"; "--output"; copy ]

        strings result.Value "applied" |> should contain "Declared the document's language as es-MX (Spanish), as you chose."
        do
            use document = new PdfDocument(new PdfReader(copy))
            document.GetCatalog().GetPdfObject().GetAsString(PdfName.Lang).ToUnicodeString() |> should equal "es-MX"
        Legibyl.Cli.Program.main [| "fix"; pdf; "--lang"; "Spanish please" |] |> should equal 64)

[<Fact>]
let ``--edits without a review says there's nothing to apply`` () =
    withTaggedPdf (fun pdf ->
        let _, result = run [ "fix"; pdf; "--edits"; "--fixes"; "none" ]

        result.Value.GetProperty("status").GetString() |> should equal "nothing-to-do"
        strings result.Value "notApplied" |> List.head |> should haveSubstring "No edits were applied: there's no garden.pdf.legibyl.json next to it")

[<Fact>]
let ``--fixes runs the fixes named`` () =
    withTaggedPdf (fun pdf ->
        let _, result = run [ "fix"; pdf; "--fixes"; "language" ]

        strings result.Value "applied" |> should equal [ "Declared the document's language as en (English), detected from its text." ])

[<Fact>]
let ``an unknown fix is a usage error`` () =
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
let ``structure nodes say what kind they are: the root, and the PDF's own elements`` () =
    withTaggedPdf (fun pdf ->
        let _, result = run [ "structure"; pdf ]
        let root = result.Value.GetProperty "structure"
        root.GetProperty("kind").GetString() |> should equal "Root"
        [ for child in root.GetProperty("children").EnumerateArray() -> child.GetProperty("kind").GetString() ]
        |> should contain "Element")

[<Fact>]
let ``validate reports compliance, with nothing about fixes`` () =
    withTaggedPdf (fun pdf ->
        let code, result = run [ "validate"; pdf ]

        code |> should equal 1
        result.Value.GetProperty("isCompliant").GetBoolean() |> should be False
        [ for property in result.Value.EnumerateObject() -> property.Name ] |> should not' (contain "status"))

[<Fact>]
let ``fix validates the copy it writes, and its exit code is about that copy`` () =
    withTaggedPdf (fun pdf ->
        let code, result = run [ "fix"; pdf; "--fixes"; "language" ]

        result.Value.GetProperty("status").GetString() |> should equal "fixed"
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
let ``list languages gives the built-in packs and where packs are read from`` () =
    let original = Console.Out
    use writer = new StringWriter()
    Console.SetOut writer
    try
        Legibyl.Cli.Program.main [| "list"; "languages"; "--format"; "json" |] |> ignore
    finally
        Console.SetOut original
    let listed = JsonDocument.Parse(writer.ToString()).RootElement

    let tags = [ for language in listed.GetProperty("languages").EnumerateArray() -> language.GetProperty("tag").GetString() ]
    tags |> should contain "en"
    tags |> should contain "es"
    [ for folder in listed.GetProperty("folders").EnumerateArray() -> folder.GetString() ]
    |> should contain (Path.Combine(AppContext.BaseDirectory, "languages"))

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

/// A page whose paragraphs hold marked content inside marked content, which veraPDF logs as it
/// reads it, in a fresh folder.
let private withNestedPdf (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try
        let pdf = Path.Combine(folder, "list.pdf")
        do
            use document = new PdfDocument(new PdfWriter(pdf))
            let canvas = PdfCanvas(document.AddNewPage())
            let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
            for i in 0..1 do
                for tag, mcid in [ "P", 2 * i; "Span", 2 * i + 1 ] do
                    let properties = PdfDictionary()
                    properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
                    canvas.BeginMarkedContent(PdfName tag, properties) |> ignore
                canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 700.0 - 20.0 * float i).ShowText($"Item {i + 1}").EndText() |> ignore
                canvas.EndMarkedContent().EndMarkedContent() |> ignore
        test pdf
    finally
        Directory.Delete(folder, true)

/// Runs the CLI as run does, returning what it wrote to stderr too.
let private runCapturingErrors (args: string list) =
    let original = Console.Error
    use writer = new StringWriter()
    Console.SetError writer
    try
        let code, result = run args
        code, result, writer.ToString()
    finally
        Console.SetError original

[<Fact>]
let ``validate keeps what veraPDF logs as validatorNotes, off stderr; --verbose prints it too`` () =
    withNestedPdf (fun pdf ->
        let _, result, errors = runCapturingErrors [ "validate"; pdf ]
        let _, _, verboseErrors = runCapturingErrors [ "validate"; pdf; "--verbose" ]

        let notes = [ for note in result.Value.GetProperty("validatorNotes").EnumerateArray() -> note ]
        let nested = notes |> List.find (fun n -> (string (n.GetProperty("message").GetString())).Contains "Nested MCID")
        nested.GetProperty("count").GetInt32() |> should equal 2
        nested.GetProperty("logger").GetString() |> should equal "OperatorParser"
        errors |> should not' (haveSubstring "Nested MCID")
        // Each as it comes, whole: the library raises it, the CLI prints it.
        verboseErrors |> should haveSubstring "veraPDF WARNING OperatorParser: Content stream"
        verboseErrors |> should haveSubstring "Nested MCID - 1")

[<Fact>]
let ``what veraPDF logs is raised as it comes, for a caller to show`` () =
    withNestedPdf (fun pdf ->
        let received = Collections.Concurrent.ConcurrentQueue<ValidatorNote>()
        use _ = VeraPdfValidator.Recorded.Subscribe received.Enqueue
        let report = VeraPdfValidator().Validate(pdf, PdfProfile.PdfUa1)

        let nested = received |> Seq.filter (fun n -> n.Message.Contains "Nested MCID") |> List.ofSeq
        // Each record, as logged (with the number the report's note leaves off).
        nested |> List.map _.Count |> should equal [ 1; 1 ]
        nested |> List.forall (fun n -> n.Logger = "OperatorParser" && n.Level = "WARNING") |> should be True
        (report.Notes |> Seq.find (fun n -> n.Message.Contains "Nested MCID")).Count |> should equal nested.Length)

[<Fact>]
let ``fix keeps what veraPDF logged reading the file as it was`` () =
    withNestedPdf (fun pdf ->
        let _, result, errors = runCapturingErrors [ "fix"; pdf; "--fixes"; "tabs" ]

        [ for note in result.Value.GetProperty("validatorNotes").EnumerateArray() -> string (note.GetProperty("message").GetString()) ]
        |> List.exists (fun m -> m.Contains "Nested MCID")
        |> should be True
        errors |> should not' (haveSubstring "Nested MCID"))

[<Fact>]
let ``--version prints the line the GUI's About window copies, then Legibyl's copyright`` () =
    let original = Console.Out
    use writer = new StringWriter()
    Console.SetOut writer
    let code =
        try
            Legibyl.Cli.Program.main [| "--version" |]
        finally
            Console.SetOut original

    code |> should equal 0
    writer.ToString().Trim().Split(Environment.NewLine) |> should equal [| BuildInfo.versionLine; "Copyright © 2026 Ekon Benefits <opensource@ekonbenefits.com>" |]
    BuildInfo.versionLine |> should startWith $"legibyl {BuildInfo.version} (vendored veraPDF engine {VeraPdfValidator.EngineVersion})"

[<Fact>]
let ``validate reports the steps a person checked, carried in a copy Legibyl wrote, and none for the original`` () =
    withTaggedPdf (fun pdf ->
        let copy = Path.ChangeExtension(pdf, ".v2.pdf") |> nonNull
        let review: ReviewFile.SavedFile =
            { Version = 1; DismissedWarnings = null; Edits = null; Title = null; Language = null; RoleMap = null; Attachments = null; Layout = null; Fixes = null; Validation = null; Checklist = null }
        ReviewFile.checkedCopy pdf copy review (SavedResults.hashOf pdf) (set [ "title"; "headings" ]) (DateTime(2026, 9, 27))

        let _, original = run [ "validate"; pdf ]
        let _, copied = run [ "validate"; copy ]

        original.Value.GetProperty("checkedByPerson").ValueKind |> should equal JsonValueKind.Null
        let checkedBy = copied.Value.GetProperty "checkedByPerson"
        [ for step in checkedBy.GetProperty("steps").EnumerateArray() -> string (step.GetString()) ] |> List.sort |> should equal [ "headings"; "title" ]
        checkedBy.GetProperty("checkedAt").GetDateTime() |> should equal (DateTime(2026, 9, 27)))

[<Fact>]
let ``a result with nothing in it has empty lists, never null, on every command`` () =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try
        // Not a PDF: it can't be validated or fixed, so there's nothing to list.
        let broken = Path.Combine(folder, "broken.pdf")
        File.WriteAllText(broken, "not a PDF")

        let _, validated = run [ "validate"; broken ]
        let _, fixedResult = run [ "fix"; broken ]

        let kind (element: JsonElement) (name: string) = element.GetProperty(name).ValueKind
        let isEmptyList (element: JsonElement) (name: string) =
            kind element name = JsonValueKind.Array && element.GetProperty(name).GetArrayLength() = 0
        validated.Value.GetProperty("error").ValueKind |> should equal JsonValueKind.String
        for name in [ "failures"; "warnings"; "validatorNotes" ] do
            (name, isEmptyList validated.Value name) |> should equal (name, true)
        // Null where null means something: no structure tree, no result, nothing checked.
        for name in [ "structure"; "isCompliant"; "checkedByPerson" ] do
            (name, kind validated.Value name) |> should equal (name, JsonValueKind.Null)
        fixedResult.Value.GetProperty("error").ValueKind |> should equal JsonValueKind.String
        for name in [ "applied"; "notApplied"; "validatorNotes" ] do
            (name, isEmptyList fixedResult.Value name) |> should equal (name, true)
        for name in [ "before"; "after" ] do
            (name, kind fixedResult.Value name) |> should equal (name, JsonValueKind.Null)
    finally
        Directory.Delete(folder, true)

[<Fact>]
let ``--fixes names each of the five, and says why when there was nothing for it to do`` () =
    withTaggedPdf (fun pdf ->
        for name in [ "link-contents"; "title-from-first-page"; "empty-toc-entries"; "qr-links"; "empty-figures" ] do
            FixNames.parse name |> Result.isOk |> should be True
        // Once fixed, the copy has nothing left for them.
        let _, first = run [ "fix"; pdf; "--fixes"; "empty-toc-entries,empty-figures" ]
        let copy = first.Value.GetProperty("outputPath").GetString() |> string
        let code, result = run [ "fix"; (if copy = "" then pdf else copy); "--fixes"; "empty-toc-entries,empty-figures" ]

        code |> should equal 1
        result.Value.GetProperty("status").GetString() |> should equal "nothing-to-do"
        strings result.Value "notApplied"
        |> should
            equal
            [ "Remove empty table-of-contents entries: nothing in this PDF needed it."
              "Remove figure tags that draw nothing: nothing in this PDF needed it." ])


/// A tagged page with a heading and no bookmarks, which Legibyl warns about (Structure-7), in a
/// fresh folder.
let private withWarning (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try
        let untagged = Path.Combine(folder, "untagged.pdf")
        do
            use document = new PdfDocument(new PdfWriter(untagged))
            let canvas = PdfCanvas(document.AddNewPage())
            let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
            canvas.BeginText().SetFontAndSize(font, 20f).MoveText(72.0, 720.0).ShowText("The Community Garden").EndText() |> ignore
            canvas.BeginText().SetFontAndSize(font, 10f).MoveText(72.0, 690.0).ShowText("Open to every resident of the neighbourhood.").EndText() |> ignore
        let region tag bounds : LayoutRegion =
            { Tag = tag; Geometry = rect bounds; ListNumbering = null; ActualText = null; QrText = null; Alt = null }
        let pdf = Path.Combine(folder, "garden.pdf")
        VeraPdfValidator(LayoutSource = FixedLayout [ region "H1" (70.0, 715.0, 400.0, 745.0); region "P" (70.0, 685.0, 400.0, 705.0) ])
            .Fix(untagged, PdfProfile.PdfUa1, pdf, PdfFixOptions.TagFromLayout)
        |> ignore
        test pdf
    finally
        Directory.Delete(folder, true)

/// A review dismissing every warning of rule (e.g. "Legibyl Structure-7").
let private dismissing (rule: string) : ReviewFile.SavedFile =
    { Version = 1
      DismissedWarnings = [| { Rule = rule; Location = ReviewFile.AnyLocation; Message = "" } |]
      Edits = null; Title = null; Language = null; RoleMap = null; Attachments = null; Layout = null; Fixes = null; Validation = null; Checklist = null }

let private warningRules (result: JsonElement) =
    [ for w in result.GetProperty("warnings").EnumerateArray() ->
          let clause = w.GetProperty("clause").GetString()
          let test = w.GetProperty("testNumber").GetInt32()
          $"{clause}-{test}" ]

[<Fact>]
let ``validate leaves out the warnings the review dismisses, says how many and where from; --all-warnings reports them`` () =
    withWarning (fun pdf ->
        let _, before = run [ "validate"; pdf ]
        File.WriteAllText(ReviewFile.filePath pdf, ReviewFile.toJson (dismissing "Legibyl Structure-7"))
        let _, after = run [ "validate"; pdf ]
        let _, all = run [ "validate"; pdf; "--all-warnings" ]

        warningRules before.Value |> should contain "Structure-7"
        before.Value.GetProperty("dismissedWarnings").GetInt32() |> should equal 0
        before.Value.GetProperty("review").ValueKind |> should equal JsonValueKind.Null
        warningRules after.Value |> should not' (contain "Structure-7")
        after.Value.GetProperty("dismissedWarnings").GetInt32() |> should equal 1
        after.Value.GetProperty("review").GetString() |> should equal (ReviewFile.filePath pdf)
        warningRules all.Value |> should contain "Structure-7"
        all.Value.GetProperty("dismissedWarnings").GetInt32() |> should equal 1)

[<Fact>]
let ``a review inside a copy is honoured, and the file next to it wins; --review uses another`` () =
    withWarning (fun pdf ->
        let folder = Path.GetDirectoryName pdf |> nonNull
        // A copy carrying a review that dismisses the warning.
        let copy = Path.Combine(folder, "garden.v2.pdf")
        EmbeddedReview.write pdf copy (ReviewFile.toJson (dismissing "Legibyl Structure-7"))
        let _, carried = run [ "validate"; copy ]
        // A file next to the copy that dismisses nothing wins over the one inside it.
        File.WriteAllText(ReviewFile.filePath copy, ReviewFile.toJson { dismissing "Legibyl Structure-7" with DismissedWarnings = [||] })
        let _, overridden = run [ "validate"; copy ]
        // --review: another's.
        let other = Path.Combine(folder, "other.legibyl.json")
        File.WriteAllText(other, ReviewFile.toJson (dismissing "Legibyl Structure-7"))
        let _, given = run [ "validate"; pdf; "--review"; other ]
        let missing, _ = run [ "validate"; pdf; "--review"; Path.Combine(folder, "none.json") ]

        carried.Value.GetProperty("dismissedWarnings").GetInt32() |> should equal 1
        carried.Value.GetProperty("review").GetString() |> should equal copy
        overridden.Value.GetProperty("dismissedWarnings").GetInt32() |> should equal 0
        given.Value.GetProperty("dismissedWarnings").GetInt32() |> should equal 1
        given.Value.GetProperty("review").GetString() |> should equal other
        missing |> should equal 64)
