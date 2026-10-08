module Legibyl.Validation.Tests.FigureRegionTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Tagging
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

type private PdfFile() =
    let path = Path.GetFullPath($"figure-region-{Guid.NewGuid():N}.pdf")
    member _.Path = path
    interface IDisposable with
        member _.Dispose() = if File.Exists path then File.Delete path

let private region: StructureGeometry =
    { PageNumber = 1; Left = 60.0; Bottom = 480.0; Right = 320.0; Top = 650.0 }

let private edit: ElementEdit =
    { Page = 1; Tag = "FigureRegion"; Box = region; Alt = "Sales rise from January to February."
      Text = null; Retag = "Figure"; MergeWith = []; Exclude = false; Move = None; Cells = [] }

/// Chart labels live in distant Art/Sect/P branches, interleaved with article paragraphs.
/// Some chart artwork is tagged, some artifact, and some outside every tag.
let private createWith (extra: PdfDocument -> PdfPage -> PdfCanvas -> unit) (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString "en-US") |> ignore
    let page = document.AddNewPage PageSize.LETTER
    let top = PdfStructElem(document, PdfName.Document)
    document.GetStructTreeRoot().AddKid top |> ignore
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let canvas = PdfCanvas page
    let text (parent: PdfStructElem) (tag: string) y x (value: string) =
        let elem = PdfStructElem(document, PdfName tag, page)
        parent.AddKid elem |> ignore
        let mcr = PdfMcrNumber(page, elem)
        elem.AddKid mcr |> ignore
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber(mcr.GetMcid())) |> ignore
        canvas.BeginMarkedContent(PdfName tag, properties).BeginText().SetFontAndSize(font, 12f).MoveText(x, y).ShowText(value).EndText().EndMarkedContent() |> ignore
        elem
    let art1 = PdfStructElem(document, PdfName.Art)
    top.AddKid art1 |> ignore
    let label1 = text art1 "P" 610.0 75.0 "January"
    text art1 "P" 720.0 72.0 "Article paragraph before chart." |> ignore
    let art2 = PdfStructElem(document, PdfName.Art)
    top.AddKid art2 |> ignore
    text art2 "P" 430.0 72.0 "Article paragraph after chart." |> ignore
    let section = PdfStructElem(document, PdfName.Sect)
    art2.AddKid section |> ignore
    text section "P" 610.0 210.0 "February" |> ignore
    let mcr = PdfMcrNumber(page, label1)
    label1.AddKid mcr |> ignore
    let properties = PdfDictionary()
    properties.Put(PdfName.MCID, PdfNumber(mcr.GetMcid())) |> ignore
    canvas.BeginMarkedContent(PdfName.P, properties).Rectangle(75.0, 500.0, 40.0, 80.0).Fill().EndMarkedContent() |> ignore
    canvas.BeginMarkedContent(PdfName.Artifact).Rectangle(210.0, 500.0, 40.0, 100.0).Fill().EndMarkedContent() |> ignore
    canvas.Rectangle(75.0, 490.0, 175.0, 2.0).Fill() |> ignore
    extra document page canvas

let private create path = createWith (fun _ _ _ -> ()) path

let private createMixedParagraph (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString "en-US") |> ignore
    let page = document.AddNewPage PageSize.LETTER
    let root = PdfStructElem(document, PdfName.Document)
    document.GetStructTreeRoot().AddKid root |> ignore
    let paragraph = PdfStructElem(document, PdfName.P, page)
    root.AddKid paragraph |> ignore
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let canvas = PdfCanvas page
    for mcid, y, value in [0, 720.0, "Unselected first sentence."; 1, 610.0, "Selected second sentence."] do
        let mcr = PdfMcrNumber(page, paragraph)
        paragraph.AddKid mcr |> ignore
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
        canvas.BeginMarkedContent(PdfName.P, properties).BeginText().SetFontAndSize(font, 12f)
            .MoveText(72.0, y).ShowText(value).EndText().EndMarkedContent() |> ignore

let private apply input output edits =
    VeraPdfValidator(Edits = edits).Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.ApplyEdits)

let rec private nodes (node: StructureNode) =
    node :: (node.Children |> Seq.collect nodes |> List.ofSeq)

let private render path =
    use renderer = new PdfRegionRenderer()
    let offPage = { region with Left = -20.0; Bottom = -20.0; Right = -10.0; Top = -10.0 }
    use bitmap = SkiaSharp.SKBitmap.Decode(renderer.RenderPageWithHighlight(path, offPage).PngBytes)
    bitmap.Pixels

[<Fact>]
let ``region gathers distant owners and artifact artwork without changing pixels or article paragraphs`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    use again = new PdfFile()
    create input.Path
    let validator = VeraPdfValidator()
    let before = validator.Validate(input.Path, PdfProfile.PdfUa1)
    let result = apply input.Path output.Path [edit]
    result.WasFixed |> should be True
    result.NotApplied |> should be Empty
    let tree = validator.ReadStructureTree output.Path |> notNull |> nodes
    let figures = tree |> List.filter (fun n -> n.Tag = "Figure")
    figures |> should haveLength 1
    figures.Head.AlternateDescription |> should equal edit.Alt
    figures.Head.ContentLeafCount |> should equal 5
    (string figures.Head.Content).Contains "January" |> should be True
    (string figures.Head.Content).Contains "February" |> should be True
    tree |> List.filter (fun n -> n.Tag = "P") |> List.map (fun n -> (string n.Content).Trim())
    |> should equal ["Article paragraph before chart."; "Article paragraph after chart."]
    render input.Path |> should equal (render output.Path)
    let failures (report: ValidationReport) =
        report.Findings |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure) |> Seq.map _.RuleId |> Set.ofSeq
    let after = validator.Validate(output.Path, PdfProfile.PdfUa1)
    let newFailures = Set.difference (failures after) (failures before)
    Assert.True(newFailures.IsEmpty,
                after.Findings |> Seq.filter (fun f -> newFailures.Contains f.RuleId) |> Seq.map (fun f -> $"{f.RuleId}: {f.Message} {f.ErrorMessage}") |> String.concat "\n")
    ElementEdits.pending output.Path [edit] |> should equal 0
    (apply output.Path again.Path [edit]).WasFixed |> should be False
    File.Exists again.Path |> should be False
    // Check both directions of ownership and /P pointers directly on the synthetic output.
    use document = new PdfDocument(new PdfReader(output.Path))
    let root = document.GetStructTreeRoot()
    let rec visit (element: PdfStructElem) =
        for kid in element.GetKids() do
            match kid with
            | :? PdfStructElem as child ->
                obj.ReferenceEquals((child.GetParent() :?> PdfStructElem).GetPdfObject(), element.GetPdfObject()) |> should be True
                visit child
            | :? PdfMcr as reference ->
                let found = root.FindMcrByMcid(document.GetPage(1).GetPdfObject(), reference.GetMcid())
                (found.GetParent() :?> PdfStructElem).GetPdfObject() |> should equal (element.GetPdfObject())
            | _ -> ()
    for kid in root.GetKids() do match kid with :? PdfStructElem as element -> visit element | _ -> ()

[<Theory>]
[<InlineData(149.0, 1, false)>]
[<InlineData(151.0, 2, true)>]
let ``whole drawings are included only when more than half covered`` (right: float) (expectedLeaves: int) (expanded: bool) =
    use input = new PdfFile()
    use output = new PdfFile()
    let drawRegion =
        { PageNumber = 1; Left = 50.0; Bottom = 200.0; Right = right; Top = 300.0 }
    let selected = {edit with Box = drawRegion}
    createWith (fun _ _ canvas ->
        canvas.BeginMarkedContent(PdfName.Artifact).Rectangle(60.0, 220.0, 10.0, 10.0).Fill().EndMarkedContent() |> ignore
        canvas.BeginMarkedContent(PdfName.Artifact).Rectangle(100.0, 200.0, 100.0, 100.0).Fill().EndMarkedContent() |> ignore) input.Path
    match ElementEdits.figureRegionPreview input.Path drawRegion with
    | Ok summary -> (summary.Contains "reaches outside the rectangle") |> should equal expanded
    | Error reason -> Assert.Fail reason
    let result = apply input.Path output.Path [selected]
    result.WasFixed |> should be True
    result.NotApplied |> should be Empty
    let figure = VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Figure")
    figure.ContentLeafCount |> should equal expectedLeaves
    render input.Path |> should equal (render output.Path)

[<Fact>]
let ``minor overlaps and outside drawings are excluded without changing appearance`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    let drawRegion = { PageNumber = 1; Left = 50.0; Bottom = 200.0; Right = 151.0; Top = 300.0 }
    createWith (fun _ _ canvas ->
        canvas.BeginMarkedContent(PdfName.Artifact).Rectangle(60.0, 220.0, 10.0, 10.0).Fill().EndMarkedContent() |> ignore
        canvas.BeginMarkedContent(PdfName.Artifact).Rectangle(150.0, 200.0, 100.0, 100.0).Fill().EndMarkedContent() |> ignore
        canvas.BeginMarkedContent(PdfName.Artifact).Rectangle(180.0, 220.0, 20.0, 20.0).Fill().EndMarkedContent() |> ignore) input.Path
    match ElementEdits.figureRegionPreview input.Path drawRegion with
    | Ok summary -> summary.Contains "reaches outside the rectangle" |> should be False
    | Error reason -> Assert.Fail reason
    let result = apply input.Path output.Path [{edit with Box = drawRegion}]
    result.WasFixed |> should be True
    let figure = VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Figure")
    figure.ContentLeafCount |> should equal 1
    render input.Path |> should equal (render output.Path)

[<Fact>]
let ``zero width and height drawing boxes are safely included only when contained`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    let drawRegion = { PageNumber = 1; Left = 50.0; Bottom = 200.0; Right = 151.0; Top = 300.0 }
    createWith (fun _ _ canvas ->
        canvas.BeginMarkedContent(PdfName.Artifact).Rectangle(60.0, 220.0, 10.0, 10.0).Fill().EndMarkedContent() |> ignore
        canvas.BeginMarkedContent(PdfName.Artifact).Rectangle(110.0, 220.0, 0.0, 20.0).Fill().EndMarkedContent() |> ignore
        canvas.BeginMarkedContent(PdfName.Artifact).Rectangle(120.0, 230.0, 20.0, 0.0).Fill().EndMarkedContent() |> ignore) input.Path
    let result = apply input.Path output.Path [{edit with Box = drawRegion}]
    result.WasFixed |> should be True
    let figure = VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Figure")
    figure.ContentLeafCount |> should equal 3
    render input.Path |> should equal (render output.Path)

[<Fact>]
let ``a Figure region refuses a partial semantic owner through the public APIs`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    createMixedParagraph input.Path
    let originalBytes = File.ReadAllBytes input.Path
    let message = "make it cover all of that tag's content, or none of it"
    match ElementEdits.figureRegionPreview input.Path region with
    | Error reason -> reason.Contains message |> should be True
    | Ok _ -> Assert.Fail "A partial semantic owner must be refused in the preview."
    let result = apply input.Path output.Path [edit]
    result.WasFixed |> should be False
    result.NotApplied |> Seq.exists (fun reason -> reason.Contains message) |> should be True
    File.Exists output.Path |> should be False
    File.ReadAllBytes input.Path |> should equal originalBytes

[<Fact>]
let ``empty region and missing alt text are explicitly refused`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    create input.Path
    for requested, reason in
        [ {edit with Alt = " "}, "enter alt text"
          {edit with Box = {region with Left = 400.0; Right = 500.0}}, "nothing drawn was found in the region" ] do
        let result = apply input.Path output.Path [requested]
        result.WasFixed |> should be False
        result.NotApplied |> Seq.exists (fun s -> s.Contains reason) |> should be True

[<Fact>]
let ``review roundtrip retains the region operation used by CLI edits`` () =
    let saved = ReviewFile.ofElementEdit edit
    // Public review JSON readers, as --edits uses them.
    let json = System.Text.Json.JsonSerializer.Serialize saved
    let file = ReviewFile.parse ($"""{{"version":1,"edits":[{json}]}}""") |> Option.get
    ReviewFile.edits file |> List.exactlyOne |> should equal edit

[<Fact>]
let ``a repeated MCID outside the region cannot pull an article paragraph into the figure`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    createWith (fun _ _ canvas ->
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber 0) |> ignore
        canvas.BeginMarkedContent(PdfName.P, properties).BeginText()
            .SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f)
            .MoveText(72.0, 400.0).ShowText("Unrelated text sharing a broken MCID.").EndText().EndMarkedContent() |> ignore) input.Path
    let result = apply input.Path output.Path [edit]
    result.WasFixed |> should be False
    result.NotApplied |> Seq.exists (fun s -> s.Contains "shares its mark") |> should be True

[<Fact>]
let ``a marked sequence spanning distant text is left intact`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    createWith (fun document page canvas ->
        let owner = PdfStructElem(document, PdfName.P, page)
        document.GetStructTreeRoot().AddKid owner |> ignore
        let mcr = PdfMcrNumber(page, owner)
        owner.AddKid mcr |> ignore
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber(mcr.GetMcid())) |> ignore
        let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
        canvas.BeginMarkedContent(PdfName.P, properties)
            .BeginText().SetFontAndSize(font, 12f).MoveText(75.0, 610.0).ShowText("Selected sequence text.").EndText()
            .BeginText().SetFontAndSize(font, 12f).MoveText(75.0, 400.0).ShowText("Distant unrelated sentence.").EndText()
            .EndMarkedContent() |> ignore) input.Path
    let result = apply input.Path output.Path [edit]
    result.WasFixed |> should be True
    let tree = VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes
    let figure = tree |> List.find (fun n -> n.Tag = "Figure")
    (string figure.Content).Contains "Distant unrelated sentence." |> should be False
    tree |> List.exists (fun n -> n.Tag = "P" && (string n.Content).Contains "Distant unrelated sentence.") |> should be True
    render input.Path |> should equal (render output.Path)

[<Fact>]
let ``a thick stroke beyond the rectangle is included whole when mostly covered`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    createWith (fun _ _ canvas ->
        canvas.BeginMarkedContent(PdfName.Artifact).SetLineWidth(40f).SetLineJoinStyle(1)
            .MoveTo(65.0, 520.0).LineTo(65.0, 580.0).Stroke().EndMarkedContent() |> ignore) input.Path
    match ElementEdits.figureRegionPreview input.Path region with
    | Ok summary -> Assert.True(summary.Contains "reaches outside the rectangle", summary)
    | Error reason -> Assert.Fail reason
    let result = apply input.Path output.Path [edit]
    result.WasFixed |> should be True
    result.NotApplied |> should be Empty
    render input.Path |> should equal (render output.Path)

[<Fact>]
let ``nested marked content and forms need explicit review rather than guessing ownership`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    for extra, reason in
        [ (fun (_: PdfDocument) (_: PdfPage) (canvas: PdfCanvas) ->
              canvas.BeginMarkedContent(PdfName.Artifact).BeginMarkedContent(PdfName.Artifact)
                  .Rectangle(100.0, 520.0, 20.0, 20.0).Fill().EndMarkedContent().EndMarkedContent() |> ignore), "marked in a way that can't be changed safely"
          (fun _ _ canvas ->
              let form = iText.Kernel.Pdf.Xobject.PdfFormXObject(Rectangle(0f, 0f, 20f, 20f))
              PdfCanvas(form, canvas.GetDocument()).Rectangle(0.0, 0.0, 20.0, 20.0).Fill() |> ignore
              canvas.AddXObjectAt(form, 100f, 520f) |> ignore), "the region includes a form" ] do
        createWith extra input.Path
        let result = apply input.Path output.Path [edit]
        result.WasFixed |> should be False
        result.NotApplied |> Seq.exists (fun s -> s.Contains reason) |> should be True

[<Fact>]
let ``saved decimal coordinates recognise an already applied figure`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    use again = new PdfFile()
    create input.Path
    let original = {edit with Box = {region with Left = 60.123; Bottom = 480.234; Right = 320.345; Top = 650.456}}
    (apply input.Path output.Path [original]).WasFixed |> should be True
    let reopened = ReviewFile.ofElementEdit original |> ReviewFile.toElementEdit |> Option.get
    ElementEdits.pending output.Path [reopened] |> should equal 0
    (apply output.Path again.Path [reopened]).WasFixed |> should be False

[<Fact>]
let ``rotated offset or interactive regions are refused explicitly`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    for extra, reason in
        [ (fun (_: PdfDocument) (page: PdfPage) (_: PdfCanvas) -> page.SetRotation 90 |> ignore), "rotated"
          (fun _ page _ -> page.SetCropBox(Rectangle(10f, 10f, 600f, 770f)) |> ignore), "visible area is offset"
          (fun _ page _ -> page.AddAnnotation(iText.Kernel.Pdf.Annot.PdfLinkAnnotation(Rectangle(100f, 520f, 20f, 20f))) |> ignore), "link, form field or other annotation" ] do
        createWith extra input.Path
        let result = apply input.Path output.Path [edit]
        result.WasFixed |> should be False
        result.NotApplied |> Seq.exists (fun s -> s.Contains reason) |> should be True

[<Fact>]
let ``unreadable drawing resources are not treated as an empty region`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    createWith (fun _ page _ ->
        page.GetFirstContentStream().SetData(System.Text.Encoding.ASCII.GetBytes "/Missing Do\n") |> ignore) input.Path
    let result = apply input.Path output.Path [edit]
    result.WasFixed |> should be False
    result.NotApplied |> Seq.exists (fun s -> s.Contains "can't be measured") |> should be True
    match ElementEdits.figureRegionPreview input.Path region with
    | Error message -> Assert.False(message.Contains "nothing drawn was found in the region")
    | Ok _ -> Assert.Fail "An unreadable resource must refuse the preview."

[<Fact>]
let ``artifact-only selection becomes a figure while labels outside it remain text`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    create input.Path
    let selected = {edit with Box = {region with Left = 200.0; Bottom = 495.0; Right = 260.0; Top = 605.0}}
    let result = apply input.Path output.Path [selected]
    result.WasFixed |> should be True
    result.NotApplied |> should be Empty
    let tree = VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes
    tree |> List.filter (fun n -> n.Tag = "Figure") |> List.map _.ContentLeafCount |> should equal [1]
    tree |> List.filter (fun n -> n.Tag = "P") |> List.length |> should equal 4
    render input.Path |> should equal (render output.Path)

[<Fact>]
let ``a figure region can be described again without duplicating its marked content`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    use changed = new PdfFile()
    create input.Path
    (apply input.Path output.Path [edit]).WasFixed |> should be True
    let updated = {edit with Alt = "Sales increased by a quarter."}
    (apply output.Path changed.Path [updated]).WasFixed |> should be True
    let tree = VeraPdfValidator().ReadStructureTree changed.Path |> notNull |> nodes
    let figures = tree |> List.filter (fun n -> n.Tag = "Figure")
    figures |> should haveLength 1
    figures.Head.AlternateDescription |> should equal updated.Alt
    figures.Head.ContentLeafCount |> should equal 5
    render output.Path |> should equal (render changed.Path)

/// The output's Figure dictionaries.
let private figuresIn (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    [ for i in 1 .. document.GetNumberOfPdfObjects() - 1 do
          match document.GetPdfObject i with
          | :? PdfDictionary as d when PdfName.StructElem.Equals(d.GetAsName PdfName.Type) && PdfName.Figure.Equals(d.GetAsName PdfName.S) ->
              let box =
                  match d.GetAsDictionary PdfName.A with
                  | NonNull a when PdfName.Layout.Equals(a.GetAsName PdfName.O) ->
                      match a.GetAsArray PdfName.BBox with
                      | NonNull b -> Some [ for j in 0..3 -> b.GetAsNumber(j).DoubleValue() ]
                      | Null -> None
                  | _ -> None
              yield box
          | _ -> () ]

[<Fact>]
let ``the Figure has a bounding box, around what it draws`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    create input.Path

    (apply input.Path output.Path [ edit ]).WasFixed |> should be True

    match figuresIn output.Path with
    | [ Some [ left; bottom; right; top ] ] ->
        // The labels, bars and baseline: from the January label and bar (75) to the end of the
        // February label (210, about 48 wide), the baseline (490) to the labels' tops.
        left |> should (equalWithin 1.0) 75.0
        bottom |> should (equalWithin 1.0) 490.0
        right |> should be (greaterThan 250.0)
        right |> should be (lessThan 265.0)
        top |> should be (greaterThan 610.0)
        top |> should be (lessThan 630.0)
    | other -> failwith $"Expected one Figure with a Layout BBox: {other}"

[<Fact>]
let ``content marked outside the tags with the next free number isn't pulled into the Figure`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    // MCIDs 0 to 4 are in the tags; 5, the next iText would number, is marked here outside them.
    createWith (fun _ _ canvas ->
        let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber 5) |> ignore
        canvas.BeginMarkedContent(PdfName.Span, properties).BeginText().SetFontAndSize(font, 12f)
            .MoveText(400.0, 100.0).ShowText("Stray text.").EndText().EndMarkedContent() |> ignore) input.Path

    (apply input.Path output.Path [ edit ]).WasFixed |> should be True

    let tree = VeraPdfValidator().ReadStructureTree output.Path |> nonNull |> nodes
    let figure = tree |> List.find (fun n -> n.Tag = "Figure")
    (string figure.Content).Contains "Stray" |> should be False

/// A tagged page of paragraphs straight under the root (no Document), the second a chart label in
/// the region, with a bar under it.
let private createRootParagraphs (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString "en-US") |> ignore
    let page = document.AddNewPage PageSize.LETTER
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let canvas = PdfCanvas page
    for value, y in [ "Before the chart.", 720.0; "Chart label", 610.0; "After the chart.", 400.0 ] do
        let paragraph = PdfStructElem(document, PdfName.P, page)
        document.GetStructTreeRoot().AddKid paragraph |> ignore
        let mcr = PdfMcrNumber(page, paragraph)
        paragraph.AddKid mcr |> ignore
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber(mcr.GetMcid())) |> ignore
        canvas.BeginMarkedContent(PdfName.P, properties).BeginText().SetFontAndSize(font, 12f).MoveText(75.0, y).ShowText(value).EndText().EndMarkedContent() |> ignore
    canvas.Rectangle(75.0, 500.0, 40.0, 80.0).Fill() |> ignore

[<Fact>]
let ``the Figure goes where its content was read, and the parent tree ties its content to it`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    createRootParagraphs input.Path

    (apply input.Path output.Path [ edit ]).WasFixed |> should be True

    let tree = VeraPdfValidator().ReadStructureTree output.Path |> nonNull
    [ for n in tree.Children -> n.Tag ] |> should equal [ "P"; "Figure"; "P" ]
    // Each MCID the Figure holds is tied to it in the parent tree, read from the file itself.
    use document = new PdfDocument(new PdfReader(output.Path))
    let page = document.GetPage(1).GetPdfObject()
    let key = page.GetAsNumber(PdfName.StructParents).IntValue()
    let rec slots (tree: PdfDictionary) =
        match tree.GetAsArray PdfName.Nums with
        | NonNull nums ->
            [ 0 .. 2 .. nums.Size() - 2 ] |> List.tryPick (fun i -> if nums.GetAsNumber(i).IntValue() = key then Some(nums.GetAsArray(i + 1)) else None)
        | Null ->
            match tree.GetAsArray PdfName.Kids with
            | NonNull kids -> [ 0 .. kids.Size() - 1 ] |> List.tryPick (fun i -> slots (kids.GetAsDictionary i))
            | Null -> None
    let array = document.GetStructTreeRoot().GetPdfObject().GetAsDictionary PdfName.ParentTree |> slots |> Option.get
    let figure =
        let k = document.GetStructTreeRoot().GetPdfObject().GetAsArray PdfName.K
        [ for i in 0 .. k.Size() - 1 -> k.GetAsDictionary i ] |> List.find (fun d -> PdfName.Figure.Equals(d.GetAsName PdfName.S))
    let mcids =
        let k = figure.GetAsArray PdfName.K
        [ for i in 0 .. k.Size() - 1 ->
              match k.Get(i, true) with
              | :? PdfNumber as n -> n.IntValue()
              | :? PdfDictionary as mcr -> mcr.GetAsNumber(PdfName.MCID).IntValue()
              | _ -> -1 ]
    mcids |> should not' (be Empty)
    for mcid in mcids do
        obj.ReferenceEquals(array.GetAsDictionary mcid, figure) |> should be True

[<Fact>]
let ``chart labels in table cells leave every cell in its row, with the Figure in the first`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        document.SetTagged() |> ignore
        document.GetCatalog().SetLang(PdfString "en-US") |> ignore
        let page = document.AddNewPage PageSize.LETTER
        let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
        let canvas = PdfCanvas page
        let top = PdfStructElem(document, PdfName.Document)
        document.GetStructTreeRoot().AddKid top |> ignore
        let table = PdfStructElem(document, PdfName.Table)
        top.AddKid table |> ignore
        let row = PdfStructElem(document, PdfName.TR)
        table.AddKid row |> ignore
        // Two labels in the region, in cells of their own, and a third cell outside it.
        for value, x, y in [ "Jan", 75.0, 610.0; "Feb", 200.0, 610.0; "Outside the chart", 400.0, 610.0 ] do
            let cell = PdfStructElem(document, PdfName.TD)
            row.AddKid cell |> ignore
            let paragraph = PdfStructElem(document, PdfName.P, page)
            cell.AddKid paragraph |> ignore
            let mcr = PdfMcrNumber(page, paragraph)
            paragraph.AddKid mcr |> ignore
            let properties = PdfDictionary()
            properties.Put(PdfName.MCID, PdfNumber(mcr.GetMcid())) |> ignore
            canvas.BeginMarkedContent(PdfName.P, properties).BeginText().SetFontAndSize(font, 12f).MoveText(x, y).ShowText(value).EndText().EndMarkedContent() |> ignore

    (apply input.Path output.Path [ edit ]).WasFixed |> should be True

    let tree = VeraPdfValidator().ReadStructureTree output.Path |> nonNull |> nodes
    let row = tree |> List.find (fun n -> n.Tag = "TR")
    // Every cell kept (the second, emptied, is still a cell), the Figure in the first.
    [ for cell in row.Children -> cell.Tag ] |> should equal [ "TD"; "TD"; "TD" ]
    [ for n in (Seq.head row.Children).Children -> n.Tag ] |> should equal [ "Figure" ]

[<Fact>]
let ``a shading elsewhere on the page doesn't stop the Figure`` () =
    use input = new PdfFile()
    use output = new PdfFile()
    createWith (fun _ _ canvas ->
        let shading =
            iText.Kernel.Pdf.Colorspace.Shading.PdfAxialShading(
                iText.Kernel.Pdf.Colorspace.PdfDeviceCs.Rgb(), 400f, 100f, [| 1f; 0f; 0f |], 450f, 150f, [| 0f; 0f; 1f |])
        canvas.SaveState().Rectangle(400.0, 100.0, 50.0, 50.0).Clip().EndPath().PaintShading(shading).RestoreState() |> ignore) input.Path

    let result = apply input.Path output.Path [ edit ]

    result.WasFixed |> should be True
    match ElementEdits.figureRegionPreview input.Path region with
    | Ok summary -> summary |> should haveSubstring "shadings (gradients)"
    | Error reason -> failwith reason
