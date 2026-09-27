module Legibyl.Validation.Tests.LayoutTaggingTests

open System
open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.IO.Image
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Action
open iText.Kernel.Pdf.Annot
open iText.Kernel.Pdf.Canvas
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// An untagged one-page PDF: a running header, a title with a rule under it, a paragraph, two
/// bullets (drawn inside a q/Q), an inline image, a paragraph with a link, and a page number.
let private createUntagged (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage PageSize.LETTER
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let text (x: float) (y: float) (size: float32) (value: string) =
        canvas.BeginText().SetFontAndSize(font, size).MoveText(x, y).ShowText(value).EndText() |> ignore
    text 72.0 750.0 10f "Running header"
    text 72.0 700.0 20f "The Title"
    canvas.MoveTo(72.0, 690.0).LineTo(540.0, 690.0).Stroke() |> ignore
    text 72.0 660.0 12f "An opening paragraph."
    canvas.SaveState() |> ignore
    text 90.0 630.0 12f "• First item"
    text 90.0 612.0 12f "• Second item"
    canvas.RestoreState() |> ignore
    let pixels = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 255uy; 255uy; 0uy |]
    let image = ImageDataFactory.Create(2, 2, 3, 8, pixels, null)
    canvas.AddImageWithTransformationMatrix(image, 100f, 0f, 0f, 100f, 72f, 450f, true) |> ignore
    text 72.0 400.0 12f "Visit the site."
    let link = PdfLinkAnnotation(Rectangle(72f, 395f, 80f, 15f))
    link.SetAction(PdfAction.CreateURI "https://example.com") |> ignore
    page.AddAnnotation link |> ignore
    text 300.0 40.0 10f "Page 1"

let private region tag (left, bottom, right, top) numbering : LayoutRegion =
    { Tag = tag
      Geometry = { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }
      ListNumbering = numbering
      ActualText = null
      QrText = null
      Alt = null }

let private regions =
    [ region "Artifact" (70.0, 745.0, 200.0, 762.0) null
      region "H1" (70.0, 695.0, 300.0, 720.0) null
      region "P" (70.0, 655.0, 300.0, 675.0) null
      region "LI" (88.0, 626.0, 300.0, 642.0) "Disc"
      region "LI" (88.0, 608.0, 300.0, 624.0) "Disc"
      region "Figure" (70.0, 448.0, 175.0, 552.0) null
      region "P" (70.0, 395.0, 300.0, 415.0) null
      region "Artifact" (295.0, 35.0, 360.0, 52.0) null ]

/// Regions from a list, as a layout detector would give them.
type private FixedLayout(regions: LayoutRegion list, ?linkDescription: string, ?title: string, ?pageTitles: Map<string, string>) =
    interface IPageLayoutSource with
        member _.PageRegions(_, _) = regions
        member _.LinkDescription(_, _, _) = Option.toObj linkDescription
        member _.PageTitles = pageTitles |> Option.map (fun t -> t :> Collections.Generic.IReadOnlyDictionary<_, _>) |> Option.toObj
        member _.DocumentTitle = Option.toObj title

let private tagFromLayout (input: string) (output: string) (source: IPageLayoutSource option) =
    let validator = VeraPdfValidator()
    source |> Option.iter (fun source -> validator.LayoutSource <- source)
    validator.Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.TagFromLayout)

let private failedRules (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

let rec private flatten (node: StructureNode) : StructureNode list =
    node :: (node.Children |> Seq.collect flatten |> List.ofSeq)

[<Fact>]
let ``tagging from the layout tags every piece of content without changing how the page looks`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path

    let result = tagFromLayout input.Path output.Path (Some(FixedLayout regions))

    result.OutputPath |> should equal output.Path
    result.AppliedFixes
    |> should
        contain
        "Tagged 1 page from their layout: 1 heading, 2 paragraphs, 2 list items, 1 figure and 1 link; page furniture marked as artifacts"
    result.AppliedFixes |> should contain "1 figure needs alt text, written by a person"

    use renderer = new PdfRegionRenderer()
    let offPage = { PageNumber = 1; Left = -20.0; Bottom = -20.0; Right = -10.0; Top = -10.0 }
    let before = renderer.RenderPageWithHighlight(input.Path, offPage).PngBytes
    let after = renderer.RenderPageWithHighlight(output.Path, offPage).PngBytes
    use beforeImage = SkiaSharp.SKBitmap.Decode before
    use afterImage = SkiaSharp.SKBitmap.Decode after
    afterImage.Pixels |> should equal beforeImage.Pixels

    let rules = failedRules output.Path
    for rule in [ "7.1-3"; "7.1-11"; "6.2-1"; "7.18.1-1"; "7.18.3-1"; "7.18.5-1" ] do
        rules |> should not' (contain rule)
    // The figure has no alt text: that's for a person to write.
    rules |> should contain "7.3-1"

[<Fact>]
let ``the structure follows the regions' reading order, with list items in a list`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path
    tagFromLayout input.Path output.Path (Some(FixedLayout regions)) |> ignore

    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    let document = root.Children |> Seq.exactlyOne
    document.Tag |> should equal "Document"
    document.Children |> Seq.map _.Tag |> List.ofSeq |> should equal [ "H1"; "P"; "L"; "Figure"; "P" ]
    document.Children[0].Content |> should equal "The Title"
    let list = document.Children[2]
    [ for item in list.Children -> item.Tag, [ for body in item.Children -> body.Tag, body.Content ] ]
    |> should equal [ "LI", [ "LBody", "• First item" ]; "LI", [ "LBody", "• Second item" ] ]
    document.Children[4].Children |> Seq.map _.Tag |> List.ofSeq |> should equal [ "Link" ]
    // Page furniture is an artifact, so it isn't read out.
    flatten root |> List.choose (fun node -> Option.ofObj node.Content) |> List.filter (fun text -> text.Contains "header" || text.Contains "Page 1")
    |> should be Empty

[<Fact>]
let ``text outside every region still reads, in the nearest text region`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path
    // No region for the opening paragraph: it joins the first list item, 22pt below it (the
    // title is 31pt above).
    let missing = regions |> List.filter (fun r -> r.Tag <> "P" || r.Geometry.Top <> 675.0)
    tagFromLayout input.Path output.Path (Some(FixedLayout missing)) |> ignore

    failedRules output.Path |> should not' (contain "7.1-3")
    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    flatten root |> List.exists (fun node -> node.Tag = "LBody" && (string node.Content).Contains "An opening paragraph.") |> should be True

[<Fact>]
let ``a page with no regions has its text as one paragraph`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path
    tagFromLayout input.Path output.Path (Some(FixedLayout [])) |> ignore

    failedRules output.Path |> should not' (contain "7.1-3")
    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    (root.Children |> Seq.exactlyOne).Children |> Seq.map _.Tag |> Seq.filter ((<>) "Link") |> List.ofSeq |> should equal [ "P" ]

[<Fact>]
let ``an untagged document's untagged content is fixable by tagging from the layout`` () =
    use input = new TempPdf()
    createUntagged input.Path
    let report = VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1)
    report.FixableCount PdfFixOptions.TagFromLayout |> should be (greaterThan 0)
    report.Findings
    |> Seq.filter (fun f -> f.RuleId = "7.1-3" && not f.Passed)
    |> Seq.forall (fun f -> f.FixOptions.HasFlag PdfFixOptions.TagFromLayout)
    |> should be True

[<Fact>]
let ``a tagged document, or no layout source, is left alone`` () =
    use tagged = new TempPdf()
    tagFromLayout (corpusFile "pdfua1-pass.pdf") tagged.Path (Some(FixedLayout regions)) |> _.OutputPath |> should be Null

    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path
    tagFromLayout input.Path output.Path None |> _.OutputPath |> should be Null

/// Text drawn as outlines: a filled shape where the words would be, and no text at all.
let private createOutlined (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
    canvas.Rectangle(72.0, 700.0, 200.0, 12.0).Fill() |> ignore
    canvas.MoveTo(72.0, 650.0).LineTo(540.0, 650.0).Stroke() |> ignore

[<Fact>]
let ``text drawn as outlines is tagged with its OCR text as ActualText`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createOutlined input.Path
    let outlined = { region "P" (70.0, 695.0, 280.0, 715.0) null with ActualText = "Outlined words" }

    let result = tagFromLayout input.Path output.Path (Some(FixedLayout [ outlined ]))

    result.AppliedFixes |> should contain "1 element's text is drawn as outlines or pixels: read with OCR into /ActualText, to check"
    let rules = failedRules output.Path
    // All drawn: the outline as the paragraph, the rule as decoration; its text's language is known.
    rules |> should not' (contain "7.1-3")
    rules |> should not' (contain "7.2-21")
    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    let paragraph = (root.Children |> Seq.exactlyOne).Children |> Seq.exactlyOne
    (paragraph.Tag, paragraph.ActualText, paragraph.ContentLeafCount) |> should equal ("P", "Outlined words", 1)

[<Fact>]
let ``a heading drawn in pieces is one marked sequence, and a wrapped line reads with a space`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
        let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
        // "Our big events" in three text objects, each in its own saved state, as Quartz writes it.
        let mutable x = 72.0
        for piece in [ "Our big e"; "v"; "ents" ] do
            canvas.SaveState().BeginText().SetFontAndSize(font, 20f).MoveText(x, 700.0).ShowText(piece).EndText().RestoreState() |> ignore
            x <- x + float (font.GetWidth(piece, 20f))
        // "Arrow of Light" wrapped onto two lines in one text object.
        canvas.BeginText().SetFontAndSize(font, 12f).SetLeading(14f).MoveText(72.0, 650.0).ShowText("Arrow of").NewlineShowText("Light").EndText()
        |> ignore
    let regions = [ region "H1" (70.0, 695.0, 300.0, 720.0) null; region "P" (70.0, 630.0, 300.0, 665.0) null ]

    tagFromLayout input.Path output.Path (Some(FixedLayout regions)) |> ignore

    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    [ for node in (root.Children |> Seq.exactlyOne).Children -> node.Tag, node.Content, node.ContentLeafCount ]
    |> should equal [ "H1", "Our big events", 1; "P", "Arrow of Light", 1 ]

let private withQrCode (regions: LayoutRegion list) =
    regions |> List.map (fun r -> if r.Tag = "Figure" then { r with QrText = "https://example.com/apply" } else r)

let private tagWithQrLinks (input: string) (output: string) (source: IPageLayoutSource) =
    let validator = VeraPdfValidator()
    validator.LayoutSource <- source
    validator.Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.TagFromLayout ||| PdfFixOptions.QrCodeLinks)

[<Fact>]
let ``a figure that's a QR code is described by what it encodes`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path

    let result = tagFromLayout input.Path output.Path (Some(FixedLayout(withQrCode regions)))

    result.AppliedFixes |> should contain "Described 1 QR code by what they encode"
    result.AppliedFixes |> should not' (contain "1 figure needs alt text, written by a person")
    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    let figure = flatten root |> List.find (fun node -> node.Tag = "Figure")
    figure.AlternateDescription |> should equal "QR code linking to https://example.com/apply"
    failedRules output.Path |> should not' (contain "7.3-1")

[<Fact>]
let ``with link titles, a QR code's description names the page it goes to`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path
    let validator = VeraPdfValidator(LayoutSource = FixedLayout(withQrCode regions), PageTitleSource = FixedTitles [ "https://example.com/apply", "Apply for benefits" ])

    let result =
        validator.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.TagFromLayout ||| PdfFixOptions.QrCodeLinks ||| PdfFixOptions.LinkTitlesFromWeb)

    result.AppliedFixes |> should contain "1 QR code's description names the title of the web page it goes to, read from the web: to check"
    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    let figure = flatten root |> List.find (fun node -> node.Tag = "Figure")
    figure.AlternateDescription |> should equal "QR code linking to Apply for benefits (https://example.com/apply)"
    use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfReader(output.Path))
    let annotations = document.GetPage(1).GetAnnotations()
    annotations
    |> Seq.exists (fun a -> not (isNull (a.GetContents())) && a.GetContents().ToUnicodeString() = "QR code link to Apply for benefits (https://example.com/apply)")
    |> should be True

[<Fact>]
let ``a QR code's link is described with the alt text a person wrote for it`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path
    let written = withQrCode regions |> List.map (fun r -> if r.Tag = "Figure" then { r with Alt = "Scan to apply online" } else r)

    tagWithQrLinks input.Path output.Path (FixedLayout written) |> ignore

    use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfReader(output.Path))
    [ for a in document.GetPage(1).GetAnnotations() do
          if not (isNull (a.GetContents())) then a.GetContents().ToUnicodeString() ]
    |> should contain "Scan to apply online"

[<Fact>]
let ``a heading given a level keeps it`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path
    let leveled = regions |> List.map (fun r -> if r.Tag = "P" then { r with Tag = "H3" } else r)

    tagFromLayout input.Path output.Path (Some(FixedLayout leveled)) |> ignore

    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    // Each paragraph given H3 is one; none is written as a paragraph.
    let tags = flatten root |> List.map _.Tag
    tags |> List.filter (fun t -> t.StartsWith "H") |> List.distinct |> should equal [ "H1"; "H3" ]
    tags |> should not' (contain "P")

[<Fact>]
let ``with QR code links, the figure is a link to what it encodes, and the page looks the same`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path

    let result = tagWithQrLinks input.Path output.Path (FixedLayout(withQrCode regions))

    result.AppliedFixes |> should contain "Linked 1 QR code to the web address they encode"
    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    let document = root.Children |> Seq.exactlyOne
    let link = document.Children |> Seq.find (fun node -> node.Tag = "Link" && node.Children |> Seq.exists (fun child -> child.Tag = "Figure"))
    link.LinkUri |> should equal "https://example.com/apply"
    // The new link adds no failures: tagged, described, and in the tab order. (The page's
    // own link, which has no description, fails the same either way.)
    use plain = new TempPdf()
    tagFromLayout input.Path plain.Path (Some(FixedLayout(withQrCode regions))) |> ignore
    let failures (path: string) =
        VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
        |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
        |> Seq.countBy _.RuleId
        |> Map.ofSeq
    failures output.Path |> should equal (failures plain.Path)
    use renderer = new PdfRegionRenderer()
    let offPage = { PageNumber = 1; Left = -20.0; Bottom = -20.0; Right = -10.0; Top = -10.0 }
    use before = SkiaSharp.SKBitmap.Decode(renderer.RenderPageWithHighlight(input.Path, offPage).PngBytes)
    use after = SkiaSharp.SKBitmap.Decode(renderer.RenderPageWithHighlight(output.Path, offPage).PngBytes)
    after.Pixels |> should equal before.Pixels

[<Fact>]
let ``a fix reports each step it runs, in order, with pages where there are pages`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path
    let reports = ResizeArray<FixProgress>()
    let progress = { new IProgress<FixProgress> with member _.Report p = reports.Add p }
    let validator = VeraPdfValidator()
    validator.LayoutSource <- FixedLayout regions

    validator.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.TagFromLayout ||| PdfFixOptions.AnnotationTabs ||| PdfFixOptions.Metadata, progress)
    |> ignore

    // The distinct steps, in order, each numbered out of four.
    reports |> Seq.map (fun p -> p.Stage, p.Step, p.Steps) |> Seq.distinct |> List.ofSeq
    |> should
        equal
        [ FixStage.CheckingOriginal, 1, 4
          FixStage.Tagging, 2, 4
          FixStage.Repairing, 3, 4
          FixStage.RepairingMetadata, 4, 4 ]
    reports |> Seq.exists (fun p -> p.Stage = FixStage.CheckingOriginal && p.Page = 1 && p.PageCount = 1) |> should be True
    reports |> Seq.exists (fun p -> p.Stage = FixStage.Tagging && p.Page = 1 && p.PageCount = 1) |> should be True

[<Fact>]
let ``alt text written for a figure is its alt text, ahead of a QR code's own`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path
    let written =
        regions |> List.map (fun r -> if r.Tag = "Figure" then { r with Alt = "Our pack's colour swatch"; QrText = "https://example.com/apply" } else r)

    let result = tagFromLayout input.Path output.Path (Some(FixedLayout written))

    result.AppliedFixes |> should contain "Gave 1 figure the alt text a person wrote"
    result.AppliedFixes |> Seq.exists (fun line -> line.Contains "need alt text" || line.Contains "needs alt text") |> should be False
    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    (flatten root |> List.find (fun node -> node.Tag = "Figure")).AlternateDescription |> should equal "Our pack's colour swatch"
    failedRules output.Path |> should not' (contain "7.3-1")

[<Fact>]
let ``a link without a description gets the one the layout source has`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path

    let result = tagFromLayout input.Path output.Path (Some(FixedLayout(regions, "Example Domain (example.com)")))

    result.AppliedFixes |> should contain "Described 1 link as reviewed"
    do
        use document = new PdfDocument(new PdfReader(output.Path))
        let link = document.GetPage(1).GetAnnotations() |> Seq.find (fun a -> PdfName.Link.Equals(a.GetSubtype()))
        link.GetContents().ToUnicodeString() |> should equal "Example Domain (example.com)"
    failedRules output.Path |> should not' (contain "7.18.5-2")

[<Fact>]
let ``a title from the layout source titles the document and shows it`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path

    let result = tagFromLayout input.Path output.Path (Some(FixedLayout(regions, title = "Cub Scout Pack 721")))

    result.AppliedFixes |> should contain "Titled the document \"Cub Scout Pack 721\", as reviewed"
    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetDocumentInfo().GetTitle() |> should equal "Cub Scout Pack 721"
    document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.ViewerPreferences).GetAsBoolean(PdfName.DisplayDocTitle).GetValue() |> should be True

[<Fact>]
let ``QR codes named by titles the source already read are described as QR codes, not as written alt text`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path
    let qr = regions |> List.map (fun r -> if r.Tag = "Figure" then { r with QrText = "https://example.com/apply" } else r)

    let result = tagFromLayout input.Path output.Path (Some(FixedLayout(qr, pageTitles = Map [ "https://example.com/apply", "Apply online" ])))

    result.AppliedFixes |> should contain "Described 1 QR code by what they encode"
    result.AppliedFixes |> Seq.exists (fun line -> line.StartsWith "Gave ") |> should be False
    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    (flatten root |> List.find (fun node -> node.Tag = "Figure")).AlternateDescription
    |> should equal "QR code linking to Apply online (https://example.com/apply)"

[<Fact>]
let ``a document's title candidates are its own title, then a title page's text`` () =
    use titled = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(titled.Path))
        document.AddNewPage() |> ignore
        document.GetDocumentInfo().SetTitle "Design" |> ignore
    DocumentTitles.candidates titled.Path |> should equal [ { DocumentTitles.Title = "Design"; DocumentTitles.Source = "document info" } ]

    use untitled = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(untitled.Path))
        let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
        canvas.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 24f).MoveText(72.0, 600.0).ShowText("Den Leader Guide").EndText() |> ignore
        document.AddNewPage() |> ignore
    DocumentTitles.candidates untitled.Path |> should equal [ { DocumentTitles.Title = "Den Leader Guide"; DocumentTitles.Source = "first page" } ]

[<Fact>]
let ``tagging says which element each region became, and the box of what it holds`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path
    let mutable tagged: TaggedRegion list = []
    let validator =
        VeraPdfValidator(
            LayoutSource = FixedLayout regions,
            Review = Func<_, _>(fun regions -> tagged <- List.ofSeq regions; null)
        )

    validator.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.TagFromLayout) |> ignore

    tagged |> List.map (fun t -> t.Region.Tag, t.Tag) |> List.sort
    |> should equal (List.sort [ "H1", "H1"; "P", "P"; "LI", "LBody"; "LI", "LBody"; "Figure", "Figure"; "P", "P" ])
    // What the figure holds is the image, inside its looser region.
    let figure = tagged |> List.find (fun t -> t.Tag = "Figure")
    let box = figure.Box |> notNull
    (box.Left, box.Bottom, box.Right, box.Top) |> should equal (72.0, 450.0, 172.0, 550.0)

[<Fact>]
let ``tagging from regions with a review writes its alt text and title`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntagged input.Path
    let review: ReviewFile.SavedFile =
        { Version = 1
          DismissedWarnings = null
          Edits = [| { Page = 1; Tag = "Figure"; Box = [| 70.0; 448.0; 175.0; 552.0 |]; Alt = "Red, green and blue squares"; Text = null; MergeWith = null; Retag = null; Exclude = false } |]
          Title = "Squares"
          Layout = null
          Fixes = null }

    VeraPdfValidator(LayoutSource = ReviewFile.ReviewedRegions(FixedLayout regions, review))
        .Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.TagFromLayout)
    |> ignore

    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    flatten root |> List.find (fun n -> n.Tag = "Figure") |> _.AlternateDescription |> should equal "Red, green and blue squares"
    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetDocumentInfo().GetTitle() |> should equal "Squares"


/// The untagged page as a PDF 2.0 file, its text in an embedded font (PDF/UA-2 wants every font
/// embedded), and enough of it for its language to be told.
let private createUntaggedPdf20 (path: string) =
    use document = new PdfDocument(new PdfWriter(path, WriterProperties().SetPdfVersion(PdfVersion.PDF_2_0)))
    let page = document.AddNewPage PageSize.LETTER
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont(corpusFile "fonts/Lato-Regular.ttf", iText.IO.Font.PdfEncodings.IDENTITY_H, PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED)
    let text (x: float) (y: float) (size: float32) (value: string) =
        canvas.BeginText().SetFontAndSize(font, size).MoveText(x, y).ShowText(value).EndText() |> ignore
    text 72.0 750.0 10f "Running header"
    text 72.0 700.0 20f "The Title"
    text 72.0 660.0 12f "An opening paragraph, written in plain English so its language can be told."
    text 90.0 630.0 12f "• First item"
    text 90.0 612.0 12f "• Second item"
    let pixels = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 255uy; 255uy; 0uy |]
    canvas.AddImageWithTransformationMatrix(ImageDataFactory.Create(2, 2, 3, 8, pixels, null), 100f, 0f, 0f, 100f, 72f, 450f, true) |> ignore
    text 72.0 400.0 12f "Visit the site."
    let link = PdfLinkAnnotation(Rectangle(72f, 395f, 80f, 15f))
    link.SetAction(PdfAction.CreateURI "https://example.com") |> ignore
    page.AddAnnotation link |> ignore
    text 300.0 40.0 10f "Page 1"

let private reviewedRegions = regions |> List.map (fun r -> if r.Tag = "Figure" then { r with Alt = "Four coloured squares" } else r)

let private failedUa2Rules (path: string) =
    let report = VeraPdfValidator().Validate(path, PdfProfile.PdfUa2)
    report.FlavourId |> should equal "ua2"
    report.FailedAssertions |> Seq.map (fun f -> $"{f.Specification} {f.RuleId}") |> List.ofSeq

[<Fact>]
let ``a PDF 2.0 file tagged from its layout, with the safe fixes, is PDF/UA-2 compliant`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createUntaggedPdf20 input.Path
    let validator = VeraPdfValidator(LayoutSource = FixedLayout(reviewedRegions, "The example site", "The Title"))

    let result = validator.Fix(input.Path, PdfProfile.PdfUa2, output.Path, RepairNames.safe ||| PdfFixOptions.TagFromLayout ||| PdfFixOptions.LanguageFromText)

    failedUa2Rules output.Path |> should be Empty
    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetPdfVersion() |> should equal PdfVersion.PDF_2_0
    // Its Document is in PDF 2.0's namespace, as PDF/UA-2 (8.2.5.2) asks.
    let documentElement = document.GetStructTreeRoot().GetKids() |> Seq.exactlyOne :?> iText.Kernel.Pdf.Tagging.PdfStructElem
    documentElement.GetRole() |> should equal PdfName.Document
    documentElement.GetPdfObject().GetAsDictionary(PdfName.NS).GetAsString(PdfName.NS).GetValue() |> should equal "http://iso.org/pdf2/ssn"
    // It claims PDF/UA-2, of 2024 (veraPDF writes no year), and says so.
    let xmp = iText.Kernel.XMP.XMPMetaFactory.ParseFromBuffer(document.GetXmpMetadataBytes())
    xmp.GetPropertyString(iText.Kernel.XMP.XMPConst.NS_PDFUA_ID, "part") |> should equal "2"
    xmp.GetPropertyString(iText.Kernel.XMP.XMPConst.NS_PDFUA_ID, "rev") |> should equal "2024"
    result.AppliedFixes |> Seq.filter (fun fix -> fix.Contains "'rev'") |> Seq.toList |> should equal [ "Added property 'rev' with value '2024' to Identification schema" ]

[<Fact>]
let ``on a tagged PDF 2.0 file, lists and bookmarks keep it PDF/UA-2 compliant, bookmarks pointing to the structure`` () =
    use input = new TempPdf()
    use tagged = new TempPdf()
    use output = new TempPdf()
    createUntaggedPdf20 input.Path
    // The bullets tagged as paragraphs, for the list fix to find.
    let bulletsAsParagraphs = reviewedRegions |> List.map (fun r -> if r.Tag = "LI" then { r with Tag = "P"; ListNumbering = null } else r)
    VeraPdfValidator(LayoutSource = FixedLayout(bulletsAsParagraphs, "The example site", "The Title"))
        .Fix(input.Path, PdfProfile.PdfUa2, tagged.Path, PdfFixOptions.TagFromLayout) |> ignore

    let result =
        VeraPdfValidator().Fix(tagged.Path, PdfProfile.PdfUa2, output.Path,
                               RepairNames.safe ||| PdfFixOptions.LanguageFromText ||| PdfFixOptions.ListsFromParagraphs ||| PdfFixOptions.BookmarksFromHeadings)

    result.AppliedFixes |> should contain "Tagged 1 run(s) of marked paragraphs as lists."
    result.AppliedFixes |> should contain "Added 1 bookmark(s) for the H1 and H2 headings."
    failedUa2Rules output.Path |> should be Empty
    // The bookmark goes to the heading's element (PDF/UA-2 8.8), and its page for older readers.
    use document = new PdfDocument(new PdfReader(output.Path))
    let bookmark = document.GetOutlines(false).GetAllChildren() |> Seq.exactlyOne
    let action = bookmark.GetContent().GetAsDictionary(PdfName.A)
    action.GetAsName(PdfName.S) |> should equal PdfName.GoTo
    (action.GetAsArray(PdfName.SD).GetAsDictionary 0).GetAsName(PdfName.S) |> should equal PdfName.H1
    action.GetAsArray(PdfName.D) |> should not' (be Null)

[<Fact>]
let ``QR code links on a PDF 2.0 file are PDF/UA-2 compliant, with alt text a person wrote or not`` () =
    // PDF/UA-2 (8.9.4.2) wants a link's description and its element's alt text to agree.
    for alt in [ null; "Scan to apply" ] do
        use input = new TempPdf()
        use output = new TempPdf()
        createUntaggedPdf20 input.Path
        let qr = reviewedRegions |> List.map (fun r -> if r.Tag = "Figure" then { r with Alt = alt; QrText = "https://example.com/apply" } else r)
        let result =
            VeraPdfValidator(LayoutSource = FixedLayout(qr, "The example site", "The Title"))
                .Fix(input.Path, PdfProfile.PdfUa2, output.Path, RepairNames.safe ||| PdfFixOptions.TagFromLayout ||| PdfFixOptions.LanguageFromText ||| PdfFixOptions.QrCodeLinks)
        result.AppliedFixes |> should contain "Linked 1 QR code to the web address they encode"
        failedUa2Rules output.Path |> should be Empty
