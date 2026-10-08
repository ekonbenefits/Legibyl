module Legibyl.Validation.Tests.UntaggedContentTests

open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Tagging
open Xunit
open FsUnit.Xunit
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A tagged page with, outside every tag: a full-page background fill before the first tag, a
/// rule between two tagged paragraphs (in q … Q), a border along the foot of the page drawn in
/// twenty short strokes, an image, and a line of text.
let private createPdf (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    let page = document.AddNewPage()
    let canvas = PdfCanvas(page)
    let font = PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA
    let documentElement = document.GetStructTreeRoot().AddKid(PdfStructElem(document, PdfName.Document))
    let paragraph (y: float) (text: string) =
        let element = documentElement.AddKid(PdfStructElem(document, PdfName.P, page))
        let reference = PdfMcrNumber(page, element)
        element.AddKid reference |> ignore
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber(reference.GetMcid())) |> ignore
        canvas.BeginMarkedContent(PdfName.P, properties).BeginText().SetFontAndSize(font, 12f).MoveText(72.0, y).ShowText(text).EndText().EndMarkedContent()
        |> ignore
    canvas.SetFillColorRgb(0.95f, 0.95f, 0.9f).Rectangle(0.0, 0.0, 612.0, 792.0).Fill() |> ignore
    paragraph 700.0 "The first section."
    canvas.SaveState().SetStrokeColorRgb(0.5f, 0.5f, 0.5f).SetLineWidth(2f).MoveTo(72.0, 690.0).LineTo(540.0, 690.0).Stroke().RestoreState() |> ignore
    paragraph 670.0 "The second section."
    for i in 0 .. 19 do
        canvas.SetStrokeColorRgb(float32 i / 20f, 0.3f, 0.6f).MoveTo(72.0 + float i * 20.0, 40.0).LineTo(88.0 + float i * 20.0, 50.0).Stroke() |> ignore
    let pixels = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 255uy; 255uy; 0uy |]
    canvas.AddImageWithTransformationMatrix(iText.IO.Image.ImageDataFactory.Create(2, 2, 3, 8, pixels, null), 60f, 0f, 0f, 60f, 72f, 560f) |> ignore
    canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 500.0).ShowText("Text outside every tag.").EndText() |> ignore

let rec private nodes (node: StructureNode) : StructureNode list =
    node :: (node.Children |> Seq.collect nodes |> List.ofSeq)

/// The pieces outside the tags, as the structure shows them.
let private outside (path: string) =
    VeraPdfValidator().ReadStructureTree path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "Untagged")

let private untagged (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions |> Seq.filter (fun f -> f.RuleId = "7.1-3") |> List.ofSeq

let private render (path: string) =
    use document = new PdfiumDocument(path)
    use page = document.Render(1, 1f)
    page.Pixels |> Array.map (fun c -> c.Red, c.Green, c.Blue, c.Alpha)

let private edit (piece: StructureNode) (retag: string) (alt: string | null) =
    { Page = 1; Tag = ElementEdits.UntaggedTag; Box = nonNull piece.Geometry; Alt = alt; Text = null; MergeWith = []; Retag = retag; Exclude = false; Move = None; Cells = [] }

let private applyEdits (input: string) (output: string) (edits: ElementEdit list) =
    VeraPdfValidator(Edits = edits).Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.ApplyEdits)

[<Fact>]
let ``what a tagged page draws outside its tags shows with its structure, piece by piece`` () =
    use pdf = new TempPdf()
    createPdf pdf.Path

    let root = VeraPdfValidator().ReadStructureTree pdf.Path |> notNull
    let group = root.Children |> Seq.find (fun n -> n.Tag = "NotInTags")
    let pieces = group.Children |> Seq.map (fun n -> string n.Content) |> List.ofSeq

    string group.Content |> should equal "Page 1: not in the tags"
    pieces
    |> should
        equal
        [ "a fill, 612 × 792 pt (the whole page)"
          // A hairline's box is at least 2 pt high.
          "a line, 468 × 2 pt"
          // Twenty strokes, one border.
          "a line, 396 × 10 pt"
          "an image, 60 × 60 pt"
          "text \"Text outside every tag.\"" ]

[<Fact>]
let ``its failures are placed on the page, where it's drawn`` () =
    use pdf = new TempPdf()
    createPdf pdf.Path
    let failures = untagged pdf.Path
    failures |> should not' (be Empty)
    for finding in failures do
        finding.Geometry |> should not' (be Null)
    // The background's is the whole page.
    failures |> List.exists (fun f -> let g = nonNull f.Geometry in g.Right - g.Left > 600.0) |> should be True

[<Fact>]
let ``chosen piece by piece: decoration isn't read, a figure and a paragraph are tagged, and the page looks the same`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf input.Path
    let pieces = outside input.Path
    let byContent (start: string) = pieces |> List.find (fun p -> (string p.Content).StartsWith start)
    let edits =
        [ edit (byContent "a fill") "Artifact" null
          edit (byContent "a line, 468") "Artifact" null
          edit (byContent "a line, 396") "Artifact" null
          edit (byContent "an image") "Figure" "Four coloured squares"
          edit (byContent "text") "P" null ]

    let result = applyEdits input.Path output.Path edits

    result.AppliedFixes |> should contain "Applied 5 of your edits (alt text, replacement text, tags, merges and decoration not read)."
    untagged output.Path |> should be Empty
    outside output.Path |> should be Empty
    render output.Path |> should equal (render input.Path)
    let tags = VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes
    let figure = tags |> List.find (fun n -> n.Tag = "Figure")
    figure.AlternateDescription |> should equal "Four coloured squares"
    tags |> List.exists (fun n -> n.Tag = "P" && string n.Content = "Text outside every tag.") |> should be True
    // The figure goes after the tagged paragraph drawn before it, in reading order.
    let order = tags |> List.filter (fun n -> n.Tag = "P" || n.Tag = "Figure") |> List.map (fun n -> n.Tag, string n.Content)
    order |> List.findIndex (fun (tag, _) -> tag = "Figure") |> should equal 2
    ElementEdits.pending output.Path edits |> should equal 0

[<Fact>]
let ``text is never made decoration: that edit isn't applied, and says why`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf input.Path
    let text = outside input.Path |> List.find (fun p -> (string p.Content).StartsWith "text")
    let edits = [ edit text "Artifact" null ]

    let reasons = ElementEdits.unapplied input.Path edits
    let result = applyEdits input.Path output.Path edits

    reasons |> List.exactlyOne |> should haveSubstring "text isn't hidden"
    ElementEdits.pending input.Path edits |> should equal 0
    // Nothing changed, so no copy.
    result.OutputPath |> should be Null

[<Fact>]
let ``an untagged PDF's structure has no pieces outside the tags: it's tagged from its layout`` () =
    use pdf = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(pdf.Path))
        PdfCanvas(document.AddNewPage()).Rectangle(0.0, 0.0, 100.0, 100.0).Fill() |> ignore
    VeraPdfValidator().ReadStructureTree pdf.Path |> should be Null

[<Fact>]
let ``a Pages export: with its parent tree rebuilt and what's outside its tags marked, nothing is untagged`` () =
    use output = new TempPdf()
    let pdf = corpusFile "pages-clipped-paragraphs.pdf"
    let pieces = outside pdf
    pieces |> should not' (be Empty)
    let edits = [ for piece in pieces -> edit piece "Artifact" null ]

    VeraPdfValidator(Edits = edits).Fix(pdf, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ParentTree ||| PdfFixOptions.ApplyEdits) |> ignore

    untagged output.Path |> should be Empty
    render output.Path |> should equal (render pdf)

[<Fact>]
let ``a PDF's own tags named Untagged and NotInTags are elements, told apart from Legibyl's own nodes by their kind`` () =
    use pdf = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(pdf.Path))
        document.SetTagged() |> ignore
        let page = document.AddNewPage()
        let canvas = PdfCanvas(page)
        let font = PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA
        let root = document.GetStructTreeRoot()
        root.AddRoleMapping("Untagged", "P") |> ignore
        root.AddRoleMapping("NotInTags", "P") |> ignore
        let documentElement = root.AddKid(PdfStructElem(document, PdfName.Document))
        for y, tag in [ 700.0, "Untagged"; 670.0, "NotInTags" ] do
            let element = documentElement.AddKid(PdfStructElem(document, PdfName tag, page))
            let reference = PdfMcrNumber(page, element)
            element.AddKid reference |> ignore
            let properties = PdfDictionary()
            properties.Put(PdfName.MCID, PdfNumber(reference.GetMcid())) |> ignore
            canvas.BeginMarkedContent(PdfName tag, properties).BeginText().SetFontAndSize(font, 12f).MoveText(72.0, y).ShowText(tag).EndText().EndMarkedContent()
            |> ignore
        canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 500.0).ShowText("Text outside every tag.").EndText() |> ignore
    let tree = VeraPdfValidator().ReadStructureTree pdf.Path |> notNull
    tree.Kind |> should equal StructureNodeKind.Root
    let kinds tag = nodes tree |> List.filter (fun n -> n.Tag = tag) |> List.map _.Kind
    kinds "Untagged" |> should equal [ StructureNodeKind.Element; StructureNodeKind.Untagged ]
    kinds "NotInTags" |> should equal [ StructureNodeKind.Element; StructureNodeKind.NotInTags ]
