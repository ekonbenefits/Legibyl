module Legibyl.Validation.Tests.ElementEditsTests

open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.IO.Image
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private box (left, bottom, right, top) : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

let private region tag bounds : LayoutRegion =
    { Tag = tag; Geometry = box bounds; ListNumbering = null; ActualText = null; QrText = null; Alt = null }

type private FixedLayout(regions: LayoutRegion list) =
    interface IPageLayoutSource with
        member _.PageRegions(_, _) = regions
        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = null

/// A tagged page: a paragraph, then a figure (an image) without alt text.
let private createTagged (path: string) =
    use untagged = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(untagged.Path))
        let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
        canvas.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f).MoveText(72.0, 700.0).ShowText("The garden in spring.").EndText()
        |> ignore
        let pixels = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 255uy; 255uy; 0uy |]
        canvas.AddImageWithTransformationMatrix(ImageDataFactory.Create(2, 2, 3, 8, pixels, null), 100f, 0f, 0f, 100f, 72f, 550f, true)
        |> ignore
    let layout = FixedLayout [ region "P" (70.0, 695.0, 300.0, 715.0); region "Figure" (70.0, 548.0, 175.0, 652.0) ]
    VeraPdfValidator(LayoutSource = layout).Fix(untagged.Path, PdfProfile.PdfUa1, path, PdfFixOptions.TagFromLayout) |> ignore

/// The figure's alt text, as given in View Structure: its box is about where the image is.
let private altEdit =
    { Page = 1; Tag = "Figure"; Box = box (73.0, 551.0, 171.0, 649.0); Alt = "Tomato seedlings in a tray"; Text = null; MergeWith = []; Retag = null; Exclude = false }

let private applyEdits (input: string) (output: string) (edits: ElementEdit list) =
    VeraPdfValidator(Edits = edits).Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.ApplyEdits)

let rec private nodes (node: StructureNode) : StructureNode list =
    node :: (node.Children |> Seq.collect nodes |> List.ofSeq)

let private figureAlt (path: string) =
    VeraPdfValidator().ReadStructureTree path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Figure") |> _.AlternateDescription

[<Fact>]
let ``an edit is written to the element with its tag in about the same place`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTagged input.Path

    let result = applyEdits input.Path output.Path [ altEdit ]

    result.AppliedFixes |> should contain "Applied 1 of your edits (alt text, replacement text, tags, merged paragraphs and decoration not read)."
    figureAlt output.Path |> should equal "Tomato seedlings in a tray"

[<Fact>]
let ``replacement text is written as ActualText`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTagged input.Path

    applyEdits input.Path output.Path [ { altEdit with Tag = "P"; Box = box (72.0, 697.0, 190.0, 711.0); Alt = null; Text = "The garden in spring!" } ]
    |> ignore

    let paragraph = VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "P")
    paragraph.ActualText |> should equal "The garden in spring!"

[<Fact>]
let ``only edits that change something are pending, and applying one again changes nothing`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    use again = new TempPdf()
    createTagged input.Path
    ElementEdits.pending input.Path [ altEdit ] |> should equal 1

    applyEdits input.Path output.Path [ altEdit ] |> ignore

    ElementEdits.pending output.Path [ altEdit ] |> should equal 0
    (applyEdits output.Path again.Path [ altEdit ]).WasFixed |> should be False

[<Fact>]
let ``an edit with no element in about the same place isn't applied, and says so`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTagged input.Path
    let elsewhere = { altEdit with Box = box (300.0, 100.0, 400.0, 200.0); Alt = "Somewhere else" }

    let result = applyEdits input.Path output.Path [ altEdit; elsewhere ]

    result.AppliedFixes |> should contain "Applied 1 of your edits (alt text, replacement text, tags, merged paragraphs and decoration not read)."
    // Each edit that isn't applied is named, with why.
    result.NotApplied
    |> should
        contain
        "Your edit for the <Figure> on page 1 at [300, 100, 400, 200] wasn't applied: no <Figure> is in about that place (its box overlapping at least 60%). Check its page, tag and box against legibyl structure."
    ElementEdits.pending input.Path [ elsewhere ] |> should equal 0

[<Fact>]
let ``an edit the PDF has already, or with nothing to change, says so`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTagged input.Path
    (applyEdits input.Path output.Path [ altEdit ]).WasFixed |> should be True

    ElementEdits.unapplied output.Path [ altEdit ]
    |> should equal [ "Your edit for the <Figure> on page 1 at [73, 551, 171, 649] changed nothing: the PDF says that already." ]

/// Three lines tagged a paragraph each.
let private createLines (path: string) =
    use untagged = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(untagged.Path))
        let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
        let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
        for y, text in [ 700.0, "Plots are assigned each spring,"; 686.0, "and members share the tools"; 672.0, "kept in the shed by the gate." ] do
            canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, y).ShowText(text).EndText() |> ignore
    let layout = FixedLayout [ for y in [ 700.0; 686.0; 672.0 ] -> region "P" (70.0, y - 4.0, 300.0, y + 11.0) ]
    VeraPdfValidator(LayoutSource = layout).Fix(untagged.Path, PdfProfile.PdfUa1, path, PdfFixOptions.TagFromLayout) |> ignore

/// About where a line's paragraph is, as View Structure would give it.
let private line y = box (72.0, y - 2.0, 236.0, y + 12.0)

let private merge (boxes: StructureGeometry list) =
    { Page = 1; Tag = "P"; Box = boxes.Head; Alt = null; Text = null; MergeWith = boxes.Tail; Retag = null; Exclude = false }

let private paragraphTexts (path: string) =
    VeraPdfValidator().ReadStructureTree path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "P") |> List.map (fun n -> (string n.Content).Trim())

[<Fact>]
let ``paragraphs chosen to merge are joined into the first`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createLines input.Path
    let edit = merge [ line 700.0; line 686.0; line 672.0 ]
    ElementEdits.pending input.Path [ edit ] |> should equal 1

    let result = applyEdits input.Path output.Path [ edit ]

    result.AppliedFixes |> should contain "Applied 1 of your edits (alt text, replacement text, tags, merged paragraphs and decoration not read)."
    paragraphTexts output.Path |> List.map (fun t -> t.Replace("  ", " "))
    |> should equal [ "Plots are assigned each spring, and members share the tools kept in the shed by the gate." ]
    ElementEdits.pending output.Path [ edit ] |> should equal 0

[<Fact>]
let ``paragraphs that aren't next to each other aren't merged`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createLines input.Path
    let edit = merge [ line 700.0; line 672.0 ]

    ElementEdits.pending input.Path [ edit ] |> should equal 0
    (applyEdits input.Path output.Path [ edit ]).WasFixed |> should be False

[<Fact>]
let ``a retag gives an element another tag, and then counts as in the PDF`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTagged input.Path
    let retag = { altEdit with Tag = "P"; Box = box (72.0, 697.0, 190.0, 711.0); Alt = null; Retag = "H1" }
    ElementEdits.pending input.Path [ retag ] |> should equal 1

    applyEdits input.Path output.Path [ retag ] |> ignore

    VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes |> List.map _.Tag |> should contain "H1"
    ElementEdits.pending output.Path [ retag ] |> should equal 0

let private suggested tag (left, bottom, right, top) actualText numbering : LayoutRegion =
    { Tag = tag
      Geometry = box (left, bottom, right, top)
      ListNumbering = numbering
      ActualText = actualText
      QrText = null
      Alt = null }

[<Fact>]
let ``edits on suggested regions leave them out, retag them, and give them alt text and corrected text`` () =
    let heading = suggested "H" (70.0, 700.0, 300.0, 720.0) null null
    let item = suggested "LI" (70.0, 680.0, 300.0, 695.0) null "Disc"
    let figure = suggested "P" (70.0, 500.0, 200.0, 600.0) null null
    let scanned = suggested "P" (70.0, 450.0, 300.0, 470.0) "Campouts, cavlng" null
    let footer = suggested "P" (70.0, 30.0, 300.0, 45.0) null null
    let on (region: LayoutRegion) : ElementEdit = { altEdit with Tag = region.Tag; Box = region.Geometry; Alt = null }
    let edits =
        [ { on heading with Retag = "H1" }
          { on item with Retag = "P" }
          // Retagged as a figure, so its alt text counts.
          { on figure with Retag = "Figure"; Alt = "A map of the campsite" }
          { on scanned with Text = "Campouts, caving" }
          { on footer with Exclude = true } ]

    let regions = ElementEdits.onRegions edits [ heading; item; figure; scanned; footer ]

    regions |> List.map (fun r -> r.Tag, Option.ofObj r.ListNumbering, Option.ofObj r.Alt, Option.ofObj r.ActualText)
    |> should
        equal
        ([ "H1", None, None, None
           "P", None, None, None
           "Figure", None, Some "A map of the campsite", None
           "P", None, None, Some "Campouts, caving" ]
         : (string * string option * string option * string option) list)

[<Fact>]
let ``edits move to the elements tagging made, retags becoming their tag and exclusions going`` () =
    let region = suggested "H" (70.0, 700.0, 300.0, 720.0) null null
    let figure = suggested "Figure" (70.0, 500.0, 200.0, 600.0) null null
    let written tag (source: LayoutRegion) (box': StructureGeometry) : TaggedRegion = { Region = source; Tag = tag; Box = box' }
    let tagged =
        [ // Tagging had the edits applied: the heading as H1.
          written "H1" { region with Tag = "H1" } (box (72.0, 702.0, 250.0, 716.0))
          written "Figure" { figure with Alt = "A map" } (box (72.0, 502.0, 198.0, 598.0)) ]
    let edits: ElementEdit list =
        [ { altEdit with Tag = "H"; Box = region.Geometry; Alt = null; Retag = "H1" }
          { altEdit with Tag = "Figure"; Box = figure.Geometry; Alt = "A map" }
          { altEdit with Tag = "P"; Box = box (70.0, 30.0, 300.0, 45.0); Alt = null; Exclude = true } ]

    ReviewFile.rekey edits tagged |> List.map (fun e -> e.Tag, Option.ofObj e.Alt, Option.ofObj e.Retag, e.Box.Left)
    |> should equal ([ "Figure", Some "A map", None, 72.0 ] : (string * string option * string option * float) list)

/// A tagged page with a sentence and a link over it that has no description.
let private createLinked (path: string) =
    use untagged = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(untagged.Path))
        let page = document.AddNewPage PageSize.LETTER
        PdfCanvas(page).BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f).MoveText(72.0, 700.0).ShowText("Visit the site.").EndText()
        |> ignore
        let link = iText.Kernel.Pdf.Annot.PdfLinkAnnotation(Rectangle(72f, 695f, 80f, 15f))
        link.SetAction(iText.Kernel.Pdf.Action.PdfAction.CreateURI "https://example.com") |> ignore
        page.AddAnnotation link |> ignore
    VeraPdfValidator(LayoutSource = FixedLayout [ region "P" (70.0, 690.0, 300.0, 715.0) ]).Fix(untagged.Path, PdfProfile.PdfUa1, path, PdfFixOptions.TagFromLayout)
    |> ignore

[<Fact>]
let ``a link's description is its annotation's, read and written from an edit`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createLinked input.Path
    let link (path: string) = VeraPdfValidator().ReadStructureTree path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Link")
    let before = link input.Path
    before.LinkDescription |> should be Null
    let described = { altEdit with Tag = "Link"; Box = before.Geometry |> notNull; Alt = "Example (example.com)" }
    ElementEdits.pending input.Path [ described ] |> should equal 1

    applyEdits input.Path output.Path [ described ] |> ignore

    let after = link output.Path
    after.LinkDescription |> should equal "Example (example.com)"
    // On the annotation, not as the element's alt text.
    after.AlternateDescription |> should be Null
    ElementEdits.pending output.Path [ described ] |> should equal 0

/// Making the figure decoration: Not Read (Artifact) in View Structure.
let private notReadEdit = { altEdit with Alt = null; Retag = "Artifact" }

let private render (path: string) =
    use document = new Legibyl.Rendering.PdfiumDocument(path)
    use page = document.Render(1, 1f)
    page.Pixels |> Array.map (fun c -> c.Red, c.Green, c.Blue, c.Alpha)

[<Fact>]
let ``a figure made not read leaves the tags, and its content is an artifact, still drawn`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTagged input.Path
    let failedBefore = VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq
    failedBefore |> should contain "7.3-1"

    let result = applyEdits input.Path output.Path [ notReadEdit ]

    result.AppliedFixes |> should contain "Applied 1 of your edits (alt text, replacement text, tags, merged paragraphs and decoration not read)."
    let tags = VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes |> List.map _.Tag
    tags |> should not' (contain "Figure")
    tags |> should contain "P"
    let failed = VeraPdfValidator().Validate(output.Path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq
    // No alt text needed; its content isn't untagged; the paragraph is still found through the parent tree.
    failed |> should not' (contain "7.3-1")
    failed |> should not' (contain "7.1-3")
    render output.Path |> should equal (render input.Path)
    // Once written, the edit says it's done.
    ElementEdits.unapplied output.Path [ notReadEdit ] |> List.exactlyOne |> should haveSubstring "it may be decoration already"
    ElementEdits.pending input.Path [ notReadEdit ] |> should equal 1
