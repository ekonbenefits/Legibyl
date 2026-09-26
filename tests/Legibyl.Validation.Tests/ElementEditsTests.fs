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
    { Page = 1; Tag = "Figure"; Box = box (73.0, 551.0, 171.0, 649.0); Alt = "Tomato seedlings in a tray"; Text = null }

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

    result.AppliedFixes |> should contain "Applied 1 of your edits (alt text and replacement text)."
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

    result.AppliedFixes
    |> should
        contain
        "Applied 1 of your edits (alt text and replacement text); 1 weren't, as no element with their tag is in about the same place."
    ElementEdits.pending input.Path [ elsewhere ] |> should equal 0
