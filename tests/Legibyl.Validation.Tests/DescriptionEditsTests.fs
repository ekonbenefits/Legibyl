module Legibyl.Validation.Tests.DescriptionEditsTests

open Xunit
open FsUnit.Xunit
open iText.Forms
open iText.Forms.Fields
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Annot
open iText.Kernel.Pdf.Canvas
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A tagged page with a sentence, then what add puts on it.
let private createTagged (path: string) (add: PdfDocument -> PdfPage -> unit) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString "en-US") |> ignore
    let page = document.AddNewPage PageSize.LETTER
    let pointer = document.GetTagStructureContext().GetAutoTaggingPointer().SetPageForTagging page
    pointer.AddTag "P" |> ignore
    PdfCanvas(page)
        .OpenTag(pointer.GetTagReference())
        .BeginText()
        .SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f)
        .MoveText(72.0, 700.0)
        .ShowText("The garden in spring.")
        .EndText()
        .CloseTag()
    |> ignore
    pointer.MoveToParent() |> ignore
    add document page

let rec private nodes (node: StructureNode) : StructureNode list =
    node :: (node.Children |> Seq.collect nodes |> List.ofSeq)

let private element (tag: string) (path: string) =
    VeraPdfValidator().ReadStructureTree path |> notNull |> nodes |> List.find (fun n -> n.Tag = tag)

let private failed (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

let private edit (tag: string) (box: StructureGeometry) (text: string) =
    { Page = 1; Tag = tag; Box = box; Alt = text; Text = null; MergeWith = []; Retag = null; Exclude = false; Move = None }

let private applyEdits (input: string) (output: string) (edits: ElementEdit list) =
    VeraPdfValidator(Edits = edits).Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.ApplyEdits)

[<Fact>]
let ``an annotation's description is its /Contents, read and written from an edit`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // A sticky note without a description.
    createTagged input.Path (fun _ page -> page.AddAnnotation(PdfTextAnnotation(Rectangle(320f, 690f, 20f, 20f))) |> ignore)
    let before = element "Annot" input.Path
    before.LinkDescription |> should be Null
    failed input.Path |> should contain "7.18.1-2"
    let described = edit "Annot" (before.Geometry |> notNull) "Reviewer's note: check the dates"
    ElementEdits.pending input.Path [ described ] |> should equal 1

    applyEdits input.Path output.Path [ described ] |> ignore

    (element "Annot" output.Path).LinkDescription |> should equal "Reviewer's note: check the dates"
    failed output.Path |> should not' (contain "7.18.1-2")
    ElementEdits.pending output.Path [ described ] |> should equal 0

[<Fact>]
let ``a form field's tooltip is its /TU and its widget's /Contents, written from an edit`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTagged input.Path (fun document _ ->
        let form = PdfFormCreator.GetAcroForm(document, true)
        let field = TextFormFieldBuilder(document, "first_name").SetWidgetRectangle(Rectangle(72f, 640f, 200f, 20f)).CreateText()
        form.AddField field)
    let before = element "Form" input.Path
    before.FormFieldName |> should equal "first_name"
    before.FormFieldDescription |> should be Null
    failed input.Path |> should contain "7.18.1-3"

    applyEdits input.Path output.Path [ edit "Form" (before.Geometry |> notNull) "First name" ] |> ignore

    (element "Form" output.Path).FormFieldDescription |> should equal "First name"
    failed output.Path |> should not' (contain "7.18.1-3")
    use document = new PdfDocument(new PdfReader(output.Path))
    let widget = document.GetPage(1).GetAnnotations() |> Seq.find (fun a -> PdfName.Widget.Equals(a.GetSubtype()))
    widget.GetContents().ToUnicodeString() |> should equal "First name"
