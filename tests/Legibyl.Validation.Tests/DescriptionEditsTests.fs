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
    { Page = 1; Tag = tag; Box = box; Alt = text; Text = null; MergeWith = []; Retag = null; Exclude = false; Move = None; Cells = []; Lang = null }

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

/// A tagged page with a text field shown twice: one field (its name on it) with two widgets,
/// each tagged Form.
let private createTwoWidgets (path: string) =
    createTagged path (fun document page ->
        let form = PdfFormCreator.GetAcroForm(document, true)
        let field = TextFormFieldBuilder(document, "first_name").SetWidgetRectangle(Rectangle(72f, 640f, 200f, 20f)).CreateText()
        let second = PdfWidgetAnnotation(Rectangle(72f, 600f, 200f, 20f))
        field.AddKid second |> ignore
        form.AddField field
        page.AddAnnotation second |> ignore)

[<Fact>]
let ``a field shown twice is read from its field, and its tooltip given to both widgets`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTwoWidgets input.Path
    let fields = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "Form")
    // Its name is on the field, not the widgets.
    fields |> List.map _.FormFieldName |> List.distinct |> should equal [ "first_name" ]

    applyEdits input.Path output.Path [ edit "Form" (fields.Head.Geometry |> notNull) "First name" ] |> ignore

    use document = new PdfDocument(new PdfReader(output.Path))
    let widgets = document.GetPage(1).GetAnnotations() |> Seq.filter (fun a -> PdfName.Widget.Equals(a.GetSubtype())) |> List.ofSeq
    widgets.Length |> should be (greaterThanOrEqualTo 2)
    widgets |> List.map (fun w -> w.GetContents() |> Option.ofObj |> Option.map _.ToUnicodeString()) |> List.distinct |> should equal [ Some "First name" ]
    VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "Form") |> List.map _.FormFieldDescription
    |> List.distinct
    |> should equal [ "First name" ]

[<Fact>]
let ``an element of the PDF's own name holding a field gets the field's tooltip`` () =
    use input = new TempPdf()
    use renamed = new TempPdf()
    use output = new TempPdf()
    createTagged input.Path (fun document _ ->
        let form = PdfFormCreator.GetAcroForm(document, true)
        form.AddField(TextFormFieldBuilder(document, "first_name").SetWidgetRectangle(Rectangle(72f, 640f, 200f, 20f)).CreateText()))
    // Its element named Field, mapped to Form.
    do
        use document = new PdfDocument(new PdfReader(input.Path), new PdfWriter(renamed.Path))
        let root = document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot
        let rec find (element: PdfDictionary) : PdfDictionary list =
            let kids =
                match element.Get(PdfName.K, true) with
                | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do match array.Get(i, true) with :? PdfDictionary as d when not (isNull (d.Get PdfName.S)) -> d | _ -> () ]
                | :? PdfDictionary as d when not (isNull (d.Get PdfName.S)) -> [ d ]
                | _ -> []
            (if PdfName.Form.Equals(element.GetAsName PdfName.S) then [ element ] else []) @ List.collect find kids
        for element in find root do
            element.Put(PdfName.S, PdfName "Field") |> ignore
        let map = match root.GetAsDictionary PdfName.RoleMap with null -> PdfDictionary() | m -> m
        map.Put(PdfName "Field", PdfName.Form) |> ignore
        root.Put(PdfName.RoleMap, map) |> ignore
    let field = element "Field" renamed.Path

    applyEdits renamed.Path output.Path [ edit "Field" (field.Geometry |> notNull) "First name" ] |> ignore

    let after = element "Field" output.Path
    after.FormFieldDescription |> should equal "First name"
    after.AlternateDescription |> should be Null
