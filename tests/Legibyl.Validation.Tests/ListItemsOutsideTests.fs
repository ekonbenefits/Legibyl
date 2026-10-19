module Legibyl.Validation.Tests.ListItemsOutsideTests

open System.IO
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// What's on the page, top to bottom: a paragraph tagged P straight under Document, or a list
/// tagged L > LI > LBody, each item's text in its LBody. Each line is drawn at its left edge and
/// its line (0 at the top, 20 points apart).
type private Entry =
    | Paragraph of left: float * line: int * text: string
    | List of (float * int * string) list

let private element (tag: string) (page: PdfPage) (kids: PdfObject list) =
    let element = PdfDictionary()
    element.Put(PdfName.Type, PdfName.StructElem) |> ignore
    element.Put(PdfName.S, PdfName tag) |> ignore
    element.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
    let array = PdfArray()
    kids |> List.iter array.Add
    element.Put(PdfName.K, array) |> ignore
    element

let private createPdf (path: string) (entries: Entry list) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    let canvas = PdfCanvas(page)
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA)
    let mutable mcid = 0
    let draw (tag: string) (left: float, line: int, text: string) =
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
        canvas
            .BeginMarkedContent(PdfName tag, properties)
            .BeginText()
            .SetFontAndSize(font, 12f)
            .MoveText(left, 700.0 - float line * 20.0)
            .ShowText(text)
            .EndText()
            .EndMarkedContent()
        |> ignore
        mcid <- mcid + 1
        PdfNumber(mcid - 1) :> PdfObject
    let body =
        [ for entry in entries ->
              match entry with
              | Paragraph(left, line, text) -> element "P" page [ draw "P" (left, line, text) ] :> PdfObject
              | List items -> element "L" page [ for item in items -> element "LI" page [ element "LBody" page [ draw "LBody" item ] ] ] ]
    let root = PdfDictionary()
    root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    root.Put(PdfName.K, element "Document" page body) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, root) |> ignore

let private warnings testNumber (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun a -> a.Specification = "Legibyl" && a.TestNumber = testNumber)
    |> List.ofSeq

let private outsideWarnings = warnings 21

/// The document's structure below Document, as nested tags: "L(LI(LBody(P)),...)".
let private shape (path: string) =
    let rec describe (node: StructureNode) =
        if node.Children.Count = 0 then node.Tag
        else $"""{node.Tag}({node.Children |> Seq.map describe |> String.concat ","})"""
    let root = VeraPdfValidator().ReadStructureTree path |> notNull
    (Seq.exactlyOne root.Children).Children |> Seq.map describe |> List.ofSeq

let private fixLists (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.Lists)

let private failedRules (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq

let private bullets =
    List [ 72.0, 2, "• Any benefits above the normal retirement benefit."
           72.0, 3, "• Disability benefits in non-pay status." ]

let private introduction = Paragraph(36.0, 0, "Some benefits aren't guaranteed, including:")

[<Fact>]
let ``a bulleted paragraph just before a list of the same bullets is reported, and moved into it as its first item`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path [ introduction; Paragraph(72.0, 1, "• A benefit increase in its first five years."); bullets ]

    let warning = outsideWarnings input.Path |> List.exactlyOne
    warning.ErrorMessage |> should equal "Page 1: 1 item(s) marked \"•\" just before a list of 2."
    warning.LocationContext |> should equal "StructTreeRoot/K/K[1]"
    warning.FixOptions |> should equal PdfFixOptions.Lists
    // Not a list of its own: one paragraph isn't a run.
    warnings 6 input.Path |> should be Empty

    let result = fixLists input.Path output.Path

    result.AppliedFixes |> should contain "Moved 1 item(s) that start with a list's marker into the list beside them."
    shape output.Path |> should equal [ "P"; "L(LI(LBody(P)),LI(LBody),LI(LBody))" ]
    let first = (VeraPdfValidator().ReadStructureTree output.Path |> notNull).Children[0].Children[1].Children[0]
    first.Children[0].Children[0].Content |> should equal "• A benefit increase in its first five years."
    outsideWarnings output.Path |> should be Empty
    Set.difference (failedRules output.Path) (failedRules input.Path) |> should be Empty

[<Fact>]
let ``moving an item into its list doesn't change how the page looks`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path [ introduction; Paragraph(72.0, 1, "• A benefit increase in its first five years."); bullets ]
    fixLists input.Path output.Path |> ignore
    use renderer = new PdfRegionRenderer()
    let page = { PageNumber = 1; Left = 0.0; Bottom = 0.0; Right = 1.0; Top = 1.0 }

    renderer.RenderPageWithHighlight(output.Path, page).PngBytes
    |> should equal (renderer.RenderPageWithHighlight(input.Path, page).PngBytes)

[<Fact>]
let ``an item's next line, tagged as a paragraph of its own, goes into the item with it`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf
        input.Path
        [ introduction
          Paragraph(72.0, 1, "• A benefit increase until it has been part of the plan")
          Paragraph(80.0, 2, "for sixty full months.")
          List [ 72.0, 3, "• Any benefits above the normal retirement benefit."
                 72.0, 4, "• Disability benefits in non-pay status." ] ]

    (outsideWarnings input.Path |> List.exactlyOne).ErrorMessage
    |> should equal "Page 1: 1 item(s) marked \"•\" just before a list of 2."

    fixLists input.Path output.Path |> ignore

    shape output.Path |> should equal [ "P"; "L(LI(LBody(P,P)),LI(LBody),LI(LBody))" ]

[<Fact>]
let ``a paragraph after an item that ends its sentence isn't taken as its next line, so the item isn't beside the list`` () =
    use input = new TempPdf()
    createPdf
        input.Path
        [ introduction
          Paragraph(72.0, 1, "• A benefit increase in its first five years.")
          Paragraph(80.0, 2, "Some of these are paid in part.")
          List [ 72.0, 3, "• Any benefits above the normal retirement benefit."
                 72.0, 4, "• Disability benefits in non-pay status." ] ]

    outsideWarnings input.Path |> should be Empty

[<Fact>]
let ``bulleted paragraphs after a list go in after its last item, not into a list of their own`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf
        input.Path
        [ introduction
          List [ 72.0, 1, "• Any benefits above the normal retirement benefit."
                 72.0, 2, "• Disability benefits in non-pay status." ]
          Paragraph(72.0, 3, "• Health insurance.")
          Paragraph(72.0, 4, "• Vacation pay.") ]

    (outsideWarnings input.Path |> List.exactlyOne).ErrorMessage
    |> should equal "Page 1: 2 item(s) marked \"•\" just after a list of 2."
    warnings 6 input.Path |> should be Empty

    let result = fixLists input.Path output.Path

    result.AppliedFixes |> should contain "Moved 2 item(s) that start with a list's marker into the list beside them."
    shape output.Path |> should equal [ "P"; "L(LI(LBody),LI(LBody),LI(LBody(P)),LI(LBody(P)))" ]

[<Fact>]
let ``a numbered paragraph goes in where its number does: before the list's first, or after its last`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf
        input.Path
        [ Paragraph(72.0, 0, "1. Find the accrual rate.")
          List [ 72.0, 1, "2. Apply the formula."; 72.0, 2, "3. Add the amounts." ]
          Paragraph(72.0, 3, "4. Multiply by years of service.") ]

    (outsideWarnings input.Path |> List.exactlyOne).ErrorMessage
    |> should equal "Page 1: 2 item(s) marked \"1.\" on both sides of a list of 2."

    fixLists input.Path output.Path |> ignore

    shape output.Path |> should equal [ "L(LI(LBody(P)),LI(LBody),LI(LBody),LI(LBody(P)))" ]

[<Theory>]
[<InlineData("• A benefit at another indent.", 100.0)>]
[<InlineData("◦ A benefit with another bullet.", 72.0)>]
[<InlineData("A benefit with no bullet.", 72.0)>]
let ``a paragraph at another indent, or with another marker or none, is left beside the list`` (text: string, left: float) =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path [ introduction; Paragraph(left, 1, text); bullets ]

    outsideWarnings input.Path |> should be Empty
    (fixLists input.Path output.Path).WasFixed |> should be False
    File.Exists output.Path |> should be False

[<Fact>]
let ``a number that doesn't carry on the list's numbering is left beside it`` () =
    use input = new TempPdf()
    createPdf
        input.Path
        [ List [ 72.0, 0, "1. Find the accrual rate."; 72.0, 1, "2. Apply the formula." ]
          Paragraph(72.0, 2, "5. Multiply by years of service.") ]

    outsideWarnings input.Path |> should be Empty
