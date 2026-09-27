module Legibyl.Validation.Tests.EmptyFiguresTests

open iText.IO.Font.Constants
open iText.IO.Image
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Xobject
open Xunit
open FsUnit.Xunit
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// An element (an indirect object, with its parent), as a tagged PDF has them.
let private element (document: PdfDocument) (tag: string) (page: PdfPage) (kids: PdfObject list) : PdfDictionary =
    let element = PdfDictionary()
    element.Put(PdfName.Type, PdfName.StructElem) |> ignore
    element.Put(PdfName.S, PdfName tag) |> ignore
    element.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
    let array = PdfArray()
    for kid in kids do
        match kid with
        | :? PdfDictionary as child -> child.Put(PdfName.P, element) |> ignore
        | _ -> ()
        array.Add kid
    element.Put(PdfName.K, array) |> ignore
    element.MakeIndirect document |> ignore
    element

/// A page with a paragraph and five figures, none with alt text: one around nothing (as some
/// authoring apps write), one around only a clipping path, and an image, a form XObject and a
/// shape, drawn.
let private createPdf (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    let canvas = PdfCanvas page
    let marked (mcid: int) (tag: string) (draw: PdfCanvas -> unit) =
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
        canvas.BeginMarkedContent(PdfName tag, properties) |> ignore
        draw canvas
        canvas.EndMarkedContent() |> ignore
    marked 0 "P" (fun c ->
        c.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f).MoveText(72.0, 700.0).ShowText("The garden in spring.").EndText()
        |> ignore)
    marked 1 "Figure" ignore
    marked 2 "Figure" (fun c -> c.SaveState().Rectangle(72.0, 600.0, 50.0, 50.0).Clip().EndPath().RestoreState() |> ignore)
    let pixels = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 255uy; 255uy; 0uy |]
    marked 3 "Figure" (fun c ->
        c.AddImageWithTransformationMatrix(ImageDataFactory.Create(2, 2, 3, 8, pixels, null), 100f, 0f, 0f, 100f, 72f, 450f, false) |> ignore)
    let form = PdfFormXObject(Rectangle(0f, 0f, 50f, 50f))
    PdfCanvas(form, document).Rectangle(0.0, 0.0, 50.0, 50.0).Fill() |> ignore
    marked 4 "Figure" (fun c -> c.AddXObjectAt(form, 200f, 450f) |> ignore)
    marked 5 "Figure" (fun c -> c.Rectangle(300.0, 450.0, 50.0, 50.0).Fill() |> ignore)

    let root = PdfDictionary()
    root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    root.MakeIndirect document |> ignore
    let tagged = element document "Document" page [ for mcid in 0..5 -> element document (if mcid = 0 then "P" else "Figure") page [ PdfNumber mcid ] ]
    tagged.Put(PdfName.P, root) |> ignore
    root.Put(PdfName.K, tagged) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, root) |> ignore

/// The figures' alt text failures, and whether each is offered an auto fix.
let private noAltFailures (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions
    |> Seq.filter (fun a -> a.RuleId = "7.3-1")
    |> Seq.map (fun a -> a.IsFixable)
    |> List.ofSeq

/// Legibyl's warnings about figures that draw nothing: whether each can be fixed.
let private emptyFigureWarnings (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun a -> a.Specification = "Legibyl" && a.TestNumber = 11)
    |> Seq.map (fun a -> a.FixOptions = PdfFixOptions.RemoveEmptyFigures)
    |> List.ofSeq

let private tags (path: string) =
    let root = VeraPdfValidator().ReadStructureTree path |> notNull
    (Seq.exactlyOne root.Children).Children |> Seq.map _.Tag |> List.ofSeq

[<Fact>]
let ``a figure that draws nothing gets a warning of its own, which removing it fixes`` () =
    use input = new TempPdf()
    createPdf input.Path

    // The two that draw nothing, told apart from the image, form and shape, which need alt text.
    emptyFigureWarnings input.Path |> should equal [ true; true ]
    // veraPDF's failure is the same for all five, and no auto fix gives alt text.
    noAltFailures input.Path |> should equal [ false; false; false; false; false ]

[<Fact>]
let ``the fix removes the figures that draw nothing, and the page looks the same`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.RemoveEmptyFigures)

    result.AppliedFixes |> should contain "Removed 2 figure tag(s) that draw nothing."
    tags output.Path |> should equal [ "P"; "Figure"; "Figure"; "Figure" ]
    noAltFailures output.Path |> should equal [ false; false; false ]
    emptyFigureWarnings output.Path |> should be Empty
    use renderer = new PdfRegionRenderer()
    let page = { PageNumber = 1; Left = 0.0; Bottom = 0.0; Right = 1.0; Top = 1.0 }
    renderer.RenderPageWithHighlight(output.Path, page).PngBytes
    |> should equal (renderer.RenderPageWithHighlight(input.Path, page).PngBytes)

[<Fact>]
let ``a figure that draws nothing but has alt text is left alone`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path
    do
        use document = new PdfDocument(new PdfReader(input.Path), new PdfWriter(output.Path))
        let root = document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot
        let empty = root.GetAsDictionary(PdfName.K).GetAsArray(PdfName.K).GetAsDictionary 1
        empty.Put(PdfName.Alt, PdfString "A spacer") |> ignore
    use fixedAgain = new TempPdf ".again"

    let result = VeraPdfValidator().Fix(output.Path, PdfProfile.PdfUa1, fixedAgain.Path, PdfFixOptions.RemoveEmptyFigures)

    result.AppliedFixes |> should contain "Removed 1 figure tag(s) that draw nothing."
    tags fixedAgain.Path |> should equal [ "P"; "Figure"; "Figure"; "Figure"; "Figure" ]
