module Legibyl.Validation.Tests.SplitFiguresTests

open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Tagging
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private box (left, bottom, right, top) : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

/// A tagged page: a border of twelve small figures side by side (each a 9 × 11 pt coloured
/// shape, as Pages tags a picture drawn in pieces), then two photographs, each a figure.
let private createPdf (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    let page = document.AddNewPage()
    let canvas = PdfCanvas(page)
    let documentElement = document.GetStructTreeRoot().AddKid(PdfStructElem(document, PdfName.Document))
    let figure (draw: PdfCanvas -> unit) =
        let element = documentElement.AddKid(PdfStructElem(document, PdfName.Figure, page))
        let reference = PdfMcrNumber(page, element)
        element.AddKid reference |> ignore
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber(reference.GetMcid())) |> ignore
        canvas.BeginMarkedContent(PdfName.Figure, properties) |> ignore
        draw canvas
        canvas.EndMarkedContent() |> ignore
    for i in 0 .. 11 do
        figure (fun c -> c.SetFillColorRgb(float32 i / 12f, 0.5f, 0.3f).Rectangle(72.0 + float i * 10.0, 60.0, 9.0, 11.0).Fill() |> ignore)
    let pixels = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 255uy; 255uy; 0uy |]
    for x in [ 72f; 320f ] do
        figure (fun c -> c.AddImageWithTransformationMatrix(iText.IO.Image.ImageDataFactory.Create(2, 2, 3, 8, pixels, null), 200f, 0f, 0f, 150f, x, 500f) |> ignore)

let private figuresWithoutAlt (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions |> Seq.filter (fun f -> f.RuleId = "7.3-1") |> Seq.length

let rec private nodes (node: StructureNode) : StructureNode list =
    node :: (node.Children |> Seq.collect nodes |> List.ofSeq)

let private figures (path: string) =
    VeraPdfValidator().ReadStructureTree path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "Figure")

[<Fact>]
let ``small figures side by side are warned about once, pointing to merging them by hand`` () =
    use input = new TempPdf()
    createPdf input.Path

    let warnings =
        VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1).Findings
        |> Seq.filter (fun f -> f.Specification = "Legibyl" && f.TestNumber = 10)
        |> List.ofSeq
    // One run: the photographs are too big to be pieces.
    warnings |> List.length |> should equal 1
    warnings.Head.ErrorMessage |> should equal "12 figures side by side on page 1."
    warnings.Head.Message |> should haveSubstring "In Structure…, shift-click them and Merge them"
    // Which pieces make one picture is a person's call: no auto fix does it.
    warnings.Head.FixOptions |> should equal PdfFixOptions.None
    figuresWithoutAlt input.Path |> should equal 14

[<Fact>]
let ``a merge edit on figures, and alt text on the first, describe the picture once`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf input.Path
    let pieces = [ for i in 0 .. 11 -> box (72.0 + float i * 10.0, 60.0, 81.0 + float i * 10.0, 71.0) ]
    let merge = { Page = 1; Tag = "Figure"; Box = pieces.Head; Alt = null; Text = null; MergeWith = pieces.Tail; Retag = null; Exclude = false; Move = None }
    let alt = { merge with MergeWith = []; Alt = "A row of coloured pencils" }

    VeraPdfValidator(Edits = [ merge; alt ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits) |> ignore

    let border = figures output.Path |> List.minBy (fun f -> (nonNull f.Geometry).Bottom)
    border.AlternateDescription |> should equal "A row of coloured pencils"
    figures output.Path |> List.length |> should equal 3
    figuresWithoutAlt output.Path |> should equal 2

[<Fact>]
let ``a merged picture made not read is decoration as a whole`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf input.Path
    let pieces = [ for i in 0 .. 11 -> box (72.0 + float i * 10.0, 60.0, 81.0 + float i * 10.0, 71.0) ]
    let merge = { Page = 1; Tag = "Figure"; Box = pieces.Head; Alt = null; Text = null; MergeWith = pieces.Tail; Retag = null; Exclude = false; Move = None }
    let notRead = { merge with MergeWith = []; Retag = "Artifact" }

    VeraPdfValidator(Edits = [ merge; notRead ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits) |> ignore

    figures output.Path |> List.length |> should equal 2
    figuresWithoutAlt output.Path |> should equal 2
    VeraPdfValidator().Validate(output.Path, PdfProfile.PdfUa1).FailedAssertions |> Seq.exists (fun f -> f.RuleId = "7.1-3") |> should be False
