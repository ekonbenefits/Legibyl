module Legibyl.Validation.Tests.ParagraphTablesTests

open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private element (document: PdfDocument) (tag: string) (page: PdfPage) (children: PdfObject list) =
    let element = PdfDictionary()
    element.Put(PdfName.Type, PdfName.StructElem) |> ignore
    element.Put(PdfName.S, PdfName tag) |> ignore
    element.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
    let kids = PdfArray()
    for child in children do
        match child with
        | :? PdfDictionary as child -> child.Put(PdfName.P, element) |> ignore
        | _ -> ()
        kids.Add child
    element.Put(PdfName.K, kids) |> ignore
    element.MakeIndirect document |> ignore
    element

let private createPdf (path: string) (cells: (float * float * string) list) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA)
    let tagged =
        cells
        |> List.mapi (fun mcid (x, y, text) ->
            let properties = PdfDictionary()
            properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
            canvas
                .BeginMarkedContent(PdfName.P, properties)
                .BeginText()
                .SetFontAndSize(font, 12f)
                .MoveText(x, y)
                .ShowText(text)
                .EndText()
                .EndMarkedContent()
            |> ignore
            element document "P" page [ PdfNumber mcid ] :> PdfObject)
    let root = PdfDictionary()
    root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    root.MakeIndirect document |> ignore
    let body = element document "Document" page tagged
    body.Put(PdfName.P, root) |> ignore
    root.Put(PdfName.K, body) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, root) |> ignore
    let markInfo = PdfDictionary()
    markInfo.Put(PdfName.Marked, PdfBoolean.TRUE) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.MarkInfo, markInfo) |> ignore

let private tableCells =
    [ 72.0, 700.0, "Years of service"
      300.0, 700.0, "Vested percent"
      72.0, 675.0, "1"
      300.0, 675.0, "20%"
      72.0, 650.0, "2"
      300.0, 650.0, "40%" ]

let private warnings (cells: (float * float * string) list) =
    use pdf = new TempPdf()
    createPdf pdf.Path cells
    VeraPdfValidator().Validate(pdf.Path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun assertion -> assertion.Specification = "Legibyl" && assertion.TestNumber = 15)
    |> List.ofSeq

[<Fact>]
let ``repeated aligned paragraph rows are reported for review`` () =
    let warning = warnings tableCells |> List.exactlyOne

    warning.ErrorMessage |> should equal "Page 1: 6 paragraphs in a repeated-row layout."
    warning.FixOptions |> should equal PdfFixOptions.None

[<Fact>]
let ``ordinary single-column paragraphs are not reported as tables`` () =
    warnings
        [ 72.0, 700.0, "The first paragraph of the section."
          72.0, 675.0, "The second paragraph of the section."
          72.0, 650.0, "The third paragraph of the section."
          72.0, 625.0, "The fourth paragraph of the section." ]
    |> should be Empty

[<Fact>]
let ``paragraphs in separate columns without aligned rows are not reported`` () =
    warnings
        [ 72.0, 700.0, "First column, first paragraph."
          72.0, 675.0, "First column, second paragraph."
          300.0, 650.0, "Second column, first paragraph."
          300.0, 625.0, "Second column, second paragraph." ]
    |> should be Empty

[<Fact>]
let ``a left-aligned label column beside a right-aligned value column is reported`` () =
    // The labels share only their left edges, and the values only their right edges.
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA)
    let value (y: float) (text: string) = 400.0 - float (font.GetWidth(text, 12f)), y, text
    warnings
        [ 72.0, 700.0, "Name"
          value 700.0 "Jordan Lee"
          72.0, 675.0, "Date of birth"
          value 675.0 "4/1/90"
          72.0, 650.0, "Plan"
          value 650.0 "Gold PPO plus" ]
    |> List.map _.ErrorMessage
    |> should equal [ "Page 1: 6 paragraphs in a repeated-row layout." ]

[<Fact>]
let ``a sentence tagged as paragraphs along its lines is not reported`` () =
    // Each line's sentence in two paragraphs, the second straight after the first (a date
    // tagged on its own, say): a line of text, not a row of cells.
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA)
    let opening = "The rate is as of "
    let after = 72.0 + float (font.GetWidth(opening, 12f))
    warnings
        [ 72.0, 700.0, opening
          after, 700.0, "September 1, 2026, and"
          72.0, 675.0, opening
          after, 675.0, "October 1, 2026." ]
    |> should be Empty
