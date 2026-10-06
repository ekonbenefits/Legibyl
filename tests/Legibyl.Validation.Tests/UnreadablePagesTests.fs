module Legibyl.Validation.Tests.UnreadablePagesTests

open System
open System.Text
open iText.Kernel.Pdf
open iText.Layout
open iText.Layout.Element
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// Page `page` of path made unreadable to PdfPig: its text names a font its resources don't have.
let private dropFont (path: string) (page: int) =
    let bytes = IO.File.ReadAllBytes path
    use document = new PdfDocument(new PdfReader(new IO.MemoryStream(bytes)), new PdfWriter(path))
    let pdfPage = document.GetPage page
    let fonts = pdfPage.GetResources().GetResource PdfName.Font |> notNull
    let name = fonts.KeySet() |> Seq.head
    let stream = pdfPage.GetContentStream 0
    let content = Encoding.Latin1.GetString(stream.GetBytes())
    stream.SetData(Encoding.Latin1.GetBytes(content.Replace($"/{name.GetValue()} ", "/Missing "))) |> ignore
    fonts.Remove name |> ignore

/// A tagged PDF of a paragraph on each page, with page 1's font missing.
let private createTagged (path: string) =
    do
        let pdf = new PdfDocument(new PdfWriter(path))
        pdf.SetTagged() |> ignore
        use document = new Document(pdf)
        document.Add(Paragraph "The garden opens at eight.") |> ignore
        document.Add(AreaBreak()) |> ignore
        document.Add(Paragraph "It closes at dusk.") |> ignore
    dropFont path 1

/// An untagged one-page PDF whose text names a font it doesn't have (and no resources at all).
let private createUntagged (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    page.GetContentStream(0).SetData(Encoding.ASCII.GetBytes "BT /Missing 12 Tf 72 700 Td (Hello) Tj ET") |> ignore
    page.GetPdfObject().Remove PdfName.Resources |> ignore

[<Fact>]
let ``A page naming a font it doesn't have is validated, not a crash`` () =
    use pdf = new TempPdf()
    createUntagged pdf.Path

    let report = VeraPdfValidator().Validate(pdf.Path, PdfProfile.PdfUa1)

    report.IsCompliant |> should be False
    report.FailedAssertions |> Seq.map _.RuleId |> should contain "7.1-11"

/// The text of every element in tree, in order.
let rec private texts (node: StructureNode) : string list =
    [ match node.Content with
      | NonNull text -> text
      | Null -> ()
      for child in node.Children do
          yield! texts child ]

[<Fact>]
let ``The structure tree still has the text of the pages that can be read`` () =
    use pdf = new TempPdf()
    createTagged pdf.Path

    let tree = VeraPdfValidator().ReadStructureTree pdf.Path |> notNull

    texts tree |> String.concat " " |> should haveSubstring "It closes at dusk."
    texts tree |> String.concat " " |> should not' (haveSubstring "The garden")

[<Fact>]
let ``Validate warns that a page's text couldn't be read`` () =
    use pdf = new TempPdf()
    createTagged pdf.Path

    let report = VeraPdfValidator().Validate(pdf.Path, PdfProfile.PdfUa1)

    let warning = report.Assertions |> single (fun a -> a.Specification = "Legibyl" && a.TestNumber = 13)
    warning.Message |> should equal "The text of some pages couldn't be read, so Legibyl's checks of what they say skipped them."
    warning.ErrorMessage |> notNull |> should startWith "Page(s) 1: "

[<Fact>]
let ``A paragraph on a page whose text couldn't be read isn't taken for empty`` () =
    use pdf = new TempPdf()
    use output = new TempPdf ".fixed"
    createTagged pdf.Path
    VeraPdfValidator().Validate(pdf.Path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun a -> a.FixOptions.HasFlag PdfFixOptions.EmptyParagraphs)
    |> should be Empty

    let result = VeraPdfValidator().Fix(pdf.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmptyParagraphs)

    result.WasFixed |> should be False
    result.NotApplied |> Seq.filter (fun note -> note.Contains("page(s) 1 couldn't be read", StringComparison.Ordinal)) |> Seq.length |> should equal 1
    // (Not also "nothing in this PDF needed it": what page 1 has is unknown.)
    result.NotApplied |> Seq.exists (fun note -> note.Contains("nothing in this PDF needed it", StringComparison.Ordinal)) |> should be False
