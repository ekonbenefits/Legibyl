/// title-from-first-page (TitleFromFirstPage): a document with no title anywhere is titled from a
/// short first page followed by more; a title it has is never replaced.
module Legibyl.Validation.Tests.TitleFromFirstPageTests

open System.IO
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A document whose first page has lines, then more pages; titled in Info if title is given.
let private create (path: string) (lines: string list) (morePages: int) (title: string option) =
    use document = new PdfDocument(new PdfWriter(path))
    let canvas = PdfCanvas(document.AddNewPage())
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    lines |> List.iteri (fun i text -> canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 700.0 - 20.0 * float i).ShowText(text).EndText() |> ignore)
    for page in 1 .. morePages do
        PdfCanvas(document.AddNewPage()).BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 700.0).ShowText($"Page {page + 1} of the plan.").EndText() |> ignore
    title |> Option.iter (fun title -> document.GetDocumentInfo().SetTitle title |> ignore)

/// Also asks readers to show the document's title (/DisplayDocTitle), as the title fix does
/// whether or not there's a title: then only a title would be left to give it.
let private createShown (path: string) (lines: string list) (morePages: int) =
    create path lines morePages None
    let bytes = File.ReadAllBytes path
    use document = new PdfDocument(new PdfReader(new MemoryStream(bytes)), new PdfWriter(path))
    document.GetCatalog().SetViewerPreferences(PdfViewerPreferences().SetDisplayDocTitle true) |> ignore

let private infoTitle (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    document.GetDocumentInfo().GetTitle()

let private titleRules (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions
    |> Seq.filter (fun a -> a.FixOptions.HasFlag PdfFixOptions.Title)
    |> Seq.map _.RuleId
    |> Set.ofSeq

let private fix (input: string) (output: string) (options: PdfFixOptions) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, options)

[<Fact>]
let ``an untitled document is titled from its title page, shown in the title bar, and its title failures go`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    create input.Path [ "Retirement Plan"; "Summary of Benefits" ] 2 None

    let result = fix input.Path output.Path (PdfFixOptions.TitleFromFirstPage ||| PdfFixOptions.Metadata)

    result.WasFixed |> should be True
    infoTitle output.Path |> should equal "Retirement Plan Summary of Benefits"
    titleRules input.Path |> should not' (be Empty)
    titleRules output.Path |> should be Empty

[<Fact>]
let ``a title the document has in Info is kept, never replaced by its first page's`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    create input.Path [ "Cover" ] 2 (Some "Retirement Plan Summary")

    fix input.Path output.Path (PdfFixOptions.TitleFromFirstPage ||| PdfFixOptions.Metadata) |> ignore

    infoTitle output.Path |> should equal "Retirement Plan Summary"

[<Fact>]
let ``a document that's only one page has nothing to title from, no copy is written, and it says why`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createShown input.Path [ "Retirement Plan" ] 0

    let result = fix input.Path output.Path PdfFixOptions.TitleFromFirstPage

    result.WasFixed |> should be False
    File.Exists output.Path |> should be False
    List.ofSeq result.NotApplied |> should equal [ "Title an untitled document from its title page's text: nothing in this PDF needed it." ]

[<Fact>]
let ``a blank first page has no text to title from`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createShown input.Path [] 2

    (fix input.Path output.Path PdfFixOptions.TitleFromFirstPage).WasFixed |> should be False

[<Fact>]
let ``with the safe fixes, it titles the document and the metadata carries the title`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    create input.Path [ "Retirement Plan" ] 1 None

    fix input.Path output.Path (FixNames.safe ||| PdfFixOptions.TitleFromFirstPage) |> ignore

    infoTitle output.Path |> should equal "Retirement Plan"
    use document = new PdfDocument(new PdfReader(output.Path))
    let xmp = iText.Kernel.XMP.XMPMetaFactory.ParseFromBuffer(document.GetXmpMetadataBytes())
    xmp.GetLocalizedText(iText.Kernel.XMP.XMPConst.NS_DC, "title", "", "x-default").GetValue() |> should equal "Retirement Plan"

