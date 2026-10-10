/// Text a tag owns through marked content nested inside its MCID's, with no MCID of its own: an
/// optional content layer (/OC) inside each paragraph's marked content, as Affinity's PDFs have.
module Legibyl.Validation.Tests.NestedMarkedContentTests

open Xunit
open FsUnit.Xunit
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Layer
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A tagged page of two paragraphs, each drawing its text inside a layer within its marked
/// content (/P <</MCID n>> BDC /OC /oc1 BDC … EMC EMC). The second's layer has an artifact in it
/// too, a page number, which isn't the paragraph's.
let private create (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString "en-US") |> ignore
    let page = document.AddNewPage PageSize.LETTER
    let font = PdfFontFactory.CreateFont(corpusFile "fonts/Lato-Regular.ttf", iText.IO.Font.PdfEncodings.IDENTITY_H, PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED)
    let layer = PdfLayer("Publisher layout", document)
    let pointer = document.GetTagStructureContext().GetAutoTaggingPointer().SetPageForTagging page
    let canvas = PdfCanvas page
    let paragraph (text: string) (y: float) (artifact: string option) =
        pointer.AddTag "P" |> ignore
        canvas.OpenTag(pointer.GetTagReference()).BeginLayer(layer) |> ignore
        canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, y).ShowText(text).EndText() |> ignore
        artifact
        |> Option.iter (fun number ->
            canvas.BeginMarkedContent(PdfName.Artifact).BeginText().SetFontAndSize(font, 12f).MoveText(500.0, y).ShowText(number).EndText().EndMarkedContent()
            |> ignore)
        canvas.EndLayer().CloseTag() |> ignore
        pointer.MoveToParent() |> ignore
    paragraph "The garden opens at eight." 700.0 None
    paragraph "Tickets are sold at the gate." 670.0 (Some "17")

let rec private nodes (node: StructureNode) : StructureNode list =
    node :: (node.Children |> Seq.collect nodes |> List.ofSeq)

let private paragraphs (path: string) =
    VeraPdfValidator().ReadStructureTree path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "P")

[<Fact>]
let ``a paragraph's text in a layer inside its marked content is the paragraph's, with its place on the page`` () =
    use pdf = new TempPdf()
    create pdf.Path

    let found = paragraphs pdf.Path

    found |> List.map _.Content |> should equal [ "The garden opens at eight."; "Tickets are sold at the gate." ]
    found |> List.iter (fun p -> p.Geometry |> should not' (be Null))
    // The artifact in the second's layer isn't its text, nor in its box.
    (found[1].Geometry |> notNull).Right |> should be (lessThan 400.0)

[<Fact>]
let ``paragraphs whose text is in a layer aren't called empty, nor removed as empty`` () =
    use pdf = new TempPdf()
    use output = new TempPdf()
    create pdf.Path

    let warnings =
        VeraPdfValidator().Validate(pdf.Path, PdfProfile.PdfUa1).Assertions
        |> Seq.filter (fun a -> a.Specification = "Legibyl" && a.TestNumber = 1)
    let result = VeraPdfValidator().Fix(pdf.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmptyParagraphs)

    warnings |> should be Empty
    result.WasFixed |> should be False
