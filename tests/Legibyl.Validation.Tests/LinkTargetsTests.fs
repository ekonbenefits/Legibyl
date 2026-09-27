module Legibyl.Validation.Tests.LinkTargetsTests

open System
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A tagged two-page file: page 1 a heading and a paragraph with two links, page 2 three
/// paragraphs. The first link goes to page 2 at the second paragraph ([page /XYZ left top]),
/// the second to page 2 by name, with no position; a bookmark goes to the third paragraph
/// ([page /FitH top]), and the document opens at page 1 ([page /Fit]).
let private createPdf (version: PdfVersion) (path: string) =
    use document = new PdfDocument(new PdfWriter(path, WriterProperties().SetPdfVersion version))
    document.SetTagged() |> ignore
    let font = PdfFontFactory.CreateFont(corpusFile "fonts/Lato-Regular.ttf", iText.IO.Font.PdfEncodings.IDENTITY_H, PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED)
    let pages = [ document.AddNewPage(); document.AddNewPage() ]
    let root = document.GetStructTreeRoot()
    let documentElement = root.AddKid(iText.Kernel.Pdf.Tagging.PdfStructElem(document, PdfName.Document))
    /// Draws text tagged as tag on the page, at y.
    let draw (page: PdfPage) (tag: PdfName) (y: float) (text: string) =
        let element = documentElement.AddKid(iText.Kernel.Pdf.Tagging.PdfStructElem(document, tag, page))
        let reference = iText.Kernel.Pdf.Tagging.PdfMcrNumber(page, element)
        element.AddKid reference |> ignore
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber(reference.GetMcid())) |> ignore
        PdfCanvas(page)
            .BeginMarkedContent(tag, properties)
            .BeginText().SetFontAndSize(font, 12f).MoveText(72.0, y).ShowText(text).EndText()
            .EndMarkedContent()
        |> ignore
        element
    draw pages[0] PdfName.H1 700.0 "Contents" |> ignore
    let paragraph = draw pages[0] PdfName.P 660.0 "See the second section, or the next page."
    draw pages[1] PdfName.P 700.0 "The first section." |> ignore
    draw pages[1] PdfName.P 650.0 "The second section." |> ignore
    draw pages[1] PdfName.P 600.0 "The third section." |> ignore
    let page2 = pages[1].GetPdfObject()
    let link (left: float32) (destination: PdfObject) =
        let annotation = iText.Kernel.Pdf.Annot.PdfLinkAnnotation(iText.Kernel.Geom.Rectangle(left, 658f, 80f, 14f))
        annotation.GetPdfObject().Put(PdfName.Dest, destination) |> ignore
        annotation.SetContents "A section" |> ignore
        pages[0].AddAnnotation(-1, annotation, false) |> ignore
        paragraph.AddKid(iText.Kernel.Pdf.Tagging.PdfObjRef(annotation, paragraph, document.GetNextStructParentIndex())) |> ignore
    link 72f (PdfArray([| page2 :> PdfObject; PdfName.XYZ; PdfNumber 72; PdfNumber 662; PdfNull.PDF_NULL |]))
    document.AddNamedDestination("next-page", PdfArray([| page2 :> PdfObject; PdfName.Fit |]))
    link 200f (PdfString "next-page")
    pages[0].GetPdfObject().Put(PdfName.Tabs, PdfName.S) |> ignore
    document.GetOutlines(false).AddOutline("The third section")
        .AddDestination(iText.Kernel.Pdf.Navigation.PdfExplicitDestination.CreateFitH(pages[1], 612f))
    document.GetCatalog().GetPdfObject().Put(PdfName.OpenAction, PdfArray([| pages[0].GetPdfObject() :> PdfObject; PdfName.Fit |])) |> ignore
    document.GetCatalog().SetLang(PdfString "en") |> ignore
    document.GetCatalog().SetViewerPreferences(PdfViewerPreferences().SetDisplayDocTitle true) |> ignore
    document.GetDocumentInfo().SetTitle "Sections" |> ignore

/// The text drawn for each (page, MCID), as createPdf draws it.
let private texts =
    dict
        [ (1, 0), "Contents"
          (1, 1), "See the second section, or the next page."
          (2, 0), "The first section."
          (2, 1), "The second section."
          (2, 2), "The third section." ]

/// The text of the element a structure destination names.
let private textOf (document: PdfDocument) (structureDestination: PdfArray) =
    let element = structureDestination.GetAsDictionary 0
    let page = document.GetPageNumber(element.GetAsDictionary PdfName.Pg)
    let kids = element.Get PdfName.K
    let mcid =
        match kids with
        | :? PdfArray as array -> array.GetAsNumber(0).IntValue()
        | :? PdfNumber as number -> number.IntValue()
        | _ -> -1
    texts[(page, mcid)]

let private failed (path: string) profile =
    VeraPdfValidator().Validate(path, profile).FailedAssertions |> List.ofSeq

[<Fact>]
let ``in a PDF 2.0 file, links, bookmarks and where it opens point at the tags they land on`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf PdfVersion.PDF_2_0 input.Path

    let before = failed input.Path PdfProfile.PdfUa2 |> List.filter (fun f -> f.Clause = "8.8")
    before |> should not' (be Empty)
    for finding in before do
        finding.FixOptions.HasFlag PdfFixOptions.LinkTargets |> should be True

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa2, output.Path, PdfFixOptions.LinkTargets)

    List.ofSeq result.AppliedFixes
    |> should
        equal
        [ "Pointed 2 link(s) within the document at the tags they go to."
          "Pointed 1 bookmark(s) at the tags they go to."
          "Pointed where the document opens at the tag there." ]
    failed output.Path PdfProfile.PdfUa2 |> List.filter (fun f -> f.Clause = "8.8") |> should be Empty
    use document = new PdfDocument(new PdfReader(output.Path))
    let actionOf (holder: PdfDictionary) =
        let action = holder.GetAsDictionary PdfName.A
        action.GetAsName PdfName.S |> should equal PdfName.GoTo
        // The destination as it was stays, for readers without structure destinations.
        action.Get PdfName.D |> should not' (be Null)
        holder.Get PdfName.Dest |> should be Null
        action.GetAsArray PdfName.SD
    let annotations = document.GetPage(1).GetPdfObject().GetAsArray PdfName.Annots
    // Where each lands: just below its position, or the page's first with none.
    textOf document (actionOf (annotations.GetAsDictionary 0)) |> should equal "The second section."
    (actionOf (annotations.GetAsDictionary 1)).GetAsName 1 |> should equal PdfName.Fit
    textOf document (actionOf (annotations.GetAsDictionary 1)) |> should equal "The first section."
    (annotations.GetAsDictionary 1).GetAsDictionary(PdfName.A).GetAsString(PdfName.D).ToUnicodeString() |> should equal "next-page"
    let bookmark = document.GetOutlines(false).GetAllChildren() |> Seq.exactlyOne
    textOf document (actionOf (bookmark.GetContent())) |> should equal "The third section."
    let opens = document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.OpenAction
    textOf document (opens.GetAsArray PdfName.SD) |> should equal "Contents"

[<Fact>]
let ``a PDF 1.7 file is left as it was: PDF/UA-1 doesn't ask for structure destinations`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf PdfVersion.PDF_1_7 input.Path

    let findings = failed input.Path PdfProfile.PdfUa1
    findings |> List.exists (fun f -> f.FixOptions.HasFlag PdfFixOptions.LinkTargets) |> should be False

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkTargets)

    result.AppliedFixes |> should be Empty
    result.OutputPath |> should be Null
