module Legibyl.Validation.Tests.StructureTreeReaderTests

open System
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private reader: IStructureTreeReader = VeraPdfValidator()

let private numbers (values: float list) =
    let array = PdfArray()
    values |> List.iter (fun v -> array.Add(PdfNumber v))
    array

let private structElem (tag: PdfName) (page: PdfPage) =
    let element = PdfDictionary()
    element.Put(PdfName.Type, PdfName.StructElem) |> ignore
    element.Put(PdfName.S, tag) |> ignore
    element.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
    element

let private setStructTreeRoot (document: PdfDocument) (kids: PdfObject) =
    let structTreeRoot = PdfDictionary()
    structTreeRoot.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    structTreeRoot.Put(PdfName.K, kids) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, structTreeRoot) |> ignore

/// Adds a link annotation on page 1 pointing at page 2 and returns an /OBJR referencing it.
let private addInternalLinkAnnotation (document: PdfDocument) (page1: PdfPage) (page2: PdfPage) =
    let destination = PdfArray()
    destination.Add(page2.GetPdfObject())
    destination.Add(PdfName.XYZ)
    destination.Add(PdfNumber 33)
    destination.Add(PdfNumber 690)
    destination.Add(PdfNumber 0)

    let annotation = PdfDictionary()
    annotation.Put(PdfName.Type, PdfName.Annot) |> ignore
    annotation.Put(PdfName.Subtype, PdfName.Link) |> ignore
    annotation.Put(PdfName.Rect, numbers [ 10.0; 10.0; 100.0; 30.0 ]) |> ignore
    annotation.Put(PdfName.Dest, destination) |> ignore
    annotation.MakeIndirect document |> ignore

    let annotations = PdfArray()
    annotations.Add annotation
    page1.GetPdfObject().Put(PdfName.Annots, annotations) |> ignore

    let objr = PdfDictionary()
    objr.Put(PdfName.Type, PdfName.OBJR) |> ignore
    objr.Put(PdfName.Obj, annotation.GetIndirectReference()) |> ignore
    objr

let private createPdfWithTaggedInternalLink (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page1 = document.AddNewPage()
    let page2 = document.AddNewPage()

    let link = structElem PdfName.Link page1
    link.Put(PdfName.K, addInternalLinkAnnotation document page1 page2) |> ignore

    setStructTreeRoot document link

let private createPdfWithTaggedTocItemLink (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page1 = document.AddNewPage()
    let page2 = document.AddNewPage()

    let visibleLink = structElem PdfName.Link page1
    let targetLink = structElem PdfName.Link page1
    targetLink.Put(PdfName.K, addInternalLinkAnnotation document page1 page2) |> ignore

    let tocItemChildren = PdfArray()
    tocItemChildren.Add visibleLink
    tocItemChildren.Add targetLink

    let tocItem = structElem (PdfName "TOCI") page1
    tocItem.Put(PdfName.K, tocItemChildren) |> ignore

    setStructTreeRoot document tocItem

/// A /P with no /Pg of its own whose text, on page 2, it references through a marked-content
/// reference (/MCR) that carries the page instead, as many PDF writers do.
let private createPdfWithParagraphReferencingContentByMcr (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.AddNewPage() |> ignore
    let page2 = document.AddNewPage()

    let properties = PdfDictionary()
    properties.Put(PdfName.MCID, PdfNumber 0) |> ignore
    PdfCanvas(page2)
        .BeginMarkedContent(PdfName.P, properties)
        .BeginText()
        .SetFontAndSize(PdfFontFactory.CreateFont(StandardFonts.HELVETICA), 12f)
        .MoveText(72.0, 700.0)
        .ShowText("Text on the second page")
        .EndText()
        .EndMarkedContent()
    |> ignore

    let mcr = PdfDictionary()
    mcr.Put(PdfName.Type, PdfName.MCR) |> ignore
    mcr.Put(PdfName.Pg, page2.GetPdfObject()) |> ignore
    mcr.Put(PdfName.MCID, PdfNumber 0) |> ignore

    let paragraph = PdfDictionary()
    paragraph.Put(PdfName.Type, PdfName.StructElem) |> ignore
    paragraph.Put(PdfName.S, PdfName.P) |> ignore
    paragraph.Put(PdfName.K, mcr) |> ignore

    setStructTreeRoot document paragraph

[<Fact>]
let ``ReadStructureTree tagged document returns expected hierarchy`` () =
    let root = reader.ReadStructureTree(corpusFile "pdfua1-pass.pdf") |> notNull

    root.Tag |> should equal "StructTreeRoot"
    let document = root.Children |> single (fun _ -> true)
    document.Tag |> should equal "Document"
    let heading = document.Children |> single (fun n -> n.Tag = "H1")
    heading.Language |> should equal "EN-US"
    heading.PageNumber |> should equal (Nullable 1)
    heading.Content |> should equal "The graphics objects in a document can be divided into two classes:"
    let geometry = heading.Geometry |> notNull
    geometry.PageNumber |> should equal 1
    geometry.Right |> should be (greaterThan geometry.Left)
    geometry.Top |> should be (greaterThan geometry.Bottom)
    document.Children |> Seq.map (fun n -> n.Tag) |> should contain "L"

    let note = document.Children |> single (fun n -> n.Tag = "Note")
    let noteText = note.Children |> single (fun n -> n.Tag = "Span")
    noteText.Content |> should startWith "Artifacts may also be used to describe areas of the document"

[<Fact>]
let ``ReadStructureTree missing file throws VeraPdfValidationException`` () =
    (fun () -> reader.ReadStructureTree(corpusFile "does-not-exist.pdf") |> ignore)
    |> should throw typeof<VeraPdfValidationException>

[<Fact>]
let ``ReadStructureTree link annotation reports internal destination`` () =
    use input = new TempPdf()
    createPdfWithTaggedInternalLink input.Path

    let root = reader.ReadStructureTree input.Path |> notNull

    let link = root.Children |> single (fun n -> n.Tag = "Link")
    link.LinkUri |> should be Null
    link.LinkDestinationPageNumber |> should equal (Nullable 2)
    link.LinkDestinationDescription |> should haveSubstring "page 2"

[<Fact>]
let ``ReadStructureTree TOC item with child link destination reports destination on TOC item`` () =
    use input = new TempPdf()
    createPdfWithTaggedTocItemLink input.Path

    let root = reader.ReadStructureTree input.Path |> notNull

    let tocItem = root.Children |> single (fun n -> n.Tag = "TOCI")
    tocItem.LinkDestinationPageNumber |> should equal (Nullable 2)
    tocItem.LinkDestinationDescription |> should haveSubstring "page 2"

[<Fact>]
let ``ReadStructureTree element without its own page takes the page of its marked content`` () =
    use input = new TempPdf()
    createPdfWithParagraphReferencingContentByMcr input.Path

    let root = reader.ReadStructureTree input.Path |> notNull

    let paragraph = root.Children |> single (fun n -> n.Tag = "P")
    paragraph.PageNumber |> should equal (Nullable 2)
    paragraph.Content |> should equal "Text on the second page"
