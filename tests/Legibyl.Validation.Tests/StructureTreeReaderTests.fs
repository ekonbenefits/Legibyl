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

/// Adds a link annotation on page 1 pointing at page 2 (by default at /XYZ 33 690 0) and
/// returns an /OBJR referencing it.
let private addInternalLinkAnnotationTo (view: PdfObject list) (document: PdfDocument) (page1: PdfPage) (page2: PdfPage) =
    let destination = PdfArray()
    destination.Add(page2.GetPdfObject())
    view |> List.iter destination.Add

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

let private addInternalLinkAnnotation =
    addInternalLinkAnnotationTo [ PdfName.XYZ; PdfNumber 33; PdfNumber 690; PdfNumber 0 ]

let private createPdfWithTaggedInternalLinkTo (view: PdfObject list) (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page1 = document.AddNewPage()
    let page2 = document.AddNewPage()

    let link = structElem PdfName.Link page1
    link.Put(PdfName.K, addInternalLinkAnnotationTo view document page1 page2) |> ignore

    setStructTreeRoot document link

let private createPdfWithTaggedInternalLink =
    createPdfWithTaggedInternalLinkTo [ PdfName.XYZ; PdfNumber 33; PdfNumber 690; PdfNumber 0 ]

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
    let destination = link.LinkDestination |> notNull
    (destination.PageNumber, destination.Kind, destination.Left, destination.Top)
    |> should equal (2, "XYZ", Nullable 33.0, Nullable 690.0)

[<Fact>]
let ``ReadStructureTree TOC item with child link destination reports destination on TOC item`` () =
    use input = new TempPdf()
    createPdfWithTaggedTocItemLink input.Path

    let root = reader.ReadStructureTree input.Path |> notNull

    let tocItem = root.Children |> single (fun n -> n.Tag = "TOCI")
    tocItem.LinkDestinationPageNumber |> should equal (Nullable 2)
    tocItem.LinkDestinationDescription |> should haveSubstring "page 2"
    // A TOC entry takes its target from its child link.
    (tocItem.LinkDestination |> notNull).Top |> should equal (Nullable 690.0)

[<Fact>]
let ``ReadStructureTree element without its own page takes the page of its marked content`` () =
    use input = new TempPdf()
    createPdfWithParagraphReferencingContentByMcr input.Path

    let root = reader.ReadStructureTree input.Path |> notNull

    let paragraph = root.Children |> single (fun n -> n.Tag = "P")
    paragraph.PageNumber |> should equal (Nullable 2)
    paragraph.Content |> should equal "Text on the second page"

[<Fact>]
let ``ReadStructureTree link to a rectangle reports the rectangle`` () =
    use input = new TempPdf()
    createPdfWithTaggedInternalLinkTo [ PdfName.FitR; PdfNumber 50; PdfNumber 400; PdfNumber 300; PdfNumber 500 ] input.Path

    let root = reader.ReadStructureTree input.Path |> notNull

    let destination = (root.Children |> single (fun n -> n.Tag = "Link")).LinkDestination |> notNull
    (destination.Kind, destination.Left, destination.Bottom, destination.Right, destination.Top)
    |> should equal ("FitR", Nullable 50.0, Nullable 400.0, Nullable 300.0, Nullable 500.0)

let private arrayOf (items: PdfObject list) =
    let array = PdfArray()
    for item in items do
        array.Add item
    array

[<Fact>]
let ``attributes followed by revision numbers are read, as Canva writes them`` () =
    use output = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(output.Path))
        let page = document.AddNewPage()
        let layout = PdfDictionary()
        layout.Put(PdfName.O, PdfName.Layout) |> ignore
        layout.Put(PdfName.BBox, numbers [ 100.0; 200.0; 300.0; 400.0 ]) |> ignore
        let figure = structElem PdfName.Figure page
        // An attribute object and its revision number.
        figure.Put(PdfName.A, arrayOf [ layout.MakeIndirect document; PdfNumber 0 ]) |> ignore
        figure.Put(PdfName.Alt, PdfString "A figure") |> ignore
        let revisionOnly = structElem PdfName.Figure page
        revisionOnly.Put(PdfName.A, arrayOf [ PdfNumber 0 ]) |> ignore
        setStructTreeRoot document (arrayOf [ figure; revisionOnly ])

    let root = reader.ReadStructureTree output.Path |> notNull

    let figure = root.Children[0]
    let geometry = figure.Geometry |> notNull
    (geometry.Left, geometry.Bottom, geometry.Right, geometry.Top) |> should equal (100.0, 200.0, 300.0, 400.0)
    root.Children[1].Geometry |> should be Null
