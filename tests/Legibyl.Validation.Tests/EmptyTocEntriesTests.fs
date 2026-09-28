module Legibyl.Validation.Tests.EmptyTocEntriesTests

open System
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private element (tag: string) (page: PdfPage) (kids: PdfObject list) : PdfDictionary =
    let element = PdfDictionary()
    element.Put(PdfName.Type, PdfName.StructElem) |> ignore
    element.Put(PdfName.S, PdfName tag) |> ignore
    element.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
    let array = PdfArray()
    kids |> List.iter array.Add
    element.Put(PdfName.K, array) |> ignore
    element

/// Document > TOC with four entries: one with text, an empty one, one holding only an empty
/// sub-entry, and one that owns a link annotation but no text.
let private createPdfWithToc (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    let properties = PdfDictionary()
    properties.Put(PdfName.MCID, PdfNumber 0) |> ignore
    PdfCanvas(page)
        .BeginMarkedContent(PdfName "Link", properties)
        .BeginText()
        .SetFontAndSize(PdfFontFactory.CreateFont(StandardFonts.HELVETICA), 12f)
        .MoveText(72.0, 700.0)
        .ShowText("Scope ......... 2")
        .EndText()
        .EndMarkedContent()
    |> ignore

    let annotation = PdfDictionary()
    annotation.Put(PdfName.Type, PdfName.Annot) |> ignore
    annotation.Put(PdfName.Subtype, PdfName.Link) |> ignore
    annotation.Put(PdfName.Rect, PdfArray([| 72f; 650f; 300f; 662f |])) |> ignore
    annotation.Put(PdfName.Contents, PdfString "Appendix") |> ignore
    annotation.MakeIndirect document |> ignore
    let annotations = PdfArray()
    annotations.Add annotation
    page.GetPdfObject().Put(PdfName.Annots, annotations) |> ignore
    let objectReference = PdfDictionary()
    objectReference.Put(PdfName.Type, PdfName.OBJR) |> ignore
    objectReference.Put(PdfName.Obj, annotation) |> ignore

    let toc =
        element "TOC" page [
            element "TOCI" page [ element "Link" page [ PdfNumber 0 ] ]
            element "TOCI" page []
            element "TOCI" page [ element "TOCI" page [] ]
            element "TOCI" page [ element "Link" page [ objectReference ] ] ]
    let root = PdfDictionary()
    root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    root.Put(PdfName.K, element "Document" page [ toc ]) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, root) |> ignore

let private emptyEntryWarnings (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun a -> a.Specification = "Legibyl" && a.TestNumber = 8)
    |> List.ofSeq

/// The TOC's entries, as nested tags: "TOCI(Link)".
let private tocShape (path: string) =
    let rec describe (node: StructureNode) =
        if node.Children.Count = 0 then node.Tag
        else $"""{node.Tag}({node.Children |> Seq.map describe |> String.concat ","})"""
    let root = VeraPdfValidator().ReadStructureTree path |> notNull
    let toc = (Seq.exactlyOne root.Children).Children |> Seq.exactlyOne
    toc.Children |> Seq.map describe |> List.ofSeq

let private failedRules (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq

[<Fact>]
let ``empty table-of-contents entries are reported`` () =
    use input = new TempPdf()
    createPdfWithToc input.Path

    let warnings = emptyEntryWarnings input.Path

    // The empty entry, and both the sub-entry and the entry holding only it; not the entry
    // with text, nor the one that owns a link annotation.
    warnings |> should haveLength 3
    warnings |> List.forall (fun w -> w.FixOptions = PdfFixOptions.EmptyTocEntries) |> should be True

[<Fact>]
let ``the fix removes them and nothing else`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithToc input.Path

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmptyTocEntries)

    result.AppliedFixes |> should contain "Removed 3 empty table-of-contents entr(ies) (TOCI)."
    tocShape output.Path |> should equal [ "TOCI(Link)"; "TOCI(Link)" ]
    emptyEntryWarnings output.Path |> should be Empty
    Set.difference (failedRules output.Path) (failedRules input.Path) |> should be Empty
    use renderer = new PdfRegionRenderer()
    let page = { PageNumber = 1; Left = 0.0; Bottom = 0.0; Right = 1.0; Top = 1.0 }
    renderer.RenderPageWithHighlight(output.Path, page).PngBytes
    |> should equal (renderer.RenderPageWithHighlight(input.Path, page).PngBytes)

[<Fact>]
let ``removing empty paragraphs leaves empty table-of-contents entries alone`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithToc input.Path

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmptyParagraphs)

    result.WasFixed |> should be False

[<Fact>]
let ``once they're gone there's nothing to remove: no copy is written, and it says why`` () =
    use input = new TempPdf()
    use once = new TempPdf ".once"
    use twice = new TempPdf ".twice"
    createPdfWithToc input.Path
    VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, once.Path, PdfFixOptions.EmptyTocEntries) |> ignore

    let result = VeraPdfValidator().Fix(once.Path, PdfProfile.PdfUa1, twice.Path, PdfFixOptions.EmptyTocEntries)

    result.WasFixed |> should be False
    System.IO.File.Exists twice.Path |> should be False
    List.ofSeq result.NotApplied |> should equal [ "Remove empty table-of-contents entries: nothing in this PDF needed it." ]

[<Fact>]
let ``an untagged PDF has no entries to remove`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        document.AddNewPage() |> ignore

    (VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmptyTocEntries)).WasFixed |> should be False

[<Fact>]
let ``with the safe fixes, the empty entries go and the others' changes are there too`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithToc input.Path

    // Not parent-tree: this page's elements are direct objects, which iText won't give children.
    let safe = FixNames.safe &&& ~~~PdfFixOptions.ParentTree
    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, safe ||| PdfFixOptions.EmptyTocEntries)

    result.AppliedFixes |> should contain "Removed 3 empty table-of-contents entr(ies) (TOCI)."
    tocShape output.Path |> should equal [ "TOCI(Link)"; "TOCI(Link)" ]
    emptyEntryWarnings output.Path |> should be Empty
    // The tab order (a safe fix) is set on the page with the link annotation.
    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetPage(1).GetPdfObject().GetAsName PdfName.Tabs |> should equal PdfName.S
