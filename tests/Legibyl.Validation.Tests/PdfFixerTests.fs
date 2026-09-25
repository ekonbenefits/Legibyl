module Legibyl.Validation.Tests.PdfFixerTests

open System
open System.IO
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.XMP
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private paragraph () =
    let paragraph = PdfDictionary()
    paragraph.Put(PdfName.Type, PdfName.StructElem) |> ignore
    paragraph.Put(PdfName.S, PdfName.P) |> ignore
    paragraph

let private setStructTreeRoot (document: PdfDocument) (kids: PdfObject) =
    let structTreeRoot = PdfDictionary()
    structTreeRoot.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    structTreeRoot.Put(PdfName.K, kids) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, structTreeRoot) |> ignore

let private arrayOf (items: PdfObject list) =
    let array = PdfArray()
    items |> List.iter array.Add
    array

let private structTreeRootOf (document: PdfDocument) =
    document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) |> notNull

let private createPdfWithAnnotationPageAndEmptyParagraphs (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    page.GetPdfObject().Put(PdfName.Annots, arrayOf [ PdfDictionary() ]) |> ignore

    let emptyArrayParagraph = paragraph ()
    emptyArrayParagraph.Put(PdfName.K, PdfArray()) |> ignore
    let nonEmptyParagraph = paragraph ()
    nonEmptyParagraph.Put(PdfName.K, PdfNumber 0) |> ignore

    setStructTreeRoot document (arrayOf [ paragraph (); emptyArrayParagraph; nonEmptyParagraph ])

let private createPdfWithAnnotationTabsAndNonEmptyParagraph (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let pageDictionary = document.AddNewPage().GetPdfObject()
    pageDictionary.Put(PdfName.Annots, arrayOf [ PdfDictionary() ]) |> ignore
    pageDictionary.Put(PdfName.Tabs, PdfName.S) |> ignore

    let nonEmptyParagraph = paragraph ()
    nonEmptyParagraph.Put(PdfName.K, PdfNumber 0) |> ignore
    setStructTreeRoot document (arrayOf [ nonEmptyParagraph ])

let private createPdfWithDirectEmptyParagraphRootKid (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.AddNewPage() |> ignore
    setStructTreeRoot document (paragraph ())

let private createPdfWithNestedEmptyParagraph (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.AddNewPage() |> ignore

    let emptySpan = PdfDictionary()
    emptySpan.Put(PdfName.Type, PdfName.StructElem) |> ignore
    emptySpan.Put(PdfName.S, PdfName.Span) |> ignore
    emptySpan.Put(PdfName.K, PdfArray()) |> ignore

    let paragraph = paragraph ()
    paragraph.Put(PdfName.K, emptySpan) |> ignore
    setStructTreeRoot document paragraph

let private createPdfWithParagraphActualText (path: string) (paragraphs: string list) =
    use document = new PdfDocument(new PdfWriter(path))
    document.AddNewPage() |> ignore

    let kids =
        paragraphs
        |> List.map (fun text ->
            let paragraph = paragraph ()
            paragraph.Put(PdfName.ActualText, PdfString text) |> ignore
            paragraph :> PdfObject)

    setStructTreeRoot document (arrayOf kids)

/// Draws each paragraph's text at (72, y) inside its own marked content and tags it as a /P.
let private createPdfWithMarkedContentParagraphs (path: string) (paragraphs: (float32 * string) list) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    let canvas = PdfCanvas(page)
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA)

    let kids =
        paragraphs
        |> List.mapi (fun mcid (y, text) ->
            let properties = PdfDictionary()
            properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
            canvas
                .BeginMarkedContent(PdfName.P, properties)
                .BeginText()
                .SetFontAndSize(font, 12f)
                .MoveText(72.0, float y)
                .ShowText(text)
                .EndText()
                .EndMarkedContent()
            |> ignore

            let paragraph = paragraph ()
            paragraph.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
            paragraph.Put(PdfName.K, PdfNumber mcid) |> ignore
            paragraph :> PdfObject)

    setStructTreeRoot document (arrayOf kids)

let private createPdfWithFigureWithoutAltAndLinkWithoutContents (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()

    let properties = PdfDictionary()
    properties.Put(PdfName.MCID, PdfNumber 0) |> ignore
    PdfCanvas(page)
        .BeginMarkedContent(PdfName.Figure, properties)
        .BeginText()
        .SetFontAndSize(PdfFontFactory.CreateFont(StandardFonts.HELVETICA), 12f)
        .MoveText(72.0, 700.0)
        .ShowText("A figure drawn as text")
        .EndText()
        .EndMarkedContent()
    |> ignore

    let link = PdfDictionary()
    link.Put(PdfName.Type, PdfName.Annot) |> ignore
    link.Put(PdfName.Subtype, PdfName.Link) |> ignore
    link.Put(PdfName.Rect, PdfArray([| 100f; 500f; 200f; 520f |])) |> ignore
    link.MakeIndirect document |> ignore
    page.GetPdfObject().Put(PdfName.Annots, arrayOf [ link ]) |> ignore

    let figure = PdfDictionary()
    figure.Put(PdfName.Type, PdfName.StructElem) |> ignore
    figure.Put(PdfName.S, PdfName.Figure) |> ignore
    figure.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
    figure.Put(PdfName.K, PdfNumber 0) |> ignore
    figure.MakeIndirect document |> ignore

    setStructTreeRoot document (arrayOf [ figure ])

let private isWarning (message: string) (a: RuleViolation) =
    a.Severity = FindingSeverity.Warning && a.Message.Contains(message, StringComparison.Ordinal)

let private isSplitParagraphWarning = isWarning "Likely split paragraph"

let private locationContains (text: string) (a: RuleViolation) =
    match a.LocationContext with
    | NonNull context -> context.Contains(text, StringComparison.Ordinal)
    | Null -> false

[<Fact>]
let ``Fix annotation tabs and empty paragraphs applies selected structural fixes`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithAnnotationPageAndEmptyParagraphs input.Path
    let fixer = VeraPdfValidator()

    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.All)

    result.WasFixed |> should be True
    result.OutputPath |> should equal output.Path
    result.AppliedFixes |> Seq.exists (fun fix -> fix.Contains("/Tabs /S", StringComparison.Ordinal)) |> should be True
    result.AppliedFixes |> Seq.exists (fun fix -> fix.Contains("empty /P", StringComparison.Ordinal)) |> should be True

    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetPage(1).GetPdfObject().GetAsName(PdfName.Tabs) |> should equal PdfName.S

    let kids = (structTreeRootOf document).GetAsArray(PdfName.K) |> notNull
    kids.Size() |> should equal 1
    let remainingParagraph = kids.GetAsDictionary 0 |> notNull
    remainingParagraph.GetAsName(PdfName.S) |> should equal PdfName.P
    remainingParagraph.Get(PdfName.K) |> should equal (PdfNumber 0)

[<Fact>]
let ``Fix with no selected structural changes does not write output`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithAnnotationTabsAndNonEmptyParagraph input.Path
    let fixer = VeraPdfValidator()

    let result =
        fixer.Fix(
            input.Path,
            PdfProfile.PdfUa1,
            output.Path,
            PdfFixOptions.AnnotationTabs ||| PdfFixOptions.RemoveEmptyParagraphs
        )

    result.WasFixed |> should be False
    result.OutputPath |> should be Null
    File.Exists output.Path |> should be False

[<Fact>]
let ``Fix direct empty paragraph root kid removes root kid`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithDirectEmptyParagraphRootKid input.Path
    let fixer = VeraPdfValidator()

    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.RemoveEmptyParagraphs)

    result.WasFixed |> should be True
    use document = new PdfDocument(new PdfReader(output.Path))
    (structTreeRootOf document).Get(PdfName.K) |> should be Null

[<Fact>]
let ``Fix nested empty paragraph removes paragraph`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithNestedEmptyParagraph input.Path
    let fixer = VeraPdfValidator()

    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.RemoveEmptyParagraphs)

    result.WasFixed |> should be True
    use document = new PdfDocument(new PdfReader(output.Path))
    (structTreeRootOf document).Get(PdfName.K) |> should be Null

[<Fact>]
let ``Validate empty paragraph adds fixable warning`` () =
    use input = new TempPdf()
    createPdfWithDirectEmptyParagraphRootKid input.Path
    let validator = VeraPdfValidator()

    let report = validator.Validate(input.Path, PdfProfile.PdfUa1)

    let warning = report.Assertions |> single (fun a -> a.Severity = FindingSeverity.Warning)
    warning.Passed |> should be True
    warning.FixOptions |> should equal PdfFixOptions.RemoveEmptyParagraphs
    warning.IsFixable |> should be True
    warning.Message |> should haveSubstring "Empty semantic /P"

[<Fact>]
let ``Validate LibreOffice role-mapped whitespace paragraph is empty with one-character geometry`` () =
    let validator = VeraPdfValidator()

    let report = validator.Validate(corpusFile "libreoffice-empty-paragraph.pdf", PdfProfile.PdfUa1)

    let warning = report.Assertions |> single (isWarning "Empty semantic /P")
    let geometry = warning.Geometry |> notNull
    geometry.PageNumber |> should equal 1
    geometry.Right - geometry.Left |> should be (inRange 1.0 12.0)
    geometry.Top - geometry.Bottom |> should be (inRange 6.0 18.0)

[<Fact>]
let ``Fix LibreOffice role-mapped whitespace paragraph is removed`` () =
    use output = new TempPdf ".fixed"

    let result =
        VeraPdfValidator()
            .Fix(corpusFile "libreoffice-empty-paragraph.pdf", PdfProfile.PdfUa1, output.Path, PdfFixOptions.RemoveEmptyParagraphs)

    result.AppliedFixes
    |> Seq.exists (fun fix -> fix.Contains("Removed 1 empty /P", StringComparison.Ordinal))
    |> should be True

[<Fact>]
let ``Validate likely split paragraph adds non-fixable warning`` () =
    use input = new TempPdf()
    createPdfWithParagraphActualText
        input.Path
        [ "This handbook explains your benefits under the plan. The pages"
          "that follow explain how you become a member." ]
    let validator = VeraPdfValidator()

    let report = validator.Validate(input.Path, PdfProfile.PdfUa1)

    let warning = report.Assertions |> single isSplitParagraphWarning
    warning.Passed |> should be True
    warning.IsFixable |> should be False
    warning.FixOptions |> should equal PdfFixOptions.None

[<Fact>]
let ``Validate likely split paragraph has geometry covering both paragraphs`` () =
    use input = new TempPdf()
    createPdfWithMarkedContentParagraphs
        input.Path
        [ 720f, "This handbook explains your benefits under the plan. The pages"
          700f, "that follow explain how you become a member." ]
    let validator = VeraPdfValidator()

    let report = validator.Validate(input.Path, PdfProfile.PdfUa1)

    let warning = report.Assertions |> single isSplitParagraphWarning
    let geometry = warning.Geometry |> notNull
    geometry.PageNumber |> should equal 1
    geometry.Left |> should be (inRange 71.0 73.0)
    geometry.Bottom |> should be (lessThan 700.0)
    geometry.Top |> should be (greaterThan 720.0)

[<Fact>]
let ``Validate veraPDF failures have geometry for structure elements and annotations`` () =
    use input = new TempPdf()
    createPdfWithFigureWithoutAltAndLinkWithoutContents input.Path
    let validator = VeraPdfValidator()

    let report = validator.Validate(input.Path, PdfProfile.PdfUa1)

    let figure = report.FailedAssertions |> single (locationContains "SEFigure")
    let figureGeometry = figure.Geometry |> notNull
    figureGeometry.PageNumber |> should equal 1
    figureGeometry.Left |> should be (inRange 71.0 73.0)
    figureGeometry.Bottom |> should be (inRange 695.0 701.0)

    let link = report.FailedAssertions |> Seq.find (locationContains "PDLinkAnnot")
    let linkGeometry = link.Geometry |> notNull
    (linkGeometry.PageNumber, linkGeometry.Left, linkGeometry.Bottom, linkGeometry.Right, linkGeometry.Top)
    |> should equal (1, 100.0, 500.0, 200.0, 520.0)

[<Fact>]
let ``Validate adjacent complete paragraphs does not add split warning`` () =
    use input = new TempPdf()
    createPdfWithParagraphActualText
        input.Path
        [ "This is a complete paragraph."; "This is the next complete paragraph." ]
    let validator = VeraPdfValidator()

    let report = validator.Validate(input.Path, PdfProfile.PdfUa1)

    report.Assertions |> Seq.exists isSplitParagraphWarning |> should be False

/// Draws each link's text inside its own marked content and tags it as a /Link that owns
/// both that marked content and a link annotation, the way a real tagged link is built.
let private createPdfWithLinks (path: string) (links: (string * string option) list) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    let canvas = PdfCanvas(page)
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA)

    let annotations = PdfArray()

    let kids =
        links
        |> List.mapi (fun mcid (text, contents) ->
            let y = 700.0 - float mcid * 20.0

            let properties = PdfDictionary()
            properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
            canvas
                .BeginMarkedContent(PdfName.Link, properties)
                .BeginText()
                .SetFontAndSize(font, 12f)
                .MoveText(72.0, y)
                .ShowText(text)
                .EndText()
                .EndMarkedContent()
            |> ignore

            let annotation = PdfDictionary()
            annotation.Put(PdfName.Type, PdfName.Annot) |> ignore
            annotation.Put(PdfName.Subtype, PdfName.Link) |> ignore
            annotation.Put(PdfName.Rect, PdfArray([| 72f; float32 y; 400f; float32 y + 12f |])) |> ignore
            contents |> Option.iter (fun c -> annotation.Put(PdfName.Contents, PdfString c) |> ignore)
            annotation.MakeIndirect document |> ignore
            annotations.Add annotation

            let objectReference = PdfDictionary()
            objectReference.Put(PdfName.Type, PdfName.OBJR) |> ignore
            objectReference.Put(PdfName.Obj, annotation) |> ignore

            let link = PdfDictionary()
            link.Put(PdfName.Type, PdfName.StructElem) |> ignore
            link.Put(PdfName.S, PdfName.Link) |> ignore
            link.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
            link.Put(PdfName.K, arrayOf [ PdfNumber mcid; objectReference ]) |> ignore
            link.MakeIndirect document |> ignore
            link :> PdfObject)

    page.GetPdfObject().Put(PdfName.Annots, annotations) |> ignore
    setStructTreeRoot document (arrayOf kids)

/// The /Contents of every link annotation on the first page, in order.
let private linkContentsOf (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    let annotations = document.GetPage(1).GetPdfObject().GetAsArray(PdfName.Annots) |> notNull
    [ for i in 0 .. annotations.Size() - 1 ->
        annotations.GetAsDictionary i
        |> notNull
        |> fun annotation -> annotation.GetAsString(PdfName.Contents) |> Option.ofObj |> Option.map _.ToUnicodeString() ]

/// Rewrites path in place, giving it an XMP dc:title and nothing in Info /Title —
/// the shape a document has when its title needs moving across rather than inventing.
let private setXmpTitle (path: string) (title: string) =
    let bytes = File.ReadAllBytes path
    use reader = new PdfReader(new MemoryStream(bytes))
    use writer = new PdfWriter(path)
    use document = new PdfDocument(reader, writer)
    let xmp = XMPMetaFactory.Create()
    xmp.SetLocalizedText(XMPConst.NS_DC, "title", null, XMPConst.X_DEFAULT, title)
    document.SetXmpMetadata xmp

[<Fact>]
let ``Fix link contents describes an undescribed link from its own text`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path [ "Our privacy policy", None ]
    let fixer = VeraPdfValidator()

    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents)

    result.WasFixed |> should be True
    result.AppliedFixes |> Seq.exists (fun fix -> fix.Contains("Described 1 link", StringComparison.Ordinal)) |> should be True
    linkContentsOf output.Path |> should equal [ Some "Our privacy policy" ]

[<Fact>]
let ``Fix link contents strips dot leaders and the printed page number from a contents entry`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path [ "Scope of this document ......... 3", None ]
    let fixer = VeraPdfValidator()

    fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents) |> ignore

    linkContentsOf output.Path |> should equal [ Some "Scope of this document" ]

[<Fact>]
let ``Fix link contents keeps a trailing number that is part of the link's own text`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // Without leaders running up to it, a trailing number is text, not a page number.
    createPdfWithLinks input.Path [ "Appendix 2", None ]
    let fixer = VeraPdfValidator()

    fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents) |> ignore

    linkContentsOf output.Path |> should equal [ Some "Appendix 2" ]

[<Fact>]
let ``Fix link contents leaves a link that already has a description alone`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path [ "Read more", Some "Hand-written description" ]
    let fixer = VeraPdfValidator()

    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents)

    result.WasFixed |> should be False
    linkContentsOf input.Path |> should equal [ Some "Hand-written description" ]

[<Fact>]
let ``Fix document title moves a title the document carries in XMP into the Info dictionary`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path [ "Read more", None ]
    setXmpTitle input.Path "Retirement Plan Summary"
    let fixer = VeraPdfValidator()

    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.DocumentTitle)

    result.WasFixed |> should be True
    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetDocumentInfo().GetTitle() |> should equal "Retirement Plan Summary"
    let preferences = document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.ViewerPreferences) |> notNull
    preferences.Get(PdfName.DisplayDocTitle) |> should equal PdfBoolean.TRUE

[<Fact>]
let ``Fix document title never invents a title for a document that carries none`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path [ "Read more", None ]
    let fixer = VeraPdfValidator()

    // /DisplayDocTitle is still worth setting, so the file does change — but a title the
    // author never wrote is not ours to make up, least of all out of the file name.
    fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.DocumentTitle) |> ignore

    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetDocumentInfo().GetTitle() |> should be NullOrEmptyString

[<Fact>]
let ``Fix writes output when a structural fix applies but the metadata fixer changes nothing`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // No title anywhere, so veraPDF's metadata fixer won't repair dc:title and reports
    // WontFix. The structural repair still has to reach outputPath — the file it moves
    // there is the one veraPDF's parser just had open.
    createPdfWithLinks input.Path [ "Our privacy policy", None ]
    let fixer = VeraPdfValidator()

    let result =
        fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents ||| PdfFixOptions.Metadata)

    result.WasFixed |> should be True
    File.Exists output.Path |> should be True
    linkContentsOf output.Path |> should equal [ Some "Our privacy policy" ]

[<Fact>]
let ``Fix document title keeps a title the document already has`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path [ "Read more", None ]
    (use document = new PdfDocument(new PdfReader(input.Path), new PdfWriter(output.Path))
     document.GetDocumentInfo().SetTitle "The Real Title" |> ignore)
    File.Copy(output.Path, input.Path, true)
    let fixer = VeraPdfValidator()

    fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.DocumentTitle) |> ignore

    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetDocumentInfo().GetTitle() |> should equal "The Real Title"
