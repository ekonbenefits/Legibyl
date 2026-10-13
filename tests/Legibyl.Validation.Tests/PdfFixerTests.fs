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

    // (All includes putting the tags under a single Document.)
    let documentElement = (structTreeRootOf document).GetAsDictionary(PdfName.K) |> notNull
    documentElement.GetAsName(PdfName.S) |> should equal PdfName.Document
    let kids = documentElement.GetAsArray(PdfName.K) |> notNull
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
            PdfFixOptions.Tabs ||| PdfFixOptions.EmptyParagraphs
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

    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmptyParagraphs)

    result.WasFixed |> should be True
    use document = new PdfDocument(new PdfReader(output.Path))
    (structTreeRootOf document).Get(PdfName.K) |> should be Null

[<Fact>]
let ``Fix removes empty paragraphs under a single Document element`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // StructTreeRoot /K is one Document dictionary rather than an array, as in most real files.
    (use document = new PdfDocument(new PdfWriter(input.Path))
     document.AddNewPage() |> ignore
     let nonEmptyParagraph = paragraph ()
     nonEmptyParagraph.Put(PdfName.K, PdfNumber 0) |> ignore
     let documentElement = PdfDictionary()
     documentElement.Put(PdfName.Type, PdfName.StructElem) |> ignore
     documentElement.Put(PdfName.S, PdfName.Document) |> ignore
     documentElement.Put(PdfName.K, arrayOf [ paragraph (); nonEmptyParagraph ]) |> ignore
     setStructTreeRoot document documentElement)
    let fixer = VeraPdfValidator()

    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmptyParagraphs)

    result.WasFixed |> should be True
    result.AppliedFixes |> should contain "Removed 1 empty /P structure element(s)."

[<Fact>]
let ``Fix nested empty paragraph removes paragraph`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithNestedEmptyParagraph input.Path
    let fixer = VeraPdfValidator()

    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmptyParagraphs)

    result.WasFixed |> should be True
    use document = new PdfDocument(new PdfReader(output.Path))
    (structTreeRootOf document).Get(PdfName.K) |> should be Null

[<Fact>]
let ``Validate empty paragraph adds fixable warning`` () =
    use input = new TempPdf()
    createPdfWithDirectEmptyParagraphRootKid input.Path
    let validator = VeraPdfValidator()

    let report = validator.Validate(input.Path, PdfProfile.PdfUa1)

    // (Its paragraph is straight under the root, so it's also warned of no Document there: Structure-16.)
    let warning = report.Assertions |> single (fun a -> a.Severity = FindingSeverity.Warning && a.TestNumber = 1)
    warning.Passed |> should be True
    warning.FixOptions |> should equal PdfFixOptions.EmptyParagraphs
    warning.IsFixable |> should be True
    warning.Message |> should haveSubstring "Empty paragraph tag"

[<Fact>]
let ``Validate LibreOffice role-mapped whitespace paragraph is empty with one-character geometry`` () =
    let validator = VeraPdfValidator()

    let report = validator.Validate(corpusFile "libreoffice-empty-paragraph.pdf", PdfProfile.PdfUa1)

    let warning = report.Assertions |> single (isWarning "Empty paragraph tag")
    let geometry = warning.Geometry |> notNull
    geometry.PageNumber |> should equal 1
    geometry.Right - geometry.Left |> should be (inRange 1.0 12.0)
    geometry.Top - geometry.Bottom |> should be (inRange 6.0 18.0)

[<Fact>]
let ``Fix LibreOffice role-mapped whitespace paragraph is removed`` () =
    use output = new TempPdf ".fixed"

    let result =
        VeraPdfValidator()
            .Fix(corpusFile "libreoffice-empty-paragraph.pdf", PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmptyParagraphs)

    result.AppliedFixes
    |> Seq.exists (fun fix -> fix.Contains("Removed 1 empty /P", StringComparison.Ordinal))
    |> should be True

[<Fact>]
let ``Validate likely split paragraph adds a warning the layout join can fix`` () =
    use input = new TempPdf()
    createPdfWithParagraphActualText
        input.Path
        [ "This handbook explains your benefits under the plan. The pages"
          "that follow explain how you become a member." ]
    let validator = VeraPdfValidator()

    let report = validator.Validate(input.Path, PdfProfile.PdfUa1)

    let warning = report.Assertions |> single isSplitParagraphWarning
    warning.Passed |> should be True
    warning.IsFixable |> should be True
    warning.FixOptions |> should equal PdfFixOptions.JoinSplitParagraphs

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

/// Where createPdfWithLinks puts the tagged links in the structure tree.
type private LinkContainer =
    /// Straight under the structure tree root, as links in body text are.
    | Body
    /// Each link in its own TOCI inside a TOC.
    | TableOfContents
    /// Each link wrapped in a Span inside its TOCI.
    | TableOfContentsWrapped
    /// Each link in a sub-entry: TOC > TOCI > TOCI > Link.
    | TableOfContentsNested

let private structElement (tag: PdfName) (kids: PdfObject list) : PdfObject =
    let element = PdfDictionary()
    element.Put(PdfName.Type, PdfName.StructElem) |> ignore
    element.Put(PdfName.S, tag) |> ignore
    element.Put(PdfName.K, arrayOf kids) |> ignore
    element

/// Draws each link's text inside its own marked content and tags it as a /Link that owns
/// both that marked content and a link annotation, the way a real tagged link is built.
let private createPdfWithLinks (path: string) (container: LinkContainer) (links: (string * string option) list) =
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
    let entry (link: PdfObject) =
        match container with
        | Body -> link
        | TableOfContents -> structElement PdfName.TOCI [ link ]
        | TableOfContentsWrapped -> structElement PdfName.TOCI [ structElement PdfName.Span [ link ] ]
        | TableOfContentsNested -> structElement PdfName.TOCI [ structElement PdfName.TOCI [ link ] ]
    let entries = kids |> List.map entry
    setStructTreeRoot document (if container = Body then arrayOf entries :> PdfObject else structElement PdfName.TOC entries)

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

let private describedLinkCount (result: FixResult) =
    result.AppliedFixes |> Seq.exists (fun fix -> fix.Contains("Described", StringComparison.Ordinal))

[<Fact>]
let ``Fix link contents describes an undescribed table-of-contents link from its own text`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path TableOfContents [ "Our privacy policy", None ]
    let fixer = VeraPdfValidator()

    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents)

    result.WasFixed |> should be True
    result.AppliedFixes |> Seq.exists (fun fix -> fix.Contains("Described 1 link", StringComparison.Ordinal)) |> should be True
    linkContentsOf output.Path |> should equal [ Some "Our privacy policy" ]

[<Fact>]
let ``Fix link contents strips dot leaders and the printed page number from a contents entry`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path TableOfContents [ "Scope of this document ......... 3", None ]
    let fixer = VeraPdfValidator()

    fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents) |> ignore

    linkContentsOf output.Path |> should equal [ Some "Scope of this document" ]

[<Fact>]
let ``Fix link contents keeps a trailing number that is part of the link's own text`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // Without leaders running up to it, a trailing number is text, not a page number.
    createPdfWithLinks input.Path TableOfContents [ "Appendix 2", None ]
    let fixer = VeraPdfValidator()

    fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents) |> ignore

    linkContentsOf output.Path |> should equal [ Some "Appendix 2" ]

[<Fact>]
let ``Fix link contents describes a table-of-contents link wrapped in another element`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path TableOfContentsWrapped [ "Eligibility ..... 4", None ]
    let fixer = VeraPdfValidator()

    fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents) |> ignore

    linkContentsOf output.Path |> should equal [ Some "Eligibility" ]

[<Fact>]
let ``Fix link contents describes a link in a nested table-of-contents entry`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path TableOfContentsNested [ "Who can join ..... 5", None ]
    let fixer = VeraPdfValidator()

    fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents) |> ignore

    linkContentsOf output.Path |> should equal [ Some "Who can join" ]

[<Theory>]
[<InlineData("Our privacy policy")>]
[<InlineData("click here")>]
let ``Fix link contents leaves a link outside a table of contents failing`` (text: string) =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // Even text that would make a fine description isn't used: outside a table of contents
    // there's no telling, and a guess would silence a failure a person should look at.
    createPdfWithLinks input.Path Body [ text, None ]
    let fixer = VeraPdfValidator()

    // Tab order is fixed too, so there's an output file to validate.
    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents ||| PdfFixOptions.Tabs)

    describedLinkCount result |> should be False
    linkContentsOf output.Path |> should equal [ (None: string option) ]
    let failedRules = VeraPdfValidator().Validate(output.Path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq
    failedRules |> should contain "7.18.1-2"
    failedRules |> should contain "7.18.5-2"

[<Theory>]
[<InlineData(true)>]
[<InlineData(false)>]
let ``Fix link contents leaves a link that already has a description alone`` (insideTableOfContents: bool) =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path (if insideTableOfContents then TableOfContents else Body) [ "Read more", Some "Hand-written description" ]
    let fixer = VeraPdfValidator()

    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents)

    result.WasFixed |> should be False
    linkContentsOf input.Path |> should equal [ Some "Hand-written description" ]

[<Fact>]
let ``Validate marks a missing link description fixable only in a table of contents`` () =
    use body = new TempPdf()
    use toc = new TempPdf()
    createPdfWithLinks body.Path Body [ "Our privacy policy", None ]
    createPdfWithLinks toc.Path TableOfContents [ "Our privacy policy", None ]
    let validator = VeraPdfValidator()
    let linkFixOptions (path: string) =
        validator.Validate(path, PdfProfile.PdfUa1).FailedAssertions
        |> Seq.filter (fun a -> a.RuleId = "7.18.5-2")
        |> Seq.map (fun a -> a.FixOptions.HasFlag PdfFixOptions.LinkContents)
        |> List.ofSeq

    linkFixOptions body.Path |> should equal [ false ]
    linkFixOptions toc.Path |> should equal [ true ]

/// Rewrites path in place, pointing each link annotation on its first page at the web
/// address given for it (None leaves it without an action).
let private setLinkAddresses (path: string) (addresses: string option list) =
    let bytes = File.ReadAllBytes path
    use reader = new PdfReader(new MemoryStream(bytes))
    use writer = new PdfWriter(path)
    use document = new PdfDocument(reader, writer)
    let annotations = document.GetPage(1).GetPdfObject().GetAsArray(PdfName.Annots) |> notNull
    addresses
    |> List.iteri (fun i address ->
        address
        |> Option.iter (fun address ->
            let action = PdfDictionary()
            action.Put(PdfName.S, PdfName.URI) |> ignore
            action.Put(PdfName.URI, PdfString address) |> ignore
            (annotations.GetAsDictionary i |> notNull).Put(PdfName.A, action) |> ignore))

let private fixWithTitles (titles: IPageTitleSource) (input: string) (output: string) (options: PdfFixOptions) =
    let fixer = VeraPdfValidator(PageTitleSource = titles)
    fixer.Fix(input, PdfProfile.PdfUa1, output, options)

[<Fact>]
let ``Fix link titles describes a web link anywhere by the title of the page it goes to`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path Body [ "click here", None; "read more", None ]
    setLinkAddresses input.Path [ Some "https://example.com/apply"; Some "https://example.com/untitled" ]
    let titles = FixedTitles [ "https://example.com/apply", "Apply for benefits" ]

    let result = fixWithTitles titles input.Path output.Path PdfFixOptions.LinkTitlesFromWeb

    result.AppliedFixes |> should contain "Described 1 web link by the title of the page it goes to, read from the web: to check."
    // A page with no title to read leaves its link for a person to describe.
    linkContentsOf output.Path |> should equal [ Some "Apply for benefits (example.com)"; None ]
    result.NotApplied |> Seq.exists (fun note -> note.Contains "https://example.com/untitled") |> should be True
    titles.Asked |> List.ofSeq |> should equal [ "https://example.com/apply"; "https://example.com/untitled" ]

[<Fact>]
let ``Fix link titles leaves links without a web address, or with a description, alone`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path Body [ "Page 3", None; "Our site", Some "Our home page"; "Write to us", None ]
    setLinkAddresses input.Path [ None; Some "https://example.com/"; Some "mailto:help@example.com" ]
    let titles = FixedTitles [ "https://example.com/", "Example" ]

    // Tab order is fixed too, so there's an output file to read.
    fixWithTitles titles input.Path output.Path (PdfFixOptions.LinkTitlesFromWeb ||| PdfFixOptions.Tabs) |> ignore

    titles.Asked |> should be Empty
    linkContentsOf output.Path |> should equal [ None; Some "Our home page"; None ]

[<Fact>]
let ``Fix cancelled while reading link titles stops at once, rather than when the web pages answer`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path Body [ "click here", None ]
    setLinkAddresses input.Path [ Some "https://example.com/apply" ]
    use answer = new Threading.ManualResetEventSlim(false)
    use asked = new Threading.ManualResetEventSlim(false)
    // Web pages that take their time: until the test's done.
    let slow =
        { new IPageTitleSource with
            member _.PageTitles _ =
                asked.Set()
                answer.Wait(TimeSpan.FromMinutes 2.0) |> ignore
                Collections.Generic.Dictionary<string, string>() }
    use cancel = new Threading.CancellationTokenSource()
    let fixing =
        Threading.Tasks.Task.Run(fun () ->
            VeraPdfValidator(PageTitleSource = slow).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkTitlesFromWeb, null, cancel.Token) |> ignore)
    asked.Wait(TimeSpan.FromSeconds 60.0) |> should be True
    let stopwatch = Diagnostics.Stopwatch.StartNew()
    cancel.Cancel()

    (fun () -> fixing.GetAwaiter().GetResult()) |> should throw typeof<OperationCanceledException>

    stopwatch.Elapsed |> should be (lessThan (TimeSpan.FromSeconds 10.0))
    File.Exists output.Path |> should be False
    answer.Set()

[<Fact>]
let ``Fix reads no link titles unless asked to`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path Body [ "click here", None ]
    setLinkAddresses input.Path [ Some "https://example.com/apply" ]
    let titles = FixedTitles [ "https://example.com/apply", "Apply for benefits" ]

    fixWithTitles titles input.Path output.Path (PdfFixOptions.All &&& ~~~PdfFixOptions.LinkTitlesFromWeb) |> ignore

    titles.Asked |> should be Empty

[<Fact>]
let ``Fix link titles describes a web link whose only description is its address`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path Body [ "Plan", Some "https://example.com/plan/"; "Elsewhere", Some "https://example.com/other" ]
    setLinkAddresses input.Path [ Some "https://example.com/plan"; Some "https://example.com/apply" ]
    let titles = FixedTitles [ "https://example.com/plan", "Retirement plan"; "https://example.com/apply", "Apply for benefits" ]

    let result = fixWithTitles titles input.Path output.Path PdfFixOptions.LinkTitlesFromWeb

    result.AppliedFixes |> should contain "Described 1 web link by the title of the page it goes to, read from the web: to check."
    // An address written as the description of a link to another one says something: it's kept.
    linkContentsOf output.Path |> should equal [ Some "Retirement plan (example.com)"; Some "https://example.com/other" ]

[<Fact>]
let ``Validate marks a missing description on a web link fixable from its page's title`` () =
    use web = new TempPdf()
    use local = new TempPdf()
    createPdfWithLinks web.Path Body [ "click here", None ]
    setLinkAddresses web.Path [ Some "https://example.com/apply" ]
    createPdfWithLinks local.Path Body [ "click here", None ]
    let linkFixOptions (path: string) =
        VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions
        |> Seq.filter (fun a -> a.RuleId = "7.18.5-2")
        |> Seq.map (fun a -> a.FixOptions.HasFlag PdfFixOptions.LinkTitlesFromWeb)
        |> List.ofSeq

    linkFixOptions web.Path |> should equal [ true ]
    linkFixOptions local.Path |> should equal [ false ]

[<Fact>]
let ``Fix document title moves a title the document carries in XMP into the Info dictionary`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path Body [ "Read more", None ]
    setXmpTitle input.Path "Retirement Plan Summary"
    let fixer = VeraPdfValidator()

    let result = fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.Title)

    result.WasFixed |> should be True
    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetDocumentInfo().GetTitle() |> should equal "Retirement Plan Summary"
    let preferences = document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.ViewerPreferences) |> notNull
    preferences.Get(PdfName.DisplayDocTitle) |> should equal PdfBoolean.TRUE

[<Fact>]
let ``Fix document title never invents a title for a document that carries none`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path Body [ "Read more", None ]
    let fixer = VeraPdfValidator()

    // /DisplayDocTitle is still worth setting, so the file does change — but a title the
    // author never wrote is not ours to make up, least of all out of the file name.
    fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.Title) |> ignore

    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetDocumentInfo().GetTitle() |> should be NullOrEmptyString

[<Fact>]
let ``Fix writes output when a structural fix applies but the metadata fixer changes nothing`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // No title anywhere, so veraPDF's metadata fixer won't fix dc:title and reports
    // WontFix. The structural fix still has to reach outputPath — the file it moves
    // there is the one veraPDF's parser just had open.
    createPdfWithLinks input.Path TableOfContents [ "Our privacy policy", None ]
    let fixer = VeraPdfValidator()

    let result =
        fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkContents ||| PdfFixOptions.Metadata)

    result.WasFixed |> should be True
    File.Exists output.Path |> should be True
    linkContentsOf output.Path |> should equal [ Some "Our privacy policy" ]
    // veraPDF's metadata changes weren't kept, so they aren't reported as made; that they
    // weren't is. (The XMP stream the document lacked is Legibyl's own fix, and kept.)
    List.ofSeq result.AppliedFixes
    |> should
        equal
        [ "Described 1 link annotation(s) from their own text."
          "Added an XMP metadata stream, from the document information dictionary." ]
    List.ofSeq result.NotApplied
    |> should
        equal
        [ "The metadata wasn't fixed: veraPDF's metadata fixer found no way to make it conform (Won't fix), so its changes weren't kept." ]

/// A document whose first page draws lines, one under another, followed by
/// extraPages blank pages.
let private createPdfWithFirstPage (path: string) (lines: string list) (extraPages: int) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    let canvas = PdfCanvas(page)
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    lines
    |> List.iteri (fun i text ->
        canvas.BeginText().SetFontAndSize(font, 12.0f).MoveText(72.0, 700.0 - 20.0 * float i).ShowText(text).EndText()
        |> ignore)
    for _ in 1 .. extraPages do
        document.AddNewPage() |> ignore

let private fixedTitle (options: PdfFixOptions) (input: TempPdf) =
    use output = new TempPdf ".fixed"
    VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, options) |> ignore
    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetDocumentInfo().GetTitle()

[<Fact>]
let ``Fix title from first page titles an untitled document from its title page`` () =
    use input = new TempPdf()
    createPdfWithFirstPage input.Path [ "Retirement Plan"; "Summary of Benefits"; "2026" ] 1

    fixedTitle PdfFixOptions.TitleFromFirstPage input |> should equal "Retirement Plan Summary of Benefits 2026"

[<Fact>]
let ``Fix document title alone does not take a title from the first page`` () =
    use input = new TempPdf()
    createPdfWithFirstPage input.Path [ "Retirement Plan" ] 1

    fixedTitle PdfFixOptions.Title input |> should be NullOrEmptyString

[<Fact>]
let ``Fix title from first page prefers a title the document carries in XMP`` () =
    use input = new TempPdf()
    createPdfWithFirstPage input.Path [ "Cover" ] 1
    setXmpTitle input.Path "Retirement Plan Summary"

    fixedTitle PdfFixOptions.TitleFromFirstPage input |> should equal "Retirement Plan Summary"

[<Fact>]
let ``Fix title from first page leaves a first page too long to be a title page`` () =
    use input = new TempPdf()
    let body = [ for i in 1 .. 8 -> $"Line {i} of body text that runs on for a while." ]
    createPdfWithFirstPage input.Path body 1

    fixedTitle PdfFixOptions.TitleFromFirstPage input |> should be NullOrEmptyString

[<Fact>]
let ``Fix title from first page leaves a one-page document untitled`` () =
    use input = new TempPdf()
    createPdfWithFirstPage input.Path [ "Retirement Plan" ] 0

    fixedTitle PdfFixOptions.TitleFromFirstPage input |> should be NullOrEmptyString

[<Fact>]
let ``Title findings are fixable from the first page only when there is a title page`` () =
    let titleFromFirstPageFixable extraPages =
        use input = new TempPdf()
        createPdfWithFirstPage input.Path [ "Retirement Plan" ] extraPages
        VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1).Findings
        |> Seq.filter (fun a -> a.FixOptions.HasFlag PdfFixOptions.Title)
        |> Seq.map (fun a -> a.FixOptions.HasFlag PdfFixOptions.TitleFromFirstPage)
        |> Seq.distinct
        |> List.ofSeq

    titleFromFirstPageFixable 1 |> should equal [ true ]
    titleFromFirstPageFixable 0 |> should equal [ false ]

[<Fact>]
let ``Fix document title keeps a title the document already has`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithLinks input.Path Body [ "Read more", None ]
    (use document = new PdfDocument(new PdfReader(input.Path), new PdfWriter(output.Path))
     document.GetDocumentInfo().SetTitle "The Real Title" |> ignore)
    File.Copy(output.Path, input.Path, true)
    let fixer = VeraPdfValidator()

    fixer.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.Title) |> ignore

    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetDocumentInfo().GetTitle() |> should equal "The Real Title"

/// A copy of the passing PDF/UA-1 sample whose header is header (its first 8 bytes and the end of
/// line after them, rewritten byte for byte so nothing else in the file moves).
let private withHeader (path: string) (header: string) =
    let bytes = File.ReadAllBytes(corpusFile "pdfua1-pass.pdf")
    let replacement = Text.Encoding.ASCII.GetBytes header
    Array.blit replacement 0 bytes 0 replacement.Length
    File.WriteAllBytes(path, bytes)

/// (6.1 has no fix of its own: the copy is written for the sample's metadata, which the safe fixes change.)
[<Fact>]
let ``A fixed copy has a header with something after its version written again, clearing 6.1`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    withHeader input.Path "%PDF-1.7 "
    let validator = VeraPdfValidator()
    let failedRules (path: string) = validator.Validate(path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq
    failedRules input.Path |> should contain "6.1-1"

    let result = validator.Fix(input.Path, PdfProfile.PdfUa1, output.Path, FixNames.safe)

    result.WasFixed |> should be True
    failedRules output.Path |> should not' (contain "6.1-1")

[<Fact>]
let ``Fix can't open a header whose version is past 1.7, so 6.1 needs the file saved again`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    withHeader input.Path "%PDF-1.9"
    let validator = VeraPdfValidator()
    validator.Validate(input.Path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> should contain "6.1-1"

    (fun () -> validator.Fix(input.Path, PdfProfile.PdfUa1, output.Path, FixNames.safe) |> ignore)
    |> should throw typeof<VeraPdfValidationException>
    File.Exists output.Path |> should be False
