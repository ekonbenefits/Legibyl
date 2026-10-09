module Legibyl.Validation.Tests.EmptySpansTests

open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// An element (an indirect object, with its parent), as a tagged PDF has them.
let private element (document: PdfDocument) (tag: string) (page: PdfPage) (kids: PdfObject list) : PdfDictionary =
    let element = PdfDictionary()
    element.Put(PdfName.Type, PdfName.StructElem) |> ignore
    element.Put(PdfName.S, PdfName tag) |> ignore
    element.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
    let array = PdfArray()
    for kid in kids do
        match kid with
        | :? PdfDictionary as child -> child.Put(PdfName.P, element) |> ignore
        | _ -> ()
        array.Add kid
    element.Put(PdfName.K, array) |> ignore
    element.MakeIndirect document |> ignore
    element

/// Four paragraphs of words in spans, with an empty span standing for the space between them, as
/// some authoring apps write ("/S /Span /ActualText ( ) /K []"):
///   "General" (its replacement text) | spacer | "Information": the space can go after "General";
///   "Plan Year:" | spacer | "December 31" (its replacement text): before "December 31";
///   "Name:" | spacer, at the end of the paragraph: a space there separates nothing;
///   "Left" | spacer | "alone", neither with replacement text: the space can't go anywhere else.
let private createPdf (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let mutable mcid = -1
    let span (text: string) (x: float) (y: float) (replacement: bool) =
        mcid <- mcid + 1
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
        canvas.BeginMarkedContent(PdfName.Span, properties).BeginText().SetFontAndSize(font, 12f).MoveText(x, y).ShowText(text).EndText().EndMarkedContent()
        |> ignore
        let span = element document "Span" page [ PdfNumber mcid ]
        if replacement then span.Put(PdfName.ActualText, PdfString text) |> ignore
        span
    let spacer () =
        let spacer = element document "Span" page []
        spacer.Put(PdfName.ActualText, PdfString " ") |> ignore
        spacer
    let paragraphs =
        [ element document "P" page [ span "General" 72.0 700.0 true; spacer (); span "Information" 125.0 700.0 false ]
          element document "P" page [ span "Plan Year:" 72.0 650.0 false; spacer (); span "December 31" 135.0 650.0 true ]
          element document "P" page [ span "Name:" 72.0 600.0 false; spacer () ]
          element document "P" page [ span "Left" 72.0 550.0 false; spacer (); span "alone" 102.0 550.0 false ] ]
    let root = PdfDictionary()
    root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    root.MakeIndirect document |> ignore
    let tagged = element document "Document" page [ for paragraph in paragraphs -> paragraph ]
    tagged.Put(PdfName.P, root) |> ignore
    root.Put(PdfName.K, tagged) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, root) |> ignore
    document.GetCatalog().Put(PdfName.Lang, PdfString "en") |> ignore

/// WCAG 2.2's redundant spans: whether each is offered the fix.
let private redundantSpans (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions
    |> Seq.filter (fun a -> a.Specification.StartsWith "WCAG" && a.RuleId = "4.1.2-17")
    |> Seq.map (fun a -> a.FixOptions.HasFlag PdfFixOptions.EmptySpans)
    |> List.ofSeq

/// Each paragraph's spans: their replacement text, "" for none.
let private spans (path: string) =
    let root = VeraPdfValidator().ReadStructureTree path |> notNull
    [ for paragraph in (Seq.exactlyOne root.Children).Children ->
          [ for span in paragraph.Children -> span.ActualText |> Option.ofObj |> Option.defaultValue "" ] ]

[<Fact>]
let ``veraPDF's redundant spans that are empty are offered the fix; the one it can't keep the space for isn't`` () =
    use input = new TempPdf()
    createPdf input.Path

    redundantSpans input.Path |> should equal [ true; true; true; false ]

[<Fact>]
let ``the fix takes the empty spans out, the space between words kept beside them, and the page looks the same`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmptySpans)

    List.ofSeq result.AppliedFixes
    |> should
        equal
        [ "Removed 3 span tag(s) with nothing in them."
          "Kept the space 2 of them stood for between words, in the replacement text of the tag beside it." ]
    spans output.Path
    |> should equal [ [ "General "; "" ]; [ ""; " December 31" ]; [ "" ]; [ ""; " "; "" ] ]
    List.ofSeq result.NotApplied
    |> should
        equal
        [ "Left 1 empty span tag(s) that stand for a space between words: the text beside them isn't replacement text, so the space couldn't go there instead." ]
    redundantSpans output.Path |> should equal [ false ]
    use renderer = new PdfRegionRenderer()
    let page = { PageNumber = 1; Left = 0.0; Bottom = 0.0; Right = 1.0; Top = 1.0 }
    renderer.RenderPageWithHighlight(output.Path, page).PngBytes
    |> should equal (renderer.RenderPageWithHighlight(input.Path, page).PngBytes)

[<Fact>]
let ``a span with nothing in it but its own alt text is left alone`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    use again = new TempPdf ".again"
    createPdf input.Path
    do
        use document = new PdfDocument(new PdfReader(input.Path), new PdfWriter(output.Path))
        let root = document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot
        let first = root.GetAsDictionary(PdfName.K).GetAsArray(PdfName.K).GetAsDictionary 0
        first.GetAsArray(PdfName.K).GetAsDictionary(1).Put(PdfName.Alt, PdfString "A spacer") |> ignore

    let result = VeraPdfValidator().Fix(output.Path, PdfProfile.PdfUa1, again.Path, PdfFixOptions.EmptySpans)

    result.AppliedFixes |> should contain "Removed 2 span tag(s) with nothing in them."
    (spans again.Path)[0] |> should equal [ "General"; " "; "" ]

[<Fact>]
let ``once they're gone there's nothing left to remove: no copy is written`` () =
    use input = new TempPdf()
    use once = new TempPdf ".once"
    use twice = new TempPdf ".twice"
    createPdf input.Path
    VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, once.Path, PdfFixOptions.EmptySpans) |> ignore

    let result = VeraPdfValidator().Fix(once.Path, PdfProfile.PdfUa1, twice.Path, PdfFixOptions.EmptySpans)

    result.WasFixed |> should be False
    System.IO.File.Exists twice.Path |> should be False
