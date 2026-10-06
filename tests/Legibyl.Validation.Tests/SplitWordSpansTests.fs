module Legibyl.Validation.Tests.SplitWordSpansTests

open iText.IO.Font.Constants
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Font
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A span of a paragraph: its text, and how it's tagged and drawn.
type private Segment =
    { Text: string
      /// The span's own /ActualText (its text, as written).
      ActualText: bool
      /// Room left before it on the line, in points (0: straight after the span before).
      Gap: float
      /// Its content referred to by an /MCR dictionary with /MCID, not by a bare MCID.
      ByReference: bool
      /// A /Lang of its own, which replacement text on the block would hide.
      Lang: bool }

let private segment text = { Text = text; ActualText = true; Gap = 0.0; ByReference = false; Lang = false }

/// A tagged page of paragraphs, each a line of Span children drawn one after another.
let private createPdf (path: string) (paragraphs: Segment list list) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont StandardFonts.COURIER
    let mutable mcid = 0

    let element (tag: string) (kids: PdfObject list) =
        let element = PdfDictionary()
        element.Put(PdfName.Type, PdfName.StructElem) |> ignore
        element.Put(PdfName.S, PdfName tag) |> ignore
        element.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
        let children = PdfArray()
        for kid in kids do
            match kid with
            | :? PdfDictionary as child when not (isNull (child.GetAsName PdfName.S)) -> child.Put(PdfName.P, element) |> ignore
            | _ -> ()
            children.Add kid
        element.Put(PdfName.K, children) |> ignore
        element.MakeIndirect document |> ignore
        element

    let paragraph (y: float) (segments: Segment list) =
        let mutable x = 72.0
        let spans =
            [ for segment in segments do
                  x <- x + segment.Gap
                  let properties = PdfDictionary()
                  properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
                  canvas.BeginMarkedContent(PdfName.Span, properties) |> ignore
                  canvas.BeginText().SetFontAndSize(font, 12f).MoveText(x, y).ShowText(segment.Text).EndText() |> ignore
                  canvas.EndMarkedContent() |> ignore
                  x <- x + float (font.GetWidth(segment.Text, 12f))
                  let content: PdfObject =
                      if segment.ByReference then
                          let reference = PdfDictionary()
                          reference.Put(PdfName.Type, PdfName.MCR) |> ignore
                          reference.Put(PdfName.MCID, PdfNumber mcid) |> ignore
                          reference.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
                          reference
                      else
                          PdfNumber mcid
                  let span = element "Span" [ content ]
                  if segment.ActualText then span.Put(PdfName.ActualText, PdfString(segment.Text)) |> ignore
                  if segment.Lang then span.Put(PdfName.Lang, PdfString "en-US") |> ignore
                  mcid <- mcid + 1
                  yield span ]
        element "P" (spans |> List.map (fun span -> span :> PdfObject))

    let blocks = paragraphs |> List.mapi (fun i segments -> paragraph (700.0 - 30.0 * float i) segments)
    let root = PdfDictionary()
    root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    root.MakeIndirect document |> ignore
    let doc = element "Document" (blocks |> List.map (fun paragraph -> paragraph :> PdfObject))
    doc.Put(PdfName.P, root) |> ignore
    root.Put(PdfName.K, doc) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, root) |> ignore
    let markInfo = PdfDictionary()
    markInfo.Put(PdfName.Marked, PdfBoolean.TRUE) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.MarkInfo, markInfo) |> ignore

/// The paragraphs the tests read: two split words to fix, and three that aren't splits.
let private paragraphs =
    [ [ segment "The "; { segment "Em" with ActualText = false }; segment "ployer will contribute." ]
      [ segment "Two "; segment "words stay separate." ]
      [ segment "re"; segment "-entry remains hyphenated." ]
      [ segment "inter"; segment ""; segment "national text." ] ]

let private splitWordWarnings (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun finding -> finding.Specification = "Legibyl" && finding.TestNumber = 14)
    |> List.ofSeq

let private paragraphTexts (path: string) =
    let root = VeraPdfValidator().ReadStructureTree(path) |> notNull
    let rec descendants (node: StructureNode) =
        seq {
            yield node
            for child in node.Children do
                yield! descendants child
        }
    root.Children
    |> Seq.collect descendants
    |> Seq.filter (fun node -> node.Tag = "P")
    |> Seq.map (_.ActualText >> Option.ofObj)
    |> List.ofSeq

let private render (path: string) =
    use renderer = new Legibyl.Rendering.PdfRegionRenderer()
    let offPage = { PageNumber = 1; Left = -20.0; Bottom = -20.0; Right = -10.0; Top = -10.0 }
    use bitmap = SkiaSharp.SKBitmap.Decode(renderer.RenderPageWithHighlight(path, offPage).PngBytes)
    bitmap.Pixels

[<Fact>]
let ``synthetic tagged PDF flags only span boundaries inside a word`` () =
    use input = new TempPdf()
    createPdf input.Path paragraphs

    let warnings = splitWordWarnings input.Path

    warnings |> should haveLength 2
    warnings |> Seq.forall (fun warning -> warning.IsFixable) |> should be True
    warnings |> Seq.forall (fun warning -> warning.FixOptions = PdfFixOptions.JoinSplitWords) |> should be True

[<Fact>]
let ``fix writes the exact reconstructed text as block replacement text, and the page looks the same`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path paragraphs

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.JoinSplitWords)

    result.WasFixed |> should be True
    result.AppliedFixes |> should contain "Added replacement text to 2 text blocks whose Span tags split words."
    paragraphTexts output.Path
    |> should equal [ Some "The Employer will contribute."; None; None; Some "international text." ]
    splitWordWarnings output.Path |> should be Empty
    render output.Path |> should equal (render input.Path)

[<Fact>]
let ``spans drawn a word space apart aren't a split word, whatever their text`` () =
    use input = new TempPdf()
    createPdf input.Path [ [ segment "Two"; { segment "words" with Gap = 7.2 } ] ]

    splitWordWarnings input.Path |> should be Empty

[<Fact>]
let ``a span whose content is an MCR with its MCID is read, and fixed`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path [ [ { segment "Ap" with ActualText = false; ByReference = true }; { segment "ple trees." with ActualText = false } ] ]

    splitWordWarnings input.Path |> List.map _.FixOptions |> should equal [ PdfFixOptions.JoinSplitWords ]
    VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.JoinSplitWords) |> ignore
    paragraphTexts output.Path |> should equal [ Some "Apple trees." ]

[<Fact>]
let ``a split word is warned about but not fixed where replacement text would hide a span's language`` () =
    use input = new TempPdf()
    createPdf input.Path [ [ segment "Green"; segment "house"; { segment " Gewächshaus." with Lang = true } ] ]

    let warnings = splitWordWarnings input.Path

    warnings |> List.map _.FixOptions |> should equal [ PdfFixOptions.None ]
    warnings |> List.map _.IsFixable |> should equal [ false ]

[<Fact>]
let ``a file with no split words isn't changed, and no copy is written`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path [ [ segment "Two "; segment "words stay separate." ] ]

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.JoinSplitWords)

    result.WasFixed |> should be False
    System.IO.File.Exists output.Path |> should be False
