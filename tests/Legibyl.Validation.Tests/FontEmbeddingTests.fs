module Legibyl.Validation.Tests.FontEmbeddingTests

open System.IO
open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private sentence = "The garden opens at 8, Thursday."

/// A page with a line in each of fonts (standard 14 names; iText embeds none of them), with their
/// /Widths or without, and then change done to each font's dictionary.
let private createText (path: string) (fonts: string list) (widths: bool) (change: PdfDocument -> PdfDictionary -> unit) =
    do
        use document = new PdfDocument(new PdfWriter(path + ".plain.pdf"))
        let page = document.AddNewPage PageSize.LETTER
        let canvas = PdfCanvas page
        fonts
        |> List.iteri (fun i name ->
            let font = PdfFontFactory.CreateFont name
            let text = if name = StandardFonts.SYMBOL then "abgdp" elif name = StandardFonts.ZAPFDINGBATS then "3456" else sentence
            canvas.BeginText().SetFontAndSize(font, 18f).MoveText(72.0, 700.0 - 40.0 * float i).ShowText(text).EndText() |> ignore)
    do
        use document = new PdfDocument(new PdfReader(path + ".plain.pdf"), new PdfWriter(path))
        for n in 1 .. document.GetNumberOfPdfObjects() - 1 do
            match document.GetPdfObject n with
            | :? PdfDictionary as d when PdfName.Font.Equals(d.GetAsName PdfName.Type) ->
                if widths then
                    // The standard metrics, as a PDF that gives them writes them.
                    let program = iText.IO.Font.FontProgramFactory.CreateFont(d.GetAsName(PdfName.BaseFont).GetValue())
                    let encoding = iText.IO.Font.FontEncoding.CreateFontEncoding "WinAnsiEncoding"
                    let width code = match program.GetGlyph(encoding.GetUnicode code) with null -> 0 | glyph -> glyph.GetWidth()
                    d.Put(PdfName.FirstChar, PdfNumber 32) |> ignore
                    d.Put(PdfName.LastChar, PdfNumber 255) |> ignore
                    d.Put(PdfName.Widths, PdfArray([| for code in 32..255 -> width code |])) |> ignore
                change document d
            | _ -> ()
    File.Delete(path + ".plain.pdf")

let private failed (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

let private fix (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.EmbedFonts) |> _.AppliedFixes |> List.ofSeq

/// The fonts' dictionaries: each one's /BaseFont, and whether it's embedded.
let private fonts (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    [ for n in 1 .. document.GetNumberOfPdfObjects() - 1 do
          match document.GetPdfObject n with
          | :? PdfDictionary as d when PdfName.Font.Equals(d.GetAsName PdfName.Type) ->
              let descriptor = d.GetAsDictionary PdfName.FontDescriptor
              (match d.GetAsName PdfName.BaseFont with null -> "(Type 3)" | name -> name.GetValue()), not (isNull descriptor) && not (isNull (descriptor.Get PdfName.FontFile2))
          | _ -> () ]
    |> List.sort

/// Where ink is on each line of the page: the leftmost and rightmost dark column in each band of
/// rows, as rendered.
let private inkExtents (path: string) =
    use renderer = new PdfRegionRenderer()
    let offPage = { PageNumber = 1; Left = -20.0; Bottom = -20.0; Right = -10.0; Top = -10.0 }
    use bitmap = SkiaSharp.SKBitmap.Decode(renderer.RenderPageWithHighlight(path, offPage).PngBytes)
    let dark x y = let c = bitmap.GetPixel(x, y) in int c.Red + int c.Green + int c.Blue < 384
    let rows = [ for y in 0 .. bitmap.Height - 1 do if List.exists (fun x -> dark x y) [ 0 .. bitmap.Width - 1 ] then y ]
    // Bands of consecutive inked rows: a line each.
    let bands =
        rows
        |> List.fold (fun bands y -> match bands with (top, bottom) :: rest when y - bottom <= 4 -> (top, y) :: rest | _ -> (y, y) :: bands) []
        |> List.rev
    [ for top, bottom in bands ->
          let columns = [ for x in 0 .. bitmap.Width - 1 do if List.exists (fun y -> dark x y) [ top..bottom ] then x ]
          List.min columns, List.max columns ]

[<Fact>]
let ``fonts are known by their names, whatever the style's spelled`` () =
    [ "Helvetica"; "Helvetica-BoldOblique"; "ABCDEF+Times-Roman"; "Times-BoldItalic"; "Courier-Oblique"
      "ArialMT"; "Arial,Bold"; "Arial-BoldItalicMT"; "TimesNewRomanPS-ItalicMT"; "Times New Roman"; "CourierNewPSMT"
      "Symbol"; "ZapfDingbats"; "ArialNarrow"; "HelveticaNeue"; "Calibri"; "Symbol-Bold" ]
    |> List.map FontEmbedding.identify
    |> should
        equal
        [ Some(FontEmbedding.Sans, false, false, false)
          Some(FontEmbedding.Sans, true, true, false)
          Some(FontEmbedding.Serif, false, false, false)
          Some(FontEmbedding.Serif, true, true, false)
          Some(FontEmbedding.Mono, false, true, false)
          Some(FontEmbedding.Sans, false, false, true)
          Some(FontEmbedding.Sans, true, false, true)
          Some(FontEmbedding.Sans, true, true, true)
          Some(FontEmbedding.Serif, false, true, true)
          Some(FontEmbedding.Serif, false, false, true)
          Some(FontEmbedding.Mono, false, false, true)
          Some(FontEmbedding.Symbol, false, false, false)
          Some(FontEmbedding.Dingbats, false, false, false)
          None
          None
          None
          None ]

[<Fact>]
let ``the PostScript fonts are embedded as URW's, the page drawn to the same measure, and they pass`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createText input.Path [ StandardFonts.HELVETICA; StandardFonts.TIMES_BOLD; StandardFonts.COURIER; StandardFonts.SYMBOL; StandardFonts.ZAPFDINGBATS ] false (fun _ _ -> ())

    let report = fix input.Path output.Path

    failed input.Path |> should contain "7.21.4.1-1"
    let after = failed output.Path
    // No font rule fails: Symbol and ZapfDingbats say what their codes are, from their encodings.
    after |> Set.filter (fun rule -> rule.StartsWith "7.21") |> should be Empty
    fonts output.Path |> List.map (fun (name, embedded) -> name.Substring 7, embedded) |> List.sort
    |> should equal [ "D050000L", true; "NimbusMonoPS-Regular", true; "NimbusRoman-Bold", true; "NimbusSans-Regular", true; "StandardSymbolsPS", true ]
    report |> should contain "Embedded Nimbus Sans in place of Helvetica, which wasn't embedded: an open font with its glyphs, each as wide."
    report |> should contain "Embedded Standard Symbols PS in place of Symbol, which wasn't embedded: an open font with its glyphs, each as wide."
    // Each line's ink starts and ends where it did, to within a pixel or two of the glyphs' shapes.
    let before, now = inkExtents input.Path, inkExtents output.Path
    now.Length |> should equal before.Length
    List.zip before now |> List.forall (fun ((l1, r1), (l2, r2)) -> abs (l1 - l2) <= 2 && abs (r1 - r2) <= 2) |> should be True

[<Fact>]
let ``Microsoft's fonts are embedded as Liberation's`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createText input.Path [ StandardFonts.HELVETICA; StandardFonts.TIMES_ROMAN ] true (fun document font ->
        let name = if font.GetAsName(PdfName.BaseFont).GetValue() = "Helvetica" then "ArialMT" else "TimesNewRomanPSMT"
        font.Put(PdfName.BaseFont, PdfName name) |> ignore
        font.Put(PdfName.Subtype, PdfName.TrueType) |> ignore
        // As Office writes a font it doesn't embed: a descriptor without a font program.
        let descriptor = PdfDictionary()
        descriptor.Put(PdfName.Type, PdfName.FontDescriptor) |> ignore
        descriptor.Put(PdfName.FontName, PdfName name) |> ignore
        descriptor.Put(PdfName.Flags, PdfNumber 32) |> ignore
        descriptor.Put(PdfName.FontBBox, PdfArray([| -665; -325; 2000; 1040 |])) |> ignore
        descriptor.Put(PdfName.ItalicAngle, PdfNumber 0) |> ignore
        descriptor.Put(PdfName.Ascent, PdfNumber 905) |> ignore
        descriptor.Put(PdfName.Descent, PdfNumber -212) |> ignore
        descriptor.Put(PdfName.CapHeight, PdfNumber 716) |> ignore
        descriptor.Put(PdfName.StemV, PdfNumber 80) |> ignore
        font.Put(PdfName.FontDescriptor, descriptor.MakeIndirect document) |> ignore)

    let report = fix input.Path output.Path

    failed output.Path |> Set.filter (fun rule -> rule.StartsWith "7.21") |> should be Empty
    fonts output.Path |> List.map (fun (name, embedded) -> name.Substring 7, embedded)
    |> should equal [ "LiberationSans", true; "LiberationSerif", true ]
    let before, now = inkExtents input.Path, inkExtents output.Path
    List.zip before now |> List.forall (fun ((l1, r1), (l2, r2)) -> abs (l1 - l2) <= 2 && abs (r1 - r2) <= 2) |> should be True
    report |> should contain "Embedded Liberation Sans in place of ArialMT, which wasn't embedded: an open font with its glyphs, each as wide."
    report |> should contain "Embedded Liberation Serif in place of TimesNewRomanPSMT, which wasn't embedded: an open font with its glyphs, each as wide."

[<Fact>]
let ``a font in the standard encoding is written in WinAnsi's, with what differs`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // Standard encoding's code 39 is a right quote (WinAnsi's is a straight one), and 96 a left one.
    createText input.Path [ StandardFonts.HELVETICA ] false (fun _ font -> font.Remove PdfName.Encoding |> ignore)

    fix input.Path output.Path |> ignore

    failed output.Path |> Set.filter (fun rule -> rule.StartsWith "7.21") |> should be Empty
    use document = new PdfDocument(new PdfReader(output.Path))
    let font = document.GetPage(1).GetResources().GetResource(PdfName.Font) |> fun fonts -> fonts.GetAsDictionary(Seq.head (fonts.KeySet()))
    let encoding = font.GetAsDictionary PdfName.Encoding
    encoding.GetAsName PdfName.BaseEncoding |> should equal PdfName.WinAnsiEncoding
    let differences = encoding.GetAsArray PdfName.Differences
    let at (code: int) =
        // The name /Differences gives code.
        let mutable current = -1
        let mutable found = None
        for i in 0 .. differences.Size() - 1 do
            match differences.Get i with
            | :? PdfNumber as n -> current <- n.IntValue()
            | :? PdfName as name ->
                if current = code then found <- Some(name.GetValue())
                current <- current + 1
            | _ -> ()
        found
    at 39 |> should equal (Some "quoteright")
    at 96 |> should equal (Some "quoteleft")
    at 65 |> should equal None

[<Fact>]
let ``a font whose widths no bundled font has is left as it is, and reported`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createText input.Path [ StandardFonts.HELVETICA ] false (fun _ font ->
        font.Put(PdfName.FirstChar, PdfNumber 0) |> ignore
        font.Put(PdfName.LastChar, PdfNumber 255) |> ignore
        font.Put(PdfName.Widths, PdfArray(Array.create 256 400)) |> ignore)

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmbedFonts)

    // Said as not applied: nothing changed, so no copy is written.
    result.NotApplied |> should contain "Helvetica isn't embedded, and was left as it is (no font Legibyl brings has its glyphs at its widths): export the PDF again with fonts embedded."
    result.AppliedFixes |> should be Empty
    result.OutputPath |> should equal null

[<Fact>]
let ``a font no bundled font stands in for is left as it is, and reported`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createText input.Path [ StandardFonts.HELVETICA ] false (fun _ font -> font.Put(PdfName.BaseFont, PdfName "Calibri") |> ignore)

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmbedFonts)

    result.OutputPath |> should equal null
    result.NotApplied |> should contain "Calibri isn't embedded, and was left as it is (no font Legibyl brings stands in for it): export the PDF again with fonts embedded."
    FixNames.nameOf PdfFixOptions.EmbedFonts |> should equal "embed-fonts"
    FixNames.safe.HasFlag PdfFixOptions.EmbedFonts |> should be False

[<Fact>]
let ``a form's font is embedded whole, and only if every code it gives a width fits`` () =
    // Widths as the standard metrics give them, Z's (which the page doesn't draw) aside.
    let create (path: string) (zWidth: int) =
        createText path [ StandardFonts.HELVETICA ] true (fun document font ->
            let widths = font.GetAsArray PdfName.Widths
            widths.Set(int 'Z' - 32, PdfNumber zWidth) |> ignore
            // A form whose fields fill in with the font.
            let resources = PdfDictionary()
            resources.Put(PdfName.Font, PdfDictionary(dict [ PdfName "Helv", font :> PdfObject ])) |> ignore
            let form = PdfDictionary()
            form.Put(PdfName.Fields, PdfArray()) |> ignore
            form.Put(PdfName.DR, resources) |> ignore
            document.GetCatalog().GetPdfObject().Put(PdfName.AcroForm, form) |> ignore)
    use input = new TempPdf()
    use output = new TempPdf()
    use wrongZ = new TempPdf()
    use notFixed = new TempPdf()
    create input.Path 611
    create wrongZ.Path 400

    fix input.Path output.Path |> ignore
    let result = VeraPdfValidator().Fix(wrongZ.Path, PdfProfile.PdfUa1, notFixed.Path, PdfFixOptions.EmbedFonts)

    // The whole font: no subset tag.
    fonts output.Path |> should equal [ "NimbusSans-Regular", true ]
    result.OutputPath |> should equal null
    result.NotApplied |> should contain "Helvetica isn't embedded, and was left as it is (no font Legibyl brings has its glyphs at its widths): export the PDF again with fonts embedded."

[<Fact>]
let ``only the glyphs the page draws are embedded`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createText input.Path [ StandardFonts.HELVETICA ] true (fun _ _ -> ())

    fix input.Path output.Path |> ignore

    let whole = FileInfo(Path.Combine(FontEmbedding.fontsFolder (), "NimbusSans-Regular.ttf")).Length
    let grown = FileInfo(output.Path).Length - FileInfo(input.Path).Length
    grown |> should be (lessThan (whole / 10L))

/// The (only) font on page 1 of path.
let private pageFont (document: PdfDocument) =
    let fonts = document.GetPage(1).GetResources().GetResource PdfName.Font
    fonts.GetAsDictionary(Seq.head (fonts.KeySet()))

[<Fact>]
let ``a default width for codes outside /Widths is kept`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // Widths for A to Z only, and a space as wide as /MissingWidth says.
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        let page = document.AddNewPage PageSize.LETTER
        let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
        PdfCanvas(page).BeginText().SetFontAndSize(font, 18f).MoveText(72.0, 700.0).ShowText("A B C").EndText() |> ignore
    use changed = new TempPdf()
    do
        use document = new PdfDocument(new PdfReader(input.Path), new PdfWriter(changed.Path))
        let font = pageFont document
        let program = iText.IO.Font.FontProgramFactory.CreateFont StandardFonts.HELVETICA
        font.Put(PdfName.FirstChar, PdfNumber 65) |> ignore
        font.Put(PdfName.LastChar, PdfNumber 90) |> ignore
        font.Put(PdfName.Widths, PdfArray([| for code in 65..90 -> program.GetGlyph(code).GetWidth() |])) |> ignore
        let descriptor = PdfDictionary()
        descriptor.Put(PdfName.Type, PdfName.FontDescriptor) |> ignore
        descriptor.Put(PdfName.FontName, PdfName "Helvetica") |> ignore
        descriptor.Put(PdfName.Flags, PdfNumber 32) |> ignore
        descriptor.Put(PdfName.MissingWidth, PdfNumber 278) |> ignore
        font.Put(PdfName.FontDescriptor, descriptor.MakeIndirect document) |> ignore

    fix changed.Path output.Path |> ignore

    failed output.Path |> Set.filter (fun rule -> rule.StartsWith "7.21") |> should be Empty
    use document = new PdfDocument(new PdfReader(output.Path))
    (pageFont document).GetAsDictionary(PdfName.FontDescriptor).GetAsNumber(PdfName.MissingWidth).IntValue() |> should equal 278
    let before, now = inkExtents changed.Path, inkExtents output.Path
    List.zip before now |> List.forall (fun ((l1, r1), (l2, r2)) -> abs (l1 - l2) <= 2 && abs (r1 - r2) <= 2) |> should be True

[<Fact>]
let ``a font drawing a character the Glyph List doesn't name is left, its encoding not rewritable`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // Code 65 is Ţ (U+021A), which Nimbus Sans has; the base encoding is Standard, so it would be
    // rewritten as WinAnsi's, and a uniXXXX name in it fails 7.21.6-2.
    let nimbus = iText.IO.Font.FontProgramFactory.CreateFont(Path.Combine(FontEmbedding.fontsFolder (), "NimbusSans-Regular.ttf"))
    createText input.Path [ StandardFonts.HELVETICA ] true (fun _ font ->
        let widths = font.GetAsArray PdfName.Widths
        widths.Set(65 - 32, PdfNumber(nimbus.GetGlyph(0x021A).GetWidth())) |> ignore
        let encoding = PdfDictionary()
        encoding.Put(PdfName.Differences, PdfArray([| PdfNumber 65 :> PdfObject; PdfName "uni021A" |])) |> ignore
        font.Put(PdfName.Encoding, encoding) |> ignore)
    use drawsA = new TempPdf()
    do
        use document = new PdfDocument(new PdfReader(input.Path), new PdfWriter(drawsA.Path))
        let content = document.GetPage(1).GetFirstContentStream()
        let text = System.Text.Encoding.Latin1.GetString(content.GetBytes())
        content.SetData(System.Text.Encoding.Latin1.GetBytes(text.Replace("(The garden opens at 8, Thursday.)", "(A)")))

    let result = VeraPdfValidator().Fix(drawsA.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmbedFonts)

    result.OutputPath |> should equal null
    result.NotApplied |> should contain "Helvetica isn't embedded, and was left as it is (its encoding can't be kept): export the PDF again with fonts embedded."

[<Fact>]
let ``a font only drawing invisible text (an OCR layer) is left as it is`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        let page = document.AddNewPage PageSize.LETTER
        let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
        PdfCanvas(page).BeginText().SetTextRenderingMode(3).SetFontAndSize(font, 18f).MoveText(72.0, 700.0).ShowText(sentence).EndText() |> ignore

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmbedFonts)

    failed input.Path |> should not' (contain "7.21.4.1-1")
    result.OutputPath |> should equal null
    result.NotApplied |> List.ofSeq |> List.exists (fun line -> line.Contains "isn't embedded") |> should be False

[<Fact>]
let ``a font a Type 3 font's glyphs draw with is embedded`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        let page = document.AddNewPage PageSize.LETTER
        // A Type 3 font whose one glyph, x, draws Helvetica's X.
        let helvetica = PdfDictionary()
        helvetica.Put(PdfName.Type, PdfName.Font) |> ignore
        helvetica.Put(PdfName.Subtype, PdfName.Type1) |> ignore
        helvetica.Put(PdfName.BaseFont, PdfName "Helvetica") |> ignore
        helvetica.Put(PdfName.Encoding, PdfName.WinAnsiEncoding) |> ignore
        let procedure = PdfStream(System.Text.Encoding.ASCII.GetBytes "600 0 d0 BT /H 700 Tf 0 0 Td (X) Tj ET")
        let type3 = PdfDictionary()
        type3.Put(PdfName.Type, PdfName.Font) |> ignore
        type3.Put(PdfName.Subtype, PdfName.Type3) |> ignore
        type3.Put(PdfName.FontBBox, PdfArray([| 0; 0; 1000; 1000 |])) |> ignore
        type3.Put(PdfName.FontMatrix, PdfArray([| 0.001; 0.0; 0.0; 0.001; 0.0; 0.0 |])) |> ignore
        type3.Put(PdfName.CharProcs, PdfDictionary(dict [ PdfName "x", procedure.MakeIndirect document ])) |> ignore
        type3.Put(PdfName.Encoding, PdfDictionary(dict [ PdfName.Differences, PdfArray([| PdfNumber 120 :> PdfObject; PdfName "x" |]) :> PdfObject ])) |> ignore
        type3.Put(PdfName.FirstChar, PdfNumber 120) |> ignore
        type3.Put(PdfName.LastChar, PdfNumber 120) |> ignore
        type3.Put(PdfName.Widths, PdfArray([| 600 |])) |> ignore
        let fontResources = PdfDictionary(dict [ PdfName "H", helvetica.MakeIndirect document ])
        type3.Put(PdfName.Resources, PdfDictionary(dict [ PdfName.Font, fontResources :> PdfObject ])) |> ignore
        let pageFonts = PdfDictionary(dict [ PdfName "T3", type3.MakeIndirect document ])
        page.GetPdfObject().Put(PdfName.Resources, PdfDictionary(dict [ PdfName.Font, pageFonts :> PdfObject ])) |> ignore
        page.GetPdfObject().Put(PdfName.Contents, PdfStream(System.Text.Encoding.ASCII.GetBytes "BT /T3 18 Tf 72 700 Td (xx) Tj ET").MakeIndirect document) |> ignore

    fix input.Path output.Path |> ignore

    failed input.Path |> should contain "7.21.4.1-1"
    failed output.Path |> should not' (contain "7.21.4.1-1")
    fonts output.Path |> List.exists (fun (name, embedded) -> name.EndsWith "+NimbusSans-Regular" && embedded) |> should be True

[<Fact>]
let ``a form field's appearance without resources of its own uses the form's, and fonts are still subset`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        let page = document.AddNewPage PageSize.LETTER
        let times = PdfFontFactory.CreateFont StandardFonts.TIMES_ROMAN
        PdfCanvas(page).BeginText().SetFontAndSize(times, 18f).MoveText(72.0, 700.0).ShowText(sentence).EndText() |> ignore
        let helvetica = PdfDictionary()
        helvetica.Put(PdfName.Type, PdfName.Font) |> ignore
        helvetica.Put(PdfName.Subtype, PdfName.Type1) |> ignore
        helvetica.Put(PdfName.BaseFont, PdfName "Helvetica") |> ignore
        helvetica.Put(PdfName.Encoding, PdfName.WinAnsiEncoding) |> ignore
        helvetica.MakeIndirect document |> ignore
        // A text field whose appearance names /Helv, which only the form's resources have.
        let appearance = PdfStream(System.Text.Encoding.ASCII.GetBytes "/Tx BMC BT /Helv 12 Tf 2 4 Td (Hello) Tj ET EMC")
        appearance.Put(PdfName.Type, PdfName.XObject) |> ignore
        appearance.Put(PdfName.Subtype, PdfName.Form) |> ignore
        appearance.Put(PdfName.BBox, PdfArray([| 0; 0; 200; 20 |])) |> ignore
        let widget = PdfDictionary()
        widget.Put(PdfName.Type, PdfName.Annot) |> ignore
        widget.Put(PdfName.Subtype, PdfName.Widget) |> ignore
        widget.Put(PdfName.FT, PdfName.Tx) |> ignore
        widget.Put(PdfName.T, PdfString "name") |> ignore
        widget.Put(PdfName.Rect, PdfArray([| 72; 600; 272; 620 |])) |> ignore
        widget.Put(PdfName.AP, PdfDictionary(dict [ PdfName.N, appearance.MakeIndirect document ])) |> ignore
        widget.MakeIndirect document |> ignore
        page.GetPdfObject().Put(PdfName.Annots, PdfArray([| widget :> PdfObject |])) |> ignore
        let fonts = PdfDictionary(dict [ PdfName "Helv", helvetica :> PdfObject ])
        let form = PdfDictionary()
        form.Put(PdfName.Fields, PdfArray([| widget :> PdfObject |])) |> ignore
        form.Put(PdfName.DR, PdfDictionary(dict [ PdfName.Font, fonts :> PdfObject ])) |> ignore
        document.GetCatalog().GetPdfObject().Put(PdfName.AcroForm, form) |> ignore

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmbedFonts ||| PdfFixOptions.FontUnicode)

    result.NotApplied |> List.ofSeq |> List.filter (fun line -> line.Contains "couldn't be read") |> should equal List.empty<string>
    // The page's font a subset (tagged), the form's whole.
    fonts output.Path |> List.map (fun (name, embedded) -> (if name.Contains "+" then "subset " + name.Substring 7 else name), embedded)
    |> List.sort
    |> should equal [ "NimbusSans-Regular", true; "subset NimbusRoman-Regular", true ]

/// A Helvetica font dictionary, not embedded.
let private helveticaDictionary (document: PdfDocument) =
    let font = PdfDictionary()
    font.Put(PdfName.Type, PdfName.Font) |> ignore
    font.Put(PdfName.Subtype, PdfName.Type1) |> ignore
    font.Put(PdfName.BaseFont, PdfName "Helvetica") |> ignore
    font.Put(PdfName.Encoding, PdfName.WinAnsiEncoding) |> ignore
    font.MakeIndirect document :?> PdfDictionary

/// A one-page PDF whose page draws content, with resources made by resources.
let private createRaw (path: string) (content: string) (resources: PdfDocument -> PdfDictionary) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage PageSize.LETTER
    page.GetPdfObject().Put(PdfName.Resources, resources document) |> ignore
    page.GetPdfObject().Put(PdfName.Contents, PdfStream(System.Text.Encoding.ASCII.GetBytes content).MakeIndirect document) |> ignore

/// How many dark pixels the page renders with.
let private ink (path: string) =
    use renderer = new PdfRegionRenderer()
    let offPage = { PageNumber = 1; Left = -20.0; Bottom = -20.0; Right = -10.0; Top = -10.0 }
    use bitmap = SkiaSharp.SKBitmap.Decode(renderer.RenderPageWithHighlight(path, offPage).PngBytes)
    bitmap.Pixels |> Array.filter (fun c -> int c.Red + int c.Green + int c.Blue < 384) |> Array.length

let private noFontFailures (path: string) =
    failed path |> Set.filter (fun rule -> rule.StartsWith "7.21" && not (rule.StartsWith "7.21.7")) |> should be Empty

[<Fact>]
let ``a Type 3 font without resources of its own draws with the page's, whose glyphs are kept`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createRaw input.Path "BT /H 18 Tf 72 700 Td (ab) Tj ET BT /T3 18 Tf 72 600 Td (xxxx) Tj ET" (fun document ->
        let procedure = PdfStream(System.Text.Encoding.ASCII.GetBytes "600 0 d0 BT /H 700 Tf 0 0 Td (X) Tj ET")
        let type3 = PdfDictionary()
        type3.Put(PdfName.Type, PdfName.Font) |> ignore
        type3.Put(PdfName.Subtype, PdfName.Type3) |> ignore
        type3.Put(PdfName.FontBBox, PdfArray([| 0; 0; 1000; 1000 |])) |> ignore
        type3.Put(PdfName.FontMatrix, PdfArray([| 0.001; 0.0; 0.0; 0.001; 0.0; 0.0 |])) |> ignore
        type3.Put(PdfName.CharProcs, PdfDictionary(dict [ PdfName "x", procedure.MakeIndirect document ])) |> ignore
        type3.Put(PdfName.Encoding, PdfDictionary(dict [ PdfName.Differences, PdfArray([| PdfNumber 120 :> PdfObject; PdfName "x" |]) :> PdfObject ])) |> ignore
        type3.Put(PdfName.FirstChar, PdfNumber 120) |> ignore
        type3.Put(PdfName.LastChar, PdfNumber 120) |> ignore
        type3.Put(PdfName.Widths, PdfArray([| 600 |])) |> ignore
        let fonts = PdfDictionary(dict [ PdfName "H", helveticaDictionary document :> PdfObject; PdfName "T3", type3.MakeIndirect document ])
        PdfDictionary(dict [ PdfName.Font, fonts :> PdfObject ]))

    fix input.Path output.Path |> ignore

    failed output.Path |> should not' (contain "7.21.4.1-1")
    // X, which only the Type 3 glyphs draw, is in the subset.
    noFontFailures output.Path
    let before, after = ink input.Path, ink output.Path
    float after |> should be (greaterThan (0.9 * float before))

[<Fact>]
let ``a pattern inside a form draws with glyphs that are kept`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createRaw input.Path "q 1 0 0 1 72 300 cm /Fm Do Q BT /H 18 Tf 72 700 Td (ab) Tj ET" (fun document ->
        let helvetica = helveticaDictionary document
        let pattern = PdfStream(System.Text.Encoding.ASCII.GetBytes "BT /H 20 Tf 10 10 Td (XYZW) Tj ET")
        pattern.Put(PdfName.Type, PdfName.Pattern) |> ignore
        pattern.Put(PdfName.PatternType, PdfNumber 1) |> ignore
        pattern.Put(PdfName.PaintType, PdfNumber 1) |> ignore
        pattern.Put(PdfName.TilingType, PdfNumber 1) |> ignore
        pattern.Put(PdfName.BBox, PdfArray([| 0; 0; 100; 40 |])) |> ignore
        pattern.Put(PdfName.XStep, PdfNumber 100) |> ignore
        pattern.Put(PdfName.YStep, PdfNumber 40) |> ignore
        pattern.Put(PdfName.Resources, PdfDictionary(dict [ PdfName.Font, PdfDictionary(dict [ PdfName "H", helvetica :> PdfObject ]) :> PdfObject ])) |> ignore
        let form = PdfStream(System.Text.Encoding.ASCII.GetBytes "/Pattern cs /P1 scn 0 0 300 200 re f")
        form.Put(PdfName.Type, PdfName.XObject) |> ignore
        form.Put(PdfName.Subtype, PdfName.Form) |> ignore
        form.Put(PdfName.BBox, PdfArray([| 0; 0; 300; 200 |])) |> ignore
        form.Put(PdfName.Resources, PdfDictionary(dict [ PdfName.Pattern, PdfDictionary(dict [ PdfName "P1", pattern.MakeIndirect document ]) :> PdfObject ])) |> ignore
        PdfDictionary(
            dict
                [ PdfName.Font, PdfDictionary(dict [ PdfName "H", helvetica :> PdfObject ]) :> PdfObject
                  PdfName.XObject, PdfDictionary(dict [ PdfName "Fm", form.MakeIndirect document ]) :> PdfObject ]
        ))

    fix input.Path output.Path |> ignore

    failed output.Path |> should not' (contain "7.21.4.1-1")
    // (veraPDF doesn't look inside patterns: the page drawn shows the glyphs are there.)
    let before, after = ink input.Path, ink output.Path
    float after |> should be (greaterThan (0.9 * float before))

[<Fact>]
let ``a kept encoding's names have to be the Glyph List's: one that isn't is rewritten, or the font left`` () =
    // WinAnsi with a uniXXXX name: at an unused code, the encoding is rewritten without it; at a
    // code the page draws, the font is left (veraPDF takes no uniXXXX name in a TrueType font's).
    let create (path: string) (code: int) (text: string) =
        createText path [ StandardFonts.HELVETICA ] true (fun _ font ->
            let encoding = PdfDictionary()
            encoding.Put(PdfName.BaseEncoding, PdfName.WinAnsiEncoding) |> ignore
            encoding.Put(PdfName.Differences, PdfArray([| PdfNumber code :> PdfObject; PdfName "uni021A" |])) |> ignore
            font.Put(PdfName.Encoding, encoding) |> ignore)
        if text <> sentence then
            let copy = path + ".copy.pdf"
            do
                use document = new PdfDocument(new PdfReader(path), new PdfWriter(copy))
                let content = document.GetPage(1).GetFirstContentStream()
                let source = System.Text.Encoding.Latin1.GetString(content.GetBytes())
                content.SetData(System.Text.Encoding.Latin1.GetBytes(source.Replace($"({sentence})", $"({text})")))
            File.Move(copy, path, true)
    use unused = new TempPdf()
    use drawn = new TempPdf()
    use fixedUnused = new TempPdf()
    use fixedDrawn = new TempPdf()
    create unused.Path 200 sentence
    create drawn.Path 65 "A"

    fix unused.Path fixedUnused.Path |> ignore
    let result = VeraPdfValidator().Fix(drawn.Path, PdfProfile.PdfUa1, fixedDrawn.Path, PdfFixOptions.EmbedFonts)

    failed fixedUnused.Path |> Set.filter (fun rule -> rule.StartsWith "7.21") |> should be Empty
    result.OutputPath |> should equal null
    result.NotApplied |> should contain "Helvetica isn't embedded, and was left as it is (its encoding can't be kept): export the PDF again with fonts embedded."

[<Fact>]
let ``Acrobat's form font encoding, with .notdef in it, is embedded`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // As Acrobat's default /Helv has it: Standard, with a /Differences that names .notdef.
    createText input.Path [ StandardFonts.HELVETICA ] false (fun _ font ->
        let differences =
            PdfArray(
                [| PdfNumber 24 :> PdfObject; PdfName "breve"; PdfName "caron"; PdfName "circumflex"
                   PdfNumber 39 :> PdfObject; PdfName "quotesingle"
                   PdfNumber 96 :> PdfObject; PdfName "grave"
                   PdfNumber 173 :> PdfObject; PdfName ".notdef" |]
            )
        let encoding = PdfDictionary()
        encoding.Put(PdfName.Type, PdfName.Encoding) |> ignore
        encoding.Put(PdfName.Differences, differences) |> ignore
        font.Put(PdfName.Encoding, encoding) |> ignore)

    let report = fix input.Path output.Path

    report |> should contain "Embedded Nimbus Sans in place of Helvetica, which wasn't embedded: an open font with its glyphs, each as wide."
    failed output.Path |> Set.filter (fun rule -> rule.StartsWith "7.21") |> should be Empty

[<Fact>]
let ``a whole font is checked without control codes, which iText gives a width`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // As iText writes widths, from code 0, with .notdef's width at the control codes (and 127);
    // and a form that fills in with the font, so it's embedded whole.
    createText input.Path [ StandardFonts.HELVETICA ] true (fun document font ->
        let widths = font.GetAsArray PdfName.Widths
        widths.Set(127 - 32, PdfNumber 278) |> ignore
        font.Put(PdfName.FirstChar, PdfNumber 0) |> ignore
        font.Put(PdfName.Widths, PdfArray(Array.append (Array.create 32 (PdfNumber 278 :> PdfObject)) [| for i in 0 .. widths.Size() - 1 -> widths.Get i |])) |> ignore
        let form = PdfDictionary()
        form.Put(PdfName.Fields, PdfArray()) |> ignore
        form.Put(PdfName.DR, PdfDictionary(dict [ PdfName.Font, PdfDictionary(dict [ PdfName "Helv", font :> PdfObject ]) :> PdfObject ])) |> ignore
        document.GetCatalog().GetPdfObject().Put(PdfName.AcroForm, form) |> ignore)

    let report = fix input.Path output.Path

    report |> should contain "Embedded Nimbus Sans in place of Helvetica, which wasn't embedded: an open font with its glyphs, each as wide."
    fonts output.Path |> should equal [ "NimbusSans-Regular", true ]
    failed output.Path |> Set.filter (fun rule -> rule.StartsWith "7.21") |> should be Empty

[<Fact>]
let ``a code the page draws that its encoding gives no character is said to be the reason`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // No /Encoding: Standard, which gives code 0x92 nothing.
    createText input.Path [ StandardFonts.HELVETICA ] true (fun _ font -> font.Remove PdfName.Encoding |> ignore)
    use drawn = new TempPdf()
    do
        use document = new PdfDocument(new PdfReader(input.Path), new PdfWriter(drawn.Path))
        let content = document.GetPage(1).GetFirstContentStream()
        let source = System.Text.Encoding.Latin1.GetString(content.GetBytes())
        content.SetData(System.Text.Encoding.Latin1.GetBytes(source.Replace($"({sentence})", "(It\\222s)")))

    let result = VeraPdfValidator().Fix(drawn.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.EmbedFonts)

    result.NotApplied |> should contain "Helvetica isn't embedded, and was left as it is (a code the pages draw has no character in its encoding): export the PDF again with fonts embedded."
