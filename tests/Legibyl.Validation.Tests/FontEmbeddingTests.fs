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
              d.GetAsName(PdfName.BaseFont).GetValue(), not (isNull descriptor) && not (isNull (descriptor.Get PdfName.FontFile2))
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
    // No font rule fails but which characters Symbol's and ZapfDingbats' codes are (see FontUnicode).
    after |> Set.filter (fun rule -> rule.StartsWith "7.21" && not (rule.StartsWith "7.21.7")) |> should be Empty
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
