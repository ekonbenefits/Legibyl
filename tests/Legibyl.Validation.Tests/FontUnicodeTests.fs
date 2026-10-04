module Legibyl.Validation.Tests.FontUnicodeTests

open System
open Xunit
open FsUnit.Xunit
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private sentence = "The garden opens at 8."

[<Fact>]
let ``glyph names give their text by the Adobe Glyph List's rules`` () =
    [ "T"; "uni0054"; "uni00540068"; "u1F600"; "f_i"; "T.alt"; "a.sc"; "quotedblleft"; "g42"; "uniD800"; ""; ".notdef" ]
    |> List.map ToUnicodeMaps.glyphNameText
    |> should equal [ Some "T"; Some "T"; Some "Th"; Some "😀"; Some "fi"; Some "T"; Some "a"; Some "“"; None; None; None; None ]

[<Fact>]
let ``a ToUnicode map's bfchar and bfrange entries are read, and written back`` () =
    let program =
        "begincmap 1 begincodespacerange <0000> <FFFF> endcodespacerange\n"
        + "2 beginbfchar <0003> <0020> <0024> <00660069> endbfchar\n"
        + "1 beginbfrange <0010> <0012> <0041> endbfrange\n"
        + "1 beginbfrange <0020> <0021> [<0061> <0062>] endbfrange endcmap"
    let entries = ToUnicodeMaps.parse program
    entries |> Map.toList |> should equal [ 3, " "; 0x10, "A"; 0x11, "B"; 0x12, "C"; 0x20, "a"; 0x21, "b"; 0x24, "fi" ]
    ToUnicodeMaps.parse (ToUnicodeMaps.write 2 entries) |> should equal entries
    // A range's destination counts up as one big-endian number, carrying past its last two bytes.
    ToUnicodeMaps.parse "1 beginbfrange <0010> <0011> <0041FFFF> endbfrange" |> Map.find 0x11 |> should equal "B\u0000"

/// The sentence in Lato as a composite (Identity-H) font, embedded whole (subset false: with its
/// Unicode cmap) or as iText subsets it (without one).
let private createText (path: string) (whole: bool) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage PageSize.LETTER
    let font = PdfFontFactory.CreateFont(corpusFile "fonts/Lato-Regular.ttf", iText.IO.Font.PdfEncodings.IDENTITY_H, PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED)
    font.SetSubset(not whole)
    PdfCanvas(page).BeginText().SetFontAndSize(font, 14f).MoveText(72.0, 700.0).ShowText(sentence).EndText() |> ignore

/// Copies input to output with change done to its Type 0 font's dictionary.
let private changeFont (input: string) (output: string) (change: PdfDocument -> PdfDictionary -> unit) =
    use document = new PdfDocument(new PdfReader(input), new PdfWriter(output))
    seq { for n in 1 .. document.GetNumberOfPdfObjects() - 1 -> document.GetPdfObject n }
    |> Seq.pick (function :? PdfDictionary as d when PdfName.Type0.Equals(d.GetAsName PdfName.Subtype) -> Some d | _ -> None)
    |> change document

let private failed (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

let private text (path: string) =
    use document = UglyToad.PdfPig.PdfDocument.Open path
    document.GetPage(1).Text

let private render (path: string) =
    use renderer = new PdfRegionRenderer()
    let offPage = { PageNumber = 1; Left = -20.0; Bottom = -20.0; Right = -10.0; Top = -10.0 }
    use bitmap = SkiaSharp.SKBitmap.Decode(renderer.RenderPageWithHighlight(path, offPage).PngBytes)
    bitmap.Pixels

let private fix (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.FontUnicode) |> _.AppliedFixes |> List.ofSeq

[<Fact>]
let ``a font with no ToUnicode gets one from its own Unicode table, and its text reads right`` () =
    use plain = new TempPdf()
    use broken = new TempPdf()
    use fixedPdf = new TempPdf()
    createText plain.Path true
    changeFont plain.Path broken.Path (fun _ font -> font.Remove PdfName.ToUnicode |> ignore)

    let report = fix broken.Path fixedPdf.Path

    failed broken.Path |> should contain "7.21.7-1"
    text broken.Path |> should not' (equal sentence)
    failed fixedPdf.Path |> should not' (contain "7.21.7-1")
    text fixedPdf.Path |> should equal sentence
    report |> should contain "Wrote which characters 1 font(s) draw (ToUnicode), from their own glyph names and Unicode tables, so a screen reader can read 15 more character code(s): to check."
    render broken.Path = render fixedPdf.Path |> should be True

[<Fact>]
let ``a subset font's Unicode table, which iText keeps, is enough too`` () =
    use plain = new TempPdf()
    use broken = new TempPdf()
    use fixedPdf = new TempPdf()
    createText plain.Path false
    changeFont plain.Path broken.Path (fun _ font -> font.Remove PdfName.ToUnicode |> ignore)

    fix broken.Path fixedPdf.Path |> ignore

    failed fixedPdf.Path |> should not' (contain "7.21.7-1")
    text fixedPdf.Path |> should equal sentence

[<Fact>]
let ``with nothing in the font to say what a code draws, nothing is made up, and it's reported`` () =
    use plain = new TempPdf()
    use broken = new TempPdf()
    use fixedPdf = new TempPdf()
    createText plain.Path false
    // No map, and no font program to read one from.
    changeFont plain.Path broken.Path (fun _ font ->
        font.Remove PdfName.ToUnicode |> ignore
        let descriptor = font.GetAsArray(PdfName.DescendantFonts).GetAsDictionary(0).GetAsDictionary PdfName.FontDescriptor
        descriptor.Remove PdfName.FontFile2 |> ignore)

    let result = VeraPdfValidator().Fix(broken.Path, PdfProfile.PdfUa1, fixedPdf.Path, PdfFixOptions.FontUnicode)

    // Said as not applied: nothing changed, so no copy is written.
    result.NotApplied |> should contain "15 character code(s) the pages use say nothing a reader can map to text: they need fixing where the PDF was made."
    result.AppliedFixes |> should be Empty
    result.OutputPath |> should equal null

[<Fact>]
let ``a ToUnicode map that gives U+0000 is put right`` () =
    use plain = new TempPdf()
    use broken = new TempPdf()
    use fixedPdf = new TempPdf()
    createText plain.Path true
    changeFont plain.Path broken.Path (fun document font ->
        // Every code the sentence uses maps to U+0000.
        let program = ToUnicodeMaps.parse (Text.Encoding.Latin1.GetString(font.GetAsStream(PdfName.ToUnicode).GetBytes())) |> Map.map (fun _ _ -> "\u0000")
        font.Put(PdfName.ToUnicode, PdfStream(Text.Encoding.ASCII.GetBytes(ToUnicodeMaps.write 2 program)).MakeIndirect document) |> ignore)

    let report = fix broken.Path fixedPdf.Path

    failed broken.Path |> should contain "7.21.7-2"
    failed fixedPdf.Path |> should not' (contain "7.21.7-2")
    failed fixedPdf.Path |> should not' (contain "7.21.7-1")
    text fixedPdf.Path |> should equal sentence
    report |> List.exists (fun line -> line.StartsWith "Wrote which characters 1 font(s)") |> should be True

[<Fact>]
let ``a font that says what it draws already is left alone`` () =
    use plain = new TempPdf()
    use fixedPdf = new TempPdf()
    createText plain.Path false

    let report = fix plain.Path fixedPdf.Path

    report |> List.exists (fun line -> line.Contains "ToUnicode" || line.Contains "character code") |> should be False
    FixNames.nameOf PdfFixOptions.FontUnicode |> should equal "font-unicode"
    FixNames.safe.HasFlag PdfFixOptions.FontUnicode |> should be False

[<Fact>]
let ``a composite font with another CMap is left as it is, map and all`` () =
    use plain = new TempPdf()
    use broken = new TempPdf()
    use fixedPdf = new TempPdf()
    createText plain.Path true
    let mutable before = ""
    changeFont plain.Path broken.Path (fun document font ->
        // A CJK CMap (its codes needn't be two bytes each), and a map with a bad value.
        font.Put(PdfName.Encoding, PdfName "UniGB-UCS2-H") |> ignore
        let program = ToUnicodeMaps.parse (Text.Encoding.Latin1.GetString(font.GetAsStream(PdfName.ToUnicode).GetBytes())) |> Map.map (fun _ _ -> "\u0000")
        before <- ToUnicodeMaps.write 2 program
        font.Put(PdfName.ToUnicode, PdfStream(Text.Encoding.ASCII.GetBytes before).MakeIndirect document) |> ignore)

    let result = VeraPdfValidator().Fix(broken.Path, PdfProfile.PdfUa1, fixedPdf.Path, PdfFixOptions.FontUnicode)

    // Nothing changed, map and all: no copy is written.
    result.AppliedFixes |> should be Empty
    result.OutputPath |> should equal null
    use document = new PdfDocument(new PdfReader(broken.Path))
    let font =
        seq { for n in 1 .. document.GetNumberOfPdfObjects() - 1 -> document.GetPdfObject n }
        |> Seq.pick (function :? PdfDictionary as d when PdfName.Type0.Equals(d.GetAsName PdfName.Subtype) -> Some d | _ -> None)
    Text.Encoding.ASCII.GetString(font.GetAsStream(PdfName.ToUnicode).GetBytes()) |> should equal before

[<Fact>]
let ``a simple font in the standard encoding is read by it`` () =
    use plain = new TempPdf()
    use broken = new TempPdf()
    use fixedPdf = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(plain.Path))
        let page = document.AddNewPage PageSize.LETTER
        let font = PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA
        PdfCanvas(page).BeginText().SetFontAndSize(font, 14f).MoveText(72.0, 700.0).ShowText(sentence).EndText() |> ignore
    // No /Encoding: a reader takes the standard one, which names every code the sentence uses.
    do
        use document = new PdfDocument(new PdfReader(plain.Path), new PdfWriter(broken.Path))
        let fonts = document.GetPage(1).GetResources().GetResource PdfName.Font
        (fonts.GetAsDictionary(Seq.head (fonts.KeySet()))).Remove PdfName.Encoding |> ignore

    let report = fix broken.Path fixedPdf.Path

    report |> List.exists (fun line -> line.Contains "ToUnicode" || line.Contains "character code") |> should be False
