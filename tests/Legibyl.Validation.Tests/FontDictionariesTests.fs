module Legibyl.Validation.Tests.FontDictionariesTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A page of text in Lato, embedded as a subset composite (Type 0) font.
let private createText (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage PageSize.LETTER
    let font = PdfFontFactory.CreateFont(corpusFile "fonts/Lato-Regular.ttf", iText.IO.Font.PdfEncodings.IDENTITY_H, PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED)
    PdfCanvas(page).BeginText().SetFontAndSize(font, 14f).MoveText(72.0, 700.0).ShowText("The garden opens at eight.").EndText() |> ignore

/// Copies input to output with break done to its Type 0 font (the font, and its CIDFont).
let private breakFont (input: string) (output: string) (breakIt: PdfDocument -> PdfDictionary -> PdfDictionary -> unit) =
    use document = new PdfDocument(new PdfReader(input), new PdfWriter(output))
    let font =
        seq { for n in 1 .. document.GetNumberOfPdfObjects() - 1 -> document.GetPdfObject n }
        |> Seq.pick (function
            | :? PdfDictionary as d when PdfName.Type0.Equals(d.GetAsName PdfName.Subtype) -> Some d
            | _ -> None)
    let cidFont = font.GetAsArray(PdfName.DescendantFonts).GetAsDictionary 0
    breakIt document font cidFont

let private failed (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

let private fix (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.FontDictionaries)

let private render (path: string) =
    use renderer = new PdfRegionRenderer()
    let offPage = { PageNumber = 1; Left = -20.0; Bottom = -20.0; Right = -10.0; Top = -10.0 }
    use bitmap = SkiaSharp.SKBitmap.Decode(renderer.RenderPageWithHighlight(path, offPage).PngBytes)
    bitmap.Pixels

/// Breaks the font, fixes it, and gives the rules failed before and after, the fix's report, and
/// whether the page looks the same.
let private check (breakIt: PdfDocument -> PdfDictionary -> PdfDictionary -> unit) =
    use plain = new TempPdf()
    use broken = new TempPdf()
    use fixedPdf = new TempPdf()
    createText plain.Path
    breakFont plain.Path broken.Path breakIt
    let result = fix broken.Path fixedPdf.Path
    failed broken.Path, failed fixedPdf.Path, List.ofSeq result.AppliedFixes, render broken.Path = render fixedPdf.Path

[<Fact>]
let ``a subset font's incomplete CIDSet is removed`` () =
    let before, after, report, same =
        check (fun document _ cidFont ->
            // A CIDSet that lists no glyphs at all.
            let cidSet = PdfStream([| 0uy |])
            cidSet.MakeIndirect document |> ignore
            cidFont.GetAsDictionary(PdfName.FontDescriptor).Put(PdfName.CIDSet, cidSet) |> ignore)

    before |> should contain "7.21.4.2-2"
    after |> should not' (contain "7.21.4.2-2")
    report |> should contain "Removed the incomplete glyph list (CharSet or CIDSet) of 1 subset font(s): it's optional, and isn't used to draw."
    same |> should be True

[<Fact>]
let ``a CIDSet that lists every outlined glyph but omits empty glyphs is removed`` () =
    let before, after, report, same =
        check (fun document _ cidFont ->
            let descriptor = cidFont.GetAsDictionary PdfName.FontDescriptor
            let program = descriptor.GetAsStream(PdfName.FontFile2).GetBytes()
            let u16 at = (int program[at] <<< 8) ||| int program[at + 1]
            let u32 at = (u16 at <<< 16) ||| u16 (at + 2)
            let tables =
                dict [ for i in 0 .. u16 4 - 1 ->
                           let entry = 12 + 16 * i
                           Text.Encoding.ASCII.GetString(program, entry, 4), u32 (entry + 8) ]
            let count = u16 (tables["maxp"] + 4)
            let longOffsets = u16 (tables["head"] + 50) = 1
            let loca = tables["loca"]
            let offset glyph = if longOffsets then u32 (loca + 4 * glyph) else 2 * u16 (loca + 2 * glyph)
            let bits = Array.zeroCreate<byte> ((count + 7) / 8)
            for glyph in 0 .. count - 1 do
                if offset (glyph + 1) > offset glyph then
                    bits[glyph / 8] <- bits[glyph / 8] ||| (0x80uy >>> (glyph % 8))
            let cidSet = PdfStream bits
            cidSet.MakeIndirect document |> ignore
            descriptor.Put(PdfName.CIDSet, cidSet) |> ignore)

    before |> should contain "7.21.4.2-2"
    after |> should not' (contain "7.21.4.2-2")
    report |> should contain "Removed the incomplete glyph list (CharSet or CIDSet) of 1 subset font(s): it's optional, and isn't used to draw."
    same |> should be True

[<Fact>]
let ``a composite font's CIDSystemInfo is set to its predefined CMap's`` () =
    let before, after, report, _ =
        check (fun _ font cidFont ->
            font.Put(PdfName.Encoding, PdfName "UniGB-UCS2-H") |> ignore
            let info = PdfDictionary()
            info.Put(PdfName.Registry, PdfString "Adobe") |> ignore
            info.Put(PdfName.Ordering, PdfString "Identity") |> ignore
            info.Put(PdfName.Supplement, PdfNumber 0) |> ignore
            cidFont.Put(PdfName.CIDSystemInfo, info) |> ignore)

    before |> should contain "7.21.3.1-1"
    after |> should not' (contain "7.21.3.1-1")
    report |> should contain "Set the character collection (CIDSystemInfo) of 1 embedded composite font(s) to their CMap's."

[<Fact>]
let ``predefined CMaps are known by their collection, the 78- Japanese ones and bare H and V included`` () =
    [ "78-EUC-H"; "78-RKSJ-V"; "90ms-RKSJ-H"; "UniJIS-UCS2-H"; "Hankaku"; "H"; "V"; "UniGB-UCS2-H"; "GBK-EUC-H"; "HKscs-B5-H"; "HKdla-B5-V"; "UniKS-UCS2-H"; "KSCms-UHC-H"; "Identity-H"; "Hypothetical-H" ]
    |> List.map PredefinedCMaps.ordering
    |> should
        equal
        [ Some "Japan1"; Some "Japan1"; Some "Japan1"; Some "Japan1"; Some "Japan1"; Some "Japan1"; Some "Japan1"
          Some "GB1"; Some "GB1"; Some "CNS1"; Some "CNS1"; Some "Korea1"; Some "Korea1"; None; None ]

[<Fact>]
let ``an embedded CMap's /WMode is set to its program's (not a comment's), and the page looks the same`` () =
    let before, after, report, same =
        check (fun document font _ ->
            let program =
                "/CIDInit /ProcSet findresource begin 12 dict begin begincmap\n"
                + "/CIDSystemInfo << /Registry (Adobe) /Ordering (Identity) /Supplement 0 >> def\n"
                // A comment that mentions /WMode isn't its definition.
                + "% Don't take this for it: /WMode 1 def\n"
                + "/CMapName /Legibyl-Test-H def /CMapType 1 def /WMode 0 def\n"
                + "1 begincodespacerange <0000> <FFFF> endcodespacerange\n"
                + "1 begincidrange <0000> <FFFF> 0 endcidrange\n"
                + "endcmap CMapName currentdict /CMap defineresource pop end end\n"
            let cmap = PdfStream(Text.Encoding.ASCII.GetBytes program)
            cmap.Put(PdfName.Type, PdfName "CMap") |> ignore
            cmap.Put(PdfName.CMapName, PdfName "Legibyl-Test-H") |> ignore
            let info = PdfDictionary()
            info.Put(PdfName.Registry, PdfString "Adobe") |> ignore
            info.Put(PdfName.Ordering, PdfString "Identity") |> ignore
            info.Put(PdfName.Supplement, PdfNumber 0) |> ignore
            cmap.Put(PdfName.CIDSystemInfo, info) |> ignore
            // The dictionary says vertical; the program, which readers use, horizontal.
            cmap.Put(PdfName "WMode", PdfNumber 1) |> ignore
            cmap.MakeIndirect document |> ignore
            font.Put(PdfName.Encoding, cmap) |> ignore)

    before |> should contain "7.21.3.3-2"
    after |> should not' (contain "7.21.3.3-2")
    report |> should contain "Set the writing direction (/WMode) of 1 embedded CMap(s) to the one in their program, which readers use."
    same |> should be True

[<Fact>]
let ``a font that's right already is left alone, and the fix is safe and named`` () =
    use plain = new TempPdf()
    use fixedPdf = new TempPdf()
    createText plain.Path
    let result = fix plain.Path fixedPdf.Path

    failed plain.Path |> should not' (contain "7.21.4.2-2")
    result.WasFixed |> should be False
    File.Exists fixedPdf.Path |> should be False
    result.AppliedFixes |> Seq.exists (fun line -> line.Contains "CIDSystemInfo" || line.Contains "CIDSet" || line.Contains "/WMode") |> should be False
    FixNames.nameOf PdfFixOptions.FontDictionaries |> should equal "font-dictionaries"
    FixNames.safe.HasFlag PdfFixOptions.FontDictionaries |> should be True

/// A page of text in Lato, embedded as a simple TrueType font flagged symbolic, with an encoding
/// whose /Differences name each letter, and no ToUnicode map: as some PDF libraries write a subset.
let private createSymbolic (path: string) (withEncoding: bool) =
    use plain = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(plain.Path))
        let page = document.AddNewPage PageSize.LETTER
        let font = PdfFontFactory.CreateFont(corpusFile "fonts/Lato-Regular.ttf", iText.IO.Font.PdfEncodings.WINANSI, PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED)
        PdfCanvas(page).BeginText().SetFontAndSize(font, 14f).MoveText(72.0, 700.0).ShowText("The garden opens at eight.").EndText() |> ignore
    use document = new PdfDocument(new PdfReader(plain.Path), new PdfWriter(path))
    let font =
        seq { for n in 1 .. document.GetNumberOfPdfObjects() - 1 -> document.GetPdfObject n }
        |> Seq.pick (function
            | :? PdfDictionary as d when PdfName.TrueType.Equals(d.GetAsName PdfName.Subtype) -> Some d
            | _ -> None)
    font.GetAsDictionary(PdfName.FontDescriptor).Put(PdfName.Flags, PdfNumber 4) |> ignore
    font.Remove PdfName.ToUnicode |> ignore
    if withEncoding then
        let differences = PdfArray()
        differences.Add(PdfNumber 84)
        differences.Add(PdfName "T")
        let encoding = PdfDictionary()
        encoding.Put(PdfName.Type, PdfName.Encoding) |> ignore
        encoding.Put(PdfName.BaseEncoding, PdfName.WinAnsiEncoding) |> ignore
        encoding.Put(PdfName.Differences, differences) |> ignore
        font.Put(PdfName.Encoding, encoding) |> ignore
    else
        font.Remove PdfName.Encoding |> ignore

let private text (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    iText.Kernel.Pdf.Canvas.Parser.PdfTextExtractor.GetTextFromPage(document.GetPage 1)

[<Fact>]
let ``a symbolic TrueType font's encoding is taken out, the characters it named kept in a ToUnicode map, and the page looks the same`` () =
    use broken = new TempPdf()
    use fixedPdf = new TempPdf()
    createSymbolic broken.Path true

    let result = fix broken.Path fixedPdf.Path

    failed broken.Path |> should contain "7.21.6-3"
    failed fixedPdf.Path |> should not' (contain "7.21.6-3")
    result.AppliedFixes
    |> should contain "Removed the encoding PDF/UA doesn't allow from 1 symbolic TrueType font(s), which draw by their own character map; a ToUnicode map now says the characters it named."
    text fixedPdf.Path |> should equal "The garden opens at eight."
    text broken.Path |> should equal (text fixedPdf.Path)
    render fixedPdf.Path |> should equal (render broken.Path)

[<Fact>]
let ``a symbolic TrueType font without an encoding is left alone`` () =
    use plain = new TempPdf()
    use fixedPdf = new TempPdf()
    createSymbolic plain.Path false

    let result = fix plain.Path fixedPdf.Path

    result.AppliedFixes |> Seq.exists (fun line -> line.Contains "symbolic") |> should be False

[<Fact>]
let ``a symbolic TrueType font that isn't embedded keeps its encoding, and it's said why`` () =
    use broken = new TempPdf()
    use unembedded = new TempPdf()
    use fixedPdf = new TempPdf()
    createSymbolic broken.Path true
    do
        use document = new PdfDocument(new PdfReader(broken.Path), new PdfWriter(unembedded.Path))
        for n in 1 .. document.GetNumberOfPdfObjects() - 1 do
            match document.GetPdfObject n with
            | :? PdfDictionary as d when PdfName.FontDescriptor.Equals(d.GetAsName PdfName.Type) -> d.Remove PdfName.FontFile2 |> ignore
            | _ -> ()

    let result = fix unembedded.Path fixedPdf.Path

    result.AppliedFixes |> Seq.exists (fun line -> line.Contains "symbolic") |> should be False
    result.NotApplied
    |> should contain "Left the encoding of 1 symbolic TrueType font(s), which PDF/UA doesn't allow: the font isn't embedded with a character map of its own to draw by, or the encoding picks other glyphs than its map, so taking it out could change what's drawn."
