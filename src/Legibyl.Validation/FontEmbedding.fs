/// Fonts a PDF uses without embedding them, embedded from the open fonts Legibyl brings, made to
/// the same measure: URW's base 35 (clones of the PostScript standard 14: Helvetica, Times,
/// Courier, Symbol, ZapfDingbats) and Liberation (clones of Arial, Times New Roman and Courier
/// New). Which stands in follows the font's name, and the PDF's own widths decide: a candidate is
/// only embedded if it has every glyph the pages draw, each as wide as the PDF says.
module Legibyl.Validation.FontEmbedding

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open iText.IO.Font
open iText.Kernel.Pdf
open UglyToad.PdfPig.Fonts.TrueType

/// A family the bundled fonts stand in for.
type Family =
    | Sans
    | Serif
    | Mono
    | Symbol
    | Dingbats

/// The font a /BaseFont names, if it's one the bundled fonts stand in for: its family, whether
/// it's bold, italic, and a Microsoft font (Arial, Times New Roman, Courier New) rather than a
/// PostScript one (Helvetica, Times, Courier). Its subset prefix (ABCDEF+) is left out, and its
/// style is read from what follows the family: "Helvetica-BoldOblique", "Arial,Bold",
/// "TimesNewRomanPS-ItalicMT". A name with anything else after the family (Arial Narrow,
/// Helvetica Neue) is none of them.
let identify (baseFont: string) : (Family * bool * bool * bool) option =
    let name = match baseFont.IndexOf '+' with 6 -> baseFont.Substring 7 | _ -> baseFont
    let name = name.Replace(" ", "").ToLowerInvariant()
    // Longest first, so Times New Roman isn't taken for Times.
    let families =
        [ "timesnewroman", Serif, true
          "couriernew", Mono, true
          "arial", Sans, true
          "helvetica", Sans, false
          "times", Serif, false
          "courier", Mono, false
          "symbol", Symbol, false
          "zapfdingbats", Dingbats, false ]
    families
    |> List.tryFind (fun (prefix, _, _) -> name.StartsWith prefix)
    |> Option.bind (fun (prefix, family, microsoft) ->
        let style =
            name.Substring(prefix.Length).Replace("-", "").Replace(",", "").Replace("psmt", "").Replace("mt", "").Replace("ps", "")
        let styles =
            [ "", (false, false)
              "roman", (false, false)
              "regular", (false, false)
              "bold", (true, false)
              "italic", (false, true)
              "oblique", (false, true)
              "bolditalic", (true, true)
              "boldoblique", (true, true) ]
        match List.tryFind (fst >> (=) style) styles with
        | Some _ when family = Symbol || family = Dingbats -> if style = "" then Some(family, false, false, false) else None
        | Some(_, (bold, italic)) -> Some(family, bold, italic, microsoft)
        | None -> None)

/// The bundled fonts that could stand in for a font, best first: the clone of the font named,
/// then the other set's (each set's Sans, Serif and Mono share their metrics with the other's).
/// Each is its file's name and the name the report gives it.
let candidates (family: Family) (bold: bool) (italic: bool) (microsoft: bool) : (string * string) list =
    let style = match bold, italic with | false, false -> "Regular" | true, false -> "Bold" | false, true -> "Italic" | true, true -> "BoldItalic"
    let urw file shown = $"{file}-{style}.ttf", shown
    let liberation file shown = $"Liberation{file}-{style}.ttf", $"Liberation {shown}"
    let both postScript microsoftOne = if microsoft then [ microsoftOne; postScript ] else [ postScript; microsoftOne ]
    match family with
    | Sans -> both (urw "NimbusSans" "Nimbus Sans") (liberation "Sans" "Sans")
    | Serif -> both (urw "NimbusRoman" "Nimbus Roman") (liberation "Serif" "Serif")
    | Mono -> both (urw "NimbusMonoPS" "Nimbus Mono PS") (liberation "Mono" "Mono")
    | Symbol -> [ "StandardSymbolsPS.ttf", "Standard Symbols PS" ]
    | Dingbats -> [ "D050000L.ttf", "D050000L" ]

/// The standard 14 font whose metrics readers use for one not embedded without /Widths (Arial's,
/// Times New Roman's and Courier New's are Helvetica's, Times' and Courier's).
let private standardFont (family: Family) (bold: bool) (italic: bool) =
    let styled (name: string) (boldName: string) (italicName: string) (both: string) =
        match bold, italic with
        | false, false -> name
        | true, false -> boldName
        | false, true -> italicName
        | true, true -> both
    match family with
    | Sans -> styled "Helvetica" "Helvetica-Bold" "Helvetica-Oblique" "Helvetica-BoldOblique"
    | Serif -> styled "Times-Roman" "Times-Bold" "Times-Italic" "Times-BoldItalic"
    | Mono -> styled "Courier" "Courier-Bold" "Courier-Oblique" "Courier-BoldOblique"
    | Symbol -> "Symbol"
    | Dingbats -> "ZapfDingbats"

/// Where the bundled fonts are: the app's fonts folder.
let fontsFolder () = Path.Combine(AppContext.BaseDirectory, "fonts")

/// What came of a font that isn't embedded.
type Outcome =
    /// Embedded as the bundled font named.
    | Embedded of shown: string
    /// Left as it is, for the reason given.
    | Left of reason: string

/// A Glyph List name's character, if it has exactly one.
let private nameUnicode (name: string) =
    let unicode = AdobeGlyphList.NameToUnicode name
    if unicode > 0 then Some unicode else None

/// The codes a simple text font's encoding gives a character, each with it: its /Differences over
/// its base encoding (the one named, else Standard). None if a /Differences name isn't in the
/// Glyph List (a reader can't find it in a TrueType font), or the base encoding isn't known.
let private encodedCharacters (font: PdfDictionary) : Map<int, int> option =
    let baseName, differences =
        match font.Get(PdfName.Encoding, true) with
        | :? PdfName as name -> name.GetValue(), Some []
        | :? PdfDictionary as encoding ->
            let names =
                match encoding.GetAsArray PdfName.Differences with
                | null -> []
                | array ->
                    let mutable code = 0
                    [ for i in 0 .. array.Size() - 1 do
                          match array.Get i with
                          | :? PdfNumber as n -> code <- n.IntValue()
                          | :? PdfName as name ->
                              yield code, name.GetValue()
                              code <- code + 1
                          | _ -> () ]
            (match encoding.GetAsName PdfName.BaseEncoding with null -> "StandardEncoding" | name -> name.GetValue()),
            (if names |> List.forall (fun (_, name) -> (nameUnicode name).IsSome) then Some names else None)
        | _ -> "StandardEncoding", Some []
    match differences, ToUnicodeMaps.baseEncoding baseName with
    | Some differences, Some encoding ->
        let characters = Dictionary<int, int>()
        for code in 0..255 do
            match encoding code with
            | unicode when unicode > 0 -> characters[code] <- unicode
            | _ -> ()
        for code, name in differences do
            if code >= 0 && code <= 255 then characters[code] <- (nameUnicode name).Value
        Some(characters |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq)
    | _ -> None

/// The widths a font's dictionary gives its codes: /Widths from /FirstChar, else the descriptor's
/// /MissingWidth. None if it has no /Widths.
let private dictionaryWidths (font: PdfDictionary) : (int -> float) option =
    match font.GetAsArray PdfName.Widths with
    | null -> None
    | widths ->
        let first = match font.GetAsNumber PdfName.FirstChar with null -> 0 | n -> n.IntValue()
        let missing =
            match font.GetAsDictionary PdfName.FontDescriptor with
            | null -> 0.0
            | descriptor -> match descriptor.GetAsNumber PdfName.MissingWidth with null -> 0.0 | n -> n.DoubleValue()
        Some(fun code ->
            match code - first with
            | i when i >= 0 && i < widths.Size() -> (match widths.GetAsNumber i with null -> missing | n -> n.DoubleValue())
            | _ -> missing)

/// A bundled font, read: its bytes, the glyph a code draws (by the code's character in its
/// Windows Unicode cmap, or for a symbolic font the code in its Windows Symbol cmap), and a
/// glyph's width in thousandths of an em.
type private Candidate =
    { Bytes: byte array
      Glyph: int -> int
      Width: int -> float }

let private readCandidate (path: string) (symbolic: bool) (characters: Map<int, int>) : Candidate option =
    if not (File.Exists path) then
        None
    else
        try
            let bytes = File.ReadAllBytes path
            let font = Parser.TrueTypeFontParser.Parse(TrueTypeDataBytes bytes)
            let unitsPerEm = float (font.GetUnitsPerEm())
            let metrics = font.TableRegister.HorizontalMetricsTable
            let lookup (table: Tables.CMapSubTables.ICMapSubTable | null) (code: int) =
                match table with
                | null -> 0
                | table -> table.CharacterCodeToGlyphIndex code
            let glyph =
                if symbolic then
                    fun code -> lookup font.WindowsSymbolCMap (0xF000 + code)
                else
                    fun code -> match Map.tryFind code characters with Some unicode -> lookup font.WindowsUnicodeCMap unicode | None -> 0
            Some
                { Bytes = bytes
                  Glyph = glyph
                  Width = fun g -> float (metrics.GetAdvanceWidth g) * 1000.0 / unitsPerEm }
        with _ ->
            None

/// A subset's tag: six capital letters, the same for the same font and glyphs.
let private subsetTag (name: string) (glyphs: int seq) =
    let hash = SHA256.HashData(Text.Encoding.UTF8.GetBytes(name + ":" + String.Join(",", glyphs)))
    String(Array.init 6 (fun i -> char (int 'A' + int hash[i] % 26)))

/// For Symbol and ZapfDingbats, which say nothing of what their codes are, a ToUnicode map from
/// their standard encodings (Symbol's a is α, ZapfDingbats' 3 is ✓), so a reader can read them
/// (7.21.7), unless they've one.
let private writeSymbolUnicode (document: PdfDocument) (font: PdfDictionary) (family: Family) =
    if isNull (font.Get PdfName.ToUnicode) then
        let encoding = if family = Dingbats then PdfEncodings.ZAPFDINGBATS else PdfEncodings.SYMBOL
        let entries =
            Map.ofList
                [ for code in 32..255 do
                      let text = PdfEncodings.ConvertToString([| byte code |], encoding)
                      if ToUnicodeMaps.isReadable text && not (String.IsNullOrWhiteSpace text && code <> 32) then code, text ]
        if not entries.IsEmpty then
            let stream = PdfStream(Text.Encoding.ASCII.GetBytes(ToUnicodeMaps.write 1 entries))
            stream.SetCompressionLevel CompressionConstants.DEFAULT_COMPRESSION
            font.Put(PdfName.ToUnicode, stream.MakeIndirect document) |> ignore

/// Embeds a bundled font in place of font, which isn't embedded, if one fits: codes are the codes
/// the pages draw with it, and whole says to embed every glyph (the font fills in form fields, or
/// some page couldn't be read), not only theirs, and to check every code the font gives a width.
/// None if font is embedded, or isn't a simple font (Type 1 or TrueType) or a composite one.
let embed (document: PdfDocument) (font: PdfDictionary) (codes: int seq) (whole: bool) : (string * Outcome) option =
    let subtype = font.GetAsName PdfName.Subtype
    let descriptor = font.GetAsDictionary PdfName.FontDescriptor
    let embedded (descriptor: PdfDictionary | null) =
        match descriptor with
        | null -> false
        | d -> not (isNull (d.Get PdfName.FontFile)) || not (isNull (d.Get PdfName.FontFile2)) || not (isNull (d.Get PdfName.FontFile3))
    let baseFont = match font.GetAsName PdfName.BaseFont with null -> "" | name -> name.GetValue()
    let shownName = match baseFont.IndexOf '+' with 6 -> baseFont.Substring 7 | _ -> baseFont
    if PdfName.Type0.Equals subtype then
        match font.GetAsArray PdfName.DescendantFonts with
        | NonNull descendants when descendants.Size() > 0 && not (isNull (descendants.GetAsDictionary 0)) && not (embedded ((descendants.GetAsDictionary 0).GetAsDictionary PdfName.FontDescriptor)) ->
            Some(shownName, Left "a composite font Legibyl can't embed")
        | _ -> None
    elif not (PdfName.Type1.Equals subtype || PdfName.TrueType.Equals subtype || PdfName.MMType1.Equals subtype) || embedded descriptor then
        None
    else
        match identify baseFont with
        | None -> Some(shownName, Left "no font Legibyl brings stands in for it")
        | Some(family, bold, italic, microsoft) ->
        let symbolic = family = Symbol || family = Dingbats
        let flagged = match descriptor with null -> false | d -> (match d.GetAsNumber PdfName.Flags with null -> false | n -> (n.IntValue() &&& 4) <> 0)
        let characters =
            if symbolic then
                // Symbol and ZapfDingbats draw by their own encoding: only without another.
                if isNull (font.Get PdfName.Encoding) then Some Map.empty else None
            elif PdfName.TrueType.Equals subtype && flagged && isNull (font.Get PdfName.Encoding) then
                // A symbolic TrueType font draws by whatever cmap the reader's copy has: unknown.
                None
            else
                encodedCharacters font
        match characters with
        | None -> Some(shownName, Left "its encoding can't be kept")
        | Some characters ->
        let codes = codes |> Seq.filter (fun c -> c >= 0 && c <= 255) |> Seq.distinct |> Seq.sort |> List.ofSeq
        if codes.IsEmpty then None else
        // The widths readers use now: the PDF's, else the standard 14's metrics.
        let expected =
            match dictionaryWidths font with
            | Some widths -> fun code -> Some(widths code)
            | None ->
                let standard = FontProgramFactory.CreateFont(standardFont family bold italic) :?> Type1Font
                fun code ->
                    let glyph =
                        if symbolic then standard.GetGlyphByCode code
                        else match Map.tryFind code characters with Some unicode -> standard.GetGlyph unicode | None -> null
                    match glyph with
                    | null -> None
                    | glyph -> Some(float (glyph.GetWidth()))
        // The codes to check: those the pages draw; and for the whole font (when some codes may
        // not be known, a page's or a form field's), every code the font gives a width.
        let checkedCodes =
            if whole then
                codes
                @ [ for code in 0..255 do
                        if (symbolic || characters.ContainsKey code) && (match expected code with Some width -> width > 0.0 | None -> false) then
                            code ]
                |> List.distinct
            else
                codes
        // Whether the encoding can be kept, or written as WinAnsi's with what differs in Glyph List
        // names (7.21.6-2: a uniXXXX name won't do), for every code checked.
        let keeps =
            match font.Get(PdfName.Encoding, true) with
            | :? PdfName as name -> PdfName.WinAnsiEncoding.Equals name || PdfName.MacRomanEncoding.Equals name
            | :? PdfDictionary as encoding ->
                let baseEncoding = encoding.GetAsName PdfName.BaseEncoding
                PdfName.WinAnsiEncoding.Equals baseEncoding || PdfName.MacRomanEncoding.Equals baseEncoding
            | _ -> false
        let rewritable =
            symbolic
            || keeps
            || (let winAnsi = FontEncoding.CreateFontEncoding "WinAnsiEncoding"
                checkedCodes
                |> List.forall (fun code ->
                    match Map.tryFind code characters with
                    | Some unicode -> winAnsi.GetUnicode code = unicode || not (isNull (AdobeGlyphList.UnicodeToName unicode))
                    | None -> true))
        if not rewritable then Some(shownName, Left "its encoding can't be kept") else
        let fits (candidate: Candidate) =
            checkedCodes
            |> List.forall (fun code ->
                let glyph = candidate.Glyph code
                glyph > 0
                && match expected code with
                   | Some width -> abs (candidate.Width glyph - width) <= 1.0
                   | None -> false)
        let chosen =
            candidates family bold italic microsoft
            |> List.tryPick (fun (file, shown) ->
                readCandidate (Path.Combine(fontsFolder (), file)) symbolic characters
                |> Option.filter fits
                |> Option.map (fun candidate -> candidate, shown))
        match chosen with
        | None ->
            Some(shownName, Left "no font Legibyl brings has its glyphs at its widths")
        | Some(candidate, shown) ->
        let program = FontProgramFactory.CreateFont(candidate.Bytes, false) :?> iText.IO.Font.TrueTypeFont
        let postScriptName = program.GetFontNames().GetFontName()
        let glyphs = SortedSet<int>([ yield 0; for code in codes -> candidate.Glyph code ])
        let bytes, fontName =
            if whole then
                candidate.Bytes, postScriptName
            else
                program.GetSubset(glyphs, true), $"{subsetTag postScriptName glyphs}+{postScriptName}"
        let file = PdfStream bytes
        file.Put(PdfName.Length1, PdfNumber bytes.Length) |> ignore
        file.SetCompressionLevel CompressionConstants.DEFAULT_COMPRESSION
        let metrics = program.GetFontMetrics()
        let bbox = metrics.GetBbox()
        let flags =
            (if symbolic then 4 else 32)
            ||| (if family = Mono then 1 else 0)
            ||| (if family = Serif then 2 else 0)
            ||| (if italic then 64 else 0)
        let newDescriptor = PdfDictionary()
        newDescriptor.Put(PdfName.Type, PdfName.FontDescriptor) |> ignore
        newDescriptor.Put(PdfName.FontName, PdfName fontName) |> ignore
        newDescriptor.Put(PdfName.Flags, PdfNumber flags) |> ignore
        newDescriptor.Put(PdfName.FontBBox, PdfArray([| bbox[0]; bbox[1]; bbox[2]; bbox[3] |])) |> ignore
        newDescriptor.Put(PdfName.ItalicAngle, PdfNumber(float (metrics.GetItalicAngle()))) |> ignore
        newDescriptor.Put(PdfName.Ascent, PdfNumber(metrics.GetTypoAscender())) |> ignore
        newDescriptor.Put(PdfName.Descent, PdfNumber(metrics.GetTypoDescender())) |> ignore
        newDescriptor.Put(PdfName.CapHeight, PdfNumber(metrics.GetCapHeight())) |> ignore
        newDescriptor.Put(PdfName.StemV, PdfNumber(metrics.GetStemV())) |> ignore
        newDescriptor.Put(PdfName.FontFile2, file.MakeIndirect document) |> ignore
        // The width readers give codes /Widths leaves out, as the font's was.
        match font.GetAsDictionary PdfName.FontDescriptor with
        | null -> ()
        | old ->
            match old.GetAsNumber PdfName.MissingWidth with
            | null -> ()
            | width -> newDescriptor.Put(PdfName.MissingWidth, width) |> ignore
        font.Put(PdfName.Subtype, PdfName.TrueType) |> ignore
        font.Put(PdfName.BaseFont, PdfName fontName) |> ignore
        font.Put(PdfName.FontDescriptor, newDescriptor.MakeIndirect document) |> ignore
        if symbolic then
            font.Remove PdfName.Encoding |> ignore
            writeSymbolUnicode document font family
        else
            // A TrueType font's encoding has to be WinAnsi or MacRoman, or differ from one by
            // Glyph List names: kept if it is, else written as WinAnsi with what differs.
            if not keeps then
                let winAnsi = FontEncoding.CreateFontEncoding "WinAnsiEncoding"
                let differences = PdfArray()
                let mutable next = -1
                for KeyValue(code, unicode) in characters do
                    if winAnsi.GetUnicode code <> unicode then
                        // (A code the Glyph List can't name isn't one checked: see rewritable.)
                        match AdobeGlyphList.UnicodeToName unicode with
                        | null -> ()
                        | name ->
                            if code <> next then differences.Add(PdfNumber code) |> ignore
                            differences.Add(PdfName name) |> ignore
                            next <- code + 1
                let encoding = PdfDictionary()
                encoding.Put(PdfName.Type, PdfName.Encoding) |> ignore
                encoding.Put(PdfName.BaseEncoding, PdfName.WinAnsiEncoding) |> ignore
                if differences.Size() > 0 then encoding.Put(PdfName.Differences, differences) |> ignore
                font.Put(PdfName.Encoding, encoding) |> ignore
        if isNull (font.Get PdfName.Widths) then
            // The program's own widths, which are the standard ones (each within 1 of them).
            let first, last = if whole then 0, 255 else List.min codes, List.max codes
            let widths = PdfArray()
            for code in first..last do
                match candidate.Glyph code with
                | 0 -> widths.Add(PdfNumber 0) |> ignore
                | glyph -> widths.Add(PdfNumber(Math.Round(candidate.Width glyph))) |> ignore
            font.Put(PdfName.FirstChar, PdfNumber first) |> ignore
            font.Put(PdfName.LastChar, PdfNumber last) |> ignore
            font.Put(PdfName.Widths, widths) |> ignore
        Some(shownName, Embedded shown)
