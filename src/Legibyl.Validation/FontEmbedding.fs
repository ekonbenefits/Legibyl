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

/// A Glyph List name's character, if it has exactly one.
let private nameUnicode (name: string) =
    let unicode = AdobeGlyphList.NameToUnicode name
    if unicode > 0 then Some unicode else None

/// A font's /Differences: each code and the glyph name it gives it.
let private differences (font: PdfDictionary) : (int * string) list =
    match font.Get(PdfName.Encoding, true) with
    | :? PdfDictionary as encoding ->
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
    | _ -> []

/// Whether a glyph name is one the Glyph List lists (as a TrueType font's /Differences must have:
/// 7.21.6-2), not a uniXXXX or uXXXX name, nor .notdef.
let private listed (name: string) =
    AdobeGlyphList.NameToUnicode name > 0
    && not (Text.RegularExpressions.Regex.IsMatch(name, "^(uni[0-9A-F]{4}|u[0-9A-F]{4,6})"))

/// The codes a simple text font's encoding gives a character, each with it: its /Differences over
/// its base encoding (the one named, else Standard); a code /Differences names .notdef is none. None
/// if another /Differences name isn't in the Glyph List (a reader can't find it in a TrueType
/// font), or the base encoding isn't known.
let private encodedCharacters (font: PdfDictionary) : Map<int, int> option =
    let baseName, differences =
        match font.Get(PdfName.Encoding, true) with
        | :? PdfName as name -> name.GetValue(), Some []
        | :? PdfDictionary as encoding ->
            let names = differences font
            (match encoding.GetAsName PdfName.BaseEncoding with null -> "StandardEncoding" | name -> name.GetValue()),
            (if names |> List.forall (fun (_, name) -> name = ".notdef" || (nameUnicode name).IsSome) then Some names else None)
        | _ -> "StandardEncoding", Some []
    match differences, ToUnicodeMaps.baseEncoding baseName with
    | Some differences, Some encoding ->
        let characters = Dictionary<int, int>()
        for code in 0..255 do
            match encoding code with
            | unicode when unicode > 0 -> characters[code] <- unicode
            | _ -> ()
        for code, name in differences do
            if code >= 0 && code <= 255 then
                if name = ".notdef" then characters.Remove code |> ignore
                else characters[code] <- (nameUnicode name).Value
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

/// A font program that could stand in, read: its bytes, as iText reads it, the glyph a code draws (by the code's
/// character in its Windows Unicode cmap, or for a symbolic font the code in its Windows Symbol
/// cmap), and a glyph's width in thousandths of an em.
type private Candidate =
    { Bytes: byte array
      Program: iText.IO.Font.TrueTypeFont
      Glyph: int -> int
      Width: int -> float }

let private readCandidate (bytes: byte array) (symbolic: bool) (characters: Map<int, int>) : Candidate option =
    try
        let font = Parser.TrueTypeFontParser.Parse(TrueTypeDataBytes bytes)
        // (One iText can't read, it can't subset either.)
        let program = FontProgramFactory.CreateFont(bytes, false) :?> iText.IO.Font.TrueTypeFont
        let unitsPerEm = float (font.GetUnitsPerEm())
        let metrics = font.TableRegister.HorizontalMetricsTable
        let lookup (table: Tables.CMapSubTables.ICMapSubTable | null) (code: int) =
            match table with
            | null -> 0
            | table -> table.CharacterCodeToGlyphIndex code
        let glyph =
            if symbolic then
                // A symbol cmap maps a code at 0xF000 up, as most do, or as itself.
                fun code ->
                    match lookup font.WindowsSymbolCMap (0xF000 + code) with
                    | 0 -> lookup font.WindowsSymbolCMap code
                    | glyph -> glyph
            else
                fun code -> match Map.tryFind code characters with Some unicode -> lookup font.WindowsUnicodeCMap unicode | None -> 0
        Some
            { Bytes = bytes
              Program = program
              Glyph = glyph
              Width = fun g -> float (metrics.GetAdvanceWidth g) * 1000.0 / unitsPerEm }
    with _ ->
        None

/// A subset's tag: six capital letters, the same for the same font and glyphs.
let private subsetTag (name: string) (glyphs: int seq) =
    let hash = SHA256.HashData(Text.Encoding.UTF8.GetBytes(name + ":" + String.Join(",", glyphs)))
    String(Array.init 6 (fun i -> char (int 'A' + int hash[i] % 26)))

/// Where a TrueType program's table is (its offset and length), if it has it.
let private sfntTable (bytes: byte array) (tag: string) =
    try
        let u16 (at: int) = (int bytes[at] <<< 8) ||| int bytes[at + 1]
        let u32 (at: int) = (u16 at <<< 16) ||| u16 (at + 2)
        [ for i in 0 .. u16 4 - 1 -> 12 + 16 * i ]
        |> List.tryFind (fun entry -> Text.Encoding.ASCII.GetString(bytes, entry, 4) = tag)
        |> Option.map (fun entry -> u32 (entry + 8), u32 (entry + 12))
    with _ ->
        None

/// A TrueType program's embedding permissions (its OS/2 table's fsType; 0, installable, without
/// one).
let private embeddingFlags (bytes: byte array) =
    match sfntTable bytes "OS/2" with
    | Some(offset, length) when length >= 10 -> (int bytes[offset + 8] <<< 8) ||| int bytes[offset + 9]
    | _ -> 0

/// One font in a font file installed on this computer: its file, where its table directory is
/// in it (0, but in a collection), and its PostScript and full names.
type InstalledFace =
    { Path: string
      Offset: int64
      PostScriptName: string
      FullName: string }

/// The faces in a TrueType or OpenType font file (.ttf, .otf, or a .ttc collection), read from
/// their name tables; none if it can't be read.
let installedFaces (path: string) : InstalledFace list =
    try
        use stream = File.OpenRead path
        let read (at: int64) (count: int) =
            let buffer = Array.zeroCreate<byte> count
            stream.Position <- at
            stream.ReadExactly(buffer, 0, count)
            buffer
        let u16 (b: byte array) (at: int) = (int b[at] <<< 8) ||| int b[at + 1]
        let u32 (b: byte array) (at: int) = (int64 (u16 b at) <<< 16) ||| int64 (u16 b (at + 2))
        let header = read 0L 12
        let offsets =
            if Text.Encoding.ASCII.GetString(header, 0, 4) = "ttcf" then
                let count = int (u32 header 8)
                let table = read 12L (4 * count)
                [ for i in 0 .. count - 1 -> u32 table (4 * i) ]
            else
                [ 0L ]
        [ for offset in offsets do
              let directory = read offset 12
              let tables = int (u16 directory 4)
              let entries = read (offset + 12L) (16 * tables)
              let entry = [ 0 .. tables - 1 ] |> List.tryFind (fun i -> Text.Encoding.ASCII.GetString(entries, 16 * i, 4) = "name")
              match entry with
              | None -> ()
              | Some i ->
                  let name = read (u32 entries (16 * i + 8)) (int (u32 entries (16 * i + 12)))
                  let count, strings = u16 name 2, u16 name 4
                  // Each name: Windows' (UTF-16) over the Mac's (Roman), US English first.
                  let named (id: int) =
                      [ for r in 0 .. count - 1 ->
                            let at = 6 + 12 * r
                            u16 name at, u16 name (at + 2), u16 name (at + 4), u16 name (at + 6), u16 name (at + 8), u16 name (at + 10) ]
                      |> List.filter (fun (platform, _, _, nameId, _, _) -> nameId = id && (platform = 3 || platform = 1))
                      |> List.sortBy (fun (platform, _, language, _, _, _) -> (if platform = 3 then 0 else 1), (if language = 0x409 || language = 0 then 0 else 1))
                      |> List.tryHead
                      |> Option.map (fun (platform, _, _, _, length, at) ->
                          let bytes = name[strings + at .. strings + at + length - 1]
                          if platform = 3 then Text.Encoding.BigEndianUnicode.GetString bytes else Text.Encoding.Latin1.GetString bytes)
                  match named 6 with
                  | Some postScript when postScript <> "" ->
                      yield { Path = path; Offset = offset; PostScriptName = postScript; FullName = defaultArg (named 4) postScript }
                  | _ -> () ]
    with _ ->
        []

/// A face's own program: its file, or for one in a collection, its tables as a font file of its
/// own.
let faceProgram (face: InstalledFace) : byte array =
    let file = File.ReadAllBytes face.Path
    if face.Offset = 0L && Text.Encoding.ASCII.GetString(file, 0, 4) <> "ttcf" then
        file
    else
        let at = int face.Offset
        let u16 (i: int) = (int file[i] <<< 8) ||| int file[i + 1]
        let u32 (i: int) = (u16 i <<< 16) ||| u16 (i + 2)
        let tables = u16 (at + 4)
        let entries = [ for i in 0 .. tables - 1 -> at + 12 + 16 * i ]
        let padded (n: int) = (n + 3) &&& ~~~3
        use out = new MemoryStream()
        out.Write(file, at, 12)
        let mutable next = 12 + 16 * tables
        for entry in entries do
            out.Write(file, entry, 8)
            out.Write([| byte (next >>> 24); byte (next >>> 16); byte (next >>> 8); byte next |], 0, 4)
            out.Write(file, entry + 12, 4)
            next <- next + padded (u32 (entry + 12))
        for entry in entries do
            let length = u32 (entry + 12)
            out.Write(file, u32 (entry + 8), length)
            out.Write(Array.zeroCreate<byte> (padded length - length), 0, padded length - length)
        let font = out.ToArray()
        // Its head table's checkSumAdjustment made the font file's own: 0xB1B0AFBA less the sum of
        // its big-endian words, counted with it 0.
        let directory = [ for i in 0 .. tables - 1 -> 12 + 16 * i ]
        match directory |> List.tryFind (fun e -> Text.Encoding.ASCII.GetString(font, e, 4) = "head") with
        | None -> font
        | Some entry ->
            let at (i: int) = (uint32 font[i] <<< 24) ||| (uint32 font[i + 1] <<< 16) ||| (uint32 font[i + 2] <<< 8) ||| uint32 font[i + 3]
            let adjustment = int (at (entry + 8)) + 8
            Array.fill font adjustment 4 0uy
            let mutable sum = 0u
            for i in 0..4 .. font.Length - 4 do
                sum <- sum + at i
            let value = 0xB1B0AFBAu - sum
            font[adjustment] <- byte (value >>> 24)
            font[adjustment + 1] <- byte (value >>> 16)
            font[adjustment + 2] <- byte (value >>> 8)
            font[adjustment + 3] <- byte value
            font

/// The folders this computer's fonts are installed in.
let private systemFontFolders () =
    let home = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
    if OperatingSystem.IsWindows() then
        [ Environment.GetFolderPath Environment.SpecialFolder.Fonts
          Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "Microsoft", "Windows", "Fonts") ]
    elif OperatingSystem.IsMacOS() then
        [ "/System/Library/Fonts"; "/Library/Fonts"; Path.Combine(home, "Library", "Fonts") ]
    else
        [ "/usr/share/fonts"; "/usr/local/share/fonts"; Path.Combine(home, ".fonts"); Path.Combine(home, ".local", "share", "fonts") ]

/// The folders to look in instead of the system's, where set (for tests).
let private folderOverride = Threading.AsyncLocal<string list option>()

/// Looks for installed fonts in folders, not the system's, until disposed.
let useInstalledFolders (folders: string list) : IDisposable =
    let previous = folderOverride.Value
    folderOverride.Value <- Some folders
    { new IDisposable with member _.Dispose() = folderOverride.Value <- previous }

/// A name as the index has it: no spaces, any case.
let private indexName (name: string) = name.Replace(" ", "").ToLowerInvariant()

/// The installed fonts in folders, by PostScript name and full name, read once.
let private installedIndex =
    let cache = Collections.Concurrent.ConcurrentDictionary<string, Lazy<IReadOnlyDictionary<string, InstalledFace>>>()
    fun (folders: string list) ->
        cache
            .GetOrAdd(
                String.Join("\n", folders),
                fun _ ->
                    lazy
                        (let index = Dictionary<string, InstalledFace>()
                         let options = EnumerationOptions(RecurseSubdirectories = true, IgnoreInaccessible = true)
                         let faces =
                             [ for folder in folders do
                                   if Directory.Exists folder then
                                       for file in Directory.EnumerateFiles(folder, "*", options) do
                                           match (string (Path.GetExtension file)).ToLowerInvariant() with
                                           | ".ttf" | ".otf" | ".ttc" -> yield! installedFaces file
                                           | _ -> () ]
                         // PostScript names first, then full names ("Lato Regular").
                         for face in faces do index.TryAdd(indexName face.PostScriptName, face) |> ignore
                         for face in faces do index.TryAdd(indexName face.FullName, face) |> ignore
                         index :> IReadOnlyDictionary<_, _>)
            )
            .Value

/// The installed font a /BaseFont names, if this computer has it: by its PostScript name
/// ("Calibri-Bold"), its family and style as PDFs write them ("Calibri,Bold"), its family alone
/// for its regular style ("Calibri" for Calibri-Regular), or its full name ("Calibri Bold").
let findInstalled (baseFont: string) : InstalledFace option =
    let name = match baseFont.IndexOf '+' with 6 -> baseFont.Substring 7 | _ -> baseFont
    let index = installedIndex (defaultArg folderOverride.Value (systemFontFolders ()))
    [ name; name.Replace(',', '-'); name + "-Regular"; name.Replace(",Regular", "") ]
    |> List.tryPick (fun candidate -> match index.TryGetValue(indexName candidate) with | true, face -> Some face | _ -> None)

/// What came of a font that isn't embedded.
type Outcome =
    /// Embedded as the font named: one Legibyl brings, or (installed) this computer's own copy.
    | Embedded of shown: string * installed: bool * wholeBytes: int option
    /// Left as it is, for the reason given.
    | Left of reason: string

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

/// Embeds the first of candidates that fits in place of font, if one does: one with a glyph for
/// each of the codes checked, as wide as expected. codes are those the pages draw, whose glyphs
/// are embedded (all of them, if whole). Its flags are flags of the program, and the symbolic bit.
/// The candidate's index; else why not ("" if none fits).
let private replace
    (document: PdfDocument)
    (font: PdfDictionary)
    (codes: int list)
    (whole: bool)
    (symbolic: bool)
    (characters: Map<int, int>)
    (expected: int -> float option)
    (candidates: (unit -> byte array option) list)
    (flags: iText.IO.Font.TrueTypeFont -> int)
    : Result<int, string> =
    // The codes to check: those the pages draw; and for the whole font (when some codes may not
    // be known, a page's or a form field's), every code the font gives a width.
    let checkedCodes =
        if whole then
            codes
            @ [ for code in 0..255 do
                    // (Not control codes: no font draws them, though encodings, as iText's
                    // WinAnsi, give them characters and PDFs widths.)
                    let drawable =
                        if symbolic then code >= 32 && code <> 127
                        else match Map.tryFind code characters with Some unicode -> not (Char.IsControl(char unicode)) | None -> false
                    if drawable && (match expected code with Some width -> width > 0.0 | None -> false) then
                        code ]
            |> List.distinct
        else
            codes
    // Whether the encoding can be kept (WinAnsi or MacRoman, any /Differences in Glyph List names),
    // or written as WinAnsi's with what differs in Glyph List names (7.21.6-2: a uniXXXX name
    // won't do), for every code checked.
    let keeps =
        match font.Get(PdfName.Encoding, true) with
        | :? PdfName as name -> PdfName.WinAnsiEncoding.Equals name || PdfName.MacRomanEncoding.Equals name
        | :? PdfDictionary as encoding ->
            let baseEncoding = encoding.GetAsName PdfName.BaseEncoding
            (PdfName.WinAnsiEncoding.Equals baseEncoding || PdfName.MacRomanEncoding.Equals baseEncoding)
            && differences font |> List.forall (snd >> listed)
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
    if not symbolic && codes |> List.exists (fun code -> not (characters.ContainsKey code)) then
        Error "a code the pages draw has no character in its encoding"
    elif not rewritable then Error "its encoding can't be kept" else
    let fits (candidate: Candidate) =
        checkedCodes
        |> List.forall (fun code ->
            let glyph = candidate.Glyph code
            glyph > 0
            && match expected code with
               | Some width -> abs (candidate.Width glyph - width) <= 1.0
               | None -> false)
    let chosen =
        candidates
        |> List.indexed
        |> List.tryPick (fun (i, bytes) ->
            bytes () |> Option.bind (fun bytes -> readCandidate bytes symbolic characters) |> Option.filter fits |> Option.map (fun c -> i, c))
    match chosen with
    | None -> Error ""
    | Some(index, candidate) ->
    let program = candidate.Program
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
    let newDescriptor = PdfDictionary()
    newDescriptor.Put(PdfName.Type, PdfName.FontDescriptor) |> ignore
    newDescriptor.Put(PdfName.FontName, PdfName fontName) |> ignore
    newDescriptor.Put(PdfName.Flags, PdfNumber((flags program &&& ~~~(4 ||| 32)) ||| (if symbolic then 4 else 32))) |> ignore
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
    Ok index

/// Embeds a font in place of font, which isn't embedded, if one fits: for the standard 14 and
/// Microsoft's three, one Legibyl brings (see candidates); for another, the same font installed
/// on this computer, if its licence lets it be embedded (its OS/2 fsType: not restricted, nor
/// bitmaps only; whole where it can't be subset). codes are the codes the pages draw with it,
/// and whole says to embed every glyph (the font fills in form fields, or some page couldn't be
/// read), not only theirs, and to check every code the font gives a width. None if font is
/// embedded, or isn't a simple font (Type 1 or TrueType) or a composite one.
let embed (document: PdfDocument) (font: PdfDictionary) (codes: int seq) (whole: bool) : (string * Outcome) option =
    let subtype = font.GetAsName PdfName.Subtype
    let descriptor = font.GetAsDictionary PdfName.FontDescriptor
    let embedded (descriptor: PdfDictionary | null) =
        match descriptor with
        | null -> false
        | d -> not (isNull (d.Get PdfName.FontFile)) || not (isNull (d.Get PdfName.FontFile2)) || not (isNull (d.Get PdfName.FontFile3))
    let baseFont = match font.GetAsName PdfName.BaseFont with null -> "" | name -> name.GetValue()
    let shownName = match baseFont.IndexOf '+' with 6 -> baseFont.Substring 7 | _ -> baseFont
    let codes = codes |> Seq.filter (fun c -> c >= 0 && c <= 255) |> Seq.distinct |> Seq.sort |> List.ofSeq
    let flagged = match descriptor with null -> false | d -> (match d.GetAsNumber PdfName.Flags with null -> false | n -> (n.IntValue() &&& 4) <> 0)
    if PdfName.Type0.Equals subtype then
        match font.GetAsArray PdfName.DescendantFonts with
        | NonNull descendants when descendants.Size() > 0 && not (isNull (descendants.GetAsDictionary 0)) && not (embedded ((descendants.GetAsDictionary 0).GetAsDictionary PdfName.FontDescriptor)) ->
            Some(shownName, Left "a composite font Legibyl can't embed")
        | _ -> None
    elif not (PdfName.Type1.Equals subtype || PdfName.TrueType.Equals subtype || PdfName.MMType1.Equals subtype) || embedded descriptor || codes.IsEmpty then
        None
    else
        match identify baseFont with
        | Some(family, bold, italic, microsoft) ->
            let symbolic = family = Symbol || family = Dingbats
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
            let bundled = candidates family bold italic microsoft
            let readBundled (file: string) () =
                let path = Path.Combine(fontsFolder (), file)
                if File.Exists path then Some(File.ReadAllBytes path) else None
            let flags _ = (if family = Mono then 1 else 0) ||| (if family = Serif then 2 else 0) ||| (if italic then 64 else 0)
            match replace document font codes whole symbolic characters expected [ for file, _ in bundled -> readBundled file ] flags with
            | Ok index ->
                if symbolic then writeSymbolUnicode document font family
                Some(shownName, Embedded(snd bundled[index], false, None))
            | Error "" -> Some(shownName, Left "no font Legibyl brings has its glyphs at its widths")
            | Error reason -> Some(shownName, Left reason)
        | None ->
        match findInstalled baseFont with
        | None -> Some(shownName, Left "it isn't installed on this computer, and no font Legibyl brings stands in for it")
        | Some face ->
        // Its own program, if it can be read (and iText, which subsets it, can read it).
        let program =
            try
                let bytes = faceProgram face
                FontProgramFactory.CreateFont(bytes, false) |> ignore
                Some bytes
            with _ ->
                None
        match program with
        | None -> Some(shownName, Left "the copy installed on this computer can't be read")
        | Some program when (sfntTable program "glyf").IsNone ->
            Some(shownName, Left "the copy installed on this computer isn't a TrueType font Legibyl can embed")
        | Some program when (embeddingFlags program &&& 0x000E) = 0x0002 || (embeddingFlags program &&& 0x0200) <> 0 ->
            Some(shownName, Left "its licence doesn't let it be embedded")
        | Some program ->
        // A symbolic TrueType font without an encoding draws its codes by its Windows Symbol cmap.
        let symbolic = flagged && isNull (font.Get PdfName.Encoding)
        let characters =
            if symbolic then (if PdfName.TrueType.Equals subtype then Some Map.empty else None)
            else encodedCharacters font
        match characters, dictionaryWidths font with
        | None, _ -> Some(shownName, Left "its encoding can't be kept")
        | _, None -> Some(shownName, Left "it gives no widths to check the installed copy against")
        | Some characters, Some widths ->
        // A font whose licence says it can't be subset is embedded whole.
        let whole = whole || (embeddingFlags program &&& 0x0100) <> 0
        let flags (program: iText.IO.Font.TrueTypeFont) = program.GetPdfFontFlags()
        match replace document font codes whole symbolic characters (fun code -> Some(widths code)) [ fun () -> Some program ] flags with
        | Ok _ -> Some(shownName, Embedded(face.FullName, true, (if whole then Some program.Length else None)))
        | Error "" -> Some(shownName, Left "the copy installed on this computer doesn't have its glyphs at its widths")
        | Error reason -> Some(shownName, Left reason)
