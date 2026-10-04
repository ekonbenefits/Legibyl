/// Which characters a font's codes draw, as a ToUnicode CMap says it (so text can be read and
/// copied), worked out from what the font itself says when the PDF doesn't: a glyph's name (by
/// the Adobe Glyph List's rules), or the embedded TrueType program's own Unicode cmap. Never
/// guessed: a code nothing says is left out.
module Legibyl.Validation.ToUnicodeMaps

open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open UglyToad.PdfPig.Fonts.TrueType

/// Whether text is a value a ToUnicode map may give (none of U+0000, U+FEFF, U+FFFE: PDF/UA
/// 7.21.7-2), and something at all.
let isReadable (text: string) =
    text <> "" && not (text.Contains '\u0000') && not (text.Contains '\uFEFF') && not (text.Contains '\uFFFE')

let private hexValue (hex: string) =
    match Int32.TryParse(hex, Globalization.NumberStyles.HexNumber, null) with
    | true, value -> Some value
    | _ -> None

/// The characters Adobe's StandardEncoding gives its codes, from the standard Helvetica's metrics
/// (an AFM's codes are its font's built-in encoding: for the standard 14's text fonts, this one).
let private standardEncoding =
    lazy
        (let helvetica = iText.IO.Font.FontProgramFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA
         Map.ofList [ for code in 0..255 do
                          match helvetica.GetGlyphByCode code with
                          | null -> ()
                          | glyph when glyph.GetUnicode() > 0 -> code, glyph.GetUnicode()
                          | _ -> () ])

/// The character a simple font's base encoding (StandardEncoding, WinAnsiEncoding,
/// MacRomanEncoding…) gives each code (0 for none), or None for an encoding it doesn't know.
let baseEncoding (name: string) : (int -> int) option =
    if name = "StandardEncoding" then
        Some(fun code -> Map.tryFind code standardEncoding.Value |> Option.defaultValue 0)
    else
        try
            let encoding = iText.IO.Font.FontEncoding.CreateFontEncoding name
            Some(fun code -> encoding.GetUnicode code)
        with _ ->
            None

/// The text a glyph name stands for, by the Adobe Glyph List's rules: what follows its first
/// full stop is a variant ("a.sc" is "a"); underscores join components ("f_i" is "fi"); each
/// component is a glyph list name, "uniXXXX" (one or more four-digit codes), or "uXXXX" to
/// "uXXXXXX". None if a component is none of those.
let glyphNameText (name: string) : string option =
    let name = match name.IndexOf '.' with -1 -> name | dot -> name.Substring(0, dot)
    if name = "" then
        None
    else
        let partText (part: string) : string option =
            let unicode = iText.IO.Font.AdobeGlyphList.NameToUnicode part
            // (iText reads uniXXXX itself, surrogates and all: only a real character counts.)
            if unicode > 0 && unicode <= 0x10FFFF && (unicode < 0xD800 || unicode > 0xDFFF) then
                Some(Char.ConvertFromUtf32 unicode)
            elif unicode > 0 then
                None
            elif Regex.IsMatch(part, "^uni([0-9A-F]{4})+$") then
                let codes = [ for i in 3 .. 4 .. part.Length - 4 -> hexValue (part.Substring(i, 4)) ]
                if codes |> List.forall (function Some c -> c < 0xD800 || c > 0xDFFF | None -> false) then
                    Some(String.Join("", codes |> List.map (fun c -> string (char c.Value))))
                else
                    None
            elif Regex.IsMatch(part, "^u[0-9A-F]{4,6}$") then
                match hexValue (part.Substring 1) with
                | Some c when (c < 0xD800 || c > 0xDFFF) && c <= 0x10FFFF -> Some(Char.ConvertFromUtf32 c)
                | _ -> None
            else
                None
        let parts = name.Split '_' |> Array.map partText
        if parts |> Array.forall Option.isSome then
            let text = String.Join("", parts |> Array.map Option.get)
            if isReadable text then Some text else None
        else
            None

/// Whether a code point is in one of Unicode's private use areas, which mean nothing to a reader.
let private privateUse (code: int) = (code >= 0xE000 && code <= 0xF8FF) || code >= 0xF0000

/// What an embedded TrueType program says each glyph is: the code point its Unicode cmaps map to it
/// (one outside the private use areas over one in them, then the lowest, where several do), else
/// its post table name's text. None if it can't be read.
let trueTypeGlyphText (program: byte array) : (int -> string option) option =
    try
        let font = Parser.TrueTypeFontParser.Parse(TrueTypeDataBytes program)
        let unicodeTables =
            // (PdfPig says its tables are never null, but a font without one gives null.)
            match Option.ofObj (box font.TableRegister.CMapTable) with
            | None -> []
            | Some cmap ->
                let cmap = cmap :?> Tables.CMapTable
                [ for table in cmap.SubTables do
                      // Windows Unicode (BMP or full), or Unicode itself.
                      if (table.PlatformId = Tables.CMapSubTables.TrueTypeCMapPlatform.Windows && (table.EncodingId = 1us || table.EncodingId = 10us))
                         || table.PlatformId = Tables.CMapSubTables.TrueTypeCMapPlatform.Unicode then
                          table ]
        let best = Dictionary<int, int>()
        let consider (glyph: int) (code: int) =
            if glyph > 0 then
                match best.TryGetValue glyph with
                | true, other when (privateUse other, other) <= (privateUse code, code) -> ()
                | _ -> best[glyph] <- code
        for table in unicodeTables do
            // The Basic Multilingual Plane (but surrogates) in every Unicode table; the planes above
            // it only in the ones that can map them (Windows full repertoire, Unicode 2.0 full).
            let full =
                (table.PlatformId = Tables.CMapSubTables.TrueTypeCMapPlatform.Windows && table.EncodingId = 10us)
                || (table.PlatformId = Tables.CMapSubTables.TrueTypeCMapPlatform.Unicode && (table.EncodingId = 4us || table.EncodingId = 6us))
            for code in Seq.append (seq { 0x01 .. 0xD7FF }) (seq { 0xE000 .. (if full then 0x10FFFF else 0xFFFF) }) do
                consider (table.CharacterCodeToGlyphIndex code) code
        let byGlyph = best |> Seq.map (fun kv -> kv.Key, Char.ConvertFromUtf32 kv.Value) |> dict
        let names =
            match Option.ofObj (box font.TableRegister.PostScriptTable) with
            | None -> [||]
            | Some post ->
                match Option.ofObj (box (post :?> Tables.PostScriptTable).GlyphNames) with
                | Some names -> Array.ofSeq (names :?> IReadOnlyList<string>)
                | None -> [||]
        Some(fun glyph ->
            match byGlyph.TryGetValue glyph with
            | true, text -> Some text
            | _ when glyph > 0 && glyph < names.Length -> glyphNameText names[glyph]
            | _ -> None)
    with _ ->
        None

/// The glyph an embedded TrueType program's post table names name, if it does.
let trueTypeGlyphNamed (program: byte array) : (string -> int option) option =
    try
        let font = Parser.TrueTypeFontParser.Parse(TrueTypeDataBytes program)
        match Option.ofObj (box font.TableRegister.PostScriptTable) with
        | Some post ->
            match Option.ofObj (box (post :?> Tables.PostScriptTable).GlyphNames) with
            | Some names ->
                let index = Dictionary<string, int>()
                (names :?> IReadOnlyList<string>) |> Seq.iteri (fun glyph name -> if not (String.IsNullOrEmpty name) && not (index.ContainsKey name) then index[name] <- glyph)
                Some(fun name -> match index.TryGetValue name with | true, glyph -> Some glyph | _ -> None)
            | None -> None
        | None -> None
    with _ ->
        None

/// The code → text pairs a ToUnicode CMap's program gives (its bfchar and bfrange entries).
let parse (program: string) : Map<int, string> =
    let hexText (hex: string) =
        let bytes = Convert.FromHexString(if hex.Length % 2 = 1 then hex + "0" else hex)
        Encoding.BigEndianUnicode.GetString bytes
    let entries = Dictionary<int, string>()
    let section (start: string) (finish: string) =
        [ for m in Regex.Matches(program, $"{start}(.*?){finish}", RegexOptions.Singleline) -> m.Groups[1].Value ]
    for body in section "beginbfchar" "endbfchar" do
        for m in Regex.Matches(body, @"<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]*)>") do
            entries[Convert.ToInt32(m.Groups[1].Value, 16)] <- hexText m.Groups[2].Value
    for body in section "beginbfrange" "endbfrange" do
        for m in Regex.Matches(body, @"<([0-9A-Fa-f]+)>\s*<([0-9A-Fa-f]+)>\s*(<([0-9A-Fa-f]*)>|\[([^\]]*)\])") do
            let first, last = Convert.ToInt32(m.Groups[1].Value, 16), Convert.ToInt32(m.Groups[2].Value, 16)
            if m.Groups[4].Success then
                // Consecutive codes, the destination's last byte counting up.
                let start = Convert.FromHexString(if m.Groups[4].Value.Length % 2 = 1 then m.Groups[4].Value + "0" else m.Groups[4].Value)
                for code in first .. min last (first + 0xFFFF) do
                    // The destination plus the code's offset, as a big-endian number of its length.
                    let bytes = Array.copy start
                    let mutable carry = code - first
                    let mutable i = bytes.Length - 1
                    while carry > 0 && i >= 0 do
                        let sum = int bytes[i] + (carry &&& 0xFF)
                        bytes[i] <- byte (sum &&& 0xFF)
                        carry <- (carry >>> 8) + (sum >>> 8)
                        i <- i - 1
                    entries[code] <- Encoding.BigEndianUnicode.GetString bytes
            else
                let values = [ for v in Regex.Matches(m.Groups[5].Value, @"<([0-9A-Fa-f]*)>") -> hexText v.Groups[1].Value ]
                for code, value in List.zip [ first .. first + values.Length - 1 ] values do
                    if code <= last then entries[code] <- value
    entries |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq

/// A ToUnicode CMap's program for entries, its codes bytes long (1 for a simple font, 2 for an
/// Identity composite one).
let write (bytes: int) (entries: Map<int, string>) =
    let code (c: int) = c.ToString(if bytes = 1 then "X2" else "X4")
    let text (t: string) = Convert.ToHexString(Encoding.BigEndianUnicode.GetBytes t)
    let out = StringBuilder()
    out.Append("/CIDInit /ProcSet findresource begin\n12 dict begin\nbegincmap\n") |> ignore
    out.Append("/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def\n") |> ignore
    out.Append("/CMapName /Adobe-Identity-UCS def\n/CMapType 2 def\n") |> ignore
    let highest = code (if bytes = 1 then 0xFF else 0xFFFF)
    out.Append($"1 begincodespacerange\n<{code 0}> <{highest}>\nendcodespacerange\n") |> ignore
    for chunk in entries |> Map.toList |> List.chunkBySize 100 do
        out.Append($"{chunk.Length} beginbfchar\n") |> ignore
        for c, t in chunk do
            out.Append($"<{code c}> <{text t}>\n") |> ignore
        out.Append("endbfchar\n") |> ignore
    out.Append("endcmap\nCMapName currentdict /CMap defineresource pop\nend\nend\n") |> ignore
    out.ToString()
