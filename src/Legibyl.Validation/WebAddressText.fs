/// Web addresses in a page's text, as the text index reads each page (see MarkedContentTextIndex
/// and UnlinkedUrls). Only what's found is kept, not the page's glyphs.
///
/// How text is taken for an address is qr-link-pdf's (github.com/jbtule/qr-link-pdf,
/// TextLinker.fs): the page's text in lines, a space where the gap between glyphs is a word's;
/// "http(s)://" or "www." up to whitespace; trailing punctuation off, a closing bracket only when
/// it isn't balanced. Lines are found by where glyphs are, not the order they're drawn in, and a
/// glyph drawn twice in one place (as bold is faked) is read once. An address goes on past a gap
/// on its line to a hyphen that touches more of it (see continuation).
module internal Legibyl.Validation.WebAddressText

open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions

/// A glyph a page draws: what it reads as, its box, where its baseline starts and ends (its
/// advance), its size, and the marked content it's in (its innermost MCID; -1 for none).
type Glyph =
    { Value: string
      Left: float
      Bottom: float
      Right: float
      Top: float
      Start: float
      End: float
      Baseline: float
      Size: float
      Mcid: int }

/// An address as a page shows it, with its box and the marked content its glyphs are in.
type Found =
    { Text: string
      Left: float
      Bottom: float
      Right: float
      Top: float
      /// The MCIDs of its glyphs, each once (-1 for glyphs in none).
      Mcids: int list
      /// Whether it's all of its one marked content's text (whitespace aside).
      WholeRun: bool }

/// "http(s)://" or "www.", not inside a word, a number or an e-mail address, up to whitespace.
let private addressPattern =
    Regex(@"(?<![\p{L}\p{N}_@.\-/])(?:https?://|www\.)[^\s<>""']+", RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant)

/// Where an address goes on past a gap on its line: a hyphen touching a letter or digit
/// (".../go/dol -diversification", the hyphen set apart as a line is justified). Prose doesn't
/// start a word with a hyphen; a dash set off as one ("... - then") has a gap after it, too.
let private continuation = Regex(@"\G {1,2}(-[\p{L}\p{N}][^\s<>""']*)", RegexOptions.CultureInvariant)

/// Trailing punctuation off ("see https://example.org." links the address, not the full stop);
/// a closing bracket only when it isn't balanced inside, so ".../wiki/Foo_(bar)" stays whole.
let rec trimTrailing (candidate: string) =
    if candidate = "" then
        candidate
    else
        let last = candidate[candidate.Length - 1]
        let count (c: char) = candidate |> Seq.filter ((=) c) |> Seq.length
        let trimmable =
            match last with
            | ')' -> count ')' > count '('
            | ']' -> count ']' > count '['
            | '}' -> count '}' > count '{'
            | c -> ".,;:!?\"'".Contains c
        if trimmable then trimTrailing (candidate.Substring(0, candidate.Length - 1)) else candidate

let private blank (s: string) = String.IsNullOrWhiteSpace s

/// The page's lines, top to bottom, each left to right: glyphs whose baselines are within half
/// their size of the line's. A glyph drawn again where it already is (the same glyph starting
/// within half its advance of it: "tt" is two) is left out.
let private lines (glyphs: Glyph array) : Glyph list list =
    let byBaseline = glyphs |> Array.sortBy (fun glyph -> -glyph.Baseline)
    let rows = List<List<Glyph>>()
    for glyph in byBaseline do
        let last = if rows.Count > 0 then Some rows[rows.Count - 1] else None
        match last with
        | Some row when abs (row[0].Baseline - glyph.Baseline) <= 0.5 * max glyph.Size row[0].Size -> row.Add glyph
        | _ -> rows.Add(List [ glyph ])
    [ for row in rows ->
          let sorted = row |> Seq.sortBy _.Start |> List.ofSeq
          sorted
          |> List.fold
              (fun (kept: Glyph list) glyph ->
                  match kept with
                  | previous :: _ when previous.Value = glyph.Value && abs (glyph.Start - previous.Start) < 0.5 * abs (previous.End - previous.Start) -> kept
                  | _ -> glyph :: kept)
              []
          |> List.rev ]

/// The addresses in a page's glyphs (those not in artifacts, which aren't read).
let find (glyphs: Glyph array) : Found list =
    let lines = lines glyphs
    let text = StringBuilder()
    let from = List<Glyph option>()
    let add (s: string) (glyph: Glyph option) =
        for c in s do
            text.Append c |> ignore
            from.Add glyph
    for line in lines do
        if text.Length > 0 then add "\n" None
        let line = Array.ofList line
        line
        |> Array.iteri (fun i glyph ->
            if i > 0 then
                let previous = line[i - 1]
                // A word's gap: more than an eighth of the type's size between one glyph's advance
                // and the next one's start (its ink can be further: after a full stop, say).
                if not (blank glyph.Value) && not (blank previous.Value) && glyph.Start - previous.End > 0.125 * max glyph.Size previous.Size then
                    add " " None
            add glyph.Value (Some glyph))
    let text = text.ToString()
    // Each marked content's text as the page's lines read it, whitespace aside.
    let runs = Dictionary<int, StringBuilder>()
    for line in lines do
        for glyph in line do
            if glyph.Mcid >= 0 && not (blank glyph.Value) then
                if not (runs.ContainsKey glyph.Mcid) then runs[glyph.Mcid] <- StringBuilder()
                runs[glyph.Mcid].Append(glyph.Value.Replace(" ", "")) |> ignore
    [ for m in addressPattern.Matches text do
          // An address that ends a sentence (trailing punctuation off) doesn't go on.
          let more = if trimTrailing m.Value = m.Value then continuation.Match(text, m.Index + m.Length) else Match.Empty
          let written, last =
              if more.Success then
                  let written = trimTrailing (m.Value + more.Groups[1].Value)
                  written, more.Groups[1].Index + (written.Length - m.Value.Length) - 1
              else
                  let written = trimTrailing m.Value
                  written, m.Index + written.Length - 1
          let glyphs = [ for i in m.Index .. last do match from[i] with Some glyph -> glyph | None -> () ] |> List.distinct
          if written <> "" && not glyphs.IsEmpty then
              let mcids = glyphs |> List.map _.Mcid |> List.distinct
              let own = String.Concat(glyphs |> List.map (fun glyph -> glyph.Value.Replace(" ", "")))
              yield
                  { Text = written
                    Left = glyphs |> List.map _.Left |> List.min
                    Bottom = glyphs |> List.map _.Bottom |> List.min
                    Right = glyphs |> List.map _.Right |> List.max
                    Top = glyphs |> List.map _.Top |> List.max
                    Mcids = mcids
                    WholeRun =
                      match mcids with
                      | [ mcid ] when mcid >= 0 -> runs[mcid].ToString() = own
                      | _ -> false } ]
