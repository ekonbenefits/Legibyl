/// Finds lists tagged as plain paragraphs: runs of sibling paragraphs that each start with a
/// list marker ("•", "(1)", "a.") drawn as ordinary text. LegibylChecks reports them; the
/// ListsFromParagraphs fix tags each run as a list (L > LI > LBody > the original P). Only the
/// structure tree changes, never the page's drawn content, so the document looks the same;
/// the marker stays part of each item's text rather than being split out into an Lbl.
module internal Legibyl.Validation.ParagraphLists

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open iText.Kernel.Pdf
open Legibyl.Validation.Interop
open Legibyl.Validation.StructureTreeObjects

/// A run of paragraphs that looks like one list.
type ListRun =
    { /// The element whose /K array holds the paragraphs.
      Parent: PdfDictionary
      /// Their positions in that array, including any empty paragraphs between items.
      Indexes: int list
      /// Each item's paragraph, followed by any empty paragraphs between it and the next.
      Items: PdfDictionary list list
      /// Where the first one is, e.g. "StructTreeRoot/K[0]/K[3]".
      Path: string
      Page: int
      /// The first and last markers, e.g. "(1)" and "(3)".
      FirstMarker: string
      LastMarker: string
      /// The /ListNumbering the markers call for, e.g. Decimal or Disc.
      Numbering: PdfName
      Geometry: StructureGeometry option }

// ---- Markers ----

type private Style =
    | Decimal
    | LowerAlpha
    | UpperAlpha
    | LowerRoman
    | UpperRoman

type private Marker =
    | Bullet of string
    /// An ordinal's possible readings ("i" could be alpha 9 or roman 1), with its punctuation
    /// ("(1)", "1.", "1)") so a list keeps one form.
    | Ordinal of readings: (Style * int) list * form: string

let private bulletPattern = Regex(@"^([•▪◦·‣∙]\s*|[-–]\s+)", RegexOptions.Compiled)
let private ordinalPattern = Regex(@"^(\()?([0-9]{1,3}|[a-zA-Z]|[ivxlcIVXLC]{1,6})([.)])\s+", RegexOptions.Compiled)

let private romanValue (text: string) =
    let digit = function 'i' -> 1 | 'v' -> 5 | 'x' -> 10 | 'l' -> 50 | 'c' -> 100 | _ -> 0
    let digits = text.ToLowerInvariant() |> Seq.map digit |> List.ofSeq
    let value =
        digits
        |> List.mapi (fun i d -> if i + 1 < digits.Length && d < digits[i + 1] then -d else d)
        |> List.sum
    // Only canonical numerals ("iv", not "iiii"), so ordinary words don't read as numbers.
    let rec toRoman n =
        [ 100, "c"; 90, "xc"; 50, "l"; 40, "xl"; 10, "x"; 9, "ix"; 5, "v"; 4, "iv"; 1, "i" ]
        |> List.tryFind (fun (v, _) -> n >= v)
        |> Option.map (fun (v, s) -> s + toRoman (n - v))
        |> Option.defaultValue ""
    if value > 0 && toRoman value = text.ToLowerInvariant() then Some value else None

/// The marker a paragraph's text starts with, and the marker text itself.
let private markerOf (text: string) : (Marker * string) option =
    let bullet = bulletPattern.Match text
    if bullet.Success then
        Some(Bullet(bullet.Value.Trim()), bullet.Value.Trim())
    else
        let ordinal = ordinalPattern.Match text
        let opening = ordinal.Groups[1].Value
        let closing = ordinal.Groups[3].Value
        // "(1)" or "1." or "1)", not "(1." ; a bracket must close.
        if not ordinal.Success || (opening = "(" && closing <> ")") then
            None
        else
            let value = ordinal.Groups[2].Value
            let readings =
                [ if Char.IsDigit value[0] then
                      yield Decimal, int value
                  else
                      if value.Length = 1 && Char.IsLower value[0] then yield LowerAlpha, int value[0] - int 'a' + 1
                      if value.Length = 1 && Char.IsUpper value[0] then yield UpperAlpha, int value[0] - int 'A' + 1
                      match romanValue value with
                      | Some roman when value = value.ToLowerInvariant() -> yield LowerRoman, roman
                      | Some roman when value = value.ToUpperInvariant() -> yield UpperRoman, roman
                      | _ -> () ]
            if readings.IsEmpty then None
            else Some(Ordinal(readings, opening + closing), ordinal.Value.Trim())

/// The /ListNumbering for a run of markers, if they form one list: the same bullet each
/// time, or ordinals of one style and form that count up by one.
let private numberingOf (markers: Marker list) : PdfName option =
    match markers with
    | Bullet first :: _ when markers |> List.forall ((=) (Bullet first)) ->
        match first with
        | "•" | "∙" | "·" -> Some(PdfName "Disc")
        | "◦" -> Some(PdfName "Circle")
        | "▪" -> Some(PdfName "Square")
        | _ -> Some(PdfName "None")
    | Ordinal(_, form) :: _ ->
        let readings =
            markers
            |> List.map (function
                | Ordinal(readings, f) when f = form -> Some readings
                | _ -> None)
        if readings |> List.exists Option.isNone then
            None
        else
            let readings = readings |> List.choose id
            [ Decimal; LowerAlpha; UpperAlpha; LowerRoman; UpperRoman ]
            |> List.tryPick (fun style ->
                let values = readings |> List.map (List.tryFind (fun (s, _) -> s = style) >> Option.map snd)
                let counting =
                    values |> List.forall Option.isSome
                    && values |> List.choose id |> List.pairwise |> List.forall (fun (a, b) -> b = a + 1)
                if counting then
                    Some(
                        match style with
                        | Decimal -> PdfName "Decimal"
                        | LowerAlpha -> PdfName "LowerAlpha"
                        | UpperAlpha -> PdfName "UpperAlpha"
                        | LowerRoman -> PdfName "LowerRoman"
                        | UpperRoman -> PdfName "UpperRoman"
                    )
                else
                    None)
    | _ -> None

/// The /ListNumbering texts would have as one list (e.g. "Disc"), if there are at least two,
/// each starts with a marker, and the markers form one list. For layout suggestions, which
/// have paragraphs' text but no structure tree yet.
let numberingOfTexts (texts: string list) : string option =
    let markers = texts |> List.map markerOf
    if texts.Length >= 2 && markers |> List.forall Option.isSome then
        numberingOf (markers |> List.choose (Option.map fst)) |> Option.map _.GetValue()
    else
        None

/// Whether text starts with a list marker.
let startsWithMarker (text: string) = (markerOf text).IsSome

// ---- Finding runs ----

// Items of one list share an indent, within this many points.
[<Literal>]
let private SameIndent = 3.0

type private Candidate =
    { Index: int
      Paragraph: PdfDictionary
      /// Empty paragraphs between this one and the next item, with their positions.
      Trailing: (int * PdfDictionary) list
      Path: string
      Marker: Marker
      MarkerText: string
      Page: int
      Geometry: StructureGeometry option }

/// Containers whose paragraphs aren't a list even when numbered: a table of contents (its
/// entries are often "1. Scope"), and a list item's label. A list item's body can hold a
/// flattened nested list, which is fair game: an L inside an LBody is valid.
let private isExcluded (tag: PdfName | null) =
    PdfName.TOC.Equals tag || PdfName.TOCI.Equals tag || PdfName.Lbl.Equals tag

/// Every run of two or more sibling paragraphs on one page that each start with a marker,
/// share an indent, and whose markers form one list. Paragraphs in a table of contents or a
/// list item's label are left alone.
let find (document: PdfDocument) context : ListRun list =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> []
    | structTreeRoot ->
        let visits = walk structTreeRoot context |> Array.ofSeq

        // Elements inside an excluded container (walk goes depth first, and each path extends
        // its parent's, so those we're inside are a stack of path prefixes).
        let inside = HashSet<PdfDictionary>(HashIdentity.Reference)
        let open' = Stack<string>()
        for visit in visits do
            while open'.Count > 0 && not (visit.Path.StartsWith(open'.Peek() + "/", StringComparison.Ordinal)) do
                open'.Pop() |> ignore
            if open'.Count > 0 then
                inside.Add visit.Element |> ignore
            if isExcluded (standardTag visit.Element context) then
                open'.Push visit.Path

        let parents =
            [ yield structTreeRoot, "StructTreeRoot/K"
              for visit in visits do
                  if not (inside.Contains visit.Element) && not (isExcluded (standardTag visit.Element context)) then
                      yield visit.Element, $"{visit.Path}/K" ]

        [ for parent, kidsPath in parents do
              match resolve (parent.Get(PdfName.K)) with
              | :? PdfArray as kids ->
                  let candidateAt i : Candidate option =
                      match resolve (kids.Get i) with
                      | :? PdfDictionary as paragraph when isParagraph paragraph context ->
                          let page = structElemPage paragraph context None
                          let geometry = textGeometry paragraph context page
                          let page = page |> Option.orElse (geometry |> Option.map _.PageNumber)
                          let text = Regex.Replace(extractText paragraph context page, @"\s+", " ").Trim()
                          match page, markerOf text with
                          | Some page, Some(marker, markerText) ->
                              Some
                                  { Index = i
                                    Paragraph = paragraph
                                    Trailing = []
                                    Path = $"{kidsPath}[{i}]"
                                    Marker = marker
                                    MarkerText = markerText
                                    Page = page
                                    Geometry = geometry }
                          | _ -> None
                      | _ -> None

                  // Maximal runs of consecutive marked paragraphs on one page, sharing an indent.
                  // Empty paragraphs between items (blank lines, common in exported documents)
                  // don't end a run; they go with the item before them.
                  let runs = List<List<Candidate>>()
                  let mutable blanks: (int * PdfDictionary) list = []
                  for i in 0 .. kids.Size() - 1 do
                      match candidateAt i with
                      | Some candidate ->
                          let run = if runs.Count > 0 then Some runs[runs.Count - 1] else None
                          let continues =
                              match run with
                              | Some run ->
                                  let last = run[run.Count - 1]
                                  last.Index + blanks.Length = i - 1
                                  && last.Page = candidate.Page
                                  && (match last.Geometry, candidate.Geometry with
                                      | Some a, Some b -> abs (a.Left - b.Left) <= SameIndent
                                      | _ -> true)
                              | None -> false
                          match run with
                          | Some run when continues ->
                              run[run.Count - 1] <- { run[run.Count - 1] with Trailing = List.rev blanks }
                              run.Add candidate
                          | _ -> runs.Add(List [ candidate ])
                          blanks <- []
                      | None ->
                          blanks <-
                              match resolve (kids.Get i) with
                              | :? PdfDictionary as paragraph when
                                  isParagraph paragraph context
                                  && isSubtreeEmpty paragraph context (structElemPage paragraph context None)
                                  ->
                                  (i, paragraph) :: blanks
                              // Anything else ends the run: the arithmetic above won't line up.
                              | _ -> []

                  for run in runs do
                      if run.Count >= 2 then
                          let items = List.ofSeq run
                          match numberingOf (items |> List.map _.Marker) with
                          | Some numbering ->
                              yield
                                  { Parent = parent
                                    Indexes = items |> List.collect (fun item -> item.Index :: List.map fst item.Trailing)
                                    Items = items |> List.map (fun item -> item.Paragraph :: List.map snd item.Trailing)
                                    Path = items.Head.Path
                                    Page = items.Head.Page
                                    FirstMarker = items.Head.MarkerText
                                    LastMarker = (List.last items).MarkerText
                                    Numbering = numbering
                                    Geometry = items |> List.choose _.Geometry |> unionGeometry }
                          | None -> ()
              | _ -> () ]

// ---- Tagging a run as a list ----

let private structElement (document: PdfDocument) (tag: PdfName) (parent: PdfDictionary) =
    let element = PdfDictionary()
    element.Put(PdfName.Type, PdfName.StructElem) |> ignore
    element.Put(PdfName.S, tag) |> ignore
    element.Put(PdfName.P, parent) |> ignore
    element.MakeIndirect document |> ignore
    element

/// Wraps the run's paragraphs in L > LI > LBody, in place of the paragraphs in their parent
/// (an item's trailing empty paragraphs go in its LBody too). Each paragraph keeps its own
/// content (and parent-tree entries); only its parent changes.
let tagAsList (document: PdfDocument) (run: ListRun) =
    let list = structElement document PdfName.L run.Parent
    let attributes = PdfDictionary()
    attributes.Put(PdfName.O, PdfName.List) |> ignore
    attributes.Put(PdfName.ListNumbering, run.Numbering) |> ignore
    list.Put(PdfName.A, attributes) |> ignore

    let items = PdfArray()
    for paragraphs in run.Items do
        let item = structElement document PdfName.LI list
        let body = structElement document PdfName.LBody item
        let contents = PdfArray()
        for paragraph in paragraphs do
            contents.Add paragraph
            paragraph.Put(PdfName.P, body) |> ignore
        body.Put(PdfName.K, contents) |> ignore
        item.Put(PdfName.K, body) |> ignore
        items.Add item
    list.Put(PdfName.K, items) |> ignore

    let kids = run.Parent.GetAsArray(PdfName.K) |> nonNull
    for index in List.rev (List.tail run.Indexes) do
        kids.Remove index
    kids.Set(List.head run.Indexes, list) |> ignore
