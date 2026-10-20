/// Finds lists tagged as plain paragraphs: runs of sibling paragraphs that each start with a
/// list marker ("•", "(1)", "a.") drawn as ordinary text, or that are only a marker with the
/// item's text in the paragraphs beside it. LegibylChecks reports them; the Lists fix tags
/// each run as a list (L > LI > LBody > the original P). Only the structure tree changes,
/// never the page's drawn content, so the document looks the same; a marker that starts its
/// paragraph stays part of the item's text rather than being split out into an Lbl, and a
/// marker that's a paragraph of its own becomes the item's Lbl.
///
/// It also finds a list's items left outside it: paragraphs that start with its marker just
/// before or after an L, at its items' indent (see ListAddition), which the fix moves into it.
module internal Legibyl.Validation.ParagraphLists

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open iText.Kernel.Pdf
open Legibyl.Validation.Interop
open Legibyl.Validation.StructureTreeObjects

/// One item of a list run.
type ListItem =
    { /// A paragraph that's only the item's marker ("•"), which becomes its label (Lbl).
      Label: PdfDictionary option
      /// The item's paragraphs: its text (the marker first, when it has no label of its own),
      /// then any empty paragraphs between it and the next item.
      Body: PdfDictionary list }

/// A run of paragraphs that looks like one list.
type ListRun =
    { /// The element whose /K array holds the paragraphs.
      Parent: PdfDictionary
      /// Their positions in that array, including any empty paragraphs between items.
      Indexes: int list
      Items: ListItem list
      /// Where the first one is, e.g. "StructTreeRoot/K[0]/K[3]".
      Path: string
      Page: int
      /// The first and last markers, e.g. "(1)" and "(3)".
      FirstMarker: string
      LastMarker: string
      /// The /ListNumbering the markers call for, e.g. Decimal or Disc.
      Numbering: PdfName
      Geometry: StructureGeometry option }

/// Paragraphs that start with a list's marker, beside the list (L) in their parent but outside
/// it: more of its items, one before its first, say, as some exports leave a list's first item.
type ListAddition =
    { /// The list they belong in.
      List: PdfDictionary
      /// The element whose /K array holds the list and the paragraphs.
      Parent: PdfDictionary
      /// The items to go before the list's first and after its last: each one's paragraphs, its
      /// marker's first, then any that carry on its text on the lines below it.
      Before: PdfDictionary list list
      After: PdfDictionary list list
      /// The paragraphs' positions in the parent's /K.
      Indexes: int list
      /// Where the first one is, e.g. "StructTreeRoot/K[0]/K[3]".
      Path: string
      Page: int
      /// The first one's marker, e.g. "•".
      Marker: string
      /// How many items the list has already.
      ListItems: int
      Geometry: StructureGeometry option }

/// What find finds: runs of paragraphs to tag as lists, and paragraphs to add to lists beside them.
type Found =
    { Runs: ListRun list
      Additions: ListAddition list }

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

/// The marker text is, when it's nothing but one ("•", "1.", "(a)"): a bullet tagged as a
/// paragraph of its own, its item's text in the paragraphs after it.
let private markerOnly (text: string) =
    match markerOf (text + " ") with
    | Some(_, markerText) as found when markerText = text -> found
    | _ -> None

/// Whether text is a list marker and nothing else ("•", "1.").
let isMarkerOnly (text: string) = (markerOnly (text.Trim())).IsSome

// ---- Finding runs ----

// Items of one list share an indent, within this many points.
[<Literal>]
let private SameIndent = 3.0

type private Candidate =
    { Index: int
      Paragraph: PdfDictionary
      /// What follows it in its item, with their positions: empty paragraphs between it and the
      /// next item, and, after a marker on its own, the paragraphs of its text.
      Trailing: (int * PdfDictionary) list
      Path: string
      Marker: Marker
      MarkerText: string
      /// Whether the paragraph is only its marker, its item's text in the paragraphs after it.
      MarkerOnly: bool
      Page: int
      Geometry: StructureGeometry option
      /// Where its item's text is, after a marker on its own.
      TextGeometry: StructureGeometry list }

/// Containers whose paragraphs aren't a list even when numbered: a table of contents (its
/// entries are often "1. Scope"), and a list item's label. A list item's body can hold a
/// flattened nested list, which is fair game: an L inside an LBody is valid.
let private isExcluded (tag: PdfName | null) =
    PdfName.TOC.Equals tag || PdfName.TOCI.Equals tag || PdfName.Lbl.Equals tag

/// Every run of two or more sibling paragraphs on one page that each start with a marker,
/// share an indent, and whose markers form one list; and paragraphs that start with a marker
/// beside a list of the same markers, at its items' indent (see ListAddition), which aren't
/// taken into runs of their own. Paragraphs in a table of contents or a list item's label are
/// left alone.
let find (document: PdfDocument) context : Found =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> { Runs = []; Additions = [] }
    | structTreeRoot ->
        let visits = walk structTreeRoot context |> Array.ofSeq
        let additions = List<ListAddition>()

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

        let runs =
          [ for parent, kidsPath in parents do
              match resolve (parent.Get(PdfName.K)) with
              | :? PdfArray as kids ->
                  let paragraphAt i =
                      match resolve (kids.Get i) with
                      | :? PdfDictionary as paragraph when isParagraph paragraph context ->
                          let page = structElemPage paragraph context None
                          let geometry = textGeometry paragraph context page
                          let page = page |> Option.orElse (geometry |> Option.map _.PageNumber)
                          let text = Regex.Replace(extractText paragraph context page, @"\s+", " ").Trim()
                          Some(paragraph, page, geometry, text)
                      | _ -> None

                  let candidateAt i : Candidate option =
                      match paragraphAt i with
                      | Some(paragraph, Some page, geometry, text) ->
                          match markerOf text with
                          | Some(marker, markerText) ->
                              Some
                                  { Index = i
                                    Paragraph = paragraph
                                    Trailing = []
                                    Path = $"{kidsPath}[{i}]"
                                    Marker = marker
                                    MarkerText = markerText
                                    MarkerOnly = (markerOnly text).IsSome
                                    Page = page
                                    Geometry = geometry
                                    TextGeometry = [] }
                          | None -> None
                      | _ -> None

                  // ---- Paragraphs that are more items of a list beside them ----

                  // A sentence ends at its full stop, even inside closing quotes or brackets.
                  let endsSentence (text: string) =
                      let text = text.TrimEnd('”', '’', '"', '\'', ')', ']', '»')
                      text <> "" && ".:;!?…".Contains text[text.Length - 1]
                  // An item at i: a paragraph that starts with a marker, and the indexes of those that
                  // carry on its text (its lines tagged apart): straight below, as near as lines of
                  // its size are (not as far as a paragraph's spacing), indented past the marker (as
                  // an item's lines hang; body text after it starts at the margin), not starting with
                  // a marker, where the text before them doesn't end a sentence.
                  let itemAt i : (Candidate * int list) option =
                      match candidateAt i, paragraphAt i with
                      | Some({ MarkerOnly = false; Geometry = Some start } as candidate), Some(paragraph, _, _, text) ->
                          let size =
                              textPieces paragraph context (Some candidate.Page)
                              |> List.tryPick _.FontSize
                              |> Option.defaultValue 12.0
                          let line = size
                          let rec more j (previous: StructureGeometry) previousText rest =
                              match (if j < kids.Size() then paragraphAt j else None) with
                              | Some(_, Some page, Some geometry, text) when
                                  page = candidate.Page
                                  && text <> ""
                                  && not (startsWithMarker text)
                                  && not (endsSentence previousText)
                                  && geometry.Left > start.Left + SameIndent
                                  && geometry.Left <= start.Left + 24.0
                                  && previous.Bottom - geometry.Top >= -2.0
                                  && previous.Bottom - geometry.Top <= line
                                  ->
                                  more (j + 1) geometry text (j :: rest)
                              | _ -> List.rev rest
                          Some(candidate, more (i + 1) start text [])
                      | _ -> None
                  let items = Array.init (kids.Size()) itemAt
                  // The item ending at each index (its last paragraph's), to walk up from a list.
                  let endingAt = Dictionary<int, Candidate * int list>()
                  for found in items do
                      match found with
                      | Some(candidate, rest) -> endingAt[List.last (candidate.Index :: rest)] <- (candidate, rest)
                      | None -> ()
                  // A list's items: its page, where each is and its marker; only a list of items
                  // that each start with one.
                  let listAt k =
                      match resolve (kids.Get k) with
                      // Not one that says what it reads as, all of it: what's moved into it wouldn't be read.
                      | :? PdfDictionary as list when
                          PdfName.L.Equals(standardTag list context)
                          && not ([ PdfName.ActualText; PdfName.Alt ] |> List.exists (fun key -> match list.GetAsString key with NonNull said -> not (String.IsNullOrWhiteSpace(said.ToUnicodeString())) | Null -> false))
                          ->
                          let entries =
                              match resolve (list.Get(PdfName.K)) with
                              | :? PdfArray as array -> [ for j in 0 .. array.Size() - 1 -> resolve (array.Get j) ]
                              | NonNull single -> [ single ]
                              | Null -> []
                          let itemOf (entry: PdfObject | null) =
                              match entry with
                              | :? PdfDictionary as item when PdfName.LI.Equals(standardTag item context) ->
                                  // (Its page its own, or its list's or above.)
                                  let page = structElemPage item context (ancestorPage list context)
                                  let geometry = textGeometry item context page
                                  let page = page |> Option.orElse (geometry |> Option.map _.PageNumber)
                                  let text = Regex.Replace(extractText item context page, @"\s+", " ").Trim()
                                  match page, geometry, markerOf text with
                                  | Some page, Some geometry, Some(marker, _) -> Some(page, geometry, marker)
                                  | _ -> None
                              | _ -> None
                          let listItems = List.map itemOf entries
                          if not listItems.IsEmpty && List.forall Option.isSome listItems then
                              Some(list, List.choose id listItems)
                          else
                              None
                      | _ -> None
                  let claimed = HashSet<int>()
                  for k in 0 .. kids.Size() - 1 do
                      match listAt k with
                      | Some(list, listItems) ->
                          let beside (candidate: Candidate) (page, geometry: StructureGeometry, _) =
                              not (claimed.Contains candidate.Index)
                              && candidate.Page = page
                              && (candidate.Geometry |> Option.exists (fun at -> abs (at.Left - geometry.Left) <= SameIndent))
                          let markers = List.map (fun (candidate: Candidate, _) -> candidate.Marker)
                          let listMarkers = listItems |> List.map (fun (_, _, marker) -> marker)
                          // Where an item's text starts and ends on the page.
                          let top (candidate: Candidate, _) = candidate.Geometry
                          let bottom (candidate: Candidate, rest) =
                              match List.tryLast rest with
                              | Some j -> paragraphAt j |> Option.bind (fun (_, _, geometry, _) -> geometry)
                              | None -> candidate.Geometry
                          // As near the list (and each other) as its items are to each other, or about
                          // a line's height where it has one: not a heading after a blank line.
                          let spacing =
                              let gaps = listItems |> List.pairwise |> List.map (fun ((_, upper: StructureGeometry, _), (_, lower: StructureGeometry, _)) -> upper.Bottom - lower.Top)
                              match gaps with
                              | [] -> let (_, only, _) = listItems.Head in only.Top - only.Bottom
                              | gaps -> List.max gaps
                          let near (upper: StructureGeometry option) (lower: StructureGeometry option) =
                              match upper, lower with
                              | Some upper, Some lower -> upper.Bottom - lower.Top <= 1.5 * spacing + 2.0
                              | _ -> false
                          let (_, firstAt, _) = listItems.Head
                          let (_, lastAt, _) = List.last listItems
                          // Each side taken an item at a time, nearest the list first, as long as its
                          // markers carry the list's numbering on: a wrong one stops it, not the rest.
                          let rec before e found =
                              match endingAt.TryGetValue e with
                              | true, ((candidate, _) as item) when
                                  beside candidate listItems.Head
                                  && near (bottom item) (match found with next :: _ -> top next | [] -> Some firstAt)
                                  && (numberingOf (markers (item :: found) @ listMarkers)).IsSome
                                  ->
                                  before (candidate.Index - 1) (item :: found)
                              | _ -> found
                          let rec after s found =
                              match (if s < items.Length then items[s] else None) with
                              | Some((candidate, rest) as item) when
                                  beside candidate (List.last listItems)
                                  && near (match found with previous :: _ -> bottom previous | [] -> Some lastAt) (top item)
                                  && (numberingOf (listMarkers @ markers (List.rev (item :: found)))).IsSome
                                  ->
                                  after (List.last (candidate.Index :: rest) + 1) (item :: found)
                              | _ -> List.rev found
                          let fits (before, after) =
                              not (List.isEmpty before && List.isEmpty after)
                              && (numberingOf (markers before @ listMarkers @ markers after)).IsSome
                          let before, after = before (k - 1) [], after (k + 1) []
                          match [ before, after; before, []; [], after ] |> List.tryFind fits with
                          | Some(before, after) ->
                              let paragraphs (candidate: Candidate, rest) =
                                  candidate.Paragraph :: [ for j in rest -> (Option.get (paragraphAt j) |> fun (p, _, _, _) -> p) ]
                              let indexes = [ for candidate: Candidate, rest in before @ after do yield candidate.Index; yield! rest ]
                              claimed.UnionWith indexes
                              let first: Candidate = fst (List.head (before @ after))
                              additions.Add
                                  { List = list
                                    Parent = parent
                                    Before = List.map paragraphs before
                                    After = List.map paragraphs after
                                    Indexes = indexes
                                    Path = first.Path
                                    Page = first.Page
                                    Marker = first.MarkerText
                                    ListItems = listItems.Length
                                    Geometry =
                                      [ for candidate: Candidate, rest in before @ after do
                                            yield! Option.toList candidate.Geometry
                                            for j in rest do
                                                match paragraphAt j with
                                                | Some(_, _, Some geometry, _) -> yield geometry
                                                | _ -> () ]
                                      |> unionGeometry }
                          | None -> ()
                      | None -> ()
                  // What's added to a list isn't a run of its own.
                  let candidateAt i = if claimed.Contains i then None else candidateAt i
                  let paragraphAt i = if claimed.Contains i then None else paragraphAt i

                  // Maximal runs of consecutive marked paragraphs on one page, sharing an indent.
                  // Empty paragraphs between items (blank lines, common in exported documents)
                  // don't end a run; they go with the item before them. After a marker that's a
                  // paragraph of its own, the paragraphs right of it are its item's text, up to
                  // the next marker.
                  let runs = List<List<Candidate>>()
                  let mutable blanks: (int * PdfDictionary) list = []
                  // Whether a paragraph at text is more of the text of a marker on its own: its first
                  // on the marker's line and right of it (past its middle: a bullet's slack is most of
                  // it); the rest straight below the one before, at its indent. Not a paragraph beside
                  // that one (a table's next cell), nor one further off (the next column's top).
                  let isItemText (marker: StructureGeometry) (before: StructureGeometry list) (text: StructureGeometry) =
                      match before with
                      | [] ->
                          text.Left >= marker.Right - SameIndent
                          && text.Left > (marker.Left + marker.Right) / 2.0
                          && text.Bottom < marker.Top
                          && text.Top > marker.Bottom
                      | first :: _ ->
                          let previous = List.last before
                          let line = max 12.0 ((marker.Top - marker.Bottom) * 2.5)
                          let below = previous.Bottom - text.Top
                          abs (text.Left - first.Left) <= SameIndent && below >= -2.0 && below <= line
                  // The run's last item, if what's at i would follow straight on from it.
                  let lastItemBefore i =
                      if runs.Count = 0 then None
                      else
                          let run = runs[runs.Count - 1]
                          let last = run[run.Count - 1]
                          if last.Index + last.Trailing.Length + blanks.Length = i - 1 then Some(run, last) else None
                  for i in 0 .. kids.Size() - 1 do
                      match candidateAt i with
                      | Some candidate ->
                          let continues =
                              match lastItemBefore i with
                              | Some(_, last) ->
                                  last.Page = candidate.Page
                                  && (not last.MarkerOnly || not last.TextGeometry.IsEmpty)
                                  && (match last.Geometry, candidate.Geometry with
                                      | Some a, Some b -> abs (a.Left - b.Left) <= SameIndent
                                      | _ -> true)
                              | None -> false
                          match lastItemBefore i with
                          | Some(run, last) when continues ->
                              run[run.Count - 1] <- { last with Trailing = last.Trailing @ List.rev blanks }
                              run.Add candidate
                          | _ -> runs.Add(List [ candidate ])
                          blanks <- []
                      | None ->
                          match paragraphAt i, lastItemBefore i with
                          | Some(paragraph, page, geometry, text), Some(run, last) when
                              text <> ""
                              && last.MarkerOnly
                              && page = Some last.Page
                              && (match last.Geometry, geometry with
                                  | Some marker, Some text -> isItemText marker last.TextGeometry text
                                  | _ -> false)
                              ->
                              run[run.Count - 1] <-
                                  { last with
                                      Trailing = last.Trailing @ List.rev blanks @ [ i, paragraph ]
                                      TextGeometry = last.TextGeometry @ Option.toList geometry }
                              blanks <- []
                          | Some(paragraph, page, _, _), _ when isSubtreeEmpty paragraph context page -> blanks <- (i, paragraph) :: blanks
                          // Anything else ends the run: the arithmetic above won't line up.
                          | _ -> blanks <- []

                  for run in runs do
                      // A marker on its own with no text after it isn't an item: at the end (a stray
                      // bullet) it's left out; anywhere else the run isn't a list.
                      let isItem (item: Candidate) = not item.MarkerOnly || not item.TextGeometry.IsEmpty
                      let run = run |> List.ofSeq |> List.rev |> List.skipWhile (isItem >> not) |> List.rev
                      let items = run |> List.filter isItem
                      if items.Length >= 2 && items.Length = run.Length then
                          match numberingOf (items |> List.map _.Marker) with
                          | Some numbering ->
                              yield
                                  { Parent = parent
                                    Indexes = items |> List.collect (fun item -> item.Index :: List.map fst item.Trailing)
                                    Items =
                                        items
                                        |> List.map (fun item ->
                                            if item.MarkerOnly then { Label = Some item.Paragraph; Body = List.map snd item.Trailing }
                                            else { Label = None; Body = item.Paragraph :: List.map snd item.Trailing })
                                    Path = items.Head.Path
                                    Page = items.Head.Page
                                    FirstMarker = items.Head.MarkerText
                                    LastMarker = (List.last items).MarkerText
                                    Numbering = numbering
                                    Geometry = items |> List.collect (fun item -> Option.toList item.Geometry @ item.TextGeometry) |> unionGeometry }
                          | None -> ()
              | _ -> () ]
        { Runs = runs; Additions = List.ofSeq additions }

// ---- Tagging a run as a list ----

let private structElement (document: PdfDocument) (tag: PdfName) (parent: PdfDictionary) =
    let element = PdfDictionary()
    element.Put(PdfName.Type, PdfName.StructElem) |> ignore
    element.Put(PdfName.S, tag) |> ignore
    element.Put(PdfName.P, parent) |> ignore
    element.MakeIndirect document |> ignore
    element

/// Wraps the run's paragraphs in L > LI > LBody, in place of the paragraphs in their parent
/// (an item's trailing empty paragraphs go in its LBody too). A paragraph that's only its item's
/// marker becomes the item's Lbl instead: retagged, with its content as it is. Each paragraph
/// keeps its own content (and parent-tree entries); only its parent changes.
let tagAsList (document: PdfDocument) (run: ListRun) =
    let list = structElement document PdfName.L run.Parent
    let attributes = PdfDictionary()
    attributes.Put(PdfName.O, PdfName.List) |> ignore
    attributes.Put(PdfName.ListNumbering, run.Numbering) |> ignore
    list.Put(PdfName.A, attributes) |> ignore

    let items = PdfArray()
    for entry in run.Items do
        let item = structElement document PdfName.LI list
        let parts = PdfArray()
        match entry.Label with
        | Some label ->
            label.Put(PdfName.S, PdfName.Lbl) |> ignore
            // Lbl is in the default namespace, as the elements made here are.
            label.Remove PdfName.NS |> ignore
            label.Put(PdfName.P, item) |> ignore
            parts.Add label
        | None -> ()
        let body = structElement document PdfName.LBody item
        let contents = PdfArray()
        for paragraph in entry.Body do
            contents.Add paragraph
            paragraph.Put(PdfName.P, body) |> ignore
        body.Put(PdfName.K, contents) |> ignore
        if parts.Size() = 0 then item.Put(PdfName.K, body) |> ignore
        else
            parts.Add body
            item.Put(PdfName.K, parts) |> ignore
        items.Add item
    list.Put(PdfName.K, items) |> ignore

    let kids = run.Parent.GetAsArray(PdfName.K) |> nonNull
    for index in List.rev (List.tail run.Indexes) do
        kids.Remove index
    kids.Set(List.head run.Indexes, list) |> ignore

/// Adds each of the paragraphs' items to the list, before its first item or after its last, as
/// LI > LBody > the paragraphs, taking them out of their parent. Each paragraph keeps its own
/// content (and parent-tree entries); only its parent changes, and the language it's read in
/// doesn't (where the list's would be another, it's given its own).
let addToList (document: PdfDocument) context (addition: ListAddition) =
    let list = addition.List
    let item (paragraphs: PdfDictionary list) =
        let item = structElement document PdfName.LI list
        let body = structElement document PdfName.LBody item
        let contents = PdfArray()
        for paragraph in paragraphs do
            let language = languageOf paragraph context
            contents.Add paragraph
            paragraph.Put(PdfName.P, body) |> ignore
            if languageOf paragraph context <> language then
                // (An empty /Lang says its language isn't known, as it wasn't.)
                paragraph.Put(PdfName.Lang, PdfString(defaultArg language "")) |> ignore
        body.Put(PdfName.K, contents) |> ignore
        item.Put(PdfName.K, body) |> ignore
        item
    let items =
        match list.Get(PdfName.K) with
        | :? PdfArray as items -> items
        | single ->
            let items = PdfArray()
            if not (isNull single) then items.Add single
            list.Put(PdfName.K, items) |> ignore
            items
    addition.Before |> List.iteri (fun i paragraphs -> items.Add(i, item paragraphs))
    for paragraphs in addition.After do
        items.Add(item paragraphs)
    let kids = addition.Parent.GetAsArray(PdfName.K) |> nonNull
    for index in List.sortDescending addition.Indexes do
        kids.Remove index
