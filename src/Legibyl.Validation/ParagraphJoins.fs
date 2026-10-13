/// Joins paragraphs that were tagged line by line: neighbouring /P elements on one page that the
/// split-paragraph warning flags, where a layout detector (see Legibyl.Layout) confirms their
/// text is in the same block. The first
/// paragraph of each run takes the others' content, in order, and the others are removed; what
/// the page draws doesn't change.
module internal Legibyl.Validation.ParagraphJoins

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Tagging
open Legibyl.Validation.Interop
open Legibyl.Validation.StructureTreeObjects

/// Paragraphs to join: siblings in parent's /K at these positions, first to last.
type JoinRun =
    { Parent: PdfDictionary
      Indexes: int list
      Page: int }

/// Regions that hold running text, whose lines belong together.
let private isTextRegion (region: LayoutRegion) =
    not (List.contains region.Tag [ Tags.Figure; Tags.Table; Tags.Artifact; Tags.Formula; Tags.Lbl ])

/// How much of a paragraph's text must lie in a region for it to count as in it.
[<Literal>]
let private Inside = 0.7

/// How much of box lies in region, 0 to 1 (box has area; see regionOf).
let private insideFraction (box: StructureGeometry) (region: StructureGeometry) =
    let width = min box.Right region.Right - max box.Left region.Left
    let height = min box.Top region.Top - max box.Bottom region.Bottom
    let area = (box.Right - box.Left) * (box.Top - box.Bottom)
    if width <= 0.0 || height <= 0.0 then 0.0 else width * height / area

/// The text region most of geometry lies in, by its position in regions.
let private regionOf (regions: IReadOnlyList<LayoutRegion>) (geometry: StructureGeometry) =
    let area = (geometry.Right - geometry.Left) * (geometry.Top - geometry.Bottom)
    let x, y = (geometry.Left + geometry.Right) / 2.0, (geometry.Bottom + geometry.Top) / 2.0
    let share (region: LayoutRegion) =
        let r = region.Geometry
        if area > 0.0 then insideFraction geometry r
        elif x >= r.Left && x <= r.Right && y >= r.Bottom && y <= r.Top then 1.0
        else 0.0
    [ for i in 0 .. regions.Count - 1 do
          if isTextRegion regions[i] then
              let share = share regions[i]
              if share >= Inside then yield i, share ]
    |> List.sortByDescending snd
    |> List.tryHead
    |> Option.map fst

// ---- Which paragraphs look split ----

let normalizeText (text: string) = Regex.Replace(text, @"\s+", " ").Trim()

/// The language pack whose words read a paragraph in language: English where its language isn't
/// known (none where it has no pack, which gives fewer warnings, never wrong ones).
let private packFor (language: string option) = LanguagePacks.forTag (defaultArg language "en")

let private isLikelyStandaloneParagraph (language: string option) (text: string) =
    // Its pack's words, else English's: one less guard would mean more warnings, not fewer.
    let headings =
        packFor language |> Option.orElseWith (fun () -> packFor None) |> Option.map _.StandaloneHeadings |> Option.defaultValue []
    text.Length <= 3
    || headings |> List.exists (fun word -> Regex.IsMatch(text, $@"^{Regex.Escape word}\b", RegexOptions.IgnoreCase))
    || Regex.IsMatch(text, @"^\d+(\.\d+)*\s+\S+")
    || Regex.IsMatch(text, @"^[a-z]\.\s+\S+", RegexOptions.IgnoreCase)

/// Whether next reads as the rest of current's sentence, each read in its language (see
/// StructureTreeObjects.languageOf): the split-paragraph warning (Structure-2), and the pairs the
/// layout is asked to confirm.
let looksSplit (currentLanguage: string option, currentText: string) (nextLanguage: string option, nextText: string) =
    let continuations = packFor nextLanguage |> Option.map _.SentenceContinuations |> Option.defaultValue []

    currentText.Length >= 20
    && nextText.Length >= 10
    && not (isLikelyStandaloneParagraph currentLanguage currentText || isLikelyStandaloneParagraph nextLanguage nextText)
    && not (".:;!?…".Contains currentText[currentText.Length - 1])
    && (Char.IsLower nextText[0]
        || continuations |> List.exists (fun word -> nextText.StartsWith(word + " ", StringComparison.OrdinalIgnoreCase)))

/// How far apart, in points, two pieces of text on one line can be and still read as one run of
/// words (a space, not a column's gap).
[<Literal>]
let private WordGap = 8.0

/// How near, in points, a line's start must be to the left edge of the text around it.
[<Literal>]
let private SameEdge = 3.0

/// Whether the text starting at nextStart picks up on the line where the text ending at
/// previousEnd stops, just after it: a sentence tagged in pieces along its line (a date, number
/// or name dropped into it, each its own paragraph). Pieces of a line are about as tall as each
/// other; one drawn over several lines is taller, and its edges aren't where its line ends or
/// starts, so it isn't taken for one.
let private continuesLine (previousEnd: StructureGeometry) (nextStart: StructureGeometry) =
    let previousHeight = previousEnd.Top - previousEnd.Bottom
    let nextHeight = nextStart.Top - nextStart.Bottom
    let shorter, taller = min previousHeight nextHeight, max previousHeight nextHeight
    let overlap = min previousEnd.Top nextStart.Top - max previousEnd.Bottom nextStart.Bottom
    let gap = nextStart.Left - previousEnd.Right
    previousEnd.PageNumber = nextStart.PageNumber
    && shorter > 0.0
    && taller <= 2.0 * shorter
    && overlap >= 0.5 * shorter
    && gap >= -1.0
    && gap <= WordGap

/// Whether the text starting at nextStart is the next line of the text ending at previousEnd,
/// wrapped there: what's left of the line after previousEnd (to block's right edge, block being
/// the text around them) is narrower than nextWord, next's first word, needs, and nextStart
/// starts the line just below at block's left edge, no further below than lines of text at size
/// are spaced (paragraphs are often spaced further apart). Were there room, the line was broken
/// on purpose (a heading over its text, say).
let private wrapsLine (block: StructureGeometry) (size: float) (previousEnd: StructureGeometry) (nextWord: float) (nextStart: StructureGeometry) =
    let below = previousEnd.Bottom - nextStart.Top
    previousEnd.PageNumber = nextStart.PageNumber
    && below >= -0.5 * size
    && below <= 0.6 * size
    && block.Right - previousEnd.Right < nextWord
    && abs (nextStart.Left - block.Left) <= SameEdge

/// Whether next's text carries on previous's sentence where the page draws it: on the same line,
/// just after it (see continuesLine), or wrapped onto the next line (see wrapsLine) at the same
/// size where previous doesn't end a sentence (not a heading over its text). Not where previous
/// is a list item's marker drawn beside its text, nor where next starts with a marker of its own.
let continuesTextOf context (previous: PdfDictionary, previousPage: int option) (next: PdfDictionary, nextPage: int option) =
    let pieceGeometry (piece: MarkedContentTextIndex.MarkedContentInfo option) = piece |> Option.bind _.Geometry
    let last = List.tryLast (textPieces previous context previousPage)
    let first = List.tryHead (textPieces next context nextPage)
    match pieceGeometry last, pieceGeometry first with
    | Some previousEnd, Some nextStart ->
        let previousText = normalizeText (extractText previous context previousPage)
        let nextText = normalizeText (extractText next context nextPage)
        // A sentence ends at its full stop, even inside closing quotes or brackets.
        let endsSentence =
            let text = previousText.TrimEnd('”', '’', '"', '\'', ')', ']', '»')
            text <> "" && ".:;!?…".Contains text[text.Length - 1]
        // The size both are drawn at, if it's the same (within a tenth).
        let size =
            match last |> Option.bind _.FontSize, first |> Option.bind _.FontSize with
            | Some a, Some b when abs (a - b) <= 0.1 * max a b -> Some(max a b)
            | _ -> None
        let wraps size =
            // The room next's first word needs, at the previous piece's average width a character
            // (a piece drawn over several lines gives less, so a wrap is found less often, not more).
            let nextWord =
                let characters = (normalizeText (last |> Option.map _.Text |> Option.defaultValue "")).Length
                let word = nextText.Split(' ')[0]
                if characters = 0 then 0.0 else float word.Length * (previousEnd.Right - previousEnd.Left) / float characters
            not endsSentence
            && (match resolve (previous.Get PdfName.P) with
                | :? PdfDictionary as parent ->
                    textPieces parent context previousPage
                    |> List.choose _.Geometry
                    |> List.filter (fun geometry -> geometry.PageNumber = previousEnd.PageNumber)
                    |> unionGeometry
                    |> Option.exists (fun block -> wrapsLine block size previousEnd nextWord nextStart)
                | _ -> false)
        previousText <> ""
        && nextText <> ""
        && not (ParagraphLists.isMarkerOnly previousText)
        && not (ParagraphLists.startsWithMarker nextText)
        && (continuesLine previousEnd nextStart || size |> Option.exists wraps)
    | _ -> false

/// A paragraph that can take part in a join, with its page and where its text is drawn.
type private Piece =
    { Paragraph: PdfDictionary
      Page: int
      Geometry: StructureGeometry
      Text: string
      Language: string option }

/// Entries that make a paragraph more than its content: joining it would lose or misapply them.
let private standsAlone (paragraph: PdfDictionary) =
    [ PdfName.ActualText; PdfName.Alt; PdfName.E; PdfName.ID ] |> List.exists paragraph.ContainsKey

let private pieceOf context (obj: PdfObject | null) =
    match resolve obj with
    | :? PdfDictionary as paragraph when isParagraph paragraph context && not (standsAlone paragraph) ->
        match ancestorPage paragraph context with
        | Some page when not (isSubtreeEmpty paragraph context (Some page)) ->
            textGeometry paragraph context (Some page)
            |> Option.map (fun geometry ->
                { Paragraph = paragraph
                  Page = page
                  Geometry = geometry
                  Text = normalizeText (extractText paragraph context (Some page))
                  Language = languageOf paragraph context })
        | _ -> None
    | _ -> None

/// Whether next can be joined onto previous if the layout agrees: the split-paragraph warning
/// flags them (so only what it reported is joined), and they're on the same page, in the same
/// language, and next isn't a list item of its own.
let private couldFollow context (previous: Piece) (next: Piece) =
    previous.Page = next.Page
    && (looksSplit (previous.Language, previous.Text) (next.Language, next.Text)
        || continuesTextOf context (previous.Paragraph, Some previous.Page) (next.Paragraph, Some next.Page))
    // In the same language: their own, or the one around them.
    && (previous.Language |> Option.map _.ToLowerInvariant()) = (next.Language |> Option.map _.ToLowerInvariant())
    && not (ParagraphLists.startsWithMarker next.Text)

/// Every run of two or more sibling paragraphs the split-paragraph warning flags whose text the
/// layout puts in one text region. regions gives a page's regions; it's only asked for pages
/// with flagged paragraphs.
let find (document: PdfDocument) context (regions: int -> IReadOnlyList<LayoutRegion>) : JoinRun list =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> []
    | structTreeRoot ->
        let pageRegions = Dictionary<int, IReadOnlyList<LayoutRegion>>()
        let regionsOn page =
            match pageRegions.TryGetValue page with
            | true, found -> found
            | _ ->
                let found = regions page
                pageRegions[page] <- found
                found
        // Only elements can hold paragraphs to join (not the StructTreeRoot itself: a paragraph
        // belongs in a Document or other element).
        [ for visit in walk structTreeRoot context do
              match resolve (visit.Element.Get(PdfName.K)) with
              | :? PdfArray as kids when kids.Size() > 1 ->
                  let pieces = Array.init (kids.Size()) (fun i -> pieceOf context (kids.Get i))
                  let joinsPrevious i =
                      match pieces[i - 1], pieces[i] with
                      | Some previous, Some next when couldFollow context previous next ->
                          let regions = regionsOn next.Page
                          match regionOf regions previous.Geometry, regionOf regions next.Geometry with
                          | Some a, Some b -> a = b
                          | _ -> false
                      | _ -> false
                  let mutable run = [ 0 ]
                  for i in 1 .. pieces.Length do
                      if i < pieces.Length && joinsPrevious i then
                          run <- i :: run
                      else
                          if run.Length > 1 then
                              yield { Parent = visit.Element; Indexes = List.rev run; Page = pieces[run.Head].Value.Page }
                          run <- [ i ]
              | _ -> () ]

// ---- Joining ----

/// The page an element's integer MCIDs are on: its own /Pg, or its nearest ancestor's.
let rec private pageObject (element: PdfDictionary | null) depth : PdfDictionary | null =
    match element with
    | null -> null
    | _ when depth > 200 -> null
    | element ->
        match element.GetAsDictionary(PdfName.Pg) with
        | null -> pageObject (element.GetAsDictionary(PdfName.P)) (depth + 1)
        | page -> page

let private samePage (a: PdfDictionary | null) (b: PdfDictionary | null) =
    match a, b with
    | NonNull a, NonNull b -> Object.ReferenceEquals(a, b)
    | _ -> false

/// Moves from's content to the end of target's, re-pointing the parent tree (which says which
/// element each piece of marked content and each annotation belongs to) at target.
let private moveContent (target: PdfStructElem) (from: PdfStructElem) =
    let targetPage = pageObject (target.GetPdfObject()) 0
    let fromPage = pageObject (from.GetPdfObject()) 0
    let kids = [ for _ in 1 .. from.GetKids().Count -> from.RemoveKid 0 ]
    for kid in kids do
        match kid with
        | :? PdfStructElem as element -> target.AddKid element |> ignore
        | :? PdfObjRef as reference -> target.AddKid(PdfObjRef(reference.GetPdfObject() :?> PdfDictionary, target)) |> ignore
        | :? PdfMcrNumber as mcid when isNull fromPage || samePage targetPage fromPage ->
            target.AddKid(PdfMcrNumber(mcid.GetPdfObject() :?> PdfNumber, target)) |> ignore
        | :? PdfMcrNumber as mcid ->
            // On target it would take target's page; name its own.
            let reference = PdfDictionary()
            reference.Put(PdfName.Type, PdfName.MCR) |> ignore
            reference.Put(PdfName.MCID, mcid.GetPdfObject()) |> ignore
            reference.Put(PdfName.Pg, fromPage) |> ignore
            target.AddKid(PdfMcrDictionary(reference, target)) |> ignore
        | :? PdfMcrDictionary as reference ->
            let dictionary = reference.GetPdfObject() :?> PdfDictionary
            if not (dictionary.ContainsKey PdfName.Pg) && not (isNull fromPage) && not (samePage targetPage fromPage) then
                dictionary.Put(PdfName.Pg, fromPage) |> ignore
            target.AddKid(PdfMcrDictionary(dictionary, target)) |> ignore
        | _ -> ()

/// Joins each run's paragraphs into its first. Returns how many paragraphs were joined away.
let join (runs: JoinRun list) =
    let mutable joined = 0
    // Later runs in a parent first: removing paragraphs shortens its /K, which would move the
    // ones after them.
    for run in runs |> List.sortByDescending (fun run -> run.Indexes.Head) do
        let parent = PdfStructElem run.Parent
        let kids = run.Parent.GetAsArray(PdfName.K) |> nonNull
        let paragraph index =
            match resolve (kids.Get index) with
            | :? PdfDictionary as dictionary -> PdfStructElem dictionary
            | _ -> invalidOp "A paragraph to join isn't a dictionary."
        let target = paragraph run.Indexes.Head
        for index in run.Indexes.Tail do
            moveContent target (paragraph index)
        for index in List.rev run.Indexes.Tail do
            parent.RemoveKid index |> ignore
            joined <- joined + 1
    joined
