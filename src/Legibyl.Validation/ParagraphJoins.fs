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
    not (List.contains region.Tag [ "Figure"; "Table"; "Artifact"; "Formula"; "Lbl" ])

/// How much of a paragraph's text must lie in a region for it to count as in it.
[<Literal>]
let private Inside = 0.7

let private overlap (a: StructureGeometry) (b: StructureGeometry) =
    let width = min a.Right b.Right - max a.Left b.Left
    let height = min a.Top b.Top - max a.Bottom b.Bottom
    if width <= 0.0 || height <= 0.0 then 0.0 else width * height

/// The text region most of geometry lies in, by its position in regions.
let private regionOf (regions: IReadOnlyList<LayoutRegion>) (geometry: StructureGeometry) =
    let area = (geometry.Right - geometry.Left) * (geometry.Top - geometry.Bottom)
    let x, y = (geometry.Left + geometry.Right) / 2.0, (geometry.Bottom + geometry.Top) / 2.0
    let share (region: LayoutRegion) =
        let r = region.Geometry
        if area > 0.0 then overlap geometry r / area
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

let private isLikelyStandaloneParagraph (text: string) =
    text.Length <= 3
    || Regex.IsMatch(text, @"^(SECTION|ARTICLE)\b", RegexOptions.IgnoreCase)
    || Regex.IsMatch(text, @"^\d+(\.\d+)*\s+\S+")
    || Regex.IsMatch(text, @"^[a-z]\.\s+\S+", RegexOptions.IgnoreCase)

/// Whether next reads as the rest of current's sentence: the split-paragraph warning
/// (Structure-2), and the pairs the layout is asked to confirm.
let looksSplit (currentText: string) (nextText: string) =
    let startsWith (prefix: string) = nextText.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)

    currentText.Length >= 20
    && nextText.Length >= 10
    && not (isLikelyStandaloneParagraph currentText || isLikelyStandaloneParagraph nextText)
    && not (".:;!?…".Contains currentText[currentText.Length - 1])
    && (Char.IsLower nextText[0]
        || startsWith "and "
        || startsWith "or "
        || startsWith "to "
        || startsWith "of "
        || startsWith "the ")

/// A paragraph that can take part in a join, with its page and where its text is drawn.
type private Piece =
    { Paragraph: PdfDictionary
      Page: int
      Geometry: StructureGeometry
      Text: string }

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
                  Text = normalizeText (extractText paragraph context (Some page)) })
        | _ -> None
    | _ -> None

/// Whether next can be joined onto previous if the layout agrees: the split-paragraph warning
/// flags them (so only what it reported is joined), and they're on the same page, in the same
/// language, and next isn't a list item of its own.
let private couldFollow (previous: Piece) (next: Piece) =
    previous.Page = next.Page
    && looksSplit previous.Text next.Text
    && Object.Equals(previous.Paragraph.Get(PdfName.Lang), next.Paragraph.Get(PdfName.Lang))
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
                      | Some previous, Some next when couldFollow previous next ->
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
        let paragraph index = PdfStructElem(nonNull (resolve (kids.Get index)) :?> PdfDictionary)
        let target = paragraph run.Indexes.Head
        for index in run.Indexes.Tail do
            moveContent target (paragraph index)
        for index in List.rev run.Indexes.Tail do
            parent.RemoveKid index |> ignore
            joined <- joined + 1
    joined
