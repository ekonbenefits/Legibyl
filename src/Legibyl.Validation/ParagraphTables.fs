/// Finds runs of sibling paragraphs whose text geometry forms repeated rows and columns.
/// This is a low-cost review heuristic for tables tagged as paragraphs; it is not a table
/// recognizer and deliberately makes no automatic structure changes.
module internal Legibyl.Validation.ParagraphTables

open System
open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

/// A likely table represented by paragraphs.
type Finding =
    { Path: string
      Page: int
      ParagraphCount: int
      Geometry: StructureGeometry option }

type private Paragraph =
    { Index: int
      Path: string
      Page: int
      Box: StructureGeometry
      Fragments: StructureGeometry list }

type private Sibling =
    { Index: int
      Page: int option
      Paragraph: Paragraph option }

let private siblingPosition (path: string) =
    let opening = path.LastIndexOf '['
    let closing = path.LastIndexOf ']'
    if opening < 0 || closing <= opening then
        None
    else
        match Int32.TryParse(path.Substring(opening + 1, closing - opening - 1)) with
        | true, index -> Some(path.Substring(0, opening), index)
        | _ -> None

let rec private textFragments (context: Context) (page: int option) (obj: PdfObject | null) =
    match resolve obj with
    | :? PdfArray as array ->
        [ for i in 0 .. array.Size() - 1 do yield! textFragments context page (array.Get i) ]
    | :? PdfDictionary as dictionary when PdfName.Span.Equals(standardTag dictionary context) ->
        if String.IsNullOrWhiteSpace(extractText dictionary context page) then
            []
        else
            textGeometry dictionary context page |> Option.toList
    | :? PdfDictionary as dictionary ->
        textFragments context page (dictionary.Get(PdfName.K))
    | _ -> []

/// The boxes a row's text makes, its paragraphs' pieces of text joined where they're no more
/// than a word's gap apart: a sentence tagged as several paragraphs along a line is one box,
/// not a row of cells.
let private joinedFragments (paragraphs: Paragraph list) =
    let merge (left: StructureGeometry) (right: StructureGeometry) : StructureGeometry =
        { left with
            Left = min left.Left right.Left
            Bottom = min left.Bottom right.Bottom
            Right = max left.Right right.Right
            Top = max left.Top right.Top }
    let rec group (current: StructureGeometry option) (remaining: StructureGeometry list) =
        match current, remaining with
        | None, [] -> []
        | Some box, [] -> [ box ]
        | None, next :: rest -> group (Some next) rest
        | Some box, next :: rest when next.Left - box.Right <= 8.0 -> group (Some(merge box next)) rest
        | Some box, next :: rest -> box :: group (Some next) rest
    paragraphs
    |> List.collect (fun paragraph -> if paragraph.Fragments.IsEmpty then [ paragraph.Box ] else paragraph.Fragments)
    |> List.sortBy _.Left
    |> group None

let private rows (paragraphs: Paragraph list) : (int * StructureGeometry * Paragraph list) list =
    let overlap (a: StructureGeometry) (b: StructureGeometry) =
        max 0.0 (min a.Top b.Top - max a.Bottom b.Bottom)
    let addToRows (found: (int * StructureGeometry * Paragraph list) list) (paragraph: Paragraph) =
        let height (box: StructureGeometry) = max 0.1 (box.Top - box.Bottom)
        let nextIndex = found |> List.map (fun (index, _, _) -> index) |> List.fold max -1 |> (+) 1
        let rec insert before = function
            | [] -> List.rev ((nextIndex, paragraph.Box, [ paragraph ]) :: before)
            | (index, box, cells) :: rest ->
                let shared = overlap box paragraph.Box
                if shared >= 0.35 * min (height box) (height paragraph.Box) then
                    List.rev before @ ((index, ({ box with Top = max box.Top paragraph.Box.Top; Bottom = min box.Bottom paragraph.Box.Bottom }: StructureGeometry), paragraph :: cells) :: rest)
                else
                    insert ((index, box, cells) :: before) rest
        insert [] found
    paragraphs
    |> List.sortByDescending (fun paragraph -> paragraph.Box.Top)
    |> List.fold addToRows []
    |> List.sortBy (fun (_, box, _) -> -box.Top)

type private Alignment =
    | Left
    | Right
    | Centre

let private anchor alignment (box: StructureGeometry) =
    match alignment with
    | Left -> box.Left
    | Right -> box.Right
    | Centre -> (box.Left + box.Right) / 2.0

/// At least two columns recur in at least two of the same rows. A column is where cells line
/// up: their left edges, right edges or centres (a left-aligned label column beside a
/// right-aligned value column, say). Two columns are separate when no cell's box is in both, so
/// one cell's left edge and right edge don't count as two columns; columns lined up the same way
/// are also at least minimumColumnGap apart.
let private hasRepeatedColumns (rows: (int * StructureGeometry * Paragraph list) list) =
    let tolerance = 6.0
    let minimumColumnGap = 12.0
    // Each cell box, by its row and its place among the row's boxes.
    let boxes =
        [ for row, _, cells in rows do
              for i, box in joinedFragments cells |> List.indexed -> (row, i), box ]
    let columns alignment =
        let entries = boxes |> List.map (fun (cell, box) -> anchor alignment box, cell) |> List.sortBy fst
        let rec group (current: (float * (int * int)) list) (entries: (float * (int * int)) list) =
            match entries with
            | [] -> if current.IsEmpty then [] else [ current ]
            | ((position, _) as entry) :: rest ->
                match current with
                | (last, _) :: _ when position - last <= tolerance -> group (entry :: current) rest
                | _ when current.IsEmpty -> group [ entry ] rest
                | _ -> current :: group [ entry ] rest
        entries
        |> group []
        |> List.map (fun cluster ->
            let positions = cluster |> List.map fst
            let cells = cluster |> List.map snd |> Set.ofList
            alignment, (List.min positions + List.max positions) / 2.0, cells)
        |> List.filter (fun (_, _, cells) -> (cells |> Set.map fst).Count >= 2)
    let all = [ Left; Right; Centre ] |> List.collect columns
    let rowsOf (cells: Set<int * int>) = cells |> Set.map fst
    all
    |> List.indexed
    |> List.exists (fun (index, (alignment, position, cells)) ->
        all
        |> List.skip (index + 1)
        |> List.exists (fun (otherAlignment, otherPosition, otherCells) ->
            Set.isEmpty (Set.intersect cells otherCells)
            && (alignment <> otherAlignment || abs (otherPosition - position) >= minimumColumnGap)
            && (Set.intersect (rowsOf cells) (rowsOf otherCells)).Count >= 2))

let private tableRows (paragraphs: Paragraph list) =
    let rows =
        rows paragraphs
        |> List.filter (fun (_, _, cells) -> (joinedFragments cells).Length >= 2)
    if rows.Length >= 2 && hasRepeatedColumns rows then Some rows else None

/// Finds maximal consecutive runs of same-page sibling paragraphs that resemble a table grid.
/// Paragraphs in listed (a list's items, as ParagraphLists finds them) aren't counted: a column
/// of bullets beside their items' text lines up as rows and columns too.
let find (context: Context) (visits: StructElemVisit list) (listed: Collections.Generic.IReadOnlySet<PdfDictionary>) : Finding list =
    let siblings =
        visits
        |> List.choose (fun visit ->
            siblingPosition visit.Path
            |> Option.map (fun (parent, index) ->
                let geometry = textGeometry visit.Element context visit.Page
                let page = visit.Page |> Option.orElse (geometry |> Option.map _.PageNumber)
                let paragraph =
                    if isParagraph visit.Element context && not (listed.Contains visit.Element) then
                        match page, geometry with
                        | Some page, Some box ->
                            if String.IsNullOrWhiteSpace(extractText visit.Element context (Some page)) then None
                            else
                                Some
                                    { Index = index
                                      Path = visit.Path
                                      Page = page
                                      Box = box
                                      Fragments = textFragments context (Some page) (visit.Element.Get(PdfName.K)) }
                        | _ -> None
                    else None
                parent, { Index = index; Page = page; Paragraph = paragraph }))
        |> List.groupBy fst
        |> List.collect (fun (_, entries) ->
            entries
            |> List.map snd
            |> List.sortBy _.Index
            |> List.fold
                (fun ((runs: Paragraph list list), (current: Paragraph list)) (sibling: Sibling) ->
                    match sibling.Paragraph with
                    | Some paragraph when
                        current.IsEmpty
                        || (sibling.Index = current.Head.Index + 1 && sibling.Page = Some current.Head.Page) ->
                        runs, paragraph :: current
                    | Some paragraph -> List.rev current :: runs, [ paragraph ]
                    | None when not current.IsEmpty -> List.rev current :: runs, []
                    | None -> runs, [])
                ([], [])
            |> fun (runs, current) -> List.rev current :: runs
            |> List.filter (not << List.isEmpty))
    [ for run in siblings do
          if run.Length >= 4 then
              match tableRows run with
              | Some rows ->
                  yield
                      { Path = run.Head.Path
                        Page = run.Head.Page
                        ParagraphCount = rows |> List.sumBy (fun (_, _, cells) -> cells.Length)
                        Geometry = rows |> Seq.map (fun (_, box, _) -> box) |> unionGeometry }
              | None -> () ]
