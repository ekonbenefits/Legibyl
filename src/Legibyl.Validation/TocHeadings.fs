/// Recovers heading levels from a table of contents, for documents that list their sections
/// in a linked TOC but tag every heading in the body as a plain paragraph. Each TOC entry
/// gives a heading's text, the page it's on (its link's destination), and its level (its
/// indent). A paragraph is only taken to be that heading when two signals agree: its text is
/// the entry's text, and it's on the entry's destination page. LegibylChecks reports what
/// this finds; the HeadingsFromToc fix applies it.
module internal Legibyl.Validation.TocHeadings

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open iText.Kernel.Pdf
open Legibyl.Validation.Interop
open Legibyl.Validation.StructureTreeObjects

/// One table-of-contents entry that has text, a destination page, and an indent.
type Entry =
    { Path: string
      Label: string
      Page: int
      Left: float
      Geometry: StructureGeometry option
      /// The entry's link annotations that go to Page.
      Links: PdfDictionary list }

/// A paragraph the table of contents lists as a heading of this level.
type Heading =
    { Entry: Entry
      Paragraph: PdfDictionary
      ParagraphPath: string
      Level: int
      Geometry: StructureGeometry option }

/// An entry that matched no paragraph on its page, or more than one.
type Unmatched = { Entry: Entry; Matches: int }

type Analysis =
    /// No table of contents, the document already tags headings, or nothing in the body
    /// matches the table of contents: this isn't a document to recover headings for.
    | NotApplicable
    | Recoverable of headings: Heading list * unmatched: Unmatched list
    /// Entries match paragraphs, but their levels can't be recovered safely.
    | Unrecoverable of tocPath: string * tocGeometry: StructureGeometry option * reason: string * unmatched: Unmatched list

// Indents this close are the same level...
[<Literal>]
let private SameLevel = 1.5

// ...a level spread wider than this is a continuum, not a level...
[<Literal>]
let private MaxLevelSpread = 3.0

// ...and levels must be at least this far apart.
[<Literal>]
let private MinLevelGap = 6.0

/// Groups indents into levels, 1 being the least indented: clusters of nearly equal indents,
/// clearly apart, at most six. Error (with the reason) if they don't fall into such levels.
let levelsOf (indents: float list) : Result<float -> int, string> =
    let groups = List<List<float>>()
    for indent in List.sort indents do
        if groups.Count > 0 && indent - groups[groups.Count - 1][groups[groups.Count - 1].Count - 1] <= SameLevel then
            groups[groups.Count - 1].Add indent
        else
            groups.Add(List [ indent ])
    let ranges = [ for group in groups -> group[0], group[group.Count - 1] ]
    let spreadOut = ranges |> List.exists (fun (low, high) -> high - low > MaxLevelSpread)
    let crowded = ranges |> List.pairwise |> List.exists (fun ((_, high), (low, _)) -> low - high < MinLevelGap)
    if spreadOut || crowded then
        Error "its entries' indents vary gradually instead of falling into clear levels"
    elif ranges.Length > 6 then
        Error "its entries fall into more than six indent levels"
    else
        Ok(fun indent -> 1 + (ranges |> List.findIndex (fun (low, high) -> indent >= low && indent <= high)))

let private normalize (text: string) = Regex.Replace(text, @"\s+", " ").Trim()

let private isHeadingTag (tag: PdfName | null) =
    match tag with
    | null -> false
    | tag -> Regex.IsMatch(tag.GetValue(), "^H[1-6]?$")

/// A TOCI's own parts: everything but its nested sub-entries.
let private ownParts (toci: PdfDictionary) context : PdfObject list =
    let kids =
        match resolve (toci.Get(PdfName.K)) with
        | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 -> array.Get i ]
        | null -> []
        | kid -> [ kid ]
    kids
    |> List.filter (fun kid ->
        match resolve kid with
        | :? PdfDictionary as dictionary -> not (PdfName.TOCI.Equals(standardTag dictionary context))
        | null -> false
        | _ -> true)

/// The /Link elements within obj, not looking inside nested TOC entries.
let rec private linksIn (obj: PdfObject | null) context : PdfDictionary list =
    match resolve obj with
    | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do yield! linksIn (array.Get i) context ]
    | :? PdfDictionary as dictionary ->
        match standardTag dictionary context with
        | null -> [] // a marked-content or object reference, not a structure element
        | tag when PdfName.TOCI.Equals tag -> []
        | tag when PdfName.Link.Equals tag -> [ dictionary ]
        | _ -> linksIn (dictionary.Get(PdfName.K)) context
    | _ -> []

/// The page a link annotation goes to: an explicit destination, a named one (in the name
/// tree or the older /Dests dictionary), or a GoTo action's.
let private destinationPage (document: PdfDocument) context =
    let catalog = document.GetCatalog()
    let namedDestinations = lazy (catalog.GetNameTree(PdfName.Dests).GetNames())
    let named (name: PdfString) : PdfObject | null =
        match namedDestinations.Value.TryGetValue name with
        | true, destination -> destination
        | _ -> null
    let pageNumberOf (reference: PdfIndirectReference | null) =
        reference |> Option.ofObj |> Option.bind (fun reference -> lookup context.PageNumberByObject (reference.GetObjNumber()))

    let rec pageOf (destination: PdfObject | null) depth =
        if depth > 4 then
            None
        else
            match resolve destination with
            | :? PdfArray as array when array.Size() > 0 ->
                match array.Get(0, false) with
                | :? PdfIndirectReference as reference -> pageNumberOf reference
                | :? PdfDictionary as page -> pageNumberOf (page.GetIndirectReference())
                | _ -> None
            | :? PdfDictionary as dictionary -> pageOf (dictionary.Get(PdfName.D)) (depth + 1)
            | :? PdfString as name -> pageOf (named name) (depth + 1)
            | :? PdfName as name ->
                match catalog.GetPdfObject().GetAsDictionary(PdfName.Dests) |?> _.Get(name) with
                | null -> pageOf (named (PdfString(name.GetValue()))) (depth + 1)
                | destination -> pageOf destination (depth + 1)
            | _ -> None

    fun (annotation: PdfDictionary) ->
        let destination =
            match annotation.Get(PdfName.Dest) with
            | null ->
                match annotation.GetAsDictionary(PdfName.A) with
                | null -> null
                | action when PdfName.GoTo.Equals(action.GetAsName(PdfName.S)) -> action.Get(PdfName.D)
                | _ -> null
            | destination -> destination
        pageOf destination 0

/// The table of contents entry at visit, if it has text, a destination page, and an indent.
let private entryAt (document: PdfDocument) context destinationPageOf (visit: StructElemVisit) =
    let parts = ownParts visit.Element context
    let label =
        parts |> List.map (fun part -> extractText part context visit.Page) |> String.concat " " |> tocEntryText
    let geometry = parts |> List.choose (fun part -> textGeometry part context visit.Page) |> unionGeometry
    let annotations =
        ownAnnotations visit.Element @ (parts |> List.collect (fun part -> linksIn part context) |> List.collect ownAnnotations)
    let page = annotations |> List.tryPick destinationPageOf
    match label, page, geometry with
    | Some label, Some page, Some geometry ->
        Some
            { Path = visit.Path
              Label = normalize label
              Page = page
              Left = geometry.Left
              Geometry = Some geometry
              Links = annotations |> List.filter (fun annotation -> destinationPageOf annotation = Some page) }
    | _ -> None

/// What recovering headings from the document's (first) table of contents would do.
let analyse (document: PdfDocument) context : Analysis =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> NotApplicable
    | structTreeRoot ->
        let visits = walk structTreeRoot context |> Array.ofSeq
        let tags = visits |> Array.map (fun visit -> standardTag visit.Element context)

        // Which elements are inside a table of contents. walk goes depth first and each
        // path extends its parent's, so the TOCs we're inside are a stack of path prefixes.
        let insideToc = Array.zeroCreate<bool> visits.Length
        let openTocs = Stack<string>()
        for i in 0 .. visits.Length - 1 do
            while openTocs.Count > 0 && not (visits[i].Path.StartsWith(openTocs.Peek() + "/", StringComparison.Ordinal)) do
                openTocs.Pop() |> ignore
            insideToc[i] <- openTocs.Count > 0
            if PdfName.TOC.Equals tags[i] then
                openTocs.Push visits[i].Path

        let firstToc = Array.indexed visits |> Array.tryFind (fun (i, _) -> PdfName.TOC.Equals tags[i] && not insideToc[i])
        let hasHeadings = Array.exists2 (fun tag inside -> not inside && isHeadingTag tag) tags insideToc

        match firstToc with
        | None -> NotApplicable
        | Some _ when hasHeadings -> NotApplicable
        | Some(_, toc) ->
            // Only the first TOC: a later one is often a list of figures or tables, whose
            // entries name captions, not headings.
            let destinationPageOf = destinationPage document context
            let entries =
                [ for i in 0 .. visits.Length - 1 do
                      if PdfName.TOCI.Equals tags[i] && visits[i].Path.StartsWith(toc.Path + "/", StringComparison.Ordinal) then
                          yield! Option.toList (entryAt document context destinationPageOf visits[i]) ]

            // Paragraphs in the body, by page and text.
            let paragraphs =
                [ for i in 0 .. visits.Length - 1 do
                      if not insideToc[i] && PdfName.P.Equals tags[i] then
                          let visit = visits[i]
                          let page =
                              visit.Page
                              |> Option.orElseWith (fun () -> subtreeGeometry visit.Element context visit.Page |> Option.map _.PageNumber)
                          match page with
                          | Some page -> yield (page, normalize (extractText visit.Element context visit.Page)), (i, visit)
                          | None -> () ]
                |> List.groupBy fst
                |> List.map (fun (key, matches) -> key, List.map snd matches)
                |> dict
            let matchesFor (entry: Entry) =
                match paragraphs.TryGetValue((entry.Page, entry.Label)) with
                | true, matches -> matches
                | _ -> []

            let matched = entries |> List.map (fun entry -> entry, matchesFor entry)
            let unmatched =
                [ for entry, matches in matched do
                      if matches.Length <> 1 then
                          yield { Entry = entry; Matches = matches.Length } ]

            if matched |> List.forall (fun (_, matches) -> matches.IsEmpty) then
                NotApplicable
            else
                let tocGeometry = subtreeGeometry toc.Element context toc.Page
                match levelsOf (entries |> List.map _.Left) with
                | Error reason -> Unrecoverable(toc.Path, tocGeometry, reason, unmatched)
                | Ok levelOf ->
                    let headings =
                        [ for entry, matches in matched do
                              match matches with
                              | [ index, visit ] ->
                                  yield
                                      index,
                                      { Entry = entry
                                        Paragraph = visit.Element
                                        ParagraphPath = visit.Path
                                        Level = levelOf entry.Left
                                        Geometry = subtreeGeometry visit.Element context visit.Page }
                              | _ -> () ]
                        |> List.distinctBy (fun (_, heading) -> heading.ParagraphPath)
                        |> List.sortBy fst
                        |> List.map snd

                    // PDF/UA wants the first heading to be H1 and no level skipped. If unmatched
                    // entries would leave a gap, retagging the rest would make things worse.
                    let levels = headings |> List.map _.Level
                    let skip = levels |> List.pairwise |> List.tryFind (fun (previous, next) -> next > previous + 1)
                    match levels, skip with
                    | first :: _, _ when first <> 1 ->
                        Unrecoverable(
                            toc.Path,
                            tocGeometry,
                            $"the headings that could be matched would start at H{first}, not H1",
                            unmatched
                        )
                    | _, Some(previous, next) ->
                        Unrecoverable(
                            toc.Path,
                            tocGeometry,
                            $"some entries couldn't be matched, so the headings that could be would skip from H{previous} to H{next}",
                            unmatched
                        )
                    | _ -> Recoverable(headings, unmatched)
