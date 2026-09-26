namespace Legibyl.Gui

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open Legibyl.Layout
open Legibyl.Validation

/// What was chosen about reading the titles of the web pages links go to, asked before Suggest
/// Structure opens.
type LinkTitlesChoice =
    | NotAsked
    | Skipped
    /// Read them, except for these addresses.
    | Read of except: Set<string>

/// What's been found and decided about one file's suggested structure: each page's layout
/// found so far (as found: what a person decides is in Edits, on top), and the page last looked
/// at. Remembered while the
/// app is open, so viewing the structure again (or tagging on Save) picks it up.
[<Sealed>]
type LayoutReview(modified: DateTime) =
    member _.Modified = modified
    member val Pages = Collections.Concurrent.ConcurrentDictionary<int, PageSuggestion>()
    member val LastPage = 1 with get, set
    /// What a person decided about the suggestions, as saved next to the PDF (see RegionEdits):
    /// suggestions left out or retagged, figures' alt text, corrected OCR text.
    member val Edits: RegionEdits.Edit list = [] with get, set
    member val LinkTitles = NotAsked with get, set
    /// The titles read for web addresses, by address.
    member val Titles: Map<string, string> = Map.empty with get, set
    /// The PDF's links to web pages without a description.
    member val WebLinks: WebLink list = [] with get, set

    member this.TitleLookup = this.Titles :> Collections.Generic.IReadOnlyDictionary<string, string>
    /// The document title as edited on the suggested structure (saved next to the PDF), if it was.
    member val Title: string option = None with get, set
    /// The title the window prefilled and showed, if it was opened.
    member val ShownTitle: string option = None with get, set

    /// The title to give the document: as edited, else as shown prefilled; None to leave it.
    member this.DocumentTitle = this.Title |> Option.orElse this.ShownTitle

    /// The layout as found (see LayoutFile): each page found so far, to record next to the PDF;
    /// None before any page is found. Edits aren't in it; they're layered on top.
    member this.ToLayout(pdfPath: string) : LayoutFile.SavedLayout option =
        if this.Pages.IsEmpty then
            None
        else
            Some(LayoutSuggestions.toLayoutFile pdfPath (this.Pages.Values |> Seq.sortBy _.PageNumber |> List.ofSeq) None)

    /// Whether a layout was recorded next to the PDF before.
    member val LayoutKept = false with get, set

    /// Whether a person has decided anything here (an edit or a title), so the layout the edits
    /// are on is worth recording next to the PDF.
    member this.HasDecisions = this.LayoutKept || not this.Edits.IsEmpty || this.Title.IsSome

    /// Whether a web address's title may be read.
    member this.MayRead(address: string) =
        match this.LinkTitles with
        | Read except -> not (except.Contains address)
        | _ -> false

    /// A QR code's alt text as prefilled: where it links, named by its page's title if read.
    member this.QrAlt(qrText: string) = LinkTitles.qrAltText this.TitleLookup qrText

    /// A web link's description as prefilled: its page's title, if read; "" if not.
    member this.LinkPrefill(address: string) =
        LinkTitles.linkDescription this.TitleLookup address |> Option.defaultValue ""

    /// A web link's description: as edited, else as prefilled; None for none.
    member this.LinkDescription(link: WebLink) =
        match RegionEdits.find this.Edits link.Page "Link" link.Box |> Option.bind _.Alt with
        | Some edited -> if String.IsNullOrWhiteSpace edited then None else Some edited
        | None -> Some(this.LinkPrefill link.Address) |> Option.filter (String.IsNullOrWhiteSpace >> not)

/// The layout reviews of the files opened this session, each forgotten if its file changes.
[<Sealed>]
type LayoutReviews() =
    let reviews = Collections.Concurrent.ConcurrentDictionary<string, LayoutReview>()

    /// This file's review, a new one if there's none or the file has changed since.
    member _.For(pdfPath: string) =
        let fullPath = Path.GetFullPath pdfPath
        let modified = File.GetLastWriteTimeUtc fullPath
        let fresh () =
            // Any edits saved next to the PDF, made for it or an earlier version of it, and the
            // layout recorded with them: its pages count as found.
            let review = LayoutReview modified
            let edits, title, layout = RegionEdits.loadReview fullPath
            review.Edits <- edits
            review.Title <- title
            match layout with
            | Some layout ->
                review.LayoutKept <- true
                for page in layout.Pages do
                    review.Pages[page.Page] <- LayoutSuggestions.ofLayoutPage page
            | None -> ()
            review
        reviews.AddOrUpdate(fullPath, (fun _ -> fresh ()), (fun _ existing -> if existing.Modified = modified then existing else fresh ()))

/// Finds every page's layout in the background, one page at a time (each takes seconds): the
/// page being viewed first, then the pages after it, then any before. So moving to another page
/// usually shows it at once.
[<Sealed>]
type LayoutPrefetcher(suggester: LayoutSuggester, results: Collections.Concurrent.ConcurrentDictionary<int, PageSuggestion>) =
    let failures = Collections.Concurrent.ConcurrentDictionary<int, string>()
    let cancel = new CancellationTokenSource()
    let pageReady = Event<int * PageSuggestion>()
    let pageFailed = Event<int * string>()
    let mutable wanted = 1
    let mutable worker: Task = Task.CompletedTask

    let next () =
        let count = suggester.PageCount
        let current = wanted
        seq { yield current; yield! [ current + 1 .. count ]; yield! [ 1 .. current - 1 ] }
        |> Seq.tryFind (fun page -> page >= 1 && page <= count && not (results.ContainsKey page) && not (failures.ContainsKey page))

    let rec run () =
        match next () with
        | Some page when not cancel.IsCancellationRequested ->
            try
                let result = suggester.Suggest page
                results[page] <- result
                if not cancel.IsCancellationRequested then pageReady.Trigger((page, result))
            with ex when not cancel.IsCancellationRequested ->
                failures[page] <- ex.Message
                pageFailed.Trigger((page, ex.Message))
            run ()
        | _ -> ()

    /// Raised on a background thread as each page's layout is found.
    member _.PageReady = pageReady.Publish
    /// Raised on a background thread when a page's layout can't be found, with why.
    member _.PageFailed = pageFailed.Publish
    member _.PageCount = suggester.PageCount
    member _.ReadyCount = results.Count

    /// Makes page the next one found, if it isn't found already.
    member _.Want(page: int) = wanted <- page

    member _.TryGet(page: int) =
        match results.TryGetValue page with
        | true, result -> Some result
        | _ -> None

    member _.Failure(page: int) =
        match failures.TryGetValue page with
        | true, reason -> Some reason
        | _ -> None

    /// A page's layout: found already, or found now (waiting for the page in progress).
    member _.Find(page: int) =
        match results.TryGetValue page with
        | true, result -> result
        | _ ->
            let result = suggester.Suggest page
            results[page] <- result
            result

    /// Starts finding pages, once (the model must be downloaded).
    member _.Start() =
        if worker.IsCompleted && results.Count + failures.Count < suggester.PageCount then
            worker <- Task.Run(fun () -> run ())

    /// Stops after the page being found, and waits for it.
    member _.Stop() =
        cancel.Cancel()
        worker.Wait()

/// The regions to tag: each page's suggestions, with the review's edits layered on (left out,
/// retagged, alt text, corrected text).
type ReviewedLayout(prefetcher: LayoutPrefetcher, review: LayoutReview) =
    interface IPageLayoutSource with
        member _.PageRegions(_, page) =
            RegionEdits.onRegions review.Edits (prefetcher.Find page).Blocks :> Collections.Generic.IReadOnlyList<_>

        // QR codes' descriptions name the titles read in the window (as it showed them).
        member _.PageTitles = review.TitleLookup
        member _.DocumentTitle = review.DocumentTitle |> Option.toObj

        member _.LinkDescription(_, page, rectangle) =
            review.WebLinks
            |> List.tryFind (fun link -> link.Page = page && RegionEdits.overlap link.Box rectangle >= RegionEdits.SameRegionOverlap)
            |> Option.bind review.LinkDescription
            |> Option.toObj

/// A suggested layout as a structure, to show as View Structure shows a tagged PDF's: each
/// region an element with the tag it was found with (edits stay on top), in reading order,
/// consecutive list items in a list, and a page's web links inside the region they're in.
module SuggestedTree =
    let private node (tag: string) (geometry: StructureGeometry | null) (children: StructureNode list) : StructureNode =
        { Tag = tag
          Role = null
          AlternateDescription = null
          ActualText = null
          Content = null
          FormFieldName = null
          FormFieldDescription = null
          FormFieldType = null
          FormFieldValue = null
          LinkUri = null
          LinkDestinationPageNumber = Nullable()
          LinkDestinationDescription = null
          LinkDestination = null
          LinkDescription = null
          Geometry = geometry
          Language = null
          PageNumber = (match geometry with NonNull g -> Nullable g.PageNumber | Null -> Nullable())
          ContentLeafCount = (if isNull geometry then 0 else 1)
          Children = children }

    /// How much of inner is inside outer, 0-1.
    let private within (outer: StructureGeometry) (inner: StructureGeometry) =
        let width = min outer.Right inner.Right - max outer.Left inner.Left
        let height = min outer.Top inner.Top - max outer.Bottom inner.Bottom
        let area = max 0.0 (inner.Right - inner.Left) * max 0.0 (inner.Top - inner.Bottom)
        if area <= 0.0 then 0.0 else max 0.0 width * max 0.0 height / area

    /// A page's suggestions as elements. A link's description is prefilled from its page's title
    /// if read, a QR code's alt text with where it links; an edit is a change from those.
    let pageNodes (review: LayoutReview) (page: PageSuggestion) : StructureNode list =
        let links = review.WebLinks |> List.filter (fun link -> link.Page = page.PageNumber)
        let linkNode (link: WebLink) =
            let prefill = review.LinkPrefill link.Address
            { node "Link" link.Box [] with
                LinkUri = link.Address
                LinkDescription = (if String.IsNullOrWhiteSpace prefill then null else prefill) }
        // Each link goes in the region it's most inside, if it's mostly inside one.
        let home (link: WebLink) =
            page.Blocks
            |> List.map (fun block -> block.Order, within block.Geometry link.Box)
            |> List.filter (fun (_, inside) -> inside >= 0.5)
            |> List.sortByDescending snd
            |> List.tryHead
            |> Option.map fst
        let homes = links |> List.map (fun link -> link, home link)
        let region (block: SuggestedBlock) =
            let text = if block.Text = "" then null else block.Text
            { node block.Tag block.Geometry [ for link, order in homes do if order = Some block.Order then linkNode link ] with
                Content = (if block.Recognized then null else text)
                ActualText = (if block.Recognized then block.Text else null)
                AlternateDescription = (block.QrText |> Option.map review.QrAlt |> Option.toObj) }
        let rec group (blocks: SuggestedBlock list) =
            match blocks with
            | [] -> []
            | first :: _ when first.Tag = "LI" ->
                let items = blocks |> List.takeWhile (fun b -> b.Tag = "LI")
                node "L" null [ for item in items -> region item ] :: group (blocks |> List.skip items.Length)
            | first :: rest -> region first :: group rest
        group (page.Blocks |> List.sortBy _.Order)
        @ [ for link, order in homes do if order.IsNone then linkNode link ]

    /// The whole document: a Document element holding every page found so far, in order.
    let build (pages: StructureNode list seq) : StructureNode =
        node "StructTreeRoot" null [ node "Document" null (List.concat pages) ]
