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

/// A PDF's review: what a person has decided about it (warnings dismissed, edits, a title) and,
/// for an untagged PDF, each page's layout found so far (as found: the edits are on top). Save
/// keeps it in the "<name>.pdf.legibyl.json" file next to the PDF (see ReviewFile); until then
/// it's only here, and IsEdited says so. The PDF itself is never changed. A review carried
/// inside the PDF (a copy Legibyl saved) is read-only: its edits and auto fixes are in the PDF
/// already, for reference; its dismissals are where the PDF's own start, until the file next to
/// it has its own.
[<Sealed>]
type DocumentReview(modified: DateTime) =
    member _.Modified = modified
    /// Warnings dismissed: as the file next to the PDF has them, else as the review carried inside it.
    member val Dismissed: Dismissals.Dismissed = Map.empty with get, set
    /// The review saved inside the PDF, if it's a copy Legibyl saved: read-only.
    member val Carried: ReviewFile.SavedFile option = None with get, set
    /// The auto fixes chosen for Export; None before any are (the dialog then starts with the
    /// safe ones the PDF needs).
    member val AutoFixes: PdfFixOptions option = None with get, set
    /// The last validation's result (see SavedResults): as saved next to the PDF, or a newer one
    /// Save hasn't kept yet.
    member val Validation: ReviewFile.SavedValidation option = None with get, set
    /// What was last saved (or read): the dismissals, edits, title, auto fixes and which validation.
    member val private Saved:
        Dismissals.Dismissed * RegionEdits.Edit list * string option * PdfFixOptions option * (string * DateTime) option * (string * Set<string>) option =
        (Map.empty, [], None, None, None, None) with get, set

    /// Which validation it has: the file checked, and when.
    member private this.ValidationStamp = this.Validation |> Option.map (fun v -> v.Hash, v.CheckedAt)

    /// A passing PDF's checklist steps ticked (see ReviewFile.SavedChecklist), and the file they
    /// were ticked for (its hash); None before any.
    member val Checklist: (string * Set<string>) option = None with get, set

    /// When a step was last ticked (or unticked).
    member val ChecklistAt: DateTime option = None with get, set

    /// The checklist carried inside the PDF, if it's a copy Legibyl wrote for steps a person
    /// checked, and its content is still what was checked (see ReviewFile.carriedChecklist).
    member val CarriedChecklist: ReviewFile.SavedChecklist option = None with get, set

    /// The steps checked in the copy it is (see CarriedChecklist): ticked already, read-only.
    member this.CarriedChecked =
        match this.CarriedChecklist with
        | Some checklist -> checklist.Checked |> Option.ofObj |> Option.map Set.ofArray |> Option.defaultValue Set.empty
        | None -> Set.empty

    /// The steps ticked for the file with hash; none if they were ticked for another.
    member this.CheckedFor(hash: string) =
        match this.Checklist with
        | Some(forHash, steps) when forHash = hash -> steps
        | _ -> Set.empty

    /// The warnings dismissed in the review carried inside the PDF.
    member this.CarriedDismissed = this.Carried |> Option.map Dismissals.ofSaved |> Option.defaultValue Map.empty
    /// The edits in the review carried inside the PDF (in it already).
    member this.CarriedEdits = this.Carried |> Option.map RegionEdits.editsIn |> Option.defaultValue []
    member val Pages = Collections.Concurrent.ConcurrentDictionary<int, PageSuggestion>()
    member val LastPage = 1 with get, set
    /// What a person decided about the suggestions, as saved next to the PDF (see RegionEdits):
    /// suggestions merged into their neighbours or retagged, figures' alt text, corrected OCR text.
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

    /// The review as Save keeps it: the layout is recorded only once something's decided.
    member this.ToFile(pdfPath: string) : ReviewFile.SavedFile =
        let edits = RegionEdits.toElementEdits this.Edits |> List.map ReviewFile.ofElementEdit |> Array.ofList
        { Version = 1
          // None dismissed, where the review carried inside the PDF has some, is kept as none.
          DismissedWarnings =
            if this.Dismissed.IsEmpty && not this.CarriedDismissed.IsEmpty then [||] else Dismissals.toSaved this.Dismissed
          Edits = (if edits.Length = 0 then null else edits)
          Title = Option.toObj this.Title
          Layout = (if this.HasDecisions then this.ToLayout pdfPath |> Option.toObj else null)
          Fixes = (this.AutoFixes |> Option.map (FixNames.ofOptions >> Array.ofList) |> Option.toObj)
          Validation = Option.toObj this.Validation
          Checklist =
            match this.Checklist with
            | Some(hash, steps) when not steps.IsEmpty ->
                { Hash = hash; Checked = Array.ofSeq steps; CheckedAt = Option.toNullable this.ChecklistAt; Length = 0L }
            | _ -> null }

    /// Keeps the review in the file next to pdfPath (deleting it if there's nothing to keep).
    /// Throws IOException/UnauthorizedAccessException if it can't be written.
    member this.Save(pdfPath: string) =
        let file = this.ToFile pdfPath
        ReviewFile.writeFile pdfPath file
        this.LayoutKept <- not (isNull file.Layout)
        this.MarkSaved()

    /// The review a copy of pdfPath carries (see VeraPdfValidator.Review): this one, saved or not,
    /// with the edits carried inside pdfPath under its own; its edits moved to the elements tagging
    /// made. Its dismissals are this one's; its auto fixes those the copy was made with.
    member this.ForCopy(pdfPath: string) =
        let file = this.CopyFile pdfPath
        ReviewFile.forCopy (if ReviewFile.isEmpty file then None else Some file)

    /// The review a copy carries: this one, with the edits carried inside pdfPath under its own.
    member private this.CopyFile(pdfPath: string) =
        let own = this.ToFile pdfPath
        match this.Carried with
        | Some carried ->
            let edits = Array.append (Option.ofObj carried.Edits |> Option.defaultValue [||]) (Option.ofObj own.Edits |> Option.defaultValue [||])
            { own with
                Edits = (if edits.Length = 0 then null else edits)
                Title = (if isNull own.Title then carried.Title else own.Title)
                Fixes = own.Fixes }
        | None -> own

    /// Writes pdfPath to outputPath unchanged, carrying its review with the steps a person checked
    /// of it (those ticked here, and any the copy it is carries), for the file with hash.
    member this.WriteCheckedCopy(pdfPath: string, outputPath: string, hash: string) =
        let checkedSteps = Set.union (this.CheckedFor hash) this.CarriedChecked
        ReviewFile.checkedCopy pdfPath outputPath (this.CopyFile pdfPath) hash checkedSteps (this.ChecklistAt |> Option.defaultValue DateTime.Now)

    /// Whether there are changes Save hasn't kept: the dismissals, edits, title or auto fixes differ
    /// from what was last saved (changing one back isn't a change), or there's a newer validation.
    member this.IsEdited = (this.Dismissed, this.Edits, this.Title, this.AutoFixes, this.ValidationStamp, this.Checklist) <> this.Saved

    /// Takes what it has now as what's saved.
    member this.MarkSaved() = this.Saved <- (this.Dismissed, this.Edits, this.Title, this.AutoFixes, this.ValidationStamp, this.Checklist)

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

/// The reviews of the files opened this session, each read again if its file changes (unless it
/// has changes not saved).
[<Sealed>]
type DocumentReviews() =
    let reviews = Collections.Concurrent.ConcurrentDictionary<string, DocumentReview>()

    /// Drops this file's review with any changes not saved: it's read again next time.
    member _.Forget(pdfPath: string) = reviews.TryRemove(Path.GetFullPath pdfPath) |> ignore

    /// This file's review, a new one if there's none or the file has changed since.
    member _.For(pdfPath: string) =
        let fullPath = Path.GetFullPath pdfPath
        let modified = File.GetLastWriteTimeUtc fullPath
        let fresh () =
            // The review saved next to the PDF, and the layout recorded with it: its pages that
            // haven't changed since count as found.
            let review = DocumentReview modified
            let file = ReviewFile.readFile (ReviewFile.filePath fullPath)
            match file with
            | Some saved ->
                review.Edits <- RegionEdits.editsIn saved
                review.Title <- Option.ofObj saved.Title
                review.AutoFixes <- Option.ofObj saved.Fixes |> Option.map FixNames.toOptions
                review.Validation <- Option.ofObj saved.Validation
                review.Checklist <-
                    match saved.Checklist with
                    | NonNull checklist ->
                        // A hand-edited file may leave the steps out.
                        let ticked = match box checklist.Checked with :? (string array) as steps -> Set.ofArray steps | _ -> Set.empty
                        review.ChecklistAt <- Option.ofNullable checklist.CheckedAt
                        Some(checklist.Hash, ticked)
                    | Null -> None
                match Option.ofObj saved.Layout |> Option.map (LayoutFile.current fullPath) with
                | Some layout ->
                    review.LayoutKept <- true
                    for page in layout.Pages do
                        review.Pages[page.Page] <- LayoutSuggestions.ofLayoutPage page
                | None -> ()
            | None -> ()
            review.Carried <- if File.Exists fullPath then EmbeddedReview.read fullPath |> Option.bind ReviewFile.parse else None
            review.CarriedChecklist <- if File.Exists fullPath then ReviewFile.carriedChecklist fullPath else None
            // The file's dismissals, if it has any (even none), override those carried inside.
            review.Dismissed <-
                match file with
                | Some saved when not (isNull saved.DismissedWarnings) -> Dismissals.ofSaved saved
                | _ -> review.CarriedDismissed
            review.MarkSaved()
            review
        reviews.AddOrUpdate(
            fullPath,
            (fun _ -> fresh ()),
            (fun _ existing -> if existing.Modified = modified || existing.IsEdited then existing else fresh ())
        )

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

/// The regions to tag: each page's suggestions, with the review's edits layered on (merged into neighbours,
/// retagged, alt text, corrected text).
type ReviewedLayout(prefetcher: LayoutPrefetcher, review: DocumentReview) =
    interface IPageLayoutSource with
        member _.PageRegions(_, page) =
            RegionEdits.onRegions review.Edits (prefetcher.Find page).Blocks :> Collections.Generic.IReadOnlyList<_>

        // QR codes' descriptions name the titles read in the window (as it showed them).
        member _.PageTitles = review.TitleLookup
        member _.DocumentTitle = review.DocumentTitle |> Option.toObj

        member _.LinkDescription(_, page, rectangle) =
            review.WebLinks
            |> List.tryFind (fun link -> link.Page = page && Boxes.overlap link.Box rectangle >= RegionEdits.SameRegionOverlap)
            |> Option.bind review.LinkDescription
            |> Option.toObj

/// A suggested layout as a structure, to show as Structure… shows a tagged PDF's: each
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
          ColSpan = 1
          RowSpan = 1
          Children = children }

    /// How much of inner is inside outer, 0-1.
    let private within (outer: StructureGeometry) (inner: StructureGeometry) =
        let width = min outer.Right inner.Right - max outer.Left inner.Left
        let height = min outer.Top inner.Top - max outer.Bottom inner.Bottom
        let area = max 0.0 (inner.Right - inner.Left) * max 0.0 (inner.Top - inner.Bottom)
        if area <= 0.0 then 0.0 else max 0.0 width * max 0.0 height / area

    /// A page's suggestions as elements. A link's description is prefilled from its page's title
    /// if read, a QR code's alt text with where it links; an edit is a change from those.
    let pageNodes (review: DocumentReview) (page: PageSuggestion) : StructureNode list =
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
            // A table's cells, after it, in its rows, with their spans, as tagging writes them
            // (see TableGrid); its own text is theirs.
            | first :: rest when first.Tag = "Table" ->
                let cells = rest |> List.takeWhile (fun b -> TableGrid.isCell b.Tag && within first.Geometry b.Geometry >= 0.5)
                if cells.IsEmpty then
                    region first :: group rest
                else
                    let places = TableGrid.place [ for cell in cells -> cell.Geometry ]
                    let rowCount = places |> List.map (fun p -> p.Row + p.RowSpan) |> List.max
                    let rows =
                        [ for row in 0 .. rowCount - 1 ->
                              node "TR" null [
                                  for cell, place in List.zip cells places |> List.filter (fun (_, p) -> p.Row = row) |> List.sortBy (fun (_, p) -> p.Column) ->
                                      { region cell with RowSpan = place.RowSpan; ColSpan = place.ColSpan }
                              ] ]
                    let table = region first
                    { table with Content = null; Children = List.ofSeq table.Children @ rows } :: group (rest |> List.skip cells.Length)
            | first :: rest -> region first :: group rest
        group (page.Blocks |> List.sortBy _.Order)
        @ [ for link, order in homes do if order.IsNone then linkNode link ]

    /// The whole document: a Document element holding every page found so far, in order.
    let build (pages: StructureNode list seq) : StructureNode =
        node "StructTreeRoot" null [ node "Document" null (List.concat pages) ]
