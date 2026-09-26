namespace Legibyl.Gui

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform.Storage
open Legibyl.Layout
open Legibyl.Validation
open Legibyl.Rendering
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
/// app is open, so reopening Suggest Structure (or tagging on Save) picks it up.
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
    /// The document title as edited in Suggest Structure (saved next to the PDF), if it was.
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

/// For an untagged PDF: the structure a layout detector suggests for each page, drawn over the
/// page. Each suggestion can be kept or discarded, and the kept ones saved as a tagged copy.
module SuggestedStructureView =
    /// 0xRRGGBB per suggested tag.
    let colorFor (tag: string) : uint32 =
        match tag with
        | "H1"
        | "H" -> 0xC2185Bu
        | "P" -> 0x1A73E8u
        | "LI" -> 0x00838Fu
        | "Figure" -> 0x2E7D32u
        | "Table" -> 0xEF6C00u
        | "Artifact" -> 0x757575u
        | _ -> 0x6A1B9Au

    let private brushFor tag =
        let color = colorFor tag
        SolidColorBrush(Color.FromRgb(byte (color >>> 16), byte (color >>> 8), byte color))

    let private snippet (text: string) =
        if text.Length > 90 then text[..89] + "…" else text

    /// Discarded regions are drawn grey.
    let discardedColor = 0x9E9E9Eu

    /// onChanged is called when the review changes (a suggestion kept or discarded, an edit, the
    /// title), so the main window can count it.
    let view
        (window: Window)
        (prefetcher: LayoutPrefetcher)
        (review: LayoutReview)
        (renderer: PdfRegionRenderer)
        (pdfPath: string)
        (onChanged: unit -> unit)
        =
        Component(fun ctx ->
            let modelReady = ctx.useState (LayoutModel.isDownloaded ())
            let downloadProgress = ctx.useState (None: float option)
            let page = ctx.useState review.LastPage
            let selected = ctx.useState (None: int option)
            let suggestion = ctx.useState (None: PageSuggestion option)
            let image = ctx.useState (None: Bitmap option)
            let status = ctx.useState ""
            let readyCount = ctx.useState prefetcher.ReadyCount
            // Bumped per page shown, so a rendering that finishes late doesn't replace a newer one.
            let version = ctx.useState (0, renderOnChange = false)
            // QR codes' page titles, read from the web only once asked for; and the addresses
            // already asked about, so each is fetched once.
            // Titles read for web addresses: from the offer before the window opened, and for QR
            // codes found since, if the offer's answer allows.
            let titles = ctx.useState review.Titles
            let asked = ctx.useState (HashSet<string>(), renderOnChange = false)
            let readingTitles = ctx.useState 0

            /// The web addresses the page's QR codes go to.
            let qrAddresses (result: PageSuggestion) =
                result.Blocks |> List.choose (fun block -> block.QrText |> Option.bind LinkTitles.webAddress)

            /// Reads the titles of the pages result's QR codes go to, if asked to and not read yet.
            let readTitlesFor (result: PageSuggestion) =
                    match
                        qrAddresses result
                        |> List.filter (fun address -> review.MayRead address && not (titles.Current.ContainsKey address))
                        |> List.filter asked.Current.Add
                    with
                    | [] -> ()
                    | wanted ->
                        readingTitles.Set(readingTitles.Current + 1)
                        task {
                            try
                                let! found = Task.Run(fun () -> (LinkTitles.WebPageTitles() :> IPageTitleSource).PageTitles wanted)
                                let merged = found |> Seq.fold (fun all (KeyValue(address, title)) -> Map.add address title all) titles.Current
                                titles.Set merged
                                review.Titles <- merged
                            finally
                                readingTitles.Set(readingTitles.Current - 1)
                        }
                        |> ignore

            let render (result: PageSuggestion) (emphasised: int option) (current: int) =
                task {
                    let regions =
                        [ for block in result.Blocks ->
                              // As edited: review.Edits is kept up to date as edits are made.
                              let isDiscarded = RegionEdits.isExcluded review.Edits result.PageNumber block
                              let tag = RegionEdits.tagOf review.Edits result.PageNumber block
                              { Geometry = block.Geometry
                                Label = if isDiscarded then $"× {block.Order} {tag}" else $"{block.Order} {tag}"
                                Color = if isDiscarded then discardedColor else colorFor tag
                                Emphasised = (emphasised = Some block.Order) } ]
                    let! bitmap =
                        Task.Run(fun () ->
                            use stream = new MemoryStream(renderer.RenderPageWithRegions(pdfPath, result.PageNumber, regions))
                            new Bitmap(stream))
                    if current = version.Current then image.Set(Some bitmap) else bitmap.Dispose()
                }

            let pagesReady () =
                if readyCount.Current >= prefetcher.PageCount then "every page's layout found"
                else $"{readyCount.Current} of {prefetcher.PageCount} pages' layouts found"

            /// Shows a page whose layout is found.
            let display (result: PageSuggestion) =
                suggestion.Set(Some result)
                status.Set(
                    if result.Rotated then "This page is rotated, and rotated pages aren't detected yet."
                    elif result.Blocks.IsEmpty then "Nothing was found on this page."
                    elif not result.HasText then
                        "This page has no text layer (it may be a scan), so tagging it would need OCR first."
                    else ""
                )
                render result None version.Current |> ignore
                readTitlesFor result

            let showPage (number: int) =
                version.Set(version.Current + 1)
                page.Set number
                review.LastPage <- number
                selected.Set None
                suggestion.Set None
                prefetcher.Want number
                match prefetcher.TryGet number, prefetcher.Failure number with
                | Some result, _ -> display result
                | None, Some reason -> status.Set $"Couldn't find page {number}'s layout: {reason}"
                | None, None -> status.Set $"Finding the layout of page {number}… (several seconds a page)"

            // Pages arrive from the prefetcher's thread; show the one being waited for.
            ctx.useEffect (
                (fun () ->
                    let ready =
                        prefetcher.PageReady.Subscribe(fun (number, result) ->
                            Avalonia.Threading.Dispatcher.UIThread.Post(fun () ->
                                readyCount.Set prefetcher.ReadyCount
                                if page.Current = number && suggestion.Current.IsNone then display result))
                    let failed =
                        prefetcher.PageFailed.Subscribe(fun (number, reason) ->
                            Avalonia.Threading.Dispatcher.UIThread.Post(fun () ->
                                if page.Current = number then status.Set $"Couldn't find page {number}'s layout: {reason}"))
                    { new IDisposable with
                        member _.Dispose() =
                            ready.Dispose()
                            failed.Dispose() }),
                [ EffectTrigger.AfterInit ]
            )

            let select (block: SuggestedBlock) =
                match suggestion.Current with
                | Some result ->
                    selected.Set(Some block.Order)
                    render result (Some block.Order) version.Current |> ignore
                | None -> ()

            /// Records the layout as found (the pages found so far) next to the PDF, with the edits
            /// on it, once a person has decided something here.
            let saveLayout () =
                if review.HasDecisions then
                    try
                        RegionEdits.saveLayout pdfPath (review.ToLayout pdfPath)
                    with
                    | :? IOException
                    | :? UnauthorizedAccessException as ex -> status.Set $"Couldn't save the review next to the PDF: {ex.Message}"

            let download _ =
                task {
                    downloadProgress.Set(Some 0.0)
                    try
                        do! LayoutModel.download (Progress<float>(fun fraction -> downloadProgress.Set(Some fraction))) CancellationToken.None
                        downloadProgress.Set None
                        modelReady.Set true
                        showPage 1
                        prefetcher.Start()
                    with ex ->
                        downloadProgress.Set None
                        status.Set $"Couldn't download the layout model: {ex.Message}"
                }
                |> ignore

            ctx.useEffect (
                (fun () ->
                    if modelReady.Current then
                        showPage page.Current
                        prefetcher.Start()),
                [ EffectTrigger.AfterInit ]
            )

            let intro =
                "Suggested structure for this untagged PDF, from what each page looks like: its regions, "
                + "the tag each suggests, and their reading order (the numbers). Keep or discard each suggestion, or pick another tag for it, then Save… in the main "
                + "window tags a copy with the kept ones (a discarded region's text joins the nearest kept one; its images and lines are "
                + "decoration), on every page. The original isn't changed. "
                + "Headings are shown as H because their levels aren't known from one page, runs of paragraphs that start with "
                + "list markers are list items (LI), and Artifact marks page furniture like running headers and page numbers. "
                + "Text drawn as outlines or pixels rather than as text is read with OCR, and can be corrected; figures can be given alt text. Both are saved next to the PDF, and QR codes are read "
                + "and shown with the alt text tagging would give them (Alt)."

            let downloadPanel =
                StackPanel.create [
                    StackPanel.spacing 12.0
                    StackPanel.maxWidth 520.0
                    StackPanel.verticalAlignment VerticalAlignment.Center
                    StackPanel.children [
                        TextBlock.create [
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.text (
                                $"Suggesting structure needs a layout detection model (PP-DocLayoutV3, about {LayoutModel.DownloadSizeMegabytes} MB). "
                                + $"It's downloaded once from {LayoutModel.source ()} into {LayoutModel.directory ()}, and runs on this computer: "
                                + "the PDF isn't sent anywhere."
                            )
                        ]
                        match downloadProgress.Current with
                        | Some fraction ->
                            ProgressBar.create [ ProgressBar.minimum 0.0; ProgressBar.maximum 1.0; ProgressBar.value fraction ]
                        | None ->
                            Button.create [
                                Button.content "Download model"
                                Button.horizontalAlignment HorizontalAlignment.Left
                                Button.onClick download
                            ]
                        TextBlock.create [
                            TextBlock.text status.Current
                            TextBlock.textWrapping TextWrapping.Wrap
                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.Danger ]
                        ]
                    ]
                ]

            // Figures' alt text and corrected OCR text; saved next to the PDF as they're typed.
            let edits = ctx.useState review.Edits

            let saveEdits (next: RegionEdits.Edit list) =
                edits.Set next
                review.Edits <- next
                onChanged ()
                try
                    RegionEdits.save pdfPath next
                with
                | :? IOException
                | :? UnauthorizedAccessException as ex -> status.Set $"Couldn't save edits next to the PDF: {ex.Message}"

            /// Keeps or leaves out some of this page's suggestions (an edit on each), and redraws it.
            let setDiscarded (orders: int list) (discard: bool) =
                match suggestion.Current with
                | Some result ->
                    result.Blocks
                    |> List.filter (fun b -> List.contains b.Order orders)
                    |> List.fold (fun all b -> RegionEdits.setExcluded all result.PageNumber b.Tag b.Geometry discard) edits.Current
                    |> saveEdits
                    saveLayout ()
                    render result selected.Current version.Current |> ignore
                | None -> ()

            /// Gives a suggestion another tag (an edit on it; the suggestion stays as found, and its
            /// other edits, found by the tag it was found with, stay with it), and redraws the page.
            let retag (block: SuggestedBlock) (tag: string) =
                match suggestion.Current with
                | Some result when tag <> RegionEdits.tagOf edits.Current result.PageNumber block ->
                    saveEdits (RegionEdits.setRetag edits.Current result.PageNumber block.Tag block.Geometry tag)
                    saveLayout ()
                    render result selected.Current version.Current |> ignore
                | _ -> ()

            let currentPage () = suggestion.Current |> Option.map _.PageNumber |> Option.defaultValue page.Current

            /// A figure's alt text, to edit. A QR code's is prefilled with where it links (named
            /// by the page's title, if read), and only a change from that is recorded.
            let altBox (block: SuggestedBlock) =
                let page = currentPage ()
                let prefilled = block.QrText |> Option.map (LinkTitles.qrAltText titles.Current)
                let shown () =
                    RegionEdits.altFor edits.Current page block
                    |> Option.orElse prefilled
                    |> Option.defaultValue ""
                TextBox.create [
                    TextBox.margin (22.0, 2.0, 0.0, 6.0)
                    TextBox.textWrapping TextWrapping.Wrap
                    TextBox.acceptsReturn false
                    TextBox.text (shown ())
                    TextBox.placeHolderText "Alt text: what this figure shows, for someone who can't see it"
                    ToolTip.tip "Written as the figure's alt text when tagging. Saved next to the PDF, and found again if the layout is suggested again."
                    TextBox.onTextChanged (fun text ->
                        if text <> shown () then
                            match prefilled with
                            | Some prefill -> saveEdits (RegionEdits.setAltAgainst edits.Current page block.Tag block.Geometry prefill text)
                            | None -> saveEdits (RegionEdits.setAlt edits.Current page block.Tag block.Geometry text))
                ]

            /// A web link's description, to edit: prefilled from its page's title, if read. Only a
            /// change from that is recorded.
            let linkBox (link: WebLink) =
                let prefilled = LinkTitles.linkDescription titles.Current link.Address |> Option.defaultValue ""
                let shown () =
                    RegionEdits.find edits.Current link.Page "Link" link.Box |> Option.bind _.Alt |> Option.defaultValue prefilled
                StackPanel.create [
                    StackPanel.margin (0.0, 2.0, 0.0, 6.0)
                    StackPanel.children [
                        TextBlock.create [
                            TextBlock.text $"Link to {link.Address}"
                            TextBlock.textWrapping TextWrapping.Wrap
                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                        ]
                        TextBox.create [
                            TextBox.margin (22.0, 2.0, 0.0, 0.0)
                            TextBox.textWrapping TextWrapping.Wrap
                            TextBox.acceptsReturn false
                            TextBox.text (shown ())
                            TextBox.placeHolderText "Description: where this link goes"
                            ToolTip.tip "Written as the link's description (/Contents) when tagging. Saved next to the PDF."
                            TextBox.onTextChanged (fun text ->
                                if text <> shown () then
                                    saveEdits (RegionEdits.setAltAgainst edits.Current link.Page "Link" link.Box prefilled text))
                        ]
                    ]
                ]

            /// A region's OCR text, to correct. Only a change is recorded; clearing it means
            /// there's nothing to read there.
            let ocrBox (block: SuggestedBlock) =
                let page = currentPage ()
                TextBox.create [
                    TextBox.margin (22.0, 2.0, 0.0, 6.0)
                    TextBox.textWrapping TextWrapping.Wrap
                    TextBox.acceptsReturn false
                    TextBox.text (RegionEdits.textFor edits.Current page block |> Option.defaultValue "")
                    TextBox.placeHolderText "Nothing to read here (cleared)"
                    ToolTip.tip
                        "What OCR read. Correct it and the correction is what a screen reader hears (its /ActualText) when tagging; it's saved next to the PDF. Put back exactly what OCR read to drop the correction."
                    TextBox.onTextChanged (fun text ->
                        if text <> (RegionEdits.textFor edits.Current page block |> Option.defaultValue "") then
                            saveEdits (RegionEdits.setText edits.Current page block.Tag block.Geometry block.Text text))
                ]

            /// Adds edits (alt text, corrected text) from another .legibyl.json file, e.g. one saved for an earlier version
            /// of this PDF under another name, or from another PDF: those saved for it, or inside it (a copy Legibyl saved).
            let importAlt _ =
                task {
                    let! files =
                        window.StorageProvider.OpenFilePickerAsync(
                            Avalonia.Platform.Storage.FilePickerOpenOptions(
                                Title = "Import edits",
                                AllowMultiple = false,
                                FileTypeFilter =
                                    [ Avalonia.Platform.Storage.FilePickerFileType("PDFs and Legibyl files", Patterns = [ "*.pdf"; "*.legibyl.json"; "*.json" ]) ]
                            )
                        )
                    match files |> Seq.tryHead |> Option.bind (fun f -> Option.ofObj (f.TryGetLocalPath())) with
                    | Some path ->
                        let! imported = Task.Run(fun () -> RegionEdits.fromFileOrPdf path)
                        let merged, added = RegionEdits.merge edits.Current imported
                        saveEdits merged
                        let found =
                            review.Pages.Values
                            |> Seq.sumBy (fun result ->
                                result.Blocks |> List.filter (fun b -> (RegionEdits.find merged result.PageNumber b.Tag b.Geometry).IsSome) |> List.length)
                        do!
                            Dialogs.showMessage
                                window
                                (if imported.IsEmpty then
                                     $"{Path.GetFileName path} has no edits (alt text or corrected text) to import."
                                 else
                                     $"Imported {added} of {imported.Length} edits (alt text and corrected text) from {Path.GetFileName path}; the rest were already here. {found} regions found so far have edits.")
                                "Import Edits"
                    | None -> ()
                }
                |> ignore

            let blockList =
                ScrollViewer.create [
                    ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Disabled
                    ScrollViewer.content (
                        StackPanel.create [
                            StackPanel.spacing 2.0
                            StackPanel.children [
                                for block in suggestion.Current |> Option.map _.Blocks |> Option.defaultValue [] do
                                  let pageNumber = suggestion.Current |> Option.map _.PageNumber |> Option.defaultValue page.Current
                                  let isDiscarded = RegionEdits.isExcluded edits.Current pageNumber block
                                  let tag = RegionEdits.tagOf edits.Current pageNumber block
                                  StackPanel.create [
                                   StackPanel.opacity (if isDiscarded then 0.5 else 1.0)
                                   StackPanel.children [
                                  Grid.create [
                                    Grid.columnDefinitions "*,Auto,Auto"
                                    Grid.children [
                                    Button.create [
                                        Button.horizontalAlignment HorizontalAlignment.Stretch
                                        Button.horizontalContentAlignment HorizontalAlignment.Left
                                        Button.padding (6.0, 4.0)
                                        if selected.Current = Some block.Order then
                                            Theme.resources
                                                [ Button.BackgroundProperty, Theme.Tokens.Fill
                                                  Button.ForegroundProperty, Theme.Tokens.Text ]
                                        else
                                            Button.background Brushes.Transparent
                                            Theme.resources [ Button.ForegroundProperty, Theme.Tokens.Text ]
                                        ToolTip.tip (
                                            match block.ListNumbering with
                                            | Some numbering ->
                                                $"Detected as {block.Label} ({block.Score:P0} confident); starts with a list marker, like the items around it (/ListNumbering /{numbering})."
                                            | None -> $"Detected as {block.Label} ({block.Score:P0} confident)."
                                        )
                                        Button.onClick (fun _ -> select block)
                                        Button.content (
                                            DockPanel.create [
                                                DockPanel.children [
                                                    Border.create [
                                                        DockPanel.dock Dock.Left
                                                        Border.width 10.0
                                                        Border.height 10.0
                                                        Border.margin (0.0, 4.0, 6.0, 0.0)
                                                        Border.verticalAlignment VerticalAlignment.Top
                                                        Border.background (brushFor tag)
                                                    ]
                                                    // Its reading order; its tag is in the picker beside it.
                                                    TextBlock.create [
                                                        DockPanel.dock Dock.Left
                                                        TextBlock.width 28.0
                                                        TextBlock.fontFamily Fonts.Monospace
                                                        TextBlock.text $"{block.Order,2}"
                                                    ]
                                                    TextBlock.create [
                                                        TextBlock.textWrapping TextWrapping.Wrap
                                                        TextBlock.text (
                                                            match block.QrText with
                                                            | Some text -> $"QR code: {snippet text}"
                                                            | None ->
                                                            if block.Text = "" then "(no text)"
                                                            // The text itself is in the box below, to correct.
                                                            elif block.Recognized then
                                                                let page = suggestion.Current |> Option.map _.PageNumber |> Option.defaultValue page.Current
                                                                match RegionEdits.find edits.Current page block.Tag block.Geometry |> Option.bind _.Text with
                                                                | Some _ -> "Read with OCR · corrected"
                                                                | None -> "Read with OCR"
                                                            else snippet block.Text
                                                        )
                                                    ]
                                                ]
                                            ]
                                        )
                                    ]
                                    ComboBox.create [
                                        Grid.column 1
                                        ComboBox.margin (4.0, 0.0, 0.0, 0.0)
                                        ComboBox.minWidth 92.0
                                        ComboBox.verticalAlignment VerticalAlignment.Top
                                        ComboBox.dataItems LayoutFile.tags
                                        ComboBox.selectedItem tag
                                        ComboBox.isEnabled (not isDiscarded)
                                        AttrBuilder<ComboBox>.CreateProperty<string | null>(
                                            Automation.AutomationProperties.NameProperty, $"Tag for region {block.Order}", ValueNone)
                                        ToolTip.tip
                                            "The tag to give it: H1 (the title), H (a heading), P, LI (a list item), Figure, Table, Caption, Note, Formula, Lbl (a label), or Artifact (page furniture, not read)."
                                        ComboBox.onSelectedItemChanged (fun item ->
                                            match item with
                                            | :? string as tag -> retag block tag
                                            | _ -> ())
                                    ]
                                    Button.create [
                                        Grid.column 2
                                        Button.margin (4.0, 0.0, 0.0, 0.0)
                                        Button.padding (8.0, 2.0)
                                        Button.verticalAlignment VerticalAlignment.Top
                                        Button.content (if isDiscarded then "Keep" else "Discard")
                                        ToolTip.tip (
                                            if isDiscarded then "Tag this region as suggested."
                                            else "Don't tag this region: its text joins the nearest kept region; its images and lines are decoration."
                                        )
                                        Button.onClick (fun _ -> setDiscarded [ block.Order ] (not isDiscarded))
                                    ]
                                    ]
                                  ]
                                  if tag = "Figure" then altBox block
                                  elif block.Recognized then ocrBox block
                                   ]
                                  ]
                                // The page's links to web pages that have no description.
                                let links =
                                    match suggestion.Current with
                                    | Some result -> review.WebLinks |> List.filter (fun link -> link.Page = result.PageNumber)
                                    | None -> []
                                if not links.IsEmpty then
                                    StackPanel.create [
                                        StackPanel.margin (0.0, 10.0, 0.0, 0.0)
                                        StackPanel.children [
                                            yield
                                                TextBlock.create [
                                                    TextBlock.text "Links on this page without a description"
                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                    TextBlock.margin (0.0, 0.0, 0.0, 4.0)
                                                ]
                                                :> Types.IView
                                            for link in links do
                                                yield linkBox link :> Types.IView
                                        ]
                                    ]
                            ]
                        ]
                    )
                ]

            // Says when titles of pages QR codes link to are being read (as the offer allowed).
            let titlesPanel =
                StackPanel.create [
                    Grid.row 1
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.margin (0.0, 6.0, 0.0, 0.0)
                    StackPanel.isVisible (readingTitles.Current > 0)
                    StackPanel.children [
                        TextBlock.create [
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.text (if readingTitles.Current > 0 then "Reading titles…" else "")
                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                        ]
                    ]
                ]

            let pagePanel =
                Grid.create [
                    Grid.rowDefinitions "Auto,Auto,Auto,*"
                    Grid.children [
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.spacing 8.0
                            StackPanel.children [
                                Button.create [
                                    Button.content "◀ Previous"
                                    Button.isEnabled (page.Current > 1)
                                    Button.onClick (fun _ -> showPage (page.Current - 1))
                                ]
                                TextBlock.create [
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                    TextBlock.text $"Page {page.Current} of {prefetcher.PageCount} · {pagesReady ()}"
                                ]
                                Button.create [
                                    Button.content "Next ▶"
                                    Button.isEnabled (page.Current < prefetcher.PageCount)
                                    Button.onClick (fun _ -> showPage (page.Current + 1))
                                ]
                                Button.create [
                                    Button.content "Keep all"
                                    Button.margin (16.0, 0.0, 0.0, 0.0)
                                    Button.isEnabled suggestion.Current.IsSome
                                    ToolTip.tip "Keep every suggestion on this page."
                                    Button.onClick (fun _ ->
                                        suggestion.Current |> Option.iter (fun result -> setDiscarded [ for b in result.Blocks -> b.Order ] false))
                                ]
                                Button.create [
                                    Button.content "Discard all"
                                    Button.isEnabled suggestion.Current.IsSome
                                    ToolTip.tip "Discard every suggestion on this page."
                                    Button.onClick (fun _ ->
                                        suggestion.Current |> Option.iter (fun result -> setDiscarded [ for b in result.Blocks -> b.Order ] true))
                                ]
                                Button.create [
                                    Button.content "Import Edits…"
                                    Button.margin (16.0, 0.0, 0.0, 0.0)
                                    ToolTip.tip "Use the alt text and corrected text saved for another PDF, such as an earlier version of this one: pick that PDF, or the .legibyl.json file saved for it."
                                    Button.onClick importAlt
                                ]
                            ]
                        ]
                        titlesPanel
                        TextBlock.create [
                            Grid.row 2
                            TextBlock.margin (0.0, 6.0, 0.0, 6.0)
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.text status.Current
                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                        ]
                        Grid.create [
                            Grid.row 3
                            Grid.columnDefinitions "2*,6,3*"
                            Grid.children [
                                blockList
                                GridSplitter.create [
                                    Grid.column 1
                                    GridSplitter.resizeDirection GridResizeDirection.Columns
                                    Theme.resources [ GridSplitter.BackgroundProperty, Theme.Tokens.Border ]
                                ]
                                Border.create [
                                    Grid.column 2
                                    Border.margin (6.0, 0.0, 0.0, 0.0)
                                    Border.padding 6.0
                                    Border.borderThickness 1.0
                                    Theme.resources
                                        [ Border.BorderBrushProperty, Theme.Tokens.Border
                                          Border.BackgroundProperty, Theme.Tokens.Fill ]
                                    Border.child (
                                        Image.create [
                                            match image.Current with
                                            | Some bitmap -> Image.source bitmap
                                            | None -> ()
                                            Image.stretch Stretch.Uniform
                                        ]
                                    )
                                ]
                            ]
                        ]
                    ]
                ]

            // The document's title: prefilled as the title fixes would find it (the document's
            // own, else a short first page), else the first H1 the layout found. Only a change
            // from what's prefilled is saved.
            let titleEdit = ctx.useState review.Title
            let documentCandidates = ctx.useStateLazy(fun () -> try DocumentTitles.candidates pdfPath with _ -> []).Current
            let headingCandidate =
                match review.Pages.TryGetValue 1 with
                | true, first ->
                    first.Blocks
                    |> List.tryFind (fun block -> RegionEdits.tagOf edits.Current 1 block = "H1")
                    |> Option.bind (fun block ->
                        RegionEdits.textFor edits.Current 1 block
                        |> Option.orElse (Some block.Text)
                        |> Option.map (fun text -> Text.RegularExpressions.Regex.Replace(text, @"\s+", " ").Trim())
                        |> Option.filter (String.IsNullOrWhiteSpace >> not))
                    |> Option.map (fun title -> { DocumentTitles.Title = title; DocumentTitles.Source = "first heading" })
                | _ -> None
            let candidates = documentCandidates @ Option.toList headingCandidate |> List.distinctBy _.Title
            let prefilled = candidates |> List.tryHead |> Option.map _.Title |> Option.defaultValue ""
            review.ShownTitle <- (if prefilled = "" then None else Some prefilled)
            let shownTitle = titleEdit.Current |> Option.defaultValue prefilled
            let setTitle (text: string) =
                let next = if text = prefilled then None else Some text
                if next <> titleEdit.Current then
                    titleEdit.Set next
                    review.Title <- next
                    onChanged ()
                    try
                        RegionEdits.saveTitle pdfPath next
                    with
                    | :? IOException
                    | :? UnauthorizedAccessException as ex -> status.Set $"Couldn't save the title next to the PDF: {ex.Message}"

            let titleRow =
                Grid.create [
                    Grid.row 1
                    Grid.margin (0.0, 0.0, 0.0, 10.0)
                    Grid.columnDefinitions "Auto,*"
                    Grid.rowDefinitions "Auto,Auto"
                    Grid.children [
                        TextBlock.create [
                            TextBlock.text "Document title"
                            TextBlock.fontWeight FontWeight.SemiBold
                            TextBlock.verticalAlignment VerticalAlignment.Center
                            TextBlock.margin (0.0, 0.0, 10.0, 0.0)
                        ]
                        TextBox.create [
                            Grid.column 1
                            TextBox.text shownTitle
                            TextBox.placeHolderText "What a screen reader and the title bar call this document"
                            ToolTip.tip "Given to the tagged copy as its title, shown in place of the file name. Saved next to the PDF if changed."
                            TextBox.onTextChanged setTitle
                        ]
                        WrapPanel.create [
                            Grid.row 1
                            Grid.column 1
                            WrapPanel.margin (0.0, 4.0, 0.0, 0.0)
                            WrapPanel.isVisible (candidates |> List.exists (fun c -> c.Title <> shownTitle))
                            WrapPanel.children [
                                yield
                                    TextBlock.create [
                                        TextBlock.text "Use:"
                                        TextBlock.verticalAlignment VerticalAlignment.Center
                                        TextBlock.margin (0.0, 0.0, 6.0, 0.0)
                                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                    ]
                                    :> Types.IView
                                for candidate in candidates do
                                    if candidate.Title <> shownTitle then
                                        yield
                                            Button.create [
                                                Button.margin (0.0, 0.0, 6.0, 0.0)
                                                Button.padding (8.0, 2.0)
                                                Button.content $"{candidate.Title} · {candidate.Source}"
                                                Button.onClick (fun _ -> setTitle candidate.Title)
                                            ]
                                            :> Types.IView
                            ]
                        ]
                    ]
                ]

            Grid.create [
                Grid.margin 12.0
                Grid.rowDefinitions "Auto,Auto,*"
                Grid.children [
                    TextBlock.create [
                        TextBlock.margin (0.0, 0.0, 0.0, 10.0)
                        TextBlock.textWrapping TextWrapping.Wrap
                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                        TextBlock.text intro
                    ]
                    titleRow
                    ContentControl.create [
                        Grid.row 2
                        ContentControl.content (if modelReady.Current then pagePanel :> Types.IView else downloadPanel)
                    ]
                ]
            ])

/// onChanged is called when the review changes.
type SuggestedStructureWindow(pdfPath: string, review: LayoutReview, onChanged: unit -> unit) as this =
    inherit
        HostWindow(Title = "Suggested Structure", Width = 1000.0, Height = 760.0, MinWidth = 600.0, MinHeight = 420.0)
    let suggester = new LayoutSuggester(pdfPath)
    let prefetcher = LayoutPrefetcher(suggester, review.Pages)
    let renderer = new PdfRegionRenderer()
    do this.Content <- SuggestedStructureView.view this prefetcher review renderer pdfPath onChanged

    new(pdfPath: string, review: LayoutReview) = new SuggestedStructureWindow(pdfPath, review, ignore)

    /// A window with a review of its own, not remembered anywhere.
    new(pdfPath: string) = new SuggestedStructureWindow(pdfPath, LayoutReview(File.GetLastWriteTimeUtc pdfPath))

    override _.OnClosed(e) =
        base.OnClosed e
        // The pages found since the last change are worth keeping too.
        if review.HasDecisions then
            try
                RegionEdits.saveLayout pdfPath (review.ToLayout pdfPath)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()
        // A page may still be being found: stop after it, then let go of the suggester.
        Task.Run(fun () ->
            prefetcher.Stop()
            (suggester :> IDisposable).Dispose())
        |> ignore
        renderer.Dispose()
