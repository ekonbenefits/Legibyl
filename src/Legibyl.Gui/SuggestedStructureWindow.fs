namespace Legibyl.Gui

open System
open System.Collections.Generic
open System.IO
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
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

/// What's been found and decided about one file's suggested structure: each page's layout
/// found so far, the suggestions discarded, and the page last looked at. Remembered while the
/// app is open, so reopening Suggest Structure (or tagging from the Fix dialog) picks it up.
[<Sealed>]
type LayoutReview(modified: DateTime) =
    member _.Modified = modified
    member val Pages = Collections.Concurrent.ConcurrentDictionary<int, PageSuggestion>()
    /// Discarded suggestions, by (page, order).
    member val Discarded: Set<int * int> = Set.empty with get, set
    member val LastPage = 1 with get, set
    /// Alt text written for figures, as saved next to the PDF (see FigureAlt).
    member val Figures: FigureAlt.Figure list = [] with get, set

    /// The alt text written for a suggested figure on page, if any.
    member this.AltFor(page: int, block: SuggestedBlock) =
        if block.Tag = "Figure" then FigureAlt.find this.Figures page block.Tag block.Geometry |> Option.map _.Alt else None

/// The layout reviews of the files opened this session, each forgotten if its file changes.
[<Sealed>]
type LayoutReviews() =
    let reviews = Collections.Concurrent.ConcurrentDictionary<string, LayoutReview>()

    /// This file's review, a new one if there's none or the file has changed since.
    member _.For(pdfPath: string) =
        let fullPath = Path.GetFullPath pdfPath
        let modified = File.GetLastWriteTimeUtc fullPath
        let fresh () =
            // Any alt text saved next to the PDF, written for it or an earlier version of it.
            let review = LayoutReview modified
            review.Figures <- FigureAlt.load fullPath
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

/// The regions to tag: each page's suggestions, less the ones the user discarded.
type ReviewedLayout(prefetcher: LayoutPrefetcher, review: LayoutReview) =
    interface IPageLayoutSource with
        member _.PageRegions(_, page) =
            let kept = (prefetcher.Find page).Blocks |> List.filter (fun block -> not (review.Discarded.Contains((page, block.Order))))
            List.map2
                (fun block (region: LayoutRegion) -> { region with Alt = review.AltFor(page, block) |> Option.toObj })
                kept
                (LayoutSuggestions.toRegions kept)
            :> Collections.Generic.IReadOnlyList<_>

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

    let view (window: Window) (prefetcher: LayoutPrefetcher) (review: LayoutReview) (renderer: PdfRegionRenderer) (pdfPath: string) =
        Component(fun ctx ->
            let modelReady = ctx.useState (LayoutModel.isDownloaded ())
            let downloadProgress = ctx.useState (None: float option)
            let page = ctx.useState review.LastPage
            let selected = ctx.useState (None: int option)
            let suggestion = ctx.useState (None: PageSuggestion option)
            let image = ctx.useState (None: Bitmap option)
            let status = ctx.useState ""
            let readyCount = ctx.useState prefetcher.ReadyCount
            // Suggestions the user discarded, by (page, order).
            let discarded = ctx.useState review.Discarded
            let saving = ctx.useState false
            // Bumped per page shown, so a rendering that finishes late doesn't replace a newer one.
            let version = ctx.useState (0, renderOnChange = false)
            // QR codes' page titles, read from the web only once asked for; and the addresses
            // already asked about, so each is fetched once.
            let readTitles = ctx.useState false
            let titles = ctx.useState (Map.empty: Map<string, string>)
            let asked = ctx.useState (HashSet<string>(), renderOnChange = false)
            let readingTitles = ctx.useState 0

            /// The web addresses the page's QR codes go to.
            let qrAddresses (result: PageSuggestion) =
                result.Blocks |> List.choose (fun block -> block.QrText |> Option.bind LinkTitles.webAddress)

            /// Reads the titles of the pages result's QR codes go to, if asked to and not read yet.
            let readTitlesFor (result: PageSuggestion) =
                if readTitles.Current then
                    match qrAddresses result |> List.filter asked.Current.Add with
                    | [] -> ()
                    | wanted ->
                        readingTitles.Set(readingTitles.Current + 1)
                        task {
                            try
                                let! found = Task.Run(fun () -> (LinkTitles.WebPageTitles() :> IPageTitleSource).PageTitles wanted)
                                titles.Set(found |> Seq.fold (fun all (KeyValue(address, title)) -> Map.add address title all) titles.Current)
                            finally
                                readingTitles.Set(readingTitles.Current - 1)
                        }
                        |> ignore

            let render (result: PageSuggestion) (emphasised: int option) (current: int) =
                task {
                    let regions =
                        [ for block in result.Blocks ->
                              let isDiscarded = discarded.Current.Contains((result.PageNumber, block.Order))
                              { Geometry = block.Geometry
                                Label = if isDiscarded then $"× {block.Order} {block.Tag}" else $"{block.Order} {block.Tag}"
                                Color = if isDiscarded then discardedColor else colorFor block.Tag
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

            /// Keeps or discards some of this page's suggestions, and redraws it.
            let setDiscarded (orders: int list) (discard: bool) =
                match suggestion.Current with
                | Some result ->
                    let keys = orders |> List.map (fun order -> result.PageNumber, order) |> Set.ofList
                    let next = if discard then Set.union discarded.Current keys else Set.difference discarded.Current keys
                    discarded.Set next
                    review.Discarded <- next
                    render result selected.Current version.Current |> ignore
                | None -> ()

            /// Tags a copy of the PDF with the kept suggestions (every page's, found now if need be).
            let save _ =
                task {
                    let! path =
                        Dialogs.pickSavePath
                            window
                            (Avalonia.Platform.Storage.FilePickerSaveOptions(
                                Title = "Save tagged copy",
                                SuggestedFileName = $"{Path.GetFileNameWithoutExtension pdfPath}.tagged.pdf",
                                DefaultExtension = "pdf"
                            ))
                            pdfPath
                    match path with
                    | None -> ()
                    | Some outputPath ->
                        saving.Set true
                        let before = status.Current
                        let progress =
                            Progress<FixProgress>(fun p -> status.Set(Legibyl.Gui.BusyStatus.statusText (Legibyl.Gui.BusyStatus.Fixing p)))
                        let source = ReviewedLayout(prefetcher, review)
                        try
                            try
                                let! result =
                                    Task.Run(fun () ->
                                        let validator = VeraPdfValidator(LayoutSource = source)
                                        validator.Fix(pdfPath, PdfProfile.PdfUa1, outputPath, PdfFixOptions.TagFromLayout, progress))
                                status.Set before
                                let message =
                                    match result.OutputPath with
                                    | NonNull saved ->
                                        let applied = String.Join("\n", result.AppliedFixes |> Seq.map (fun line -> "• " + line))
                                        $"Saved a tagged copy to {saved}.\n\n{applied}"
                                    | Null -> "Nothing was tagged: the document already has a structure tree."
                                do! Dialogs.showMessage window message "Save Tagged Copy"
                            with ex ->
                                status.Set before
                                do! Dialogs.showMessage window $"Couldn't save a tagged copy: {ex.Message}" "Save Tagged Copy"
                        finally
                            saving.Set false
                }
                |> ignore

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
                + "the tag each suggests, and their reading order (the numbers). Keep or discard each suggestion, then Save Tagged Copy "
                + "to tag a copy with the kept ones (a discarded region's text joins the nearest kept one; its images and lines are "
                + "decoration). The original isn't changed. "
                + "Headings are shown as H because their levels aren't known from one page, runs of paragraphs that start with "
                + "list markers are list items (LI), and Artifact marks page furniture like running headers and page numbers. "
                + "Text drawn as outlines or pixels rather than as text is read with OCR (marked OCR), and QR codes are read "
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

            // Alt text written for figures; saved next to the PDF as it's typed.
            let figures = ctx.useState review.Figures

            let saveFigures (next: FigureAlt.Figure list) =
                figures.Set next
                review.Figures <- next
                try
                    FigureAlt.save pdfPath next
                with
                | :? IOException
                | :? UnauthorizedAccessException as ex -> status.Set $"Couldn't save alt text next to the PDF: {ex.Message}"

            /// A figure's alt text, to edit. A QR code's own description shows as the hint.
            let altBox (block: SuggestedBlock) =
                let page = suggestion.Current |> Option.map _.PageNumber |> Option.defaultValue page.Current
                let written = FigureAlt.find figures.Current page block.Tag block.Geometry |> Option.map _.Alt |> Option.defaultValue ""
                TextBox.create [
                    TextBox.margin (22.0, 2.0, 0.0, 6.0)
                    TextBox.textWrapping TextWrapping.Wrap
                    TextBox.acceptsReturn false
                    TextBox.text written
                    TextBox.placeHolderText (
                        match block.QrText with
                        | Some text -> LinkTitles.qrAltText titles.Current text
                        | None -> "Alt text: what this figure shows, for someone who can't see it"
                    )
                    ToolTip.tip "Written as the figure's alt text when tagging. Saved next to the PDF, and found again if the layout is suggested again."
                    TextBox.onTextChanged (fun text ->
                        let current = FigureAlt.find figures.Current page block.Tag block.Geometry |> Option.map _.Alt |> Option.defaultValue ""
                        if text <> current then
                            saveFigures (FigureAlt.set figures.Current page block.Tag block.Geometry text))
                ]

            /// Adds alt text from another .legibyl.json file, e.g. one saved for an earlier version
            /// of this PDF under another name.
            let importAlt _ =
                task {
                    let! files =
                        window.StorageProvider.OpenFilePickerAsync(
                            Avalonia.Platform.Storage.FilePickerOpenOptions(
                                Title = "Import alt text",
                                AllowMultiple = false,
                                FileTypeFilter = [ Avalonia.Platform.Storage.FilePickerFileType("Legibyl files", Patterns = [ "*.legibyl.json"; "*.json" ]) ]
                            )
                        )
                    match files |> Seq.tryHead |> Option.bind (fun f -> Option.ofObj (f.TryGetLocalPath())) with
                    | Some path ->
                        let imported = FigureAlt.fromFile path
                        let merged, added = FigureAlt.merge figures.Current imported
                        saveFigures merged
                        let found =
                            review.Pages.Values
                            |> Seq.sumBy (fun result ->
                                result.Blocks |> List.filter (fun b -> b.Tag = "Figure" && (FigureAlt.find merged result.PageNumber b.Tag b.Geometry).IsSome) |> List.length)
                        do!
                            Dialogs.showMessage
                                window
                                $"Imported {added} of {imported.Length} alt texts from {Path.GetFileName path} (the rest were already here). {found} figures found so far have alt text."
                                "Import Alt Text"
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
                                for block in suggestion.Current |> Option.map _.Blocks |> Option.defaultValue [] ->
                                  let isDiscarded =
                                      suggestion.Current |> Option.exists (fun result -> discarded.Current.Contains((result.PageNumber, block.Order)))
                                  StackPanel.create [
                                   StackPanel.opacity (if isDiscarded then 0.5 else 1.0)
                                   StackPanel.children [
                                  Grid.create [
                                    Grid.columnDefinitions "*,Auto"
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
                                                        Border.background (brushFor block.Tag)
                                                    ]
                                                    TextBlock.create [
                                                        DockPanel.dock Dock.Left
                                                        TextBlock.width 84.0
                                                        TextBlock.fontFamily Fonts.Monospace
                                                        TextBlock.text $"{block.Order,2} {block.Tag}"
                                                    ]
                                                    TextBlock.create [
                                                        TextBlock.textWrapping TextWrapping.Wrap
                                                        TextBlock.text (
                                                            match block.QrText with
                                                            | Some text -> $"Alt: {LinkTitles.qrAltText titles.Current text}"
                                                            | None ->
                                                            if block.Text = "" then "(no text)"
                                                            elif block.Recognized then $"OCR: {snippet block.Text}"
                                                            else snippet block.Text
                                                        )
                                                    ]
                                                ]
                                            ]
                                        )
                                    ]
                                    Button.create [
                                        Grid.column 1
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
                                  if block.Tag = "Figure" then altBox block
                                   ]
                                  ]
                            ]
                        ]
                    )
                ]

            // Offered only on a page with a QR code that links somewhere: reading titles visits those sites.
            let titlesPanel =
                StackPanel.create [
                    Grid.row 1
                    StackPanel.orientation Orientation.Horizontal
                    StackPanel.spacing 8.0
                    StackPanel.margin (0.0, 6.0, 0.0, 0.0)
                    StackPanel.isVisible (suggestion.Current |> Option.exists (fun result -> not (qrAddresses result).IsEmpty))
                    StackPanel.children [
                        CheckBox.create [
                            CheckBox.content "Read link titles from the web"
                            CheckBox.isChecked readTitles.Current
                            ToolTip.tip (
                                "Visits the web page each QR code on this page links to, and names it by its title in the alt text. "
                                + "Only public web pages are visited, and only their titles read; the sites see the visit."
                            )
                            CheckBox.onIsCheckedChanged (fun e ->
                                match e.Source with
                                | :? CheckBox as checkBox ->
                                    let isChecked = checkBox.IsChecked.GetValueOrDefault()
                                    if isChecked <> readTitles.Current then
                                        readTitles.Set isChecked
                                        if isChecked then suggestion.Current |> Option.iter readTitlesFor
                                | _ -> ())
                        ]
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
                                    Button.content "Import Alt Text…"
                                    Button.margin (16.0, 0.0, 0.0, 0.0)
                                    ToolTip.tip "Use the alt text saved for another PDF, such as an earlier version of this one saved under another name."
                                    Button.onClick importAlt
                                ]
                                Button.create [
                                    Button.content "Save Tagged Copy…"
                                    Button.margin (16.0, 0.0, 0.0, 0.0)
                                    Button.isEnabled (not saving.Current)
                                    ToolTip.tip "Tag a copy of the PDF with the kept suggestions, on every page. The original isn't changed."
                                    Button.onClick save
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

            Grid.create [
                Grid.margin 12.0
                Grid.rowDefinitions "Auto,*"
                Grid.children [
                    TextBlock.create [
                        TextBlock.margin (0.0, 0.0, 0.0, 10.0)
                        TextBlock.textWrapping TextWrapping.Wrap
                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                        TextBlock.text intro
                    ]
                    ContentControl.create [
                        Grid.row 1
                        ContentControl.content (if modelReady.Current then pagePanel :> Types.IView else downloadPanel)
                    ]
                ]
            ])

type SuggestedStructureWindow(pdfPath: string, review: LayoutReview) as this =
    inherit
        HostWindow(Title = "Suggested Structure", Width = 1000.0, Height = 760.0, MinWidth = 600.0, MinHeight = 420.0)
    let suggester = new LayoutSuggester(pdfPath)
    let prefetcher = LayoutPrefetcher(suggester, review.Pages)
    let renderer = new PdfRegionRenderer()
    do this.Content <- SuggestedStructureView.view this prefetcher review renderer pdfPath

    /// A window with a review of its own, not remembered anywhere.
    new(pdfPath: string) = new SuggestedStructureWindow(pdfPath, LayoutReview(File.GetLastWriteTimeUtc pdfPath))

    override _.OnClosed(e) =
        base.OnClosed e
        // A page may still be being found: stop after it, then let go of the suggester.
        Task.Run(fun () ->
            prefetcher.Stop()
            (suggester :> IDisposable).Dispose())
        |> ignore
        renderer.Dispose()
