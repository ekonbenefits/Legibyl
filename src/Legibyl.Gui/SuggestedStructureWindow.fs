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
open Legibyl.Layout
open Legibyl.Rendering
open Legibyl.Validation

/// Finds every page's layout in the background, one page at a time (each takes seconds): the
/// page being viewed first, then the pages after it, then any before. So moving to another page
/// usually shows it at once.
[<Sealed>]
type LayoutPrefetcher(suggester: LayoutSuggester) =
    let results = Collections.Concurrent.ConcurrentDictionary<int, PageSuggestion>()
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

    /// Starts finding pages, once (the model must be downloaded).
    member _.Start() =
        if worker.IsCompleted && results.Count + failures.Count < suggester.PageCount then
            worker <- Task.Run(fun () -> run ())

    /// Stops after the page being found, and waits for it.
    member _.Stop() =
        cancel.Cancel()
        worker.Wait()

/// For an untagged PDF: the structure a layout detector suggests for each page, drawn over the
/// page. A preview only; nothing is written to the PDF.
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

    let view (prefetcher: LayoutPrefetcher) (renderer: PdfRegionRenderer) (pdfPath: string) =
        Component(fun ctx ->
            let modelReady = ctx.useState (LayoutModel.isDownloaded ())
            let downloadProgress = ctx.useState (None: float option)
            let page = ctx.useState 1
            let selected = ctx.useState (None: int option)
            let suggestion = ctx.useState (None: PageSuggestion option)
            let image = ctx.useState (None: Bitmap option)
            let status = ctx.useState ""
            let readyCount = ctx.useState prefetcher.ReadyCount
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
                              { Geometry = block.Geometry
                                Label = $"{block.Order} {block.Tag}"
                                Color = colorFor block.Tag
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
                        showPage 1
                        prefetcher.Start()),
                [ EffectTrigger.AfterInit ]
            )

            let intro =
                "Suggested structure for this untagged PDF, from what each page looks like: its regions, "
                + "the tag each suggests, and their reading order (the numbers). This is a preview; nothing is written to the PDF. "
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

            let blockList =
                ScrollViewer.create [
                    ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Disabled
                    ScrollViewer.content (
                        StackPanel.create [
                            StackPanel.spacing 2.0
                            StackPanel.children [
                                for block in suggestion.Current |> Option.map _.Blocks |> Option.defaultValue [] ->
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

type SuggestedStructureWindow(pdfPath: string) as this =
    inherit
        HostWindow(Title = "Suggested Structure", Width = 1000.0, Height = 760.0, MinWidth = 600.0, MinHeight = 420.0)
    let suggester = new LayoutSuggester(pdfPath)
    let prefetcher = LayoutPrefetcher(suggester)
    let renderer = new PdfRegionRenderer()
    do this.Content <- SuggestedStructureView.view prefetcher renderer pdfPath

    override _.OnClosed(e) =
        base.OnClosed e
        // A page may still be being found: stop after it, then let go of the suggester.
        Task.Run(fun () ->
            prefetcher.Stop()
            (suggester :> IDisposable).Dispose())
        |> ignore
        renderer.Dispose()
