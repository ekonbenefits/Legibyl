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

/// For an untagged PDF: the structure a layout detector suggests for each page, drawn over the
/// page. A preview only; nothing is written to the PDF.
module SuggestedStructureView =
    /// 0xRRGGBB per suggested tag.
    let colorFor (tag: string) : uint32 =
        match tag with
        | "H1"
        | "H" -> 0xC2185Bu
        | "P" -> 0x1A73E8u
        | "Figure" -> 0x2E7D32u
        | "Table" -> 0xEF6C00u
        | "Artifact" -> 0x757575u
        | _ -> 0x6A1B9Au

    let private brushFor tag =
        let color = colorFor tag
        SolidColorBrush(Color.FromRgb(byte (color >>> 16), byte (color >>> 8), byte color))

    let private snippet (text: string) =
        if text.Length > 90 then text[..89] + "…" else text

    let view (suggester: LayoutSuggester) (renderer: PdfRegionRenderer) (pdfPath: string) =
        Component(fun ctx ->
            let modelReady = ctx.useState (LayoutModel.isDownloaded ())
            let downloadProgress = ctx.useState (None: float option)
            let page = ctx.useState 1
            let selected = ctx.useState (None: int option)
            let suggestion = ctx.useState (None: PageSuggestion option)
            let image = ctx.useState (None: Bitmap option)
            let status = ctx.useState ""
            // Pages already detected: each takes seconds.
            let detected = ctx.useState (Dictionary<int, PageSuggestion>(), renderOnChange = false)
            // Bumped per request, so a slow page that finishes late doesn't replace a newer one.
            let version = ctx.useState (0, renderOnChange = false)

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

            let showPage (number: int) =
                task {
                    version.Set(version.Current + 1)
                    let current = version.Current
                    page.Set number
                    selected.Set None
                    suggestion.Set None
                    try
                        let! result =
                            match detected.Current.TryGetValue number with
                            | true, result -> Task.FromResult result
                            | _ ->
                                status.Set $"Finding the layout of page {number}… (this takes several seconds a page)"
                                Task.Run(fun () -> suggester.Suggest number)
                        detected.Current[number] <- result
                        if current = version.Current then
                            suggestion.Set(Some result)
                            status.Set(
                                if result.Blocks.IsEmpty then "Nothing was found on this page."
                                elif not result.HasText then
                                    "This page has no text layer (it may be a scan), so tagging it would need OCR first."
                                else ""
                            )
                            do! render result None current
                    with ex ->
                        if current = version.Current then
                            status.Set $"Couldn't find page {number}'s layout: {ex.Message}"
                }
                |> ignore

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
                    with ex ->
                        downloadProgress.Set None
                        status.Set $"Couldn't download the layout model: {ex.Message}"
                }
                |> ignore

            ctx.useEffect ((fun () -> if modelReady.Current then showPage 1), [ EffectTrigger.AfterInit ])

            let intro =
                "Suggested structure for this untagged PDF, from what each page looks like: its regions, "
                + "the tag each suggests, and their reading order (the numbers). This is a preview; nothing is written to the PDF. "
                + "Headings are shown as H because their levels aren't known from one page, and Artifact marks page furniture "
                + "like running headers and page numbers."

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
                                        ToolTip.tip $"Detected as {block.Label} ({block.Score:P0} confident)."
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
                                                        TextBlock.width 76.0
                                                        TextBlock.fontFamily Fonts.Monospace
                                                        TextBlock.text $"{block.Order,2} {block.Tag}"
                                                    ]
                                                    TextBlock.create [
                                                        TextBlock.textWrapping TextWrapping.Wrap
                                                        TextBlock.text (if block.Text = "" then "(no text)" else snippet block.Text)
                                                    ]
                                                ]
                                            ]
                                        )
                                    ]
                            ]
                        ]
                    )
                ]

            let pagePanel =
                Grid.create [
                    Grid.rowDefinitions "Auto,Auto,*"
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
                                    TextBlock.text $"Page {page.Current} of {suggester.PageCount}"
                                ]
                                Button.create [
                                    Button.content "Next ▶"
                                    Button.isEnabled (page.Current < suggester.PageCount)
                                    Button.onClick (fun _ -> showPage (page.Current + 1))
                                ]
                            ]
                        ]
                        TextBlock.create [
                            Grid.row 1
                            TextBlock.margin (0.0, 6.0, 0.0, 6.0)
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.text status.Current
                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                        ]
                        Grid.create [
                            Grid.row 2
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
    let renderer = new PdfRegionRenderer()
    do this.Content <- SuggestedStructureView.view suggester renderer pdfPath

    override _.OnClosed(e) =
        base.OnClosed e
        // A detection may still be running; it holds the suggester's lock until it finishes.
        Task.Run(fun () -> (suggester :> IDisposable).Dispose()) |> ignore
        renderer.Dispose()
