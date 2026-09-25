namespace Legibyl.Gui

open System
open System.IO
open System.Text.Json
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform.Storage
open Avalonia.VisualTree
open Legibyl.Rendering
open Legibyl.Validation

/// Formats a StructureNode into a single-line summary for the structure TreeView.
module StructureSummary =
    let format (node: StructureNode) =
        let parts =
            [ yield $"<{node.Tag}>"
              match node.Role with NonNull role -> yield $"role={role}" | Null -> ()
              if node.PageNumber.HasValue then yield $"p.{node.PageNumber.Value}"
              match node.Language with NonNull lang -> yield $"[{lang}]" | Null -> ()
              match node.AlternateDescription with NonNull alt -> yield $"alt: \"{alt}\"" | Null -> ()
              match node.ActualText with NonNull text -> yield $"actualText: \"{text}\"" | Null -> ()
              match node.FormFieldDescription with NonNull tooltip -> yield $"tooltip: \"{tooltip}\"" | Null -> ()
              match node.FormFieldName with NonNull name -> yield $"field: {name}" | Null -> ()
              match node.FormFieldType with NonNull fieldType -> yield $"type: {fieldType}" | Null -> ()
              match node.LinkUri with NonNull href -> yield $"href: {href}" | Null -> ()
              match node.LinkDestinationDescription with NonNull dest -> yield $"dest: {dest}" | Null -> ()
              if node.ContentLeafCount > 0 then
                  yield $"""({node.ContentLeafCount} content item{if node.ContentLeafCount = 1 then "" else "s"})""" ]

        let summary = String.Join("  ", parts)
        match node.Content with
        | NonNull content ->
            let maxPreview = 60
            let preview = if content.Length > maxPreview then content[.. maxPreview - 1] + "…" else content
            summary + $"  — \"{preview}\""
        | Null -> summary

/// Shows a tagged PDF's logical structure tree (the semantic hierarchy that screen
/// readers use) both as an HTML-like nested "boxes with borders" layout (so a sighted
/// user can visually see block-level nesting and the real text each element owns) and
/// as a browsable outline, with an option to export the whole tree — including content
/// text — as JSON for AI agents or other tooling.
module private StructureView =
    let jsonOptions = JsonSerializerOptions(WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

    // Tag -> (border color, header background) by rough semantic category, so the
    // layout view reads like a color-coded HTML block diagram at a glance.
    let categoryColors =
        let heading = Colors.SteelBlue, Color.FromRgb(0xDDuy, 0xE9uy, 0xFBuy)
        let text = Colors.Gray, Color.FromRgb(0xF0uy, 0xF0uy, 0xF0uy)
        let list = Colors.SeaGreen, Color.FromRgb(0xDEuy, 0xF2uy, 0xE3uy)
        let reference = Colors.MediumPurple, Color.FromRgb(0xEBuy, 0xE3uy, 0xF7uy)
        let table = Colors.DarkOrange, Color.FromRgb(0xFDuy, 0xEAuy, 0xD3uy)
        let figure = Colors.IndianRed, Color.FromRgb(0xFBuy, 0xE1uy, 0xE1uy)
        dict
            [ for tag in [ "H1"; "H2"; "H3"; "H4"; "H5"; "H6" ] -> tag, heading
              for tag in [ "P"; "Span" ] -> tag, text
              for tag in [ "L"; "LI"; "Lbl"; "LBody" ] -> tag, list
              for tag in [ "TOC"; "TOCI"; "Link"; "Form" ] -> tag, reference
              for tag in [ "Table"; "TR"; "TH"; "TD" ] -> tag, table
              yield "Figure", figure ]

    let colorsFor (tag: string) =
        match categoryColors.TryGetValue tag with
        | true, colors -> colors
        | _ -> Colors.DarkGray, Color.FromRgb(0xEEuy, 0xEEuy, 0xEEuy)

    let brush (color: Color) : IBrush = SolidColorBrush(color)

    /// Called with the clicked block's node and its border control.
    type Selection = { Select: StructureNode -> Border -> unit }

    let detailLine (text: string) (italic: bool) =
        TextBlock.create [
            TextBlock.text text
            TextBlock.fontSize 11.0
            if italic then TextBlock.fontStyle FontStyle.Italic
            TextBlock.foreground Brushes.DimGray
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.margin (0.0, 0.0, 0.0, 4.0)
        ]
        :> IView

    let tagChip (tag: string) (borderColor: Color) (headerColor: Color) =
        Border.create [
            Border.background (brush headerColor)
            Border.borderBrush (brush borderColor)
            Border.borderThickness 1.0
            Border.cornerRadius 3.0
            Border.padding (5.0, 1.0, 5.0, 1.0)
            Border.margin (0.0, 0.0, 6.0, 0.0)
            Border.child (
                TextBlock.create [
                    TextBlock.text tag
                    TextBlock.fontFamily Fonts.Monospace
                    TextBlock.fontWeight FontWeight.Bold
                    TextBlock.fontSize 12.0
                ]
            )
        ]

    /// A bordered, clickable block. Selection restyles the clicked border directly
    /// (see StructureTreeWindow's view) rather than re-rendering the whole layout.
    let selectableBorder
        (selection: Selection)
        (node: StructureNode)
        (borderColor: Color)
        (thickness: float)
        (attrs: IAttr<Border> list)
        (tip: string)
        =
        Border.create [
            yield! attrs
            Border.borderBrush (brush borderColor)
            Border.borderThickness thickness
            Border.tag node
            ToolTip.tip tip
            Border.onPointerPressed (fun e ->
                match e.Source with
                // FuncUI also subscribes to the tunnel phase, where the outermost block would
                // see the click first; only the innermost block (bubble phase) should select.
                | :? Visual as source when
                    e.Route = Interactivity.RoutingStrategies.Bubble
                    && e.GetCurrentPoint(source).Properties.IsLeftButtonPressed
                    ->
                    // This handler's own border: the nearest border tagged with this node.
                    let owner =
                        source.GetSelfAndVisualAncestors()
                        |> Seq.pick (function
                            | :? Border as border when obj.ReferenceEquals(border.Tag, node) -> Some border
                            | _ -> None)
                    e.Handled <- true
                    selection.Select node owner
                | _ -> ())
        ]

    let geometryTip (node: StructureNode) (what: string) (noun: string) =
        match node.Geometry with
        | null -> $"Select: no page geometry is available for this {noun}."
        | geometry -> $"Select: highlight this {what} on page {geometry.PageNumber}."

    /// Recursively renders one structure node as an HTML-like bordered block: a colored
    /// tag "chip" header (color = semantic category), an attributes line, optional
    /// alt-text, the element's own extracted content text (if any), and nested child
    /// boxes indented inside — so nesting depth is visually obvious, matching how a
    /// browser's block-level box model would draw it.
    let rec element (selection: Selection) (margin: Thickness) (node: StructureNode) : IView =
        let borderColor, headerColor = colorsFor node.Tag

        let attributes =
            [ match node.Role with NonNull role -> yield $"role={role}" | Null -> ()
              if node.PageNumber.HasValue then yield $"p.{node.PageNumber.Value}"
              match node.Language with NonNull lang -> yield $"[{lang}]" | Null -> ()
              match node.FormFieldType with NonNull fieldType -> yield $"field type: {fieldType}" | Null -> ()
              match node.FormFieldName with NonNull name -> yield $"field: {name}" | Null -> ()
              if node.LinkDestinationPageNumber.HasValue then yield $"links to p.{node.LinkDestinationPageNumber.Value}" ]

        let header =
            WrapPanel.create [
                WrapPanel.margin (0.0, 0.0, 0.0, 4.0)
                WrapPanel.children [
                    tagChip node.Tag borderColor headerColor
                    if not attributes.IsEmpty then
                        TextBlock.create [
                            TextBlock.text (String.Join("  ", attributes))
                            TextBlock.fontSize 11.0
                            TextBlock.foreground Brushes.DimGray
                            TextBlock.verticalAlignment VerticalAlignment.Center
                        ]
                ]
            ]

        let linkTarget: string | null =
            match node.LinkUri with
            | NonNull uri -> uri
            | Null -> node.LinkDestinationDescription

        let children: IView list =
            match node.Tag with
            | "TOC" -> [ tocGrid selection node ]
            | "Table" -> [ tableGrid selection node ]
            | _ -> [ for child in node.Children -> element selection (Thickness(0.0, 0.0, 0.0, 6.0)) child ]

        let inner =
            StackPanel.create [
                StackPanel.children [
                    header
                    match node.AlternateDescription with NonNull alt -> detailLine $"alt: \"{alt}\"" true | Null -> ()
                    match node.FormFieldDescription with NonNull tooltip -> detailLine $"tooltip: \"{tooltip}\"" true | Null -> ()
                    match node.FormFieldValue with NonNull value -> detailLine $"value: \"{value}\"" false | Null -> ()
                    match linkTarget with NonNull target -> detailLine $"↳ {target}" false | Null -> ()
                    match node.Content |> Option.ofObj |> Option.orElse (Option.ofObj node.ActualText) with
                    | Some text ->
                        TextBlock.create [
                            TextBlock.text text
                            TextBlock.fontSize 13.0
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.margin (0.0, 0.0, 0.0, 4.0)
                        ]
                    | None -> ()
                    yield! children
                ]
            ]

        selectableBorder
            selection
            node
            borderColor
            1.5
            [ Border.cornerRadius 4.0
              Border.padding 8.0
              Border.margin margin
              Border.background Brushes.White
              Border.child inner ]
            (geometryTip node "node" "structure node")

    and fallbackStack (selection: Selection) (node: StructureNode) : IView =
        StackPanel.create [
            StackPanel.children [ for child in node.Children -> element selection (Thickness(0.0, 0.0, 0.0, 6.0)) child ]
        ]

    and tocCell (selection: Selection) (item: StructureNode) row column (attrs: IAttr<Border> list) : IView =
        let borderColor, _ = colorsFor "TOCI"
        selectableBorder
            selection
            item
            borderColor
            1.0
            [ Border.padding (6.0, 4.0, 6.0, 4.0)
              Border.margin ((if column = 0 then 0.0 else -1.0), (if row = 0 then 0.0 else -1.0), 0.0, 0.0)
              Border.background Brushes.White
              Grid.row row
              Grid.column column
              yield! attrs ]
            (geometryTip item "TOC item" "TOC item")

    /// Renders a TOC as a grid: one row per TOCI, with the TOCI tag in the first
    /// column and each of its children in the following columns.
    and tocGrid (selection: Selection) (toc: StructureNode) : IView =
        let items = toc.Children |> Seq.filter (fun child -> child.Tag = "TOCI") |> List.ofSeq
        if items.IsEmpty then
            fallbackStack selection toc
        else
            let columnCount = items |> List.map (fun item -> item.Children.Count + 1) |> List.max
            Grid.create [
                Grid.margin (0.0, 4.0, 0.0, 0.0)
                Grid.background (brush (Color.FromRgb(0xF8uy, 0xF5uy, 0xFCuy)))
                Grid.columnDefinitions (String.Join(",", "Auto" :: List.replicate (columnCount - 1) "*"))
                Grid.rowDefinitions (String.Join(",", List.replicate items.Length "Auto"))
                Grid.children [
                    for row, item in List.indexed items do
                        let _, headerColor = colorsFor "TOCI"
                        tocCell selection item row 0 [
                            Border.background (brush headerColor)
                            Border.child (
                                TextBlock.create [
                                    TextBlock.text "TOCI"
                                    TextBlock.fontFamily Fonts.Monospace
                                    TextBlock.fontWeight FontWeight.Bold
                                    TextBlock.fontSize 12.0
                                    TextBlock.foreground Brushes.Black
                                    TextBlock.verticalAlignment VerticalAlignment.Top
                                ]
                            )
                        ]

                        for index, child in Seq.indexed item.Children do
                            tocCell selection child row (index + 1) [
                                Border.child (element selection (Thickness 0.0) child)
                            ]

                        if item.Children.Count + 1 < columnCount then
                            tocCell selection item row (item.Children.Count + 1) [
                                Grid.columnSpan (columnCount - item.Children.Count - 1)
                            ]
                ]
            ]

    /// Renders semantic PDF tables as a visual grid instead of a vertical stack:
    /// Table -> TR elements become rows, and TH/TD children become cells. This keeps
    /// the exported/tree semantics unchanged while making the layout tab much closer
    /// to how an HTML table with visible borders would look.
    and tableGrid (selection: Selection) (table: StructureNode) : IView =
        let tableCells (row: StructureNode) =
            match row.Children |> Seq.filter (fun child -> child.Tag = "TH" || child.Tag = "TD") |> List.ofSeq with
            | [] -> List.ofSeq row.Children
            | cells -> cells

        let rows = table.Children |> Seq.filter (fun child -> child.Tag = "TR") |> List.ofSeq
        if rows.IsEmpty then
            fallbackStack selection table
        else
            let columnCount = max 1 (rows |> List.map (tableCells >> List.length) |> List.max)
            Grid.create [
                Grid.margin (0.0, 4.0, 0.0, 0.0)
                Grid.background (brush (Color.FromRgb(0xFFuy, 0xFAuy, 0xF3uy)))
                Grid.columnDefinitions (String.Join(",", List.replicate columnCount "*"))
                Grid.rowDefinitions (String.Join(",", List.replicate rows.Length "Auto"))
                Grid.children [
                    for rowIndex, row in List.indexed rows do
                        match tableCells row with
                        | [] ->
                            ContentControl.create [
                                Grid.row rowIndex
                                Grid.columnSpan columnCount
                                ContentControl.content (element selection (Thickness(0.0, 0.0, 0.0, 6.0)) row)
                            ]
                        | cells ->
                            for columnIndex, cell in List.indexed cells do
                                ContentControl.create [
                                    Grid.row rowIndex
                                    Grid.column columnIndex
                                    ContentControl.content (element selection (Thickness 1.0) cell)
                                ]
                ]
            ]

    let view (window: Window) (root: StructureNode) (sourceFilePath: string) =
        Component(fun ctx ->
            // The highlighted border and the style to restore when the selection moves.
            let highlighted = ctx.useState ((None: (Border * (IBrush | null) * Thickness) option), false)
            let previewImage = ctx.useState (None: Bitmap option)
            let previewNote =
                ctx.useState "Select a structure block to highlight its best-effort page geometry when available."
            let renderer = PdfRegionRenderer()

            let select (node: StructureNode) (border: Border) =
                // Restyle just the two affected borders; re-rendering the layout would
                // re-diff every block, which is slow for large documents.
                match highlighted.Current with
                | Some(previous, previousBrush, previousThickness) ->
                    previous.BorderBrush <- previousBrush
                    previous.BorderThickness <- previousThickness
                | None -> ()
                highlighted.Set(Some(border, border.BorderBrush, border.BorderThickness))
                border.BorderBrush <- Brushes.DodgerBlue
                border.BorderThickness <- Thickness 3.0

                task {
                    previewImage.Set None
                    match node.Geometry with
                    | null ->
                        previewNote.Set(
                            $"Selected <{node.Tag}>. No same-page geometry is available for this node "
                            + "(it may be a pure container, non-text content without a rectangle, or span multiple pages)."
                        )
                    | geometry ->
                        previewNote.Set $"Rendering page {geometry.PageNumber} highlight for <{node.Tag}>…"
                        try
                            let! result = Task.Run(fun () -> renderer.RenderPageWithHighlight(sourceFilePath, geometry))
                            use stream = new MemoryStream(result.PngBytes)
                            previewImage.Set(Some(new Bitmap(stream)))
                            previewNote.Set $"{result.Note} Page {result.PageNumber}."
                        with :? InvalidOperationException | :? IOException as ex ->
                            previewNote.Set $"Could not render highlight for <{node.Tag}>: {ex.Message}"
                }
                |> ignore

            let selection = { Select = select }

            let exportJson _ =
                task {
                    let! path =
                        Dialogs.pickSavePath
                            window
                            (FilePickerSaveOptions(
                                Title = "Export structure as JSON",
                                SuggestedFileName = $"{Path.GetFileNameWithoutExtension sourceFilePath}-structure.json",
                                DefaultExtension = "json",
                                FileTypeChoices = [ FilePickerFileTypes.Json; FilePickerFileTypes.All ]
                            ))
                            sourceFilePath
                    match path with
                    | Some path -> File.WriteAllText(path, JsonSerializer.Serialize(root, jsonOptions))
                    | None -> ()
                }
                |> ignore

            // The synthetic StructTreeRoot node itself carries no semantic info, so show
            // its children (the actual document hierarchy) as the top level in both views.
            let layoutTab =
                Grid.create [
                    Grid.columnDefinitions [
                        ColumnDefinition(GridLength(3.0, GridUnitType.Star), MinWidth = 320.0)
                        ColumnDefinition(GridLength 6.0)
                        ColumnDefinition(GridLength(2.0, GridUnitType.Star), MinWidth = 260.0)
                    ]
                    Grid.children [
                        // Its own component, so preview updates don't re-render (and re-diff) the layout.
                        Component.create("structure-layout", fun _ ->
                        ScrollViewer.create [
                            ScrollViewer.verticalScrollBarVisibility Primitives.ScrollBarVisibility.Auto
                            ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Disabled
                            ScrollViewer.background Brushes.White
                            ScrollViewer.content (
                                StackPanel.create [
                                    StackPanel.margin 8.0
                                    StackPanel.children [
                                        for child in root.Children -> element selection (Thickness(0.0, 0.0, 0.0, 6.0)) child
                                    ]
                                ]
                            )
                        ])
                        GridSplitter.create [
                            Grid.column 1
                            GridSplitter.width 6.0
                            GridSplitter.horizontalAlignment HorizontalAlignment.Stretch
                            GridSplitter.background "#FFE0E0E0"
                            GridSplitter.resizeDirection GridResizeDirection.Columns
                        ]
                        Grid.create [
                            Grid.column 2
                            Grid.margin (10.0, 0.0, 0.0, 0.0)
                            Grid.rowDefinitions "Auto,Auto,*"
                            Grid.children [
                                TextBlock.create [
                                    TextBlock.text "Selected block geometry"
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.margin (0.0, 0.0, 0.0, 6.0)
                                ]
                                TextBlock.create [
                                    Grid.row 1
                                    TextBlock.text previewNote.Current
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    TextBlock.foreground "#FF555555"
                                    TextBlock.margin (0.0, 0.0, 0.0, 8.0)
                                ]
                                Border.create [
                                    Grid.row 2
                                    Border.borderBrush "#FFD0D0D0"
                                    Border.borderThickness 1.0
                                    Border.background "#FFF5F5F5"
                                    Border.padding 6.0
                                    Border.child (
                                        Image.create [
                                            match previewImage.Current with
                                            | Some image -> Image.source image
                                            | None -> ()
                                            Image.stretch Stretch.Uniform
                                        ]
                                    )
                                ]
                            ]
                        ]
                    ]
                ]

            let treeTab =
                TreeView.create [
                    TreeView.dataItems root.Children
                    TreeView.itemTemplate (
                        DataTemplateView<_>.create (
                            Func<StructureNode, StructureNode seq>(fun node -> node.Children),
                            fun node ->
                                TextBlock.create [
                                    TextBlock.text (StructureSummary.format node)
                                    TextBlock.fontFamily Fonts.Monospace
                                    TextBlock.fontSize 12.0
                                ]
                        )
                    )
                ]

            Grid.create [
                Grid.margin 12.0
                Grid.rowDefinitions "Auto,*,Auto"
                Grid.children [
                    TextBlock.create [
                        TextBlock.margin (0.0, 0.0, 0.0, 8.0)
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.foreground "#FF555555"
                        TextBlock.text
                            "This is the tagged logical structure tree — the semantic heading/paragraph/list/table/link hierarchy that determines what content and reading order a screen reader presents to a user. Only elements that directly own page content show a page number."
                    ]
                    TabControl.create [
                        Grid.row 1
                        TabControl.viewItems [
                            TabItem.create [ TabItem.header "Layout"; TabItem.content layoutTab ]
                            TabItem.create [ TabItem.header "Tree"; TabItem.content treeTab ]
                        ]
                    ]
                    StackPanel.create [
                        Grid.row 2
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.horizontalAlignment HorizontalAlignment.Right
                        StackPanel.margin (0.0, 8.0, 0.0, 0.0)
                        StackPanel.children [
                            Button.create [
                                Button.content "Export JSON…"
                                Button.padding (12.0, 4.0)
                                Button.margin (0.0, 0.0, 8.0, 0.0)
                                ToolTip.tip "Save this hierarchy as JSON — useful for feeding to an AI agent or other tooling."
                                Button.onClick exportJson
                            ]
                            Button.create [
                                Button.content "Close"
                                Button.padding (12.0, 4.0)
                                Button.isCancel true
                                Button.onClick (fun _ -> window.Close())
                            ]
                        ]
                    ]
                ]
            ])

open StructureView

type StructureTreeWindow(root: StructureNode, sourceFilePath: string) as this =
    inherit HostWindow(Title = "Document Structure", Width = 820.0, Height = 700.0, MinWidth = 500.0, MinHeight = 400.0)
    do this.Content <- view this root sourceFilePath
