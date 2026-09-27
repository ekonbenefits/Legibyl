namespace Legibyl.Gui

open System
open System.IO
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform.Storage
open Avalonia.VisualTree
open Legibyl.Layout
open Legibyl.Rendering
open Legibyl.Validation

/// What View Structure shows: a tagged PDF's structure, or for an untagged one, the layout
/// suggested for it (found page by page as the window is open), with the review's edits on it.
type StructureSource =
    | Tagged of StructureNode
    | Suggested

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
              match node.LinkDescription with NonNull description -> yield $"description: \"{description}\"" | Null -> ()
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

    /// A tag's outline color on a page overview (0xRRGGBB), strong enough to see on a page.
    let outlineColor (tag: string) =
        match tag with
        | "H"
        | "H1"
        | "H2"
        | "H3"
        | "H4"
        | "H5"
        | "H6" -> 0xC2185Bu
        | "P"
        | "Span" -> 0x1A73E8u
        | "L"
        | "LI"
        | "Lbl"
        | "LBody" -> 0x00838Fu
        | "Figure" -> 0x2E7D32u
        | "Table"
        | "TR"
        | "TH"
        | "TD" -> 0xEF6C00u
        | "Artifact" -> 0x757575u
        | _ -> 0x6A1B9Au

    /// The elements a page's overview outlines, in reading order: those that own content on it,
    /// outermost first (one inside another that does is within that one's outline).
    let pageElements (root: StructureNode) (page: int) : StructureNode list =
        let owns (node: StructureNode) =
            node.ContentLeafCount > 0
            && (match node.Geometry with
                | NonNull geometry -> geometry.PageNumber = page
                | Null -> false)
        let rec collect (node: StructureNode) : StructureNode list =
            if owns node then [ node ] else [ for child in node.Children do yield! collect child ]
        [ for child in root.Children do yield! collect child ]

    /// The first page node's content is on: its own, else its first descendant's.
    let rec firstPage (node: StructureNode) : int option =
        match node.Geometry with
        | NonNull geometry -> Some geometry.PageNumber
        | Null -> node.Children |> Seq.tryPick firstPage

    /// What can be edited on an element: its alt text, or its replacement text (/ActualText).
    type EditKind =
        | AltText
        | ReplacementText
        /// A link's description (its annotation's /Contents).
        | LinkDescription

    /// What blocks can do: select (called with the clicked block's node, its border control, and
    /// whether to extend the selection to it, as shift-click does), show the user's edits, and edit
    /// an element.
    type Selection =
        { Select: StructureNode -> Border -> bool -> unit
          Edits: IReadable<RegionEdits.Edit list>
          Edit: StructureNode -> EditKind -> unit
          /// Whether it's a suggested layout, which nothing is in yet (not a tagged PDF).
          Suggested: bool }

    /// The edits an element can take. Alt text: figures and formulas, and anything that already
    /// has alt text. Replacement text: only what already has it (like OCR'd text), since writing
    /// it over real text would hide what the page says. Either needs the element's place on its
    /// page, which is how an edit finds it again.
    let editKinds (node: StructureNode) =
        match node.Geometry with
        | null -> []
        | _ ->
            [ if node.Tag = "Link" then LinkDescription
              elif node.Tag = "Figure" || node.Tag = "Formula" || not (isNull node.AlternateDescription) then AltText
              if not (isNull node.ActualText) then ReplacementText ]

    /// The tags a block of text can be given instead, e.g. a heading tagged as a paragraph.
    let retagTags = [ "H1"; "H2"; "H3"; "H4"; "H5"; "H6"; "P"; "Caption"; "Note"; "BlockQuote" ]

    /// Whether node can be given another tag: in a tagged PDF, a block of text; in a suggested
    /// layout, any region (it needs its place on its page, which is how the edit finds it again).
    let canRetag (suggested: bool) (node: StructureNode) =
        not (isNull node.Geometry)
        && (if suggested then node.Tag <> "Link" else node.Tag = "H" || List.contains node.Tag retagTags)

    /// The suggested regions in the selection: those selected, and those inside a selected list
    /// or document, to leave out or keep.
    let regionsIn (nodes: StructureNode list) =
        let rec regions (node: StructureNode) : StructureNode list =
            if node.Tag = "Link" then []
            elif not (isNull node.Geometry) then [ node ]
            else [ for child in node.Children do yield! regions child ]
        nodes |> List.collect regions

    /// The user's edit for node, if any.
    let editFor (edits: RegionEdits.Edit list) (node: StructureNode) =
        match node.Geometry with
        | null -> None
        | geometry -> RegionEdits.find edits geometry.PageNumber node.Tag geometry

    /// For a paragraph: how it's in a merge the user chose, if it is (merges are only ever not in
    /// the PDF yet where the paragraphs are still apart, as they are wherever a block shows them).
    let mergeLine (edits: RegionEdits.Edit list) (node: StructureNode) =
        match node.Geometry with
        | NonNull box when node.Tag = "P" ->
            match RegionEdits.findMerge edits box.PageNumber box with
            | Some merge ->
                let others = merge.MergeWith.Length
                Some(if others = 1 then "✎ merged with the next paragraph, not in the PDF yet" else $"✎ merged with the next {others} paragraphs, not in the PDF yet")
            | None ->
                let merged =
                    edits
                    |> List.exists (fun e ->
                        e.Page = box.PageNumber && e.MergeWith |> List.exists (fun other -> RegionEdits.overlap other box >= RegionEdits.SameRegionOverlap))
                if merged then Some "✎ merged into the paragraph before, not in the PDF yet" else None
        | _ -> None

    /// The selected blocks, if they're paragraphs that can be merged: two or more on one page,
    /// none with alt text or replacement text (merging would lose them).
    let mergeable (nodes: StructureNode list) =
        match nodes with
        | first :: _ :: _ when
            nodes
            |> List.forall (fun n ->
                n.Tag = "P"
                && isNull n.AlternateDescription
                && isNull n.ActualText
                && (match n.Geometry, first.Geometry with
                    | NonNull box, NonNull firstBox -> box.PageNumber = firstBox.PageNumber
                    | _ -> false))
            ->
            Some nodes
        | _ -> None

    /// What of edit isn't in the element yet: (label, text) lines.
    let pendingLines (edit: RegionEdits.Edit) (node: StructureNode) =
        [ match edit.Retag with
          | Some tag when tag <> node.Tag -> "tag", tag
          | _ -> ()
          match edit.Alt with
          // A link's is its description.
          | Some alt when node.Tag = "Link" && alt <> string node.LinkDescription -> "description", alt
          | Some alt when node.Tag <> "Link" && alt <> string node.AlternateDescription -> "alt", alt
          | _ -> ()
          match edit.Text with
          | Some text when text <> string node.ActualText -> "text", text
          | _ -> () ]

    let detailLine (text: string) (italic: bool) =
        TextBlock.create [
            TextBlock.text text
            TextBlock.fontSize 11.0
            if italic then TextBlock.fontStyle FontStyle.Italic
            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
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
                    // The chip's pastel background is the same in light and dark mode.
                    TextBlock.foreground Brushes.Black
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
            match editKinds node with
            | [] -> ()
            | kinds ->
                Border.contextMenu (
                    ContextMenu.create [
                        ContextMenu.viewItems [
                            for kind in kinds ->
                                MenuItem.create [
                                    MenuItem.header (
                                        match kind with
                                        | AltText -> "Edit Alt Text…"
                                        | ReplacementText -> "Edit Replacement Text…"
                                        | LinkDescription -> "Edit Link Description…"
                                    )
                                    MenuItem.onClick (fun _ -> selection.Edit node kind)
                                ]
                        ]
                    ]
                )
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
                    selection.Select node owner (e.KeyModifiers.HasFlag KeyModifiers.Shift)
                | _ -> ())
        ]

    let geometryTip (node: StructureNode) (what: string) (noun: string) =
        match node.Geometry with
        | null -> $"Select: no page geometry is available for this {noun}."
        | geometry -> $"Select: highlight this {what} on page {geometry.PageNumber}."

    /// Blocks the Layout view draws up front. Big documents' deeper levels start collapsed
    /// (click to expand) so the window opens quickly; most documents fit and show in full.
    let private layoutBudget = 2000

    /// How many levels of nodes, starting with `nodes` themselves, fit in the budget. Whole
    /// levels only, so siblings are always shown or collapsed together. At least 1.
    let levelsWithinBudget (nodes: StructureNode seq) =
        let rec fit levels (level: StructureNode list) total =
            let next = level |> List.collect (fun node -> List.ofSeq node.Children)
            if next.IsEmpty || total + next.Length > layoutBudget then levels
            else fit (levels + 1) next (total + next.Length)
        let first = List.ofSeq nodes
        fit 1 first first.Length

    let rec private descendantCount (node: StructureNode) : int =
        node.Children |> Seq.sumBy (fun child -> 1 + descendantCount child)

    /// Recursively renders one structure node as an HTML-like bordered block: a colored
    /// tag "chip" header (color = semantic category), an attributes line, optional
    /// alt-text, the element's own extracted content text (if any), and nested child
    /// boxes indented inside — so nesting depth is visually obvious, matching how a
    /// browser's block-level box model would draw it.
    /// childLevels is how many levels of descendants to draw; below that, children start
    /// collapsed behind a button.
    let rec element (selection: Selection) (margin: Thickness) (childLevels: int) (node: StructureNode) : IView =
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
                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                            TextBlock.verticalAlignment VerticalAlignment.Center
                        ]
                ]
            ]

        let linkTarget: string | null =
            match node.LinkUri with
            | NonNull uri -> uri
            | Null -> node.LinkDestinationDescription

        let children: IView list =
            if node.Children.Count = 0 then []
            elif childLevels <= 0 then [ collapsedChildren selection node ]
            else
                match node.Tag with
                | "TOC" -> [ tocGrid selection node ]
                | "Table" -> [ tableGrid selection node ]
                | _ -> [ for child in node.Children -> element selection (Thickness(0.0, 0.0, 0.0, 6.0)) (childLevels - 1) child ]

        let inner =
            StackPanel.create [
                StackPanel.children [
                    header
                    match node.AlternateDescription with NonNull alt -> detailLine $"alt: \"{alt}\"" true | Null -> ()
                    match node.LinkDescription with NonNull description -> detailLine $"description: \"{description}\"" true | Null -> ()
                    if not (editKinds node).IsEmpty || canRetag selection.Suggested node then
                        // Its own component, so an edit re-renders just this line.
                        Component.create("pending-edit", fun ctx ->
                            let edits = ctx.usePassedRead selection.Edits
                            let lines =
                                editFor edits.Current node |> Option.map (fun edit -> pendingLines edit node) |> Option.defaultValue []
                            StackPanel.create [
                                StackPanel.children [
                                    for label, text in lines do
                                        if selection.Suggested then detailLine $"✎ your {label}: \"{text}\"" true
                                        else detailLine $"✎ your {label}, not in the PDF yet: \"{text}\"" true
                                    if editFor edits.Current node |> Option.exists _.Exclude then
                                        detailLine "✎ left out: not tagged (its text joins the nearest region kept)" true
                                    match mergeLine edits.Current node with
                                    | Some line -> detailLine line true
                                    | None -> ()
                                ]
                            ])
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
              Theme.resources [ Border.BackgroundProperty, Theme.Tokens.Background ]
              Border.child inner ]
            (geometryTip node "node" "structure node")

    /// A node's children, drawn only once asked for: "▸ 31 child elements (824 in all)".
    /// Its own component, so expanding re-renders just this block, not the whole layout.
    and collapsedChildren (selection: Selection) (node: StructureNode) : IView =
        Component.create("collapsed-children", fun ctx ->
            let expanded = ctx.useState false
            if expanded.Current then
                let levels = levelsWithinBudget node.Children
                StackPanel.create [
                    StackPanel.children [
                        for child in node.Children -> element selection (Thickness(0.0, 0.0, 0.0, 6.0)) (levels - 1) child
                    ]
                ]
            else
                let count = node.Children.Count
                let total = descendantCount node
                let label = if count = 1 then "▸ 1 child element" else $"▸ {count:N0} child elements"
                Button.create [
                    Button.content (if total > count then $"{label} ({total:N0} in all)" else label)
                    Button.fontSize 12.0
                    Button.padding (8.0, 2.0)
                    Button.margin (0.0, 2.0, 0.0, 2.0)
                    Button.onClick (fun _ -> expanded.Set true)
                ])

    and fallbackStack (selection: Selection) (node: StructureNode) : IView =
        StackPanel.create [
            StackPanel.children [ for child in node.Children -> element selection (Thickness(0.0, 0.0, 0.0, 6.0)) Int32.MaxValue child ]
        ]

    and tocCell (selection: Selection) (item: StructureNode) row column (background: IBrush option) (attrs: IAttr<Border> list) : IView =
        let borderColor, _ = colorsFor "TOCI"
        selectableBorder
            selection
            item
            borderColor
            1.0
            [ Border.padding (6.0, 4.0, 6.0, 4.0)
              Border.margin ((if column = 0 then 0.0 else -1.0), (if row = 0 then 0.0 else -1.0), 0.0, 0.0)
              match background with
              | Some background -> Border.background background
              | None -> Theme.resources [ Border.BackgroundProperty, Theme.Tokens.Background ]
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
                Theme.resources [ Grid.BackgroundProperty, Theme.Tokens.Fill ]
                Grid.columnDefinitions (String.Join(",", "Auto" :: List.replicate (columnCount - 1) "*"))
                Grid.rowDefinitions (String.Join(",", List.replicate items.Length "Auto"))
                Grid.children [
                    for row, item in List.indexed items do
                        let _, headerColor = colorsFor "TOCI"
                        tocCell selection item row 0 (Some(brush headerColor)) [
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
                            tocCell selection child row (index + 1) None [
                                Border.child (element selection (Thickness 0.0) Int32.MaxValue child)
                            ]

                        if item.Children.Count + 1 < columnCount then
                            tocCell selection item row (item.Children.Count + 1) None [
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
                Theme.resources [ Grid.BackgroundProperty, Theme.Tokens.Fill ]
                Grid.columnDefinitions (String.Join(",", List.replicate columnCount "*"))
                Grid.rowDefinitions (String.Join(",", List.replicate rows.Length "Auto"))
                Grid.children [
                    for rowIndex, row in List.indexed rows do
                        match tableCells row with
                        | [] ->
                            ContentControl.create [
                                Grid.row rowIndex
                                Grid.columnSpan columnCount
                                ContentControl.content (element selection (Thickness(0.0, 0.0, 0.0, 6.0)) Int32.MaxValue row)
                            ]
                        | cells ->
                            for columnIndex, cell in List.indexed cells do
                                ContentControl.create [
                                    Grid.row rowIndex
                                    Grid.column columnIndex
                                    ContentControl.content (element selection (Thickness 1.0) Int32.MaxValue cell)
                                ]
                ]
            ]

    /// A suggested region left out is outlined grey.
    let leftOutColor = 0x9E9E9Eu

    /// Edits change review (Save in the main window keeps it); onEditsChanged is called after each
    /// (so the main window can count them). For a suggested layout, prefetcher finds its pages and
    /// tagged is unused.
    let view
        (window: Window)
        (renderer: PdfRegionRenderer)
        (tagged: StructureNode)
        (review: DocumentReview)
        (prefetcher: LayoutPrefetcher option)
        (sourceFilePath: string)
        (onEditsChanged: unit -> unit)
        =
        Component(fun ctx ->
            let suggested = prefetcher |> Option.map (fun prefetcher -> review, prefetcher)
            let isSuggested = suggested.IsSome
            // A suggested layout's pages as elements, by page: each built once, so a selection
            // stays with its elements as more pages are found.
            let pageNodes = ctx.useState (Collections.Generic.Dictionary<int, StructureNode list>(), false)
            let buildRoot () =
                match suggested with
                | Some(review, _) ->
                    for KeyValue(number, page) in review.Pages do
                        if not (pageNodes.Current.ContainsKey number) then
                            pageNodes.Current[number] <- SuggestedTree.pageNodes review page
                    SuggestedTree.build [ for number in Seq.sort pageNodes.Current.Keys -> pageNodes.Current[number] ]
                | None -> tagged
            let root = ctx.useStateLazy buildRoot
            // The highlighted borders and the styles to restore when the selection moves.
            let highlighted = ctx.useState (([]: (Border * (IBrush | null) * Thickness) list), false)
            // The selected blocks' nodes, in document order, and the one a shift-click extends from.
            let selected = ctx.useState ([]: StructureNode list)
            let anchor = ctx.useState ((None: StructureNode option), false)
            // Each node's parent, to extend a selection over its siblings (worked out again when a
            // suggested layout's root changes).
            let parentsCache =
                ctx.useState ((None: (StructureNode * Collections.Generic.Dictionary<StructureNode, StructureNode>) option), false)
            let parents () =
                match parentsCache.Current with
                | Some(forRoot, parents) when obj.ReferenceEquals(forRoot, root.Current) -> parents
                | _ ->
                    let parents = Collections.Generic.Dictionary<StructureNode, StructureNode>(HashIdentity.Reference)
                    let rec add (node: StructureNode) =
                        for child in node.Children do
                            parents[child] <- node
                            add child
                    add root.Current
                    parentsCache.Set(Some(root.Current, parents))
                    parents
            // The user's edits for this PDF: its review's.
            let edits = ctx.useState review.Edits
            let modelReady = ctx.useState (not isSuggested || LayoutModel.isDownloaded ())
            // The page the preview shows, if any.
            let shownPage = ctx.useState ((None: int option), false)
            let previewImage = ctx.useState (None: Bitmap option)
            // For a link: the page it goes to, with its destination marked in blue.
            let targetImage = ctx.useState (None: Bitmap option)
            let targetNote = ctx.useState ""
            let previewNote =
                ctx.useState "Select an element to highlight where it is on its page, when that's known."
            // Bumped on every selection so a slower preview for an earlier one can't overwrite it.
            let previewVersion = ctx.useState (0, false)

            /// Shows the page the selection is on, every element on it outlined and numbered in
            /// reading order, the selected ones (or those inside a selected container) filled; and
            /// for a link, the page it goes to. With nothing selected, the first page.
            let showPage (nodes: StructureNode list) =
                task {
                    previewVersion.Set(previewVersion.Current + 1)
                    let version = previewVersion.Current
                    let tree = root.Current
                    let currentEdits = edits.Current
                    previewImage.Set None
                    targetImage.Set None
                    targetNote.Set ""
                    let within (node: StructureNode) =
                        let rec up (current: StructureNode) depth =
                            nodes |> List.exists (fun s -> obj.ReferenceEquals(s, current))
                            || (depth < 200 && (match (parents ()).TryGetValue current with | true, parent -> up parent (depth + 1) | _ -> false))
                        up node 0
                    let page =
                        match nodes |> List.tryPick firstPage with
                        | Some page -> Some page
                        | None when nodes.IsEmpty -> tree.Children |> Seq.tryPick firstPage
                        | None -> None
                    shownPage.Set page
                    let destination =
                        match nodes with
                        | [ node ] -> Option.ofObj node.LinkDestination
                        | _ -> None
                    match page with
                    | None ->
                        previewNote.Set(
                            match nodes with
                            | [] when isSuggested && not modelReady.Current -> "Each page's layout is found once the layout model is downloaded (above)."
                            | [] when isSuggested -> "Finding each page's layout… (several seconds a page)"
                            | [ node ] ->
                                $"Selected <{node.Tag}>. It has no place on a page to show (a container without content, or one across pages)."
                            | _ -> "Nothing here has a place on a page to show."
                        )
                    | Some page -> previewNote.Set $"Rendering page {page}…"
                    if page.IsSome || destination.IsSome then
                        try
                            // Render and decode off the UI thread; a full page is a large image.
                            let! source, target =
                                Task.Run(fun () ->
                                    let decode (bytes: byte array) =
                                        use stream = new MemoryStream(bytes)
                                        new Bitmap(stream)
                                    page
                                    |> Option.map (fun page ->
                                        let elements = pageElements tree page
                                        // As edited: given another tag, or left out.
                                        let outlined =
                                            [ for i, element in List.indexed elements ->
                                                  let edit = editFor currentEdits element
                                                  let tag = edit |> Option.bind _.Retag |> Option.defaultValue element.Tag
                                                  let leftOut = edit |> Option.exists _.Exclude
                                                  { Geometry = nonNull element.Geometry
                                                    Label = if leftOut then $"× {i + 1} {tag}" else $"{i + 1} {tag}"
                                                    Color = if leftOut then leftOutColor else outlineColor tag
                                                    Emphasised = within element } ]
                                        // A selection that isn't one of them (a span in a paragraph, say) is filled too.
                                        let extra =
                                            [ for node in nodes do
                                                  match node.Geometry with
                                                  | NonNull geometry when geometry.PageNumber = page && not (elements |> List.exists (fun e -> obj.ReferenceEquals(e, node))) ->
                                                      { Geometry = geometry; Label = node.Tag; Color = outlineColor node.Tag; Emphasised = true }
                                                  | _ -> () ]
                                        let note =
                                            let count = elements.Length
                                            let what = if count = 1 then "its element" else $"its {count} elements"
                                            if nodes.IsEmpty then $"Page {page}: {what}, outlined and numbered in reading order."
                                            else $"Page {page}: {what}, outlined and numbered in reading order; the selection filled."
                                        page, decode (renderer.RenderPageWithRegions(sourceFilePath, page, outlined @ extra)), note),
                                    destination |> Option.map (fun d -> let result = renderer.RenderPageWithTarget(sourceFilePath, d) in result, decode result.PngBytes))
                            if version = previewVersion.Current then
                                match source with
                                | Some(_, image, note) ->
                                    previewImage.Set(Some image)
                                    previewNote.Set(if destination.IsSome then $"{note} Where the link goes is below." else note)
                                | None -> ()
                                match target with
                                | Some(result, image) ->
                                    targetImage.Set(Some image)
                                    targetNote.Set $"Where the link goes (marked in blue), page {result.PageNumber}: {result.Note}"
                                | None -> ()
                            else
                                source |> Option.iter (fun (_, image, _) -> image.Dispose())
                                target |> Option.iter (snd >> _.Dispose())
                        with :? InvalidOperationException | :? IOException as ex ->
                            if version = previewVersion.Current then
                                previewNote.Set $"Could not render the page: {ex.Message}"
                }
                |> ignore

            // The first page's overview, to start with.
            ctx.useEffect ((fun () -> showPage []), [ EffectTrigger.AfterInit ])

            let select (node: StructureNode) (border: Border) (extend: bool) =
                // Extending: every sibling from the anchor to this one (a shift-click on a block
                // with another parent selects just it).
                let nodes =
                    match anchor.Current with
                    | Some from when extend && not (obj.ReferenceEquals(from, node)) ->
                        match (parents ()).TryGetValue from, (parents ()).TryGetValue node with
                        | (true, parent), (true, other) when obj.ReferenceEquals(parent, other) ->
                            let siblings = List.ofSeq parent.Children
                            let i = siblings |> List.findIndex (fun s -> obj.ReferenceEquals(s, from))
                            let j = siblings |> List.findIndex (fun s -> obj.ReferenceEquals(s, node))
                            siblings[min i j .. max i j]
                        | _ -> [ node ]
                    | _ -> [ node ]
                if nodes.Length = 1 then anchor.Set(Some node)
                selected.Set nodes
                // Restyle just the affected borders; re-rendering the layout would re-diff every
                // block, which is slow for large documents.
                for previous, previousBrush, previousThickness in highlighted.Current do
                    previous.BorderBrush <- previousBrush
                    previous.BorderThickness <- previousThickness
                let borders =
                    if nodes.Length = 1 then
                        [ border ]
                    else
                        window.GetVisualDescendants()
                        |> Seq.choose (fun v ->
                            match v with
                            | :? Border as b when (match b.Tag with :? StructureNode as n -> nodes |> List.exists (fun s -> obj.ReferenceEquals(s, n)) | _ -> false) ->
                                Some b
                            | _ -> None)
                        |> List.ofSeq
                highlighted.Set [ for b in borders -> b, b.BorderBrush, b.BorderThickness ]
                for b in borders do
                    b.BorderBrush <- Brushes.DodgerBlue
                    b.BorderThickness <- Thickness 3.0

                showPage nodes

            /// Makes next the review's edits (Save in the main window keeps them).
            let saveEdits (next: RegionEdits.Edit list) =
                task {
                    review.Edits <- next
                    edits.Set next
                    onEditsChanged ()
                    // The overview labels elements as edited.
                    showPage selected.Current
                }

            /// What Export does with an edit.
            let savedHow =
                if isSuggested then "Export… in the main window tags a copy with it."
                else "Export… in the main window writes it to a copy."

            let editNode (node: StructureNode) (kind: EditKind) =
                match node.Geometry with
                | null -> ()
                | box ->
                    task {
                        let page = box.PageNumber
                        let existing = editFor edits.Current node
                        let title, prompt, current, placeholder =
                            match kind with
                            | AltText ->
                                "Alt Text",
                                $"Alt text for this <{node.Tag}> on page {page}: what a screen reader says for it. {savedHow}",
                                existing |> Option.bind _.Alt |> Option.defaultValue (string node.AlternateDescription),
                                "Describe what it shows"
                            | LinkDescription ->
                                let target = match node.LinkUri with NonNull uri -> $" (it goes to {uri})" | Null -> ""
                                "Link Description",
                                $"What a screen reader announces for this link on page {page}{target}: where it goes, e.g. \"Apply for benefits (example.com)\". {savedHow}",
                                existing |> Option.bind _.Alt |> Option.defaultValue (string node.LinkDescription),
                                "Where the link goes"
                            | ReplacementText ->
                                "Replacement Text",
                                $"What a screen reader reads for this <{node.Tag}> on page {page} (its /ActualText), in place of what's drawn. {savedHow}",
                                existing |> Option.bind _.Text |> Option.defaultValue (string node.ActualText),
                                "Nothing to read here (cleared)"
                        match! Dialogs.promptText window title prompt current placeholder with
                        | Some text ->
                            let next =
                                match kind with
                                // Blank drops the edit: an element's alt text can't be made empty.
                                | AltText when String.IsNullOrWhiteSpace text -> RegionEdits.setAlt edits.Current page node.Tag box ""
                                | AltText -> RegionEdits.setAltAgainst edits.Current page node.Tag box (string node.AlternateDescription) text
                                | LinkDescription when String.IsNullOrWhiteSpace text -> RegionEdits.setAlt edits.Current page node.Tag box ""
                                | LinkDescription -> RegionEdits.setAltAgainst edits.Current page node.Tag box (string node.LinkDescription) text
                                | ReplacementText -> RegionEdits.setText edits.Current page node.Tag box (string node.ActualText) text
                            do! saveEdits next
                        | None -> ()
                    }
                    |> ignore

            let selection = { Select = select; Edits = edits; Edit = editNode; Suggested = isSuggested }

            // For a suggested layout: how many pages are found, and the titles of the pages its QR
            // codes link to, read as they're found (if the offer before the window allowed it).
            let readyCount = ctx.useState (suggested |> Option.map (fun (review, _) -> review.Pages.Count) |> Option.defaultValue 0)
            let failures = ctx.useState (Map.empty: Map<int, string>)
            let readingTitles = ctx.useState 0
            let asked = ctx.useState (Collections.Generic.HashSet<string>(), false)
            let status = ctx.useState ""
            let downloadProgress = ctx.useState (None: float option)

            /// Reads the titles of the pages a found page's QR codes go to, if allowed and not read
            /// yet; then builds the pages again, their alt text prefilled with them.
            let readTitlesFor (review: DocumentReview) (page: PageSuggestion) =
                match
                    page.Blocks
                    |> List.choose (fun block -> block.QrText |> Option.bind LinkTitles.webAddress)
                    |> List.filter (fun address -> review.MayRead address && not (review.Titles.ContainsKey address))
                    |> List.filter asked.Current.Add
                with
                | [] -> ()
                | wanted ->
                    readingTitles.Set(readingTitles.Current + 1)
                    task {
                        try
                            let! found = Task.Run(fun () -> (LinkTitles.WebPageTitles() :> IPageTitleSource).PageTitles wanted)
                            review.Titles <- found |> Seq.fold (fun all (KeyValue(address, title)) -> Map.add address title all) review.Titles
                            pageNodes.Current.Clear()
                            root.Set(buildRoot ())
                            showPage selected.Current
                        finally
                            readingTitles.Set(readingTitles.Current - 1)
                    }
                    |> ignore

            // Pages are found in the background; each is added to the structure as it is.
            ctx.useEffect (
                (fun () ->
                    match suggested with
                    | Some(review, prefetcher) ->
                        let ready =
                            prefetcher.PageReady.Subscribe(fun (_, page) ->
                                Avalonia.Threading.Dispatcher.UIThread.Post(fun () ->
                                    readyCount.Set review.Pages.Count
                                    root.Set(buildRoot ())
                                    if shownPage.Current.IsNone then showPage selected.Current
                                    readTitlesFor review page))
                        let failed =
                            prefetcher.PageFailed.Subscribe(fun (number, reason) ->
                                Avalonia.Threading.Dispatcher.UIThread.Post(fun () -> failures.Set(failures.Current.Add(number, reason))))
                        for page in review.Pages.Values do
                            readTitlesFor review page
                        if modelReady.Current then prefetcher.Start()
                        { new IDisposable with
                            member _.Dispose() =
                                ready.Dispose()
                                failed.Dispose() }
                    | None -> { new IDisposable with member _.Dispose() = () }),
                [ EffectTrigger.AfterInit ]
            )

            let download _ =
                task {
                    downloadProgress.Set(Some 0.0)
                    try
                        do! LayoutModel.download (Progress<float>(fun fraction -> downloadProgress.Set(Some fraction))) CancellationToken.None
                        downloadProgress.Set None
                        modelReady.Set true
                        suggested |> Option.iter (fun (_, prefetcher) -> prefetcher.Start())
                        showPage selected.Current
                    with ex ->
                        downloadProgress.Set None
                        status.Set $"Couldn't download the layout model: {ex.Message}"
                }
                |> ignore

            /// Adds edits from another .legibyl.json file, e.g. one saved for an earlier version of
            /// this PDF under another name, or from another PDF: those saved for it, or inside it (a
            /// copy Legibyl saved).
            let importEdits _ =
                task {
                    let! files =
                        window.StorageProvider.OpenFilePickerAsync(
                            FilePickerOpenOptions(
                                Title = "Import edits",
                                AllowMultiple = false,
                                FileTypeFilter = [ FilePickerFileType("PDFs and Legibyl files", Patterns = [ "*.pdf"; "*.legibyl.json"; "*.json" ]) ]
                            )
                        )
                    match files |> Seq.tryHead |> Option.bind (fun f -> Option.ofObj (f.TryGetLocalPath())) with
                    | Some path ->
                        let! imported = Task.Run(fun () -> RegionEdits.fromFileOrPdf path)
                        let merged, added = RegionEdits.merge edits.Current imported
                        if added > 0 then do! saveEdits merged
                        do!
                            Dialogs.showMessage
                                window
                                (if imported.IsEmpty then $"{Path.GetFileName path} has no edits to import."
                                 else $"Imported {added} of {imported.Length} edits from {Path.GetFileName path}; the rest were already here. They're in the Edits tab.")
                                "Import Edits"
                    | None -> ()
                }
                |> ignore

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
                    | Some path -> File.WriteAllText(path, JsonSerializer.Serialize(root.Current, jsonOptions))
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
                        Component.create("structure-layout", fun ctx ->
                        let root = ctx.usePassedRead root
                        ScrollViewer.create [
                            ScrollViewer.verticalScrollBarVisibility Primitives.ScrollBarVisibility.Auto
                            ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Disabled
                            Theme.resources [ ScrollViewer.BackgroundProperty, Theme.Tokens.Background ]
                            ScrollViewer.content (
                                StackPanel.create [
                                    StackPanel.margin 8.0
                                    StackPanel.children [
                                        let levels = levelsWithinBudget root.Current.Children
                                        for child in root.Current.Children -> element selection (Thickness(0.0, 0.0, 0.0, 6.0)) (levels - 1) child
                                    ]
                                ]
                            )
                        ])
                        GridSplitter.create [
                            Grid.column 1
                            GridSplitter.width 6.0
                            GridSplitter.horizontalAlignment HorizontalAlignment.Stretch
                            Theme.resources [ GridSplitter.BackgroundProperty, Theme.Tokens.Border ]
                            GridSplitter.resizeDirection GridResizeDirection.Columns
                        ]
                        Grid.create [
                            Grid.column 2
                            Grid.margin (10.0, 0.0, 0.0, 0.0)
                            Grid.rowDefinitions "Auto,Auto,Auto,*"
                            Grid.children [
                                TextBlock.create [
                                    TextBlock.text "Selected element"
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.margin (0.0, 0.0, 0.0, 6.0)
                                ]
                                TextBlock.create [
                                    Grid.row 1
                                    TextBlock.text previewNote.Current
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                    TextBlock.margin (0.0, 0.0, 0.0, 8.0)
                                ]
                                // A block of text can be given another tag; paragraphs next to each
                                // other can be merged into one.
                                StackPanel.create [
                                  Grid.row 2
                                  StackPanel.children [
                                match selected.Current with
                                | [ single ] when canRetag isSuggested single ->
                                    let box = nonNull single.Geometry
                                    let tag =
                                        editFor edits.Current single |> Option.bind _.Retag |> Option.defaultValue single.Tag
                                    let tags = if isSuggested then LayoutFile.tags else retagTags
                                    DockPanel.create [
                                        DockPanel.margin (0.0, 0.0, 0.0, 8.0)
                                        DockPanel.children [
                                            TextBlock.create [
                                                DockPanel.dock Dock.Left
                                                TextBlock.text "Tag"
                                                TextBlock.verticalAlignment VerticalAlignment.Center
                                                TextBlock.margin (0.0, 0.0, 8.0, 0.0)
                                            ]
                                            ComboBox.create [
                                                ComboBox.minWidth 120.0
                                                ComboBox.horizontalAlignment HorizontalAlignment.Left
                                                ComboBox.dataItems (if List.contains tag tags then tags else tag :: tags)
                                                ComboBox.selectedItem tag
                                                AttrBuilder<ComboBox>.CreateProperty<string | null>(
                                                    Automation.AutomationProperties.NameProperty, $"Tag for this <{single.Tag}>", ValueNone)
                                                ToolTip.tip (
                                                    if isSuggested then
                                                        "The tag to give it: H1 (the title), H (a heading), P, LI (a list item), Figure, Table, Caption, Note, Formula, Lbl (a label), or Artifact (page furniture, not read). An edit on the suggestion: Export… in the main window tags a copy with it."
                                                    else
                                                        "Give this element another tag, e.g. a heading tagged as a paragraph (as an edit: Export… in the main window writes it to a copy)."
                                                )
                                                // Each render's handler, for the element selected now; and
                                                // only while it is (moving the selection sets the picker to
                                                // the next element's tag before its handler replaces this).
                                                ComboBox.onSelectedItemChanged (
                                                    (fun item ->
                                                        match item with
                                                        | :? string as picked when
                                                            picked <> tag
                                                            && (match selected.Current with [ now ] -> obj.ReferenceEquals(now, single) | _ -> false)
                                                            ->
                                                            saveEdits (RegionEdits.setRetag edits.Current box.PageNumber single.Tag box picked) |> ignore
                                                        | _ -> ()),
                                                    SubPatchOptions.Always
                                                )
                                            ]
                                        ]
                                    ]
                                | _ -> ()
                                // A suggested region can be left out, or kept again; several at once.
                                match (if isSuggested then regionsIn selected.Current else []) with
                                | [] -> ()
                                | regions ->
                                    let leftOut = regions |> List.forall (fun r -> editFor edits.Current r |> Option.exists _.Exclude)
                                    let what = if regions.Length = 1 then "" else $" {regions.Length} Regions"
                                    Button.create [
                                        Button.content (if leftOut then $"Keep{what}" else $"Leave Out{what}")
                                        Button.margin (0.0, 0.0, 0.0, 8.0)
                                        Button.horizontalAlignment HorizontalAlignment.Left
                                        ToolTip.tip (
                                            if leftOut then "Tag it after all."
                                            else "Don't tag it: its text joins the nearest region kept; its images and lines are decoration."
                                        )
                                        Button.onClick (
                                            (fun _ ->
                                                regions
                                                |> List.fold
                                                    (fun all r ->
                                                        let box = nonNull r.Geometry
                                                        RegionEdits.setExcluded all box.PageNumber r.Tag box (not leftOut))
                                                    edits.Current
                                                |> saveEdits
                                                |> ignore),
                                            SubPatchOptions.Always
                                        )
                                    ]
                                // Merging is for a tagged PDF's paragraphs.
                                let mergeable nodes = if isSuggested then None else mergeable nodes
                                let pendingMerge =
                                    match mergeable selected.Current with
                                    | Some(first :: rest) ->
                                        let box = nonNull first.Geometry
                                        RegionEdits.findMerge edits.Current box.PageNumber box
                                        |> Option.filter (fun m -> m.MergeWith.Length = rest.Length)
                                    | _ -> None
                                match mergeable selected.Current, pendingMerge with
                                | Some _, Some merge ->
                                    Button.create [
                                        Button.content "Don't Merge These"
                                        Button.margin (0.0, 0.0, 0.0, 8.0)
                                        ToolTip.tip "Drop this merge: they stay separate paragraphs."
                                        Button.onClick (
                                            (fun _ -> saveEdits (edits.Current |> List.filter (fun e -> not (obj.ReferenceEquals(e, merge)))) |> ignore),
                                            SubPatchOptions.Always
                                        )
                                    ]
                                | Some paragraphs, None ->
                                    Button.create [
                                        Button.content $"Merge {paragraphs.Length} Paragraphs"
                                        Button.margin (0.0, 0.0, 0.0, 8.0)
                                        ToolTip.tip "Make these paragraphs one, as an edit: Export… in the main window writes it to a copy."
                                        Button.onClick (
                                            (fun _ ->
                                                let boxes = paragraphs |> List.map (fun p -> nonNull p.Geometry)
                                                saveEdits (RegionEdits.mergeParagraphs edits.Current boxes.Head.PageNumber boxes) |> ignore),
                                            SubPatchOptions.Always
                                        )
                                    ]
                                | None, _ ->
                                    match selected.Current with
                                    | [ single ] when single.Tag = "P" && not isSuggested ->
                                        TextBlock.create [
                                            TextBlock.text "Shift-click the paragraphs after it to merge them into one."
                                            TextBlock.textWrapping TextWrapping.Wrap
                                            TextBlock.fontSize 11.0
                                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                            TextBlock.margin (0.0, 0.0, 0.0, 8.0)
                                        ]
                                    | _ -> ()
                                  ]
                                ]
                                // The selected block's page, and for a link, the page it goes to below it.
                                let imageBox (row: int) (image: Bitmap option) =
                                    Border.create [
                                        Grid.row row
                                        Theme.resources
                                            [ Border.BorderBrushProperty, Theme.Tokens.Border
                                              Border.BackgroundProperty, Theme.Tokens.Fill ]
                                        Border.borderThickness 1.0
                                        Border.padding 6.0
                                        Border.child (
                                            Image.create [
                                                match image with
                                                | Some image -> Image.source image
                                                | None -> ()
                                                Image.stretch Stretch.Uniform
                                            ]
                                        )
                                    ]
                                Grid.create [
                                    Grid.row 3
                                    Grid.rowDefinitions (if targetImage.Current.IsSome then "*,Auto,*" else "*")
                                    Grid.children [
                                        imageBox 0 previewImage.Current
                                        if targetImage.Current.IsSome then
                                            TextBlock.create [
                                                Grid.row 1
                                                TextBlock.text targetNote.Current
                                                TextBlock.textWrapping TextWrapping.Wrap
                                                Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                                TextBlock.margin (0.0, 8.0, 0.0, 8.0)
                                            ]
                                            imageBox 2 targetImage.Current
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]

            /// Every edit for this PDF, and whether it's in the PDF yet.
            let editsTab =
                Component.create("structure-edits", fun ctx ->
                    let edits = ctx.usePassedRead selection.Edits
                    let root = ctx.usePassedRead root
                    let allNodes =
                        let rec flatten (node: StructureNode) : StructureNode list =
                            node :: (node.Children |> Seq.collect flatten |> List.ofSeq)
                        root.Current.Children |> Seq.collect flatten |> List.ofSeq
                    // The element each edit is for: its tag, in about the same place.
                    let elementFor (edit: RegionEdits.Edit) =
                        allNodes
                        |> List.choose (fun node ->
                            match node.Geometry with
                            // Once retagged, it has its new tag.
                            | NonNull geometry when (node.Tag = edit.Tag || edit.Retag = Some node.Tag) && geometry.PageNumber = edit.Page ->
                                Some(node, RegionEdits.overlap geometry edit.Box)
                            | _ -> None)
                        |> List.filter (fun (_, o) -> o >= RegionEdits.SameRegionOverlap)
                        |> List.sortByDescending snd
                        |> List.tryHead
                        |> Option.map fst
                    /// An edit, and whether it's in the PDF; carried, one from the review saved inside it,
                    /// which is read-only.
                    let row (carried: bool) (edit: RegionEdits.Edit) =
                        // A paragraph in about this place, if there is one.
                        let paragraphAt (box: StructureGeometry) =
                            allNodes
                            |> List.exists (fun node ->
                                match node.Geometry with
                                | NonNull own when node.Tag = "P" && own.PageNumber = edit.Page ->
                                    RegionEdits.overlap own box >= RegionEdits.SameRegionOverlap
                                | _ -> false)
                        let status =
                            match suggested with
                            | _ when carried -> "In this PDF: from the review saved inside it, which is read-only."
                            | Some(review, _) ->
                                match elementFor edit with
                                | Some _ -> "Export… tags a copy with it."
                                | None when not (review.Pages.ContainsKey edit.Page) -> $"Page {edit.Page}'s layout isn't found yet."
                                | None -> $"No <{edit.Tag}> suggested in about this place, so tagging won't use it."
                            | None ->
                            if not edit.MergeWith.IsEmpty then
                                let boxes = edit.Box :: edit.MergeWith
                                let all =
                                    { edit.Box with
                                        Left = boxes |> List.map _.Left |> List.min
                                        Bottom = boxes |> List.map _.Bottom |> List.min
                                        Right = boxes |> List.map _.Right |> List.max
                                        Top = boxes |> List.map _.Top |> List.max }
                                if boxes |> List.forall paragraphAt then "Not in this PDF yet: Export… writes it to a copy."
                                elif paragraphAt all then "In this PDF."
                                else "These paragraphs aren't in this PDF, so Save can't merge them."
                            else
                                match elementFor edit with
                                | None -> $"No <{edit.Tag}> in about this place in this PDF, so Save can't apply it."
                                | Some node when (pendingLines edit node).IsEmpty -> "In this PDF."
                                | Some _ -> "Not in this PDF yet: Export… writes it to a copy."
                        Border.create [
                            Border.borderThickness (0.0, 0.0, 0.0, 1.0)
                            Theme.resources [ Border.BorderBrushProperty, Theme.Tokens.Border ]
                            Border.padding (0.0, 6.0)
                            Border.child (
                                Grid.create [
                                    Grid.columnDefinitions "*,Auto"
                                    Grid.children [
                                        StackPanel.create [
                                            StackPanel.children [
                                                TextBlock.create [
                                                    TextBlock.text $"<{edit.Tag}>  p.{edit.Page}"
                                                    TextBlock.fontFamily Fonts.Monospace
                                                    TextBlock.fontSize 12.0
                                                ]
                                                match edit.Retag with
                                                | Some tag -> detailLine $"tag: {tag}" false
                                                | None -> ()
                                                if edit.Exclude then detailLine "left out: not tagged" false
                                                match edit.Alt with
                                                | Some alt when edit.Tag = "Link" -> detailLine $"description: \"{alt}\"" false
                                                | Some alt -> detailLine $"alt: \"{alt}\"" false
                                                | None -> ()
                                                match edit.Text with
                                                | Some "" -> detailLine "text: (nothing to read)" false
                                                | Some text -> detailLine $"text: \"{text}\"" false
                                                | None -> ()
                                                if not edit.MergeWith.IsEmpty then
                                                    // Its first paragraph's text, to tell merges apart.
                                                    let start =
                                                        allNodes
                                                        |> List.tryFind (fun node ->
                                                            match node.Geometry with
                                                            | NonNull own when node.Tag = "P" && own.PageNumber = edit.Page ->
                                                                RegionEdits.overlap own edit.Box >= RegionEdits.SameRegionOverlap
                                                            | _ -> false)
                                                        |> Option.bind (fun node -> Option.ofObj node.Content)
                                                        |> Option.map (fun text ->
                                                            let text = text.Trim()
                                                            if text.Length > 50 then $", from \"{text[..49]}…\"" else $", from \"{text}\"")
                                                        |> Option.defaultValue ""
                                                    detailLine $"merge: {edit.MergeWith.Length + 1} paragraphs into one{start}" false
                                                detailLine status true
                                            ]
                                        ]
                                        if not carried then
                                            Button.create [
                                                Grid.column 1
                                                Button.content "Remove"
                                                Button.verticalAlignment VerticalAlignment.Top
                                                ToolTip.tip "Drop this edit. What's already in the PDF stays."
                                                Button.onClick (fun _ ->
                                                    saveEdits (edits.Current |> List.filter (fun e -> not (obj.ReferenceEquals(e, edit)))) |> ignore)
                                            ]
                                    ]
                                ]
                            )
                        ]
                    ScrollViewer.create [
                        ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Disabled
                        ScrollViewer.content (
                            StackPanel.create [
                                StackPanel.margin 8.0
                                StackPanel.children [
                                    TextBlock.create [
                                        TextBlock.textWrapping TextWrapping.Wrap
                                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                        TextBlock.margin (0.0, 0.0, 0.0, 8.0)
                                        TextBlock.text (
                                            let file = Path.GetFileName(Dismissals.filePath sourceFilePath)
                                            if edits.Current.IsEmpty && isSuggested then
                                                "No edits yet. In the Layout tab, select a region to give it another tag or leave it out (shift-click to select several); right-click a figure or a link to describe it, or text read with OCR to correct it."
                                            elif isSuggested then
                                                $"Your edits on the suggested layout. Save in the main window keeps them in {file} next to the PDF; Export… tags a copy with the layout and them."
                                            elif edits.Current.IsEmpty then
                                                "No edits yet. In the Layout tab, right-click a figure or a link to describe it, or an element with replacement text to correct it; select a paragraph or heading to give it another tag, or shift-click neighbouring paragraphs to merge them."
                                            else
                                                $"Your edits. Save in the main window keeps them in {file} next to the PDF; Export… writes the ones not in it yet to a copy."
                                        )
                                    ]
                                    for edit in edits.Current do
                                        row false edit
                                    // The review saved inside this PDF (a copy Legibyl saved): in it already.
                                    if not review.CarriedEdits.IsEmpty then
                                        TextBlock.create [
                                            TextBlock.text "From the review saved inside this PDF"
                                            TextBlock.fontWeight FontWeight.SemiBold
                                            TextBlock.margin (0.0, 16.0, 0.0, 4.0)
                                        ]
                                        for edit in review.CarriedEdits do
                                            row true edit
                                ]
                            ]
                        )
                    ])

            let treeTab =
                TreeView.create [
                    TreeView.dataItems root.Current.Children
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

            // For a suggested layout, the document's title: prefilled as the title fixes would find
            // it (the document's own, else a short first page), else the first H1 found. Only a
            // change from what's prefilled is saved.
            let titleEdit = ctx.useState (suggested |> Option.bind (fun (review, _) -> review.Title))
            let documentCandidates =
                ctx.useStateLazy(fun () -> if isSuggested then (try DocumentTitles.candidates sourceFilePath with _ -> []) else []).Current

            let titleRow (review: DocumentReview) =
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
                        onEditsChanged ()
                Grid.create [
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
                            ToolTip.tip "Given to the tagged copy as its title, shown in place of the file name. Save keeps a change next to the PDF."
                            // What's prefilled can change as the first page is found.
                            TextBox.onTextChanged (setTitle, SubPatchOptions.Always)
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
                                    :> IView
                                for candidate in candidates do
                                    if candidate.Title <> shownTitle then
                                        yield
                                            Button.create [
                                                Button.margin (0.0, 0.0, 6.0, 0.0)
                                                Button.padding (8.0, 2.0)
                                                Button.content $"{candidate.Title} · {candidate.Source}"
                                                Button.onClick ((fun _ -> setTitle candidate.Title), SubPatchOptions.Always)
                                            ]
                                            :> IView
                            ]
                        ]
                    ]
                ]

            /// For a suggested layout: that it isn't tagged yet, how far finding it has got, and
            /// the layout model to download first if it isn't yet.
            let banner (review: DocumentReview) (prefetcher: LayoutPrefetcher) =
                let secondary (text: string) =
                    TextBlock.create [
                        TextBlock.text text
                        TextBlock.textWrapping TextWrapping.Wrap
                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                    ]
                    :> IView
                let progress =
                    [ if readyCount.Current >= prefetcher.PageCount then "Every page's layout found."
                      elif modelReady.Current then $"{readyCount.Current} of {prefetcher.PageCount} pages' layouts found; the rest are being found (several seconds a page)."
                      else $"{readyCount.Current} of {prefetcher.PageCount} pages' layouts found."
                      if readingTitles.Current > 0 then "Reading the titles of pages QR codes link to…"
                      for page in review.Pages.Values |> Seq.sortBy _.PageNumber do
                          if page.Rotated then $"Page {page.PageNumber} is rotated, and rotated pages aren't detected yet."
                          elif page.Blocks.IsEmpty then $"Nothing was found on page {page.PageNumber}."
                          elif not page.HasText then
                              $"Page {page.PageNumber} has no text layer (it may be a scan), so tagging it would need OCR first."
                      for KeyValue(page, reason) in failures.Current do
                          $"Couldn't find page {page}'s layout: {reason}" ]
                Border.create [
                    Border.padding 10.0
                    Border.cornerRadius 4.0
                    Border.borderThickness 1.0
                    Theme.resources
                        [ Border.BorderBrushProperty, Theme.Tokens.Border
                          Border.BackgroundProperty, Theme.Tokens.Fill ]
                    Border.child (
                        StackPanel.create [
                            StackPanel.spacing 6.0
                            StackPanel.children [
                                TextBlock.create [
                                    TextBlock.text "Not tagged yet: this is the structure suggested for it"
                                    TextBlock.fontWeight FontWeight.SemiBold
                                ]
                                secondary (
                                    "This PDF has no tags, so this is the structure a layout detector suggests from what each page looks like: "
                                    + "its regions, the tag each suggests, and their reading order (the numbers on the page). The suggestions stay as found; "
                                    + "what you change (another tag, leaving a region out, alt text, corrected text) is kept as edits on top (Save in the main window keeps them next to the PDF), "
                                    + "and Export… in the main window tags a copy with them (a region left out has its text join the nearest region kept). "
                                    + "Headings are H because their levels aren't known from one page, runs of paragraphs that start with list markers are "
                                    + "list items (LI), and Artifact marks page furniture like running headers and page numbers."
                                )
                                secondary (String.Join(" ", progress))
                                if not modelReady.Current then
                                    secondary (
                                        $"Finding the layout needs a layout detection model (PP-DocLayoutV3, about {LayoutModel.DownloadSizeMegabytes} MB). "
                                        + $"It's downloaded once from {LayoutModel.source ()} into {LayoutModel.directory ()}, and runs on this computer: "
                                        + "the PDF isn't sent anywhere."
                                    )
                                    match downloadProgress.Current with
                                    | Some fraction ->
                                        ProgressBar.create [ ProgressBar.minimum 0.0; ProgressBar.maximum 1.0; ProgressBar.value fraction ]
                                    | None ->
                                        Button.create [
                                            Button.content "Download model"
                                            Button.horizontalAlignment HorizontalAlignment.Left
                                            Button.onClick download
                                        ]
                                if status.Current <> "" then
                                    TextBlock.create [
                                        TextBlock.text status.Current
                                        TextBlock.textWrapping TextWrapping.Wrap
                                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.Danger ]
                                    ]
                            ]
                        ]
                    )
                ]

            Grid.create [
                Grid.margin 12.0
                Grid.rowDefinitions "Auto,*,Auto"
                Grid.children [
                    StackPanel.create [
                        StackPanel.margin (0.0, 0.0, 0.0, 8.0)
                        StackPanel.spacing 10.0
                        StackPanel.children [
                            match suggested with
                            | Some(review, prefetcher) ->
                                banner review prefetcher
                                titleRow review
                            | None ->
                                TextBlock.create [
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                    TextBlock.text
                                        "This is the tagged logical structure tree — the semantic heading/paragraph/list/table/link hierarchy that determines what content and reading order a screen reader presents to a user. Only elements that directly own page content show a page number."
                                ]
                        ]
                    ]
                    TabControl.create [
                        Grid.row 1
                        TabControl.viewItems [
                            TabItem.create [ TabItem.header "Layout"; TabItem.content layoutTab ]
                            TabItem.create [ TabItem.header "Tree"; TabItem.content treeTab ]
                            TabItem.create [
                                TabItem.header (if edits.Current.IsEmpty then "Edits" else $"Edits ({edits.Current.Length})")
                                TabItem.content editsTab
                            ]
                        ]
                    ]
                    StackPanel.create [
                        Grid.row 2
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.horizontalAlignment HorizontalAlignment.Right
                        StackPanel.margin (0.0, 8.0, 0.0, 0.0)
                        StackPanel.children [
                            Button.create [
                                Button.content "Import Edits…"
                                Button.padding (12.0, 4.0)
                                Button.margin (0.0, 0.0, 8.0, 0.0)
                                ToolTip.tip
                                    "Use the edits made for another PDF, such as an earlier version of this one: pick that PDF (its edits saved next to it, or inside a copy Legibyl saved), or the .legibyl.json file saved for it."
                                Button.onClick importEdits
                            ]
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

/// Edits change review; onEditsChanged is called after each (or a suggested layout's title changed).
type StructureTreeWindow(source: StructureSource, sourceFilePath: string, review: DocumentReview, onEditsChanged: unit -> unit) as this =
    inherit
        HostWindow(
            Title = (match source with Tagged _ -> "Document Structure" | Suggested _ -> "Suggested Structure"),
            Width = 1000.0,
            Height = 760.0,
            MinWidth = 500.0,
            MinHeight = 400.0
        )
    // Keeps the PDF open and recent pages rendered while the window is open.
    let renderer = new PdfRegionRenderer()
    // For a suggested layout: what finds each page's, in the background.
    let finding =
        match source with
        | Suggested ->
            let suggester = new LayoutSuggester(sourceFilePath)
            Some(suggester, LayoutPrefetcher(suggester, review.Pages))
        | Tagged _ -> None
    do
        let tagged = match source with Tagged root -> root | Suggested -> SuggestedTree.build []
        this.Content <- view this renderer tagged review (finding |> Option.map snd) sourceFilePath onEditsChanged

    /// The review its edits change.
    member _.Review = review

    /// A tagged PDF's structure, with a review of its own (read from the file next to it).
    new(root: StructureNode, sourceFilePath: string) =
        new StructureTreeWindow(Tagged root, sourceFilePath, DocumentReviews().For sourceFilePath, ignore)

    override _.OnClosed(e) =
        base.OnClosed e
        match finding with
        | Some(suggester, prefetcher) ->
            // A page may still be being found: stop after it, then let go of the suggester.
            Task.Run(fun () ->
                prefetcher.Stop()
                (suggester :> IDisposable).Dispose())
            |> ignore
        | None -> ()
        renderer.Dispose()
