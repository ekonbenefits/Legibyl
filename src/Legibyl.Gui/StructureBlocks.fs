/// The Structure window's Blocks tab: each element drawn as a block (a coloured tag, its text, its
/// children inside), selectable, with its findings marked and its edits noted.
module internal Legibyl.Gui.StructureBlocks


open System
open System.IO
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Templates
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
open Legibyl.Gui.StructureColors
open Legibyl.Gui.StructurePreview
open Legibyl.Gui.StructureActions

/// What blocks can do: select (called with the clicked block's node, its border control, and
/// whether to extend the selection to it, as shift-click does), show the user's edits, and edit
/// an element.
type Selection =
    { Select: StructureNode -> Border -> bool -> unit
      Edits: IReadable<RegionEdits.Edit list>
      Edit: StructureNode -> EditKind -> unit
      /// Whether it's a suggested layout, which nothing is in yet (not a tagged PDF).
      Suggested: bool
      /// Nodes whose collapsed children are shown anyway: those above a block selected on the
      /// page preview, to bring it into view.
      Expanded: IReadable<StructureNode list>
      /// The failures and warnings on each block (see FindingMarkers), and how to show one in
      /// the main window's results.
      Findings: Collections.Generic.IReadOnlyDictionary<StructureNode, RuleViolation list>
      ShowFinding: RuleViolation -> unit
      /// The edits as they were when an element was last moved (see movesIn): a block's
      /// children follow it, so other edits don't re-render them.
      Moves: IReadable<RegionEdits.Edit list>
      /// Whether a moved element isn't in that place in the PDF yet.
      MovePending: StructureNode -> bool
      /// Every block's right-click menu: one, filled as it opens with what Selected element offers
      /// for the block clicked (see StructureActions.selectionMenu).
      Menu: ContextMenu }

/// A finding as a marker's tooltip lists it: "Failure: ISO 14289-1 7.3, test 1: Figure …",
/// and what it means, where that's known.
let findingLine (finding: RuleViolation) =
    let standard = finding.Specification.Split(':').[0]
    let line = $"{finding.Severity}: {standard} {finding.Clause}, test {finding.TestNumber}: {finding.Message}"
    match RuleGuide.forFinding finding with
    | Some guide -> $"{line}\n    {guide.Meaning}"
    | None -> line

/// A block's failures (or, with none, warnings) as a marker: its icon, and how many if more
/// than one; its tooltip lists them. Clicking it shows the first in the main window's results.
let rec findingMarker (selection: Selection) (node: StructureNode) : IView option =
    match selection.Findings.TryGetValue node with
    | true, findings -> findingsMarker findings selection.ShowFinding
    | _ -> None

/// Findings as a marker (see findingMarker): on a block, or the document title's row.
and findingsMarker (findings: RuleViolation list) (showFinding: RuleViolation -> unit) : IView option =
    match findings with
    | _ when not findings.IsEmpty ->
        let failures = findings |> List.filter (fun f -> f.Severity = FindingSeverity.Failure) |> List.length
        let warnings = findings.Length - failures
        let icon, color = if failures > 0 then Icons.failure, Theme.Tokens.Danger else Icons.warning, Theme.Tokens.Warning
        let count (n: int) (what: string) = if n = 1 then $"1 {what}" else $"{n} {what}s"
        let summary =
            [ if failures > 0 then count failures "failure"
              if warnings > 0 then count warnings "warning" ]
            |> String.concat " and "
        let tip =
            String.concat "\n" [ $"{summary} (as validated; edits aren't checked until exported). Click to show it in the results."; yield! findings |> List.map findingLine ]
        Some(
            Button.create [
                Button.padding (4.0, 0.0)
                Button.margin (0.0, 0.0, 6.0, 0.0)
                Button.minHeight 0.0
                Button.verticalAlignment VerticalAlignment.Center
                Button.background Brushes.Transparent
                Button.borderThickness 0.0
                Button.tag "finding-marker"
                ToolTip.tip tip
                AttrBuilder<Button>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, $"{summary}: show in the results", ValueNone)
                Button.content (
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.spacing 3.0
                        StackPanel.children [
                            Avalonia.FuncUI.DSL.Path.create [
                                AttrBuilder<Shapes.Path>.CreateProperty<Geometry | null>(Shapes.Path.DataProperty, icon, ValueNone)
                                Shapes.Shape.stretch Stretch.Uniform
                                Control.width 13.0
                                Control.height 13.0
                                Control.verticalAlignment VerticalAlignment.Center
                                Theme.resources [ Shapes.Shape.FillProperty, color ]
                            ]
                            if findings.Length > 1 then
                                TextBlock.create [
                                    TextBlock.text (string findings.Length)
                                    TextBlock.fontSize 11.0
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                    Theme.resources [ TextBlock.ForegroundProperty, color ]
                                ]
                        ]
                    ]
                )
                Button.onClick ((fun e -> e.Handled <- true; showFinding findings.Head), SubPatchOptions.Always)
            ]
            :> IView
        )
    | _ -> None


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

/// A block as a screen reader names it: its tag and the start of what it says.
let blockName (node: StructureNode) =
    let said =
        [ node.Content; node.ActualText; node.AlternateDescription; node.LinkDescription ]
        |> List.tryPick (fun text -> match text with NonNull text when not (String.IsNullOrWhiteSpace text) -> Some(text.Trim()) | _ -> None)
    match said with
    | Some text when text.Length > 80 -> $"{node.Tag}: {text.Substring(0, 80)}…"
    | Some text -> $"{node.Tag}: {text}"
    | None -> node.Tag

/// The outline a focused block gets.
let private focusOutline: ITemplate<Control> | null =
    FuncTemplate<Control>(fun () ->
        Shapes.Rectangle(Stroke = Brushes.DodgerBlue, StrokeThickness = 2.0, StrokeDashArray = Collections.AvaloniaList<float>([ 2.0; 2.0 ])) :> Control)

/// A focused block's keys: Enter or Space selects it (with Shift, extends the selection to it, as
/// shift-click does); Up and Down move to the block before or after it; F2 edits what a screen
/// reader says for it (its alt text, replacement text or link description, the first it has).
let private blockKeys (selection: Selection) (node: StructureNode) (e: KeyEventArgs) =
    match e.Source with
    // Only the focused block's own handler (the event also bubbles to the blocks around it).
    | :? Border as border when obj.ReferenceEquals(border.Tag, node) ->
        match e.Key with
        | Key.Enter
        | Key.Space ->
            e.Handled <- true
            selection.Select node border (e.KeyModifiers.HasFlag KeyModifiers.Shift)
        | Key.F2 ->
            match editKinds node with
            | kind :: _ ->
                e.Handled <- true
                selection.Edit node kind
            | [] -> ()
        | Key.Up
        | Key.Down when e.KeyModifiers = KeyModifiers.None ->
            let blocks =
                border.GetVisualAncestors()
                |> Seq.tryPick (function :? ScrollViewer as scroll -> Some scroll | _ -> None)
                |> Option.map (fun scroll ->
                    scroll.GetVisualDescendants()
                    |> Seq.choose (function :? Border as b when (b.Tag :? StructureNode) && b.Focusable -> Some b | _ -> None)
                    |> List.ofSeq)
                |> Option.defaultValue []
            match blocks |> List.tryFindIndex (fun b -> obj.ReferenceEquals(b, border)) with
            | Some index ->
                let next = if e.Key = Key.Up then index - 1 else index + 1
                if next >= 0 && next < blocks.Length then
                    blocks[next].Focus(NavigationMethod.Directional) |> ignore
                    blocks[next].BringIntoView()
                e.Handled <- true
            | None -> ()
        | _ -> ()
    | _ -> ()

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
        // Reachable from the keyboard (see blockKeys), and named for a screen reader by its tag and text.
        Border.focusable true
        AttrBuilder<Border>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, blockName node, ValueNone)
        AttrBuilder<Border>.CreateProperty<ITemplate<Control> | null>(Control.FocusAdornerProperty, focusOutline, ValueNone)
        Border.onKeyDown (blockKeys selection node, SubPatchOptions.Always)
        AttrBuilder<Border>.CreateProperty<ContextMenu | null>(Control.ContextMenuProperty, selection.Menu, ValueNone)
        Border.onPointerPressed ((fun e ->
            match e.Source with
            // FuncUI also subscribes to the tunnel phase, where the outermost block would
            // see the click first; only the innermost block (bubble phase) should select.
            | :? Visual as source when
                e.Route = Interactivity.RoutingStrategies.Bubble
                && e.GetCurrentPoint(source).Properties.IsLeftButtonPressed
                ->
                // Reordering reuses borders for other nodes: select the innermost block's
                // current tag, not a node captured before the move.
                let owner =
                    source.GetSelfAndVisualAncestors()
                    |> Seq.tryPick (function
                        | :? Border as border ->
                            match border.Tag with
                            | :? StructureNode as current -> Some(current, border)
                            | _ -> None
                        | _ -> None)
                match owner with
                | Some(current, border) ->
                    e.Handled <- true
                    selection.Select current border (e.KeyModifiers.HasFlag KeyModifiers.Shift)
                | None -> ()
            | _ -> ()), SubPatchOptions.Always)
    ]

let geometryTip (node: StructureNode) (what: string) (noun: string) =
    match node.Geometry with
    | null -> $"Select: no page geometry is available for this {noun}."
    | geometry -> $"Select: highlight this {what} on page {geometry.PageNumber}."

// ---- Content outside every tag (see UntaggedContent) ----

/// What kind of piece it is, as its description starts: "a fill", "a line", "an image"…
let untaggedKind (node: StructureNode) = (string node.Content).Split(',').[0]

/// How likely a piece outside the tags is decoration, most likely first: a page's background,
/// then rules and thin borders, then other drawing, then images, then text. Only a hint:
/// nothing is marked until a person chooses.
let decorationRank (node: StructureNode) =
    let content = string node.Content
    let thin =
        match node.Geometry with
        | NonNull g -> min (g.Right - g.Left) (g.Top - g.Bottom) <= 4.0
        | Null -> false
    if content.StartsWith "a fill" && content.EndsWith "(the whole page)" then 0
    elif content.StartsWith "a line" || thin then 1
    elif content.StartsWith "a fill" || content.StartsWith "a drawing" then 2
    elif content.StartsWith "an image" then 3
    else 4

/// The hint a piece's block gives, if it's likely decoration.
let decorationHint (node: StructureNode) =
    match decorationRank node with
    | 0 -> Some "likely decoration: the page's background"
    | 1 -> Some "likely decoration: a rule or border"
    | _ -> None

/// The structure with each page's pieces outside the tags in order of how likely they're
/// decoration.
let sortUntagged (root: StructureNode) =
    { root with
        Children =
            [| for child in root.Children ->
                   if child.Kind = StructureNodeKind.NotInTags then { child with Children = child.Children |> List.ofSeq |> List.sortBy decorationRank |> Array.ofList }
                   else child |] }

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
                tagChip
                    (match node.Kind with
                     | StructureNodeKind.NotInTags -> "Not in the tags"
                     | StructureNodeKind.Disconnected -> "Tied to tags that are gone"
                     | StructureNodeKind.DisconnectedTag -> "Tag that's gone"
                     | _ -> node.Tag)
                    borderColor headerColor
                match findingMarker selection node with
                | Some marker -> marker
                | None -> ()
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
        else childViews selection childLevels node

    let inner =
        StackPanel.create [
            StackPanel.children [
                header
                match node.AlternateDescription with NonNull alt -> detailLine $"alt: \"{alt}\"" true | Null -> ()
                match node.LinkDescription with NonNull description -> detailLine $"description: \"{description}\"" true | Null -> ()
                if isUntagged node then
                    match decorationHint node with
                    | Some hint -> detailLine hint true
                    | None -> ()
                if not (editKinds node).IsEmpty || canRetag selection.Suggested node || isUntagged node || not (isNull node.Geometry) then
                    // Its own component, so an edit re-renders just this line.
                    Component.create("pending-edit", fun ctx ->
                        let edits = ctx.usePassedRead selection.Edits
                        let lines =
                            editFor edits.Current node |> Option.map (fun edit -> pendingLines edit node) |> Option.defaultValue []
                        StackPanel.create [
                            StackPanel.children [
                                for label, text in lines do
                                    match label, text with
                                    | "tag", Tags.Artifact when not selection.Suggested -> detailLine "✎ not read (decoration), not in the PDF yet" true
                                    | "tag", Tags.Figure when isUntagged node -> detailLine "✎ tagged as a figure, not in the PDF yet" true
                                    | "tag", Tags.P when isUntagged node -> detailLine "✎ tagged as a paragraph, not in the PDF yet" true
                                    | _ when selection.Suggested -> detailLine $"✎ your {label}: \"{text}\"" true
                                    | _ -> detailLine $"✎ your {label}, not in the PDF yet: \"{text}\"" true
                                if editFor edits.Current node |> Option.exists _.Exclude then
                                    detailLine "✎ merged into its neighbour: its text joins the nearest region kept" true
                                if selection.MovePending node then
                                    detailLine "✎ moved in the reading order, not in the PDF yet" true
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

/// A node's children, childLevels levels deep: a TOC's and a table's in their grids, the rest
/// stacked (in the order the moves give them, unless their order is fixed).
and childViews (selection: Selection) (childLevels: int) (node: StructureNode) : IView list =
    match node.Tag with
    | Tags.TOC when node.Kind = StructureNodeKind.Element -> [ tocGrid selection node ]
    | Tags.Table when node.Kind = StructureNodeKind.Element -> [ tableGrid selection node ]
    | _ when keepsOrder node ->
        [ for child in node.Children -> element selection (Thickness(0.0, 0.0, 0.0, 6.0)) (childLevels - 1) child ]
    | _ ->
        // Its own component, so a move re-renders just these. Keyed by the node: a component
        // keeps the node it was made for (a suggested structure's is made again as pages are found).
        [ Component.create($"ordered-children-{Runtime.CompilerServices.RuntimeHelpers.GetHashCode node}", fun ctx ->
              let moves = ctx.usePassedRead selection.Moves
              StackPanel.create [
                  StackPanel.children [
                      for child in orderedChildren moves.Current node ->
                          element selection (Thickness(0.0, 0.0, 0.0, 6.0)) (childLevels - 1) child
                  ]
              ]) ]

/// A node's children, drawn only once asked for: "▸ 31 child elements (824 in all)".
/// Its own component, so expanding re-renders just this block, not the whole layout.
and collapsedChildren (selection: Selection) (node: StructureNode) : IView =
    Component.create("collapsed-children", fun ctx ->
        let expanded = ctx.useState false
        let opened = ctx.usePassedRead selection.Expanded
        if expanded.Current || opened.Current |> List.exists (fun n -> obj.ReferenceEquals(n, node)) then
            StackPanel.create [ StackPanel.children (childViews selection (levelsWithinBudget node.Children) node) ]
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
    let borderColor, _ = colorsFor Tags.TOCI
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
    let items = toc.Children |> Seq.filter (fun child -> child.Tag = Tags.TOCI) |> List.ofSeq
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
                    let _, headerColor = colorsFor Tags.TOCI
                    tocCell selection item row 0 (Some(brush headerColor)) [
                        Border.child (
                            TextBlock.create [
                                TextBlock.text Tags.TOCI
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
/// to how an HTML table with visible borders would look. Cells are placed as HTML places
/// them: each in the first column the cells before it (and those spanning down from rows
/// above) leave free, across its ColSpan columns and down its RowSpan rows.
and tableGrid (selection: Selection) (table: StructureNode) : IView =
    let tableCells (row: StructureNode) =
        match row.Children |> Seq.filter (fun child -> child.Tag = Tags.TH || child.Tag = Tags.TD) |> List.ofSeq with
        | [] -> List.ofSeq row.Children
        | cells -> cells

    let rows = table.Children |> Seq.filter (fun child -> child.Tag = Tags.TR) |> List.ofSeq
    if rows.IsEmpty then
        fallbackStack selection table
    else
        let placed = TableLayout.place (rows |> List.map (tableCells >> List.map (fun cell -> cell, cell.ColSpan, cell.RowSpan)))
        let columnCount = max 1 (placed |> List.map (fun p -> p.Column + p.ColumnSpan) |> List.fold max 0)
        Grid.create [
            Grid.margin (0.0, 4.0, 0.0, 0.0)
            Theme.resources [ Grid.BackgroundProperty, Theme.Tokens.Fill ]
            Grid.columnDefinitions (String.Join(",", List.replicate columnCount "*"))
            Grid.rowDefinitions (String.Join(",", List.replicate rows.Length "Auto"))
            Grid.children [
                for rowIndex, row in List.indexed rows do
                    if (tableCells row).IsEmpty then
                        ContentControl.create [
                            Grid.row rowIndex
                            Grid.columnSpan columnCount
                            ContentControl.content (element selection (Thickness(0.0, 0.0, 0.0, 6.0)) Int32.MaxValue row)
                        ]
                for p in placed do
                    ContentControl.create [
                        Grid.row p.Row
                        Grid.column p.Column
                        Grid.columnSpan p.ColumnSpan
                        Grid.rowSpan p.RowSpan
                        ContentControl.content (element selection (Thickness 1.0) Int32.MaxValue p.Cell)
                    ]
            ]
        ]
