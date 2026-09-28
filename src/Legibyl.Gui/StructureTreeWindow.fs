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

    /// The elements a page's overview outlines, in reading order (each node's children in the
    /// order children gives): those that own content on it, outermost first (one inside another
    /// that does is within that one's outline).
    let pageElements (children: StructureNode -> StructureNode list) (root: StructureNode) (page: int) : StructureNode list =
        let owns (node: StructureNode) =
            node.ContentLeafCount > 0
            && (match node.Geometry with
                | NonNull geometry -> geometry.PageNumber = page
                | Null -> false)
        let rec collect (node: StructureNode) : StructureNode list =
            if owns node then [ node ] else [ for child in children node do yield! collect child ]
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
          MovePending: StructureNode -> bool }

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

    /// node's children in the order the moves in edits give them (see SiblingOrder), as read when
    /// none are moved.
    let orderedChildren (edits: RegionEdits.Edit list) (node: StructureNode) : StructureNode list =
        let children = List.ofSeq node.Children
        if edits |> List.forall _.Move.IsNone then children
        else SiblingOrder.reorder [ for child in children -> child, Option.ofObj child.Geometry, editFor edits child |> Option.bind _.Move ]

    /// The moves in edits, to tell whether the order has changed: an edit of alt text doesn't.
    let movesIn (edits: RegionEdits.Edit list) =
        edits |> List.choose (fun e -> e.Move |> Option.map (fun move -> e.Page, e.Tag, e.Box, move))

    /// The tags whose children keep their order: a table's cells and rows, a table of contents'
    /// entries, which Move Up and Move Down don't change.
    let fixedOrder = set [ "Table"; "THead"; "TBody"; "TFoot"; "TR"; "TOC"; "StructTreeRoot"; "NotInTags" ]

    /// The positions in order of the longest run of it that's in the order sequence already has
    /// (a longest increasing subsequence of positions).
    let private longestInOrder (positions: int list) : Set<int> =
        let positions = Array.ofList positions
        // tails[k]: the index ending the best run of length k + 1 found so far.
        let tails = ResizeArray<int>()
        let previous = Array.create positions.Length -1
        for i in 0 .. positions.Length - 1 do
            let mutable lo, hi = 0, tails.Count
            while lo < hi do
                let mid = (lo + hi) / 2
                if positions[tails[mid]] < positions[i] then lo <- mid + 1 else hi <- mid
            if lo > 0 then previous[i] <- tails[lo - 1]
            if lo = tails.Count then tails.Add i else tails[lo] <- i
        let mutable kept = Set.empty
        let mutable at = if tails.Count = 0 then -1 else tails[tails.Count - 1]
        while at >= 0 do
            kept <- kept.Add at
            at <- previous[at]
        kept

    /// edits with parent's children in order: the fewest moves that give it (see SiblingOrder),
    /// replacing those it had. The longest run of order that's in the order they were read in
    /// stays; each of the rest moves after the one before it (or first).
    let recordOrder (edits: RegionEdits.Edit list) (parent: StructureNode) (order: StructureNode list) =
        let setMove edits (node: StructureNode) move =
            match node.Geometry with
            | NonNull box -> RegionEdits.setMove edits box.PageNumber node.Tag box move
            | Null -> edits
        let cleared =
            parent.Children
            |> Seq.fold (fun edits node -> if (editFor edits node |> Option.bind _.Move).IsSome then setMove edits node None else edits) edits
        let asRead = List.ofSeq parent.Children
        let positionOf (node: StructureNode) = asRead |> List.findIndex (fun n -> obj.ReferenceEquals(n, node))
        let kept = longestInOrder (order |> List.map positionOf)
        let moved =
            order
            |> List.indexed
            |> List.fold
                (fun edits (i, node) ->
                    if kept.Contains i then edits
                    else
                        let move = if i = 0 then Some First else order[i - 1].Geometry |> Option.ofObj |> Option.map After
                        setMove edits node move)
                cleared
        let gives edits = List.forall2 (fun a b -> obj.ReferenceEquals(a, b)) (orderedChildren edits parent) order
        if gives moved then moved
        else
            // One with no place on a page can't be named by a move: place them one at a time instead.
            let rec place edits tries =
                let now = orderedChildren edits parent
                match List.zip now order |> List.tryFindIndex (fun (a, b) -> not (obj.ReferenceEquals(a, b))) with
                | Some i when tries > 0 ->
                    let move = if i = 0 then Some First else order[i - 1].Geometry |> Option.ofObj |> Option.map After
                    match move, order[i].Geometry with
                    | Some _, NonNull _ -> place (setMove edits order[i] move) (tries - 1)
                    | _ -> edits
                | _ -> edits
            place cleared (order.Length + 1)

    /// For a paragraph or figure: how it's in a merge the user chose, if it is (merges are only
    /// ever not in the PDF yet where they're still apart, as they are wherever a block shows them).
    let mergeLine (edits: RegionEdits.Edit list) (node: StructureNode) =
        match node.Geometry with
        | NonNull box when node.Tag = "P" || node.Tag = "Figure" ->
            let noun = if node.Tag = "P" then "paragraph" else "figure"
            match RegionEdits.findMerge edits box.PageNumber box with
            | Some merge ->
                let others = merge.MergeWith.Length
                Some(if others = 1 then $"✎ merged with the next {noun}, not in the PDF yet" else $"✎ merged with the next {others} {noun}s, not in the PDF yet")
            | None ->
                let merged =
                    edits
                    |> List.exists (fun e ->
                        e.Page = box.PageNumber && e.MergeWith |> List.exists (fun other -> RegionEdits.overlap other box >= RegionEdits.SameRegionOverlap))
                if merged then Some $"✎ merged into the {noun} before, not in the PDF yet" else None
        | _ -> None

    /// The selected blocks, if they're paragraphs or figures that can be merged: two or more of
    /// one of them on one page. Paragraphs with no alt text or replacement text (merging would lose
    /// it); figures with none, or all the same (a picture drawn in pieces).
    let mergeable (nodes: StructureNode list) =
        match nodes with
        | first :: _ :: _ when
            (first.Tag = "P" || first.Tag = "Figure")
            && nodes
               |> List.forall (fun n ->
                   n.Tag = first.Tag
                   && isNull n.ActualText
                   && (if n.Tag = "P" then isNull n.AlternateDescription else n.AlternateDescription = first.AlternateDescription)
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

    // ---- Content outside every tag (see UntaggedContent) ----

    /// Whether it's a piece of what a page draws outside every tag.
    let isUntagged (node: StructureNode) = node.Tag = "Untagged"

    /// Whether a piece outside the tags is text, which is never made decoration.
    let isUntaggedText (node: StructureNode) = (string node.Content).StartsWith "text "

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
                       if child.Tag = "NotInTags" then { child with Children = child.Children |> List.ofSeq |> List.sortBy decorationRank |> Array.ofList }
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
                    tagChip (if node.Tag = "NotInTags" then "Not in the tags" else node.Tag) borderColor headerColor
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
            else
                match node.Tag with
                | "TOC" -> [ tocGrid selection node ]
                | "Table" -> [ tableGrid selection node ]
                | tag when fixedOrder.Contains tag ->
                    [ for child in node.Children -> element selection (Thickness(0.0, 0.0, 0.0, 6.0)) (childLevels - 1) child ]
                | _ ->
                    // In the order the moves give them; its own component, so a move re-renders just these.
                    // Keyed by the node: a component keeps the node it was made for (a suggested
                    // structure's is made again as pages are found).
                    [ Component.create($"ordered-children-{Runtime.CompilerServices.RuntimeHelpers.GetHashCode node}", fun ctx ->
                          let moves = ctx.usePassedRead selection.Moves
                          StackPanel.create [
                              StackPanel.children [
                                  for child in orderedChildren moves.Current node ->
                                      element selection (Thickness(0.0, 0.0, 0.0, 6.0)) (childLevels - 1) child
                              ]
                          ]) ]

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
                                        | "tag", "Artifact" when not selection.Suggested -> detailLine "✎ not read (decoration), not in the PDF yet" true
                                        | "tag", "Figure" when isUntagged node -> detailLine "✎ tagged as a figure, not in the PDF yet" true
                                        | "tag", "P" when isUntagged node -> detailLine "✎ tagged as a paragraph, not in the PDF yet" true
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

    /// A node's children, drawn only once asked for: "▸ 31 child elements (824 in all)".
    /// Its own component, so expanding re-renders just this block, not the whole layout.
    and collapsedChildren (selection: Selection) (node: StructureNode) : IView =
        Component.create("collapsed-children", fun ctx ->
            let expanded = ctx.useState false
            let opened = ctx.usePassedRead selection.Expanded
            let moves = ctx.usePassedRead selection.Moves
            if expanded.Current || opened.Current |> List.exists (fun n -> obj.ReferenceEquals(n, node)) then
                let levels = levelsWithinBudget node.Children
                let children = if fixedOrder.Contains node.Tag then List.ofSeq node.Children else orderedChildren moves.Current node
                StackPanel.create [
                    StackPanel.children [
                        for child in children -> element selection (Thickness(0.0, 0.0, 0.0, 6.0)) (levels - 1) child
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

    /// A suggested region merged into its neighbour is outlined grey.
    let leftOutColor = 0x9E9E9Eu

    /// Pixels per point the page preview is rendered at.
    let previewScale = 2.0f

    /// Whether the page preview draws only the selection, for this session: a busy page's
    /// outlines can hide it.
    let mutable private selectionOnlyThisSession = false

    /// Over an element on the page preview, to say it can be clicked.
    let private handCursor = lazy (new Cursor(StandardCursorType.Hand))

    /// The smallest element on page whose region holds (x, y), in PDF points: the innermost of
    /// nested ones (a link in its paragraph, an item in its list).
    let nodeAt (root: StructureNode) (page: int) (x: float) (y: float) =
        let rec all (node: StructureNode) : StructureNode seq =
            seq {
                yield node
                for child in node.Children do
                    yield! all child
            }
        all root
        |> Seq.choose (fun node ->
            match node.Geometry with
            | NonNull g when g.PageNumber = page && x >= g.Left && x <= g.Right && y >= g.Bottom && y <= g.Top ->
                Some(node, (g.Right - g.Left) * (g.Top - g.Bottom))
            | _ -> None)
        |> Seq.sortBy snd
        |> Seq.tryHead
        |> Option.map fst

    /// Where on its page, in PDF points, a pointer is over a page preview rendered at
    /// previewScale; None off the page (the image is scaled to fit, centred).
    let pointOnPage (image: Image) (bitmap: Bitmap) (position: Point) =
        let size = bitmap.Size
        let bounds = image.Bounds.Size
        if size.Width <= 0.0 || size.Height <= 0.0 || bounds.Width <= 0.0 || bounds.Height <= 0.0 then
            None
        else
            let fit = min (bounds.Width / size.Width) (bounds.Height / size.Height)
            let x = (position.X - (bounds.Width - size.Width * fit) / 2.0) / fit
            let y = (position.Y - (bounds.Height - size.Height * fit) / 2.0) / fit
            if x < 0.0 || y < 0.0 || x > size.Width || y > size.Height then
                None
            else
                // The bitmap's pixels to points; PDF's y runs up from the bottom.
                let pixels = float bitmap.PixelSize.Width / size.Width
                let scale = float previewScale
                Some(x * pixels / scale, (size.Height - y) * pixels / scale)

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
        (findings: FindingMarkers.Findings)
        (layoutRegions: ((int -> StructureGeometry list) option) ref)
        =
        // What's drawn outside every tag, most likely decoration first.
        let tagged = sortUntagged tagged
        Component(fun ctx ->
            let suggested = prefetcher |> Option.map (fun prefetcher -> review, prefetcher)
            let isSuggested = suggested.IsSome
            // The findings on each block: a tagged PDF's only (a suggested structure's findings are
            // about the untagged file, not its blocks).
            let placed =
                ctx.useStateLazy(fun () ->
                    FindingMarkers.place tagged (if isSuggested then [] else findings.Items)).Current
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
            // Collapsed blocks opened to bring one selected on the preview into view.
            let expanded = ctx.useState ([]: StructureNode list)
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
            // The edits as of the last move, which the blocks' order follows.
            let moves = ctx.useState review.Edits
            /// Whether node is moved and isn't in that place in the PDF yet: its predecessor as read
            /// isn't the one its move names.
            let movePending (node: StructureNode) =
                match editFor edits.Current node |> Option.bind _.Move, (parents ()).TryGetValue node with
                | Some move, (true, parent) ->
                    let asRead = List.ofSeq parent.Children
                    let before =
                        match asRead |> List.tryFindIndex (fun n -> obj.ReferenceEquals(n, node)) with
                        | Some i when i > 0 -> Some asRead[i - 1]
                        | _ -> None
                    match move, before with
                    | First, None -> false
                    | After box, Some previous ->
                        match previous.Geometry with
                        | NonNull own -> own.PageNumber <> box.PageNumber || RegionEdits.overlap own box < RegionEdits.SameRegionOverlap
                        | Null -> true
                    | _ -> true
                | _ -> false
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
            // Draw only the selection on the preview, not every element on its page.
            let selectionOnly = ctx.useState selectionOnlyThisSession

            /// A region's marker on the preview for the findings on it and on what's inside it (an
            /// element's own block may be nested in the one outlined): red with failures, amber with
            /// only warnings, and how many.
            let markerFor (node: StructureNode) =
                let rec findingsIn (node: StructureNode) : RuleViolation list =
                    (match placed.TryGetValue node with | true, found -> found | _ -> [])
                    @ [ for child in node.Children do yield! findingsIn child ]
                match findingsIn node with
                | [] -> None
                | found ->
                    let color = if found |> List.exists (fun f -> f.Severity = FindingSeverity.Failure) then 0xE53935u else 0xF9A825u
                    Some(color, string found.Length)

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
                                        let elements = pageElements (orderedChildren currentEdits) tree page
                                        // As edited: given another tag, or merged into its neighbour.
                                        let outlined =
                                            [ for i, element in List.indexed elements ->
                                                  let edit = editFor currentEdits element
                                                  let tag = edit |> Option.bind _.Retag |> Option.defaultValue element.Tag
                                                  let leftOut = edit |> Option.exists _.Exclude
                                                  { Geometry = nonNull element.Geometry
                                                    Label = if leftOut then $"× {i + 1} {tag}" else $"{i + 1} {tag}"
                                                    // Not a region of its own, not read, or outside every tag: grey.
                                                    Color = if leftOut || tag = "Artifact" || tag = "Untagged" then leftOutColor else outlineColor tag
                                                    // Outside every tag (whatever's chosen for it): dashed.
                                                    Dashed = isUntagged element
                                                    Emphasised = within element
                                                    Marker = markerFor element } ]
                                        // A selection that isn't one of them (a span in a paragraph, say) is filled too.
                                        let extra =
                                            [ for node in nodes do
                                                  match node.Geometry with
                                                  | NonNull geometry when geometry.PageNumber = page && not (elements |> List.exists (fun e -> obj.ReferenceEquals(e, node))) ->
                                                      { Geometry = geometry; Label = node.Tag; Color = outlineColor node.Tag; Emphasised = true; Marker = markerFor node; Dashed = isUntagged node }
                                                  | _ -> () ]
                                        // Only the selection, when asked: the rest are still there to click.
                                        let drawn =
                                            if selectionOnly.Current then (outlined |> List.filter _.Emphasised) @ extra
                                            else outlined @ extra
                                        let note =
                                            let count = elements.Length
                                            let what = if count = 1 then "its element" else $"its {count} elements"
                                            let hidden = count - (outlined |> List.filter _.Emphasised).Length
                                            let hiddenNote = if hidden = 1 then "1 element hidden" else $"{hidden} elements hidden"
                                            if selectionOnly.Current && nodes.IsEmpty then
                                                $"Page {page}: select an element (here, or in the list) to show it; {hiddenNote}."
                                            elif selectionOnly.Current then $"Page {page}: the selection only; {hiddenNote}."
                                            elif nodes.IsEmpty then $"Page {page}: {what}, outlined and numbered in reading order."
                                            else $"Page {page}: {what}, outlined and numbered in reading order; the selection filled."
                                        page, decode (renderer.RenderPageWithRegions(sourceFilePath, page, drawn, previewScale)), note),
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
                    if movesIn next <> movesIn moves.Current then moves.Set next
                    onEditsChanged ()
                    // The overview labels elements as edited.
                    showPage selected.Current
                }

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
                                // Decoration isn't described but tagged Artifact, not read: a suggested region,
                                // or a tagged PDF's figure.
                                let decoration =
                                    if isSuggested || node.Tag = "Figure" then " If it's only decoration, close this and give it the tag Artifact instead."
                                    else ""
                                // A piece outside the tags is given alt text as the figure it's tagged as.
                                let shownTag = if isUntagged node then "Figure" else node.Tag
                                "Alt Text",
                                $"Alt text for this <{shownTag}> on page {page}: what a screen reader says for it. "
                                + "Say what it's for here, not only what it shows: \"Map of the route to the campsite\", not \"map\". "
                                + $"Keep it to a sentence or so, and leave out \"image of\": the reader says it's an image.{decoration}",
                                existing |> Option.bind _.Alt |> Option.defaultValue (string node.AlternateDescription),
                                "What it's for, in a sentence"
                            | LinkDescription ->
                                let target = match node.LinkUri with NonNull uri -> $" (it goes to {uri})" | Null -> ""
                                "Link Description",
                                $"What a screen reader announces for this link on page {page}{target}: where it goes, e.g. \"Apply for benefits (example.com)\".",
                                existing |> Option.bind _.Alt |> Option.defaultValue (string node.LinkDescription),
                                "Where the link goes"
                            | ReplacementText ->
                                "Replacement Text",
                                $"What a screen reader reads for this <{node.Tag}> on page {page} (its /ActualText), in place of what's drawn.",
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

            /// Selects the block for node, as clicking it does, opening collapsed blocks above it if
            /// it's in one, and scrolls it into view. A node without a block of its own (a table's
            /// row, say) selects the nearest one above it that has.
            let selectNode (node: StructureNode) (extend: bool) =
                let borderFor (node: StructureNode) =
                    window.GetVisualDescendants()
                    |> Seq.tryPick (fun v ->
                        match v with
                        | :? Border as border when obj.ReferenceEquals(border.Tag, node) -> Some border
                        | _ -> None)
                let rec ancestors (node: StructureNode) acc =
                    match (parents ()).TryGetValue node with
                    | true, parent -> ancestors parent (parent :: acc)
                    | _ -> acc
                let finish () =
                    match node :: List.rev (ancestors node []) |> List.tryPick (fun n -> borderFor n |> Option.map (fun b -> n, b)) with
                    | Some(shown, border) ->
                        select shown border extend
                        border.BringIntoView() |> ignore
                    | None -> ()
                match borderFor node with
                | Some _ -> finish ()
                | None ->
                    expanded.Set(ancestors node [] @ expanded.Current)
                    // Once the opened blocks are laid out.
                    Avalonia.Threading.Dispatcher.UIThread.Post(finish, Avalonia.Threading.DispatcherPriority.Background)

            /// The page preview's element under the pointer, if any.
            let previewNodeAt (e: PointerEventArgs) =
                match e.Source, shownPage.Current, previewImage.Current with
                | (:? Visual as source), Some page, Some bitmap ->
                    source.GetSelfAndVisualAncestors()
                    |> Seq.tryPick (fun v -> match v with :? Image as image -> Some image | _ -> None)
                    |> Option.bind (fun image ->
                        pointOnPage image bitmap (e.GetPosition image) |> Option.map (fun point -> image, page, point))
                    |> Option.bind (fun (image, page, (x, y)) -> nodeAt root.Current page x y |> Option.map (fun node -> image, node))
                | _ -> None

            /// Marks every piece outside the tags like this one (the same kind, about the same size,
            /// on any page) as decoration, once a person has seen how many and where.
            let markAllLike (piece: StructureNode) =
                task {
                    let size (n: StructureNode) = let g = nonNull n.Geometry in g.Right - g.Left, g.Top - g.Bottom
                    let width, height = size piece
                    let near (a: float) (b: float) = abs (a - b) <= max 2.0 (b * 0.1)
                    let rec all (node: StructureNode) : StructureNode list =
                        node :: [ for child in node.Children do yield! all child ]
                    let alike =
                        all root.Current
                        |> List.filter (fun n ->
                            isUntagged n
                            && not (isNull n.Geometry)
                            && untaggedKind n = untaggedKind piece
                            && (let w, h = size n in near w width && near h height)
                            && (editFor edits.Current n |> Option.bind _.Retag) <> Some "Artifact")
                    let pages = alike |> List.map (fun n -> (nonNull n.Geometry).PageNumber) |> List.distinct |> List.sort
                    let where =
                        match pages with
                        | [ page ] -> $"page {page}"
                        | pages -> "pages " + (pages |> List.map string |> String.concat ", ")
                    let n (value: float) = Math.Round(value).ToString(Globalization.CultureInfo.InvariantCulture)
                    let count = if alike.Length = 1 then "1 piece" else $"{alike.Length} pieces"
                    let! confirmed =
                        Dialogs.showMessageWithAction
                            window
                            $"Mark {count} like this ({untaggedKind piece}, about {n width} × {n height} pt), on {where}, as decoration? Each is still drawn, but a screen reader skips it. Check none is a chart or picture that needs describing."
                            "Mark All Like This"
                            $"Mark {count} as Decoration"
                    if confirmed then
                        do!
                            alike
                            |> List.fold (fun edits n -> let box = nonNull n.Geometry in RegionEdits.setRetag edits box.PageNumber n.Tag box "Artifact") edits.Current
                            |> saveEdits
                }
                |> ignore

            let selection =
                { Select = select
                  Edits = edits
                  Edit = editNode
                  Suggested = isSuggested
                  Expanded = expanded
                  Findings = placed
                  ShowFinding = findings.Show
                  Moves = moves
                  MovePending = movePending }

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
            // Order Page N by Layout: finding the page's layout.
            let ordering = ctx.useState false
            /// The element whose children are the tagged PDF's top level (its Document), if there's one.
            let topLevel () =
                match root.Current.Children |> Seq.filter (fun n -> n.Tag <> "NotInTags") |> List.ofSeq with
                | [ document ] when not (fixedOrder.Contains document.Tag) -> Some document
                | _ -> None
            /// Suggests page's reading order from its layout, to accept or not.
            let orderByLayout (page: int) =
                task {
                    match topLevel () with
                    | None -> status.Set "Ordering a page by its layout orders a document's top-level elements; this PDF has no one element they're in."
                    | Some parent ->
                        ordering.Set true
                        try
                            try
                                let! regions =
                                    match layoutRegions.Value with
                                    | Some regionsOf -> Task.FromResult(regionsOf page)
                                    | None ->
                                        task {
                                            if not (LayoutModel.isDownloaded ()) then
                                                downloadProgress.Set(Some 0.0)
                                                do! LayoutModel.download (Progress<float>(fun fraction -> downloadProgress.Set(Some fraction))) CancellationToken.None
                                                downloadProgress.Set None
                                            return! Task.Run(fun () -> LayoutOrder.detect sourceFilePath page)
                                        }
                                let now = orderedChildren edits.Current parent
                                let suggestion = LayoutOrder.propose now page regions
                                if List.forall2 (fun a b -> obj.ReferenceEquals(a, b)) now suggestion then
                                    status.Set $"Page {page}'s layout reads it in the order it's tagged already."
                                else
                                    // Put in that order at once, as moves; Restore Page N Original Order undoes them.
                                    status.Set ""
                                    recordOrder edits.Current parent suggestion |> saveEdits |> ignore
                            with ex ->
                                downloadProgress.Set None
                                status.Set $"Couldn't find page {page}'s layout: {ex.Message}"
                        finally
                            ordering.Set false
                }
                |> ignore

            // Above a tagged PDF's blocks: the reading order. The selected element moves one place among
            // its siblings (on its page: not past one on another page, nor out of its parent); the
            // shown page is put in the order its layout reads, or back in the order it was tagged in.
            let orderBar () : IView =
                let page = shownPage.Current
                let moving =
                    match selected.Current with
                    | [ single ] when not (isNull single.Geometry) && not (isUntagged single) ->
                        match (parents ()).TryGetValue single with
                        | true, parent when not (fixedOrder.Contains parent.Tag) ->
                            let order = orderedChildren edits.Current parent
                            let index = order |> List.findIndex (fun n -> obj.ReferenceEquals(n, single))
                            let onPage (other: StructureNode) =
                                match other.Geometry, single.Geometry with
                                | NonNull g, NonNull own -> g.PageNumber = own.PageNumber
                                | _ -> false
                            Some(parent, order, index, index > 0 && onPage order[index - 1], index < order.Length - 1 && onPage order[index + 1])
                        | _ -> None
                    | _ -> None
                let move (up: bool) =
                    match moving with
                    | Some(parent, order, index, canUp, canDown) when (if up then canUp else canDown) ->
                        let other = if up then index - 1 else index + 1
                        let swapped = order |> List.mapi (fun k n -> if k = index then order[other] elif k = other then order[index] else n)
                        recordOrder edits.Current parent swapped |> saveEdits |> ignore
                    | _ -> ()
                let moved = page |> Option.map (fun page -> edits.Current |> List.filter (fun e -> e.Move.IsSome && e.Page = page)) |> Option.defaultValue []
                let pageName = page |> Option.map string |> Option.defaultValue ""
                WrapPanel.create [
                    DockPanel.dock Dock.Top
                    WrapPanel.margin (8.0, 0.0, 8.0, 6.0)
                    WrapPanel.children [
                        Button.create [
                            Button.content "Move Up"
                            Button.margin (0.0, 0.0, 6.0, 4.0)
                            Button.isEnabled (match moving with Some(_, _, _, canUp, _) -> canUp | None -> false)
                            ToolTip.tip "Read the selected element before the one above it: one place earlier in the reading order, among the elements in the same one, on its page."
                            Button.onClick ((fun _ -> move true), SubPatchOptions.Always)
                        ]
                        Button.create [
                            Button.content "Move Down"
                            Button.margin (0.0, 0.0, 16.0, 4.0)
                            Button.isEnabled (match moving with Some(_, _, _, _, canDown) -> canDown | None -> false)
                            ToolTip.tip "Read the selected element after the one below it: one place later in the reading order, among the elements in the same one, on its page."
                            Button.onClick ((fun _ -> move false), SubPatchOptions.Always)
                        ]
                        Button.create [
                            Button.content (
                                match ordering.Current, downloadProgress.Current with
                                | true, Some fraction -> $"Downloading the layout model… {fraction:P0}"
                                | true, None -> $"Finding page {pageName}'s layout…"
                                | false, _ -> $"Order Page {pageName} by Layout"
                            )
                            Button.margin (0.0, 0.0, 6.0, 4.0)
                            Button.isEnabled (page.IsSome && not ordering.Current && (topLevel ()).IsSome)
                            ToolTip.tip (
                                $"Put page {pageName} in the reading order its layout gives, found by a layout detector (several seconds; "
                                + $"downloads its model, about {LayoutModel.DownloadSizeMegabytes} MB, the first time), as moves you can see in the Edits tab. "
                                + "Never within a list or table, or across pages. Restore Page Original Order puts it back."
                            )
                            Button.onClick ((fun _ -> page |> Option.iter orderByLayout), SubPatchOptions.Always)
                        ]
                        if not moved.IsEmpty then
                            Button.create [
                                Button.content $"Restore Page {pageName} Original Order"
                                Button.margin (0.0, 0.0, 0.0, 4.0)
                                ToolTip.tip $"Drop every move on page {pageName}: its elements go back to the order they're tagged in."
                                Button.onClick (
                                    (fun _ ->
                                        moved
                                        |> List.fold (fun all e -> RegionEdits.setMove all e.Page e.Tag e.Box None) edits.Current
                                        |> saveEdits
                                        |> ignore),
                                    SubPatchOptions.Always
                                )
                            ]
                    ]
                ]

            let layoutTab =
                Grid.create [
                    Grid.columnDefinitions [
                        ColumnDefinition(GridLength(3.0, GridUnitType.Star), MinWidth = 320.0)
                        ColumnDefinition(GridLength 6.0)
                        ColumnDefinition(GridLength(2.0, GridUnitType.Star), MinWidth = 260.0)
                    ]
                    Grid.children [
                        DockPanel.create [
                          DockPanel.children [
                        if not isSuggested then orderBar ()
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
                          ]
                        ]
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
                                DockPanel.create [
                                    DockPanel.margin (0.0, 0.0, 0.0, 6.0)
                                    DockPanel.children [
                                        TextBlock.create [
                                            TextBlock.text "Selected element"
                                            TextBlock.fontWeight FontWeight.SemiBold
                                            TextBlock.verticalAlignment VerticalAlignment.Center
                                        ]
                                    ]
                                ]
                                StackPanel.create [
                                  Grid.row 1
                                  StackPanel.margin (0.0, 0.0, 0.0, 8.0)
                                  StackPanel.children [
                                    TextBlock.create [
                                        TextBlock.text previewNote.Current
                                        TextBlock.textWrapping TextWrapping.Wrap
                                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                    ]
                                  ]
                                ]
                                // A block of text can be given another tag; paragraphs next to each
                                // other can be merged into one.
                                StackPanel.create [
                                  Grid.row 2
                                  StackPanel.children [
                                // What the Tag picker tags: a tagged PDF's block of text, or its figures (Figure,
                                // or Artifact for decoration not read), several at once; a suggested
                                // structure's selected regions, several at once (Artifact for those not read).
                                let figures = selected.Current |> List.filter (fun n -> n.Tag = "Figure" && not (isNull n.Geometry))
                                let tagging =
                                    match selected.Current with
                                    | _ when isSuggested -> regionsIn selected.Current |> List.filter (canRetag true)
                                    | [ single ] when canRetag false single -> [ single ]
                                    | chosen when not figures.IsEmpty && figures.Length = chosen.Length -> figures
                                    | _ -> []
                                match tagging with
                                | [] -> ()
                                | targets ->
                                    let tagOf (node: StructureNode) = editFor edits.Current node |> Option.bind _.Retag |> Option.defaultValue node.Tag
                                    // Their tag, when they share one.
                                    let tag =
                                        match targets |> List.map tagOf |> List.distinct with
                                        | [ tag ] -> Some tag
                                        | _ -> None
                                    let chosen = selected.Current
                                    let tags =
                                        if isSuggested then LayoutFile.tags
                                        elif targets |> List.forall (fun n -> n.Tag = "Figure") then [ "Figure"; "Artifact" ]
                                        else retagTags
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
                                                ComboBox.dataItems (match tag with Some tag when not (List.contains tag tags) -> tag :: tags | _ -> tags)
                                                ComboBox.selectedItem (match tag with Some tag -> box tag | None -> null)
                                                AttrBuilder<ComboBox>.CreateProperty<string | null>(
                                                    Automation.AutomationProperties.NameProperty,
                                                    (match targets with
                                                     | [ single ] -> $"Tag for this <{single.Tag}>"
                                                     | _ when isSuggested -> $"Tag for these {targets.Length} regions"
                                                     | _ -> $"Tag for these {targets.Length} elements"),
                                                    ValueNone)
                                                ToolTip.tip (
                                                    if isSuggested then
                                                        "The tag to give it: H1 (the title), H (a heading, given the next level under the title when tagged, since one page can't tell), H2 to H6, P, LI (a list item), Figure, Table, Caption, Note, Formula, Lbl (a label), or Artifact (page furniture, not read)."
                                                    elif targets |> List.forall (fun n -> n.Tag = "Figure") then
                                                        "Figure, or Artifact for decoration a screen reader should skip, like a rule, a background or a border: on export, it's still drawn but marked as an artifact, and leaves the tags."
                                                    else
                                                        "Give this element another tag, e.g. a heading tagged as a paragraph."
                                                )
                                                // Each render's handler, for the element selected now; and
                                                // only while it is (moving the selection sets the picker to
                                                // the next element's tag before its handler replaces this).
                                                ComboBox.onSelectedItemChanged (
                                                    (fun item ->
                                                        match item with
                                                        | :? string as picked when Some picked <> tag && obj.ReferenceEquals(selected.Current, chosen) ->
                                                            targets
                                                            |> List.fold
                                                                (fun edits (node: StructureNode) ->
                                                                    let box = nonNull node.Geometry
                                                                    let retagged = RegionEdits.setRetag edits box.PageNumber node.Tag box picked
                                                                    // An artifact isn't read, so it isn't merged into a neighbour either.
                                                                    if isSuggested && picked = "Artifact" then RegionEdits.setExcluded retagged box.PageNumber node.Tag box false
                                                                    else retagged)
                                                                edits.Current
                                                            |> saveEdits
                                                            |> ignore
                                                        | _ -> ()),
                                                    SubPatchOptions.Always
                                                )
                                            ]
                                        ]
                                    ]
                                | _ -> ()
                                // A suggested region can be merged into its neighbour (its text still read), and
                                // kept again; several at once. (Not read is the Tag picker's Artifact.)
                                match (if isSuggested then regionsIn selected.Current else []) with
                                | [] -> ()
                                | regions ->
                                    let what = if regions.Length = 1 then "" else $" {regions.Length} Regions"
                                    let merged = regions |> List.forall (fun r -> editFor edits.Current r |> Option.exists _.Exclude)
                                    Button.create [
                                        Button.content (if merged then $"Don't Merge{what}" else $"Merge into Neighbour{what}")
                                        Button.margin (0.0, 0.0, 0.0, 8.0)
                                        ToolTip.tip (
                                            if merged then "Keep it as a region of its own."
                                            else "Not a region of its own: its text joins the nearest region kept, so it's still read; its images and lines are decoration. To keep it from being read at all, give it the tag Artifact."
                                        )
                                        Button.onClick (
                                            (fun _ ->
                                                regions
                                                |> List.fold
                                                    (fun all r ->
                                                        let box = nonNull r.Geometry
                                                        RegionEdits.setExcluded all box.PageNumber r.Tag box (not merged))
                                                    edits.Current
                                                |> saveEdits
                                                |> ignore),
                                            SubPatchOptions.Always
                                        )
                                    ]
                                // What's drawn outside every tag: made decoration (never text), or tagged as a
                                // figure or a paragraph, as a person chooses; several at once.
                                match (if isSuggested then [] else selected.Current |> List.filter (fun n -> isUntagged n && not (isNull n.Geometry))) with
                                | [] -> ()
                                | pieces ->
                                    let chosenFor (n: StructureNode) = editFor edits.Current n |> Option.bind _.Retag
                                    let text = pieces |> List.filter isUntaggedText
                                    let what = if pieces.Length = 1 then "" else $" {pieces.Length} Pieces"
                                    let setAll (retag: string) =
                                        pieces
                                        |> List.fold (fun all n -> let box = nonNull n.Geometry in RegionEdits.setRetag all box.PageNumber n.Tag box retag) edits.Current
                                        |> saveEdits
                                        |> ignore
                                    let action (label: string) (tip: string) (onClick: unit -> unit) =
                                        Button.create [
                                            Button.content label
                                            Button.margin (0.0, 0.0, 8.0, 8.0)
                                            ToolTip.tip tip
                                            Button.onClick ((fun _ -> onClick ()), SubPatchOptions.Always)
                                        ]
                                    WrapPanel.create [
                                        WrapPanel.children [
                                            if text.IsEmpty then
                                                action
                                                    $"Not Read (Artifact){what}"
                                                    "Decoration a screen reader should skip, like a background, a rule or a border: on export it's still drawn, but marked as an artifact."
                                                    (fun () -> setAll "Artifact")
                                                action
                                                    $"Tag as Figure{what}"
                                                    "It's content, like a chart or a diagram: on export it's tagged as a figure, which then needs alt text."
                                                    (fun () -> setAll "Figure")
                                            elif text.Length = pieces.Length then
                                                action $"Tag as Paragraph{what}" "Text should be read: on export it's tagged as a paragraph, in reading order after what's tagged before it." (fun () -> setAll "P")
                                            if pieces |> List.exists (fun n -> (chosenFor n).IsSome) then
                                                action "Keep Untagged" "Drop what was chosen for it: it stays outside the tags." (fun () -> setAll "Untagged")
                                            match pieces with
                                            | [ single ] when chosenFor single = Some "Figure" ->
                                                action "Edit Alt Text…" "What a screen reader says for it: what it's for here, in a sentence." (fun () -> editNode single AltText)
                                            | [ single ] when not (isUntaggedText single) ->
                                                action
                                                    "Mark All Like This…"
                                                    "Every piece like this one (the same kind, about the same size, on any page) as decoration, once you've seen the list."
                                                    (fun () -> markAllLike single)
                                            | _ -> ()
                                        ]
                                    ]
                                // Merging is for a tagged PDF's paragraphs, and figures drawn in pieces.
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
                                        ToolTip.tip "Drop this merge: they stay separate."
                                        Button.onClick (
                                            (fun _ -> saveEdits (edits.Current |> List.filter (fun e -> not (obj.ReferenceEquals(e, merge)))) |> ignore),
                                            SubPatchOptions.Always
                                        )
                                    ]
                                | Some nodes, None ->
                                    let figures = nodes.Head.Tag = "Figure"
                                    Button.create [
                                        Button.content (if figures then $"Merge {nodes.Length} Figures" else $"Merge {nodes.Length} Paragraphs")
                                        Button.margin (0.0, 0.0, 0.0, 8.0)
                                        ToolTip.tip (
                                            if figures then "Make these one figure: a picture drawn in pieces. Then give the first alt text, or the tag Artifact, for all of them."
                                            else "Make these paragraphs one."
                                        )
                                        Button.onClick (
                                            (fun _ ->
                                                let boxes = nodes |> List.map (fun p -> nonNull p.Geometry)
                                                saveEdits (RegionEdits.mergeElements edits.Current boxes.Head.PageNumber nodes.Head.Tag boxes) |> ignore),
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
                                // Clicking an element on the page selects it (shift-click extends).
                                let imageBox (row: int) (image: Bitmap option) (clickable: bool) =
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
                                                if clickable then
                                                    Image.onPointerPressed (
                                                        (fun e ->
                                                            match previewNodeAt e with
                                                            | Some(image, node) when e.GetCurrentPoint(image).Properties.IsLeftButtonPressed ->
                                                                e.Handled <- true
                                                                selectNode node (e.KeyModifiers.HasFlag KeyModifiers.Shift)
                                                            | _ -> ()),
                                                        SubPatchOptions.Always
                                                    )
                                                    // A hand over an element, to say it can be clicked.
                                                    Image.onPointerMoved (
                                                        (fun e ->
                                                            match e.Source with
                                                            | :? Image as image ->
                                                                let cursor = if (previewNodeAt e).IsSome then handCursor.Value else Cursor.Default
                                                                if not (obj.ReferenceEquals(image.Cursor, cursor)) then image.Cursor <- cursor
                                                            | _ -> ()),
                                                        SubPatchOptions.Always
                                                    )
                                            ]
                                        )
                                    ]
                                Grid.create [
                                    Grid.row 3
                                    Grid.rowDefinitions (if targetImage.Current.IsSome then "*,Auto,Auto,*" else "*,Auto")
                                    Grid.children [
                                        imageBox 0 previewImage.Current true
                                        CheckBox.create [
                                            Grid.row 1
                                            CheckBox.margin (0.0, 6.0, 0.0, 0.0)
                                            CheckBox.content "Show selected block only"
                                            CheckBox.isChecked selectionOnly.Current
                                            ToolTip.tip "Outline only the selected block on the page above, not every block: for a busy page. Clicking the page still selects what's there."
                                            CheckBox.onIsCheckedChanged (
                                                (fun e ->
                                                    match e.Source with
                                                    | :? CheckBox as checkBox ->
                                                        let only = checkBox.IsChecked.GetValueOrDefault()
                                                        if only <> selectionOnly.Current then
                                                            selectionOnly.Set only
                                                            selectionOnlyThisSession <- only
                                                            showPage selected.Current
                                                    | _ -> ()),
                                                SubPatchOptions.Always
                                            )
                                        ]
                                        if targetImage.Current.IsSome then
                                            TextBlock.create [
                                                Grid.row 2
                                                TextBlock.text targetNote.Current
                                                TextBlock.textWrapping TextWrapping.Wrap
                                                Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                                TextBlock.margin (0.0, 8.0, 0.0, 8.0)
                                            ]
                                            imageBox 3 targetImage.Current false
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
                        // An element with the edit's tag (a paragraph, or a figure) in about that place.
                        let paragraphAt (box: StructureGeometry) =
                            allNodes
                            |> List.exists (fun node ->
                                match node.Geometry with
                                | NonNull own when node.Tag = edit.Tag && own.PageNumber = edit.Page ->
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
                                else if edit.Tag = "Figure" then "These figures aren't in this PDF, so Save can't merge them."
                                else "These paragraphs aren't in this PDF, so Save can't merge them."
                            else
                                match elementFor edit with
                                | None -> $"No <{edit.Tag}> in about this place in this PDF, so Save can't apply it."
                                | Some node when (pendingLines edit node).IsEmpty && not (edit.Move.IsSome && movePending node) -> "In this PDF."
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
                                                if edit.Exclude then detailLine "merged into its neighbour" false
                                                match edit.Move with
                                                | Some First -> detailLine "moved: first in the reading order among its siblings" false
                                                | Some(After box) ->
                                                    // What it now follows, by its text (or its tag).
                                                    let previous =
                                                        allNodes
                                                        |> List.choose (fun node ->
                                                            match node.Geometry with
                                                            | NonNull own when own.PageNumber = box.PageNumber -> Some(node, RegionEdits.overlap own box)
                                                            | _ -> None)
                                                        |> List.filter (fun (_, o) -> o >= RegionEdits.SameRegionOverlap)
                                                        |> List.sortByDescending snd
                                                        |> List.tryHead
                                                        |> Option.map fst
                                                    let name =
                                                        match previous with
                                                        | Some node ->
                                                            match node.Content |> Option.ofObj |> Option.map _.Trim() with
                                                            | Some text when text.Length > 40 -> $"\"{text[..39]}…\""
                                                            | Some text when text <> "" -> $"\"{text}\""
                                                            | _ -> $"the <{node.Tag}>"
                                                        | None -> $"the element on page {box.PageNumber}"
                                                    detailLine $"moved: after {name}" false
                                                | None -> ()
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
                                                    let noun = if edit.Tag = "Figure" then "figures" else "paragraphs"
                                                    detailLine $"merge: {edit.MergeWith.Length + 1} {noun} into one{start}" false
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
                                                "No edits yet. In the Blocks tab, select a region to give it another tag, merge it into its neighbour, or mark it not read (shift-click to select several); right-click a figure or a link to describe it, or text read with OCR to correct it."
                                            elif isSuggested then
                                                $"Your edits on the suggested structure. Save in the main window keeps them in {file} next to the PDF; Export… tags a copy with the layout and them."
                                            elif edits.Current.IsEmpty then
                                                "No edits yet. In the Blocks tab, right-click a figure or a link to describe it, or an element with replacement text to correct it; select a paragraph or heading to give it another tag, or shift-click neighbouring paragraphs (or figures drawn in pieces) to merge them; select a figure that's only decoration to tag it Artifact (not read)."
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

            // The document's title. For a suggested layout, prefilled as the title fixes would find
            // it (the document's own, else a short first page), else the first H1 found; for a
            // tagged PDF, its own title (empty if it has none), the others offered. Only a change
            // from what's prefilled is saved, and Export writes it.
            let titleEdit = ctx.useState review.Title
            let documentCandidates = ctx.useStateLazy(fun () -> try DocumentTitles.candidates sourceFilePath with _ -> []).Current
            // PDF/UA's title failures (no title in the metadata; the file name shown in its place),
            // marked on the title's row.
            let titleFindings =
                let titleRules = set [ PdfProfile.PdfUa1, "7.1-9"; PdfProfile.PdfUa1, "7.1-10"; PdfProfile.PdfUa2, "8.11.1-1"; PdfProfile.PdfUa2, "8.11.2-1" ]
                if isSuggested then []
                else
                    findings.Items
                    |> List.filter (fun f ->
                        match PdfUaTarget.ofSpecification f.Specification with
                        | Some standard -> titleRules.Contains((standard, f.RuleId))
                        | None -> false)

            let titleRow (review: DocumentReview) =
                let tidy (text: string) = Text.RegularExpressions.Regex.Replace(text, @"\s+", " ").Trim()
                let headingCandidate =
                    if isSuggested then
                        match review.Pages.TryGetValue 1 with
                        | true, first ->
                            first.Blocks
                            |> List.tryFind (fun block -> RegionEdits.tagOf edits.Current 1 block = "H1")
                            |> Option.bind (fun block ->
                                RegionEdits.textFor edits.Current 1 block
                                |> Option.orElse (Some block.Text)
                                |> Option.map tidy
                                |> Option.filter (String.IsNullOrWhiteSpace >> not))
                        | _ -> None
                    else
                        let rec firstHeading (node: StructureNode) : string option =
                            if node.Tag = "H1" then Option.ofObj node.Content |> Option.map tidy |> Option.filter (String.IsNullOrWhiteSpace >> not)
                            else node.Children |> Seq.tryPick firstHeading
                        firstHeading root.Current
                    |> Option.map (fun title -> { DocumentTitles.Title = title; DocumentTitles.Source = "first heading" })
                let candidates = documentCandidates @ Option.toList headingCandidate |> List.distinctBy _.Title
                let prefilled =
                    if isSuggested then candidates |> List.tryHead |> Option.map _.Title |> Option.defaultValue ""
                    else
                        // Its own title, only: one taken from its first page or heading is offered.
                        candidates
                        |> List.tryFind (fun c -> c.Source <> "first page" && c.Source <> "first heading")
                        |> Option.map _.Title
                        |> Option.defaultValue ""
                if isSuggested then review.ShownTitle <- (if prefilled = "" then None else Some prefilled)
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
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.verticalAlignment VerticalAlignment.Center
                            StackPanel.margin (0.0, 0.0, 10.0, 0.0)
                            StackPanel.children [
                                TextBlock.create [
                                    TextBlock.text "Document title"
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                    TextBlock.margin (0.0, 0.0, 6.0, 0.0)
                                ]
                                match findingsMarker titleFindings findings.Show with
                                | Some marker -> marker
                                | None -> ()
                            ]
                        ]
                        TextBox.create [
                            Grid.column 1
                            TextBox.text shownTitle
                            TextBox.placeHolderText "What a screen reader and the title bar call this document"
                            ToolTip.tip (
                                if isSuggested then "Given to the tagged copy as its title, shown in place of the file name. Save keeps a change next to the PDF."
                                else "Export… gives the copy this title, shown in place of the file name. Save keeps a change next to the PDF."
                            )
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
                                    + "its regions, the tag each suggests, and their reading order (the numbers on the page). "
                                    + "What you change is kept as edits on top: Save in the main window keeps them, and Export… tags a copy with them."
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
                                titleRow review
                        ]
                    ]
                    TabControl.create [
                        Grid.row 1
                        TabControl.viewItems [
                            // "Layout" is a suggested structure's: this is the elements drawn as blocks.
                            TabItem.create [ TabItem.header "Blocks"; TabItem.content layoutTab ]
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
/// findings are marked on the blocks they're about (a tagged PDF's; see FindingMarkers).
type StructureTreeWindow
    (source: StructureSource, sourceFilePath: string, review: DocumentReview, onEditsChanged: unit -> unit, findings: FindingMarkers.Findings) as this =
    inherit
        HostWindow(
            Title = (match source with Tagged _ -> "Structure" | Suggested -> "Suggested Structure"),
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
    // Where Order Page N by Layout gets a page's regions: the layout detector, unless replaced.
    let layoutRegions = ref None
    do
        let tagged = match source with Tagged root -> root | Suggested -> SuggestedTree.build []
        this.Content <- view this renderer tagged review (finding |> Option.map snd) sourceFilePath onEditsChanged findings layoutRegions

    /// The review its edits change.
    member _.Review = review

    /// Where Order Page N by Layout gets a page's regions, in reading order: None for the layout
    /// detector (downloading its model first, if need be).
    member _.LayoutRegions
        with get (): (int -> StructureGeometry list) option = layoutRegions.Value
        and set (value: (int -> StructureGeometry list) option) = layoutRegions.Value <- value

    /// Without findings to mark.
    new(source: StructureSource, sourceFilePath: string, review: DocumentReview, onEditsChanged: unit -> unit) =
        new StructureTreeWindow(source, sourceFilePath, review, onEditsChanged, FindingMarkers.none)

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
