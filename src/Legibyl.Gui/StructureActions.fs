/// What can be done to the Structure window's elements, as edits: alt text and replacement text,
/// another tag, merging, and moving among siblings in the reading order.
module internal Legibyl.Gui.StructureActions


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
open Legibyl.Gui.StructureColors
open Legibyl.Gui.StructurePreview

/// What can be edited on an element: its alt text, or its replacement text (/ActualText).
type EditKind =
    | AltText
    | ReplacementText
    /// A link's description (its annotation's /Contents).
    | LinkDescription


/// Whether it's a piece of what a page draws outside every tag.
let isUntagged (node: StructureNode) = node.Tag = "Untagged"

/// Whether a piece outside the tags is text, which is never made decoration.
let isUntaggedText (node: StructureNode) = (string node.Content).StartsWith "text "

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
                    e.Page = box.PageNumber && e.MergeWith |> List.exists (fun other -> Boxes.overlap other box >= RegionEdits.SameRegionOverlap))
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

/// What the reading-order bar works on, from the window.
type OrderBarNeeds =
    { /// The page shown on the preview: the selected block's, or the first.
      Page: int option
      Selected: StructureNode list
      /// Each element's parent.
      Parents: Collections.Generic.Dictionary<StructureNode, StructureNode>
      Edits: RegionEdits.Edit list
      /// Finding a page's layout, and the model's download (a fraction) while it downloads.
      Ordering: bool
      Download: float option
      /// Whether the document has one element its top-level ones are in.
      CanOrderByLayout: bool
      OrderByLayout: int -> unit
      SaveEdits: RegionEdits.Edit list -> unit }

// Above a tagged PDF's blocks: the reading order. The selected element moves one place among
// its siblings (on its page: not past one on another page, nor out of its parent); the
// shown page is put in the order its layout reads, or back in the order it was tagged in.
let orderBar (needs: OrderBarNeeds) : IView =
        let page = needs.Page
        let moving =
            match needs.Selected with
            | [ single ] when not (isNull single.Geometry) && not (isUntagged single) ->
                match needs.Parents.TryGetValue single with
                | true, parent when not (fixedOrder.Contains parent.Tag) ->
                    let order = orderedChildren needs.Edits parent
                    let index = order |> List.findIndex (fun n -> obj.ReferenceEquals(n, single))
                    // The sibling it moves past: the nearest with a place on a page (one with none,
                    // like a figure that draws nothing, stays where it is), if that's on its page.
                    let page = (nonNull single.Geometry).PageNumber
                    let past (indexes: int list) =
                        indexes
                        |> List.tryFind (fun i -> not (isNull order[i].Geometry))
                        |> Option.filter (fun i -> (nonNull order[i].Geometry).PageNumber = page)
                    Some(parent, order, index, past [ index - 1 .. -1 .. 0 ], past [ index + 1 .. order.Length - 1 ])
                | _ -> None
            | _ -> None
        let move (up: bool) =
            match moving with
            | Some(parent, order, index, before, after) ->
                match (if up then before else after) with
                | Some other ->
                    // Taken out and put in front of (or after) that sibling: its index, either
                    // way, once the moved one is out.
                    let target = order |> List.removeAt index |> List.insertAt other order[index]
                    recordOrder needs.Edits parent target |> needs.SaveEdits
                | None -> ()
            | None -> ()
        let moved = page |> Option.map (fun page -> needs.Edits |> List.filter (fun e -> e.Move.IsSome && e.Page = page)) |> Option.defaultValue []
        let pageName = page |> Option.map string |> Option.defaultValue ""
        WrapPanel.create [
            DockPanel.dock Dock.Top
            WrapPanel.margin (8.0, 0.0, 8.0, 6.0)
            WrapPanel.children [
                Button.create [
                    Button.content "Move Up"
                    Button.margin (0.0, 0.0, 6.0, 4.0)
                    Button.isEnabled (match moving with Some(_, _, _, before, _) -> before.IsSome | None -> false)
                    ToolTip.tip "Read the selected element before the one above it: one place earlier in the reading order, among the elements in the same one, on its page."
                    Button.onClick ((fun _ -> move true), SubPatchOptions.Always)
                ]
                Button.create [
                    Button.content "Move Down"
                    Button.margin (0.0, 0.0, 16.0, 4.0)
                    Button.isEnabled (match moving with Some(_, _, _, _, after) -> after.IsSome | None -> false)
                    ToolTip.tip "Read the selected element after the one below it: one place later in the reading order, among the elements in the same one, on its page."
                    Button.onClick ((fun _ -> move false), SubPatchOptions.Always)
                ]
                Button.create [
                    Button.content (
                        match needs.Ordering, needs.Download with
                        | true, Some fraction -> $"Downloading the layout model… {fraction:P0}"
                        | true, None -> $"Finding page {pageName}'s layout…"
                        | false, _ -> $"Order Page {pageName} by Layout"
                    )
                    Button.margin (0.0, 0.0, 6.0, 4.0)
                    Button.isEnabled (page.IsSome && not needs.Ordering && needs.CanOrderByLayout)
                    ToolTip.tip (
                        $"Put page {pageName} in the reading order its layout gives, found by a layout detector (several seconds; "
                        + $"downloads its model, about {LayoutModel.DownloadSizeMegabytes} MB, the first time), as moves you can see in the Edits tab. "
                        + "Never within a list or table, or across pages. Restore Page Original Order puts it back."
                    )
                    Button.onClick ((fun _ -> page |> Option.iter needs.OrderByLayout), SubPatchOptions.Always)
                ]
                if not moved.IsEmpty then
                    Button.create [
                        Button.content $"Restore Page {pageName} Original Order"
                        Button.margin (0.0, 0.0, 0.0, 4.0)
                        ToolTip.tip $"Drop every move on page {pageName}: its elements go back to the order they're tagged in."
                        Button.onClick (
                            (fun _ ->
                                moved
                                |> List.fold (fun all e -> RegionEdits.setMove all e.Page e.Tag e.Box None) needs.Edits
                                |> needs.SaveEdits),
                            SubPatchOptions.Always
                        )
                    ]
            ]
        ]

/// What the selection's actions work on, from the window.
type SelectionNeeds =
    { /// The selected elements (read as a handler runs, so a handler for an earlier selection
      /// does nothing).
      Selected: IReadable<StructureNode list>
      Edits: IReadable<RegionEdits.Edit list>
      /// Whether it's a suggested structure (not a tagged PDF).
      Suggested: bool
      SaveEdits: RegionEdits.Edit list -> System.Threading.Tasks.Task<unit>
      /// Marks not read every piece drawn outside the tags like this one (the same kind and size).
      MarkAllLike: StructureNode -> unit
      /// Opens the prompt for an element's alt text, replacement text or link description.
      EditNode: StructureNode -> EditKind -> unit }

/// Under the page preview: what can be done to the selection (its tag; merging; and for what's
/// drawn outside the tags, not read or tagged).
let selectionActions (needs: SelectionNeeds) : IView =
    // A block of text can be given another tag; paragraphs next to each
    // other can be merged into one.
    StackPanel.create [
      Grid.row 2
      StackPanel.children [
    // What the Tag picker tags: a tagged PDF's block of text, or its figures (Figure,
    // or Artifact for decoration not read), several at once; a suggested
    // structure's selected regions, several at once (Artifact for those not read).
    let figures = needs.Selected.Current |> List.filter (fun n -> n.Tag = "Figure" && not (isNull n.Geometry))
    let tagging =
        match needs.Selected.Current with
        | _ when needs.Suggested -> regionsIn needs.Selected.Current |> List.filter (canRetag true)
        | [ single ] when canRetag false single -> [ single ]
        | chosen when not figures.IsEmpty && figures.Length = chosen.Length -> figures
        | _ -> []
    match tagging with
    | [] -> ()
    | targets ->
        let tagOf (node: StructureNode) = editFor needs.Edits.Current node |> Option.bind _.Retag |> Option.defaultValue node.Tag
        // Their tag, when they share one.
        let tag =
            match targets |> List.map tagOf |> List.distinct with
            | [ tag ] -> Some tag
            | _ -> None
        let chosen = needs.Selected.Current
        let tags =
            if needs.Suggested then LayoutFile.tags
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
                         | _ when needs.Suggested -> $"Tag for these {targets.Length} regions"
                         | _ -> $"Tag for these {targets.Length} elements"),
                        ValueNone)
                    ToolTip.tip (
                        if needs.Suggested then
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
                            | :? string as picked when Some picked <> tag && obj.ReferenceEquals(needs.Selected.Current, chosen) ->
                                targets
                                |> List.fold
                                    (fun edits (node: StructureNode) ->
                                        let box = nonNull node.Geometry
                                        let retagged = RegionEdits.setRetag edits box.PageNumber node.Tag box picked
                                        // An artifact isn't read, so it isn't merged into a neighbour either.
                                        if needs.Suggested && picked = "Artifact" then RegionEdits.setExcluded retagged box.PageNumber node.Tag box false
                                        else retagged)
                                    needs.Edits.Current
                                |> needs.SaveEdits
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
    match (if needs.Suggested then regionsIn needs.Selected.Current else []) with
    | [] -> ()
    | regions ->
        let what = if regions.Length = 1 then "" else $" {regions.Length} Regions"
        let merged = regions |> List.forall (fun r -> editFor needs.Edits.Current r |> Option.exists _.Exclude)
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
                        needs.Edits.Current
                    |> needs.SaveEdits
                    |> ignore),
                SubPatchOptions.Always
            )
        ]
    // What's drawn outside every tag: made decoration (never text), or tagged as a
    // figure or a paragraph, as a person chooses; several at once.
    match (if needs.Suggested then [] else needs.Selected.Current |> List.filter (fun n -> isUntagged n && not (isNull n.Geometry))) with
    | [] -> ()
    | pieces ->
        let chosenFor (n: StructureNode) = editFor needs.Edits.Current n |> Option.bind _.Retag
        let text = pieces |> List.filter isUntaggedText
        let what = if pieces.Length = 1 then "" else $" {pieces.Length} Pieces"
        let setAll (retag: string) =
            pieces
            |> List.fold (fun all n -> let box = nonNull n.Geometry in RegionEdits.setRetag all box.PageNumber n.Tag box retag) needs.Edits.Current
            |> needs.SaveEdits
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
                    action "Edit Alt Text…" "What a screen reader says for it: what it's for here, in a sentence." (fun () -> needs.EditNode single AltText)
                | [ single ] when not (isUntaggedText single) ->
                    action
                        "Mark All Like This…"
                        "Every piece like this one (the same kind, about the same size, on any page) as decoration, once you've seen the list."
                        (fun () -> needs.MarkAllLike single)
                | _ -> ()
            ]
        ]
    // Merging is for a tagged PDF's paragraphs, and figures drawn in pieces.
    let mergeable nodes = if needs.Suggested then None else mergeable nodes
    let pendingMerge =
        match mergeable needs.Selected.Current with
        | Some(first :: rest) ->
            let box = nonNull first.Geometry
            RegionEdits.findMerge needs.Edits.Current box.PageNumber box
            |> Option.filter (fun m -> m.MergeWith.Length = rest.Length)
        | _ -> None
    match mergeable needs.Selected.Current, pendingMerge with
    | Some _, Some merge ->
        Button.create [
            Button.content "Don't Merge These"
            Button.margin (0.0, 0.0, 0.0, 8.0)
            ToolTip.tip "Drop this merge: they stay separate."
            Button.onClick (
                (fun _ -> needs.SaveEdits (needs.Edits.Current |> List.filter (fun e -> not (obj.ReferenceEquals(e, merge)))) |> ignore),
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
                    needs.SaveEdits (RegionEdits.mergeElements needs.Edits.Current boxes.Head.PageNumber nodes.Head.Tag boxes) |> ignore),
                SubPatchOptions.Always
            )
        ]
    | None, _ ->
        match needs.Selected.Current with
        | [ single ] when single.Tag = "P" && not needs.Suggested ->
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
