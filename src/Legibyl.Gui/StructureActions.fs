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
    /// Another annotation's description (a comment's, a stamp's, a highlight's: its /Contents).
    | Description
    /// A form field's tooltip (its /TU): what a screen reader says when it reaches the field.
    | Tooltip


/// Whether it's a piece of what a page draws outside every tag.
let isUntagged (node: StructureNode) = node.Tag = "Untagged"

/// Whether a piece outside the tags is text, which is never made decoration.
let isUntaggedText (node: StructureNode) = (string node.Content).StartsWith "text "

/// Whether a character (by its UTF-16 unit) is in one of Unicode's private use areas, as an icon
/// font's characters often are: the Basic one (U+E000 to U+F8FF), or the planes above (by their
/// surrogates).
let private isIconUnit (c: char) = (c >= '\uE000' && c <= '\uF8FF') || (c >= '\uDB80' && c <= '\uDBFF')

/// Whether text uses icon characters (Unicode's private use areas), which mean nothing to a
/// screen reader without replacement text.
let hasIconCharacters (text: string | null) =
    match text with
    | NonNull text -> text |> Seq.exists isIconUnit
    | Null -> false

/// text without its icon characters (and the low surrogate after one above the Basic plane,
/// where there is one: a lone high surrogate goes alone).
let withoutIconCharacters (text: string) =
    let kept = Text.StringBuilder()
    let mutable i = 0
    while i < text.Length do
        let c = text[i]
        if c >= '\uDB80' && c <= '\uDBFF' then i <- i + (if i + 1 < text.Length && Char.IsLowSurrogate text[i + 1] then 2 else 1)
        elif isIconUnit c then i <- i + 1
        else
            kept.Append c |> ignore
            i <- i + 1
    Text.RegularExpressions.Regex.Replace(kept.ToString(), "\\s+", " ").Trim()

/// The edits an element can take. Alt text: figures and formulas, and anything that already
/// has alt text. Replacement text: what already has it (like OCR'd text), and text that uses
/// icon characters a screen reader can't read; not other text, since writing it over real text
/// would hide what the page says. Either needs the element's place on its page, which is how an
/// edit finds it again.
let editKinds (node: StructureNode) =
    // (One of the PDF's own tag names by what its role map makes it: an Image mapped to Figure.)
    let tag = match node.Role with NonNull role -> role | Null -> node.Tag
    match node.Geometry with
    | _ when node.Tag = "Disconnected" || node.Tag = "DisconnectedTag" -> []
    | null -> []
    | _ ->
        [ if tag = "Link" then LinkDescription
          elif tag = "Annot" then Description
          elif tag = "Form" then Tooltip
          elif tag = "Figure" || tag = "Formula" || not (isNull node.AlternateDescription) then AltText
          if not (isNull node.ActualText) || hasIconCharacters node.Content then ReplacementText ]

/// What editing it is called: on the selected element's buttons, and its block's right-click menu.
let editKindLabel (kind: EditKind) =
    match kind with
    | AltText -> "Edit Alt Text…"
    | ReplacementText -> "Edit Replacement Text…"
    | LinkDescription -> "Edit Link Description…"
    | Description -> "Edit Description…"
    | Tooltip -> "Edit Tooltip…"

/// A form field's name as a tooltip might say it: its last part, without an index, in words
/// ("topmostSubform[0].Page1[0].first_name[0]" is "First name", "EmailAddress" "Email address").
let fieldNameText (name: string) =
    let last = name.Split('.') |> Array.last
    let plain = Text.RegularExpressions.Regex.Replace(last, "\\[\\d+\\]$", "")
    let spaced = Text.RegularExpressions.Regex.Replace(plain, "(?<=[a-z0-9])(?=[A-Z])", " ").Replace('_', ' ').Replace('-', ' ')
    let words = spaced.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    match List.ofArray words with
    | [] -> ""
    | first :: rest ->
        String.Join(" ", (first.Substring(0, 1).ToUpperInvariant() + first.Substring(1).ToLowerInvariant()) :: (rest |> List.map _.ToLowerInvariant()))

/// The tags a block of text can be given instead, e.g. a heading tagged as a paragraph.
let offeredRetags = [ "H1"; "H2"; "H3"; "H4"; "H5"; "H6"; "P"; "Caption"; "Note"; "BlockQuote" ]

/// The tag a screen reader hears for node: one of the PDF's own tag names by the standard tag
/// chosen for it (roleMap, the Role map tab's), else by its role map's; a standard tag, itself.
let heardTag (roleMap: Map<string, string>) (node: StructureNode) =
    if Tags.standard.Contains node.Tag then node.Tag
    else roleMap.TryFind node.Tag |> Option.orElse (Option.ofObj node.Role) |> Option.defaultValue node.Tag

/// Whether node, which a screen reader hears as heard, can be given another tag.
let private canRetagAs (suggested: bool) (heard: string) (node: StructureNode) =
    not (isNull node.Geometry)
    && (if suggested then node.Tag <> "Link" else heard = "H" || List.contains heard offeredRetags)

/// Whether node can be given another tag: in a tagged PDF, a block of text (one of the PDF's own
/// tag names by its role map's tag); in a suggested layout, any region (it needs its place on its
/// page, which is how the edit finds it again).
let canRetag (suggested: bool) (node: StructureNode) = canRetagAs suggested (heardTag Map.empty node) node

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
let fixedOrder = set [ "Table"; "THead"; "TBody"; "TFoot"; "TR"; "TOC"; "StructTreeRoot"; "NotInTags"; "Disconnected" ]

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
        | Some merge when merge.Retag = Some "Table" && not merge.Cells.IsEmpty ->
            Some $"✎ converting {merge.MergeWith.Length + 1} paragraphs to a table, not in the PDF yet"
        | Some merge ->
            let others = merge.MergeWith.Length
            Some(if others = 1 then $"✎ merged with the next {noun}, not in the PDF yet" else $"✎ merged with the next {others} {noun}s, not in the PDF yet")
        | None ->
            let converted =
                edits
                |> List.exists (fun e ->
                    e.Page = box.PageNumber
                    && e.Retag = Some "Table"
                    && not e.Cells.IsEmpty
                    && e.MergeWith |> List.exists (fun other -> Boxes.overlap other box >= RegionEdits.SameRegionOverlap))
            if converted then Some "✎ included in a table conversion, not in the PDF yet"
            else
                let merged =
                    edits
                    |> List.exists (fun e ->
                        e.Page = box.PageNumber
                        && e.MergeWith |> List.exists (fun other -> Boxes.overlap other box >= RegionEdits.SameRegionOverlap))
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

/// Paragraphs Convert to Table is offered for: two or more on one page, without alt text. Unlike
/// merging, replacement text (/ActualText) doesn't rule them out: the conversion goes ahead when
/// it says only what they draw, and says why not otherwise.
let convertibleToTable (roleMap: Map<string, string>) (nodes: StructureNode list) =
    match nodes with
    | first :: _ :: _ when
        nodes
        |> List.forall (fun n ->
            // A paragraph (or a name of the PDF's own for one), with no alt text that says anything.
            heardTag roleMap n = "P"
            && String.IsNullOrWhiteSpace n.AlternateDescription
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
      // A link's or another annotation's is its description; a form field's, its tooltip.
      | Some alt when (node.Tag = "Link" || node.Tag = "Annot") && alt <> string node.LinkDescription -> "description", alt
      | Some alt when node.Tag = "Form" && alt <> string node.FormFieldDescription -> "tooltip", alt
      | Some alt when node.Tag <> "Link" && node.Tag <> "Annot" && node.Tag <> "Form" && alt <> string node.AlternateDescription -> "alt", alt
      | _ -> ()
      match edit.Text with
      | Some text when text <> string node.ActualText -> "text", text
      | _ -> ()
      if not edit.Cells.IsEmpty then
          if edit.Tag = "P" && edit.Retag = Some "Table" then "table conversion", $"checked grid ({edit.Cells.Length} cells)"
          else "rows and cells", $"rebuilt from the page ({edit.Cells.Length} cells)" ]

/// What the reading-order buttons work on, from the window.
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

/// One thing that can be done to the selection: drawn as a button under Selected element, and as
/// an item on a block's right-click menu, so the two offer the same.
type Action =
    { Label: string
      /// The pencil for an edit a person makes; the wand for one the layout detector suggests.
      Icon: Geometry
      Tip: string
      Enabled: bool
      /// Its keys, if it has any (a button's shortcut; shown on the menu).
      Keys: KeyGesture option
      Run: unit -> unit }

let private action (label: string) (tip: string) (run: unit -> unit) =
    { Label = label; Icon = Icons.pencil; Tip = tip; Enabled = true; Keys = None; Run = run }

/// A button that edits the structure (the edit goes into the PDF on Export), its icon before its
/// label, so it's told from the buttons that only go somewhere. Named for a screen reader by its
/// label.
let private editButtonWith (icon: Geometry) (label: string) (attrs: IAttr<Button> list) : IView =
    Button.create [
        Button.content (
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.children [
                    Icons.decorativeIcon icon Theme.Tokens.Primary 12.0
                    TextBlock.create [ TextBlock.text label; TextBlock.verticalAlignment VerticalAlignment.Center ]
                ]
            ]
        )
        AttrBuilder<Button>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, label, ValueNone)
        yield! attrs
    ]

/// An edit a person makes: a pencil, as the Edits tab marks edits.
let editButton = editButtonWith Icons.pencil

/// An edit suggested from the page's layout, by the layout detector: the wand, as Suggest
/// Structure has.
let suggestButton = editButtonWith Icons.wand

/// An action as a button (its keys its shortcut).
let private actionButton (margin: Thickness) (a: Action) : IView =
    editButtonWith a.Icon a.Label [
        Button.margin margin
        Button.isEnabled a.Enabled
        ToolTip.tip a.Tip
        match a.Keys with
        | Some keys -> AttrBuilder<Button>.CreateProperty<KeyGesture | null>(Button.HotKeyProperty, keys, ValueNone)
        | None -> ()
        Button.onClick ((fun _ -> a.Run()), SubPatchOptions.Always)
    ]

/// An action as a menu item (its keys shown, not taken: the button has them).
let private actionItem (a: Action) =
    let item = MenuItem(Header = a.Label, IsEnabled = a.Enabled)
    a.Keys |> Option.iter (fun keys -> item.InputGesture <- keys)
    ToolTip.SetTip(item, a.Tip)
    item.Click.Add(fun _ -> a.Run())
    item

/// The selected element's place among its siblings (on its page: not past one on another page,
/// nor out of its parent): its parent, their order, its index, and the index of the sibling it
/// would move past going up and going down, if it can.
let private moving (needs: OrderBarNeeds) =
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

/// For a tagged PDF's selected element: one place earlier or later in the reading order.
let moveActions (needs: OrderBarNeeds) : Action list =
    let moving = moving needs
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
    [ { action
            "Move Up"
            "Read the selected element before the one above it (Alt+Up): one place earlier in the reading order, among the elements in the same one, on its page."
            (fun () -> move true) with
          Enabled = (match moving with Some(_, _, _, before, _) -> before.IsSome | None -> false)
          Keys = Some(KeyGesture(Key.Up, KeyModifiers.Alt)) }
      { action
            "Move Down"
            "Read the selected element after the one below it (Alt+Down): one place later in the reading order, among the elements in the same one, on its page."
            (fun () -> move false) with
          Enabled = (match moving with Some(_, _, _, _, after) -> after.IsSome | None -> false)
          Keys = Some(KeyGesture(Key.Down, KeyModifiers.Alt)) } ]
    |> List.filter (fun _ -> needs.Selected |> List.forall (fun node -> node.Tag <> "Disconnected" && node.Tag <> "DisconnectedTag"))

// For a tagged PDF's shown page: put in the order its layout reads, or back in the order it was
// tagged in.
let pageOrderButtons (needs: OrderBarNeeds) : IView =
        let page = needs.Page
        let moved = page |> Option.map (fun page -> needs.Edits |> List.filter (fun e -> e.Move.IsSome && e.Page = page)) |> Option.defaultValue []
        let pageName = page |> Option.map string |> Option.defaultValue ""
        WrapPanel.create [
            WrapPanel.children [
                suggestButton
                    (match needs.Ordering, needs.Download with
                     | true, Some fraction -> $"Downloading the layout model… {fraction:P0}"
                     | true, None -> $"Finding page {pageName}'s layout…"
                     | false, _ -> $"Order Page {pageName} by Layout")
                    [ Button.margin (0.0, 0.0, 6.0, 8.0)
                      Button.isEnabled (page.IsSome && not needs.Ordering && needs.CanOrderByLayout)
                      ToolTip.tip (
                          $"Put page {pageName} in the reading order its layout gives, found by a layout detector (several seconds"
                          + (match LayoutModel.downloadMegabytes () with
                             | 0 -> ")"
                             | size -> $"; downloads its models, about {size} MB, the first time)")
                          + ", as moves you can see in the Edits tab. "
                          + $"Never within a list or table, or across pages. Restore Page {pageName} Original Order, shown once it's moved, puts it back."
                      )
                      Button.onClick ((fun _ -> page |> Option.iter needs.OrderByLayout), SubPatchOptions.Always) ]
                if not moved.IsEmpty then
                    editButton $"Restore Page {pageName} Original Order" [
                        Button.margin (0.0, 0.0, 0.0, 8.0)
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
      EditNode: StructureNode -> EditKind -> unit
      /// The standard tag chosen for each of the PDF's own tag names (the Role map tab).
      RoleMap: unit -> Map<string, string>
      /// Rebuilds a tagged table's rows and cells from the page (the table-structure model), for
      /// a person to check.
      RebuildTable: StructureNode -> unit
      /// Converts selected sibling paragraphs into a table after its detected cells are checked.
      ConvertParagraphsToTable: StructureNode list -> unit
      /// Replaces what's drawn in a region of a page with one Figure (see FigureRegion), after
      /// asking for its alt text, as drawing the region does.
      ReplaceWithFigure: StructureGeometry -> unit }

/// What a screen reader says for the selected element (alt text, replacement text, a link's
/// description): only those it has, for one element at a time. (F2 on its block opens the first.)
let textActions (needs: SelectionNeeds) : Action list =
    match needs.Selected.Current with
    | [ single ] ->
        [ for kind in editKinds single ->
              let inPdf =
                  match kind with
                  | AltText -> single.AlternateDescription
                  | ReplacementText -> single.ActualText
                  | LinkDescription | Description -> single.LinkDescription
                  | Tooltip -> single.FormFieldDescription
              let yours =
                  editFor needs.Edits.Current single
                  |> Option.bind (fun e -> match kind with ReplacementText -> e.Text | AltText | LinkDescription | Description | Tooltip -> e.Alt)
              action
                  (editKindLabel kind)
                  (match yours, inPdf with
                   | Some text, _ -> $"What a screen reader says for it (F2). Your edit, not in the PDF yet: \"{text}\""
                   | None, NonNull text when not (String.IsNullOrWhiteSpace text) -> $"What a screen reader says for it (F2). In the PDF now: \"{text}\""
                   | None, _ -> "What a screen reader says for it (F2). It has none in the PDF now.")
                  (fun () -> needs.EditNode single kind) ]
    | _ -> []

/// What the Tag picker offers for the selection: what it tags, the tags, the one they share (if
/// they do), what it's called, and giving them a tag.
type TagChoice =
    { Targets: StructureNode list
      Tags: string list
      Current: string option
      Name: string
      Tip: string
      Pick: string -> unit }

/// What the Tag picker tags: a tagged PDF's block of text, or its figures (Figure, or Artifact for
/// decoration not read), several at once; a suggested structure's selected regions, several at
/// once (Artifact for those not read).
let tagChoice (needs: SelectionNeeds) : TagChoice option =
    // What each is heard as: one of the PDF's own tag names by the tag chosen for it, else its map's.
    let roleMap = if needs.Suggested then Map.empty else needs.RoleMap ()
    let heard = heardTag roleMap
    let figures = needs.Selected.Current |> List.filter (fun n -> heard n = "Figure" && not (isNull n.Geometry))
    let tagging =
        match needs.Selected.Current with
        | _ when needs.Suggested -> regionsIn needs.Selected.Current |> List.filter (canRetag true)
        | [ single ] when canRetagAs false (heard single) single -> [ single ]
        | chosen when not figures.IsEmpty && figures.Length = chosen.Length -> figures
        | _ -> []
    match tagging with
    | [] -> None
    | targets ->
        // One of the PDF's own tag names shows what it's mapped to: "Heading1 → H1", what a screen
        // reader hears.
        let named (node: StructureNode) =
            match heard node with
            | tag when tag <> node.Tag -> $"{node.Tag} → {tag}"
            | _ -> node.Tag
        let tagOf (node: StructureNode) = editFor needs.Edits.Current node |> Option.bind _.Retag |> Option.defaultValue (named node)
        // Their tag, when they share one.
        let tag =
            match targets |> List.map tagOf |> List.distinct with
            | [ tag ] -> Some tag
            | _ -> None
        let tags =
            // A table's cell is a header cell or a data cell: anything else would leave a hole in it.
            if needs.Suggested && targets |> List.forall (fun n -> TableGrid.isCell n.Tag) then [ "TH"; "TD" ]
            elif needs.Suggested then LayoutFile.tags
            elif targets |> List.forall (fun n -> heard n = "Figure") then [ "Figure"; "Artifact" ]
            else offeredRetags
        Some
            { Targets = targets
              Tags = (match tag with Some tag when not (List.contains tag tags) -> tag :: tags | _ -> tags)
              Current = tag
              Name =
                (match targets with
                 | [ single ] -> $"Tag for this <{single.Tag}>"
                 | _ when needs.Suggested -> $"Tag for these {targets.Length} regions"
                 | _ -> $"Tag for these {targets.Length} elements")
              Tip =
                (if needs.Suggested then
                     "The tag to give it: H1 (the title), H (a heading, given the next level under the title when tagged, since one page can't tell), H2 to H6, P, LI (a list item), Figure, Table, Caption, Note, Formula, Lbl (a label), or Artifact (page furniture, not read)."
                 elif targets |> List.forall (fun n -> heard n = "Figure") then
                     "Figure, or Artifact for decoration a screen reader should skip, like a rule, a background or a border: on export, it's still drawn but marked as an artifact, and leaves the tags."
                 else
                     "Give this element another tag, e.g. a heading tagged as a paragraph.")
              Pick =
                fun picked ->
                    if Some picked <> tag then
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
                        |> ignore }

/// The rest: a suggested region merged into its neighbour (its text still read) or kept again;
/// what's drawn outside every tag made decoration, or tagged as a figure or a paragraph; a tagged
/// PDF's paragraphs, or a figure drawn in pieces, merged into one, or kept apart again.
let otherActions (needs: SelectionNeeds) : Action list =
    // A tagged table's rows and cells, rebuilt from how it looks (the wand: the model suggests them).
    [ match needs.Selected.Current with
      | _ when not needs.Suggested ->
          match convertibleToTable (needs.RoleMap()) needs.Selected.Current with
          | Some (first :: _ :: _ as nodes) ->
              let savedConversion =
                  match first.Geometry with
                  | NonNull box ->
                      RegionEdits.findMerge needs.Edits.Current box.PageNumber box
                      |> Option.exists (fun edit -> edit.Retag = Some "Table" && not edit.Cells.IsEmpty && edit.MergeWith.Length = nodes.Length - 1)
                  | Null -> false
              { action
                    (if savedConversion then "Check Table Conversion…" else $"Convert {nodes.Length} Paragraphs to Table…")
                    (if savedConversion then
                         "Review the grid and header choices saved for these paragraphs, or revise them before Export."
                     else
                         "Finds the rows and cells in these paragraphs' page region (the table-structure model), for you to check. Export then replaces the paragraph tags with a semantic table.")
                    (fun () -> needs.ConvertParagraphsToTable nodes) with
                  Icon = Icons.wand }
          | _ -> ()
      | _ -> ()
      match needs.Selected.Current with
      | [ single ] when not needs.Suggested && heardTag (needs.RoleMap ()) single = "Table" && not (isNull single.Geometry) ->
          let rebuilt = editFor needs.Edits.Current single |> Option.exists (fun e -> not e.Cells.IsEmpty)
          { action
                (if rebuilt then "Check Rebuilt Rows and Cells…" else "Rebuild Rows and Cells from the Page…")
                (if rebuilt then "The rows and cells this table is to be rebuilt as: check each cell is a header cell (TH) or a data cell (TD)."
                 else "Finds the table's rows, columns, header and spanning cells from how it looks (the table-structure model), for you to check; Export… then tags it as those, each piece of it in the cell it's drawn in. For a table tagged wrongly: cells outside rows, rows of different lengths.")
                (fun () -> needs.RebuildTable single) with
              Icon = Icons.wand }
      | _ -> ()
      // (Not a table's cell: merged into its neighbour, it would leave a hole in the table.)
      match (if needs.Suggested then regionsIn needs.Selected.Current |> List.filter (fun r -> not (TableGrid.isCell r.Tag)) else []) with
      | [] -> ()
      | regions ->
          let what = if regions.Length = 1 then "" else $" {regions.Length} Regions"
          let merged = regions |> List.forall (fun r -> editFor needs.Edits.Current r |> Option.exists _.Exclude)
          action
              (if merged then $"Don't Merge{what}" else $"Merge into Neighbour{what}")
              (if merged then "Keep it as a region of its own."
               else "Not a region of its own: its text joins the nearest region kept, so it's still read; its images and lines are decoration. To keep it from being read at all, give it the tag Artifact.")
              (fun () ->
                  regions
                  |> List.fold
                      (fun all r ->
                          let box = nonNull r.Geometry
                          RegionEdits.setExcluded all box.PageNumber r.Tag box (not merged))
                      needs.Edits.Current
                  |> needs.SaveEdits
                  |> ignore)
      match (if needs.Suggested then [] else needs.Selected.Current |> List.filter (fun n -> isUntagged n && not (isNull n.Geometry))) with
      | [] -> ()
      | pieces ->
          let chosenFor (n: StructureNode) = editFor needs.Edits.Current n |> Option.bind _.Retag
          let text = pieces |> List.filter isUntaggedText
          let what = if pieces.Length = 1 then "" else $" {pieces.Length} Pieces"
          let setAll (retag: string) () =
              pieces
              |> List.fold (fun all n -> let box = nonNull n.Geometry in RegionEdits.setRetag all box.PageNumber n.Tag box retag) needs.Edits.Current
              |> needs.SaveEdits
              |> ignore
          if text.IsEmpty then
              action
                  $"Not Read (Artifact){what}"
                  "Decoration a screen reader should skip, like a background, a rule or a border: on export it's still drawn, but marked as an artifact."
                  (setAll "Artifact")
              action
                  $"Tag as Figure{what}"
                  "It's content, like a chart or a diagram: on export it's tagged as a figure, which then needs alt text."
                  (setAll "Figure")
          elif text.Length = pieces.Length then
              action $"Tag as Paragraph{what}" "Text should be read: on export it's tagged as a paragraph, in reading order after what's tagged before it." (setAll "P")
          if pieces |> List.exists (fun n -> (chosenFor n).IsSome) then
              action "Keep Untagged" "Drop what was chosen for it: it stays outside the tags." (setAll "Untagged")
          match pieces with
          | [ single ] when chosenFor single = Some "Figure" ->
              action "Edit Alt Text…" "What a screen reader says for it: what it's for here, in a sentence." (fun () -> needs.EditNode single AltText)
          | [ single ] when not (isUntaggedText single) ->
              action
                  "Mark All Like This…"
                  "Every piece like this one (the same kind, about the same size, on any page) as decoration, once you've seen the list."
                  (fun () -> needs.MarkAllLike single)
          | _ -> ()
      // Merging is for a tagged PDF's paragraphs, and figures drawn in pieces.
      let mergeable nodes = if needs.Suggested then None else mergeable nodes
      let pendingMerge =
          match mergeable needs.Selected.Current with
          | Some(first :: rest) ->
              let box = nonNull first.Geometry
              RegionEdits.findMerge needs.Edits.Current box.PageNumber box
              |> Option.filter (fun m -> m.MergeWith.Length = rest.Length && (m.Retag <> Some "Table" || m.Cells.IsEmpty))
          | _ -> None
      let conversion =
          match (if needs.Suggested then None else convertibleToTable (needs.RoleMap()) needs.Selected.Current), pendingMerge with
          | Some(first :: rest), None when needs.Edits.Current |> List.exists (fun edit ->
                  edit.Page = (nonNull first.Geometry).PageNumber
                  && edit.Retag = Some "Table"
                  && not edit.Cells.IsEmpty
                  && Boxes.overlap edit.Box (nonNull first.Geometry) >= RegionEdits.SameRegionOverlap
                  && edit.MergeWith.Length = rest.Length) ->
              Some first
          | _ -> None
      match conversion, mergeable needs.Selected.Current, pendingMerge with
      | Some first, _, _ ->
          action "Don't Convert to Table" "Drop this conversion: the selected paragraphs stay separate." (fun () ->
              needs.SaveEdits (needs.Edits.Current |> List.filter (fun edit ->
                  not (edit.Retag = Some "Table" && edit.Page = (nonNull first.Geometry).PageNumber && Boxes.overlap edit.Box (nonNull first.Geometry) >= RegionEdits.SameRegionOverlap))) |> ignore)
      | None, Some _, Some merge ->
          action "Don't Merge These" "Drop this merge: they stay separate." (fun () ->
              needs.SaveEdits (needs.Edits.Current |> List.filter (fun e -> not (obj.ReferenceEquals(e, merge)))) |> ignore)
      | None, Some nodes, None ->
          let figures = nodes.Head.Tag = "Figure"
          action
              (if figures then $"Merge {nodes.Length} Figures" else $"Merge {nodes.Length} Paragraphs")
              (if figures then "Make these one figure: a picture drawn in pieces. Then give the first alt text, or the tag Artifact, for all of them."
               else "Make these paragraphs one.")
              (fun () ->
                  let boxes = nodes |> List.map (fun p -> nonNull p.Geometry)
                  needs.SaveEdits (RegionEdits.mergeElements needs.Edits.Current boxes.Head.PageNumber nodes.Head.Tag boxes) |> ignore)
      | None, None, _ -> ()
      // The keyboard's way to Replace Structure with Figure from Region: the region round what
      // the selected elements, on one page, draw. For several elements, or one holding others (a
      // chart's group or section); not for one paragraph alone.
      match needs.Selected.Current with
      | nodes when
          not needs.Suggested
          && (match nodes with
              | [] -> false
              | [ single ] -> single.Children.Count > 0
              | _ -> true)
          && nodes |> List.forall (fun n -> not (isNull n.Geometry) && not (List.contains n.Tag [ "StructTreeRoot"; "NotInTags"; "Disconnected"; "DisconnectedTag" ]))
          && (nodes |> List.map (fun n -> (nonNull n.Geometry).PageNumber) |> List.distinct |> List.length) = 1
          ->
          let boxes = nodes |> List.map (fun n -> nonNull n.Geometry)
          // Round what they draw, with room: the elements' boxes are their glyphs' tight boxes,
          // while what goes in is measured by the font's whole height and a stroke's width.
          let room = 4.0
          let region =
              { PageNumber = boxes.Head.PageNumber
                Left = max 0.0 ((boxes |> List.map _.Left |> List.min) - room)
                Bottom = max 0.0 ((boxes |> List.map _.Bottom |> List.min) - room)
                Right = (boxes |> List.map _.Right |> List.max) + room
                Top = (boxes |> List.map _.Top |> List.max) + room }
          action
              "Replace with Figure…"
              "Make what the selection draws one Figure, with alt text you give: for a chart or diagram tagged as many paragraphs and spans. The same as drawing a region round it (Replace Structure with Figure from Region…)."
              (fun () -> needs.ReplaceWithFigure region)
      | _ -> () ]

/// Under Selected element: what can be done to the selection (what a screen reader says for it;
/// its place in the reading order, moves; its tag; merging; and for what's drawn outside the tags,
/// not read or tagged), in the order its right-click menu has them.
let selectionActions (needs: SelectionNeeds) (moves: Action list) : IView =
    let row (actions: Action list) : IView list =
        match actions with
        | [] -> []
        | actions -> [ WrapPanel.create [ WrapPanel.children [ for a in actions -> actionButton (Thickness(0.0, 0.0, 6.0, 8.0)) a ] ] ]
    StackPanel.create [
      StackPanel.children [
        yield! row (textActions needs)
        yield! row moves
        match tagChoice needs with
        | None -> ()
        | Some choice ->
            let chosen = needs.Selected.Current
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
                        ComboBox.dataItems choice.Tags
                        // Nothing chosen when they don't share a tag.
                        ComboBox.selectedIndex (match choice.Current with Some tag -> List.findIndex ((=) tag) choice.Tags | None -> -1)
                        AttrBuilder<ComboBox>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, choice.Name, ValueNone)
                        ToolTip.tip choice.Tip
                        // Each render's handler, for the element selected now; and
                        // only while it is (moving the selection sets the picker to
                        // the next element's tag before its handler replaces this).
                        ComboBox.onSelectedItemChanged (
                            (fun item ->
                                match item with
                                | :? string as picked when obj.ReferenceEquals(needs.Selected.Current, chosen) -> choice.Pick picked
                                | _ -> ()),
                            SubPatchOptions.Always
                        )
                    ]
                ]
            ]
        yield! row (otherActions needs)
        match needs.Selected.Current with
        | [ single ] when single.Tag = "P" && not needs.Suggested && (otherActions needs).IsEmpty ->
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

/// The selection's right-click menu: what Selected element offers, the same (its Tag as a submenu,
/// a tick by the one it has), in groups.
let selectionMenu (needs: SelectionNeeds) (moves: Action list) : Control list =
    let groups =
        [ [ for a in textActions needs -> actionItem a :> Control ]
          [ for a in moves -> actionItem a :> Control ]
          [ match tagChoice needs with
            | Some choice ->
                let tag = MenuItem(Header = "Tag")
                ToolTip.SetTip(tag, choice.Tip)
                for name in choice.Tags do
                    let item = MenuItem(Header = name, ToggleType = MenuItemToggleType.Radio, IsChecked = (choice.Current = Some name))
                    item.Click.Add(fun _ -> choice.Pick name)
                    tag.Items.Add item |> ignore
                tag :> Control
            | None -> () ]
          [ for a in otherActions needs -> actionItem a :> Control ] ]
        |> List.filter (List.isEmpty >> not)
    groups
    |> List.mapi (fun i group ->
        [ if i > 0 then Separator() :> Control
          yield! group ])
    |> List.concat
