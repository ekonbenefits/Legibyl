/// The Structure window's Edits tab: every edit made for the PDF, each saying whether it's in the
/// PDF yet (or, for a suggested structure, whether tagging will use it), with Remove.
module internal Legibyl.Gui.StructureEditsTab

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
open Legibyl.Gui.StructureActions
open Legibyl.Gui.StructureBlocks

/// What the tab shows and does, from the window.
type Needs =
    { Edits: IReadable<RegionEdits.Edit list>
      Root: IReadable<StructureNode>
      Review: DocumentReview
      /// A suggested structure's review (whose pages are found as the window is open); None for a
      /// tagged PDF.
      Suggested: (DocumentReview * LayoutPrefetcher) option
      SourceFilePath: string
      /// Whether a moved element isn't in that place in the PDF yet.
      MovePending: StructureNode -> bool
      /// Makes these the review's edits.
      SaveEdits: RegionEdits.Edit list -> unit }

/// Every edit for this PDF, and whether it's in the PDF yet.
let view (needs: Needs) : IView =
    Component.create("structure-edits", fun ctx ->
        let edits = ctx.usePassedRead needs.Edits
        let root = ctx.usePassedRead needs.Root
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
                    Some(node, Boxes.overlap geometry edit.Box)
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
                        Boxes.overlap own box >= RegionEdits.SameRegionOverlap
                    | _ -> false)
            let status =
                match needs.Suggested with
                | _ when carried -> "In this PDF: from the review saved inside it, which is read-only."
                | Some(review, _) ->
                    match elementFor edit with
                    | Some _ -> "Export… tags a copy with it."
                    | None when not (review.Pages.ContainsKey edit.Page) -> $"Page {edit.Page}'s layout isn't found yet."
                    | None -> $"No <{edit.Tag}> suggested in about this place, so tagging won't use it."
                | None ->
                if edit.Tag = "FigureRegion" then
                    "Figure from selected region: Export… checks its boundaries and writes it to a copy, or explains why it can't."
                elif not edit.MergeWith.IsEmpty then
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
                    | Some node when (pendingLines edit node).IsEmpty && not (edit.Move.IsSome && needs.MovePending node) -> "In this PDF."
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
                                                | NonNull own when own.PageNumber = box.PageNumber -> Some(node, Boxes.overlap own box)
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
                                    | Some alt when edit.Tag = "Link" || edit.Tag = "Annot" -> detailLine $"description: \"{alt}\"" false
                                    | Some alt when edit.Tag = "Form" -> detailLine $"tooltip: \"{alt}\"" false
                                    | Some alt -> detailLine $"alt: \"{alt}\"" false
                                    | None -> ()
                                    if not edit.Cells.IsEmpty then
                                        let headers = edit.Cells |> List.filter (fun c -> c.Tag = "TH") |> List.length
                                        if edit.Tag = "P" && edit.Retag = Some "Table" then
                                            detailLine $"table conversion: checked grid, {edit.Cells.Length} cells ({headers} header)" false
                                        else
                                            detailLine $"rows and cells: rebuilt from the page, {edit.Cells.Length} cells ({headers} header)" false
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
                                                    Boxes.overlap own edit.Box >= RegionEdits.SameRegionOverlap
                                                | _ -> false)
                                            |> Option.bind (fun node -> Option.ofObj node.Content)
                                            |> Option.map (fun text ->
                                                let text = text.Trim()
                                                if text.Length > 50 then $", from \"{text[..49]}…\"" else $", from \"{text}\"")
                                            |> Option.defaultValue ""
                                        if edit.Tag = "P" && edit.Retag = Some "Table" then
                                            detailLine $"table conversion: {edit.MergeWith.Length + 1} paragraphs into checked rows{start}" false
                                        else
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
                                        needs.SaveEdits(edits.Current |> List.filter (fun e -> not (obj.ReferenceEquals(e, edit)))))
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
                                let file = Path.GetFileName(Dismissals.filePath needs.SourceFilePath)
                                if edits.Current.IsEmpty && needs.Suggested.IsSome then
                                    "No edits yet. In the Blocks tab, select a region (shift-click to select several): under Selected element, or on its right-click menu, give it another tag (Artifact for one that's not read; a table's cell, TH or TD), merge it into its neighbour, describe a figure or a link, or correct text read with OCR (F2)."
                                elif needs.Suggested.IsSome then
                                    $"Your edits on the suggested structure. Save in the main window keeps them in {file} next to the PDF; Export… tags a copy with the layout and them."
                                elif edits.Current.IsEmpty then
                                    "No edits yet. In the Blocks tab, select a block: under Selected element, or on its right-click menu, describe a figure or a link, or correct replacement text (F2); Move Up or Move Down (Alt+Up, Alt+Down); give a paragraph or heading another tag, or a figure that's only decoration the tag Artifact (not read); shift-click neighbouring paragraphs (or figures drawn in pieces) to merge them; and for what's under Not in the tags, Not Read (Artifact), Tag as Figure or Tag as Paragraph. Under Page n, Order Page n by Layout reorders a whole page."
                                else
                                    $"Your edits. Save in the main window keeps them in {file} next to the PDF; Export… writes the ones not in it yet to a copy."
                            )
                        ]
                        for edit in edits.Current do
                            row false edit
                        // The review saved inside this PDF (a copy Legibyl saved): in it already.
                        if not needs.Review.CarriedEdits.IsEmpty then
                            TextBlock.create [
                                TextBlock.text "From the review saved inside this PDF"
                                TextBlock.fontWeight FontWeight.SemiBold
                                TextBlock.margin (0.0, 16.0, 0.0, 4.0)
                            ]
                            for edit in needs.Review.CarriedEdits do
                                row true edit
                    ]
                ]
            )
        ])
