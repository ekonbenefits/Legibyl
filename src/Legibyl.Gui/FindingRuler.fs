/// A thin strip beside the Structure window's blocks, as tall as their scroll viewer: a tick where
/// each block with failures (red) or warnings (amber) is in the whole list, so they can be seen
/// and gone to without scrolling through. Clicking a tick goes to its block.
module Legibyl.Gui.FindingRuler

open System
open System.Collections.Generic
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open Legibyl.Validation

/// A block to mark: its node, and whether it has a failure (else it has warnings only).
type Mark = { Node: StructureNode; Failure: bool }

/// The marks for the findings on each block (see FindingMarkers.place), in the order given.
let marks (order: StructureNode list) (findings: IReadOnlyDictionary<StructureNode, RuleViolation list>) =
    [ for node in order do
          match findings.TryGetValue node with
          | true, found when not found.IsEmpty ->
              { Node = node; Failure = found |> List.exists (fun f -> f.Severity = FindingSeverity.Failure) }
          | _ -> () ]

/// A tick: where it is on the strip (its top, 0 to 1 of the blocks' whole height), and its mark.
type private Tick = { At: float; Mark: Mark }

let private tickHeight = 4.0

let private hand = lazy (new Cursor(StandardCursorType.Hand))

/// The name of the blocks' scroll viewer, which the strip follows (it's beside it, in a panel
/// above both); each block in it is a Border whose Tag is its node.
let blocksName = "structure-blocks"

/// The strip. Marks, what to mark; ParentOf, a node's parent (a block inside a collapsed one is marked on
/// the nearest one shown); Go, what clicking a tick does with its node.
type Ruler() as this =
    inherit Control()

    let mutable ticks: Tick list = []
    let mutable pending = false
    let mutable blocks: ScrollViewer | null = null
    // Where the blocks' scroll bar's track is beside it (its top, and how tall): the thumb moves
    // along that, not the scroll bar's whole height, which its arrow buttons take a part of when
    // they show. None: the strip's own height.
    let mutable track: (float * float) option = None

    /// The span ticks are laid along: the scroll bar's track, else the whole strip.
    let span () =
        match track with
        | Some(top, height) -> top, height
        | None -> 0.0, this.Bounds.Height

    /// Where a tick at at (0 to 1 down the blocks) is drawn: its top.
    let tickTop (at: float) =
        let top, height = span ()
        top + at * (height - tickHeight)

    /// Where each mark's block is, once the blocks are laid out.
    let measure () =
        pending <- false
        match blocks with
        | NonNull scroll when scroll.Extent.Height > 0.0 ->
            match scroll.Content with
            | :? Visual as content ->
                let shown = Dictionary<StructureNode, Border>(HashIdentity.Reference)
                for v in content.GetVisualDescendants() do
                    match v with
                    | :? Border as border when border.IsVisible ->
                        match border.Tag with
                        | :? StructureNode as node when not (shown.ContainsKey node) -> shown[node] <- border
                        | _ -> ()
                    | _ -> ()
                let rec blockOf (node: StructureNode) =
                    match shown.TryGetValue node with
                    | true, border -> Some border
                    | _ -> this.ParentOf node |> Option.bind blockOf
                let extent = scroll.Extent.Height
                // The vertical scroll bar's track (the thumb's run), as it's laid out now.
                track <-
                    scroll.GetVisualDescendants()
                    |> Seq.tryPick (function
                        | :? Primitives.Track as t when t.Orientation = Layout.Orientation.Vertical && t.IsEffectivelyVisible && t.Bounds.Height > 0.0 ->
                            t.TranslatePoint(Point(0.0, 0.0), this) |> Option.ofNullable |> Option.map (fun p -> p.Y, t.Bounds.Height)
                        | _ -> None)
                ticks <-
                    [ for mark in this.Marks do
                          match blockOf mark.Node |> Option.bind (fun b -> Option.ofNullable (b.TranslatePoint(Point(0.0, 0.0), content))) with
                          | Some point -> { At = Math.Clamp(point.Y / extent, 0.0, 1.0); Mark = mark }
                          | None -> () ]
                this.InvalidateVisual()
            | _ -> ()
        | _ -> ()

    /// Measured again after the blocks' layout changes (opened, moved, scrolled), once.
    let remeasure () =
        if not pending then
            pending <- true
            Dispatcher.UIThread.Post(measure, DispatcherPriority.Background)

    let onLayout = EventHandler(fun _ _ -> remeasure ())

    let tickAt (y: float) =
        ticks
        |> List.map (fun t -> t, abs (tickTop t.At + tickHeight / 2.0 - y))
        |> List.filter (fun (_, distance) -> distance <= 6.0)
        // The nearest; a failure over a warning as near.
        |> List.sortBy (fun (t, distance) -> distance, not t.Mark.Failure)
        |> List.tryHead
        |> Option.map fst

    do
        this.Width <- 10.0
        this.Margin <- Thickness(2.0, 0.0, 0.0, 0.0)
        // Its ticks are a picture of what Previous and Next (beside the blocks) go to.
        Automation.AutomationProperties.SetAccessibilityView(this, Automation.AccessibilityView.Raw)

    member val Marks: Mark list = [] with get, set
    member val ParentOf: StructureNode -> StructureNode option = (fun _ -> None) with get, set
    member val Go: StructureNode -> unit = ignore with get, set
    /// Where its ticks are (0 to 1, down the blocks) and what they mark, as last measured.
    member _.Ticks = ticks |> List.map (fun t -> t.At, t.Mark)
    /// Where its ticks are laid along it: the top and height of the scroll bar's track beside it
    /// (or its own, without one), as last measured.
    member _.Span = span ()
    /// How a tick's block is said in its tooltip.
    member val Describe: StructureNode -> string = (fun n -> n.Tag) with get, set

    /// Follows value's layout (null: none).
    member private this.Follow(value: ScrollViewer | null) =
        if not (obj.ReferenceEquals(blocks, value)) then
            match blocks with
            | NonNull old -> old.LayoutUpdated.RemoveHandler onLayout
            | Null -> ()
            blocks <- value
            match value with
            | NonNull scroll -> scroll.LayoutUpdated.AddHandler onLayout
            | Null -> ()

    /// The marks changed (a render gave new ones): found and measured again.
    member this.Refresh() =
        let shown = match blocks with NonNull scroll -> not (isNull (TopLevel.GetTopLevel scroll)) | Null -> false
        if not shown then
            // In the nearest panel above that has it.
            this.GetVisualAncestors()
            |> Seq.tryPick (fun above ->
                above.GetVisualDescendants()
                |> Seq.tryPick (function :? ScrollViewer as scroll when scroll.Name = blocksName -> Some scroll | _ -> None))
            |> Option.iter this.Follow
        remeasure ()

    override this.Render(context) =
        let height = this.Bounds.Height
        let width = this.Bounds.Width
        let brush (key: string) =
            match this.TryFindResource(key, this.ActualThemeVariant) with
            | true, (:? IBrush as brush) -> brush
            | true, (:? Color as color) -> SolidColorBrush color :> IBrush
            | _ -> Brushes.Gray
        let track = brush Theme.Tokens.Fill
        context.FillRectangle(track, Rect(0.0, 0.0, width, height), 2.0f)
        let failure, warning = brush Theme.Tokens.Danger, brush Theme.Tokens.Warning
        // Warnings first, so a failure at the same place is drawn over it.
        for tick in ticks |> List.sortBy _.Mark.Failure do
            let y = tickTop tick.At
            context.FillRectangle((if tick.Mark.Failure then failure else warning), Rect(0.0, y, width, tickHeight), 1.0f)

    override this.OnPointerMoved(e) =
        base.OnPointerMoved e
        match tickAt (e.GetPosition(this).Y) with
        | Some tick ->
            this.Cursor <- hand.Value
            let what = if tick.Mark.Failure then "failures" else "warnings"
            ToolTip.SetTip(this, $"{this.Describe tick.Mark.Node}: {what}. Click to go to it.")
        | None ->
            this.Cursor <- Cursor.Default
            ToolTip.SetTip(this, "Where the blocks with failures (red) and warnings (amber) are. Previous and Next (F8) go to them too.")

    override this.OnPointerPressed(e) =
        base.OnPointerPressed e
        if e.GetCurrentPoint(this).Properties.IsLeftButtonPressed then
            match tickAt (e.GetPosition(this).Y) with
            | Some tick ->
                e.Handled <- true
                this.Go tick.Mark.Node
            | None -> ()

    override this.OnAttachedToVisualTree(e) =
        base.OnAttachedToVisualTree e
        // Once the blocks beside it are there too.
        Dispatcher.UIThread.Post(this.Refresh, DispatcherPriority.Background)

    override this.OnDetachedFromVisualTree(e) =
        base.OnDetachedFromVisualTree e
        this.Follow null
