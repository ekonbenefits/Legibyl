module Legibyl.Gui.Tests.StructureBlockSelectionTests

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Visual> (root: Visual) =
    root.GetVisualDescendants()
    |> Seq.choose (function :? 'T as control -> Some control | _ -> None)
    |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private button (window: Window) label =
    descendants<Button> window
    |> List.find (fun button ->
        let text =
            match Automation.AutomationProperties.GetName button with
            | null | "" -> string button.Content
            | name -> name
        text = label)

let private block (window: Window) (node: StructureNode) =
    descendants<Border> window |> List.find (fun border -> obj.ReferenceEquals(border.Tag, node))

let private click (source: Control) =
    let properties = PointerPointProperties(RawInputModifiers.LeftMouseButton, PointerUpdateKind.LeftButtonPressed)
    source.RaiseEvent(
        PointerPressedEventArgs(source, new Pointer(0, PointerType.Mouse, true), source, Point(4.0, 4.0), 0UL, properties, KeyModifiers.None))
    pump ()

let private enter (border: Border) =
    border.Focus() |> ignore
    border.RaiseEvent(KeyEventArgs(RoutedEvent = InputElement.KeyDownEvent, Key = Key.Enter, Source = border))
    pump ()

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``reused Art blocks select their current node after repeated moves`` keyboard =
    onUiThread (fun () ->
        let folder = Path.Combine(AppContext.BaseDirectory, $"selection-crash-{Guid.NewGuid():N}")
        Directory.CreateDirectory folder |> ignore
        try
            let node = LayoutOrderTests.node
            let wrapper index =
                let geometry: StructureGeometry =
                    { PageNumber = 1; Left = 72.0; Bottom = 600.0 - float index * 60.0; Right = 300.0; Top = 640.0 - float index * 60.0 }
                node "Art" null geometry [ node "P" $"Article {index}" geometry [] ]
            let articles = [ wrapper 0; wrapper 1; wrapper 2 ]
            let root = node "StructTreeRoot" null null [ node "Document" null null articles ]
            let window = StructureTreeWindow(root, Path.Combine(folder, "unused.pdf"), Width = 1000.0, Height = 760.0)
            try
                window.Show()
                pump ()
                click (block window articles[2])
                for expected in [ [ articles[0]; articles[2]; articles[1] ]; [ articles[2]; articles[0]; articles[1] ] ] do
                    let before = block window articles[2]
                    (button window "Move Up").RaiseEvent(RoutedEventArgs Button.ClickEvent)
                    pump ()
                    let shown =
                        descendants<Border> window
                        |> List.choose (fun border ->
                            match border.Tag with
                            | :? StructureNode as node when node.Tag = "Art" -> Some node
                            | _ -> None)
                    List.forall2 (fun a b -> obj.ReferenceEquals(a, b)) shown expected |> should be True
                    // FuncUI reuses the control at the old position for a different sibling.
                    obj.ReferenceEquals(before.Tag, articles[2]) |> should be False
                    for _ in 1 .. 2 do
                        for index in [ 2; 0; 1 ] do
                            let current = block window expected[index]
                            if keyboard then enter current
                            else
                                let chip = descendants<TextBlock> current |> List.find (fun text -> text.Text = "Art")
                                click chip
                            current.BorderThickness |> should equal (Thickness 3.0)
                            (button window "Move Up").IsEnabled |> should equal (index > 0)
                            (button window "Move Down").IsEnabled |> should equal (index < 2)
                    click (block window articles[2])

                // A click on a nested content control belongs to the child, not its Art wrapper.
                let child = articles[0].Children[0]
                let childBlock = block window child
                let text = descendants<TextBlock> childBlock |> List.find (fun text -> text.Text = child.Content)
                click text
                childBlock.BorderThickness |> should equal (Thickness 3.0)
                (block window articles[0]).BorderThickness |> should equal (Thickness 1.5)
                let picker = descendants<ComboBox> window |> List.exactlyOne
                Automation.AutomationProperties.GetName picker |> should equal "Tag for this <P>"
                picker.SelectedItem <- "H1"
                pump ()
                let edit = window.Review.Edits |> List.find (fun edit -> edit.Retag = Some "H1")
                edit.Tag |> should equal "P"
                edit.Box |> should equal child.Geometry
            finally
                window.Close()
        finally
            Directory.Delete(folder, true))
