/// What an empty window says: the steps to making a PDF accessible, and which Legibyl this is,
/// with About….
module Legibyl.Gui.Guidance

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Data
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Input.Platform
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform.Storage
open Avalonia.Styling
open Legibyl.Layout
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Gui.BusyStatus

/// The steps, beside the empty results; About… opens over owner.
let emptyWindow (owner: Window) : IView =
    let heading (text: string) =
        TextBlock.create [
            TextBlock.text text
            TextBlock.fontSize 16.0
            TextBlock.fontWeight FontWeight.SemiBold
            TextBlock.margin (0.0, 0.0, 0.0, 10.0)
            TextBlock.textWrapping TextWrapping.Wrap
        ]
        :> IView
    let line (marker: string) (text: string) =
        Grid.create [
            Grid.columnDefinitions "Auto,*"
            Grid.margin (0.0, 0.0, 0.0, 8.0)
            Grid.children [
                TextBlock.create [
                    TextBlock.text marker
                    TextBlock.width 22.0
                    Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                ]
                TextBlock.create [ Grid.column 1; TextBlock.text text; TextBlock.textWrapping TextWrapping.Wrap ]
            ]
        ]
        :> IView
    let note (text: string) =
        TextBlock.create [
            TextBlock.text text
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.margin (0.0, 6.0, 0.0, 0.0)
            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
        ]
        :> IView
    let panel (children: IView list) : IView =
        ScrollViewer.create [
            Grid.column 2
            ScrollViewer.margin (12.0, 0.0, 0.0, 0.0)
            ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Disabled
            ScrollViewer.content (StackPanel.create [ StackPanel.maxWidth 560.0; StackPanel.horizontalAlignment HorizontalAlignment.Left; StackPanel.children children ])
        ]
    panel [
        heading "Making a PDF accessible"
        line "1." "Open a PDF: Browse…, drop it on this window, or File › Open…."
        line "2." "Read what's found. Failures (red) are what PDF/UA requires; warnings (orange) are worth a look. A wrench marks what has an auto fix; a wand, what suggesting the structure fixes."
        line "3." "Structure… shows the tags a screen reader uses. Check the headings, the figures' alt text and the reading order, and edit them there. A PDF with no tags gets Suggest Structure… instead."
        line "4." "Auto Fixes… chooses fixes, like the document's language and title."
        line "5." "Export… writes a fixed copy and opens it, to check again. The PDF you opened isn't changed. File › Save keeps your review (edits, dismissed warnings, chosen fixes) to pick up later."
        note "PDF/UA is the standard for PDFs that screen readers and other assistive technology can use."
        // Which Legibyl this is, and About…, without the menu bar.
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.margin (0.0, 12.0, 0.0, 0.0)
            StackPanel.children [
                TextBlock.create [
                    TextBlock.text $"Legibyl {Legibyl.Validation.BuildInfo.version} · "
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                ]
                Button.create [
                    Button.content "About…"
                    Button.padding (0.0, 0.0)
                    Button.background Brushes.Transparent
                    Button.borderThickness 0.0
                    Theme.resources [ Button.ForegroundProperty, Theme.Tokens.Primary ]
                    Button.onClick (fun _ -> About.show owner |> ignore)
                ]
            ]
        ]
    ]
