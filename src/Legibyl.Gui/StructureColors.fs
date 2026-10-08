/// The Structure window's colours: each tag's, by rough semantic category, for its block and its
/// outline on the page preview.
module internal Legibyl.Gui.StructureColors


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

// Tag -> (border color, header background) by rough semantic category, so the
// layout view reads like a color-coded HTML block diagram at a glance.
/// (Each border is at least 3:1 against the blocks' background, light and dark, and each tag's text
/// 4.5:1 against its chip; see AccessibilityTests.)
let categoryColors =
    let heading = Colors.SteelBlue, Color.FromRgb(0xDDuy, 0xE9uy, 0xFBuy)
    let text = Colors.Gray, Color.FromRgb(0xF0uy, 0xF0uy, 0xF0uy)
    let list = Colors.SeaGreen, Color.FromRgb(0xDEuy, 0xF2uy, 0xE3uy)
    let reference = Colors.MediumPurple, Color.FromRgb(0xEBuy, 0xE3uy, 0xF7uy)
    let table = Colors.Chocolate, Color.FromRgb(0xFDuy, 0xEAuy, 0xD3uy)
    let figure = Colors.IndianRed, Color.FromRgb(0xFBuy, 0xE1uy, 0xE1uy)
    dict
        [ for tag in Tags.headings -> tag, heading
          for tag in [ Tags.P; Tags.Span ] -> tag, text
          for tag in [ Tags.L; Tags.LI; Tags.Lbl; Tags.LBody ] -> tag, list
          for tag in [ Tags.TOC; Tags.TOCI; Tags.Link; Tags.Form ] -> tag, reference
          for tag in [ Tags.Table; Tags.TR; Tags.TH; Tags.TD ] -> tag, table
          yield Tags.Figure, figure ]

let colorsFor (tag: string) =
    match categoryColors.TryGetValue tag with
    | true, colors -> colors
    | _ -> Colors.SlateGray, Color.FromRgb(0xEEuy, 0xEEuy, 0xEEuy)

let brush (color: Color) : IBrush = SolidColorBrush(color)

/// A tag's outline color on a page overview (0xRRGGBB), strong enough to see on a page.
let outlineColor (tag: string) =
    match tag with
    | Tags.H
    | Tags.H1
    | Tags.H2
    | Tags.H3
    | Tags.H4
    | Tags.H5
    | Tags.H6 -> 0xC2185Bu
    | Tags.P
    | Tags.Span -> 0x1A73E8u
    | Tags.L
    | Tags.LI
    | Tags.Lbl
    | Tags.LBody -> 0x00838Fu
    | Tags.Figure -> 0x2E7D32u
    | Tags.Table
    | Tags.TR
    | Tags.TH
    | Tags.TD -> 0xEF6C00u
    | Tags.Artifact -> 0x757575u
    | _ -> 0x6A1B9Au


/// A suggested region merged into its neighbour is outlined grey.
let leftOutColor = 0x8A8A8Au
