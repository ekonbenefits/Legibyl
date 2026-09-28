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


/// A suggested region merged into its neighbour is outlined grey.
let leftOutColor = 0x9E9E9Eu
