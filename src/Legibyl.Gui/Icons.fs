/// 16x16 icons, shared by the main window and the Structure window. The even-odd fill cuts the
/// "!" and "×" out of the shapes.
module Legibyl.Gui.Icons

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media

/// A warning: a triangle with "!".
let warning =
    Geometry.Parse "F0 M8,1 L15.5,14.5 L0.5,14.5 Z M7.2,5.5 L8.8,5.5 L8.6,10 L7.4,10 Z M7.1,11.2 L8.9,11.2 L8.9,13 L7.1,13 Z"

/// A pass: a circle with a tick.
let pass =
    Geometry.Parse "F0 M8,0.5 A7.5,7.5 0 1 1 7.99,0.5 Z M4.2,8.3 L5.3,7.2 L7,8.9 L10.7,5.2 L11.8,6.3 L7,11.1 Z"

/// A failure: a circle with "×".
let failure =
    Geometry.Parse
        "F0 M8,0.5 A7.5,7.5 0 1 1 7.99,0.5 Z M4.9,5.9 L5.9,4.9 L8,7 L10.1,4.9 L11.1,5.9 L9,8 L11.1,10.1 L10.1,11.1 L8,9 L5.9,11.1 L4.9,10.1 L7,8 Z"

/// Information: a circle with "i", for the Next step row.
let info =
    Geometry.Parse "F0 M8,0.5 A7.5,7.5 0 1 1 7.99,0.5 Z M7.1,3.4 L8.9,3.4 L8.9,5.2 L7.1,5.2 Z M7.2,6.6 L8.8,6.6 L8.8,12.6 L7.2,12.6 Z"

/// A wrench, for an auto fix: a single outline (head, notch, handle) rotated 45°.
let wrench =
    Geometry.Parse
        "F1 M10.68,8.56 L17.25,15.13 A1.5,1.5 0 0 1 15.13,17.25 L8.56,10.68 A4,4 0 0 1 3.45,5.15 L5.44,7.14 L7.14,5.44 L5.15,3.45 A4,4 0 0 1 10.68,8.56 Z"

/// A magic wand (a stick, and two sparkles): for what suggesting the structure fixes.
let wand =
    Geometry.Parse
        "F1 M1.5,13.4 L9.9,5 L11,6.1 L2.6,14.5 Z M12.5,0.5 L13.3,2.7 L15.5,3.5 L13.3,4.3 L12.5,6.5 L11.7,4.3 L9.5,3.5 L11.7,2.7 Z M5,1.5 L5.5,2.5 L6.5,3 L5.5,3.5 L5,4.5 L4.5,3.5 L3.5,3 L4.5,2.5 Z"

// ---- As views ----

let iconPath (icon: Geometry) (color: string) (size: float) (attrs: IAttr<Shapes.Path> list) =
    // Qualified: System.IO also defines Path.
    Avalonia.FuncUI.DSL.Path.create [
        AttrBuilder<Shapes.Path>.CreateProperty<Geometry | null>(Shapes.Path.DataProperty, icon, ValueNone)
        Shapes.Shape.stretch Stretch.Uniform
        Control.width size
        Control.height size
        Control.verticalAlignment VerticalAlignment.Center
        Theme.resources [ Shapes.Shape.FillProperty, color ]
        yield! attrs
    ]

/// A small results-grid icon, with name as its tooltip and screen-reader text.
let gridIcon (icon: Geometry) (color: string) (name: string) =
    iconPath icon color 14.0 [
        Control.horizontalAlignment HorizontalAlignment.Center
        ToolTip.tip name
        AttrBuilder<Shapes.Path>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, name, ValueNone)
    ]

/// An icon beside text that already says the same thing, so screen readers skip it.
let decorativeIcon (icon: Geometry) (color: string) (size: float) =
    iconPath icon color size [
        Control.margin (0.0, 0.0, 6.0, 0.0)
        AttrBuilder<Shapes.Path>.CreateProperty<Automation.AccessibilityView>(
            Automation.AutomationProperties.AccessibilityViewProperty, Automation.AccessibilityView.Raw, ValueNone)
    ]

/// Puts a decorative icon in front of content, keeping the content's own wrapping.
let withIcon (icon: IView) (content: IView) =
    Grid.create [
        Grid.columnDefinitions "Auto,*"
        Grid.children [
            icon
            Border.create [ Grid.column 1; Border.child content ]
        ]
    ]

/// A button's label, after an icon (its geometry and colour token) if given, with a count
/// beside it (none when it's 0).
let badged (icon: (Geometry * string) option) (label: string) (count: int) : IView =
    StackPanel.create [
        StackPanel.orientation Orientation.Horizontal
        StackPanel.children [
            match icon with
            | Some(geometry, color) -> decorativeIcon geometry color 14.0
            | None -> ()
            TextBlock.create [ TextBlock.text label; TextBlock.verticalAlignment VerticalAlignment.Center ]
            if count > 0 then
                Border.create [
                    Border.margin (6.0, 0.0, 0.0, 0.0)
                    Border.padding (6.0, 0.0)
                    Border.cornerRadius 8.0
                    Border.verticalAlignment VerticalAlignment.Center
                    Theme.resources [ Border.BackgroundProperty, Theme.Tokens.Primary ]
                    Border.child (
                        TextBlock.create [
                            TextBlock.text (string count)
                            TextBlock.fontSize 11.0
                            TextBlock.fontWeight FontWeight.SemiBold
                            // The background's colour on the primary one: white in light mode, near black in dark,
                            // 4.5:1 or more either way (see AccessibilityTests).
                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.Background ]
                        ]
                    )
                ]
        ]
    ]
