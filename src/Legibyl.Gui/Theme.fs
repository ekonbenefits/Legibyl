/// Theme colors that follow light/dark mode.
module Legibyl.Gui.Theme

open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

/// Semi design tokens used by the app. These are theme resources, so they switch
/// with the light/dark variant.
module Tokens =
    let SecondaryText = "SemiColorText2"
    let Border = "SemiColorBorder"
    let Fill = "SemiColorFill0"
    let Background = "SemiColorBackground0"
    let Success = "SemiColorSuccess"
    let Danger = "SemiColorDanger"
    let Warning = "SemiColorWarning"
    let Primary = "SemiColorPrimary"

/// Binds properties to theme resources, so they follow light/dark changes without a
/// re-render (FuncUI has no dynamic-resource attribute). FuncUI keys every init
/// function by the same name, so use a single call per control.
let resources<'t when 't :> Control> (bindings: (AvaloniaProperty * string) list) : IAttr<'t> =
    AvaloniaObject.init<'t> (fun control ->
        for property, key in bindings do
            control.Bind(property, control.GetResourceObservable key) |> ignore)
