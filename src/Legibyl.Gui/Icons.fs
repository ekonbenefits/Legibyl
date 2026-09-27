/// 16x16 icons, shared by the main window and the Structure window. The even-odd fill cuts the
/// "!" and "×" out of the shapes.
module Legibyl.Gui.Icons

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
