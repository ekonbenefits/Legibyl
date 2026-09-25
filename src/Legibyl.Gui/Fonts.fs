/// Shared font choices that resolve to an installed face on each OS.
module Legibyl.Gui.Fonts

open Avalonia.Media

/// Consolas on Windows, Menlo on macOS, DejaVu Sans Mono on most Linux desktops.
let Monospace = FontFamily("Consolas, Menlo, DejaVu Sans Mono, monospace")
