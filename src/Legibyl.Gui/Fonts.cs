using Avalonia.Media;

namespace Legibyl.Gui;

/// <summary>Shared font choices that resolve to an installed face on each OS.</summary>
public static class Fonts
{
    /// <summary>Consolas on Windows, Menlo on macOS, DejaVu Sans Mono on most Linux desktops.</summary>
    public static readonly FontFamily Monospace = new("Consolas, Menlo, DejaVu Sans Mono, monospace");
}
