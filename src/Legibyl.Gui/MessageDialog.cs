using Avalonia;
using Avalonia.Controls;
using Avalonia.Layout;

namespace Legibyl.Gui;

/// <summary>
/// Minimal modal OK-only message box. Avalonia has no built-in equivalent of WPF's
/// MessageBox, and this is all the app needs.
/// </summary>
public static class MessageDialog
{
    public static Task ShowAsync(Window owner, string message, string title)
    {
        var okButton = new Button
        {
            Content = "OK",
            MinWidth = 82,
            IsDefault = true,
            IsCancel = true,
            HorizontalAlignment = HorizontalAlignment.Right,
            HorizontalContentAlignment = HorizontalAlignment.Center,
        };

        var dialog = new Window
        {
            Title = title,
            Width = 460,
            SizeToContent = SizeToContent.Height,
            CanResize = false,
            ShowInTaskbar = false,
            WindowStartupLocation = WindowStartupLocation.CenterOwner,
            Content = new StackPanel
            {
                Margin = new Thickness(16),
                Spacing = 16,
                Children =
                {
                    new SelectableTextBlock { Text = message, TextWrapping = Avalonia.Media.TextWrapping.Wrap },
                    okButton,
                },
            },
        };

        okButton.Click += (_, _) => dialog.Close();
        return dialog.ShowDialog(owner);
    }
}
