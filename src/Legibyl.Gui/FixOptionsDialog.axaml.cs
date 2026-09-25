using Avalonia.Controls;
using Avalonia.Interactivity;
using Legibyl.Validation;

namespace Legibyl.Gui;

/// <summary>
/// Lets the user pick which repairs to run. Show with <c>ShowDialog&lt;bool&gt;</c>;
/// the result is true when the user continued with at least one fix selected.
/// </summary>
public partial class FixOptionsDialog : Window
{
    public FixOptionsDialog()
    {
        InitializeComponent();
        MetadataCheckBox.IsChecked = true;
        AnnotationTabsCheckBox.IsChecked = true;
        EmptyParagraphsCheckBox.IsChecked = false;
    }

    public PdfFixOptions SelectedOptions { get; private set; } = PdfFixOptions.Metadata | PdfFixOptions.AnnotationTabs;

    private async void ContinueButton_Click(object? sender, RoutedEventArgs e)
    {
        PdfFixOptions options = PdfFixOptions.None;
        if (MetadataCheckBox.IsChecked == true)
        {
            options |= PdfFixOptions.Metadata;
        }
        if (AnnotationTabsCheckBox.IsChecked == true)
        {
            options |= PdfFixOptions.AnnotationTabs;
        }
        if (EmptyParagraphsCheckBox.IsChecked == true)
        {
            options |= PdfFixOptions.RemoveEmptyParagraphs;
        }

        if (options == PdfFixOptions.None)
        {
            await MessageDialog.ShowAsync(this, "Select at least one fix to apply.", "Fix PDF");
            return;
        }

        SelectedOptions = options;
        Close(true);
    }

    private void CancelButton_Click(object? sender, RoutedEventArgs e) => Close(false);
}
