using System.Windows;
using Legibyl.Validation;

namespace Legibyl.Gui;

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

    private void ContinueButton_Click(object sender, RoutedEventArgs e)
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
            MessageBox.Show(this, "Select at least one fix to apply.", "Fix PDF",
                MessageBoxButton.OK, MessageBoxImage.Information);
            return;
        }

        SelectedOptions = options;
        DialogResult = true;
    }
}
