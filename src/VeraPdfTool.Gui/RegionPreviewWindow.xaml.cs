using System.IO;
using System.Windows;
using System.Windows.Media.Imaging;
using VeraPdfTool.Rendering;

namespace VeraPdfTool.Gui;

/// <summary>Shows a rendered page image with a best-effort highlight box around a failure's location.</summary>
public partial class RegionPreviewWindow : Window
{
    public RegionPreviewWindow(HighlightImageResult result, string title)
    {
        InitializeComponent();
        Title = title;

        var image = new BitmapImage();
        using (var stream = new MemoryStream(result.PngBytes))
        {
            image.BeginInit();
            image.CacheOption = BitmapCacheOption.OnLoad;
            image.StreamSource = stream;
            image.EndInit();
        }
        image.Freeze();
        PageImage.Source = image;

        NoteText.Text = result.Note;
        NoteBanner.Visibility = result.HasPreciseRegion ? Visibility.Collapsed : Visibility.Visible;
    }
}
