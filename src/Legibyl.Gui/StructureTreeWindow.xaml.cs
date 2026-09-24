using System.IO;
using System.Text.Json;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using Microsoft.Win32;
using Legibyl.Validation;

namespace Legibyl.Gui;

/// <summary>
/// Shows a tagged PDF's logical structure tree (the semantic hierarchy that screen
/// readers use) both as an HTML-like nested "boxes with borders" layout (so a sighted
/// user can visually see block-level nesting and the real text each element owns) and
/// as a browsable outline, with an option to export the whole tree — including content
/// text — as JSON for AI agents or other tooling.
/// </summary>
public partial class StructureTreeWindow : Window
{
    private static readonly JsonSerializerOptions JsonOptions = new()
    {
        WriteIndented = true,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
    };

    // Tag -> (border color, header background) by rough semantic category, so the
    // layout view reads like a color-coded HTML block diagram at a glance.
    private static readonly Dictionary<string, (Color Border, Color Header)> CategoryColors = new()
    {
        ["H1"] = (Colors.SteelBlue, Color.FromRgb(0xDD, 0xE9, 0xFB)),
        ["H2"] = (Colors.SteelBlue, Color.FromRgb(0xDD, 0xE9, 0xFB)),
        ["H3"] = (Colors.SteelBlue, Color.FromRgb(0xDD, 0xE9, 0xFB)),
        ["H4"] = (Colors.SteelBlue, Color.FromRgb(0xDD, 0xE9, 0xFB)),
        ["H5"] = (Colors.SteelBlue, Color.FromRgb(0xDD, 0xE9, 0xFB)),
        ["H6"] = (Colors.SteelBlue, Color.FromRgb(0xDD, 0xE9, 0xFB)),
        ["P"] = (Colors.Gray, Color.FromRgb(0xF0, 0xF0, 0xF0)),
        ["Span"] = (Colors.Gray, Color.FromRgb(0xF0, 0xF0, 0xF0)),
        ["L"] = (Colors.SeaGreen, Color.FromRgb(0xDE, 0xF2, 0xE3)),
        ["LI"] = (Colors.SeaGreen, Color.FromRgb(0xDE, 0xF2, 0xE3)),
        ["Lbl"] = (Colors.SeaGreen, Color.FromRgb(0xDE, 0xF2, 0xE3)),
        ["LBody"] = (Colors.SeaGreen, Color.FromRgb(0xDE, 0xF2, 0xE3)),
        ["Table"] = (Colors.DarkOrange, Color.FromRgb(0xFD, 0xEA, 0xD3)),
        ["TR"] = (Colors.DarkOrange, Color.FromRgb(0xFD, 0xEA, 0xD3)),
        ["TH"] = (Colors.DarkOrange, Color.FromRgb(0xFD, 0xEA, 0xD3)),
        ["TD"] = (Colors.DarkOrange, Color.FromRgb(0xFD, 0xEA, 0xD3)),
        ["Link"] = (Colors.MediumPurple, Color.FromRgb(0xEB, 0xE3, 0xF7)),
        ["Form"] = (Colors.MediumPurple, Color.FromRgb(0xEB, 0xE3, 0xF7)),
        ["Figure"] = (Colors.IndianRed, Color.FromRgb(0xFB, 0xE1, 0xE1)),
    };

    private static readonly (Color Border, Color Header) DefaultCategoryColors =
        (Colors.DarkGray, Color.FromRgb(0xEE, 0xEE, 0xEE));

    private readonly StructureNode _root;
    private readonly string _suggestedFileName;

    public StructureTreeWindow(StructureNode root, string sourceFilePath)
    {
        InitializeComponent();
        _root = root;
        _suggestedFileName = Path.GetFileNameWithoutExtension(sourceFilePath) + "-structure.json";
        // The synthetic StructTreeRoot node itself carries no semantic info, so show
        // its children (the actual document hierarchy) as the top level in both views.
        StructureTreeControl.ItemsSource = root.Children;

        var layoutPanel = new StackPanel();
        foreach (var child in root.Children)
        {
            layoutPanel.Children.Add(BuildElement(child));
        }
        LayoutHost.Content = layoutPanel;
    }

    /// <summary>
    /// Recursively renders one structure node as an HTML-like bordered block: a colored
    /// tag "chip" header (color = semantic category), an attributes line, optional
    /// alt-text, the element's own extracted content text (if any), and nested child
    /// boxes indented inside — so nesting depth is visually obvious, matching how a
    /// browser's block-level box model would draw it.
    /// </summary>
    private static Border BuildElement(StructureNode node)
    {
        var (borderColor, headerColor) = CategoryColors.TryGetValue(node.Tag, out var colors) ? colors : DefaultCategoryColors;

        var inner = new StackPanel();

        var headerPanel = new WrapPanel { Margin = new Thickness(0, 0, 0, 4) };
        headerPanel.Children.Add(new Border
        {
            Background = new SolidColorBrush(headerColor),
            BorderBrush = new SolidColorBrush(borderColor),
            BorderThickness = new Thickness(1),
            CornerRadius = new CornerRadius(3),
            Padding = new Thickness(5, 1, 5, 1),
            Margin = new Thickness(0, 0, 6, 0),
            Child = new TextBlock
            {
                Text = node.Tag,
                FontFamily = new FontFamily("Consolas"),
                FontWeight = FontWeights.Bold,
                FontSize = 12,
            },
        });

        var attrParts = new List<string>();
        if (node.Role is not null) attrParts.Add($"role={node.Role}");
        if (node.PageNumber is not null) attrParts.Add($"p.{node.PageNumber}");
        if (node.Language is not null) attrParts.Add($"[{node.Language}]");
        if (node.FormFieldType is not null) attrParts.Add($"field type: {node.FormFieldType}");
        if (node.FormFieldName is not null) attrParts.Add($"field: {node.FormFieldName}");
        if (attrParts.Count > 0)
        {
            headerPanel.Children.Add(new TextBlock
            {
                Text = string.Join("  ", attrParts),
                FontSize = 11,
                Foreground = Brushes.DimGray,
                VerticalAlignment = VerticalAlignment.Center,
            });
        }
        inner.Children.Add(headerPanel);

        if (node.AlternateDescription is not null)
        {
            inner.Children.Add(new TextBlock
            {
                Text = $"alt: \"{node.AlternateDescription}\"",
                FontStyle = FontStyles.Italic,
                FontSize = 11,
                Foreground = Brushes.DimGray,
                TextWrapping = TextWrapping.Wrap,
                Margin = new Thickness(0, 0, 0, 4),
            });
        }

        if (node.FormFieldDescription is not null)
        {
            inner.Children.Add(new TextBlock
            {
                Text = $"tooltip: \"{node.FormFieldDescription}\"",
                FontStyle = FontStyles.Italic,
                FontSize = 11,
                Foreground = Brushes.DimGray,
                TextWrapping = TextWrapping.Wrap,
                Margin = new Thickness(0, 0, 0, 4),
            });
        }

        if (node.FormFieldValue is not null)
        {
            inner.Children.Add(new TextBlock
            {
                Text = $"value: \"{node.FormFieldValue}\"",
                FontSize = 11,
                Foreground = Brushes.DimGray,
                TextWrapping = TextWrapping.Wrap,
                Margin = new Thickness(0, 0, 0, 4),
            });
        }

        string? contentText = node.Content ?? node.ActualText;
        if (contentText is not null)
        {
            inner.Children.Add(new TextBlock
            {
                Text = contentText,
                FontSize = 13,
                TextWrapping = TextWrapping.Wrap,
                Margin = new Thickness(0, 0, 0, 4),
            });
        }

        foreach (var child in node.Children)
        {
            inner.Children.Add(BuildElement(child));
        }

        return new Border
        {
            BorderBrush = new SolidColorBrush(borderColor),
            BorderThickness = new Thickness(1.5),
            CornerRadius = new CornerRadius(4),
            Padding = new Thickness(8),
            Margin = new Thickness(0, 0, 0, 6),
            Background = Brushes.White,
            Child = inner,
        };
    }

    private void ExportJsonButton_Click(object sender, RoutedEventArgs e)
    {
        var dialog = new SaveFileDialog
        {
            Filter = "JSON files (*.json)|*.json|All files (*.*)|*.*",
            FileName = _suggestedFileName,
        };

        if (dialog.ShowDialog(this) == true)
        {
            File.WriteAllText(dialog.FileName, JsonSerializer.Serialize(_root, JsonOptions));
        }
    }

    private void CloseButton_Click(object sender, RoutedEventArgs e) => Close();
}
