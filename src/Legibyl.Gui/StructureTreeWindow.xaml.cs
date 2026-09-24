using System.IO;
using System.Text.Json;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using Microsoft.Win32;
using Legibyl.Rendering;
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
    private readonly string _sourceFilePath;
    private readonly IPdfRegionRenderer _regionRenderer = new PdfRegionRenderer();
    private Border? _selectedBlock;

    public StructureTreeWindow(StructureNode root, string sourceFilePath)
    {
        InitializeComponent();
        _root = root;
        _sourceFilePath = sourceFilePath;
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
    private Border BuildElement(StructureNode node)
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
        if (node.LinkDestinationPageNumber is not null) attrParts.Add($"links to p.{node.LinkDestinationPageNumber}");
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

        string? linkTarget = node.LinkUri is not null
            ? node.LinkUri
            : node.LinkDestinationDescription;
        if (linkTarget is not null)
        {
            inner.Children.Add(new TextBlock
            {
                Text = $"↳ {linkTarget}",
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

        if (node.Tag == "TOC")
        {
            inner.Children.Add(BuildTocGrid(node));
        }
        else if (node.Tag == "Table")
        {
            inner.Children.Add(BuildTableGrid(node));
        }
        else
        {
            foreach (var child in node.Children)
            {
                inner.Children.Add(BuildElement(child));
            }
        }

        var border = new Border
        {
            BorderBrush = new SolidColorBrush(borderColor),
            BorderThickness = new Thickness(1.5),
            CornerRadius = new CornerRadius(4),
            Padding = new Thickness(8),
            Margin = new Thickness(0, 0, 0, 6),
            Background = Brushes.White,
            Child = inner,
            DataContext = node,
        };
        border.MouseLeftButtonDown += StructureBlock_MouseLeftButtonDown;
        border.ToolTip = node.Geometry is null
            ? "Select: no page geometry is available for this structure node."
            : $"Select: highlight this node on page {node.Geometry.PageNumber}.";
        return border;
    }

    private UIElement BuildTocGrid(StructureNode toc)
    {
        var items = toc.Children.Where(child => child.Tag == "TOCI").ToList();
        if (items.Count == 0)
        {
            var fallback = new StackPanel();
            foreach (var child in toc.Children)
            {
                fallback.Children.Add(BuildElement(child));
            }
            return fallback;
        }

        var grid = new Grid
        {
            Margin = new Thickness(0, 4, 0, 0),
            Background = new SolidColorBrush(Color.FromRgb(0xF8, 0xF5, 0xFC)),
        };
        grid.ColumnDefinitions.Add(new ColumnDefinition { Width = new GridLength(3, GridUnitType.Star) });
        grid.ColumnDefinitions.Add(new ColumnDefinition { Width = GridLength.Auto });
        grid.ColumnDefinitions.Add(new ColumnDefinition { Width = new GridLength(2, GridUnitType.Star) });

        for (int rowIndex = 0; rowIndex < items.Count; rowIndex++)
        {
            grid.RowDefinitions.Add(new RowDefinition { Height = GridLength.Auto });
            AddTocItemRow(grid, items[rowIndex], rowIndex);
        }

        return grid;
    }

    private void AddTocItemRow(Grid grid, StructureNode item, int rowIndex)
    {
        var texts = FlattenContent(item)
            .Select(text => text.Trim())
            .Where(text => text.Length > 0 && !IsLeaderText(text))
            .ToList();
        string? printedPage = texts.LastOrDefault(IsPageNumberText);
        string label = texts.FirstOrDefault(text => !IsPageNumberText(text)) ?? "(empty TOC item)";
        string? target = item.LinkUri ?? item.LinkDestinationDescription;

        AddTocCell(grid, item, rowIndex, 0, label, FontWeights.SemiBold, Brushes.Black);
        AddTocCell(grid, item, rowIndex, 1, printedPage is null ? string.Empty : printedPage, FontWeights.Normal, Brushes.DimGray);
        AddTocCell(
            grid,
            item,
            rowIndex,
            2,
            target is null ? "No link target found" : target,
            FontWeights.Normal,
            target is null ? Brushes.DarkGoldenrod : Brushes.DimGray);
    }

    private void AddTocCell(
        Grid grid,
        StructureNode item,
        int rowIndex,
        int columnIndex,
        string text,
        FontWeight fontWeight,
        Brush foreground)
    {
        var border = new Border
        {
            BorderBrush = new SolidColorBrush(CategoryColors["Link"].Border),
            BorderThickness = new Thickness(1),
            Padding = new Thickness(6, 4, 6, 4),
            Margin = new Thickness(columnIndex == 0 ? 0 : -1, rowIndex == 0 ? 0 : -1, 0, 0),
            Background = Brushes.White,
            Child = new TextBlock
            {
                Text = text,
                FontSize = columnIndex == 0 ? 13 : 12,
                FontWeight = fontWeight,
                Foreground = foreground,
                TextWrapping = TextWrapping.Wrap,
                VerticalAlignment = VerticalAlignment.Top,
            },
            DataContext = item,
            ToolTip = item.Geometry is null
                ? "Select: no page geometry is available for this TOC item."
                : $"Select: highlight this TOC item on page {item.Geometry.PageNumber}.",
        };
        border.MouseLeftButtonDown += StructureBlock_MouseLeftButtonDown;
        Grid.SetRow(border, rowIndex);
        Grid.SetColumn(border, columnIndex);
        grid.Children.Add(border);
    }

    /// <summary>
    /// Renders semantic PDF tables as a visual grid instead of a vertical stack:
    /// Table -> TR elements become rows, and TH/TD children become cells. This keeps
    /// the exported/tree semantics unchanged while making the layout tab much closer
    /// to how an HTML table with visible borders would look.
    /// </summary>
    private UIElement BuildTableGrid(StructureNode table)
    {
        var rows = table.Children.Where(child => child.Tag == "TR").ToList();
        if (rows.Count == 0)
        {
            var fallback = new StackPanel();
            foreach (var child in table.Children)
            {
                fallback.Children.Add(BuildElement(child));
            }
            return fallback;
        }

        int columnCount = rows
            .Select(row => TableCells(row).Count)
            .DefaultIfEmpty(1)
            .Max();
        columnCount = Math.Max(columnCount, 1);

        var grid = new Grid
        {
            Margin = new Thickness(0, 4, 0, 0),
            Background = new SolidColorBrush(Color.FromRgb(0xFF, 0xFA, 0xF3)),
        };

        for (int column = 0; column < columnCount; column++)
        {
            grid.ColumnDefinitions.Add(new ColumnDefinition { Width = new GridLength(1, GridUnitType.Star) });
        }

        for (int rowIndex = 0; rowIndex < rows.Count; rowIndex++)
        {
            grid.RowDefinitions.Add(new RowDefinition { Height = GridLength.Auto });
            var cells = TableCells(rows[rowIndex]);

            if (cells.Count == 0)
            {
                var rowElement = BuildElement(rows[rowIndex]);
                Grid.SetRow(rowElement, rowIndex);
                Grid.SetColumn(rowElement, 0);
                Grid.SetColumnSpan(rowElement, columnCount);
                grid.Children.Add(rowElement);
                continue;
            }

            for (int columnIndex = 0; columnIndex < cells.Count; columnIndex++)
            {
                var cell = BuildElement(cells[columnIndex]);
                cell.Margin = new Thickness(1);
                Grid.SetRow(cell, rowIndex);
                Grid.SetColumn(cell, columnIndex);
                grid.Children.Add(cell);
            }
        }

        return grid;
    }

    private static List<StructureNode> TableCells(StructureNode row)
    {
        var cells = row.Children.Where(child => child.Tag is "TH" or "TD").ToList();
        return cells.Count > 0 ? cells : row.Children.ToList();
    }

    private static IEnumerable<string> FlattenContent(StructureNode node)
    {
        if (node.Content is not null)
        {
            yield return node.Content;
        }

        foreach (StructureNode child in node.Children)
        {
            foreach (string text in FlattenContent(child))
            {
                yield return text;
            }
        }
    }

    private static bool IsLeaderText(string text) =>
        text.All(ch => ch is '.' or '…' or ' ' or '\t');

    private static bool IsPageNumberText(string text) =>
        text.All(char.IsDigit);

    private async void StructureBlock_MouseLeftButtonDown(object sender, System.Windows.Input.MouseButtonEventArgs e)
    {
        e.Handled = true;
        if (sender is not Border border || border.DataContext is not StructureNode node)
        {
            return;
        }

        SelectBlock(border, node);
        SelectionPreviewImage.Source = null;

        if (node.Geometry is null)
        {
            SelectionPreviewNoteText.Text =
                $"Selected <{node.Tag}>. No same-page geometry is available for this node " +
                "(it may be a pure container, non-text content without a rectangle, or span multiple pages).";
            return;
        }

        SelectionPreviewNoteText.Text = $"Rendering page {node.Geometry.PageNumber} highlight for <{node.Tag}>…";
        try
        {
            HighlightImageResult result = await Task.Run(
                () => _regionRenderer.RenderPageWithHighlight(_sourceFilePath, node.Geometry));

            SelectionPreviewImage.Source = CreateBitmap(result.PngBytes);
            SelectionPreviewNoteText.Text = $"{result.Note} Page {result.PageNumber}.";
        }
        catch (Exception ex) when (ex is InvalidOperationException or IOException)
        {
            SelectionPreviewNoteText.Text = $"Could not render highlight for <{node.Tag}>: {ex.Message}";
        }
    }

    private void SelectBlock(Border border, StructureNode node)
    {
        if (_selectedBlock?.DataContext is StructureNode previousNode)
        {
            var (previousBorderColor, _) = CategoryColors.TryGetValue(previousNode.Tag, out var previousColors)
                ? previousColors
                : DefaultCategoryColors;
            _selectedBlock.BorderBrush = new SolidColorBrush(previousBorderColor);
            _selectedBlock.BorderThickness = new Thickness(1.5);
        }

        _selectedBlock = border;
        border.BorderBrush = Brushes.DodgerBlue;
        border.BorderThickness = new Thickness(3);
    }

    private static BitmapImage CreateBitmap(byte[] pngBytes)
    {
        var image = new BitmapImage();
        using var stream = new MemoryStream(pngBytes);
        image.BeginInit();
        image.CacheOption = BitmapCacheOption.OnLoad;
        image.StreamSource = stream;
        image.EndInit();
        image.Freeze();
        return image;
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
