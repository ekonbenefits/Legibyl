using System.IO;
using System.Text.Json;
using System.Windows;
using Microsoft.Win32;
using Legibyl.Validation;

namespace Legibyl.Gui;

/// <summary>
/// Shows a tagged PDF's logical structure tree (the semantic hierarchy that screen
/// readers use) as a browsable outline, with an option to export it as JSON for AI
/// agents or other tooling.
/// </summary>
public partial class StructureTreeWindow : Window
{
    private static readonly JsonSerializerOptions JsonOptions = new()
    {
        WriteIndented = true,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
    };

    private readonly StructureNode _root;
    private readonly string _suggestedFileName;

    public StructureTreeWindow(StructureNode root, string sourceFilePath)
    {
        InitializeComponent();
        _root = root;
        _suggestedFileName = Path.GetFileNameWithoutExtension(sourceFilePath) + "-structure.json";
        // The synthetic StructTreeRoot node itself carries no semantic info, so show
        // its children (the actual document hierarchy) as the tree's top level.
        StructureTreeControl.ItemsSource = root.Children;
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
