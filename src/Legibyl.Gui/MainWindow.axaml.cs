using System.IO;
using System.Text;
using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Input.Platform;
using Avalonia.Interactivity;
using Avalonia.Media;
using Avalonia.Media.Imaging;
using Avalonia.Platform.Storage;
using Legibyl.Rendering;
using Legibyl.Validation;

namespace Legibyl.Gui;

/// <summary>
/// Interaction logic for MainWindow.axaml
/// </summary>
public partial class MainWindow : Window
{
    private static readonly IBrush PassBrush = new SolidColorBrush(Color.FromRgb(0x1E, 0x7E, 0x34));
    private static readonly IBrush FailBrush = new SolidColorBrush(Color.FromRgb(0xB0, 0x2A, 0x37));
    private static readonly IBrush ErrorBrush = new SolidColorBrush(Color.FromRgb(0x99, 0x66, 0x00));

    internal static readonly FilePickerFileType PdfFileType = new("PDF files")
    {
        Patterns = ["*.pdf"],
        MimeTypes = ["application/pdf"],
        AppleUniformTypeIdentifiers = ["com.adobe.pdf"],
    };

    private readonly IPdfValidator _validator = new VeraPdfValidator();
    private readonly IPdfFixer _fixer;
    private readonly IStructureTreeReader _structureReader;
    private readonly IPdfRegionRenderer _regionRenderer = new PdfRegionRenderer();
    private string? _selectedFilePath;
    private ValidationReport? _lastReport;
    private bool _isBusy;
    private int _previewRequestVersion;

    public MainWindow()
    {
        InitializeComponent();
        ProfileComboBox.ItemsSource = ProfileItem.All;
        ProfileComboBox.SelectedIndex = 0;
        _fixer = (IPdfFixer)_validator;
        _structureReader = (IStructureTreeReader)_validator;

        AddHandler(DragDrop.DragOverEvent, Window_DragOver);
        AddHandler(DragDrop.DropEvent, Window_Drop);
    }

    private async void BrowseButton_Click(object? sender, RoutedEventArgs e)
    {
        var files = await StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
        {
            Title = "Select a PDF to validate",
            AllowMultiple = false,
            FileTypeFilter = [PdfFileType, FilePickerFileTypes.All],
        });

        if (files.Count > 0 && files[0].TryGetLocalPath() is { } path)
        {
            SetSelectedFile(path);
        }
    }

    private void Window_DragOver(object? sender, DragEventArgs e)
    {
        e.DragEffects = e.DataTransfer.Contains(DataFormat.File) ? DragDropEffects.Copy : DragDropEffects.None;
        e.Handled = true;
    }

    private void Window_Drop(object? sender, DragEventArgs e)
    {
        if (e.DataTransfer.TryGetFiles() is { Length: > 0 } files &&
            files[0].TryGetLocalPath() is { } path)
        {
            SetSelectedFile(path);
        }
    }

    private void SetSelectedFile(string path)
    {
        _selectedFilePath = path;
        FilePathTextBox.Text = path;
        ValidateButton.IsEnabled = true;
        ViewStructureButton.IsEnabled = true;
        ClearResults();
        _ = RunValidationAsync(); // Auto-validate as soon as a file is picked/dropped, using the current profile.
    }

    private void ClearResults()
    {
        _lastReport = null;
        ResultsGrid.ItemsSource = null;
        StatusBanner.IsVisible = false;
        CopySummaryButton.IsEnabled = false;
        SaveReportButton.IsEnabled = false;
        FixButton.IsEnabled = false;
        FindingDetailsPanel.IsVisible = false;
        RegionImage.Source = null;
        CopyFindingButton.IsEnabled = false;
        PreviewNoteText.Text = string.Empty;
        PreviewProgressBar.IsVisible = false;
        _previewRequestVersion++;
    }

    private async void ValidateButton_Click(object? sender, RoutedEventArgs e) => await RunValidationAsync();

    private async void ProfileComboBox_SelectionChanged(object? sender, SelectionChangedEventArgs e)
    {
        // Re-validate automatically when the profile changes, as long as a file is already selected
        // and we're not already mid-validation (guards against re-entrancy/startup selection events).
        if (_selectedFilePath is not null && !_isBusy)
        {
            await RunValidationAsync();
        }
    }

    private async Task RunValidationAsync()
    {
        if (_selectedFilePath is null || _isBusy)
        {
            return;
        }

        var profile = ((ProfileItem)ProfileComboBox.SelectedItem!).Profile;
        string filePath = _selectedFilePath;

        SetBusy(true);
        ClearResults();

        try
        {
            ValidationReport report = await Task.Run(() => _validator.Validate(filePath, profile));
            ShowReport(report);
        }
        catch (VeraPdfValidationException ex)
        {
            ShowError(ex.Message);
        }
        finally
        {
            SetBusy(false);
        }
    }

    private void SetBusy(bool busy)
    {
        _isBusy = busy;
        ProgressBar.IsVisible = busy;
        ValidateButton.IsEnabled = !busy && _selectedFilePath is not null;
        BrowseButton.IsEnabled = !busy;
        ProfileComboBox.IsEnabled = !busy;
        FixButton.IsEnabled = !busy && CanAttemptFix;
        ViewStructureButton.IsEnabled = !busy && _selectedFilePath is not null;
    }

    /// <summary>
    /// Whether it's worth offering fixes right now. The current fixers target known
    /// validation failures, so hide them once the file is already compliant.
    /// </summary>
    private bool CanAttemptFix => _lastReport is { IsCompliant: false } or { WarningCount: > 0 };

    private void ShowReport(ValidationReport report)
    {
        _lastReport = report;

        StatusBanner.IsVisible = true;
        StatusBanner.Background = report.IsCompliant ? PassBrush : FailBrush;
        StatusText.Foreground = Brushes.White;
        StatusText.Text = report.IsCompliant
            ? $"PASS — compliant with {report.FlavourId.ToUpperInvariant()} ({report.TotalAssertions} checks, {report.WarningCount} warnings)"
            : $"FAIL — {report.FailedCount} of {report.TotalAssertions} checks failed against {report.FlavourId.ToUpperInvariant()} ({report.WarningCount} warnings)";

        List<RuleViolationRow> rows = report.Findings
            .Select(RuleViolationRow.FromViolation)
            // Unlocated failures (no page number) sort to the end, after page-anchored ones in page order.
            .OrderBy(row => row.PageNumber ?? int.MaxValue)
            .ThenBy(row => row.Clause)
            .ThenBy(row => row.TestNumber)
            .ToList();
        ResultsGrid.ItemsSource = rows;

        CopySummaryButton.IsEnabled = true;
        SaveReportButton.IsEnabled = true;
        FixButton.IsEnabled = CanAttemptFix;

        if (rows.Count > 0)
        {
            ResultsGrid.SelectedIndex = 0;
        }
    }

    private void ShowError(string message)
    {
        StatusBanner.IsVisible = true;
        StatusBanner.Background = ErrorBrush;
        StatusText.Foreground = Brushes.White;
        StatusText.Text = $"Error: {message}";
    }

    private async void ResultsGrid_SelectionChanged(object? sender, SelectionChangedEventArgs e)
    {
        int requestVersion = ++_previewRequestVersion;
        if (ResultsGrid.SelectedItem is not RuleViolationRow row)
        {
            FindingDetailsPanel.IsVisible = false;
            CopyFindingButton.IsEnabled = false;
            return;
        }

        FindingDetailsPanel.IsVisible = true;
        FindingHeadingText.Text = $"{row.Severity}: clause {row.Clause}, test {row.TestNumber} — {row.PageDisplay}";
        FindingMessageText.Text = row.Message;
        FindingLocationText.Text = row.FriendlyLocation;
        FindingRawLocationText.Text = row.LocationContext ?? "Not available";
        CopyFindingButton.IsEnabled = true;
        RegionImage.Source = null;
        PreviewNoteText.Text = string.Empty;

        PdfLocation? location = PdfLocationParser.Parse(row.LocationContext);
        string? filePath = _selectedFilePath;
        if (filePath is null || location is null)
        {
            PreviewProgressBar.IsVisible = false;
            PreviewNoteText.Text = "This finding is not associated with a specific PDF page.";
            return;
        }

        PreviewProgressBar.IsVisible = true;
        try
        {
            HighlightImageResult result = await Task.Run(
                () => _regionRenderer.RenderPageWithHighlight(filePath, location));

            if (requestVersion != _previewRequestVersion)
            {
                return;
            }

            RegionImage.Source = CreateBitmap(result.PngBytes);
            PreviewNoteText.Text = result.HasPreciseRegion ? string.Empty : result.Note;
        }
        catch (Exception ex) when (ex is InvalidOperationException or IOException)
        {
            if (requestVersion == _previewRequestVersion)
            {
                PreviewNoteText.Text = $"The page preview could not be rendered: {ex.Message}";
            }
        }
        finally
        {
            if (requestVersion == _previewRequestVersion)
            {
                PreviewProgressBar.IsVisible = false;
            }
        }
    }

    internal static Bitmap CreateBitmap(byte[] pngBytes)
    {
        using var stream = new MemoryStream(pngBytes);
        return new Bitmap(stream);
    }

    private async void ViewStructureButton_Click(object? sender, RoutedEventArgs e)
    {
        if (_selectedFilePath is null)
        {
            return;
        }

        string filePath = _selectedFilePath;
        SetBusy(true);
        try
        {
            StructureNode? structure = await Task.Run(() => _structureReader.ReadStructureTree(filePath));
            if (structure is null)
            {
                await MessageDialog.ShowAsync(this,
                    "This PDF has no structure tree at all — it is completely untagged, so there is no " +
                    "semantic hierarchy for a screen reader to use.",
                    "Document Structure");
                return;
            }

            var window = new StructureTreeWindow(structure, filePath);
            window.Show(this);
        }
        catch (VeraPdfValidationException ex)
        {
            await MessageDialog.ShowAsync(this, ex.Message, "Could not read structure");
        }
        finally
        {
            SetBusy(false);
        }
    }

    private async void FixButton_Click(object? sender, RoutedEventArgs e)
    {
        if (_lastReport is null || _selectedFilePath is null)
        {
            return;
        }

        string sourcePath = _selectedFilePath;
        var fixOptionsDialog = new FixOptionsDialog();
        if (!await fixOptionsDialog.ShowDialog<bool>(this))
        {
            return;
        }

        string? outputPath = await PickSavePathAsync(this, new FilePickerSaveOptions
        {
            Title = "Save repaired PDF",
            SuggestedFileName = Path.GetFileNameWithoutExtension(sourcePath) + ".fixed" + Path.GetExtension(sourcePath),
            DefaultExtension = "pdf",
            FileTypeChoices = [PdfFileType, FilePickerFileTypes.All],
        }, sourcePath);

        if (outputPath is null)
        {
            return;
        }

        var profile = ((ProfileItem)ProfileComboBox.SelectedItem!).Profile;
        string? repairedPath = null;

        SetBusy(true);
        try
        {
            PdfFixOptions options = fixOptionsDialog.SelectedOptions;
            FixResult fix = await Task.Run(() => _fixer.Fix(sourcePath, profile, outputPath, options));
            await ShowFixResultAsync(fix);

            if (fix.WasFixed)
            {
                repairedPath = fix.OutputPath;
            }
        }
        catch (VeraPdfValidationException ex)
        {
            await MessageDialog.ShowAsync(this, ex.Message, "Fix failed");
        }
        finally
        {
            SetBusy(false);
        }

        if (repairedPath is not null)
        {
            // Switch after clearing the busy flag so SetSelectedFile's auto-validation
            // can actually start instead of being ignored by RunValidationAsync.
            SetSelectedFile(repairedPath);
        }
    }

    /// <summary>
    /// Shows a save dialog starting in the source file's folder and returns the chosen
    /// local path, or null if the user cancelled.
    /// </summary>
    internal static async Task<string?> PickSavePathAsync(TopLevel topLevel, FilePickerSaveOptions options, string? nearPath)
    {
        string? folder = nearPath is null ? null : Path.GetDirectoryName(nearPath);
        if (!string.IsNullOrEmpty(folder))
        {
            options.SuggestedStartLocation = await topLevel.StorageProvider.TryGetFolderFromPathAsync(folder);
        }

        IStorageFile? file = await topLevel.StorageProvider.SaveFilePickerAsync(options);
        return file?.TryGetLocalPath();
    }

    private Task ShowFixResultAsync(FixResult fix)
    {
        var sb = new StringBuilder();
        sb.AppendLine($"Repair status: {fix.Status}");
        sb.AppendLine(fix.OutputPath is not null
            ? $"Repaired copy written to: {fix.OutputPath}"
            : "No output file written (nothing the fixer could repair, or the file was already valid).");

        if (fix.AppliedFixes.Count > 0)
        {
            sb.AppendLine();
            sb.AppendLine("Fixes applied:");
            foreach (var applied in fix.AppliedFixes)
            {
                sb.AppendLine($"  - {applied}");
            }
        }

        sb.AppendLine();
        sb.AppendLine("Note: fixes are best-effort and limited to the selected repair types.");
        sb.AppendLine("They do not add missing alt text, retag untagged content, or infer table headers.");
        if (fix.WasFixed)
        {
            sb.AppendLine("Switching to the repaired copy and re-validating to show what improved.");
        }

        return MessageDialog.ShowAsync(this, sb.ToString(), "Fix PDF");
    }

    private async void CopySummaryButton_Click(object? sender, RoutedEventArgs e)
    {
        if (_lastReport is null || Clipboard is null)
        {
            return;
        }

        await Clipboard.SetTextAsync(ValidationReportTextFormatter.BuildReport(_lastReport));
    }

    private async void CopyFindingButton_Click(object? sender, RoutedEventArgs e)
    {
        if (ResultsGrid.SelectedItem is RuleViolationRow row && Clipboard is not null)
        {
            await Clipboard.SetTextAsync(ValidationReportTextFormatter.BuildFinding(row.Violation));
        }
    }

    private async void SaveReportButton_Click(object? sender, RoutedEventArgs e)
    {
        if (_lastReport is null)
        {
            return;
        }

        ValidationReport report = _lastReport;
        string? path = await PickSavePathAsync(this, new FilePickerSaveOptions
        {
            Title = "Save validation report",
            SuggestedFileName = Path.GetFileNameWithoutExtension(report.FilePath) + "-validation-report.txt",
            DefaultExtension = "txt",
            FileTypeChoices = [new FilePickerFileType("Text report") { Patterns = ["*.txt"], MimeTypes = ["text/plain"] }, FilePickerFileTypes.All],
        }, report.FilePath);

        if (path is not null)
        {
            File.WriteAllText(path, ValidationReportTextFormatter.BuildReport(report));
        }
    }
}
