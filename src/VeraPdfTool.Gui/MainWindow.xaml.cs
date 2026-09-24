using System.IO;
using System.Text;
using System.Windows;
using System.Windows.Media;
using Microsoft.Win32;
using VeraPdfTool.Validation;

namespace VeraPdfTool.Gui;

/// <summary>
/// Interaction logic for MainWindow.xaml
/// </summary>
public partial class MainWindow : Window
{
    private static readonly Brush PassBrush = new SolidColorBrush(Color.FromRgb(0x1E, 0x7E, 0x34));
    private static readonly Brush FailBrush = new SolidColorBrush(Color.FromRgb(0xB0, 0x2A, 0x37));
    private static readonly Brush ErrorBrush = new SolidColorBrush(Color.FromRgb(0x99, 0x66, 0x00));

    private readonly IPdfValidator _validator = new VeraPdfValidator();
    private readonly IPdfFixer _fixer;
    private string? _selectedFilePath;
    private ValidationReport? _lastReport;

    public MainWindow()
    {
        InitializeComponent();
        ProfileComboBox.ItemsSource = ProfileItem.All;
        ProfileComboBox.SelectedIndex = 0;
        _fixer = (IPdfFixer)_validator;
    }

    private void BrowseButton_Click(object sender, RoutedEventArgs e)
    {
        var dialog = new OpenFileDialog
        {
            Filter = "PDF files (*.pdf)|*.pdf|All files (*.*)|*.*",
            Title = "Select a PDF to validate",
        };

        if (dialog.ShowDialog(this) == true)
        {
            SetSelectedFile(dialog.FileName);
        }
    }

    private void Window_DragOver(object sender, DragEventArgs e)
    {
        e.Effects = e.Data.GetDataPresent(DataFormats.FileDrop) ? DragDropEffects.Copy : DragDropEffects.None;
        e.Handled = true;
    }

    private void Window_Drop(object sender, DragEventArgs e)
    {
        if (e.Data.GetDataPresent(DataFormats.FileDrop) &&
            e.Data.GetData(DataFormats.FileDrop) is string[] { Length: > 0 } files)
        {
            SetSelectedFile(files[0]);
        }
    }

    private void SetSelectedFile(string path)
    {
        _selectedFilePath = path;
        FilePathTextBox.Text = path;
        ValidateButton.IsEnabled = true;
        ClearResults();
    }

    private void ClearResults()
    {
        _lastReport = null;
        ResultsListView.ItemsSource = null;
        StatusBanner.Visibility = Visibility.Collapsed;
        CopySummaryButton.IsEnabled = false;
        SaveReportButton.IsEnabled = false;
        FixButton.IsEnabled = false;
    }

    private async void ValidateButton_Click(object sender, RoutedEventArgs e)
    {
        if (_selectedFilePath is null)
        {
            return;
        }

        var profile = ((ProfileItem)ProfileComboBox.SelectedItem).Profile;

        SetBusy(true);
        ClearResults();

        try
        {
            ValidationReport report = await Task.Run(() => _validator.Validate(_selectedFilePath, profile));
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
        ProgressBar.Visibility = busy ? Visibility.Visible : Visibility.Collapsed;
        ValidateButton.IsEnabled = !busy && _selectedFilePath is not null;
        BrowseButton.IsEnabled = !busy;
        FixButton.IsEnabled = !busy && _lastReport is not null;
    }

    private void ShowReport(ValidationReport report)
    {
        _lastReport = report;

        StatusBanner.Visibility = Visibility.Visible;
        StatusBanner.Background = report.IsCompliant ? PassBrush : FailBrush;
        StatusText.Foreground = Brushes.White;
        StatusText.Text = report.IsCompliant
            ? $"PASS — compliant with {report.FlavourId.ToUpperInvariant()} ({report.TotalAssertions} checks)"
            : $"FAIL — {report.FailedCount} of {report.TotalAssertions} checks failed against {report.FlavourId.ToUpperInvariant()}";

        ResultsListView.ItemsSource = report.Assertions
            .Where(a => !a.Passed)
            .Select(RuleViolationRow.FromViolation)
            .ToList();

        CopySummaryButton.IsEnabled = true;
        SaveReportButton.IsEnabled = true;
        FixButton.IsEnabled = true;
    }

    private void ShowError(string message)
    {
        StatusBanner.Visibility = Visibility.Visible;
        StatusBanner.Background = ErrorBrush;
        StatusText.Foreground = Brushes.White;
        StatusText.Text = $"Error: {message}";
    }

    private async void FixButton_Click(object sender, RoutedEventArgs e)
    {
        if (_lastReport is null || _selectedFilePath is null)
        {
            return;
        }

        var dialog = new SaveFileDialog
        {
            Filter = "PDF files (*.pdf)|*.pdf|All files (*.*)|*.*",
            FileName = Path.GetFileNameWithoutExtension(_selectedFilePath) + ".fixed" + Path.GetExtension(_selectedFilePath),
        };

        if (dialog.ShowDialog(this) != true)
        {
            return;
        }

        var profile = ((ProfileItem)ProfileComboBox.SelectedItem).Profile;

        SetBusy(true);
        try
        {
            FixResult fix = await Task.Run(() => _fixer.Fix(_selectedFilePath, profile, dialog.FileName));
            ShowFixResult(fix);
        }
        catch (VeraPdfValidationException ex)
        {
            MessageBox.Show(this, ex.Message, "Fix failed", MessageBoxButton.OK, MessageBoxImage.Error);
        }
        finally
        {
            SetBusy(false);
        }
    }

    private void ShowFixResult(FixResult fix)
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
        sb.AppendLine("Note: this only repairs XMP metadata (missing metadata stream, PDF/A|PDF/UA");
        sb.AppendLine("identifier, false conformance claims, Info-dictionary sync). It does not fix");
        sb.AppendLine("structural/tagging issues such as untagged content, missing alt-text, or table");
        sb.AppendLine("header scope — re-validate the repaired copy to see what (if anything) improved.");

        MessageBox.Show(this, sb.ToString(), "Fix Metadata", MessageBoxButton.OK, MessageBoxImage.Information);
    }

    private void CopySummaryButton_Click(object sender, RoutedEventArgs e)
    {
        if (_lastReport is null)
        {
            return;
        }

        Clipboard.SetText(BuildSummaryText(_lastReport));
    }

    private void SaveReportButton_Click(object sender, RoutedEventArgs e)
    {
        if (_lastReport is null)
        {
            return;
        }

        var dialog = new SaveFileDialog
        {
            Filter = "Text report (*.txt)|*.txt|All files (*.*)|*.*",
            FileName = Path.GetFileNameWithoutExtension(_lastReport.FilePath) + "-validation-report.txt",
        };

        if (dialog.ShowDialog(this) == true)
        {
            File.WriteAllText(dialog.FileName, BuildSummaryText(_lastReport));
        }
    }

    private static string BuildSummaryText(ValidationReport report)
    {
        var sb = new StringBuilder();
        sb.AppendLine($"File: {report.FilePath}");
        sb.AppendLine($"Profile requested: {report.Profile}");
        sb.AppendLine($"Flavour validated: {report.FlavourId}");
        sb.AppendLine($"Compliant: {report.IsCompliant}");
        sb.AppendLine($"Total checks: {report.TotalAssertions}");
        sb.AppendLine($"Failed checks: {report.FailedCount}");
        sb.AppendLine();

        foreach (var violation in report.FailedAssertions)
        {
            sb.AppendLine(violation.ToString());
        }

        return sb.ToString();
    }
}