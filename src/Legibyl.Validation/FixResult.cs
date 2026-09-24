namespace Legibyl.Validation;

/// <summary>
/// The outcome of running selected best-effort repairs against a PDF.
/// </summary>
public sealed record FixResult(
    string FilePath,
    PdfProfile Profile,
    PdfFixOptions RequestedOptions,

    /// <summary>
    /// Path to the repaired copy, or null if no output file was produced
    /// (nothing needed fixing, or veraPDF determined it couldn't fix anything).
    /// </summary>
    string? OutputPath,
    RepairStatus Status,
    IReadOnlyList<string> AppliedFixes,

    /// <summary>The validation report for the original (unfixed) file, for reference.</summary>
    ValidationReport OriginalReport)
{
    public bool WasFixed => OutputPath is not null && Status is RepairStatus.Success or RepairStatus.IdRemoved;
}
