namespace VeraPdfTool.Validation;

/// <summary>
/// The outcome of running veraPDF's metadata fixer (org.verapdf.pdfa.MetadataFixer)
/// against a PDF. The fixer only repairs the XMP metadata packet — e.g. adding a
/// missing metadata stream, writing/removing the PDF/A or PDF/UA identification
/// schema, and syncing basic Dublin Core / AdobePDF / XMPBasic fields with the
/// Info dictionary. It does not repair structural/tagging problems (untagged
/// content, missing alt-text, table header scope, etc).
/// </summary>
public sealed record FixResult(
    string FilePath,
    PdfProfile Profile,

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
