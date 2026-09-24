namespace VeraPdfTool.Validation;

/// <summary>
/// Mirrors org.verapdf.pdfa.results.MetadataFixerResult.RepairStatus.
/// </summary>
public enum RepairStatus
{
    /// <summary>Metadata repair was carried out successfully.</summary>
    Success,

    /// <summary>Metadata repair was attempted but failed.</summary>
    FixError,

    /// <summary>The fixer could not determine any action that could repair the document.</summary>
    WontFix,

    /// <summary>No action was taken because the file was already valid.</summary>
    NoAction,

    /// <summary>A false PDF/A or PDF/UA conformance claim was removed.</summary>
    IdRemoved,
}
