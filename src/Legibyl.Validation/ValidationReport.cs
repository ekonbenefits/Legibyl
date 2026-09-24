namespace Legibyl.Validation;

/// <summary>
/// The result of validating a single PDF document against a <see cref="PdfProfile"/>.
/// </summary>
public sealed class ValidationReport
{
    public required string FilePath { get; init; }
    public required PdfProfile Profile { get; init; }

    /// <summary>The flavour id actually reported by veraPDF, e.g. "ua1".</summary>
    public required string FlavourId { get; init; }

    /// <summary>True if the document complies with the requested profile.</summary>
    public required bool IsCompliant { get; init; }

    /// <summary>Total number of individual rule checks performed.</summary>
    public required int TotalAssertions { get; init; }

    /// <summary>All recorded rule assertions (both passed and failed).</summary>
    public required IReadOnlyList<RuleViolation> Assertions { get; init; }

    /// <summary>Convenience accessor for only the failed assertions.</summary>
    public IEnumerable<RuleViolation> FailedAssertions => Assertions.Where(a => !a.Passed);

    /// <summary>Failed assertions plus non-failing Legibyl warnings worth showing to the user.</summary>
    public IEnumerable<RuleViolation> Findings => Assertions.Where(a => a.IsFinding);

    public int FailedCount => Assertions.Count(a => !a.Passed);

    public int WarningCount => Assertions.Count(a => a.Severity == FindingSeverity.Warning);
}
