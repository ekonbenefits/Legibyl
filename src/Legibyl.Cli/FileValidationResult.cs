namespace Legibyl.Cli;

/// <summary>JSON-serializable result for a single validated file. Field names are camelCased on output.</summary>
public sealed class FileValidationResult
{
    public required string File { get; init; }
    public required string Profile { get; init; }

    /// <summary>The flavour id actually validated against (e.g. "ua1"), null if validation errored.</summary>
    public string? FlavourId { get; init; }

    /// <summary>True/false if validation completed, null if it errored before a result was produced.</summary>
    public bool? IsCompliant { get; init; }

    public int? TotalAssertions { get; init; }
    public int? FailedCount { get; init; }

    /// <summary>Only the failed rule checks; empty (not omitted) when compliant, null when errored.</summary>
    public IReadOnlyList<FailureDetail>? Failures { get; init; }

    /// <summary>Non-null only if the file could not be validated (missing, corrupt, encrypted, etc).</summary>
    public string? Error { get; init; }

    /// <summary>
    /// Repair status from veraPDF's metadata fixer (e.g. "Success", "NoAction"), only
    /// present when --fix was requested. See <see cref="FixOutputPath"/> remarks.
    /// </summary>
    public string? FixStatus { get; init; }

    /// <summary>
    /// Path to the repaired copy, present only if a fix was actually written (the
    /// fixer only writes an output file when it changes something — metadata only,
    /// e.g. adding a missing XMP stream or PDF/A|UA identifier; it never repairs
    /// structural/tagging issues).
    /// </summary>
    public string? FixOutputPath { get; init; }

    /// <summary>Human-readable descriptions of the fixes actually applied, if any.</summary>
    public IReadOnlyList<string>? FixAppliedFixes { get; init; }

    /// <summary>Non-null only if --fix was requested but the fix attempt itself errored.</summary>
    public string? FixError { get; init; }
}

public sealed class FailureDetail
{
    public required string Specification { get; init; }
    public required string Clause { get; init; }
    public required int TestNumber { get; init; }
    public required string Message { get; init; }
    public string? ErrorMessage { get; init; }
    public string? LocationContext { get; init; }
}
