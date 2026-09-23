namespace VeraPdfTool.Cli;

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
