using Legibyl.Validation;

namespace Legibyl.Gui;

/// <summary>Flattened, display-friendly view of a <see cref="RuleViolation"/> for the results grid.</summary>
public sealed class RuleViolationRow
{
    public required string StatusLabel { get; init; }
    public required string Clause { get; init; }
    public required int TestNumber { get; init; }
    public required string Message { get; init; }
    public required string? LocationContext { get; init; }

    /// <summary>1-based page number, if the failure is anchored to a page. Null otherwise.</summary>
    public required int? PageNumber { get; init; }

    /// <summary>"Page 3" or "—" if no page is known — bound directly to the grid's Page column.</summary>
    public string PageDisplay => PageNumber is { } page ? $"Page {page}" : "—";

    /// <summary>Plain-English breadcrumb, e.g. "Document → Page 2 → Link annotation". Falls back to the raw context.</summary>
    public required string FriendlyLocation { get; init; }

    public static RuleViolationRow FromViolation(RuleViolation violation)
    {
        PdfLocation? location = violation.Location;
        return new RuleViolationRow
        {
            StatusLabel = violation.Passed ? "Pass" : "Fail",
            Clause = violation.Clause,
            TestNumber = violation.TestNumber,
            Message = violation.Message,
            LocationContext = violation.LocationContext,
            PageNumber = location?.PageNumber,
            FriendlyLocation = location?.Breadcrumb ?? violation.LocationContext ?? string.Empty,
        };
    }
}
