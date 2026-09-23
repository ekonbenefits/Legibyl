using VeraPdfTool.Validation;

namespace VeraPdfTool.Gui;

/// <summary>Flattened, display-friendly view of a <see cref="RuleViolation"/> for the results grid.</summary>
public sealed class RuleViolationRow
{
    public required string StatusLabel { get; init; }
    public required string Clause { get; init; }
    public required int TestNumber { get; init; }
    public required string Message { get; init; }
    public required string? LocationContext { get; init; }

    public static RuleViolationRow FromViolation(RuleViolation violation) => new()
    {
        StatusLabel = violation.Passed ? "Pass" : "Fail",
        Clause = violation.Clause,
        TestNumber = violation.TestNumber,
        Message = violation.Message,
        LocationContext = violation.LocationContext,
    };
}
