namespace Legibyl.Validation;

/// <summary>
/// The outcome of a single validation rule check (org.verapdf.pdfa.results.TestAssertion).
/// </summary>
/// <param name="Specification">The specification identifier, e.g. "ISO 14289-1:2014".</param>
/// <param name="Clause">The specification clause, e.g. "7.1".</param>
/// <param name="TestNumber">The test number within the clause.</param>
/// <param name="Passed">Whether this individual assertion passed.</param>
/// <param name="Message">The human-readable rule description/message.</param>
/// <param name="ErrorMessage">Additional error detail, if any.</param>
/// <param name="LocationContext">The context/path within the PDF where the assertion was checked.</param>
public sealed record RuleViolation(
    string Specification,
    string Clause,
    int TestNumber,
    bool Passed,
    string Message,
    string? ErrorMessage,
    string? LocationContext)
{
    /// <summary>A short "clause.testNumber" identifier, e.g. "7.1-2".</summary>
    public string RuleId => $"{Clause}-{TestNumber}";

    /// <summary>
    /// Best-effort, human-readable interpretation of <see cref="LocationContext"/>
    /// (page number + plain-English breadcrumb). Null if there's no location context
    /// to parse. See <see cref="PdfLocationParser"/>.
    /// </summary>
    public PdfLocation? Location => PdfLocationParser.Parse(LocationContext);

    public override string ToString() =>
        $"[{Specification} {RuleId}] {Message}";
}
