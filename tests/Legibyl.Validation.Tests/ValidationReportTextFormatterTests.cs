using Legibyl.Validation;

namespace Legibyl.Validation.Tests;

public class ValidationReportTextFormatterTests
{
    private const string RawLocation =
        "root/document[0]/pages[1](7 0 obj PDPage)/annots[0](9 0 obj PDLinkAnnot)";

    [Fact]
    public void BuildFinding_IncludesFriendlyAndRawLocations()
    {
        RuleViolation violation = CreateViolation();

        string text = ValidationReportTextFormatter.BuildFinding(violation);

        Assert.Contains("Location: Document → Page 2 → Link annotation", text);
        Assert.Contains($"Raw location: {RawLocation}", text);
        Assert.Contains("Rule: ISO 14289-1:2014 7.1-2", text);
        Assert.Contains("Message: A link annotation needs alternate text.", text);
        Assert.Contains("Error: Missing Contents entry.", text);
    }

    [Fact]
    public void BuildReport_IncludesLocationsForEachFailedFinding()
    {
        var report = new ValidationReport
        {
            FilePath = @"C:\documents\sample.pdf",
            Profile = PdfProfile.PdfUa1,
            FlavourId = "ua1",
            IsCompliant = false,
            TotalAssertions = 2,
            Assertions =
            [
                CreateViolation(),
                CreateViolation() with
                {
                    Clause = "7.2",
                    TestNumber = 3,
                    LocationContext = null,
                    ErrorMessage = null,
                },
            ],
        };

        string text = ValidationReportTextFormatter.BuildReport(report);

        Assert.Contains("Finding 1", text);
        Assert.Contains("Finding 2", text);
        Assert.Contains($"Raw location: {RawLocation}", text);
        Assert.Contains("Location: Not available", text);
        Assert.Contains("Raw location: Not available", text);
    }

    private static RuleViolation CreateViolation() => new(
        Specification: "ISO 14289-1:2014",
        Clause: "7.1",
        TestNumber: 2,
        Passed: false,
        Message: "A link annotation needs alternate text.",
        ErrorMessage: "Missing Contents entry.",
        LocationContext: RawLocation);
}
