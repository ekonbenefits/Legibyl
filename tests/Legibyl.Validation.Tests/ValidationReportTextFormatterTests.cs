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
    public void BuildFinding_DescribesAvailableFixesInPlainWords()
    {
        RuleViolation violation = CreateViolation(
            fixOptions: PdfFixOptions.Metadata | PdfFixOptions.RemoveEmptyParagraphs);

        string text = ValidationReportTextFormatter.BuildFinding(violation);

        Assert.Contains("Fixable: Fix XMP metadata; Remove empty /P structure elements", text);
    }

    [Fact]
    public void BuildFinding_OmitsFixableWhenNoFixApplies()
    {
        string text = ValidationReportTextFormatter.BuildFinding(CreateViolation());

        Assert.DoesNotContain("Fixable:", text);
    }

    [Fact]
    public void BuildReport_IncludesLocationsForEachFailedFinding()
    {
        var report = new ValidationReport(
            filePath: @"C:\documents\sample.pdf",
            profile: PdfProfile.PdfUa1,
            flavourId: "ua1",
            isCompliant: false,
            totalAssertions: 2,
            assertions:
            [
                CreateViolation(),
                CreateViolation(clause: "7.2", testNumber: 3, errorMessage: null, locationContext: null),
            ]);

        string text = ValidationReportTextFormatter.BuildReport(report);

        Assert.Contains("Finding 1", text);
        Assert.Contains("Finding 2", text);
        Assert.Contains($"Raw location: {RawLocation}", text);
        Assert.Contains("Location: Not available", text);
        Assert.Contains("Raw location: Not available", text);
    }

    private static RuleViolation CreateViolation(
        string clause = "7.1",
        int testNumber = 2,
        string? errorMessage = "Missing Contents entry.",
        string? locationContext = RawLocation,
        PdfFixOptions fixOptions = PdfFixOptions.None) => new(
        specification: "ISO 14289-1:2014",
        clause: clause,
        testNumber: testNumber,
        passed: false,
        message: "A link annotation needs alternate text.",
        errorMessage: errorMessage,
        locationContext: locationContext,
        severity: FindingSeverity.Failure,
        fixOptions: fixOptions,
        geometry: null);
}
