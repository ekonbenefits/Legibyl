using System.IO;
using VeraPdfTool.Validation;
using Xunit;

namespace VeraPdfTool.Validation.Tests;

public class VeraPdfValidatorTests
{
    private static string CorpusFile(string name) =>
        Path.Combine(AppContext.BaseDirectory, "SampleCorpus", name);

    [Fact]
    public void Validate_KnownGoodPdfUa1Document_ReportsCompliant()
    {
        var validator = new VeraPdfValidator();

        ValidationReport report = validator.Validate(CorpusFile("pdfua1-pass.pdf"), PdfProfile.PdfUa1);

        Assert.True(report.IsCompliant);
        Assert.Equal("ua1", report.FlavourId);
        Assert.True(report.TotalAssertions > 0);
        Assert.Empty(report.FailedAssertions);
    }

    [Fact]
    public void Validate_KnownBadPdfUa1Document_ReportsNonCompliantWithFailedRules()
    {
        var validator = new VeraPdfValidator();

        ValidationReport report = validator.Validate(CorpusFile("pdfua1-fail.pdf"), PdfProfile.PdfUa1);

        Assert.False(report.IsCompliant);
        Assert.NotEmpty(report.FailedAssertions);
        Assert.Contains(report.FailedAssertions, a => a.Clause == "7.1");
    }

    [Fact]
    public void Validate_MissingFile_ThrowsVeraPdfValidationException()
    {
        var validator = new VeraPdfValidator();

        Assert.Throws<VeraPdfValidationException>(
            () => validator.Validate(CorpusFile("does-not-exist.pdf"), PdfProfile.PdfUa1));
    }
}
