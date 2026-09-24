using VeraPdfTool.Validation;
using Xunit;

namespace VeraPdfTool.Rendering.Tests;

public class PdfRegionRendererTests
{
    private static string CorpusFile(string name) =>
        Path.Combine(AppContext.BaseDirectory, "SampleCorpus", name);

    private const string PngSignatureHex = "89504E47";

    [Fact]
    public void RenderPageWithHighlight_ContentItemFailure_RendersFullPageWithoutPreciseRegion()
    {
        // The known-bad corpus PDF's failures are content-marking issues (Artifact/tagged content),
        // which have no attached geometry in veraPDF's model - only annotations do currently.
        var renderer = new PdfRegionRenderer();
        var location = PdfLocationParser.Parse(
            "root/document[0]/pages[0](11 0 obj PDPage)/contentStream[0](21 0 obj PDSemanticContentStream)/content[10]/contentItem[0]");

        Assert.NotNull(location);

        HighlightImageResult result = renderer.RenderPageWithHighlight(CorpusFile("pdfua1-fail.pdf"), location!);

        Assert.Equal(1, result.PageNumber);
        Assert.False(result.HasPreciseRegion);
        Assert.NotEmpty(result.Note);
        AssertIsPng(result.PngBytes);
    }

    [Fact]
    public void RenderPageWithHighlight_LocationWithoutPageNumber_Throws()
    {
        var renderer = new PdfRegionRenderer();
        var location = PdfLocationParser.Parse("root/document[0]");
        Assert.NotNull(location);

        Assert.Throws<InvalidOperationException>(
            () => renderer.RenderPageWithHighlight(CorpusFile("pdfua1-fail.pdf"), location!));
    }

    [Fact]
    public void RenderPageWithHighlight_PageOutOfRange_Throws()
    {
        var renderer = new PdfRegionRenderer();
        var location = PdfLocationParser.Parse("root/document[0]/pages[999](11 0 obj PDPage)");
        Assert.NotNull(location);

        Assert.Throws<InvalidOperationException>(
            () => renderer.RenderPageWithHighlight(CorpusFile("pdfua1-fail.pdf"), location!));
    }

    private static void AssertIsPng(byte[] bytes)
    {
        Assert.True(bytes.Length > 8);
        string header = Convert.ToHexString(bytes, 0, 4);
        Assert.Equal(PngSignatureHex, header);
    }
}
