using VeraPdfTool.Validation;
using Xunit;

namespace VeraPdfTool.Validation.Tests;

public class PdfLocationParserTests
{
    [Fact]
    public void Parse_Null_ReturnsNull()
    {
        Assert.Null(PdfLocationParser.Parse(null));
    }

    [Fact]
    public void Parse_Empty_ReturnsNull()
    {
        Assert.Null(PdfLocationParser.Parse(""));
    }

    [Fact]
    public void Parse_DocumentOnly_HasNoPageNumber()
    {
        PdfLocation? location = PdfLocationParser.Parse("root/document[0]");

        Assert.NotNull(location);
        Assert.Null(location!.PageNumber);
        Assert.Equal("Document", location.Breadcrumb);
    }

    [Fact]
    public void Parse_PageWithContentItem_ExtractsOneBasedPageNumber()
    {
        // veraPDF's page index is 0-based ("pages[0]" is page 1).
        PdfLocation? location = PdfLocationParser.Parse(
            "root/document[0]/pages[0](2 0 obj PDPage)/contentStream[0](6 0 obj PDSemanticContentStream)/content[0]/contentItem[0]");

        Assert.NotNull(location);
        Assert.Equal(1, location!.PageNumber);
        Assert.Contains("Page 1", location.Breadcrumb);
        Assert.Contains("Content stream", location.Breadcrumb);
    }

    [Fact]
    public void Parse_LinkAnnotationOnSecondPage_ExtractsPageNumberAndObjectInfo()
    {
        PdfLocation? location = PdfLocationParser.Parse(
            "root/document[0]/pages[1](7 0 obj PDPage)/annots[0](9 0 obj PDLinkAnnot)");

        Assert.NotNull(location);
        Assert.Equal(2, location!.PageNumber);
        Assert.Equal(9, location.ObjectNumber);
        Assert.Equal("PDLinkAnnot", location.ObjectType);
        Assert.Contains("Page 2", location.Breadcrumb);
        Assert.Contains("Link annotation", location.Breadcrumb);
    }

    [Fact]
    public void Parse_TableCellInStructureTree_DescribesTagsReadably()
    {
        PdfLocation? location = PdfLocationParser.Parse(
            "root/document[0]/StructTreeRoot[0](123 0 obj PDStructTreeRoot)/K[0](124 0 obj SEDocument Document)/" +
            "K[231](545 0 obj SETable Table)/K[1](551 0 obj SETR TR)/K[0](552 0 obj SETD TD)");

        Assert.NotNull(location);
        Assert.Null(location!.PageNumber);
        Assert.Equal(552, location.ObjectNumber);
        Assert.Equal("SETD TD", location.ObjectType);
        Assert.Contains("table row (TR)", location.Breadcrumb, StringComparison.OrdinalIgnoreCase);
        Assert.Contains("table cell (TD)", location.Breadcrumb, StringComparison.OrdinalIgnoreCase);
        Assert.Contains("structure tree", location.Breadcrumb, StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void Parse_UnrecognisableString_FallsBackToRawValue()
    {
        PdfLocation? location = PdfLocationParser.Parse("!!! not a real location context ???");

        Assert.NotNull(location);
        Assert.Null(location!.PageNumber);
        Assert.Equal("!!! not a real location context ???", location.Breadcrumb);
    }
}
