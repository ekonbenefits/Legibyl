using System.IO;
using System.Linq;
using Legibyl.Validation;
using Xunit;

namespace Legibyl.Validation.Tests;

public class StructureTreeReaderTests
{
    private static string CorpusFile(string name) =>
        Path.Combine(AppContext.BaseDirectory, "SampleCorpus", name);

    [Fact]
    public void ReadStructureTree_TaggedDocument_ReturnsExpectedHierarchy()
    {
        IStructureTreeReader reader = new VeraPdfValidator();

        StructureNode? root = reader.ReadStructureTree(CorpusFile("pdfua1-pass.pdf"));

        Assert.NotNull(root);
        Assert.Equal("StructTreeRoot", root!.Tag);
        StructureNode document = Assert.Single(root.Children);
        Assert.Equal("Document", document.Tag);
        Assert.Contains(document.Children, n => n.Tag == "H1" && n.Language == "EN-US" && n.PageNumber == 1);
        Assert.Contains(document.Children, n => n.Tag == "L");
    }

    [Fact]
    public void ReadStructureTree_MissingFile_ThrowsVeraPdfValidationException()
    {
        IStructureTreeReader reader = new VeraPdfValidator();

        Assert.Throws<VeraPdfValidationException>(
            () => reader.ReadStructureTree(CorpusFile("does-not-exist.pdf")));
    }
}
