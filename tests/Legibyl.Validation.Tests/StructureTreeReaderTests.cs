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
        StructureNode heading = Assert.Single(document.Children, n => n.Tag == "H1");
        Assert.Equal("EN-US", heading.Language);
        Assert.Equal(1, heading.PageNumber);
        Assert.Equal("The graphics objects in a document can be divided into two classes:", heading.Content);
        Assert.Contains(document.Children, n => n.Tag == "L");

        StructureNode note = Assert.Single(document.Children, n => n.Tag == "Note");
        StructureNode noteText = Assert.Single(note.Children, n => n.Tag == "Span");
        Assert.StartsWith("Artifacts may also be used to describe areas of the document", noteText.Content);
    }

    [Fact]
    public void ReadStructureTree_MissingFile_ThrowsVeraPdfValidationException()
    {
        IStructureTreeReader reader = new VeraPdfValidator();

        Assert.Throws<VeraPdfValidationException>(
            () => reader.ReadStructureTree(CorpusFile("does-not-exist.pdf")));
    }
}
