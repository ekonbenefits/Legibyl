using System.IO;
using System.Linq;
using iText.Kernel.Pdf;
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
        Assert.NotNull(heading.Geometry);
        Assert.Equal(1, heading.Geometry!.PageNumber);
        Assert.True(heading.Geometry.Right > heading.Geometry.Left);
        Assert.True(heading.Geometry.Top > heading.Geometry.Bottom);
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

    [Fact]
    public void ReadStructureTree_LinkAnnotation_ReportsInternalDestination()
    {
        string input = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.pdf");
        try
        {
            CreatePdfWithTaggedInternalLink(input);
            IStructureTreeReader reader = new VeraPdfValidator();

            StructureNode? root = reader.ReadStructureTree(input);

            Assert.NotNull(root);
            StructureNode link = Assert.Single(root!.Children, n => n.Tag == "Link");
            Assert.Null(link.LinkUri);
            Assert.Equal(2, link.LinkDestinationPageNumber);
            Assert.Contains("page 2", link.LinkDestinationDescription);
        }
        finally
        {
            if (File.Exists(input))
            {
                File.Delete(input);
            }
        }
    }

    [Fact]
    public void ReadStructureTree_TocItemWithChildLinkDestination_ReportsDestinationOnTocItem()
    {
        string input = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.pdf");
        try
        {
            CreatePdfWithTaggedTocItemLink(input);
            IStructureTreeReader reader = new VeraPdfValidator();

            StructureNode? root = reader.ReadStructureTree(input);

            Assert.NotNull(root);
            StructureNode tocItem = Assert.Single(root!.Children, n => n.Tag == "TOCI");
            Assert.Equal(2, tocItem.LinkDestinationPageNumber);
            Assert.Contains("page 2", tocItem.LinkDestinationDescription);
        }
        finally
        {
            if (File.Exists(input))
            {
                File.Delete(input);
            }
        }
    }

    private static void CreatePdfWithTaggedTocItemLink(string path)
    {
        using var document = new PdfDocument(new PdfWriter(path));
        PdfPage page1 = document.AddNewPage();
        PdfPage page2 = document.AddNewPage();

        var destination = new PdfArray();
        destination.Add(page2.GetPdfObject());
        destination.Add(PdfName.XYZ);
        destination.Add(new PdfNumber(33));
        destination.Add(new PdfNumber(690));
        destination.Add(new PdfNumber(0));

        var annotation = new PdfDictionary();
        annotation.Put(PdfName.Type, PdfName.Annot);
        annotation.Put(PdfName.Subtype, PdfName.Link);
        var rect = new PdfArray();
        rect.Add(new PdfNumber(10));
        rect.Add(new PdfNumber(10));
        rect.Add(new PdfNumber(100));
        rect.Add(new PdfNumber(30));
        annotation.Put(PdfName.Rect, rect);
        annotation.Put(PdfName.Dest, destination);
        annotation.MakeIndirect(document);

        var annotations = new PdfArray();
        annotations.Add(annotation);
        page1.GetPdfObject().Put(PdfName.Annots, annotations);

        var visibleLink = new PdfDictionary();
        visibleLink.Put(PdfName.Type, PdfName.StructElem);
        visibleLink.Put(PdfName.S, PdfName.Link);
        visibleLink.Put(PdfName.Pg, page1.GetPdfObject());

        var objr = new PdfDictionary();
        objr.Put(PdfName.Type, PdfName.OBJR);
        objr.Put(PdfName.Obj, annotation.GetIndirectReference());

        var targetLink = new PdfDictionary();
        targetLink.Put(PdfName.Type, PdfName.StructElem);
        targetLink.Put(PdfName.S, PdfName.Link);
        targetLink.Put(PdfName.Pg, page1.GetPdfObject());
        targetLink.Put(PdfName.K, objr);

        var tocItemChildren = new PdfArray();
        tocItemChildren.Add(visibleLink);
        tocItemChildren.Add(targetLink);

        var tocItem = new PdfDictionary();
        tocItem.Put(PdfName.Type, PdfName.StructElem);
        tocItem.Put(PdfName.S, new PdfName("TOCI"));
        tocItem.Put(PdfName.Pg, page1.GetPdfObject());
        tocItem.Put(PdfName.K, tocItemChildren);

        var structTreeRoot = new PdfDictionary();
        structTreeRoot.Put(PdfName.Type, PdfName.StructTreeRoot);
        structTreeRoot.Put(PdfName.K, tocItem);
        document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, structTreeRoot);
    }

    private static void CreatePdfWithTaggedInternalLink(string path)
    {
        using var document = new PdfDocument(new PdfWriter(path));
        PdfPage page1 = document.AddNewPage();
        PdfPage page2 = document.AddNewPage();

        var destination = new PdfArray();
        destination.Add(page2.GetPdfObject());
        destination.Add(PdfName.XYZ);
        destination.Add(new PdfNumber(33));
        destination.Add(new PdfNumber(690));
        destination.Add(new PdfNumber(0));

        var annotation = new PdfDictionary();
        annotation.Put(PdfName.Type, PdfName.Annot);
        annotation.Put(PdfName.Subtype, PdfName.Link);
        var rect = new PdfArray();
        rect.Add(new PdfNumber(10));
        rect.Add(new PdfNumber(10));
        rect.Add(new PdfNumber(100));
        rect.Add(new PdfNumber(30));
        annotation.Put(PdfName.Rect, rect);
        annotation.Put(PdfName.Dest, destination);
        annotation.MakeIndirect(document);

        var annotations = new PdfArray();
        annotations.Add(annotation);
        page1.GetPdfObject().Put(PdfName.Annots, annotations);

        var objr = new PdfDictionary();
        objr.Put(PdfName.Type, PdfName.OBJR);
        objr.Put(PdfName.Obj, annotation.GetIndirectReference());

        var link = new PdfDictionary();
        link.Put(PdfName.Type, PdfName.StructElem);
        link.Put(PdfName.S, PdfName.Link);
        link.Put(PdfName.Pg, page1.GetPdfObject());
        link.Put(PdfName.K, objr);

        var structTreeRoot = new PdfDictionary();
        structTreeRoot.Put(PdfName.Type, PdfName.StructTreeRoot);
        structTreeRoot.Put(PdfName.K, link);
        document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, structTreeRoot);
    }
}
