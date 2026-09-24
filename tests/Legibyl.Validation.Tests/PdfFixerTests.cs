using iText.Kernel.Pdf;
using Legibyl.Validation;

namespace Legibyl.Validation.Tests;

public class PdfFixerTests
{
    [Fact]
    public void Fix_AnnotationTabsAndEmptyParagraphs_AppliesSelectedStructuralFixes()
    {
        string input = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.pdf");
        string output = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.fixed.pdf");
        try
        {
            CreatePdfWithAnnotationPageAndEmptyParagraphs(input);
            var fixer = new VeraPdfValidator();

            FixResult result = fixer.Fix(
                input,
                PdfProfile.PdfUa1,
                output,
                PdfFixOptions.AnnotationTabs | PdfFixOptions.RemoveEmptyParagraphs);

            Assert.True(result.WasFixed);
            Assert.Equal(output, result.OutputPath);
            Assert.Contains(result.AppliedFixes, fix => fix.Contains("/Tabs /S", StringComparison.Ordinal));
            Assert.Contains(result.AppliedFixes, fix => fix.Contains("empty /P", StringComparison.Ordinal));

            using var document = new PdfDocument(new PdfReader(output));
            Assert.Equal(PdfName.S, document.GetPage(1).GetPdfObject().GetAsName(PdfName.Tabs));

            PdfArray kids = document.GetCatalog()
                .GetPdfObject()
                .GetAsDictionary(PdfName.StructTreeRoot)!
                .GetAsArray(PdfName.K)!;
            Assert.Single(kids);
            PdfDictionary remainingParagraph = kids.GetAsDictionary(0)!;
            Assert.Equal(PdfName.P, remainingParagraph.GetAsName(PdfName.S));
            Assert.Equal(new PdfNumber(0), remainingParagraph.Get(PdfName.K));
        }
        finally
        {
            DeleteIfExists(input);
            DeleteIfExists(output);
        }
    }

    [Fact]
    public void Fix_NoSelectedStructuralChanges_DoesNotWriteOutput()
    {
        string input = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.pdf");
        string output = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.fixed.pdf");
        try
        {
            CreatePdfWithAnnotationTabsAndNonEmptyParagraph(input);
            var fixer = new VeraPdfValidator();

            FixResult result = fixer.Fix(
                input,
                PdfProfile.PdfUa1,
                output,
                PdfFixOptions.AnnotationTabs | PdfFixOptions.RemoveEmptyParagraphs);

            Assert.False(result.WasFixed);
            Assert.Null(result.OutputPath);
            Assert.False(File.Exists(output));
        }
        finally
        {
            DeleteIfExists(input);
            DeleteIfExists(output);
        }
    }

    [Fact]
    public void Fix_DirectEmptyParagraphRootKid_RemovesRootKid()
    {
        string input = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.pdf");
        string output = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.fixed.pdf");
        try
        {
            CreatePdfWithDirectEmptyParagraphRootKid(input);
            var fixer = new VeraPdfValidator();

            FixResult result = fixer.Fix(
                input,
                PdfProfile.PdfUa1,
                output,
                PdfFixOptions.RemoveEmptyParagraphs);

            Assert.True(result.WasFixed);
            using var document = new PdfDocument(new PdfReader(output));
            PdfDictionary structTreeRoot = document.GetCatalog()
                .GetPdfObject()
                .GetAsDictionary(PdfName.StructTreeRoot)!;
            Assert.Null(structTreeRoot.Get(PdfName.K));
        }
        finally
        {
            DeleteIfExists(input);
            DeleteIfExists(output);
        }
    }


    private static void CreatePdfWithAnnotationPageAndEmptyParagraphs(string path)
    {
        using var document = new PdfDocument(new PdfWriter(path));
        PdfPage page = document.AddNewPage();
        var annotations = new PdfArray();
        annotations.Add(new PdfDictionary());
        page.GetPdfObject().Put(PdfName.Annots, annotations);

        var kids = new PdfArray();
        kids.Add(Paragraph());
        PdfDictionary emptyArrayParagraph = Paragraph();
        emptyArrayParagraph.Put(PdfName.K, new PdfArray());
        kids.Add(emptyArrayParagraph);
        PdfDictionary nonEmptyParagraph = Paragraph();
        nonEmptyParagraph.Put(PdfName.K, new PdfNumber(0));
        kids.Add(nonEmptyParagraph);

        var structTreeRoot = new PdfDictionary();
        structTreeRoot.Put(PdfName.Type, PdfName.StructTreeRoot);
        structTreeRoot.Put(PdfName.K, kids);
        document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, structTreeRoot);
    }

    private static void CreatePdfWithAnnotationTabsAndNonEmptyParagraph(string path)
    {
        using var document = new PdfDocument(new PdfWriter(path));
        PdfPage page = document.AddNewPage();
        var annotations = new PdfArray();
        annotations.Add(new PdfDictionary());
        PdfDictionary pageDictionary = page.GetPdfObject();
        pageDictionary.Put(PdfName.Annots, annotations);
        pageDictionary.Put(PdfName.Tabs, PdfName.S);

        PdfDictionary nonEmptyParagraph = Paragraph();
        nonEmptyParagraph.Put(PdfName.K, new PdfNumber(0));
        var kids = new PdfArray();
        kids.Add(nonEmptyParagraph);
        var structTreeRoot = new PdfDictionary();
        structTreeRoot.Put(PdfName.Type, PdfName.StructTreeRoot);
        structTreeRoot.Put(PdfName.K, kids);
        document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, structTreeRoot);
    }

    private static void CreatePdfWithDirectEmptyParagraphRootKid(string path)
    {
        using var document = new PdfDocument(new PdfWriter(path));
        document.AddNewPage();
        var structTreeRoot = new PdfDictionary();
        structTreeRoot.Put(PdfName.Type, PdfName.StructTreeRoot);
        structTreeRoot.Put(PdfName.K, Paragraph());
        document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, structTreeRoot);
    }

    private static PdfDictionary Paragraph()
    {
        var paragraph = new PdfDictionary();
        paragraph.Put(PdfName.Type, PdfName.StructElem);
        paragraph.Put(PdfName.S, PdfName.P);
        return paragraph;
    }

    private static void DeleteIfExists(string path)
    {
        if (File.Exists(path))
        {
            File.Delete(path);
        }
    }
}
