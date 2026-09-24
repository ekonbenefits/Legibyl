using UglyToad.PdfPig;
using UglyToad.PdfPig.Content;

namespace Legibyl.Validation;

/// <summary>
/// Builds a (page number, MCID) -> visible text index for a PDF using PdfPig, so that
/// veraPDF's structure tree walk (<see cref="VeraPdfValidator.ReadStructureTree"/>) can
/// resolve each structure element's directly-owned marked content back to the actual
/// text a screen reader would read there. This is intentionally a separate, independent
/// parse of the file (PdfPig, not veraPDF/IKVM) — text extraction and PDF/UA structural
/// validation are unrelated concerns, and PdfPig already exposes marked-content/MCID
/// data directly (<see cref="Page.GetMarkedContents"/>) that veraPDF's PD model does not.
/// </summary>
internal sealed record MarkedContentInfo(string Text, StructureGeometry? Geometry);

internal static class MarkedContentTextIndex
{
    public static Dictionary<(int Page, int Mcid), MarkedContentInfo> Build(string pdfPath)
    {
        var index = new Dictionary<(int, int), MarkedContentInfo>();
        using var document = PdfDocument.Open(pdfPath);
        foreach (Page page in document.GetPages())
        {
            IndexMarkedContent(page.Number, page.GetMarkedContents(), index);
        }
        return index;
    }

    private static void IndexMarkedContent(
        int pageNumber, IReadOnlyList<MarkedContentElement> elements, Dictionary<(int, int), MarkedContentInfo> index)
    {
        foreach (var element in elements)
        {
            string text = ConcatenateLetters(element.Letters);
            StructureGeometry? geometry = GeometryFromMarkedContent(pageNumber, element);
            if (!string.IsNullOrEmpty(text) || geometry is not null)
            {
                index[(pageNumber, element.MarkedContentIdentifier)] = new MarkedContentInfo(text, geometry);
            }

            if (element.Children.Count > 0)
            {
                IndexMarkedContent(pageNumber, element.Children, index);
            }
        }
    }

    private static StructureGeometry? GeometryFromMarkedContent(int pageNumber, MarkedContentElement element)
    {
        List<StructureGeometry> geometries = [];

        if (GeometryFromLetters(pageNumber, element.Letters) is { } textGeometry)
        {
            geometries.Add(textGeometry);
        }

        foreach (var image in element.Images)
        {
            // PdfPig 0.1.16 marks IPdfImage.Bounds obsolete in favor of BoundingBox, but
            // the public IPdfImage interface in this version only exposes Bounds. Keep the
            // warning contained here until the package is upgraded.
#pragma warning disable CS0618
            geometries.Add(new StructureGeometry(
                pageNumber,
                image.Bounds.Left,
                image.Bounds.Bottom,
                image.Bounds.Right,
                image.Bounds.Top));
#pragma warning restore CS0618
        }

        foreach (var path in element.Paths)
        {
            var bounds = path.GetBoundingRectangle();
            if (bounds.HasValue)
            {
                geometries.Add(new StructureGeometry(
                    pageNumber,
                    bounds.Value.Left,
                    bounds.Value.Bottom,
                    bounds.Value.Right,
                    bounds.Value.Top));
            }
        }

        return UnionGeometry(geometries);
    }

    private static StructureGeometry? GeometryFromLetters(int pageNumber, IReadOnlyList<Letter> letters)
    {
        if (letters.Count == 0)
        {
            return null;
        }

        double left = letters.Min(l => l.BoundingBox.Left);
        double right = letters.Max(l => l.BoundingBox.Right);
        double bottom = letters.Min(l => l.BoundingBox.Bottom);
        double top = letters.Max(l => l.BoundingBox.Top);
        return new StructureGeometry(pageNumber, left, bottom, right, top);
    }

    private static StructureGeometry? UnionGeometry(IReadOnlyList<StructureGeometry> geometries)
    {
        if (geometries.Count == 0)
        {
            return null;
        }

        int pageNumber = geometries[0].PageNumber;
        return new StructureGeometry(
            pageNumber,
            geometries.Min(geometry => geometry.Left),
            geometries.Min(geometry => geometry.Bottom),
            geometries.Max(geometry => geometry.Right),
            geometries.Max(geometry => geometry.Top));
    }

    /// <summary>
    /// Joins a marked content sequence's glyphs into readable text, in the order they
    /// were painted. Inserts a space wherever there's a noticeable horizontal gap
    /// between consecutive glyphs that isn't already whitespace — many PDF writers
    /// position words apart via spacing operators rather than an explicit space glyph,
    /// so naive concatenation alone would run words together.
    /// </summary>
    private static string ConcatenateLetters(IReadOnlyList<Letter> letters)
    {
        if (letters.Count == 0)
        {
            return string.Empty;
        }

        var ordered = letters.OrderBy(l => l.TextSequence).ToList();
        var sb = new System.Text.StringBuilder();
        Letter? previous = null;
        foreach (var letter in ordered)
        {
            if (previous is not null && letter.Value.Length > 0 && !char.IsWhiteSpace(letter.Value[0]))
            {
                bool previousEndsWithSpace = previous.Value.Length > 0 && char.IsWhiteSpace(previous.Value[^1]);
                double gap = letter.StartBaseLine.X - previous.EndBaseLine.X;
                if (!previousEndsWithSpace && gap > previous.Width * 0.15)
                {
                    sb.Append(' ');
                }
            }
            sb.Append(letter.Value);
            previous = letter;
        }
        return sb.ToString();
    }
}
