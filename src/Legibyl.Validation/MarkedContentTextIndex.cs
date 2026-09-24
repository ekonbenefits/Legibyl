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
internal static class MarkedContentTextIndex
{
    public static Dictionary<(int Page, int Mcid), string> Build(string pdfPath)
    {
        var index = new Dictionary<(int, int), string>();
        using var document = PdfDocument.Open(pdfPath);
        foreach (Page page in document.GetPages())
        {
            IndexMarkedContent(page.Number, page.GetMarkedContents(), index);
        }
        return index;
    }

    private static void IndexMarkedContent(
        int pageNumber, IReadOnlyList<MarkedContentElement> elements, Dictionary<(int, int), string> index)
    {
        foreach (var element in elements)
        {
            string text = ConcatenateLetters(element.Letters);
            if (!string.IsNullOrEmpty(text))
            {
                index[(pageNumber, element.MarkedContentIdentifier)] = text;
            }

            if (element.Children.Count > 0)
            {
                IndexMarkedContent(pageNumber, element.Children, index);
            }
        }
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
