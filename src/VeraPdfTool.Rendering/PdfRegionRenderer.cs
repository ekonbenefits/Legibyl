using SkiaSharp;
using UglyToad.PdfPig;
using UglyToad.PdfPig.Annotations;
using UglyToad.PdfPig.Content;
using UglyToad.PdfPig.Core;
using UglyToad.PdfPig.Rendering.Skia;
using VeraPdfTool.Validation;

namespace VeraPdfTool.Rendering;

/// <summary>
/// Result of rendering a page image for a rule violation, with a best-effort highlight box
/// drawn around the failing object when its exact region on the page could be determined.
/// </summary>
/// <param name="PngBytes">The rendered page image, PNG-encoded.</param>
/// <param name="PageNumber">The 1-based page number that was rendered.</param>
/// <param name="HasPreciseRegion">
/// True if a highlight box was drawn around the specific failing object. False means the full
/// page is shown for context only — most structural/tagging failures (e.g. an untagged
/// paragraph, a table cell missing scope) aren't attached to page geometry in veraPDF's model,
/// only annotations (links, form widgets, etc.) currently expose a usable rectangle.
/// </param>
/// <param name="Note">Human-readable explanation of what was (or wasn't) highlighted, for display alongside the image.</param>
public sealed record HighlightImageResult(byte[] PngBytes, int PageNumber, bool HasPreciseRegion, string Note);

public interface IPdfRegionRenderer
{
    /// <summary>
    /// Renders the page containing <paramref name="location"/> as a PNG, highlighting the
    /// failing object's region when it can be determined. Throws <see cref="InvalidOperationException"/>
    /// if <paramref name="location"/> isn't anchored to a page.
    /// </summary>
    HighlightImageResult RenderPageWithHighlight(string pdfPath, PdfLocation location, float scale = 2.0f);
}

public sealed class PdfRegionRenderer : IPdfRegionRenderer
{
    private static readonly SKColor HighlightColor = new(0xE8, 0x1B, 0x23);

    public HighlightImageResult RenderPageWithHighlight(string pdfPath, PdfLocation location, float scale = 2.0f)
    {
        if (location.PageNumber is not { } pageNumber)
        {
            throw new InvalidOperationException("This failure isn't anchored to a specific page, so no page image can be rendered.");
        }

        using PdfDocument document = PdfDocument.Open(pdfPath, SkiaRenderingParsingOptions.Instance);
        document.AddSkiaPageFactory();

        if (pageNumber < 1 || pageNumber > document.NumberOfPages)
        {
            throw new InvalidOperationException($"Page {pageNumber} is out of range (document has {document.NumberOfPages} pages).");
        }

        using SKBitmap bitmap = document.GetPageAsSKBitmap(pageNumber, scale, SKColors.White);

        (PdfRectangle Rect, string Note)? region = TryFindAnnotationRect(document, pageNumber, location);

        bool hasPreciseRegion = false;
        string note;
        if (region is { } found)
        {
            DrawHighlight(bitmap, found.Rect, scale);
            hasPreciseRegion = true;
            note = found.Note;
        }
        else
        {
            note = "Exact region unavailable for this failure type (only annotations like links and form " +
                   "fields currently expose page geometry) — showing the full page for context.";
        }

        using SKImage image = SKImage.FromBitmap(bitmap);
        using SKData encoded = image.Encode(SKEncodedImageFormat.Png, 100);
        return new HighlightImageResult(encoded.ToArray(), pageNumber, hasPreciseRegion, note);
    }

    private static (PdfRectangle, string)? TryFindAnnotationRect(PdfDocument document, int pageNumber, PdfLocation location)
    {
        if (location.AnnotationIndex is not { } annotationIndex)
        {
            return null;
        }

        Page page = document.GetPage(pageNumber);
        List<Annotation> annotations = page.GetAnnotations().ToList();
        if (annotationIndex < 0 || annotationIndex >= annotations.Count)
        {
            return null;
        }

        Annotation annotation = annotations[annotationIndex];

        // Sanity-check the annotation type against veraPDF's reported type (e.g. "PDLinkAnnot" -> Link)
        // so a page whose annotation array has shifted (e.g. a slightly different PDF revision) doesn't
        // silently highlight the wrong region.
        string? expectedTypeName = ExpectedAnnotationTypeName(location.ObjectType);
        if (expectedTypeName is not null &&
            !string.Equals(expectedTypeName, annotation.Type.ToString(), StringComparison.OrdinalIgnoreCase))
        {
            return null;
        }

        return (annotation.Rectangle, $"Highlighted the {annotation.Type.ToString().ToLowerInvariant()} annotation on this page.");
    }

    private static string? ExpectedAnnotationTypeName(string? objectType)
    {
        if (objectType is null || !objectType.StartsWith("PD", StringComparison.Ordinal) ||
            !objectType.EndsWith("Annot", StringComparison.Ordinal))
        {
            return null;
        }

        return objectType[2..^5]; // strip "PD" prefix and "Annot" suffix, e.g. "PDLinkAnnot" -> "Link"
    }

    private static void DrawHighlight(SKBitmap bitmap, PdfRectangle rect, float scale)
    {
        using var canvas = new SKCanvas(bitmap);

        double left = Math.Min(rect.Left, rect.Right);
        double right = Math.Max(rect.Left, rect.Right);
        double bottom = Math.Min(rect.Top, rect.Bottom);
        double top = Math.Max(rect.Top, rect.Bottom);

        // PdfPig's rectangle is bottom-left-origin, y-up; convert to the bitmap's top-left-origin, y-down space.
        const float padding = 4f;
        var box = new SKRect(
            (float)(left * scale) - padding,
            bitmap.Height - (float)(top * scale) - padding,
            (float)(right * scale) + padding,
            bitmap.Height - (float)(bottom * scale) + padding);

        using var fill = new SKPaint
        {
            Color = HighlightColor.WithAlpha(40),
            Style = SKPaintStyle.Fill,
            IsAntialias = true,
        };
        using var stroke = new SKPaint
        {
            Color = HighlightColor,
            Style = SKPaintStyle.Stroke,
            StrokeWidth = 3f,
            IsAntialias = true,
        };

        canvas.DrawRect(box, fill);
        canvas.DrawRect(box, stroke);
    }
}
