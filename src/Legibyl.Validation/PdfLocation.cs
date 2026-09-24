namespace Legibyl.Validation;

/// <summary>
/// A human-friendly interpretation of a veraPDF <c>locationContext</c> string
/// (e.g. <c>root/document[0]/pages[1](7 0 obj PDPage)/annots[0](9 0 obj PDLinkAnnot)</c>),
/// which otherwise requires knowledge of veraPDF's internal object model to read.
/// </summary>
/// <param name="PageNumber">1-based page number, if the location is anchored to a page.</param>
/// <param name="ObjectType">The raw veraPDF/PDF object type of the deepest object on the path (e.g. "PDLinkAnnot").</param>
/// <param name="ObjectNumber">The PDF indirect object number of the deepest object on the path, if any.</param>
/// <param name="AnnotationIndex">
/// If the deepest object is an annotation, its 0-based position within the page's /Annots array
/// (i.e. the index from an "annots[N]" path segment). Used to look up its /Rect for region highlighting.
/// </param>
/// <param name="Breadcrumb">A readable "Document → Page 2 → Link annotation" style path.</param>
public sealed record PdfLocation(
    int? PageNumber,
    string? ObjectType,
    int? ObjectNumber,
    int? AnnotationIndex,
    string Breadcrumb)
{
    public override string ToString() => PageNumber is { } page
        ? $"Page {page} — {Breadcrumb}"
        : Breadcrumb;
}
