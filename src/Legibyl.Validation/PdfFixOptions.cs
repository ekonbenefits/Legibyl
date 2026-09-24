namespace Legibyl.Validation;

[Flags]
public enum PdfFixOptions
{
    None = 0,
    Metadata = 1,
    AnnotationTabs = 2,
    RemoveEmptyParagraphs = 4,
    All = Metadata | AnnotationTabs | RemoveEmptyParagraphs,
}
