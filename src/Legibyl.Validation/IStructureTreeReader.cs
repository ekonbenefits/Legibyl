namespace Legibyl.Validation;

/// <summary>
/// Reads a tagged PDF's logical structure tree — the semantic hierarchy (headings,
/// paragraphs, lists, tables, links, etc.) that screen readers use to present the
/// document. Implementations wrap veraPDF's Java PD model (loaded via IKVM).
/// </summary>
public interface IStructureTreeReader
{
    /// <summary>
    /// Reads the structure tree of the PDF at <paramref name="pdfPath"/>, rooted at
    /// its StructTreeRoot, or returns null if the document has no StructTreeRoot at
    /// all (i.e. it is completely untagged).
    /// </summary>
    /// <exception cref="VeraPdfValidationException">
    /// Thrown if the file cannot be parsed (corrupt/invalid PDF, encrypted, IO error, etc).
    /// </exception>
    StructureNode? ReadStructureTree(string pdfPath);
}
