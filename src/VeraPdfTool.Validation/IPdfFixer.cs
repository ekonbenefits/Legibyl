namespace VeraPdfTool.Validation;

/// <summary>
/// Repairs PDF/A or PDF/UA XMP metadata. Implementations wrap veraPDF's Java
/// metadata-fixer (loaded via IKVM); callers never need to reference IKVM or
/// Java types directly.
/// </summary>
public interface IPdfFixer
{
    /// <summary>
    /// Validates the PDF at <paramref name="pdfPath"/> against <paramref name="profile"/>,
    /// then attempts to repair its XMP metadata, writing a repaired copy to
    /// <paramref name="outputPath"/>. Only metadata is repaired (see <see cref="FixResult"/>
    /// remarks) — structural/tagging issues are left untouched.
    /// </summary>
    /// <exception cref="VeraPdfValidationException">
    /// Thrown if the file cannot be parsed (corrupt/invalid PDF, encrypted, IO error, etc).
    /// </exception>
    FixResult Fix(string pdfPath, PdfProfile profile, string outputPath);
}
