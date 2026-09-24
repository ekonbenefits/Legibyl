namespace Legibyl.Validation;

/// <summary>
/// Validates PDF documents against PDF/A or PDF/UA profiles. Implementations
/// wrap the veraPDF Java validation engine (loaded via IKVM); callers never
/// need to reference IKVM or Java types directly.
/// </summary>
public interface IPdfValidator
{
    /// <summary>
    /// Validates the PDF at <paramref name="pdfPath"/> against <paramref name="profile"/>.
    /// </summary>
    /// <exception cref="VeraPdfValidationException">
    /// Thrown if the file cannot be parsed (corrupt/invalid PDF, encrypted, IO error, etc).
    /// </exception>
    ValidationReport Validate(string pdfPath, PdfProfile profile);
}
