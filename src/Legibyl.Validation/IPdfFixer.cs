namespace Legibyl.Validation;

/// <summary>
/// Repairs selected PDF/A or PDF/UA issues. Implementations keep veraPDF/IKVM
/// and PDF writer details behind this façade.
/// </summary>
public interface IPdfFixer
{
    /// <summary>
    /// Validates the PDF at <paramref name="pdfPath"/> against <paramref name="profile"/>,
    /// then attempts the selected fixes, writing a repaired copy to
    /// <paramref name="outputPath"/> only if at least one fix changed the file.
    /// </summary>
    /// <exception cref="VeraPdfValidationException">
    /// Thrown if the file cannot be parsed (corrupt/invalid PDF, encrypted, IO error, etc).
    /// </exception>
    FixResult Fix(string pdfPath, PdfProfile profile, string outputPath, PdfFixOptions options = PdfFixOptions.Metadata);
}
