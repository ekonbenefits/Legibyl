namespace VeraPdfTool.Validation;

/// <summary>
/// The PDF/A or PDF/UA validation flavour to check a document against.
/// Mirrors a subset of org.verapdf.pdfa.flavours.PDFAFlavour.
/// </summary>
public enum PdfProfile
{
    /// <summary>PDF/UA-1 (ISO 14289-1) — the primary supported profile.</summary>
    PdfUa1,

    /// <summary>PDF/A-1b (basic conformance).</summary>
    PdfA1B,

    /// <summary>PDF/A-2b (basic conformance).</summary>
    PdfA2B,

    /// <summary>PDF/A-2u (Unicode conformance).</summary>
    PdfA2U,

    /// <summary>PDF/A-3b (basic conformance, embedded files allowed).</summary>
    PdfA3B,

    /// <summary>PDF/A-4.</summary>
    PdfA4,
}
