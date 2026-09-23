using VeraPdfTool.Validation;

namespace VeraPdfTool.Gui;

/// <summary>An entry for the profile selector ComboBox.</summary>
public sealed record ProfileItem(string DisplayName, PdfProfile Profile)
{
    public override string ToString() => DisplayName;

    public static readonly IReadOnlyList<ProfileItem> All =
    [
        new("PDF/UA-1", PdfProfile.PdfUa1),
        new("PDF/A-1b", PdfProfile.PdfA1B),
        new("PDF/A-2b", PdfProfile.PdfA2B),
        new("PDF/A-2u", PdfProfile.PdfA2U),
        new("PDF/A-3b", PdfProfile.PdfA3B),
        new("PDF/A-4", PdfProfile.PdfA4),
    ];
}
