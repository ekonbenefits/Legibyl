using VeraPdfTool.Validation;

namespace VeraPdfTool.Cli;

/// <summary>Maps user-supplied profile strings (case-insensitive, with common aliases) to <see cref="PdfProfile"/>.</summary>
internal static class ProfileNames
{
    private static readonly Dictionary<string, PdfProfile> Aliases = new(StringComparer.OrdinalIgnoreCase)
    {
        ["pdfua1"] = PdfProfile.PdfUa1,
        ["pdf/ua-1"] = PdfProfile.PdfUa1,
        ["ua1"] = PdfProfile.PdfUa1,
        ["ua"] = PdfProfile.PdfUa1,

        ["pdfa1b"] = PdfProfile.PdfA1B,
        ["pdf/a-1b"] = PdfProfile.PdfA1B,
        ["1b"] = PdfProfile.PdfA1B,

        ["pdfa2b"] = PdfProfile.PdfA2B,
        ["pdf/a-2b"] = PdfProfile.PdfA2B,
        ["2b"] = PdfProfile.PdfA2B,

        ["pdfa2u"] = PdfProfile.PdfA2U,
        ["pdf/a-2u"] = PdfProfile.PdfA2U,
        ["2u"] = PdfProfile.PdfA2U,

        ["pdfa3b"] = PdfProfile.PdfA3B,
        ["pdf/a-3b"] = PdfProfile.PdfA3B,
        ["3b"] = PdfProfile.PdfA3B,

        ["pdfa4"] = PdfProfile.PdfA4,
        ["pdf/a-4"] = PdfProfile.PdfA4,
        ["4"] = PdfProfile.PdfA4,
    };

    /// <summary>The default profile used when none is specified.</summary>
    public const string DefaultName = "pdfua1";

    public static bool TryParse(string name, out PdfProfile profile) =>
        Aliases.TryGetValue(name.Trim(), out profile);

    /// <summary>All recognised profile names, for --list-profiles / help output.</summary>
    public static IEnumerable<(string CanonicalName, PdfProfile Profile)> CanonicalNames { get; } =
    [
        ("pdfua1", PdfProfile.PdfUa1),
        ("pdfa1b", PdfProfile.PdfA1B),
        ("pdfa2b", PdfProfile.PdfA2B),
        ("pdfa2u", PdfProfile.PdfA2U),
        ("pdfa3b", PdfProfile.PdfA3B),
        ("pdfa4", PdfProfile.PdfA4),
    ];
}
