using System.Text.RegularExpressions;

namespace VeraPdfTool.Validation;

/// <summary>
/// Parses veraPDF's <c>locationContext</c> strings — a slash-separated walk of its internal
/// object model, e.g. <c>root/document[0]/pages[1](7 0 obj PDPage)/annots[0](9 0 obj PDLinkAnnot)</c>
/// — into a page number and a plain-English breadcrumb, so users don't need to know veraPDF's
/// object model to figure out where in the document a failure is.
/// </summary>
public static partial class PdfLocationParser
{
    // Each path segment looks like: name[index](objNum genNum obj Type...)
    // The parenthesised part and/or the [index] are optional.
    [GeneratedRegex(@"^(?<name>[A-Za-z]+)(\[(?<idx>\d+)\])?(\((?<num>\d+)\s+\d+\s+obj\s+(?<type>.+)\))?$")]
    private static partial Regex SegmentPattern();

    private static readonly Dictionary<string, string> TagFriendlyNames = new(StringComparer.OrdinalIgnoreCase)
    {
        ["Document"] = "document root",
        ["Table"] = "table",
        ["TR"] = "table row (TR)",
        ["TH"] = "table header cell (TH)",
        ["TD"] = "table cell (TD)",
        ["Figure"] = "figure",
        ["Link"] = "link",
        ["L"] = "list",
        ["LI"] = "list item",
        ["Lbl"] = "list item label",
        ["LBody"] = "list item body",
        ["P"] = "paragraph",
        ["Span"] = "text span",
        ["Div"] = "division",
        ["Sect"] = "section",
        ["Art"] = "article",
        ["H1"] = "heading (H1)",
        ["H2"] = "heading (H2)",
        ["H3"] = "heading (H3)",
        ["H4"] = "heading (H4)",
        ["H5"] = "heading (H5)",
        ["H6"] = "heading (H6)",
        ["Caption"] = "caption",
        ["THead"] = "table header group",
        ["TBody"] = "table body group",
        ["TFoot"] = "table footer group",
    };

    private static readonly Dictionary<string, string> TypeOverrides = new(StringComparer.OrdinalIgnoreCase)
    {
        ["PDPage"] = "page",
        ["PDLinkAnnot"] = "link annotation",
        ["PDStructTreeRoot"] = "structure tree",
        ["PDSemanticContentStream"] = "content stream",
        ["PDAnnot"] = "annotation",
        ["PDWidgetAnnot"] = "form field annotation",
    };

    private static readonly Dictionary<string, string> NameOverrides = new(StringComparer.OrdinalIgnoreCase)
    {
        ["document"] = "document",
        ["annots"] = "annotation",
        ["contentStream"] = "content stream",
        ["content"] = "content",
        ["contentItem"] = "content item",
        ["structtreeroot"] = "structure tree",
        ["k"] = "child element",
    };

    /// <summary>
    /// Parses a veraPDF locationContext string. Returns null if <paramref name="locationContext"/>
    /// is null/empty or doesn't look like a recognisable veraPDF path (falls back gracefully
    /// rather than throwing, since this is a best-effort readability aid).
    /// </summary>
    public static PdfLocation? Parse(string? locationContext)
    {
        if (string.IsNullOrWhiteSpace(locationContext))
        {
            return null;
        }

        string[] segments = locationContext.Split('/', StringSplitOptions.RemoveEmptyEntries);
        int? pageNumber = null;
        string? objectType = null;
        int? objectNumber = null;
        var breadcrumb = new List<string>();

        foreach (string segment in segments)
        {
            if (string.Equals(segment, "root", StringComparison.OrdinalIgnoreCase))
            {
                continue;
            }

            Match match = SegmentPattern().Match(segment);
            if (!match.Success)
            {
                breadcrumb.Add(segment);
                continue;
            }

            string name = match.Groups["name"].Value;
            int? index = match.Groups["idx"].Success ? int.Parse(match.Groups["idx"].Value) : null;
            int? number = match.Groups["num"].Success ? int.Parse(match.Groups["num"].Value) : null;
            string? type = match.Groups["type"].Success ? match.Groups["type"].Value : null;

            if (string.Equals(name, "pages", StringComparison.OrdinalIgnoreCase) && index.HasValue)
            {
                pageNumber = index.Value + 1; // veraPDF's page index is 0-based.
            }

            if (number.HasValue)
            {
                objectNumber = number.Value;
                objectType = type;
            }

            breadcrumb.Add(DescribeSegment(name, index, type));
        }

        if (breadcrumb.Count == 0)
        {
            breadcrumb.Add(locationContext);
        }

        return new PdfLocation(pageNumber, objectType, objectNumber, string.Join(" → ", breadcrumb));
    }

    private static string DescribeSegment(string name, int? index, string? type)
    {
        if (string.Equals(name, "pages", StringComparison.OrdinalIgnoreCase) && index.HasValue)
        {
            return $"Page {index + 1}";
        }

        if (type is not null)
        {
            return Capitalize(HumanizeType(type));
        }

        return NameOverrides.TryGetValue(name, out string? overrideName)
            ? Capitalize(overrideName)
            : Capitalize(name);
    }

    private static string HumanizeType(string type)
    {
        if (TypeOverrides.TryGetValue(type, out string? overrideValue))
        {
            return overrideValue;
        }

        // Structure element types look like "SETable Table" / "SETD TD" — the second
        // word is the actual PDF structure tag, which is far more meaningful to a user
        // than the internal "SE..." class name.
        string[] parts = type.Split(' ', 2);
        if (parts.Length == 2 && parts[0].StartsWith("SE", StringComparison.Ordinal))
        {
            string tag = parts[1];
            return TagFriendlyNames.TryGetValue(tag, out string? friendly) ? friendly : $"{tag} element";
        }

        string stripped = type.StartsWith("PD", StringComparison.Ordinal) ? type[2..] : type;
        return SplitCamelCase(stripped).ToLowerInvariant();
    }

    private static string SplitCamelCase(string value) =>
        CamelCasePattern().Replace(value, " $1");

    [GeneratedRegex("(?<=[a-z0-9])(?=[A-Z])")]
    private static partial Regex CamelCasePattern();

    private static string Capitalize(string value) =>
        value.Length == 0 ? value : char.ToUpperInvariant(value[0]) + value[1..];
}
