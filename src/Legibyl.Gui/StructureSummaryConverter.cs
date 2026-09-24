using System.Globalization;
using System.Windows.Data;
using Legibyl.Validation;

namespace Legibyl.Gui;

/// <summary>Formats a <see cref="StructureNode"/> into a single-line summary for the structure TreeView.</summary>
public sealed class StructureSummaryConverter : IValueConverter
{
    public object Convert(object? value, Type targetType, object? parameter, CultureInfo culture)
    {
        if (value is not StructureNode node)
        {
            return string.Empty;
        }

        var parts = new List<string> { $"<{node.Tag}>" };
        if (node.Role is not null) parts.Add($"role={node.Role}");
        if (node.PageNumber is not null) parts.Add($"p.{node.PageNumber}");
        if (node.Language is not null) parts.Add($"[{node.Language}]");
        if (node.AlternateDescription is not null) parts.Add($"alt: \"{node.AlternateDescription}\"");
        if (node.ActualText is not null) parts.Add($"actualText: \"{node.ActualText}\"");
        if (node.ContentLeafCount > 0)
        {
            parts.Add($"({node.ContentLeafCount} content item{(node.ContentLeafCount == 1 ? "" : "s")})");
        }

        return string.Join("  ", parts);
    }

    public object ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
        => throw new NotSupportedException();
}
