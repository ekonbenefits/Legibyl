using System.Globalization;
using Avalonia.Data.Converters;
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
        if (node.FormFieldDescription is not null) parts.Add($"tooltip: \"{node.FormFieldDescription}\"");
        if (node.FormFieldName is not null) parts.Add($"field: {node.FormFieldName}");
        if (node.FormFieldType is not null) parts.Add($"type: {node.FormFieldType}");
        if (node.LinkUri is not null) parts.Add($"href: {node.LinkUri}");
        if (node.LinkDestinationDescription is not null) parts.Add($"dest: {node.LinkDestinationDescription}");
        if (node.ContentLeafCount > 0)
        {
            parts.Add($"({node.ContentLeafCount} content item{(node.ContentLeafCount == 1 ? "" : "s")})");
        }

        string summary = string.Join("  ", parts);
        if (node.Content is not null)
        {
            const int maxPreview = 60;
            string preview = node.Content.Length > maxPreview ? node.Content[..maxPreview] + "…" : node.Content;
            summary += $"  \u2014 \"{preview}\"";
        }

        return summary;
    }

    public object ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
        => throw new NotSupportedException();
}
