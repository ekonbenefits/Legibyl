namespace Legibyl.Validation;

/// <summary>
/// One node of a tagged PDF's logical structure tree (the Catalog's StructTreeRoot) —
/// the same semantic hierarchy (headings, paragraphs, lists, tables, links, etc.) and
/// reading order that assistive technology (screen readers) uses to present the
/// document to a user. Built by walking veraPDF's own PD model, so it reflects
/// exactly what the validator itself sees.
/// </summary>
/// <param name="Tag">
/// The structure element's type, resolved to its standard/base tag name where possible
/// (e.g. "H1", "P", "L", "Table", "TH"). This is the value that determines the
/// element's semantic role to a screen reader.
/// </param>
/// <param name="Role">
/// If this element uses a custom (non-standard) tag that is role-mapped to a
/// different standard tag within the same namespace, the mapped-to tag name;
/// otherwise null (no distinct mapping worth reporting).
/// </param>
/// <param name="AlternateDescription">The element's Alt text (e.g. image/figure/link alt text), if any.</param>
/// <param name="ActualText">The element's ActualText (a plain-text substitute for its content), if any.</param>
/// <param name="Content">
/// The actual visible text drawn under this element's own directly-owned marked
/// content (extracted from the page's text glyphs, matched to this element by
/// page + MCID) — the "what does a screen reader actually read here" text. Null if
/// this element owns no text-bearing marked content directly (e.g. it's a purely
/// structural container whose text lives entirely in its children, or it owns only
/// non-text content like a raw image). Does not include descendant elements' text,
/// to avoid duplicating text that already appears under a child node.
/// </param>
/// <param name="FormFieldName">For /Form structure elements, the referenced widget annotation's field name (/T), if any.</param>
/// <param name="FormFieldDescription">For /Form structure elements, the referenced widget annotation's tooltip/user description (/TU), if any.</param>
/// <param name="FormFieldType">For /Form structure elements, the referenced widget annotation's field type (/FT), e.g. "Tx" or "Btn".</param>
/// <param name="FormFieldValue">For /Form structure elements, the referenced widget annotation's current value (/V), if any.</param>
/// <param name="LinkUri">For /Link structure elements, the external URI target, if any.</param>
/// <param name="LinkDestinationPageNumber">For /Link structure elements, the target page number for an internal destination, if resolvable.</param>
/// <param name="LinkDestinationDescription">For /Link structure elements, a human-readable internal destination summary, if any.</param>
/// <param name="Geometry">
/// Best-effort same-page geometry covered by this node's directly-owned content and
/// child nodes, if determinable. Null when the node has no geometric content or
/// spans multiple pages.
/// </param>
/// <param name="Language">The element's Lang tag (e.g. "en-US"), if explicitly set on this element.</param>
/// <param name="PageNumber">
/// The 1-based page this element's content appears on, if determinable. Null for
/// purely structural container elements that don't directly own any page content.
/// </param>
/// <param name="ContentLeafCount">
/// Number of marked-content/annotation references directly under this element (i.e.
/// actual tagged page content it owns), not counting nested structure elements.
/// </param>
/// <param name="Children">Child structure elements, in document (reading) order.</param>
public sealed record StructureNode(
    string Tag,
    string? Role,
    string? AlternateDescription,
    string? ActualText,
    string? Content,
    string? FormFieldName,
    string? FormFieldDescription,
    string? FormFieldType,
    string? FormFieldValue,
    string? LinkUri,
    int? LinkDestinationPageNumber,
    string? LinkDestinationDescription,
    StructureGeometry? Geometry,
    string? Language,
    int? PageNumber,
    int ContentLeafCount,
    IReadOnlyList<StructureNode> Children);
