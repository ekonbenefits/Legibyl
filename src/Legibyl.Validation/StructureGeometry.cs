namespace Legibyl.Validation;

/// <summary>
/// Best-effort page geometry for a semantic structure node, in PDF user-space
/// coordinates (origin at the bottom-left of the page).
/// </summary>
public sealed record StructureGeometry(
    int PageNumber,
    double Left,
    double Bottom,
    double Right,
    double Top);
