module Legibyl.Validation.ValidationReportTextFormatter

open System
open System.Text

/// "page 2, x 72–540, y 698–732" (PDF points, origin at the bottom-left of the page).
let FormatRegion (geometry: StructureGeometry) : string =
    $"page {geometry.PageNumber}, x {geometry.Left:F0}–{geometry.Right:F0}, y {geometry.Bottom:F0}–{geometry.Top:F0}"

let private appendFinding (sb: StringBuilder) (violation: RuleViolation) =
    sb.AppendLine($"Rule: {violation.Specification} {violation.RuleId}")
      .AppendLine($"Type: {violation.Severity}")
    |> ignore
    if violation.IsFixable then
        sb.AppendLine($"Fixable: {violation.FixDescription}") |> ignore
    sb.AppendLine($"Message: {violation.Message}") |> ignore
    if not (String.IsNullOrWhiteSpace violation.ErrorMessage) then
        sb.AppendLine($"Error: {violation.ErrorMessage}") |> ignore

    let breadcrumb =
        match violation.Location with
        | null -> "Not available"
        | location -> location.Breadcrumb

    let rawLocation =
        match violation.LocationContext with
        | null -> "Not available"
        | context -> context

    sb.AppendLine($"Location: {breadcrumb}").AppendLine($"Raw location: {rawLocation}") |> ignore
    match violation.Geometry with
    | NonNull geometry -> sb.AppendLine($"Region: {FormatRegion geometry}") |> ignore
    | Null -> ()

let BuildReport (report: ValidationReport) : string =
    let sb = StringBuilder()
    sb.AppendLine($"File: {report.FilePath}")
      .AppendLine($"Profile requested: {report.Profile}")
      .AppendLine($"Flavour validated: {report.FlavourId}")
      .AppendLine($"Compliant: {report.IsCompliant}")
      .AppendLine($"Total checks: {report.TotalAssertions}")
      .AppendLine($"Failed checks: {report.FailedCount}")
      .AppendLine($"Warnings: {report.WarningCount}")
      .AppendLine()
    |> ignore

    report.Findings
    |> Seq.iteri (fun i violation ->
        if i > 0 then
            sb.AppendLine() |> ignore
        sb.AppendLine($"Finding {i + 1}") |> ignore
        appendFinding sb violation)

    sb.ToString()

let BuildFinding (violation: RuleViolation) : string =
    let sb = StringBuilder()
    appendFinding sb violation
    sb.ToString()
