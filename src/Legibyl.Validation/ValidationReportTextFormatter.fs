module Legibyl.Validation.ValidationReportTextFormatter

open System
open System.Text

let private appendFinding (sb: StringBuilder) (violation: RuleViolation) =
    sb.AppendLine($"Rule: {violation.Specification} {violation.RuleId}")
      .AppendLine($"Type: {violation.Severity}")
    |> ignore
    if violation.IsFixable then
        sb.AppendLine($"Fixable: {violation.FixOptions}") |> ignore
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
