using System.Text;

namespace Legibyl.Validation;

public static class ValidationReportTextFormatter
{
    public static string BuildReport(ValidationReport report)
    {
        var sb = new StringBuilder();
        sb.AppendLine($"File: {report.FilePath}");
        sb.AppendLine($"Profile requested: {report.Profile}");
        sb.AppendLine($"Flavour validated: {report.FlavourId}");
        sb.AppendLine($"Compliant: {report.IsCompliant}");
        sb.AppendLine($"Total checks: {report.TotalAssertions}");
        sb.AppendLine($"Failed checks: {report.FailedCount}");
        sb.AppendLine($"Warnings: {report.WarningCount}");
        sb.AppendLine();

        int findingNumber = 1;
        foreach (RuleViolation violation in report.Findings)
        {
            if (findingNumber > 1)
            {
                sb.AppendLine();
            }

            sb.AppendLine($"Finding {findingNumber++}");
            AppendFinding(sb, violation);
        }

        return sb.ToString();
    }

    public static string BuildFinding(RuleViolation violation)
    {
        var sb = new StringBuilder();
        AppendFinding(sb, violation);
        return sb.ToString();
    }

    private static void AppendFinding(StringBuilder sb, RuleViolation violation)
    {
        sb.AppendLine($"Rule: {violation.Specification} {violation.RuleId}");
        sb.AppendLine($"Type: {violation.Severity}");
        if (violation.IsFixable)
        {
            sb.AppendLine($"Fixable: {violation.FixOptions}");
        }
        sb.AppendLine($"Message: {violation.Message}");
        if (!string.IsNullOrWhiteSpace(violation.ErrorMessage))
        {
            sb.AppendLine($"Error: {violation.ErrorMessage}");
        }

        sb.AppendLine($"Location: {violation.Location?.Breadcrumb ?? "Not available"}");
        sb.AppendLine($"Raw location: {violation.LocationContext ?? "Not available"}");
    }
}
