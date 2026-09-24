using System.Text.Json;
using System.Text.Json.Serialization;
using Legibyl.Cli;
using Legibyl.Validation;

const string helpText = """
legibyl - PDF/UA & PDF/A validator (CLI) powered by veraPDF via IKVM.

Designed for scripting and AI-agent use: default output is machine-readable
JSON on stdout, and the process exit code tells you the outcome without
needing to parse output.

USAGE:
  legibyl <file.pdf> [<file2.pdf> ...] [options]
  legibyl --list-profiles
  legibyl --help

OPTIONS:
  --profile <name>     Profile to validate against (default: pdfua1).
                        See --list-profiles for all accepted names.
  --format <json|text> Output format (default: json).
  --output <path>      Write output to a file instead of stdout.
  --fix                 Also attempt to repair XMP metadata (missing metadata
                        stream, missing PDF/A|PDF/UA identifier, false
                        conformance claims, Info-dictionary sync) for each
                        file, writing a repaired copy next to the original as
                        "<name>.fixed<ext>". NOTE: this only repairs metadata —
                        it does not fix structural/tagging issues (untagged
                        content, missing alt-text, table header scope, etc).
  --fix-output <path>  Use with --fix and exactly one input file to write the
                        repaired copy to an exact path instead of the
                        auto-derived name.
  --structure          Include each file's tagged logical structure tree (the
                        semantic heading/paragraph/list/table/link hierarchy
                        that screen readers use) in the output. In --format
                        text mode this prints an indented outline; in JSON
                        mode (default) it's the full tree, ideal for feeding
                        to an AI agent or other tooling.
  --list-profiles      Print supported profile names and exit 0.
  --version            Print tool + vendored veraPDF engine version and exit 0.
  --help, -h           Print this help and exit 0.

EXIT CODES:
  0   all files validated and are compliant.
  1   all files validated successfully, but at least one is non-compliant.
  2   at least one file could not be validated (missing/corrupt/encrypted).
  64  invalid command-line usage.

EXAMPLES:
  legibyl document.pdf
  legibyl document.pdf --profile pdfa1b --format text
  legibyl *.pdf --output results.json
  legibyl document.pdf --fix
  legibyl document.pdf --fix --fix-output document-repaired.pdf
  legibyl document.pdf --structure --output structure.json
""";

var files = new List<string>();
string profileName = ProfileNames.DefaultName;
string format = "json";
string? outputPath = null;
bool doFix = false;
string? fixOutputPath = null;
bool includeStructure = false;

for (int i = 0; i < args.Length; i++)
{
    string arg = args[i];
    switch (arg)
    {
        case "--help":
        case "-h":
            Console.WriteLine(helpText);
            return ExitCodes.Success;

        case "--list-profiles":
            foreach (var (name, _) in ProfileNames.CanonicalNames)
            {
                Console.WriteLine(name);
            }
            return ExitCodes.Success;

        case "--version":
            var asmVersion = typeof(Program).Assembly.GetName().Version;
            Console.WriteLine($"legibyl {asmVersion} (vendored veraPDF engine {ProfileNames.VendoredEngineVersion})");
            return ExitCodes.Success;

        case "--profile":
            if (!TryTakeValue(args, ref i, out profileName!))
            {
                return UsageError("--profile requires a value.");
            }
            break;

        case "--format":
            if (!TryTakeValue(args, ref i, out format!))
            {
                return UsageError("--format requires a value.");
            }
            if (format is not ("json" or "text"))
            {
                return UsageError($"Unknown --format '{format}'. Expected 'json' or 'text'.");
            }
            break;

        case "--output":
            if (!TryTakeValue(args, ref i, out outputPath!))
            {
                return UsageError("--output requires a value.");
            }
            break;

        case "--fix":
            doFix = true;
            break;

        case "--fix-output":
            if (!TryTakeValue(args, ref i, out fixOutputPath!))
            {
                return UsageError("--fix-output requires a value.");
            }
            break;

        case "--structure":
            includeStructure = true;
            break;

        default:
            if (arg.StartsWith('-') && arg.Length > 1)
            {
                return UsageError($"Unknown option '{arg}'.");
            }
            files.Add(arg);
            break;
    }
}

if (files.Count == 0)
{
    return UsageError("No input PDF files given.");
}

if (!ProfileNames.TryParse(profileName, out PdfProfile profile))
{
    return UsageError($"Unknown profile '{profileName}'. Run --list-profiles to see valid names.");
}

if (fixOutputPath is not null && !doFix)
{
    return UsageError("--fix-output requires --fix.");
}

if (fixOutputPath is not null && files.Count > 1)
{
    return UsageError("--fix-output can only be used with a single input file.");
}

var validator = new VeraPdfValidator();
var structureReader = (IStructureTreeReader)validator;
var results = new List<FileValidationResult>(files.Count);

foreach (string file in files)
{
    results.Add(ValidateOne(validator, validator, structureReader, file, profileName, profile, doFix, fixOutputPath, includeStructure));
}

string rendered = format == "json" ? RenderJson(results) : RenderText(results);

if (outputPath is not null)
{
    File.WriteAllText(outputPath, rendered);
}
else
{
    Console.WriteLine(rendered);
}

bool anyErrors = results.Any(r => r.Error is not null);
bool anyNonCompliant = results.Any(r => r.IsCompliant == false);

return anyErrors ? ExitCodes.ValidationError
     : anyNonCompliant ? ExitCodes.NonCompliant
     : ExitCodes.Success;

static FileValidationResult ValidateOne(
    IPdfValidator validator, IPdfFixer fixer, IStructureTreeReader structureReader, string file,
    string profileName, PdfProfile profile, bool doFix, string? fixOutputPath, bool includeStructure)
{
    ValidationReport report;
    try
    {
        report = validator.Validate(file, profile);
    }
    catch (VeraPdfValidationException ex)
    {
        return new FileValidationResult
        {
            File = file,
            Profile = profileName,
            Error = ex.Message,
        };
    }

    string? fixStatus = null;
    string? fixOutput = null;
    IReadOnlyList<string>? fixAppliedFixes = null;
    string? fixError = null;

    if (doFix)
    {
        string targetPath = fixOutputPath ?? DeriveFixOutputPath(file);
        try
        {
            FixResult fix = fixer.Fix(file, profile, targetPath);
            fixStatus = fix.Status.ToString();
            fixOutput = fix.OutputPath;
            fixAppliedFixes = fix.AppliedFixes.Count > 0 ? fix.AppliedFixes : null;
        }
        catch (VeraPdfValidationException ex)
        {
            fixError = ex.Message;
        }
    }

    StructureNode? structure = null;
    string? structureError = null;
    if (includeStructure)
    {
        try
        {
            structure = structureReader.ReadStructureTree(file);
        }
        catch (VeraPdfValidationException ex)
        {
            structureError = ex.Message;
        }
    }

    return new FileValidationResult
    {
        File = file,
        Profile = profileName,
        FlavourId = report.FlavourId,
        IsCompliant = report.IsCompliant,
        TotalAssertions = report.TotalAssertions,
        FailedCount = report.FailedCount,
        Failures = report.FailedAssertions.Select(a => new FailureDetail
        {
            Specification = a.Specification,
            Clause = a.Clause,
            TestNumber = a.TestNumber,
            Message = a.Message,
            ErrorMessage = a.ErrorMessage,
            LocationContext = a.LocationContext,
            IsFixable = a.IsFixable,
            FixOptions = a.IsFixable ? a.FixOptions.ToString() : null,
        }).ToList(),
        Warnings = report.Assertions
            .Where(a => a.Severity == FindingSeverity.Warning)
            .Select(a => new FailureDetail
            {
                Specification = a.Specification,
                Clause = a.Clause,
                TestNumber = a.TestNumber,
                Message = a.Message,
                ErrorMessage = a.ErrorMessage,
                LocationContext = a.LocationContext,
                IsFixable = a.IsFixable,
                FixOptions = a.IsFixable ? a.FixOptions.ToString() : null,
            })
            .ToList(),
        FixStatus = fixStatus,
        FixOutputPath = fixOutput,
        FixAppliedFixes = fixAppliedFixes,
        FixError = fixError,
        Structure = structure,
        StructureError = structureError,
    };
}

static string DeriveFixOutputPath(string file)
{
    string? dir = Path.GetDirectoryName(file);
    string name = Path.GetFileNameWithoutExtension(file);
    string ext = Path.GetExtension(file);
    string fixedName = $"{name}.fixed{ext}";
    return string.IsNullOrEmpty(dir) ? fixedName : Path.Combine(dir, fixedName);
}

static string RenderJson(List<FileValidationResult> results)
{
    var options = new JsonSerializerOptions
    {
        WriteIndented = true,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        DefaultIgnoreCondition = JsonIgnoreCondition.Never,
    };
    return JsonSerializer.Serialize(results, options);
}

static string RenderText(List<FileValidationResult> results)
{
    var sb = new System.Text.StringBuilder();
    foreach (var r in results)
    {
        if (r.Error is not null)
        {
            sb.AppendLine($"{r.File}: ERROR — {r.Error}");
            continue;
        }

        sb.AppendLine(r.IsCompliant == true
            ? $"{r.File}: PASS ({r.Profile}, {r.TotalAssertions} checks)"
            : $"{r.File}: FAIL ({r.Profile}, {r.FailedCount}/{r.TotalAssertions} checks failed)");

        foreach (var f in r.Failures ?? [])
        {
            sb.AppendLine($"  [{f.Specification} {f.Clause}-{f.TestNumber}] {f.Message}{FixableSuffix(f)}");
            if (!string.IsNullOrEmpty(f.LocationContext))
            {
                sb.AppendLine($"    at: {f.LocationContext}");
            }
        }

        foreach (var warning in r.Warnings ?? [])
        {
            sb.AppendLine($"  WARNING [{warning.Specification} {warning.Clause}-{warning.TestNumber}] {warning.Message}{FixableSuffix(warning)}");
            if (!string.IsNullOrEmpty(warning.LocationContext))
            {
                sb.AppendLine($"    at: {warning.LocationContext}");
            }
        }

        if (r.FixError is not null)
        {
            sb.AppendLine($"  fix: ERROR — {r.FixError}");
        }
        else if (r.FixStatus is not null)
        {
            sb.AppendLine(r.FixOutputPath is not null
                ? $"  fix: {r.FixStatus} → {r.FixOutputPath}"
                : $"  fix: {r.FixStatus} (no output written)");

            foreach (var applied in r.FixAppliedFixes ?? [])
            {
                sb.AppendLine($"    - {applied}");
            }
        }

        if (r.StructureError is not null)
        {
            sb.AppendLine($"  structure: ERROR — {r.StructureError}");
        }
        else if (r.Structure is not null)
        {
            sb.AppendLine("  structure:");
            AppendStructure(sb, r.Structure, indent: 4);
        }
    }
    return sb.ToString().TrimEnd('\n', '\r');

    static string FixableSuffix(FailureDetail detail) =>
        detail.IsFixable ? $" (fixable: {detail.FixOptions})" : string.Empty;
}

static void AppendStructure(System.Text.StringBuilder sb, StructureNode node, int indent)
{
    string pad = new(' ', indent);
    var parts = new List<string> { $"<{node.Tag}>" };
    if (node.Role is not null) parts.Add($"role={node.Role}");
    if (node.PageNumber is not null) parts.Add($"page={node.PageNumber}");
    if (node.Language is not null) parts.Add($"lang={node.Language}");
    if (node.AlternateDescription is not null) parts.Add($"alt=\"{node.AlternateDescription}\"");
    if (node.ActualText is not null) parts.Add($"actualText=\"{node.ActualText}\"");
    if (node.FormFieldName is not null) parts.Add($"field=\"{node.FormFieldName}\"");
    if (node.FormFieldType is not null) parts.Add($"fieldType={node.FormFieldType}");
    if (node.FormFieldDescription is not null) parts.Add($"tooltip=\"{node.FormFieldDescription}\"");
    if (node.FormFieldValue is not null) parts.Add($"value=\"{node.FormFieldValue}\"");
    if (node.LinkUri is not null) parts.Add($"href=\"{node.LinkUri}\"");
    if (node.LinkDestinationDescription is not null) parts.Add($"dest=\"{node.LinkDestinationDescription}\"");
    if (node.ContentLeafCount > 0) parts.Add($"content×{node.ContentLeafCount}");
    sb.AppendLine(pad + string.Join(' ', parts));
    if (node.Content is not null)
    {
        sb.AppendLine(pad + $"  text=\"{node.Content}\"");
    }

    foreach (var child in node.Children)
    {
        AppendStructure(sb, child, indent + 2);
    }
}

static bool TryTakeValue(string[] args, ref int i, out string value)
{
    if (i + 1 >= args.Length)
    {
        value = "";
        return false;
    }
    value = args[++i];
    return true;
}

static int UsageError(string message)
{
    Console.Error.WriteLine($"error: {message}");
    Console.Error.WriteLine("Run with --help for usage.");
    return ExitCodes.UsageError;
}
