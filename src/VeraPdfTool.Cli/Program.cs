using System.Text.Json;
using System.Text.Json.Serialization;
using VeraPdfTool.Cli;
using VeraPdfTool.Validation;

const string helpText = """
verapdf-tool - PDF/UA & PDF/A validator (CLI) powered by veraPDF via IKVM.

Designed for scripting and AI-agent use: default output is machine-readable
JSON on stdout, and the process exit code tells you the outcome without
needing to parse output.

USAGE:
  verapdf-tool <file.pdf> [<file2.pdf> ...] [options]
  verapdf-tool --list-profiles
  verapdf-tool --help

OPTIONS:
  --profile <name>     Profile to validate against (default: pdfua1).
                        See --list-profiles for all accepted names.
  --format <json|text> Output format (default: json).
  --output <path>      Write output to a file instead of stdout.
  --list-profiles      Print supported profile names and exit 0.
  --version            Print tool + vendored veraPDF engine version and exit 0.
  --help, -h           Print this help and exit 0.

EXIT CODES:
  0   all files validated and are compliant.
  1   all files validated successfully, but at least one is non-compliant.
  2   at least one file could not be validated (missing/corrupt/encrypted).
  64  invalid command-line usage.

EXAMPLES:
  verapdf-tool document.pdf
  verapdf-tool document.pdf --profile pdfa1b --format text
  verapdf-tool *.pdf --output results.json
""";

var files = new List<string>();
string profileName = ProfileNames.DefaultName;
string format = "json";
string? outputPath = null;

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
            Console.WriteLine($"verapdf-tool {asmVersion} (vendored veraPDF engine {ProfileNames.VendoredEngineVersion})");
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

var validator = new VeraPdfValidator();
var results = new List<FileValidationResult>(files.Count);

foreach (string file in files)
{
    results.Add(ValidateOne(validator, file, profileName, profile));
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
    IPdfValidator validator, string file, string profileName, PdfProfile profile)
{
    try
    {
        ValidationReport report = validator.Validate(file, profile);
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
            }).ToList(),
        };
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
            sb.AppendLine($"  [{f.Specification} {f.Clause}-{f.TestNumber}] {f.Message}");
            if (!string.IsNullOrEmpty(f.LocationContext))
            {
                sb.AppendLine($"    at: {f.LocationContext}");
            }
        }
    }
    return sb.ToString().TrimEnd('\n', '\r');
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
