using System.IO;
using org.verapdf.core;
using org.verapdf.gf.foundry;
using org.verapdf.pdfa;
using org.verapdf.pdfa.flavours;
using org.verapdf.pdfa.results;

namespace VeraPdfTool.Validation;

/// <summary>
/// <see cref="IPdfValidator"/> implementation backed by veraPDF's Greenfield
/// Java validation core, executed in-process via IKVM. All Java/IKVM types
/// are confined to this file; callers only see the plain C# types in this
/// namespace (<see cref="ValidationReport"/>, <see cref="RuleViolation"/>, etc).
/// </summary>
public sealed class VeraPdfValidator : IPdfValidator, IPdfFixer
{
    private static readonly object InitLock = new();
    private static bool _initialised;

    public VeraPdfValidator()
    {
        EnsureInitialised();
    }

    private static void EnsureInitialised()
    {
        if (_initialised)
        {
            return;
        }

        lock (InitLock)
        {
            if (_initialised)
            {
                return;
            }

            VeraGreenfieldFoundryProvider.initialise();
            _initialised = true;
        }
    }

    public ValidationReport Validate(string pdfPath, PdfProfile profile)
    {
        if (!File.Exists(pdfPath))
        {
            throw new VeraPdfValidationException($"File not found: {pdfPath}");
        }

        PDFAFlavour flavour = ToJavaFlavour(profile);
        VeraPDFFoundry foundry = Foundries.defaultInstance();

        PDFAParser? parser = null;
        try
        {
            parser = foundry.createParser(new java.io.File(pdfPath), flavour);
            PDFAValidator validator = foundry.createValidator(flavour, true);
            ValidationResult result = validator.validate(parser);
            return ToReport(pdfPath, profile, result);
        }
        catch (VeraPDFException ex)
        {
            throw new VeraPdfValidationException($"veraPDF failed to validate '{pdfPath}': {ex.Message}", ex);
        }
        catch (java.lang.Exception ex)
        {
            throw new VeraPdfValidationException($"veraPDF failed to validate '{pdfPath}': {ex.getMessage()}", ex);
        }
        finally
        {
            parser?.close();
        }
    }

    public FixResult Fix(string pdfPath, PdfProfile profile, string outputPath)
    {
        if (!File.Exists(pdfPath))
        {
            throw new VeraPdfValidationException($"File not found: {pdfPath}");
        }

        PDFAFlavour flavour = ToJavaFlavour(profile);
        VeraPDFFoundry foundry = Foundries.defaultInstance();

        PDFAParser? parser = null;
        java.io.OutputStream? outputStream = null;
        try
        {
            parser = foundry.createParser(new java.io.File(pdfPath), flavour);
            PDFAValidator validator = foundry.createValidator(flavour, false);
            ValidationResult result = validator.validate(parser);
            ValidationReport originalReport = ToReport(pdfPath, profile, result);

            MetadataFixer fixer = foundry.createMetadataFixer();
            outputStream = new java.io.FileOutputStream(outputPath);
            MetadataFixerResult fixResult = fixer.fixMetadata(parser, outputStream, result);
            outputStream.close();
            outputStream = null;

            var appliedFixes = new List<string>();
            var fixIterator = fixResult.getAppliedFixes().iterator();
            while (fixIterator.hasNext())
            {
                appliedFixes.Add((string)fixIterator.next());
            }

            // The fixer only writes a repaired copy when it actually changed something;
            // otherwise our freshly-created output file is left empty. Don't report a
            // 0-byte file as a usable "fixed" output.
            bool wroteOutput = File.Exists(outputPath) && new FileInfo(outputPath).Length > 0;
            if (!wroteOutput && File.Exists(outputPath))
            {
                File.Delete(outputPath);
            }

            return new FixResult(
                FilePath: pdfPath,
                Profile: profile,
                OutputPath: wroteOutput ? outputPath : null,
                Status: ToRepairStatus(fixResult.getRepairStatus()),
                AppliedFixes: appliedFixes,
                OriginalReport: originalReport);
        }
        catch (VeraPDFException ex)
        {
            throw new VeraPdfValidationException($"veraPDF failed to fix '{pdfPath}': {ex.Message}", ex);
        }
        catch (java.lang.Exception ex)
        {
            throw new VeraPdfValidationException($"veraPDF failed to fix '{pdfPath}': {ex.getMessage()}", ex);
        }
        finally
        {
            outputStream?.close();
            parser?.close();
        }
    }

    private static RepairStatus ToRepairStatus(MetadataFixerResult.RepairStatus status)
    {
        if (status == MetadataFixerResult.RepairStatus.SUCCESS) return RepairStatus.Success;
        if (status == MetadataFixerResult.RepairStatus.FIX_ERROR) return RepairStatus.FixError;
        if (status == MetadataFixerResult.RepairStatus.WONT_FIX) return RepairStatus.WontFix;
        if (status == MetadataFixerResult.RepairStatus.NO_ACTION) return RepairStatus.NoAction;
        if (status == MetadataFixerResult.RepairStatus.ID_REMOVED) return RepairStatus.IdRemoved;
        throw new ArgumentOutOfRangeException(nameof(status), status.ToString(), "Unknown veraPDF repair status");
    }

    private static PDFAFlavour ToJavaFlavour(PdfProfile profile) => profile switch
    {
        PdfProfile.PdfUa1 => PDFAFlavour.PDFUA_1,
        PdfProfile.PdfA1B => PDFAFlavour.PDFA_1_B,
        PdfProfile.PdfA2B => PDFAFlavour.PDFA_2_B,
        PdfProfile.PdfA2U => PDFAFlavour.PDFA_2_U,
        PdfProfile.PdfA3B => PDFAFlavour.PDFA_3_B,
        PdfProfile.PdfA4 => PDFAFlavour.PDFA_4,
        _ => throw new ArgumentOutOfRangeException(nameof(profile), profile, "Unsupported PDF profile"),
    };

    private static ValidationReport ToReport(string pdfPath, PdfProfile profile, ValidationResult result)
    {
        var assertions = new List<RuleViolation>();
        var iterator = result.getTestAssertions().iterator();
        while (iterator.hasNext())
        {
            var assertion = (TestAssertion)iterator.next();
            var ruleId = assertion.getRuleId();
            // NB: assertion.getLocationContext() is a rarely-populated, model-object-specific
            // "extra context" field (almost always null). The real per-failure tree-walk
            // path (e.g. "root/Catalog[0]/StructTreeRoot[0]/.../TH[3]") — the same value
            // veraPDF's own report writer uses — lives on assertion.getLocation().getContext().
            assertions.Add(new RuleViolation(
                Specification: ruleId.getSpecification().ToString(),
                Clause: ruleId.getClause(),
                TestNumber: ruleId.getTestNumber(),
                Passed: assertion.getStatus() == TestAssertion.Status.PASSED,
                Message: assertion.getMessage(),
                ErrorMessage: assertion.getErrorMessage(),
                LocationContext: assertion.getLocation()?.getContext()));
        }

        return new ValidationReport
        {
            FilePath = pdfPath,
            Profile = profile,
            FlavourId = result.getPDFAFlavour().getId(),
            IsCompliant = result.isCompliant(),
            TotalAssertions = result.getTotalAssertions(),
            Assertions = assertions,
        };
    }
}
