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
public sealed class VeraPdfValidator : IPdfValidator
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
            assertions.Add(new RuleViolation(
                Specification: ruleId.getSpecification().ToString(),
                Clause: ruleId.getClause(),
                TestNumber: ruleId.getTestNumber(),
                Passed: assertion.getStatus() == TestAssertion.Status.PASSED,
                Message: assertion.getMessage(),
                ErrorMessage: assertion.getErrorMessage(),
                LocationContext: assertion.getLocationContext()));
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
