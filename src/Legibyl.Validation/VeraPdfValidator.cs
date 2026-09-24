using System.IO;
using org.verapdf.core;
using org.verapdf.gf.foundry;
using org.verapdf.pdfa;
using org.verapdf.pdfa.flavours;
using org.verapdf.pdfa.results;

namespace Legibyl.Validation;

/// <summary>
/// <see cref="IPdfValidator"/> implementation backed by veraPDF's Greenfield
/// Java validation core, executed in-process via IKVM. All Java/IKVM types
/// are confined to this file; callers only see the plain C# types in this
/// namespace (<see cref="ValidationReport"/>, <see cref="RuleViolation"/>, etc).
/// </summary>
public sealed class VeraPdfValidator : IPdfValidator, IPdfFixer, IStructureTreeReader
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

            RepairStatus status = ToRepairStatus(fixResult.getRepairStatus());

            // Only Success/IdRemoved reflect the fixer actually changing something; NoAction,
            // WontFix and FixError leave a repaired copy that's either empty or just a useless
            // byte-for-byte (or broken) duplicate of the original. Don't keep a file the user
            // didn't ask for and can't use — clean it up and report no output path.
            bool wroteUsableOutput = status is RepairStatus.Success or RepairStatus.IdRemoved
                && File.Exists(outputPath) && new FileInfo(outputPath).Length > 0;
            if (!wroteUsableOutput && File.Exists(outputPath))
            {
                File.Delete(outputPath);
            }

            return new FixResult(
                FilePath: pdfPath,
                Profile: profile,
                OutputPath: wroteUsableOutput ? outputPath : null,
                Status: status,
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

    /// <summary>
    /// Reads the structure tree by opening the PDF a second time through veraPDF's
    /// plain PD model (org.verapdf.pd.PDDocument) — the same model the validator
    /// itself uses internally. This is intentionally independent of <see cref="Validate"/>
    /// / <see cref="Fix"/> (no shared parser/session state), since reading the tagged
    /// structure is a distinct concern from PDF/A|UA rule validation and callers may
    /// want it without running a full validation pass.
    /// </summary>
    public StructureNode? ReadStructureTree(string pdfPath)
    {
        if (!File.Exists(pdfPath))
        {
            throw new VeraPdfValidationException($"File not found: {pdfPath}");
        }

        org.verapdf.pd.PDDocument? doc = null;
        try
        {
            doc = new org.verapdf.pd.PDDocument(pdfPath);
            org.verapdf.pd.structure.PDStructTreeRoot? structTreeRoot = doc.getStructTreeRoot();
            if (structTreeRoot == null)
            {
                return null;
            }

            var pageNumberByKey = BuildPageNumberIndex(doc);
            var children = WalkStructureChildren(structTreeRoot, pageNumberByKey, depth: 0);
            return new StructureNode(
                Tag: "StructTreeRoot",
                Role: null,
                AlternateDescription: null,
                ActualText: null,
                Language: null,
                PageNumber: null,
                ContentLeafCount: 0,
                Children: children);
        }
        catch (VeraPDFException ex)
        {
            throw new VeraPdfValidationException($"veraPDF failed to read the structure tree of '{pdfPath}': {ex.Message}", ex);
        }
        catch (java.lang.Exception ex)
        {
            throw new VeraPdfValidationException($"veraPDF failed to read the structure tree of '{pdfPath}': {ex.getMessage()}", ex);
        }
        finally
        {
            doc?.close();
        }
    }

    // Guards against pathological/malformed structure trees (shouldn't happen for a
    // real tree, but a corrupt or maliciously crafted PDF could contain very deep or
    // cyclic-looking nesting) so a single bad file can't hang or stack-overflow the walk.
    private const int MaxStructureDepth = 200;

    /// <summary>
    /// Maps each page's underlying COS object key (e.g. "12 0 obj") to its 1-based
    /// page number, so structure elements (which only know their owning page via an
    /// indirect object reference) can be resolved back to a page number.
    /// </summary>
    private static Dictionary<string, int> BuildPageNumberIndex(org.verapdf.pd.PDDocument doc)
    {
        var index = new Dictionary<string, int>();
        var pages = doc.getPages();
        int count = pages.size();
        for (int i = 0; i < count; i++)
        {
            if (pages.get(i) is org.verapdf.pd.PDPage page)
            {
                var key = page.getObject()?.getKey();
                if (key != null)
                {
                    index[key.toString()] = i + 1;
                }
            }
        }
        return index;
    }

    private static IReadOnlyList<StructureNode> WalkStructureChildren(
        org.verapdf.pd.structure.PDStructTreeNode node, Dictionary<string, int> pageNumberByKey, int depth)
    {
        if (depth >= MaxStructureDepth)
        {
            return [];
        }

        var result = new List<StructureNode>();
        var iterator = node.getChildren().iterator();
        while (iterator.hasNext())
        {
            // Non-structure-element children are marked-content references and object
            // references (e.g. to a Link annotation) — the tree's actual leaves, i.e. the
            // tagged page content itself. They carry no hierarchy/semantic info of their
            // own, so they aren't represented as nodes; ToStructureNode below counts them
            // via ContentLeafCount instead.
            if (iterator.next() is org.verapdf.pd.structure.PDStructElem elem)
            {
                result.Add(ToStructureNode(elem, pageNumberByKey, depth));
            }
        }
        return result;
    }

    private static StructureNode ToStructureNode(
        org.verapdf.pd.structure.PDStructElem elem, Dictionary<string, int> pageNumberByKey, int depth)
    {
        string tag = ResolveStructureTag(elem);
        string? role = elem.getRoleMapToSameNamespaceTag();
        if (role == tag)
        {
            role = null;
        }

        int? pageNumber = null;
        var pageKey = elem.getPageObjectNumber();
        if (pageKey != null && pageNumberByKey.TryGetValue(pageKey.toString(), out int number))
        {
            pageNumber = number;
        }

        int contentLeafCount = 0;
        var iterator = elem.getChildren().iterator();
        while (iterator.hasNext())
        {
            if (iterator.next() is not org.verapdf.pd.structure.PDStructElem)
            {
                contentLeafCount++;
            }
        }

        return new StructureNode(
            Tag: tag,
            Role: role,
            AlternateDescription: NullIfEmpty(elem.getAlternateDescription()),
            ActualText: NullIfEmpty(ActualTextOf(elem)),
            Language: NullIfEmpty(elem.getLang()?.toString()),
            PageNumber: pageNumber,
            ContentLeafCount: contentLeafCount,
            Children: WalkStructureChildren(elem, pageNumberByKey, depth + 1));
    }

    private static string ResolveStructureTag(org.verapdf.pd.structure.PDStructElem elem)
    {
        // Prefer the resolved type atom / structure-type / raw COS name, in that order
        // of preference — for standard tags these all agree, but falling back covers
        // edge cases (e.g. custom-namespace tags) where the higher-level accessor
        // returns null. See scratch investigation in this session for how each of
        // these was found to behave against a real veraPDF-corpus PDF/UA sample.
        return elem.getType()?.getValue()
            ?? elem.getStructureType()?.getType()?.getValue()
            ?? elem.getCOSStructureType()?.getName()?.getValue()
            ?? "?";
    }

    private static string? ActualTextOf(org.verapdf.pd.structure.PDStructElem elem)
    {
        var actualText = elem.getActualText();
        if (actualText == null || actualText.empty())
        {
            return null;
        }

        try
        {
            return actualText.getString();
        }
        catch (java.lang.Exception)
        {
            return null;
        }
    }

    private static string? NullIfEmpty(string? value) => string.IsNullOrEmpty(value) ? null : value;

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
