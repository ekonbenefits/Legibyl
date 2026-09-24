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
            // Built via a second, independent parse (PdfPig) — see MarkedContentTextIndex
            // remarks. If this fails for any reason (e.g. a PDF quirk PdfPig can't parse
            // that veraPDF tolerates), degrade gracefully: keep the structure tree itself,
            // just without extracted text content, rather than failing the whole read.
            Dictionary<(int Page, int Mcid), string> textIndex;
            try
            {
                textIndex = MarkedContentTextIndex.Build(pdfPath);
            }
            catch (Exception)
            {
                textIndex = [];
            }

            var children = WalkStructureChildren(structTreeRoot, pageNumberByKey, textIndex, inheritedPage: null, depth: 0);
            return new StructureNode(
                Tag: "StructTreeRoot",
                Role: null,
                AlternateDescription: null,
                ActualText: null,
                Content: null,
                FormFieldName: null,
                FormFieldDescription: null,
                FormFieldType: null,
                FormFieldValue: null,
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
        org.verapdf.pd.structure.PDStructTreeNode node, Dictionary<string, int> pageNumberByKey,
        Dictionary<(int Page, int Mcid), string> textIndex, int? inheritedPage, int depth)
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
            // own, so they aren't represented as nodes; ToStructureNode below resolves them
            // to visible text (via textIndex) and counts them via ContentLeafCount.
            if (iterator.next() is org.verapdf.pd.structure.PDStructElem elem)
            {
                result.Add(ToStructureNode(elem, pageNumberByKey, textIndex, inheritedPage, depth));
            }
        }
        return result;
    }

    private static StructureNode ToStructureNode(
        org.verapdf.pd.structure.PDStructElem elem, Dictionary<string, int> pageNumberByKey,
        Dictionary<(int Page, int Mcid), string> textIndex, int? inheritedPage, int depth)
    {
        string tag = ResolveStructureTag(elem);
        string? role = elem.getRoleMapToSameNamespaceTag();
        if (role == tag)
        {
            role = null;
        }

        // A structure element's own page is only explicit via /Pg on itself or an
        // ancestor; page-less elements (most containers) inherit their nearest
        // ancestor's page for the purpose of resolving their *own* marked content
        // (an element's /K array of direct MCID integers is always relative to
        // whichever page it/its ancestor declared).
        int? ownPage = inheritedPage;
        var pageKey = elem.getPageObjectNumber();
        if (pageKey != null && pageNumberByKey.TryGetValue(pageKey.toString(), out int number))
        {
            ownPage = number;
        }

        int contentLeafCount = 0;
        var contentTexts = new List<string>();
        FormFieldInfo? formField = null;
        var iterator = elem.getChildren().iterator();
        while (iterator.hasNext())
        {
            var child = iterator.next();
            if (child is org.verapdf.pd.structure.PDStructElem)
            {
                continue;
            }

            contentLeafCount++;
            if (TryResolveContentText(child, ownPage, pageNumberByKey, textIndex, out string? text))
            {
                contentTexts.Add(text!);
            }

            formField ??= TryReadFormFieldInfo(child);
        }

        string? content = contentTexts.Count > 0 ? string.Join(" ", contentTexts) : null;

        return new StructureNode(
            Tag: tag,
            Role: role,
            AlternateDescription: NullIfEmpty(elem.getAlternateDescription()),
            ActualText: NullIfEmpty(ActualTextOf(elem)),
            Content: content,
            FormFieldName: formField?.Name,
            FormFieldDescription: formField?.Description,
            FormFieldType: formField?.Type,
            FormFieldValue: formField?.Value,
            Language: NullIfEmpty(elem.getLang()?.toString()),
            PageNumber: ownPage,
            ContentLeafCount: contentLeafCount,
            Children: WalkStructureChildren(elem, pageNumberByKey, textIndex, ownPage, depth + 1));
    }

    private sealed record FormFieldInfo(string? Name, string? Description, string? Type, string? Value);

    private static FormFieldInfo? TryReadFormFieldInfo(object child)
    {
        if (child is not org.verapdf.pd.structure.PDOBJRDictionary objr)
        {
            return null;
        }

        var referenced = objr.getReferencedObject();
        if (referenced == null || referenced.empty())
        {
            return null;
        }

        string? subtype = AtomValue(referenced.getNameKey(Atom("Subtype")));
        if (subtype != "Widget")
        {
            return null;
        }

        string? name = NullIfEmpty(referenced.getStringKey(Atom("T")));
        string? description = NullIfEmpty(referenced.getStringKey(Atom("TU")));
        string? type = AtomValue(referenced.getNameKey(Atom("FT")));
        string? value = NullIfEmpty(referenced.getStringKey(Atom("V"))) ?? AtomValue(referenced.getNameKey(Atom("V")));
        return new FormFieldInfo(name, description, type, value);
    }

    private static org.verapdf.@as.ASAtom Atom(string value) => org.verapdf.@as.ASAtom.getASAtom(value);

    private static string? AtomValue(org.verapdf.@as.ASAtom? atom) => atom?.getValue();

    /// <summary>
    /// Resolves one of a structure element's direct (non-structure-element) children —
    /// either a plain MCID integer (content on the element's own/inherited page) or an
    /// explicit <c>PDMCRDictionary</c> (content on a different page, e.g. artifacts
    /// spanning a page boundary) — to its extracted visible text via <paramref name="textIndex"/>.
    /// Object references (<c>PDOBJRDictionary</c>, e.g. a Link's associated annotation)
    /// have no marked-content text of their own and are skipped.
    /// </summary>
    private static bool TryResolveContentText(
        object child, int? ownPage, Dictionary<string, int> pageNumberByKey,
        Dictionary<(int Page, int Mcid), string> textIndex, out string? text)
    {
        text = null;

        if (child is org.verapdf.pd.structure.PDMCRDictionary mcr)
        {
            int? page = ownPage;
            var pageKey = mcr.getPageObjectKey();
            if (pageKey != null && pageNumberByKey.TryGetValue(pageKey.toString(), out int number))
            {
                page = number;
            }

            var mcid = mcr.getMCID();
            if (page is not null && mcid is not null && textIndex.TryGetValue((page.Value, (int)mcid.longValue()), out string? resolved))
            {
                text = resolved;
                return true;
            }
            return false;
        }

        if (child is org.verapdf.cos.COSObject cos && cos.getType()?.isNumber() == true && ownPage is not null)
        {
            var mcid = cos.getInteger();
            if (mcid is not null && textIndex.TryGetValue((ownPage.Value, (int)mcid.longValue()), out string? resolved))
            {
                text = resolved;
                return true;
            }
        }

        return false;
    }

    private static string ResolveStructureTag(org.verapdf.pd.structure.PDStructElem elem)
    {
        // Prefer the semantic structure role (/S), not the object dictionary type
        // (/Type /StructElem). Some valid tagged PDFs use /S /Form with /Type
        // /StructElem; if we prefer getType() first, those controls display as the
        // generic implementation object "StructElem" instead of the screen-reader
        // role "Form".
        return elem.getCOSStructureType()?.getName()?.getValue()
            ?? elem.getStructureType()?.getType()?.getValue()
            ?? elem.getType()?.getValue()
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
