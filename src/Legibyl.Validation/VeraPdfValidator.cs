using System.IO;
using iText.Kernel.Pdf;
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

    public FixResult Fix(string pdfPath, PdfProfile profile, string outputPath, PdfFixOptions options = PdfFixOptions.Metadata)
    {
        if (!File.Exists(pdfPath))
        {
            throw new VeraPdfValidationException($"File not found: {pdfPath}");
        }
        if (options == PdfFixOptions.None)
        {
            throw new VeraPdfValidationException("At least one fix option must be selected.");
        }

        ValidationReport originalReport = Validate(pdfPath, profile);
        var appliedFixes = new List<string>();
        string currentInput = pdfPath;
        string? tempMetadataOutput = null;
        string? tempStructuralOutput = null;
        bool changed = false;
        RepairStatus status = RepairStatus.NoAction;

        try
        {
            if (options.HasFlag(PdfFixOptions.Metadata))
            {
                tempMetadataOutput = CreateTemporaryPdfPath(outputPath);
                MetadataFixOutcome metadataFix = FixMetadataOnly(currentInput, profile, tempMetadataOutput, originalReport);
                status = metadataFix.Status;
                appliedFixes.AddRange(metadataFix.AppliedFixes);
                if (metadataFix.Changed)
                {
                    currentInput = tempMetadataOutput;
                    changed = true;
                }
                else
                {
                    DeleteIfExists(tempMetadataOutput);
                    tempMetadataOutput = null;
                }
            }

            if (options.HasFlag(PdfFixOptions.AnnotationTabs) || options.HasFlag(PdfFixOptions.RemoveEmptyParagraphs))
            {
                tempStructuralOutput = CreateTemporaryPdfPath(outputPath);
                int tabsFixed = 0;
                int emptyParagraphsRemoved = 0;
                bool structuralChanged = ApplyStructuralFixes(
                    currentInput,
                    tempStructuralOutput,
                    options,
                    ref tabsFixed,
                    ref emptyParagraphsRemoved);

                if (structuralChanged)
                {
                    if (tabsFixed > 0)
                    {
                        appliedFixes.Add($"Set /Tabs /S on {tabsFixed} page(s) containing annotations.");
                    }
                    if (emptyParagraphsRemoved > 0)
                    {
                        appliedFixes.Add($"Removed {emptyParagraphsRemoved} empty /P structure element(s).");
                    }

                    currentInput = tempStructuralOutput;
                    changed = true;
                    status = RepairStatus.Success;
                }
                else
                {
                    DeleteIfExists(tempStructuralOutput);
                    tempStructuralOutput = null;
                }
            }

            if (!changed)
            {
                DeleteIfExists(outputPath);
                return new FixResult(
                    FilePath: pdfPath,
                    Profile: profile,
                    RequestedOptions: options,
                    OutputPath: null,
                    Status: status,
                    AppliedFixes: appliedFixes,
                    OriginalReport: originalReport);
            }

            System.IO.Directory.CreateDirectory(Path.GetDirectoryName(Path.GetFullPath(outputPath))!);
            File.Copy(currentInput, outputPath, overwrite: true);

            return new FixResult(
                FilePath: pdfPath,
                Profile: profile,
                RequestedOptions: options,
                OutputPath: outputPath,
                Status: status,
                AppliedFixes: appliedFixes,
                OriginalReport: originalReport);
        }
        catch (iText.Kernel.Exceptions.PdfException ex)
        {
            throw new VeraPdfValidationException($"iText failed to fix '{pdfPath}': {ex.Message}", ex);
        }
        catch (IOException ex)
        {
            throw new VeraPdfValidationException($"Could not write fixed PDF '{outputPath}': {ex.Message}", ex);
        }
        finally
        {
            if (!string.Equals(tempMetadataOutput, outputPath, StringComparison.OrdinalIgnoreCase))
            {
                DeleteIfExists(tempMetadataOutput);
            }
            if (!string.Equals(tempStructuralOutput, outputPath, StringComparison.OrdinalIgnoreCase))
            {
                DeleteIfExists(tempStructuralOutput);
            }
        }
    }

    private static MetadataFixOutcome FixMetadataOnly(
        string pdfPath,
        PdfProfile profile,
        string outputPath,
        ValidationReport originalReport)
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

            return new MetadataFixOutcome(wroteUsableOutput, status, appliedFixes);
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

    private static bool ApplyStructuralFixes(
        string inputPath,
        string outputPath,
        PdfFixOptions options,
        ref int tabsFixed,
        ref int emptyParagraphsRemoved)
    {
        using var reader = new PdfReader(inputPath);
        using var writer = new PdfWriter(outputPath);
        using var document = new PdfDocument(reader, writer);

        if (options.HasFlag(PdfFixOptions.AnnotationTabs))
        {
            for (int pageNumber = 1; pageNumber <= document.GetNumberOfPages(); pageNumber++)
            {
                PdfDictionary pageDictionary = document.GetPage(pageNumber).GetPdfObject();
                PdfArray? annotations = pageDictionary.GetAsArray(PdfName.Annots);
                if (annotations is null || annotations.Size() == 0)
                {
                    continue;
                }

                PdfName? tabs = pageDictionary.GetAsName(PdfName.Tabs);
                if (!PdfName.S.Equals(tabs))
                {
                    pageDictionary.Put(PdfName.Tabs, PdfName.S);
                    tabsFixed++;
                }
            }
        }

        if (options.HasFlag(PdfFixOptions.RemoveEmptyParagraphs))
        {
            PdfDictionary catalog = document.GetCatalog().GetPdfObject();
            PdfDictionary? structTreeRoot = catalog.GetAsDictionary(PdfName.StructTreeRoot);
            if (structTreeRoot is not null)
            {
                emptyParagraphsRemoved += RemoveEmptyParagraphsFromStructTreeRoot(structTreeRoot);
            }
        }

        bool changed = tabsFixed > 0 || emptyParagraphsRemoved > 0;
        if (!changed)
        {
            document.Close();
            DeleteIfExists(outputPath);
        }

        return changed;
    }

    private static int RemoveEmptyParagraphsFromStructTreeRoot(PdfDictionary structTreeRoot)
    {
        PdfObject? kids = structTreeRoot.Get(PdfName.K);
        if (kids is null)
        {
            return 0;
        }

        if (ShouldRemoveEmptyParagraph(kids, out int removedFromChildren))
        {
            structTreeRoot.Remove(PdfName.K);
            return removedFromChildren + 1;
        }

        return RemoveEmptyParagraphsFromKids(kids);
    }

    private static int RemoveEmptyParagraphsFromKids(PdfObject kids)
    {
        PdfObject resolvedKids = Resolve(kids);
        if (resolvedKids is PdfArray array)
        {
            int removed = 0;
            for (int i = array.Size() - 1; i >= 0; i--)
            {
                PdfObject? child = array.Get(i);
                if (ShouldRemoveEmptyParagraph(child, out int childRemoved))
                {
                    array.Remove(i);
                    removed++;
                }
                removed += childRemoved;
            }
            return removed;
        }

        return resolvedKids is PdfDictionary dict ? RemoveEmptyParagraphsInside(dict) : 0;
    }

    private static bool ShouldRemoveEmptyParagraph(PdfObject? obj, out int removedFromChildren)
    {
        removedFromChildren = 0;
        PdfObject? resolved = Resolve(obj);
        if (resolved is not PdfDictionary dictionary)
        {
            return false;
        }

        removedFromChildren = RemoveEmptyParagraphsInside(dictionary);
        return PdfName.P.Equals(dictionary.GetAsName(PdfName.S)) && IsStructElemEmpty(dictionary);
    }

    private static int RemoveEmptyParagraphsInside(PdfDictionary dictionary)
    {
        PdfObject? kids = dictionary.Get(PdfName.K);
        return kids is null ? 0 : RemoveEmptyParagraphsFromKids(kids);
    }

    private static bool IsStructElemEmpty(PdfDictionary dictionary)
    {
        PdfObject? kids = dictionary.Get(PdfName.K);
        if (kids is null)
        {
            return true;
        }

        PdfObject resolvedKids = Resolve(kids);
        return resolvedKids is PdfArray array && array.Size() == 0;
    }

    private static PdfObject Resolve(PdfObject? obj)
    {
        return obj is PdfIndirectReference reference ? reference.GetRefersTo() : obj!;
    }

    private static string CreateTemporaryPdfPath(string outputPath)
    {
        string directory = Path.GetDirectoryName(Path.GetFullPath(outputPath)) ?? Path.GetTempPath();
        System.IO.Directory.CreateDirectory(directory);
        return Path.Combine(directory, $"{Path.GetFileNameWithoutExtension(outputPath)}.{Guid.NewGuid():N}.tmp.pdf");
    }

    private static void DeleteIfExists(string? path)
    {
        if (path is not null && File.Exists(path))
        {
            File.Delete(path);
        }
    }

    private sealed record MetadataFixOutcome(bool Changed, RepairStatus Status, IReadOnlyList<string> AppliedFixes);

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
            Dictionary<(int Page, int Mcid), MarkedContentInfo> textIndex;
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
                Geometry: UnionGeometry(children.Select(child => child.Geometry)),
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
        Dictionary<(int Page, int Mcid), MarkedContentInfo> textIndex, int? inheritedPage, int depth)
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
        Dictionary<(int Page, int Mcid), MarkedContentInfo> textIndex, int? inheritedPage, int depth)
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
        var directGeometries = new List<StructureGeometry>();
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
            if (TryResolveContentInfo(child, ownPage, pageNumberByKey, textIndex, out MarkedContentInfo? info))
            {
                if (!string.IsNullOrEmpty(info!.Text))
                {
                    contentTexts.Add(info.Text);
                }
                if (info.Geometry is not null)
                {
                    directGeometries.Add(info.Geometry);
                }
            }

            formField ??= TryReadFormFieldInfo(child, ownPage);
        }

        string? content = contentTexts.Count > 0 ? string.Join(" ", contentTexts) : null;
        IReadOnlyList<StructureNode> children = WalkStructureChildren(elem, pageNumberByKey, textIndex, ownPage, depth + 1);
        StructureGeometry? geometry = UnionGeometry(directGeometries.Concat(children.Select(child => child.Geometry)));
        geometry ??= formField?.Geometry;
        geometry ??= TryReadLayoutBBoxGeometry(elem, ownPage);

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
            Geometry: geometry,
            Language: NullIfEmpty(elem.getLang()?.toString()),
            PageNumber: ownPage,
            ContentLeafCount: contentLeafCount,
            Children: children);
    }

    private sealed record FormFieldInfo(string? Name, string? Description, string? Type, string? Value, StructureGeometry? Geometry);

    private static FormFieldInfo? TryReadFormFieldInfo(object child, int? ownPage)
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
        StructureGeometry? geometry = TryReadWidgetGeometry(ownPage, referenced);
        return new FormFieldInfo(name, description, type, value, geometry);
    }

    private static StructureGeometry? TryReadWidgetGeometry(int? pageNumber, org.verapdf.cos.COSObject referenced)
    {
        var rect = referenced.getKey(Atom("Rect"));
        if (rect == null || rect.size().intValue() < 4)
        {
            return null;
        }

        double? left = NumberAt(rect, 0);
        double? bottom = NumberAt(rect, 1);
        double? right = NumberAt(rect, 2);
        double? top = NumberAt(rect, 3);
        if (left is null || bottom is null || right is null || top is null || pageNumber is null)
        {
            return null;
        }

        return new StructureGeometry(
            pageNumber.Value,
            Math.Min(left.Value, right.Value),
            Math.Min(bottom.Value, top.Value),
            Math.Max(left.Value, right.Value),
            Math.Max(bottom.Value, top.Value));
    }

    private static StructureGeometry? TryReadLayoutBBoxGeometry(org.verapdf.pd.structure.PDStructElem elem, int? pageNumber)
    {
        if (pageNumber is null)
        {
            return null;
        }

        var attributes = elem.getKey(Atom("A"));
        return TryReadBBoxFromAttributes(attributes, pageNumber.Value);
    }

    private static StructureGeometry? TryReadBBoxFromAttributes(org.verapdf.cos.COSObject? attributes, int pageNumber)
    {
        if (attributes == null || attributes.empty())
        {
            return null;
        }

        if (attributes.knownKey(Atom("BBox")).booleanValue())
        {
            return GeometryFromArray(pageNumber, attributes.getKey(Atom("BBox")));
        }

        int size = attributes.size()?.intValue() ?? 0;
        for (int i = 0; i < size; i++)
        {
            try
            {
                if (TryReadBBoxFromAttributes(attributes.at(i), pageNumber) is { } geometry)
                {
                    return geometry;
                }
            }
            catch (java.lang.Exception)
            {
                // Not an array-like attributes object; ignore and leave geometry unavailable.
                return null;
            }
        }

        return null;
    }

    private static StructureGeometry? GeometryFromArray(int pageNumber, org.verapdf.cos.COSObject? rect)
    {
        if (rect == null || rect.size().intValue() < 4)
        {
            return null;
        }

        double? left = NumberAt(rect, 0);
        double? bottom = NumberAt(rect, 1);
        double? right = NumberAt(rect, 2);
        double? top = NumberAt(rect, 3);
        if (left is null || bottom is null || right is null || top is null)
        {
            return null;
        }

        return new StructureGeometry(
            pageNumber,
            Math.Min(left.Value, right.Value),
            Math.Min(bottom.Value, top.Value),
            Math.Max(left.Value, right.Value),
            Math.Max(bottom.Value, top.Value));
    }

    private static double? NumberAt(org.verapdf.cos.COSObject array, int index)
    {
        var item = array.at(index);
        if (item == null)
        {
            return null;
        }

        if (item.getType()?.isNumber() == true)
        {
            var real = item.getReal();
            if (real != null)
            {
                return real.doubleValue();
            }
            var integer = item.getInteger();
            return integer?.doubleValue();
        }

        return null;
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
    private static bool TryResolveContentInfo(
        object child, int? ownPage, Dictionary<string, int> pageNumberByKey,
        Dictionary<(int Page, int Mcid), MarkedContentInfo> textIndex, out MarkedContentInfo? info)
    {
        info = null;

        if (child is org.verapdf.pd.structure.PDMCRDictionary mcr)
        {
            int? page = ownPage;
            var pageKey = mcr.getPageObjectKey();
            if (pageKey != null && pageNumberByKey.TryGetValue(pageKey.toString(), out int number))
            {
                page = number;
            }

            var mcid = mcr.getMCID();
            if (page is not null && mcid is not null && textIndex.TryGetValue((page.Value, (int)mcid.longValue()), out MarkedContentInfo? resolved))
            {
                info = resolved;
                return true;
            }
            return false;
        }

        if (child is org.verapdf.cos.COSObject cos && cos.getType()?.isNumber() == true && ownPage is not null)
        {
            var mcid = cos.getInteger();
            if (mcid is not null && textIndex.TryGetValue((ownPage.Value, (int)mcid.longValue()), out MarkedContentInfo? resolved))
            {
                info = resolved;
                return true;
            }
        }

        return false;
    }

    private static StructureGeometry? UnionGeometry(IEnumerable<StructureGeometry?> geometries)
    {
        var list = geometries.Where(geometry => geometry is not null).Cast<StructureGeometry>().ToList();
        if (list.Count == 0)
        {
            return null;
        }

        int pageNumber = list[0].PageNumber;
        if (list.Any(geometry => geometry.PageNumber != pageNumber))
        {
            return null;
        }

        return new StructureGeometry(
            pageNumber,
            list.Min(geometry => geometry.Left),
            list.Min(geometry => geometry.Bottom),
            list.Max(geometry => geometry.Right),
            list.Max(geometry => geometry.Top));
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
