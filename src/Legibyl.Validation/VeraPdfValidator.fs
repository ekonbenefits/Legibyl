namespace Legibyl.Validation

open System
open System.IO
open System.Runtime.InteropServices
open System.Threading
open org.verapdf.core
open org.verapdf.gf.foundry
open org.verapdf.pdfa
open org.verapdf.pdfa.flavours
open org.verapdf.pdfa.results
open Legibyl.Validation.Interop

module internal VeraPdf =
    let private initLock = obj ()
    let mutable private initialised = false

    let ensureInitialised () =
        if not initialised then
            lock initLock (fun () ->
                if not initialised then
                    VeraGreenfieldFoundryProvider.initialise ()
                    initialised <- true)

    let ensureExists (pdfPath: string) =
        if not (File.Exists pdfPath) then
            raise (VeraPdfValidationException $"File not found: {pdfPath}")

    /// Runs f, rethrowing veraPDF/Java failures as VeraPdfValidationException.
    let wrapErrors (failedTo: string) f =
        try
            f ()
        with
        | :? VeraPDFException as ex -> raise (VeraPdfValidationException($"{failedTo}: {ex.Message}", ex))
        | :? java.lang.Exception as ex -> raise (VeraPdfValidationException($"{failedTo}: {ex.getMessage ()}", ex))

    let toJavaFlavour profile =
        match profile with
        | PdfProfile.PdfUa1 -> PDFAFlavour.PDFUA_1
        | PdfProfile.PdfA1B -> PDFAFlavour.PDFA_1_B
        | PdfProfile.PdfA2B -> PDFAFlavour.PDFA_2_B
        | PdfProfile.PdfA2U -> PDFAFlavour.PDFA_2_U
        | PdfProfile.PdfA3B -> PDFAFlavour.PDFA_3_B
        | PdfProfile.PdfA4 -> PDFAFlavour.PDFA_4
        | _ -> raise (ArgumentOutOfRangeException(nameof profile, profile, "Unsupported PDF profile"))

    let private repairStatuses =
        [ MetadataFixerResult.RepairStatus.SUCCESS, RepairStatus.Success
          MetadataFixerResult.RepairStatus.FIX_ERROR, RepairStatus.FixError
          MetadataFixerResult.RepairStatus.WONT_FIX, RepairStatus.WontFix
          MetadataFixerResult.RepairStatus.NO_ACTION, RepairStatus.NoAction
          MetadataFixerResult.RepairStatus.ID_REMOVED, RepairStatus.IdRemoved ]

    let toRepairStatus (status: MetadataFixerResult.RepairStatus) =
        match repairStatuses |> List.tryFind (fun (javaStatus, _) -> javaStatus = status) with
        | Some(_, repairStatus) -> repairStatus
        | None -> raise (ArgumentOutOfRangeException(nameof status, string status, "Unknown veraPDF repair status"))

    let private determineFixOptions (message: string | null) =
        let text = Option.ofObj message |> Option.defaultValue ""
        let contains (value: string) = text.Contains(value, StringComparison.OrdinalIgnoreCase)
        if contains "Tabs" && contains "annotation" && contains "S" then
            PdfFixOptions.AnnotationTabs
        elif
            contains "metadata"
            || contains "PDF/UA Identification"
            || contains "PDF/A Identification"
            || contains "conformance level"
            || contains "XMP"
        then
            PdfFixOptions.Metadata
        else
            PdfFixOptions.None

    let toReport (pdfPath: string) profile (result: ValidationResult) : ValidationReport =
        let assertions =
            [ for item in javaSeq (result.getTestAssertions ()) do
                  let assertion = item :?> TestAssertion
                  let ruleId = assertion.getRuleId ()
                  // NB: assertion.getLocationContext() is a rarely-populated, model-object-specific
                  // "extra context" field (almost always null). The real per-failure tree-walk
                  // path (e.g. "root/Catalog[0]/StructTreeRoot[0]/.../TH[3]") — the same value
                  // veraPDF's own report writer uses — lives on assertion.getLocation().getContext().
                  { Specification = ruleId.getSpecification().ToString()
                    Clause = ruleId.getClause ()
                    TestNumber = ruleId.getTestNumber ()
                    Passed = (assertion.getStatus () = TestAssertion.Status.PASSED)
                    Message = assertion.getMessage ()
                    ErrorMessage = assertion.getErrorMessage ()
                    LocationContext = assertion.getLocation () |?> _.getContext()
                    Severity = FindingSeverity.Failure
                    FixOptions = determineFixOptions (assertion.getMessage ()) } ]

        { FilePath = pdfPath
          Profile = profile
          FlavourId = result.getPDFAFlavour().getId()
          IsCompliant = result.isCompliant ()
          TotalAssertions = result.getTotalAssertions ()
          Assertions = List.toArray (assertions @ StructureChecks.warnings pdfPath) }

    let createTemporaryPdfPath (outputPath: string) =
        let prefix =
            match Path.GetFileNameWithoutExtension outputPath with
            | NonNull prefix when not (String.IsNullOrWhiteSpace prefix) -> prefix
            | _ -> "legibyl"
        Path.Combine(Path.GetTempPath(), $"{prefix}.{Guid.NewGuid():N}.tmp.pdf")

    let deleteIfExists (path: string | null) =
        match path with
        | NonNull path when File.Exists path -> File.Delete path
        | _ -> ()

    /// Deletes a temporary file, retrying briefly in case another handle is still closing.
    let tryDeleteTemporary (path: string | null) =
        let rec attempt n (path: string) =
            try
                File.Delete path
            with
            | :? IOException
            | :? UnauthorizedAccessException when n < 4 ->
                Thread.Sleep 100
                attempt (n + 1) path
            | :? IOException
            | :? UnauthorizedAccessException -> ()

        match path with
        | NonNull path when File.Exists path -> attempt 0 path
        | _ -> ()

    type MetadataFixOutcome =
        { Changed: bool
          Status: RepairStatus
          AppliedFixes: string list }

    let fixMetadataOnly (pdfPath: string) profile (outputPath: string) =
        ensureExists pdfPath
        let flavour = toJavaFlavour profile
        let foundry = Foundries.defaultInstance ()
        wrapErrors $"veraPDF failed to fix '{pdfPath}'" (fun () ->
            use parser = foundry.createParser (java.io.File pdfPath, flavour)
            let result = foundry.createValidator(flavour, false).validate parser
            let fixResult =
                use outputStream = new java.io.FileOutputStream(outputPath)
                foundry.createMetadataFixer().fixMetadata (parser, outputStream, result)

            let status = toRepairStatus (fixResult.getRepairStatus ())

            // Only Success/IdRemoved reflect the fixer actually changing something; NoAction,
            // WontFix and FixError leave a repaired copy that's either empty or just a useless
            // byte-for-byte (or broken) duplicate of the original. Don't keep a file the user
            // didn't ask for and can't use — clean it up and report no output path.
            let wroteUsableOutput =
                (status = RepairStatus.Success || status = RepairStatus.IdRemoved)
                && File.Exists outputPath
                && FileInfo(outputPath).Length > 0L
            if not wroteUsableOutput && File.Exists outputPath then
                File.Delete outputPath

            { Changed = wroteUsableOutput
              Status = status
              AppliedFixes = [ for fix in javaSeq (fixResult.getAppliedFixes ()) -> fix :?> string ] })

    let rewritePdfWithIText (inputPath: string) (outputPath: string) =
        use reader = new iText.Kernel.Pdf.PdfReader(inputPath)
        use writer = new iText.Kernel.Pdf.PdfWriter(outputPath)
        use _document = new iText.Kernel.Pdf.PdfDocument(reader, writer)
        ()

open VeraPdf

/// IPdfValidator implementation backed by veraPDF's Greenfield Java validation core,
/// executed in-process via IKVM. All Java/IKVM types stay inside this assembly;
/// callers only see the plain .NET types in this namespace (ValidationReport,
/// RuleViolation, etc).
[<Sealed>]
type VeraPdfValidator() =
    do ensureInitialised ()

    member _.Validate(pdfPath: string, profile: PdfProfile) : ValidationReport =
        ensureExists pdfPath
        let flavour = toJavaFlavour profile
        let foundry = Foundries.defaultInstance ()
        wrapErrors $"veraPDF failed to validate '{pdfPath}'" (fun () ->
            use parser = foundry.createParser (java.io.File pdfPath, flavour)
            let result = foundry.createValidator(flavour, true).validate parser
            toReport pdfPath profile result)

    member this.Fix
        (
            pdfPath: string,
            profile: PdfProfile,
            outputPath: string,
            [<Optional; DefaultParameterValue(PdfFixOptions.Metadata)>] options: PdfFixOptions
        ) : FixResult =
        ensureExists pdfPath
        if options = PdfFixOptions.None then
            raise (VeraPdfValidationException "At least one fix option must be selected.")

        let originalReport = this.Validate(pdfPath, profile)
        let appliedFixes = ResizeArray<string>()
        let mutable currentInput = pdfPath
        let mutable tempMetadataOutput: string | null = null
        let mutable tempStructuralOutput: string | null = null
        let mutable changed = false
        let mutable status = RepairStatus.NoAction

        try
            try
                if options.HasFlag PdfFixOptions.AnnotationTabs || options.HasFlag PdfFixOptions.RemoveEmptyParagraphs then
                    let tempOutput = createTemporaryPdfPath outputPath
                    tempStructuralOutput <- tempOutput
                    match StructureChecks.applyFixes currentInput tempOutput options with
                    | 0, 0 ->
                        tryDeleteTemporary tempOutput
                        tempStructuralOutput <- null
                    | tabsFixed, emptyParagraphsRemoved ->
                        if tabsFixed > 0 then
                            appliedFixes.Add $"Set /Tabs /S on {tabsFixed} page(s) containing annotations."
                        if emptyParagraphsRemoved > 0 then
                            appliedFixes.Add $"Removed {emptyParagraphsRemoved} empty /P structure element(s)."
                        currentInput <- tempOutput
                        changed <- true
                        status <- RepairStatus.Success

                if options.HasFlag PdfFixOptions.Metadata then
                    let tempOutput = createTemporaryPdfPath outputPath
                    tempMetadataOutput <- tempOutput
                    let metadataFix = fixMetadataOnly currentInput profile tempOutput
                    appliedFixes.AddRange metadataFix.AppliedFixes
                    if metadataFix.Changed then
                        currentInput <- tempOutput
                        changed <- true
                        status <- metadataFix.Status
                    else
                        if not changed then
                            status <- metadataFix.Status
                        tryDeleteTemporary tempOutput
                        tempMetadataOutput <- null

                let result =
                    { FilePath = pdfPath
                      Profile = profile
                      RequestedOptions = options
                      OutputPath = null
                      Status = status
                      AppliedFixes = appliedFixes
                      OriginalReport = originalReport }

                if not changed then
                    deleteIfExists outputPath
                    result
                else
                    this.FinalizeFixedPdf(currentInput, outputPath, profile)
                    { result with OutputPath = outputPath }
            with
            | :? iText.Kernel.Exceptions.PdfException as ex ->
                raise (VeraPdfValidationException($"iText failed to fix '{pdfPath}': {ex.Message}", ex))
            | :? IOException as ex ->
                raise (VeraPdfValidationException($"Could not write fixed PDF '{outputPath}': {ex.Message}", ex))
        finally
            for tempOutput in [ tempMetadataOutput; tempStructuralOutput ] do
                if not (String.Equals(tempOutput, outputPath, StringComparison.OrdinalIgnoreCase)) then
                    tryDeleteTemporary tempOutput

    member private this.FinalizeFixedPdf(inputPath: string, outputPath: string, profile: PdfProfile) =
        Directory.CreateDirectory(nonNull (Path.GetDirectoryName(Path.GetFullPath outputPath))) |> ignore
        rewritePdfWithIText inputPath outputPath
        try
            this.Validate(outputPath, profile) |> ignore
        with _ ->
            deleteIfExists outputPath
            reraise ()

    /// Reads the structure tree by opening the PDF a second time through veraPDF's
    /// plain PD model (org.verapdf.pd.PDDocument) — the same model the validator
    /// itself uses internally. This is intentionally independent of Validate / Fix
    /// (no shared parser/session state), since reading the tagged structure is a
    /// distinct concern from PDF/A|UA rule validation and callers may want it without
    /// running a full validation pass.
    member _.ReadStructureTree(pdfPath: string) : StructureNode | null =
        ensureExists pdfPath
        wrapErrors $"veraPDF failed to read the structure tree of '{pdfPath}'" (fun () -> StructureTreeWalk.read pdfPath)

    interface IPdfValidator with
        member this.Validate(pdfPath, profile) = this.Validate(pdfPath, profile)

    interface IPdfFixer with
        member this.Fix(pdfPath, profile, outputPath, options) = this.Fix(pdfPath, profile, outputPath, options)

    interface IStructureTreeReader with
        member this.ReadStructureTree pdfPath = this.ReadStructureTree pdfPath
