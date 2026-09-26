module Legibyl.Cli.Program

open System
open System.CommandLine
open System.CommandLine.Help
open System.CommandLine.Invocation
open System.CommandLine.Parsing
open System.IO
open System.Reflection
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open FSharp.SystemCommandLine
open Input
open Legibyl.Validation

/// Process exit codes. Designed so an agent/script can distinguish "the PDF has
/// compliance problems" (1) from "the tool itself failed to run" (2/64).
module ExitCodes =
    /// All requested files validated and are compliant.
    [<Literal>]
    let Success = 0

    /// All files were validated successfully, but at least one is non-compliant.
    [<Literal>]
    let NonCompliant = 1

    /// At least one file could not be validated (missing, corrupt, encrypted, etc).
    [<Literal>]
    let ValidationError = 2

    /// Invalid command-line usage (bad arguments, unknown profile, no files given).
    [<Literal>]
    let UsageError = 64 // matches sysexits.h EX_USAGE convention

type Args =
    { Files: string array
      ProfileName: string
      Format: string
      OutputPath: string option
      Fix: bool
      /// The repairs --fixes names, as given.
      Fixes: string option
      /// Apply the edits in each file's own review (the file next to it, or inside it).
      Edits: bool
      /// Apply the edits in this review (a .legibyl.json file, or a PDF's own review).
      EditsFrom: string option
      /// Suggest each file's layout, in the output.
      SuggestLayout: bool
      /// Also write the suggested layout here (one input file).
      SuggestLayoutOutput: string option
      /// Write a picture of each figure beside the layout written there.
      FigureImages: bool
      /// Tag the file from the layout in this file (see --suggest-layout).
      ApplyLayout: string option
      TitleFromFirstPage: bool
      LinkTitlesFromWeb: bool
      FixOutputPath: string option
      IncludeStructure: bool }

let private usageError (message: string) =
    eprintfn $"error: {message}"
    eprintfn "Run with --help for usage."
    ExitCodes.UsageError

/// Whether the arguments ask for a repaired copy: --fix, or what implies it.
let private fixing (args: Args) =
    args.Fix || args.Fixes.IsSome || args.Edits || args.EditsFrom.IsSome || args.ApplyLayout.IsSome

/// The repairs asked for: those --fixes names (the safe ones with just --fix), and the ones
/// their own flags ask for. Edits are added per file, when it has some.
let private requestedFixes (args: Args) =
    let named =
        match args.Fixes with
        | Some names -> FixNames.parse names |> Result.defaultValue PdfFixOptions.None
        | None when args.Fix -> FixNames.safe
        | None -> PdfFixOptions.None
    [ args.TitleFromFirstPage, PdfFixOptions.TitleFromFirstPage
      args.LinkTitlesFromWeb, PdfFixOptions.LinkTitlesFromWeb ]
    |> List.fold (fun all (asked, option) -> if asked then all ||| option else all) named

/// Downloads the layout model if it isn't yet, saying how it's going on stderr.
let private ensureLayoutModel () =
    if not (Legibyl.Layout.LayoutModel.isDownloaded ()) then
        eprintfn $"Downloading the layout model (about {Legibyl.Layout.LayoutModel.DownloadSizeMegabytes} MB), once…"
        let mutable shown = -1
        let progress =
            { new IProgress<float> with
                member _.Report fraction =
                    let percent = int (fraction * 10.0) * 10
                    if percent > shown then
                        shown <- percent
                        eprintfn $"  {percent}%%" }
        Legibyl.Layout.LayoutModel.download progress Threading.CancellationToken.None |> _.GetAwaiter().GetResult()

let private deriveFixOutputPath (file: string) =
    let fixedName = CopyNames.nextVersion file
    match Path.GetDirectoryName file with
    | NonNull dir when dir <> "" -> Path.Combine(dir, fixedName)
    | _ -> fixedName

/// The layout the model suggests for each of file's pages, with a title, saying which page it's
/// on as it goes (each takes seconds).
let private suggestLayout (file: string) =
    ensureLayoutModel ()
    use suggester = new Legibyl.Layout.LayoutSuggester(file)
    let pages =
        [ for page in 1 .. suggester.PageCount ->
              eprintfn $"Finding page {page} of {suggester.PageCount}'s layout…"
              suggester.Suggest page ]
    Legibyl.Layout.LayoutSuggestions.toLayoutFile file pages (Legibyl.Layout.LayoutSuggestions.suggestedTitle file pages)

/// appliedLayout is --apply-layout's layout, read and checked.
let private validateOne (validator: VeraPdfValidator) (args: Args) (appliedLayout: LayoutFile.SavedLayout option) profile (file: string) =
    let result = FileValidationResult.Empty(file, args.ProfileName)
    match (try Ok(validator.Validate(file, profile)) with :? VeraPdfValidationException as ex -> Error ex.Message) with
    | Error message -> { result with Error = message }
    | Ok report ->
        let result =
            { result with
                FlavourId = report.FlavourId
                IsCompliant = Nullable report.IsCompliant
                TotalAssertions = Nullable report.TotalAssertions
                FailedCount = Nullable report.FailedCount
                Failures = report.FailedAssertions |> Seq.map FailureDetail.Of |> Array.ofSeq
                Warnings =
                    report.Assertions
                    |> Seq.filter (fun a -> a.Severity = FindingSeverity.Warning)
                    |> Seq.map FailureDetail.Of
                    |> Array.ofSeq }

        let result =
            if not (fixing args) then
                result
            else
                let targetPath = args.FixOutputPath |> Option.defaultWith (fun () -> deriveFixOutputPath file)
                // The review whose edits to apply, if asked for.
                let review =
                    match args.EditsFrom with
                    | Some path -> ReviewFile.readFrom path
                    | None when args.Edits -> ReviewFile.readFor file
                    | None -> None
                let noReview =
                    [ if (args.Edits || args.EditsFrom.IsSome) && review.IsNone then
                          match args.EditsFrom with
                          | Some path -> $"No edits were applied: {path} has no review to read them from."
                          | None -> $"No edits were applied: there's no {Path.GetFileName(ReviewFile.filePath file)} next to it, and no review inside it." ]
                // A review of an untagged PDF can hold its reviewed layout, to tag from.
                let reviewLayout = review |> Option.bind (fun review -> Option.ofObj review.Layout)
                let options =
                    requestedFixes args
                    ||| (if appliedLayout.IsSome || reviewLayout.IsSome then PdfFixOptions.TagFromLayout else PdfFixOptions.None)
                    ||| (match review with
                         | Some review when not (ReviewFile.edits review).IsEmpty -> PdfFixOptions.ApplyEdits
                         | _ -> PdfFixOptions.None)
                if options = PdfFixOptions.None then
                    { result with
                        FixStatus = RepairStatus.NoAction.ToString()
                        FixNotApplied = if noReview.IsEmpty then null else withNull (ResizeArray noReview) }
                else
                    // The layout model, for the repairs that read the page's layout, unless a layout
                    // given or reviewed has every page.
                    let pageCount () =
                        use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfReader(file))
                        document.GetNumberOfPages()
                    let needsModel =
                        FixNames.usesLayout options
                        && (match appliedLayout |> Option.orElse reviewLayout with
                            | Some layout -> not (LayoutFile.covers layout (pageCount ()))
                            | None -> true)
                    let model =
                        if needsModel then
                            ensureLayoutModel ()
                            Some(new Legibyl.Layout.LayoutRegionSource(null))
                        else
                            None
                    let modelSource = model |> Option.map (fun model -> model :> IPageLayoutSource)
                    let layout: IPageLayoutSource option =
                        match appliedLayout, review with
                        | Some saved, _ -> Some(LayoutFile.FileRegions(saved, modelSource))
                        | None, Some review when FixNames.usesLayout options ->
                            Some(ReviewFile.ReviewedRegions(modelSource |> Option.defaultValue ReviewFile.noRegions, review))
                        | None, _ -> modelSource
                    try
                        try
                            layout |> Option.iter (fun source -> validator.LayoutSource <- source)
                            match review with
                            | Some review ->
                                validator.Edits <- ReviewFile.edits review
                                validator.Review <- ReviewFile.forCopy (Some review)
                            | None -> ()
                            let fix = validator.Fix(file, profile, targetPath, options)
                            let tagged = fix.AppliedFixes |> Seq.exists (fun line -> line.StartsWith "Tagged ")
                            let layoutUnused =
                                [ if appliedLayout.IsSome && not tagged then
                                      "The layout wasn't applied: the file already has a structure tree, and tagging from a layout is for an untagged file." ]
                            let notApplied = List.ofSeq fix.NotApplied @ noReview @ layoutUnused
                            { result with
                                FixStatus = fix.Status.ToString()
                                FixOutputPath = fix.OutputPath
                                FixAppliedFixes = if fix.AppliedFixes.Count > 0 then withNull fix.AppliedFixes else null
                                FixNotApplied = if notApplied.IsEmpty then null else withNull (ResizeArray notApplied) }
                        with
                        | :? VeraPdfValidationException as ex -> { result with FixError = ex.Message }
                        | :? Net.Http.HttpRequestException as ex -> { result with FixError = $"Couldn't download the layout model: {ex.Message}" }
                    finally
                        validator.LayoutSource <- null
                        validator.Edits <- []
                        validator.Review <- null
                        model |> Option.iter (fun model -> (model :> IDisposable).Dispose())

        let result =
            if not args.IncludeStructure then
                result
            else
                try
                    { result with Structure = validator.ReadStructureTree file }
                with :? VeraPdfValidationException as ex ->
                    { result with StructureError = ex.Message }

        if not args.SuggestLayout then
            result
        else
            try
                let layout = suggestLayout file
                let layout =
                    match args.SuggestLayoutOutput with
                    | Some path ->
                        let layout =
                            if args.FigureImages then
                                use renderer = new Legibyl.Rendering.PdfRegionRenderer()
                                LayoutFile.withFigureImages path (fun geometry -> renderer.RenderRegion(file, geometry)) layout
                            else
                                layout
                        File.WriteAllText(path, LayoutFile.toJson layout)
                        layout
                    | None -> layout
                { result with SuggestedLayout = layout }
            with
            | :? Net.Http.HttpRequestException as ex -> { result with SuggestedLayoutError = $"Couldn't download the layout model: {ex.Message}" }
            | :? IOException
            | :? UnauthorizedAccessException as ex -> { result with SuggestedLayoutError = ex.Message }

let private renderJson (results: FileValidationResult list) =
    let options =
        JsonSerializerOptions(
            WriteIndented = true,
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
            DefaultIgnoreCondition = JsonIgnoreCondition.Never
        )
    JsonSerializer.Serialize(results, options)

let rec private appendStructure (sb: StringBuilder) (node: StructureNode) (indent: int) =
    let pad = String(' ', indent)
    let parts =
        [ yield $"<{node.Tag}>"
          match node.Role with NonNull role -> yield $"role={role}" | Null -> ()
          if node.PageNumber.HasValue then yield $"page={node.PageNumber.Value}"
          match node.Language with NonNull lang -> yield $"lang={lang}" | Null -> ()
          match node.AlternateDescription with NonNull alt -> yield $"alt=\"{alt}\"" | Null -> ()
          match node.ActualText with NonNull text -> yield $"actualText=\"{text}\"" | Null -> ()
          match node.FormFieldName with NonNull name -> yield $"field=\"{name}\"" | Null -> ()
          match node.FormFieldType with NonNull fieldType -> yield $"fieldType={fieldType}" | Null -> ()
          match node.FormFieldDescription with NonNull tooltip -> yield $"tooltip=\"{tooltip}\"" | Null -> ()
          match node.FormFieldValue with NonNull value -> yield $"value=\"{value}\"" | Null -> ()
          match node.LinkUri with NonNull href -> yield $"href=\"{href}\"" | Null -> ()
          match node.LinkDestinationDescription with NonNull dest -> yield $"dest=\"{dest}\"" | Null -> ()
          if node.ContentLeafCount > 0 then yield $"content×{node.ContentLeafCount}" ]

    sb.AppendLine(pad + String.Join(' ', parts)) |> ignore
    match node.Content with
    | NonNull content -> sb.AppendLine(pad + $"  text=\"{content}\"") |> ignore
    | Null -> ()

    for child in node.Children do
        appendStructure sb child (indent + 2)

let private renderText (results: FileValidationResult list) =
    let sb = StringBuilder()
    let line (text: string) = sb.AppendLine text |> ignore
    let fixableSuffix (detail: FailureDetail) =
        if detail.IsFixable then $" (fixable: {detail.FixOptions})" else ""
    let details (items: FailureDetail seq | null) = match items with NonNull items -> items | Null -> Seq.empty

    for r in results do
        match r.Error with
        | NonNull error -> line $"{r.File}: ERROR — {error}"
        | Null ->
            line (
                if r.IsCompliant = Nullable true then
                    $"{r.File}: PASS ({r.Profile}, {r.TotalAssertions} checks)"
                else
                    $"{r.File}: FAIL ({r.Profile}, {r.FailedCount}/{r.TotalAssertions} checks failed)"
            )

            for prefix, items in [ "", r.Failures; "WARNING ", r.Warnings ] do
                for f in details items do
                    line $"  {prefix}[{f.Specification} {f.Clause}-{f.TestNumber}] {f.Message}{fixableSuffix f}"
                    if not (String.IsNullOrEmpty f.LocationContext) then
                        line $"    at: {f.LocationContext}"
                    match f.Region with
                    | NonNull region -> line $"    region: {ValidationReportTextFormatter.FormatRegion region}"
                    | Null -> ()

            match r.FixError, r.FixStatus with
            | NonNull error, _ -> line $"  fix: ERROR — {error}"
            | Null, NonNull status ->
                match r.FixOutputPath with
                | NonNull path -> line $"  fix: {status} → {path}"
                | Null -> line $"  fix: {status} (no output written)"
                match r.FixAppliedFixes with
                | NonNull applied -> for fix in applied do line $"    - {fix}"
                | Null -> ()
                match r.FixNotApplied with
                | NonNull notApplied -> for fix in notApplied do line $"    ! {fix}"
                | Null -> ()
            | Null, Null -> ()

            match r.SuggestedLayoutError, r.SuggestedLayout with
            | NonNull error, _ -> line $"  suggested layout: ERROR — {error}"
            | Null, NonNull layout ->
                line "  suggested layout:"
                match layout.Title with
                | NonNull title -> line $"    title: \"{title}\""
                | Null -> ()
                for page in layout.Pages do
                    line $"    page {page.Page}:"
                    for region in page.Regions do
                        let text =
                            match region.QrText, region.Text with
                            | NonNull qr, _ -> $" QR: {qr}"
                            | _, NonNull text when text <> "" -> $" \"{text}\""
                            | _ -> ""
                        let recognized = if region.Recognized then " (OCR)" else ""
                        line $"      {region.Order} {region.Tag}{recognized}{text}"
                        match region.Image with
                        | NonNull image -> line $"        image: {image}"
                        | Null -> ()
            | Null, Null -> ()

            match r.StructureError, r.Structure with
            | NonNull error, _ -> line $"  structure: ERROR — {error}"
            | Null, NonNull structure ->
                line "  structure:"
                appendStructure sb structure 4
            | Null, Null -> ()

    sb.ToString().TrimEnd('\n', '\r')

let private run (args: Args) =
    if args.Files.Length = 0 then
        usageError "No input PDF files given."
    elif args.FixOutputPath.IsSome && not (fixing args) then
        usageError "--fix-output requires --fix (or --fixes or --edits)."
    elif args.TitleFromFirstPage && not (fixing args) then
        usageError "--title-from-first-page requires --fix."
    elif args.LinkTitlesFromWeb && not (fixing args) then
        usageError "--link-titles-from-web requires --fix."
    elif args.Fixes |> Option.exists (fun names -> Result.isError (FixNames.parse names)) then
        match FixNames.parse args.Fixes.Value with
        | Error message -> usageError message
        | Ok _ -> ExitCodes.UsageError
    elif args.Edits && args.EditsFrom.IsSome then
        usageError "Use --edits or --edits-from, not both."
    elif args.SuggestLayoutOutput.IsSome && not args.SuggestLayout then
        usageError "--suggest-layout-output requires --suggest-layout."
    elif args.FigureImages && args.SuggestLayoutOutput.IsNone then
        usageError "--figure-images requires --suggest-layout-output: the images go in a folder beside that file."
    elif args.SuggestLayoutOutput.IsSome && args.Files.Length > 1 then
        usageError "--suggest-layout-output can only be used with a single input file."
    elif args.ApplyLayout.IsSome && args.Files.Length > 1 then
        usageError "--apply-layout can only be used with a single input file: a layout is for one document."
    elif args.FixOutputPath.IsSome && args.Files.Length > 1 then
        usageError "--fix-output can only be used with a single input file."
    else
        // The --profile validator has already rejected unknown names.
        let profile = (ProfileNames.tryParse args.ProfileName).Value
        let validator = VeraPdfValidator()
        match args.ApplyLayout |> Option.map LayoutFile.read with
        | Some(Error message) -> usageError message
        | appliedLayout ->
            let appliedLayout = appliedLayout |> Option.bind Result.toOption
            let results = [ for file in args.Files -> validateOne validator args appliedLayout profile file ]
            let rendered = if args.Format = "json" then renderJson results else renderText results

            match args.OutputPath with
            | Some path -> File.WriteAllText(path, rendered)
            | None -> Console.WriteLine rendered

            if results |> List.exists (fun r -> not (isNull r.Error)) then ExitCodes.ValidationError
            elif results |> List.exists (fun r -> r.IsCompliant = Nullable false) then ExitCodes.NonCompliant
            else ExitCodes.Success

/// Runs a function in place of a command's action, e.g. for --list-profiles or --version.
type private InvokeAction(invoke: ParseResult -> int) =
    inherit SynchronousCommandLineAction()
    override _.Invoke(parseResult) = invoke parseResult

let private helpFooter =
    """EXIT CODES:
  0   all files validated and are compliant.
  1   all files validated successfully, but at least one is non-compliant.
  2   at least one file could not be validated (missing/corrupt/encrypted).
  64  invalid command-line usage.

EXAMPLES:
  legibyl document.pdf
  legibyl document.pdf --profile pdfa1b --format text
  legibyl *.pdf --output results.json
  legibyl document.pdf --fix
  legibyl document.pdf --fixes safe,lists,headings-from-toc
  legibyl document.pdf --fixes all --edits
  legibyl flyer.v3.pdf --edits-from flyer.v2.pdf
  legibyl scan.pdf --suggest-layout --suggest-layout-output scan.layout.json --figure-images
  legibyl scan.pdf --apply-layout scan.layout.json
  legibyl document.pdf --fix --fix-output document-repaired.pdf
  legibyl document.pdf --structure --output structure.json
"""

let private listProfiles _ =
    ProfileNames.CanonicalNames |> List.iter (printfn "%s")
    ExitCodes.Success

let private listFixes _ =
    FixNames.listing () |> List.iter (printfn "%s")
    ExitCodes.Success

let private printVersion _ =
    printfn $"legibyl {Assembly.GetExecutingAssembly().GetName().Version} (vendored veraPDF engine {ProfileNames.VendoredEngineVersion})"
    ExitCodes.Success

/// Reports a parse error the same way as the CLI's other usage errors.
let private reportParseError (parseResult: ParseResult) =
    let error = parseResult.Errors[0]
    match error.SymbolResult with
    | :? OptionResult as option when option.Tokens.Count = 0 -> usageError $"{option.Option.Name} requires a value."
    | _ -> usageError error.Message

[<EntryPoint>]
let main argv =
    let root =
        ManualInvocation.rootCommand {
            description
                "legibyl - PDF/UA & PDF/A validator (CLI) powered by veraPDF via IKVM.\n\n\
                 Designed for scripting and AI-agent use: default output is machine-readable\n\
                 JSON on stdout, and the process exit code tells you the outcome without\n\
                 needing to parse output."

            input {
                let! files =
                    argument<string array> "files"
                    |> desc "PDF files to validate."
                    |> arity Arity.ZeroOrMore
                    |> validate (fun files ->
                        match files |> Array.tryFind (fun f -> f.StartsWith '-' && f.Length > 1) with
                        | Some unknown -> Error $"Unknown option '{unknown}'."
                        | None -> Ok())

                and! profileName =
                    option "--profile"
                    |> desc "Profile to validate against. See --list-profiles for all accepted names."
                    |> helpName "name"
                    |> def ProfileNames.DefaultName
                    |> validate (fun name ->
                        match ProfileNames.tryParse name with
                        | Some _ -> Ok()
                        | None -> Error $"Unknown profile '{name}'. Run --list-profiles to see valid names.")

                and! format =
                    option "--format"
                    |> desc "Output format."
                    |> helpName "json|text"
                    |> def "json"
                    |> validate (fun format ->
                        if format = "json" || format = "text" then Ok()
                        else Error $"Unknown --format '{format}'. Expected 'json' or 'text'.")

                and! outputPath =
                    optionMaybe "--output"
                    |> desc "Write output to a file instead of stdout."
                    |> helpName "path"

                and! fix =
                    option<bool> "--fix"
                    |> desc
                        "Also attempt to repair each file, writing a repaired copy next to the original as its \
                         next version: \"<name>.v2<ext>\" (\"<name>.v3<ext>\" for \"<name>.v2<ext>\"), skipping \
                         versions that already exist. Applies the safe repairs, which need no guesswork about \
                         what the document means: XMP metadata (missing metadata stream, missing PDF/A|PDF/UA \
                         identifier, false conformance claims, Info-dictionary sync); /Tabs /S on pages \
                         containing annotations; the glyph mapping embedded TrueType CID fonts leave out; a \
                         description for links that have none, taken from the link's own text; and a document \
                         title shown in place of the file name. --fixes chooses others."

                and! fixes =
                    optionMaybe "--fixes"
                    |> desc
                        "Repair each file (implies --fix) with the repairs named, comma-separated: \"safe\" \
                         (what --fix applies), \"all\" (every repair but link-titles-from-web, which visits \
                         web pages), or names from --list-fixes, e.g. \"safe,lists,headings-from-toc\". \
                         tag-from-layout and join-split-paragraphs download the layout model (about 130 MB) the \
                         first time."
                    |> helpName "names"

                and! edits =
                    option<bool> "--edits"
                    |> desc
                        "Repair each file (implies --fix) with the edits in its review: the \
                         \"<name>.pdf.legibyl.json\" file next to it (as the GUI saves it), or else the review \
                         inside a copy Legibyl saved. Alt text, replacement text and merged paragraphs are \
                         written to the elements they're for; an untagged file tagged with tag-from-layout is \
                         tagged from the review. The repaired copy carries the review."

                and! editsFrom =
                    optionMaybe "--edits-from"
                    |> desc
                        "Like --edits, with the review in path: a .legibyl.json file, or a PDF whose review to \
                         use, e.g. an earlier version's."
                    |> helpName "path"

                and! titleFromFirstPage =
                    option<bool> "--title-from-first-page"
                    |> desc
                        "Use with --fix. When a file carries no title in Info or XMP, title it from the text \
                         of its first page, if that page is short enough to be a title page (at most 30 words) \
                         and more pages follow."

                and! linkTitlesFromWeb =
                    option<bool> "--link-titles-from-web"
                    |> desc
                        "Use with --fix. Describe links to web pages that have no description by the title of \
                         the page each goes to, with its site (\"Apply for benefits (example.com)\"). This visits \
                         those addresses: only public internet ones, reading only their titles, but the sites see \
                         the visit. Check the descriptions."

                and! fixOutputPath =
                    optionMaybe "--fix-output"
                    |> desc
                        "Use with --fix and exactly one input file to write the repaired copy to an exact \
                         path instead of the auto-derived name."
                    |> helpName "path"

                and! suggestLayout =
                    option<bool> "--suggest-layout"
                    |> desc
                        "Include the layout the layout model suggests for each file's pages in the output, \
                         under \"suggestedLayout\": each page's regions in reading order, with the tag each \
                         suggests (H1, H, P, LI, Figure, Table, Caption, Note, Formula, Lbl, Artifact), its box, \
                         its text (read by OCR where it's drawn rather than written), what a QR code encodes, an \
                         \"alt\" to fill in for a figure, and a title. Change it as needed (retag, remove or \
                         reorder regions, write alt text, correct OCR'd text) and tag the file from it with \
                         --apply-layout. Downloads the layout model (about 130 MB) the first time; several \
                         seconds a page."

                and! suggestLayoutOutput =
                    optionMaybe "--suggest-layout-output"
                    |> desc "Use with --suggest-layout and one input file to also write the suggested layout to path, ready for --apply-layout."
                    |> helpName "path"

                and! figureImages =
                    option<bool> "--figure-images"
                    |> desc
                        "Use with --suggest-layout-output. Also writes a PNG of each figure into a folder \
                         beside the layout file (\"scan.layout.json\" -> \"scan.layout.images/p1-r3.png\"), \
                         and gives the figure its path as \"image\", for someone (or an agent) to look at \
                         when writing its alt text. Each run empties the folder of the last run's images; \
                         delete it with the layout file when done. --apply-layout doesn't use them."

                and! applyLayout =
                    optionMaybe "--apply-layout"
                    |> desc
                        "Tag an untagged file from the layout in path (implies --fix; one input file), as \
                         --suggest-layout writes it: its regions become the structure, in their order, with \
                         their alt text, recognized text as replacement text, and its title. Needs no layout \
                         model."
                    |> helpName "path"

                and! includeStructure =
                    option<bool> "--structure"
                    |> desc
                        "Include each file's tagged logical structure tree (the semantic \
                         heading/paragraph/list/table/link hierarchy that screen readers use) in the output. \
                         In text format this prints an indented outline; in JSON (default) it's the full \
                         tree, ideal for feeding to an AI agent or other tooling."

                and! _ =
                    option<bool> "--list-profiles"
                    |> desc "Print supported profile names and exit 0."
                    |> editOption (fun o -> o.Action <- InvokeAction listProfiles)

                and! _ =
                    option<bool> "--list-fixes"
                    |> desc "Print the repairs --fixes can name, with what each does, and exit 0."
                    |> editOption (fun o -> o.Action <- InvokeAction listFixes)

                return
                    { Files = files
                      ProfileName = profileName
                      Format = format
                      OutputPath = outputPath
                      Fix = fix
                      Fixes = fixes
                      Edits = edits
                      EditsFrom = editsFrom
                      SuggestLayout = suggestLayout
                      SuggestLayoutOutput = suggestLayoutOutput
                      FigureImages = figureImages
                      ApplyLayout = applyLayout
                      TitleFromFirstPage = titleFromFirstPage
                      LinkTitlesFromWeb = linkTitlesFromWeb
                      FixOutputPath = fixOutputPath
                      IncludeStructure = includeStructure }
            }

            setAction run
        }

    for option in root.Options do
        match option with
        | :? VersionOption as version ->
            version.Description <- "Print tool + vendored veraPDF engine version and exit 0."
            version.Action <- InvokeAction printVersion
        | :? HelpOption as help ->
            match help.Action with
            | :? SynchronousCommandLineAction as showHelp ->
                help.Action <-
                    InvokeAction(fun parseResult ->
                        let exitCode = showHelp.Invoke parseResult
                        Console.Write helpFooter
                        exitCode)
            | _ -> ()
        | _ -> ()

    // Treat arguments starting with '@' as file names, not response files.
    let parseResult = root.Parse(argv, ParserConfiguration(ResponseFileTokenReplacer = null))
    match parseResult.Action with
    | :? ParseErrorAction -> reportParseError parseResult
    | _ ->
        // Let unexpected exceptions crash as before rather than exit 1, which means "non-compliant".
        parseResult.Invoke(InvocationConfiguration(EnableDefaultExceptionHandler = false))
