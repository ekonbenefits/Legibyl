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

/// Process exit codes, the same for every command: 0 done and fine, 1 done but the document has a
/// problem (it doesn't comply, has no structure, was already tagged), 2 the tool couldn't do it
/// (a file missing, corrupt, encrypted), 64 invalid usage.
module ExitCodes =
    [<Literal>]
    let Success = 0

    [<Literal>]
    let DocumentProblem = 1

    [<Literal>]
    let ToolError = 2

    [<Literal>]
    let UsageError = 64 // matches sysexits.h EX_USAGE convention

let private usageError (message: string) =
    eprintfn $"error: {message}"
    eprintfn "Run with --help for usage."
    ExitCodes.UsageError

// Relaxed escaping keeps text readable ("page's", not "page\u0027s").
let private jsonOptions =
    JsonSerializerOptions(
        WriteIndented = true,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        DefaultIgnoreCondition = JsonIgnoreCondition.Never,
        Encoder = Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping
    )

let private toJson (value: 'T) = JsonSerializer.Serialize(value, jsonOptions)

/// Writes rendered output to path, or stdout if none.
let private write (path: string option) (rendered: string) =
    match path with
    | Some path -> File.WriteAllText(path, rendered)
    | None -> Console.WriteLine rendered

/// The worst of several exit codes (a tool error over a document problem over success).
let private worst (codes: int seq) = if Seq.isEmpty codes then ExitCodes.Success else Seq.max codes

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

// ---- Shared inputs ----

let private filesArgument () =
    argument<string array> "files"
    |> desc "PDF files."
    |> arity Arity.OneOrMore
    |> validate (fun files ->
        match files |> Array.tryFind (fun f -> f.StartsWith '-' && f.Length > 1) with
        | Some unknown -> Error $"Unknown option '{unknown}'."
        | None -> Ok())

let private fileArgument () = argument<string> "file" |> desc "A PDF file."

let private uaOption () =
    optionMaybe<string> "--ua"
    |> desc
        "Check every file against PDF/UA-1 or PDF/UA-2. Without it, each file is checked against the one its PDF \
         version calls for: PDF/UA-2 for PDF 2.0, PDF/UA-1 for earlier versions. PDF/UA-2 requires PDF 2.0, so a \
         PDF 1.7 file checked with --ua 2 fails on its version alone (a way to see how far it is from PDF/UA-2)."
    |> helpName "1|2"
    |> arity Arity.ExactlyOne
    |> validate (fun ua ->
        match ua with
        | None
        | Some "1"
        | Some "2" -> Ok()
        | Some other -> Error $"--ua takes 1 or 2, not '{other}'.")

/// The PDF/UA standard to check file against: --ua's, else the one its PDF version calls for.
let private targetFor (ua: string option) (file: string) : Target =
    let _, version = PdfUaTarget.detect file
    match ua with
    | Some "2" -> { Profile = PdfProfile.PdfUa2; PdfVersion = Option.toObj version; FromFile = false }
    | Some _ -> { Profile = PdfProfile.PdfUa1; PdfVersion = Option.toObj version; FromFile = false }
    | None -> { Profile = PdfUaTarget.forVersion version; PdfVersion = Option.toObj version; FromFile = true }

let private formatOption () =
    option "--format"
    |> desc "Output format."
    |> helpName "json|text"
    |> def "json"
    |> validate (fun format ->
        if format = "json" || format = "text" then Ok() else Error $"Unknown --format '{format}'. Expected 'json' or 'text'.")

let private fixesOption (defaultNames: string) =
    optionMaybe<string> "--fixes"
    |> desc
        $"The repairs to run, comma-separated: \"safe\" (the ones that need no guesswork), \"all\" (every one \
          but link-titles-from-web, which visits web pages), \"none\", or names from 'legibyl list fixes', e.g. \
          \"safe,lists,headings-from-toc\". Default: {defaultNames}. tag-from-layout and join-split-paragraphs \
          download the layout model (about 130 MB) the first time."
    |> helpName "names"
    |> arity Arity.ExactlyOne
    |> validate (fun names ->
        match names with
        | Some names -> FixNames.parse names |> Result.map ignore
        | None -> Ok())

let private editsOption () =
    option<bool> "--edits"
    |> desc
        "Also write the edits in each file's review: the \"<name>.pdf.legibyl.json\" file next to it (as the GUI \
         saves it), or else the review inside a copy Legibyl saved. Alt text, replacement text, tags, merged \
         paragraphs and link descriptions go to the elements they're for; the copy carries the review."

let private editsFromOption () =
    optionMaybe<string> "--edits-from"
    |> desc "Like --edits, with the review in path: a .legibyl.json file, or a PDF whose review to use (an earlier version's, say)."
    |> helpName "path"
    |> arity Arity.ExactlyOne

let private verboseOption () =
    option<bool> "--verbose"
    |> desc "Print what veraPDF logs while it reads each file on stderr as it comes (it's always in the report's validatorNotes)."

let private reportOption () =
    optionMaybe<string> "--report" |> desc "Write the JSON (or text) report to path instead of stdout." |> helpName "path"

// ---- validate ----

type private ValidateArgs =
    { Files: string array
      Ua: string option
      Format: string
      Output: string option
      Structure: bool
      Verbose: bool }

let private validateFile (validator: VeraPdfValidator) (args: ValidateArgs) (file: string) =
    let target = targetFor args.Ua file
    let result = ValidationResult.Empty(file, target)
    match (try Ok(validator.Validate(file, target.Profile)) with :? VeraPdfValidationException as ex -> Error ex.Message) with
    | Error message -> { result with Error = message }
    | Ok report ->
        let result =
            { result with
                FlavourId = report.FlavourId
                IsCompliant = Nullable report.IsCompliant
                TotalAssertions = Nullable report.TotalAssertions
                FailedCount = Nullable report.FailedCount
                ValidatorNotes = report.Notes
                Failures = report.FailedAssertions |> Seq.map FailureDetail.Of |> Array.ofSeq
                Warnings =
                    report.Assertions
                    |> Seq.filter (fun a -> a.Severity = FindingSeverity.Warning)
                    |> Seq.map FailureDetail.Of
                    |> Array.ofSeq }
        if not args.Structure then
            result
        else
            try
                { result with Structure = validator.ReadStructureTree file }
            with :? VeraPdfValidationException as ex ->
                { result with StructureError = ex.Message }

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
          match node.LinkDescription with NonNull description -> yield $"description=\"{description}\"" | Null -> ()
          match node.LinkDestinationDescription with NonNull dest -> yield $"dest=\"{dest}\"" | Null -> ()
          if node.ContentLeafCount > 0 then yield $"content×{node.ContentLeafCount}" ]

    sb.AppendLine(pad + String.Join(' ', parts)) |> ignore
    match node.Content with
    | NonNull content -> sb.AppendLine(pad + $"  text=\"{content}\"") |> ignore
    | Null -> ()

    for child in node.Children do
        appendStructure sb child (indent + 2)

/// A note veraPDF logged, as the text reports list it.
let private noteLine (note: ValidatorNote) =
    let times = if note.Count > 1 then $" (×{note.Count})" else ""
    $"  veraPDF noted ({note.Level}, {note.Logger}): {note.Message}{times}"

let private validationText (results: ValidationResult list) =
    let sb = StringBuilder()
    let line (text: string) = sb.AppendLine text |> ignore
    let fixableSuffix (detail: FailureDetail) =
        if detail.IsFixable then $" (fixable: {String.Join(',', detail.Fixes)})" else ""
    let details (items: FailureDetail seq | null) = match items with NonNull items -> items | Null -> Seq.empty

    for r in results do
        match r.Error with
        | NonNull error -> line $"{r.File}: ERROR — {error}"
        | Null ->
            line (
                if r.IsCompliant = Nullable true then $"{r.File}: PASS ({r.Standard}, {r.TotalAssertions} checks)"
                else $"{r.File}: FAIL ({r.Standard}, {r.FailedCount}/{r.TotalAssertions} checks failed)"
            )
            for prefix, items in [ "", r.Failures; "WARNING ", r.Warnings ] do
                for f in details items do
                    line $"  {prefix}[{f.Specification} {f.Clause}-{f.TestNumber}] {f.Message}{fixableSuffix f}"
                    match f.Meaning with
                    | NonNull meaning -> line $"    means: {meaning}"
                    | Null -> ()
                    match f.Location with
                    | NonNull location -> line $"    where: {location}"
                    | Null -> ()
                    if not (String.IsNullOrEmpty f.LocationContext) then
                        line $"    at: {f.LocationContext}"
                    match f.Region with
                    | NonNull region -> line $"    region: {ValidationReportTextFormatter.FormatRegion region}"
                    | Null -> ()
            match r.ValidatorNotes with
            | NonNull notes -> for note in notes do line (noteLine note)
            | Null -> ()
            match r.StructureError, r.Structure with
            | NonNull error, _ -> line $"  structure: ERROR — {error}"
            | Null, NonNull structure ->
                line "  structure:"
                appendStructure sb structure 4
            | Null, Null -> ()

    sb.ToString().TrimEnd('\n', '\r')

let private runValidate (args: ValidateArgs) =
    VeraPdfValidator.EchoLog <- args.Verbose
    let validator = VeraPdfValidator()
    let results = [ for file in args.Files -> validateFile validator args file ]
    write args.Output (if args.Format = "json" then toJson results else validationText results)
    results
    |> List.map (fun r ->
        if not (isNull r.Error) then ExitCodes.ToolError
        elif r.IsCompliant = Nullable false then ExitCodes.DocumentProblem
        else ExitCodes.Success)
    |> worst

// ---- fix and layout apply ----

/// What to repair a file with.
type private Repair =
    { /// --ua's standard, if given; else each file's own.
      Ua: string option
      Fixes: PdfFixOptions
      Review: ReviewFile.SavedFile option
      /// Whether a review was asked for (--edits): one that isn't there is then worth saying so.
      ReviewAsked: bool
      /// `layout apply`'s layout, to tag from.
      Layout: LayoutFile.SavedLayout option
      Output: string option }

let private compliance (report: ValidationReport) = { IsCompliant = report.IsCompliant; FailedCount = report.FailedCount }

let private nextVersion (file: string) =
    let name = CopyNames.nextVersion file
    match Path.GetDirectoryName file with
    | NonNull dir when dir <> "" -> Path.Combine(dir, name)
    | _ -> name

let private repairFile (validator: VeraPdfValidator) (repair: Repair) (file: string) : RepairResult =
    let target = targetFor repair.Ua file
    let profile = target.Profile
    let result =
        { File = file
          Standard = PdfUaTarget.displayName target.Profile
          PdfVersion = target.PdfVersion
          StandardFrom = target.From
          Status = FixStatus.Error
          OutputPath = null
          Applied = [||]
          NotApplied = [||]
          Error = null
          Before = None
          After = None
          ValidatorNotes = [||] }
    let targetPath = repair.Output |> Option.defaultWith (fun () -> nextVersion file)
    // The review's edits (--edits: the file's own review), and a layout it recorded for an
    // untagged PDF (only the pages that haven't changed since).
    let review =
        repair.Review
        |> Option.orElseWith (fun () -> if repair.ReviewAsked then ReviewFile.readFor file else None)
        |> Option.map (fun review ->
            match review.Layout with
            | NonNull layout -> { review with Layout = LayoutFile.current file layout }
            | Null -> review)
    let noReview =
        [ if repair.ReviewAsked && review.IsNone then
              $"No edits were applied: there's no {Path.GetFileName(ReviewFile.filePath file)} next to it, and no review inside it." ]
    let reviewLayout = review |> Option.bind (fun review -> Option.ofObj review.Layout)
    let options =
        repair.Fixes
        ||| (if repair.Layout.IsSome || reviewLayout.IsSome then PdfFixOptions.TagFromLayout else PdfFixOptions.None)
        ||| (match review with
             | Some review when not (ReviewFile.edits review).IsEmpty || not (String.IsNullOrWhiteSpace review.Title) -> PdfFixOptions.ApplyEdits
             | _ -> PdfFixOptions.None)
    if options = PdfFixOptions.None then
        let before = try Some(validator.Validate(file, profile)) with :? VeraPdfValidationException -> None
        match before with
        | Some before ->
            { result with
                Status = FixStatus.NothingToDo
                NotApplied = Array.ofList noReview
                Before = Some(compliance before)
                ValidatorNotes = before.Notes }
        | None -> { result with Error = $"'{file}' couldn't be validated." }
    else
        // The layout model, for the repairs that read the page's layout, unless a layout given or
        // reviewed has every page.
        let pageCount () =
            use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfReader(file))
            document.GetNumberOfPages()
        let model =
            let needed =
                FixNames.usesLayout options
                && (match repair.Layout |> Option.orElse reviewLayout with
                    | Some layout -> not (LayoutFile.covers layout (pageCount ()))
                    | None -> true)
            if needed then
                ensureLayoutModel ()
                Some(new Legibyl.Layout.LayoutRegionSource(null))
            else
                None
        let modelSource = model |> Option.map (fun model -> model :> IPageLayoutSource)
        let layout: IPageLayoutSource option =
            match repair.Layout, review with
            // The layout given, with the review's edits layered on (not a layout of its own).
            | Some given, Some review -> Some(ReviewFile.ReviewedRegions(LayoutFile.FileRegions(given, modelSource), { review with Layout = null }))
            | Some given, None -> Some(LayoutFile.FileRegions(given, modelSource))
            | None, Some review when FixNames.usesLayout options ->
                Some(ReviewFile.ReviewedRegions(modelSource |> Option.defaultValue ReviewFile.noRegions, review))
            | None, _ -> modelSource
        try
            try
                layout |> Option.iter (fun source -> validator.LayoutSource <- source)
                match review with
                | Some review ->
                    validator.Edits <- ReviewFile.edits review
                    // Its title too, on a tagged PDF (tagging from the layout gives it as well).
                    validator.Title <- (if String.IsNullOrWhiteSpace review.Title then null else review.Title)
                    validator.Review <- ReviewFile.forCopy (Some review)
                | None -> ()
                let fix = validator.Fix(file, profile, targetPath, options)
                let before = Some(compliance fix.OriginalReport)
                let result = { result with ValidatorNotes = fix.OriginalReport.Notes }
                let applied = Array.ofSeq fix.AppliedFixes
                let tagged = applied |> Array.exists (fun line -> line.StartsWith "Tagged ")
                // Edits of the layout tagged from that it couldn't use, and why.
                let unmatched =
                    match repair.Layout |> Option.orElse reviewLayout, review with
                    | Some layout, Some review when tagged -> LayoutFile.unmatched layout (ReviewFile.edits review)
                    | _ -> []
                let notApplied = Array.ofList (List.ofSeq fix.NotApplied @ unmatched @ noReview)
                match fix.OutputPath with
                | _ when repair.Layout.IsSome && not tagged ->
                    { result with
                        Status = FixStatus.AlreadyTagged
                        OutputPath = fix.OutputPath
                        Applied = applied
                        NotApplied = notApplied
                        Before = before
                        After = fix.OutputPath |> Option.ofObj |> Option.map (fun copy -> compliance (validator.Validate(copy, profile))) }
                | NonNull copy ->
                    { result with
                        Status = FixStatus.Repaired
                        OutputPath = copy
                        Applied = applied
                        NotApplied = notApplied
                        Before = before
                        After = Some(compliance (validator.Validate(copy, profile))) }
                | Null -> { result with Status = FixStatus.NothingToDo; Applied = applied; NotApplied = notApplied; Before = before }
            with
            | :? VeraPdfValidationException as ex -> { result with Error = ex.Message }
            | :? Net.Http.HttpRequestException as ex -> { result with Error = $"Couldn't download the layout model: {ex.Message}" }
        finally
            validator.LayoutSource <- null
            validator.Edits <- []
            validator.Title <- null
            validator.Review <- null
            model |> Option.iter (fun model -> (model :> IDisposable).Dispose())

let private fixText (results: RepairResult list) =
    let sb = StringBuilder()
    let line (text: string) = sb.AppendLine text |> ignore
    let standing (c: Compliance) = if c.IsCompliant then "PASS" else $"FAIL, {c.FailedCount} failed"
    for r in results do
        match r.Status, r.Before, r.After with
        | FixStatus.Error, _, _ -> line $"{r.File}: ERROR — {r.Error}"
        | FixStatus.Repaired, Some before, Some after ->
            line $"{r.File}: repaired → {r.OutputPath} (before: {standing before}; after: {standing after})"
        | FixStatus.AlreadyTagged, _, _ -> line $"{r.File}: already tagged; a layout is for an untagged file"
        | _, Some before, _ -> line $"{r.File}: nothing to do ({standing before})"
        | status, _, _ -> line $"{r.File}: {status}"
        for applied in r.Applied do
            line $"  - {applied}"
        for skipped in r.NotApplied do
            line $"  ! {skipped}"
        for note in r.ValidatorNotes do
            line (noteLine note)
    sb.ToString().TrimEnd('\n', '\r')

/// The exit code for a repair: whether the document it leaves (the copy, else the original)
/// complies.
let private fixExit (r: RepairResult) =
    match r.Status, r.After, r.Before with
    | FixStatus.Error, _, _ -> ExitCodes.ToolError
    | FixStatus.AlreadyTagged, _, _ -> ExitCodes.DocumentProblem
    | _, Some after, _ -> if after.IsCompliant then ExitCodes.Success else ExitCodes.DocumentProblem
    | _, None, Some before -> if before.IsCompliant then ExitCodes.Success else ExitCodes.DocumentProblem
    | _ -> ExitCodes.Success

/// What's wrong with the edits in a review file (see ReviewFile.problems), as one message naming
/// it; None if nothing is, or it's a PDF (whose review Legibyl wrote).
let private editProblems (option: string) (path: string) =
    if String.Equals(Path.GetExtension path, ".pdf", StringComparison.OrdinalIgnoreCase) || not (File.Exists path) then
        None
    else
        match ReviewFile.problems (File.ReadAllText path) with
        | [] -> None
        | problems -> Some(String.Join(Environment.NewLine, $"{option}: {path} has edits that can't be applied as written:" :: [ for p in problems -> $"  {p}" ]))

/// The review --edits-from names, or why it can't be used.
let private reviewFrom (path: string) =
    if not (File.Exists path) then Error $"--edits-from: {path} doesn't exist."
    else
        match editProblems "--edits-from" path, ReviewFile.readFrom path with
        | Some problems, _ -> Error problems
        | None, Some review -> Ok review
        | None, None -> Error $"--edits-from: {path} has no review in it."

/// What's wrong with the reviews next to files that --edits applies, if anything.
let private sidecarProblems (edits: bool) (files: string seq) =
    if not edits then None
    else files |> Seq.tryPick (fun file -> editProblems "--edits" (ReviewFile.filePath file))

let private runRepair (validator: VeraPdfValidator) (files: string list) (repair: Repair) (format: string) (report: string option) =
    let results = [ for file in files -> repairFile validator repair file ]
    write report (if format = "json" then toJson results else fixText results)
    results |> List.map fixExit |> worst

type private FixArgs =
    { Files: string array
      Ua: string option
      Format: string
      Report: string option
      Fixes: string option
      Edits: bool
      EditsFrom: string option
      Output: string option
      Verbose: bool }

let private runFix (args: FixArgs) =
    VeraPdfValidator.EchoLog <- args.Verbose
    if args.Edits && args.EditsFrom.IsSome then
        usageError "Use --edits or --edits-from, not both."
    elif (sidecarProblems args.Edits args.Files).IsSome then
        usageError (sidecarProblems args.Edits args.Files).Value
    elif args.Output.IsSome && args.Files.Length > 1 then
        usageError "--output names one repaired copy: use it with a single file (several get their next versions)."
    else
        match args.EditsFrom |> Option.map reviewFrom with
        | Some(Error message) -> usageError message
        | review ->
            let fixes = FixNames.parse (defaultArg args.Fixes "safe") |> Result.defaultValue FixNames.safe
            runRepair
                (VeraPdfValidator())
                (List.ofArray args.Files)
                { Ua = args.Ua
                  Fixes = fixes
                  Review = review |> Option.bind Result.toOption
                  ReviewAsked = args.Edits
                  Layout = None
                  Output = args.Output }
                args.Format
                args.Report

// ---- structure ----

type private StructureArgs = { Files: string array; Format: string; Output: string option }

let private runStructure (args: StructureArgs) =
    let validator = VeraPdfValidator()
    let results =
        [ for file in args.Files ->
              try
                  { File = file; Structure = validator.ReadStructureTree file; Error = null }
              with :? VeraPdfValidationException as ex ->
                  { File = file; Structure = null; Error = ex.Message } ]
    let text () =
        let sb = StringBuilder()
        for r in results do
            match r.Error, r.Structure with
            | NonNull error, _ -> sb.AppendLine $"{r.File}: ERROR — {error}" |> ignore
            | Null, Null -> sb.AppendLine $"{r.File}: no structure tree (untagged)" |> ignore
            | Null, NonNull structure ->
                sb.AppendLine $"{r.File}:" |> ignore
                appendStructure sb structure 2
        sb.ToString().TrimEnd('\n', '\r')
    write args.Output (if args.Format = "json" then toJson results else text ())
    results
    |> List.map (fun r ->
        if not (isNull r.Error) then ExitCodes.ToolError
        elif isNull r.Structure then ExitCodes.DocumentProblem
        else ExitCodes.Success)
    |> worst

// ---- layout suggest and layout apply ----

type private SuggestArgs = { File: string; Output: string option; FigureImages: bool }

let private runSuggest (args: SuggestArgs) =
    if args.FigureImages && args.Output.IsNone then
        usageError "--figure-images needs --output: the images go in a folder beside the layout file."
    elif not (File.Exists args.File) then
        eprintfn $"error: {args.File} doesn't exist."
        ExitCodes.ToolError
    else
        try
            ensureLayoutModel ()
            use suggester = new Legibyl.Layout.LayoutSuggester(args.File)
            let pages =
                [ for page in 1 .. suggester.PageCount ->
                      eprintfn $"Finding page {page} of {suggester.PageCount}'s layout…"
                      suggester.Suggest page ]
            let layout =
                Legibyl.Layout.LayoutSuggestions.toLayoutFile args.File pages (Legibyl.Layout.LayoutSuggestions.suggestedTitle args.File pages)
            match args.Output with
            | None -> Console.WriteLine(LayoutFile.toJson layout)
            | Some path ->
                let layout =
                    if args.FigureImages then
                        use renderer = new Legibyl.Rendering.PdfRegionRenderer()
                        LayoutFile.withFigureImages path (fun geometry -> renderer.RenderRegion(args.File, geometry)) layout
                    else
                        layout
                File.WriteAllText(path, LayoutFile.toJson layout)
                Console.WriteLine(
                    toJson
                        { File = args.File
                          Layout = path
                          Images = if args.FigureImages then LayoutFile.imagesFolder path else null
                          Pages = layout.Pages.Length
                          Regions = layout.Pages |> Array.sumBy _.Regions.Length }
                )
            ExitCodes.Success
        with
        | :? Net.Http.HttpRequestException as ex ->
            eprintfn $"error: couldn't download the layout model: {ex.Message}"
            ExitCodes.ToolError
        | :? IOException
        | :? UnauthorizedAccessException
        | :? InvalidOperationException as ex ->
            eprintfn $"error: {ex.Message}"
            ExitCodes.ToolError

type private ApplyArgs =
    { File: string
      Layout: string
      Ua: string option
      Format: string
      Report: string option
      Fixes: string option
      Edits: bool
      EditsFrom: string option
      Output: string option }

let private runApply (args: ApplyArgs) =
    if args.Edits && args.EditsFrom.IsSome then
        usageError "Use --edits or --edits-from, not both."
    elif (sidecarProblems args.Edits [ args.File ]).IsSome then
        usageError (sidecarProblems args.Edits [ args.File ]).Value
    else
        match LayoutFile.read args.Layout, args.EditsFrom |> Option.map reviewFrom with
        | Error message, _ -> usageError message
        | _, Some(Error message) -> usageError message
        | Ok layout, review ->
            let fixes = FixNames.parse (defaultArg args.Fixes "none") |> Result.defaultValue PdfFixOptions.None
            runRepair
                (VeraPdfValidator())
                [ args.File ]
                { Ua = args.Ua
                  Fixes = fixes
                  Review = review |> Option.bind Result.toOption
                  ReviewAsked = args.Edits
                  Layout = Some layout
                  Output = args.Output }
                args.Format
                args.Report

// ---- list ----

let private listFixes (format: string) =
    if format = "json" then
        Console.WriteLine(
            toJson
                [ for name, option in FixNames.names ->
                      {| Name = name
                         Description = PdfFixDescriptions.Describe option |> List.tryHead |> Option.toObj
                         Safe = FixNames.safe.HasFlag option
                         InAll = FixNames.all.HasFlag option
                         UsesLayoutModel = FixNames.usesLayout option |} ]
        )
    else
        FixNames.listing () |> List.iter (printfn "%s")
    ExitCodes.Success

// ---- The command line ----

/// Runs a function in place of a command's action, e.g. for --version.
type private InvokeAction(invoke: ParseResult -> int) =
    inherit SynchronousCommandLineAction()
    override _.Invoke(parseResult) = invoke parseResult

let private helpFooter =
    """EXIT CODES (every command):
  0   done, and the document is fine (complies, has a structure, was tagged).
  1   done, but a document has a problem (doesn't comply, has no structure, was already tagged).
  2   the tool couldn't do it for a file (missing, corrupt, encrypted, model download failed).
  64  invalid command-line usage.

EXAMPLES:
  legibyl validate document.pdf
  legibyl validate *.pdf --output results.json
  legibyl validate report.pdf --ua 2
  legibyl fix document.pdf --fixes safe,lists,headings-from-toc
  legibyl fix flyer.v3.pdf --edits-from flyer.v2.pdf --output flyer.v4.pdf
  legibyl structure document.pdf --output structure.json
  legibyl layout suggest scan.pdf --output scan.layout.json --figure-images
  legibyl layout apply scan.pdf --layout scan.layout.json --edits-from scan.edits.json --fixes safe,qr-links
  legibyl list fixes --format json
"""

/// The version line, as the GUI's About window gives it too (see BuildInfo).
let private printVersion _ =
    printfn $"{BuildInfo.versionLine}"
    ExitCodes.Success

/// Reports a parse error the same way as the CLI's other usage errors.
let private reportParseError (parseResult: ParseResult) =
    let error = parseResult.Errors[0]
    match error.SymbolResult with
    | :? OptionResult as option when option.Tokens.Count = 0 -> usageError $"{option.Option.Name} requires a value."
    | _ -> usageError error.Message

let private validateCommand =
    command "validate" {
        description
            "Validate PDFs against PDF/UA (PDF/UA-2 for a PDF 2.0 file, PDF/UA-1 for earlier ones; see --ua), reporting each failure and Legibyl's own warnings."
        inputs (
            input {
                let! files = filesArgument ()
                and! ua = uaOption ()
                and! format = formatOption ()
                and! output = optionMaybe "--output" |> desc "Write the report to path instead of stdout." |> helpName "path"
                and! structure =
                    option<bool> "--structure"
                    |> desc "Also include each file's structure tree (as 'legibyl structure' gives it)."
                and! verbose = verboseOption ()
                return
                    { Files = files
                      Ua = ua
                      Format = format
                      Output = output
                      Structure = structure
                      Verbose = verbose }
            }
        )
        setAction runValidate
    }

let private fixCommand =
    command "fix" {
        description
            "Repair PDFs, writing each one's next version (\"<name>.v2.pdf\"; \"<name>.v3.pdf\" for \"<name>.v2.pdf\"), and validate the copy."
        inputs (
            input {
                let! files = filesArgument ()
                and! fixes = fixesOption "safe"
                and! edits = editsOption ()
                and! editsFrom = editsFromOption ()
                and! output = optionMaybe "--output" |> desc "The repaired copy's path (one file only)." |> helpName "path"
                and! ua = uaOption ()
                and! format = formatOption ()
                and! report = reportOption ()
                and! verbose = verboseOption ()
                return
                    { Files = files
                      Ua = ua
                      Format = format
                      Report = report
                      Fixes = fixes
                      Edits = edits
                      EditsFrom = editsFrom
                      Output = output
                      Verbose = verbose }
            }
        )
        setAction runFix
    }

let private structureCommand =
    command "structure" {
        description
            "Read each PDF's structure tree: the heading/paragraph/list/table/link hierarchy screen readers use, with each element's text, page and box."
        inputs (
            input {
                let! files = filesArgument ()
                and! format = formatOption ()
                and! output = optionMaybe "--output" |> desc "Write it to path instead of stdout." |> helpName "path"
                return { Files = files; Format = format; Output = output }
            }
        )
        setAction runStructure
    }

let private suggestCommand =
    command "suggest" {
        description
            "Suggest a layout for an untagged PDF: each page's regions in reading order, with the tag each suggests, its page, \
             box and text, as JSON. Leave it as found and write changes as edits (exclude, retag, alt, text) in a review \
             file; then 'legibyl layout apply'. Downloads the layout model (about 130 MB) the first time; several seconds a page."
        inputs (
            input {
                let! file = fileArgument ()
                and! output =
                    optionMaybe "--output"
                    |> desc "Write the layout to path (stdout then gets a summary); without it, the layout goes to stdout."
                    |> helpName "path"
                and! figureImages =
                    option<bool> "--figure-images"
                    |> desc
                        "With --output: also write a PNG of each figure into a folder beside the layout \
                         (\"scan.layout.json\" -> \"scan.layout.images/p1-r3.png\"), named in the figure's \"image\", to \
                         look at when writing its alt text. Each run clears the last run's images."
                return { File = file; Output = output; FigureImages = figureImages }
            }
        )
        setAction runSuggest
    }

let private applyCommand =
    command "apply" {
        description
            "Tag an untagged PDF from a layout ('legibyl layout suggest''s), with a review's edits layered on, writing its next \
             version and validating it."
        inputs (
            input {
                let! file = fileArgument ()
                and! layout =
                    option<string> "--layout" |> desc "The layout to tag from." |> helpName "path" |> required
                and! edits = editsOption ()
                and! editsFrom = editsFromOption ()
                and! fixes = fixesOption "none"
                and! output = optionMaybe "--output" |> desc "The tagged copy's path." |> helpName "path"
                and! ua = uaOption ()
                and! format = formatOption ()
                and! report = reportOption ()
                return
                    { File = file
                      Layout = layout
                      Ua = ua
                      Format = format
                      Report = report
                      Fixes = fixes
                      Edits = edits
                      EditsFrom = editsFrom
                      Output = output }
            }
        )
        setAction runApply
    }

let private layoutCommand =
    command "layout" {
        description "Suggest a layout for an untagged PDF, and tag it from one."
        noAction
        addCommand suggestCommand
        addCommand applyCommand
    }

let private listFormatOption () =
    option "--format"
    |> desc "Output format."
    |> helpName "text|json"
    |> def "text"
    |> validate (fun format ->
        if format = "json" || format = "text" then Ok() else Error $"Unknown --format '{format}'. Expected 'text' or 'json'.")

let private listCommand =
    command "list" {
        description "List what the other commands accept."
        noAction
        addCommand (
            command "fixes" {
                description "The repairs 'fix --fixes' can name, with what each does."
                inputs (listFormatOption ())
                setAction listFixes
            }
        )
    }

[<EntryPoint>]
let main argv =
    let root =
        ManualInvocation.rootCommand {
            description
                "legibyl - PDF/UA & PDF/A validator and repairer (CLI), powered by veraPDF via IKVM.\n\n\
                 Designed for scripting and AI-agent use: output is machine-readable JSON on stdout by default, and\n\
                 the exit code tells you the outcome without needing to parse output."
            noAction
            addCommand validateCommand
            addCommand fixCommand
            addCommand structureCommand
            addCommand layoutCommand
            addCommand listCommand
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
    | _ when argv.Length = 0 -> usageError "Name a command: validate, fix, structure, layout or list."
    | _ ->
        // Let unexpected exceptions crash rather than exit with a code that means something else.
        parseResult.Invoke(InvocationConfiguration(EnableDefaultExceptionHandler = false))
