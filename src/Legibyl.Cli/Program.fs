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

/// Downloads the layout models (the layout detector's, and the table-structure model) if they
/// aren't yet, saying how it's going on stderr.
let private ensureLayoutModel () =
    if not (Legibyl.Layout.LayoutModel.isDownloaded ()) then
        eprintfn $"Downloading the layout models (about {Legibyl.Layout.LayoutModel.downloadMegabytes ()} MB), once…"
        let mutable shown = -1
        let progress =
            { new IProgress<float> with
                member _.Report fraction =
                    let percent = int (fraction * 10.0) * 10
                    if percent > shown then
                        shown <- percent
                        eprintfn $"  {percent}%%" }
        Legibyl.Layout.LayoutModel.download progress Threading.CancellationToken.None |> _.GetAwaiter().GetResult()

/// What went wrong, as a file's error says it: the innermost exception's message (a task's comes
/// wrapped), a download's failure said as one.
let private failure (ex: exn) =
    let rec inner (ex: exn) =
        match ex with
        | :? AggregateException as aggregate when aggregate.InnerExceptions.Count = 1 -> inner aggregate.InnerExceptions[0]
        | ex -> ex
    match inner ex with
    | :? Net.Http.HttpRequestException as ex -> $"Couldn't download the layout model: {ex.Message}"
    | :? Threading.Tasks.TaskCanceledException -> "Couldn't download the layout model: the connection timed out."
    | ex -> ex.Message

/// The standard a file is checked against when even that couldn't be read from it.
let private unknownTarget: Target = { Profile = PdfProfile.PdfUa1; PdfVersion = null; FromFile = true }

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
        $"The fixes to run, comma-separated: \"safe\" (the ones that need no guesswork), \"all\" (every one \
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
      Verbose: bool
      /// Report the warnings the review dismisses too.
      AllWarnings: bool
      /// --review: the review to use for every file (a .legibyl.json file, or a PDF whose review
      /// to use), instead of each one's own.
      Review: string option }

/// Validates file; its warnings the review (given, else its own) dismisses are left out.
let private validateFile (validator: VeraPdfValidator) (args: ValidateArgs) (given: (string * ReviewFile.SavedFile) option) (file: string) =
    let target = targetFor args.Ua file
    let result = ValidationResult.Empty(file, target)
    match (try Ok(validator.Validate(file, target.Profile)) with :? VeraPdfValidationException as ex -> Error ex.Message) with
    | Error message -> { result with Error = message }
    | Ok report ->
        // The review's dismissals, as the GUI shows them: --review's, else the file's own.
        let reviewPath, review =
            match given with
            | Some(path, review) -> Some path, Some review
            | None -> ReviewFile.sourceFor file, ReviewFile.readFor file
        let warnings = report.Assertions |> Seq.filter (fun a -> a.Severity = FindingSeverity.Warning) |> List.ofSeq
        let dismissed = warnings |> List.filter (fun w -> review |> Option.exists (fun r -> ReviewFile.dismisses r w))
        let result =
            { result with
                FlavourId = report.FlavourId
                IsCompliant = Nullable report.IsCompliant
                MeetsWcag = Nullable report.MeetsWcag
                WcagFailedCount = Nullable(Seq.length report.WcagFailures)
                WcagError = report.WcagError
                TotalAssertions = Nullable report.TotalAssertions
                FailedCount = Nullable report.FailedCount
                ValidatorNotes = report.Notes
                CheckedByPerson =
                    match ReviewFile.carriedChecklist file with
                    | Some checklist -> { Steps = nonNull checklist.Checked; CheckedAt = checklist.CheckedAt }
                    | None -> null
                Failures = report.FailedAssertions |> Seq.map FailureDetail.Of |> Array.ofSeq
                Warnings =
                    warnings
                    |> List.filter (fun w -> args.AllWarnings || not (List.contains w dismissed))
                    |> List.map FailureDetail.Of
                    |> Array.ofList
                DismissedWarnings = dismissed.Length
                Review = Option.toObj reviewPath }
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

/// A validated file's first line: PASS when it complies with PDF/UA and WCAG 2.2's machine checks
/// pass, else FAIL, with how many of each standard's checks failed.
let private standingLine (r: ValidationResult) =
    let wcagFailed = r.WcagFailedCount.GetValueOrDefault()
    let uaFailed = r.FailedCount.GetValueOrDefault() - wcagFailed
    if r.IsCompliant = Nullable true && r.MeetsWcag = Nullable true then
        $"{r.File}: PASS ({r.Standard} and {WcagSpecification.Name}, {r.TotalAssertions} checks)"
    else
        let standing (name: string) (passes: bool) (failed: int) = if passes then $"{name} passes" else $"{name}: {failed} failed"
        let wcag =
            if isNull r.WcagError then standing WcagSpecification.Name (r.MeetsWcag = Nullable true) wcagFailed
            else $"{WcagSpecification.Name} not checked"
        $"{r.File}: FAIL ({standing r.Standard (r.IsCompliant = Nullable true) uaFailed}; {wcag}; {r.TotalAssertions} checks)"

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
            line (standingLine r)
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
            if r.DismissedWarnings > 0 then
                let what = if r.DismissedWarnings = 1 then "1 warning" else $"{r.DismissedWarnings} warnings"
                line $"  {what} dismissed in the review ({r.Review})"
            for note in r.ValidatorNotes do
                line (noteLine note)
            match r.CheckedByPerson with
            | NonNull checkedBy ->
                let date = checkedBy.CheckedAt |> Option.ofNullable |> Option.map (fun at -> " on " + at.ToString("yyyy-MM-dd", Globalization.CultureInfo.InvariantCulture)) |> Option.defaultValue ""
                line $"""  checked by a person{date}: {String.Join(", ", checkedBy.Steps)}"""
            | Null -> ()
            match r.StructureError, r.Structure with
            | NonNull error, _ -> line $"  structure: ERROR — {error}"
            | Null, NonNull structure ->
                line "  structure:"
                appendStructure sb structure 4
            | Null, Null -> ()

    sb.ToString().TrimEnd('\n', '\r')

/// With --verbose, prints each record veraPDF logs on stderr as it comes, until disposed.
let private printingLog (verbose: bool) : IDisposable =
    if verbose then VeraPdfValidator.Recorded.Subscribe(fun note -> eprintfn $"veraPDF {note.Level} {note.Logger}: {note.Message}")
    else { new IDisposable with member _.Dispose() = () }

let private runValidate (args: ValidateArgs) =
    use _ = printingLog args.Verbose
    let given =
        args.Review
        |> Option.map (fun path ->
            if not (File.Exists path) then Error $"--review: {path} doesn't exist."
            else
                match ReviewFile.readFrom path with
                | Some review -> Ok(path, review)
                | None -> Error $"--review: {path} has no review in it.")
    match given with
    | Some(Error message) -> usageError message
    | _ ->
        let given = given |> Option.bind Result.toOption
        let validator = VeraPdfValidator()
        // Each file on its own: one failing some unexpected way doesn't lose the others' results.
        let results =
            [ for file in args.Files ->
                  try
                      validateFile validator args given file
                  with ex ->
                      { ValidationResult.Empty(file, (try targetFor args.Ua file with _ -> unknownTarget)) with Error = failure ex } ]
        write args.Output (if args.Format = "json" then toJson results else validationText results)
        results
        |> List.map (fun r ->
            if not (isNull r.Error) then ExitCodes.ToolError
            elif r.IsCompliant = Nullable false then ExitCodes.DocumentProblem
            // WCAG's checks couldn't run: the tool's fault, not the document's.
            elif not (isNull r.WcagError) then ExitCodes.ToolError
            elif r.MeetsWcag = Nullable false then ExitCodes.DocumentProblem
            else ExitCodes.Success)
        |> worst

// ---- fix and layout apply ----

/// What to fix a file with.
type private FixRequest =
    { /// --ua's standard, if given; else each file's own.
      Ua: string option
      Fixes: PdfFixOptions
      Review: ReviewFile.SavedFile option
      /// Whether a review was asked for (--edits): one that isn't there is then worth saying so.
      ReviewAsked: bool
      /// `layout apply`'s layout, to tag from.
      Layout: LayoutFile.SavedLayout option
      /// --lang's language, declared in place of the review's or any the PDF says.
      Language: string option
      Output: string option }

let private compliance (report: ValidationReport) =
    { IsCompliant = report.IsCompliant; MeetsWcag = report.MeetsWcag; FailedCount = report.FailedCount; WcagError = report.WcagError }

let private nextVersion (file: string) =
    let name = CopyNames.nextVersion file
    match Path.GetDirectoryName file with
    | NonNull dir when dir <> "" -> Path.Combine(dir, name)
    | _ -> name

let private fixFile (validator: VeraPdfValidator) (request: FixRequest) (file: string) : FixOutcome =
    let target = targetFor request.Ua file
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
          Before = null
          After = null
          ValidatorNotes = [||] }
    let targetPath = request.Output |> Option.defaultWith (fun () -> nextVersion file)
    // The review's edits (--edits: the file's own review), and a layout it recorded for an
    // untagged PDF (only the pages that haven't changed since).
    let review =
        request.Review
        |> Option.orElseWith (fun () -> if request.ReviewAsked then ReviewFile.readFor file else None)
        |> Option.map (fun review ->
            match review.Layout with
            | NonNull layout -> { review with Layout = LayoutFile.current file layout }
            | Null -> review)
    let noReview =
        [ if request.ReviewAsked && review.IsNone then
              $"No edits were applied: there's no {Path.GetFileName(ReviewFile.filePath file)} next to it, and no review inside it." ]
    let reviewLayout = review |> Option.bind (fun review -> Option.ofObj review.Layout)
    // The language chosen: --lang's, else the review's.
    let language =
        request.Language
        |> Option.orElse (review |> Option.bind (fun review -> Option.ofObj review.Language) |> Option.filter (String.IsNullOrWhiteSpace >> not))
    let options =
        request.Fixes
        ||| (if language.IsSome then PdfFixOptions.Language else PdfFixOptions.None)
        ||| (if request.Layout.IsSome || reviewLayout.IsSome then PdfFixOptions.TagFromLayout else PdfFixOptions.None)
        ||| (match review with
             | Some review when
                 not (ReviewFile.edits review).IsEmpty
                 || not (String.IsNullOrWhiteSpace review.Title)
                 // (Its role map, and what's given its attachments, are edits too.)
                 || (match review.RoleMap with NonNull map -> map.Count > 0 | Null -> false)
                 || (match review.Attachments with NonNull edits -> edits.Count > 0 | Null -> false)
                 ->
                 PdfFixOptions.ApplyEdits
             | _ -> PdfFixOptions.None)
    if options = PdfFixOptions.None then
        let before = try Some(validator.Validate(file, profile)) with :? VeraPdfValidationException -> None
        match before with
        | Some before ->
            { result with
                Status = FixStatus.NothingToDo
                NotApplied = Array.ofList noReview
                Before = compliance before
                ValidatorNotes = before.Notes }
        | None -> { result with Error = $"'{file}' couldn't be validated." }
    else
        // The layout model, for the fixes that read the page's layout, unless a layout given or
        // reviewed has every page.
        let pageCount () =
            use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfReader(file))
            document.GetNumberOfPages()
        let model =
            let needed =
                FixNames.usesLayout options
                && (match request.Layout |> Option.orElse reviewLayout with
                    | Some layout -> not (LayoutFile.covers layout (pageCount ()))
                    | None -> true)
            if needed then
                ensureLayoutModel ()
                Some(new Legibyl.Layout.LayoutRegionSource(null))
            else
                None
        let modelSource = model |> Option.map (fun model -> model :> IPageLayoutSource)
        let layout: IPageLayoutSource option =
            match request.Layout, review with
            // The layout given, with the review's edits layered on (not a layout of its own).
            | Some given, Some review -> Some(ReviewFile.ReviewedRegions(LayoutFile.FileRegions(given, modelSource), { review with Layout = null }))
            | Some given, None -> Some(LayoutFile.FileRegions(given, modelSource))
            | None, Some review when FixNames.usesLayout options ->
                Some(ReviewFile.ReviewedRegions(modelSource |> Option.defaultValue ReviewFile.noRegions, review))
            | None, _ -> modelSource
        try
            try
                layout |> Option.iter (fun source -> validator.LayoutSource <- source)
                validator.Language <- Option.toObj language
                match review with
                | Some review ->
                    validator.Edits <- ReviewFile.edits review
                    // Its title too, on a tagged PDF (tagging from the layout gives it as well).
                    validator.Title <- (if String.IsNullOrWhiteSpace review.Title then null else review.Title)
                    // And the tags chosen for its own tag names.
                    match review.RoleMap with
                    | NonNull map -> validator.RoleMap <- map
                    | Null -> ()
                    // And what's given its attached files and media.
                    match review.Attachments with
                    | NonNull edits -> validator.Attachments <- edits
                    | Null -> ()
                    validator.Review <- ReviewFile.forCopy (Some review)
                | None -> ()
                let fix = validator.Fix(file, profile, targetPath, options)
                let before = compliance fix.OriginalReport
                let result = { result with ValidatorNotes = fix.OriginalReport.Notes }
                let applied = Array.ofSeq fix.AppliedFixes
                let tagged = applied |> Array.exists (fun line -> line.StartsWith "Tagged ")
                // Edits of the layout tagged from that it couldn't use, and why.
                let unmatched =
                    match request.Layout |> Option.orElse reviewLayout, review with
                    | Some layout, Some review when tagged -> LayoutFile.unmatched layout (ReviewFile.edits review)
                    | _ -> []
                let notApplied = Array.ofList (List.ofSeq fix.NotApplied @ unmatched @ noReview)
                match fix.OutputPath with
                | _ when request.Layout.IsSome && not tagged ->
                    { result with
                        Status = FixStatus.AlreadyTagged
                        OutputPath = fix.OutputPath
                        Applied = applied
                        NotApplied = notApplied
                        Before = before
                        After =
                            match fix.OutputPath with
                            | NonNull copy -> compliance (validator.Validate(copy, profile))
                            | Null -> null }
                | NonNull copy ->
                    { result with
                        Status = FixStatus.Fixed
                        OutputPath = copy
                        Applied = applied
                        NotApplied = notApplied
                        Before = before
                        After = compliance (validator.Validate(copy, profile)) }
                | Null -> { result with Status = FixStatus.NothingToDo; Applied = applied; NotApplied = notApplied; Before = before }
            with
            | :? VeraPdfValidationException as ex -> { result with Error = ex.Message }
        finally
            validator.LayoutSource <- null
            validator.Edits <- []
            validator.Title <- null
            validator.Language <- null
            validator.RoleMap <- Collections.Generic.Dictionary<string, string>()
            validator.Attachments <- Collections.Generic.Dictionary<string, Attachments.AttachmentEdit>()
            validator.Review <- null
            model |> Option.iter (fun model -> (model :> IDisposable).Dispose())

let private fixText (results: FixOutcome list) =
    let sb = StringBuilder()
    let line (text: string) = sb.AppendLine text |> ignore
    let standing (c: Compliance) =
        if c.IsCompliant && c.MeetsWcag then "PASS"
        else
            let notChecked = if isNull c.WcagError then "" else $"; {WcagSpecification.Name} not checked"
            if c.IsCompliant && not (isNull c.WcagError) then $"FAIL, {WcagSpecification.Name} not checked"
            elif c.IsCompliant then $"FAIL, {c.FailedCount} failed ({WcagSpecification.Name}'s)"
            else $"FAIL, {c.FailedCount} failed{notChecked}"
    for r in results do
        match r.Status, r.Before, r.After with
        | FixStatus.Error, _, _ -> line $"{r.File}: ERROR — {r.Error}"
        | FixStatus.Fixed, NonNull before, NonNull after ->
            line $"{r.File}: fixed → {r.OutputPath} (before: {standing before}; after: {standing after})"
        | FixStatus.AlreadyTagged, _, _ -> line $"{r.File}: already tagged; a layout is for an untagged file"
        | _, NonNull before, _ -> line $"{r.File}: nothing to do ({standing before})"
        | status, _, _ -> line $"{r.File}: {status}"
        for applied in r.Applied do
            line $"  - {applied}"
        for skipped in r.NotApplied do
            line $"  ! {skipped}"
        for note in r.ValidatorNotes do
            line (noteLine note)
    sb.ToString().TrimEnd('\n', '\r')

/// The exit code for a document's standing: WCAG's checks not running is the tool's fault, not
/// the document's, unless PDF/UA fails anyway.
let private standingExit (c: Compliance) =
    if c.IsCompliant && c.MeetsWcag then ExitCodes.Success
    elif c.IsCompliant && not (isNull c.WcagError) then ExitCodes.ToolError
    else ExitCodes.DocumentProblem

/// The exit code for a fix: whether the document it leaves (the copy, else the original)
/// complies.
let private fixExit (r: FixOutcome) =
    match r.Status, r.After, r.Before with
    | FixStatus.Error, _, _ -> ExitCodes.ToolError
    | FixStatus.AlreadyTagged, _, _ -> ExitCodes.DocumentProblem
    | _, NonNull after, _ -> standingExit after
    | _, Null, NonNull before -> standingExit before
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

let private runFixes (validator: VeraPdfValidator) (files: string list) (request: FixRequest) (format: string) (report: string option) =
    // Each file on its own: a model that won't download, or a PDF that won't open, is that file's
    // error (exit 2), and the others' results are kept.
    let failed (file: string) (ex: exn) : FixOutcome =
        // On stderr as well: after a download's last percentage, say, rather than nothing.
        eprintfn $"error: {file}: {failure ex}"
        let target = try targetFor request.Ua file with _ -> unknownTarget
        { File = file
          Standard = PdfUaTarget.displayName target.Profile
          PdfVersion = target.PdfVersion
          StandardFrom = target.From
          Status = FixStatus.Error
          OutputPath = null
          Applied = [||]
          NotApplied = [||]
          Error = failure ex
          Before = null
          After = null
          ValidatorNotes = [||] }
    let results = [ for file in files -> try fixFile validator request file with ex -> failed file ex ]
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
      Lang: string option
      Output: string option
      Verbose: bool }

/// Whether tag is a language tag (BCP 47): a language, then subtags, e.g. en, en-US, zh-Hant-TW.
let private isLanguageTag (tag: string) = RegularExpressions.Regex.IsMatch(tag, "^[A-Za-z]{2,8}(-[A-Za-z0-9]{1,8})*$")

let private runFix (args: FixArgs) =
    use _ = printingLog args.Verbose
    if args.Edits && args.EditsFrom.IsSome then
        usageError "Use --edits or --edits-from, not both."
    elif (sidecarProblems args.Edits args.Files).IsSome then
        usageError (sidecarProblems args.Edits args.Files).Value
    elif args.Output.IsSome && args.Files.Length > 1 then
        usageError "--output names one fixed copy: use it with a single file (several get their next versions)."
    elif args.Lang |> Option.exists (isLanguageTag >> not) then
        usageError $"--lang takes a language tag, e.g. es-MX, not '{args.Lang.Value}'."
    else
        match args.EditsFrom |> Option.map reviewFrom with
        | Some(Error message) -> usageError message
        | review ->
            let fixes = FixNames.parse (defaultArg args.Fixes "safe") |> Result.defaultValue FixNames.safe
            runFixes
                (VeraPdfValidator())
                (List.ofArray args.Files)
                { Ua = args.Ua
                  Fixes = fixes
                  Review = review |> Option.bind Result.toOption
                  ReviewAsked = args.Edits
                  Layout = None
                  Language = args.Lang
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

// ---- append ----

type private AppendArgs =
    { Files: string array
      Output: string option
      Title: string option
      Lang: string option
      PartTag: string option
      Format: string
      Report: string option
      Verbose: bool }

let private appendText (r: AppendOutcome) =
    let sb = StringBuilder()
    let line (text: string) = sb.AppendLine text |> ignore
    // A joined file is always validated; a refusal writes none.
    match r.After with
    | NonNull after ->
        let standing = if after.IsCompliant then "PASS" else $"FAIL, {after.FailedCount} failed"
        let structure = if r.StructureDifferences.Count = 0 then "structure as the files'" else $"{r.StructureDifferences.Count} structure difference(s)"
        line $"joined {r.Inputs.Count} files → {r.OutputPath} ({r.Standard}: {standing}; {structure})"
    | Null -> line "Nothing was written:"
    for refusal in r.Refusals do
        match refusal.File with
        | NonNull file -> line $"  ! {file}: {refusal.Reason}"
        | Null -> line $"  ! {refusal.Reason}"
    for note in r.Notes do
        line $"  - {note}"
    for prefix, items in [ "", r.Failures; "WARNING ", r.Warnings ] do
        for f in items do
            line $"  {prefix}[{f.Specification} {f.Clause}-{f.TestNumber}] {f.Message}"
            match f.Location with
            | NonNull location -> line $"    where: {location}"
            | Null -> ()
    for difference in r.StructureDifferences do
        line $"  ≠ {difference}"
    sb.ToString().TrimEnd('\n', '\r')

let private runAppend (args: AppendArgs) =
    use _ = printingLog args.Verbose
    let files = List.ofArray args.Files
    if files.Length < 2 then
        usageError "Name two or more PDFs to join."
    else
        let profile, _ = PdfUaTarget.detect files.Head
        match args.PartTag, args.Lang with
        | Some tag, _ when not (List.contains tag (Append.partTags profile)) ->
            usageError $"""--part-tag for {PdfUaTarget.displayName profile} is one of {String.Join(", ", Append.partTags profile)}, not '{tag}'."""
        // A language tag (BCP 47): a language, then subtags, e.g. en, en-US, zh-Hant-TW.
        | _, Some lang when not (isLanguageTag lang) ->
            usageError $"--lang takes a language tag, e.g. en-US, not '{lang}'."
        | _ ->
            let output = args.Output |> Option.defaultWith (fun () -> nextVersion files.Head)
            let options: Append.Options = { Title = args.Title; Language = args.Lang; PartTag = args.PartTag }
            match
                (try Ok(Append.run (VeraPdfValidator()) files output options (fun stage -> if args.Verbose then eprintfn $"{stage}")) with
                 | :? VeraPdfValidationException
                 | :? IOException
                 | :? UnauthorizedAccessException
                 | :? InvalidOperationException
                 | :? iText.Commons.Exceptions.ITextException as ex -> Error ex.Message)
            with
            | Error message ->
                eprintfn $"error: the PDFs couldn't be joined: {message}"
                ExitCodes.ToolError
            | Ok outcome ->
                let warnings, failures =
                    match outcome.After with
                    | Some after -> after.Assertions |> Seq.filter (fun a -> a.Severity = FindingSeverity.Warning) |> List.ofSeq, List.ofSeq after.FailedAssertions
                    | None -> [], []
                let result =
                    { Status =
                        if outcome.Joined.IsNone then AppendStatus.Refused
                        elif outcome.Passes then AppendStatus.Joined
                        else AppendStatus.Fails
                      OutputPath = outcome.Joined |> Option.map _.Output |> Option.toObj
                      Standard = outcome.Joined |> Option.map (fun joined -> PdfUaTarget.displayName joined.Profile) |> Option.toObj
                      Title = outcome.Joined |> Option.map _.Title |> Option.toObj
                      Language = outcome.Joined |> Option.bind _.Language |> Option.toObj
                      Inputs =
                        [| for file, report in outcome.Inputs ->
                               { File = file
                                 Standard = PdfUaTarget.displayName report.Profile
                                 IsCompliant = report.IsCompliant
                                 FailedCount = report.FailedCount } |]
                      Refusals = [| for refusal in outcome.Refusals -> { File = refusal.File; Reason = refusal.Reason } |]
                      Notes = outcome.Joined |> Option.map (_.AllNotes >> Array.ofList) |> Option.defaultValue [||]
                      After = outcome.After |> Option.map compliance |> Option.toObj
                      Failures = failures |> List.map FailureDetail.Of |> Array.ofList
                      Warnings = warnings |> List.map FailureDetail.Of |> Array.ofList
                      StructureDifferences = Array.ofList outcome.Differences }
                write args.Report (if args.Format = "json" then toJson result else appendText result)
                match result.Status with
                | AppendStatus.Joined -> ExitCodes.Success
                | AppendStatus.Fails -> ExitCodes.DocumentProblem
                | _ -> ExitCodes.ToolError

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
        with ex ->
            // A download failing or timing out, the file or the models unreadable, anything else.
            eprintfn $"error: {failure ex}"
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
            runFixes
                (VeraPdfValidator())
                [ args.File ]
                { Ua = args.Ua
                  Fixes = fixes
                  Review = review |> Option.bind Result.toOption
                  ReviewAsked = args.Edits
                  Layout = Some layout
                  Language = None
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

/// The language packs installed (see LanguagePacks), where they're read from, and what's wrong
/// with any: exit 1 if something is.
let private listLanguages (format: string) =
    let roots = LanguagePacks.roots ()
    let packs, problems = LanguagePacks.load roots
    if format = "json" then
        Console.WriteLine(
            toJson
                {| Folders = roots
                   Languages =
                    [ for pack in packs ->
                          {| Tag = pack.Tag
                             Name = pack.Name
                             Folders = pack.Folders
                             Ocr = pack.Ocr |> Option.toObj
                             Phrases = pack.Phrases.Keys |> Array.ofSeq
                             SentenceContinuations = pack.SentenceContinuations
                             StandaloneHeadings = pack.StandaloneHeadings |} ]
                   Problems = [ for problem in problems -> {| Folder = problem.Folder; Message = problem.Message |} ] |}
        )
    else
        for pack in packs do
            let has =
                [ if pack.Ocr.IsSome then "OCR"
                  if not pack.Phrases.IsEmpty then $"{pack.Phrases.Count} of {LanguagePacks.phrases.Count} phrases"
                  if not pack.SentenceContinuations.IsEmpty then "sentence continuations"
                  if not pack.StandaloneHeadings.IsEmpty then "standalone headings" ]
            printfn "%-8s %-12s %s" pack.Tag pack.Name (String.Join(", ", has))
            for folder in pack.Folders do
                printfn "         %s" folder
        printfn ""
        printfn "Packs are read from (a folder per language, named for its tag, holding language.yaml):"
        for root in roots do
            printfn "  %s" root
        if not problems.IsEmpty then
            printfn ""
            printfn "Left out:"
            for problem in problems do
                printfn "  %s: %s" problem.Folder problem.Message
    if problems.IsEmpty then ExitCodes.Success else ExitCodes.DocumentProblem

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
  legibyl append intro.pdf newsletter.pdf plan-summary.pdf --output packet.pdf
  legibyl layout suggest scan.pdf --output scan.layout.json --figure-images
  legibyl layout apply scan.pdf --layout scan.layout.json --edits-from scan.edits.json --fixes safe,qr-links
  legibyl list fixes --format json
"""

/// The version line, as the GUI's About window gives it too (see BuildInfo), then the copyright.
let private printVersion _ =
    printfn $"{BuildInfo.versionLine}"
    printfn $"{BuildInfo.copyright} <{BuildInfo.contact}>"
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
                and! allWarnings =
                    option<bool> "--all-warnings"
                    |> desc "Report every warning, those the review dismisses too (they're counted in dismissedWarnings either way)."
                and! review =
                    optionMaybe<string> "--review"
                    |> desc
                        "Leave out the warnings this review dismisses (a .legibyl.json file, or a PDF whose review to use), instead of                          each file's own: the file next to it, or the review inside a copy Legibyl saved."
                    |> helpName "path"
                return
                    { Files = files
                      Ua = ua
                      Format = format
                      Output = output
                      Structure = structure
                      Verbose = verbose
                      AllWarnings = allWarnings
                      Review = review }
            }
        )
        setAction runValidate
    }

let private fixCommand =
    command "fix" {
        description
            "Fix PDFs, writing each one's next version (\"<name>.v2.pdf\"; \"<name>.v3.pdf\" for \"<name>.v2.pdf\"), and validate the copy."
        inputs (
            input {
                let! files = filesArgument ()
                and! fixes = fixesOption "safe"
                and! edits = editsOption ()
                and! editsFrom = editsFromOption ()
                and! lang =
                    optionMaybe "--lang"
                    |> desc "The document's language, e.g. es-MX: declared (the language fix), in place of a review's or any it says."
                    |> helpName "tag"
                and! output = optionMaybe "--output" |> desc "The fixed copy's path (one file only)." |> helpName "path"
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
                      Lang = lang
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

let private appendCommand =
    command "append" {
        description
            "Join PDFs that pass PDF/UA into one, in the order given, keeping each one's tags as they are (each becomes a \
             part of the joined file), and check the result: veraPDF, Legibyl's warnings, and each part's structure \
             against its file's. No fixes or reading-order guesses are made. If any file fails PDF/UA or can't be joined (inconsistent tags, a form, layers, \
             attached files), nothing is written."
        inputs (
            input {
                let! files = filesArgument ()
                and! output =
                    optionMaybe "--output"
                    |> desc "The joined file's path. Default: the first file's next version (\"<name>.v2.pdf\")."
                    |> helpName "path"
                and! title = optionMaybe "--title" |> desc "The joined file's title. Default: the first file's." |> helpName "text"
                and! lang =
                    optionMaybe "--lang"
                    |> desc "The joined file's language, e.g. en-US. Default: the first file's. A part in another language says so."
                    |> helpName "tag"
                and! partTag =
                    optionMaybe "--part-tag"
                    |> desc "The tag each file's part gets: Sect or Part for PDF/UA-1 (default Sect); DocumentFragment, Sect or Part for PDF/UA-2 (default DocumentFragment)."
                    |> helpName "tag"
                and! format = formatOption ()
                and! report = reportOption ()
                and! verbose = verboseOption ()
                return
                    { Files = files
                      Output = output
                      Title = title
                      Lang = lang
                      PartTag = partTag
                      Format = format
                      Report = report
                      Verbose = verbose }
            }
        )
        setAction runAppend
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
                description "The fixes 'fix --fixes' can name, with what each does."
                inputs (listFormatOption ())
                setAction listFixes
            }
        )
        addCommand (
            command "languages" {
                description "The language packs installed (English and Spanish are built in), where packs are read from, and what's wrong with any. Exits 1 if a pack has a problem."
                inputs (listFormatOption ())
                setAction listLanguages
            }
        )
    }

[<EntryPoint>]
let main argv =
    let root =
        ManualInvocation.rootCommand {
            description
                "legibyl - PDF/UA validator and fixer (CLI), powered by veraPDF via IKVM.\n\n\
                 Designed for scripting and AI-agent use: output is machine-readable JSON on stdout by default, and\n\
                 the exit code tells you the outcome without needing to parse output."
            noAction
            addCommand validateCommand
            addCommand fixCommand
            addCommand structureCommand
            addCommand appendCommand
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
    | _ when argv.Length = 0 -> usageError "Name a command: validate, fix, structure, append, layout or list."
    | _ ->
        // Let unexpected exceptions crash rather than exit with a code that means something else.
        parseResult.Invoke(InvocationConfiguration(EnableDefaultExceptionHandler = false))
