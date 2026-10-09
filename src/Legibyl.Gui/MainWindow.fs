namespace Legibyl.Gui

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Data
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Input.Platform
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform.Storage
open Avalonia.Styling
open Legibyl.Layout
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Gui.BusyStatus

/// What the document windows (one per PDF) share: the reviews of the PDFs open, and the
/// structures read.
[<Sealed>]
type DocumentShell() =
    member val Reviews = DocumentReviews()
    member val Structures = StructureCache()
    /// The app's.
    static member val Shared = DocumentShell()

/// A PDF for a window to open, and what's told its report once it's validated.
type OpenRequest = { Path: string; Opened: TaskCompletionSource<ValidationReport option> }

module private MainView =
    let pdfFileType =
        FilePickerFileType(
            "PDF files",
            Patterns = [ "*.pdf" ],
            MimeTypes = [ "application/pdf" ],
            AppleUniformTypeIdentifiers = [ "com.adobe.pdf" ]
        )

    type Outcome =
        | NotValidated
        | Validated of ValidationReport * RuleViolationRow list
        | ValidationFailed of string

    type Preview =
        { Image: Bitmap option
          Note: string
          Loading: bool }

    let noPreview = { Image = None; Note = ""; Loading = false }

    /// What Export writes besides the chosen auto fixes: how many of the user's changes aren't in
    /// the PDF yet (edits of its elements; for an untagged PDF, its edits on the suggested structure),
    /// and whether it tags an untagged PDF from that review.
    type PendingChanges = { Changes: int; TagsFromReview: bool }

    let noChanges = { Changes = 0; TagsFromReview = false }

    /// A menu item's command (see MainMenu.command).
    let command = MainMenu.command

    let createBitmap (pngBytes: byte array) =
        use stream = new MemoryStream(pngBytes)
        new Bitmap(stream)

    open Legibyl.Gui.Icons
    open Legibyl.Gui.FindingDetails

    /// Equal-width digits, so a number that changes doesn't change width.
    let private tabularFigures: IAttr<TextBlock> =
        let features = FontFeatureCollection()
        features.Add(FontFeature.Parse "tnum")
        AttrBuilder<TextBlock>.CreateProperty<FontFeatureCollection | null>(TextBlock.FontFeaturesProperty, features, ValueNone)

    let private formatElapsed (elapsed: TimeSpan) =
        if elapsed.TotalHours >= 1.0 then elapsed.ToString(@"h\:mm\:ss") else elapsed.ToString(@"m\:ss")

    /// e.g. "Validating: page 57 of 675   Checks 394,185   Rules failed 1   Elapsed 0:07": each value sits
    /// in a fixed-width slot with equal-width digits, so the line doesn't shift as it updates.
    let private progressLine (progress: BusyProgress) : IView =
        let secondaryText = Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
        let slot (width: float) (alignment: TextAlignment) (text: string) =
            TextBlock.create [
                TextBlock.minWidth width
                TextBlock.textAlignment alignment
                TextBlock.text text
                tabularFigures
                secondaryText
            ]
        let stat (label: string) (width: float) (value: string) =
            StackPanel.create [
                StackPanel.orientation Orientation.Horizontal
                StackPanel.margin (16.0, 0.0, 0.0, 0.0)
                StackPanel.children [
                    TextBlock.create [ TextBlock.text label; secondaryText ]
                    slot width TextAlignment.Right value
                ]
            ]
        let status = statusText progress
        let stats =
            match progress with
            | Validating(_, progress) ->
                [ stat "Checks" 80.0 (progress.Checks.ToString "N0")
                  stat "Rules failed" 32.0 (progress.FailedRules.ToString "N0")
                  stat "Elapsed" 48.0 (formatElapsed progress.Elapsed) ]
            | ReadingStructure progress ->
                [ stat "Elements" 64.0 (progress.Elements.ToString "N0")
                  stat "Elapsed" 48.0 (formatElapsed progress.Elapsed) ]
            | _ -> []
        WrapPanel.create [
            WrapPanel.margin (0.0, 4.0, 0.0, 0.0)
            WrapPanel.children [ yield slot 210.0 TextAlignment.Left status :> IView; for stat in stats do yield stat :> IView ]
        ]

    /// The bar fills page by page, which is most of the time for both validating (veraPDF
    /// checking pages) and reading the structure (extracting page text). It's full for the
    /// short steps after that, and indeterminate before the first page.
    let private progressBarState (progress: BusyProgress option) : IAttr<ProgressBar> list =
        let fraction page pageCount =
            [ ProgressBar.isIndeterminate false
              ProgressBar.maximum (float pageCount)
              ProgressBar.value (float (page - 1)) ]
        let full = [ ProgressBar.isIndeterminate false; ProgressBar.maximum 1.0; ProgressBar.value 1.0 ]
        match progress with
        | Some(Validating(_, progress)) when progress.Stage = ValidationStage.CheckingPages && progress.PageCount > 0 ->
            fraction progress.Page progress.PageCount
        | Some(Validating(_, progress)) when progress.Stage = ValidationStage.Finishing -> full
        | Some(ReadingStructure progress) when progress.Stage = StructureReadStage.ReadingText && progress.PageCount > 0 ->
            fraction progress.Page progress.PageCount
        | Some(ReadingStructure _) -> full
        | Some(DownloadingLayoutModel fraction) -> [ ProgressBar.isIndeterminate false; ProgressBar.maximum 1.0; ProgressBar.value fraction ]
        | Some(Fixing progress) when progress.Page > 0 && progress.PageCount > 0 -> fraction progress.Page progress.PageCount
        | Some(Fixing _) -> [ ProgressBar.isIndeterminate true ]
        | _ -> [ ProgressBar.isIndeterminate true ]

    /// A document window's view: one PDF. opened is told the PDF it opens; openElsewhere opens
    /// another PDF (in the window showing it, or a new one), with that window and its report;
    /// told the PDF has changed, a window showing it validates it again.
    let view
        (window: Window)
        (shell: DocumentShell)
        (openRequests: IReadable<OpenRequest option>)
        (opened: string -> unit)
        (openElsewhere: string -> bool -> Task<Window * ValidationReport option>)
        =
        Component(fun ctx ->
            let validator = ctx.useStateLazy(fun () -> VeraPdfValidator()).Current
            let renderer = ctx.useStateLazy(fun () -> new PdfRegionRenderer()).Current
            let structureCache = shell.Structures
            let layoutReviews = shell.Reviews
            let selectedFile = ctx.useState (None: string option)
            let isBusy = ctx.useState false
            let busyProgress = ctx.useState (None: BusyProgress option)
            let outcome = ctx.useState NotValidated
            let selectedRow = ctx.useState (None: RuleViolationRow option)
            // A rule's row, when that's what's selected (its first finding is selectedRow, for its
            // preview); and the rules opened or closed, by key (see RuleGroup.ExpandedAtFirst).
            let selectedRule = ctx.useState (None: RuleGroup option)
            let ruleExpanded = ctx.useState (Map.empty: Map<string, bool>)
            // When the results shown were checked, if they're the review's saved ones (see
            // SavedResults); None for a validation just run.
            let checkedAt = ctx.useState (None: DateTime option)
            // For a PDF that passes: whether its Pass row is what's selected, what its checklist
            // shows of it (read once it passes), and a tick's change, to show it.
            let passSelected = ctx.useState false
            // For a PDF that fails: whether its Next step row is what's selected.
            let nextSelected = ctx.useState false
            let passFacts = ctx.useState (None: DocumentFacts.Facts option)
            // Why the checklist couldn't be read, if it couldn't.
            let passFactsFailed = ctx.useState (None: string option)
            let checklistTicks = ctx.useState 0
            let preview = ctx.useState noPreview
            // Bumped on every selection change so a slow page render for an earlier
            // selection can't overwrite the preview for the current one.
            let previewVersion = ctx.useState (0, false)
            // Warnings dismissed for the selected file (in its review, and any carried inside it),
            // whether the results list shows them, and whether its review has changes not saved.
            let dismissed = ctx.useState (Map.empty: Dismissals.Dismissed)
            let showDismissed = ctx.useState false
            let edited = ctx.useState false
            // What was just done (the review saved, a copy exported), announced to a screen reader.
            let notice = ctx.useState ""
            // The fixes chosen in Auto Fixes for the selected file, as its review has them (None
            // until they're chosen), and the user's changes Export writes too.
            let autoFixes = ctx.useState (None: PdfFixOptions option)
            let pending = ctx.useState noChanges
            // The last count of edits not in a tagged PDF: (path, its last write, edits, count).
            // Counting reads the PDF, so it's only done again when one of those changes.
            let pendingCount = ctx.useStateLazy(fun () -> ref (None: (string * DateTime * ElementEdit list * string option * string option * Map<string, string> * Map<string, Attachments.AttachmentEdit> * int) option)).Current
            // The Structure window open for each PDF, and those being read or opened.
            let structureWindows = ctx.useStateLazy(fun () -> Collections.Generic.Dictionary<string, Window>()).Current
            let structureOpening = ctx.useStateLazy(fun () -> Collections.Generic.HashSet<string>()).Current
            // What's running that the progress row's Cancel stops (validating, reading the
            // structure, Export…, Append PDFs…), and what had the focus before it, given back after.
            let cancelSource = ctx.useState (None: CancellationTokenSource option)
            let focusBefore = ctx.useStateLazy(fun () -> ref (None: IInputElement option)).Current

            /// Starts something Cancel can stop, returning what stops it. Cancel takes the focus while
            /// it runs (the button that started it is turned off), so the keyboard stays where it is.
            let cancellable () =
                let source = new CancellationTokenSource()
                focusBefore.Value <- Option.ofObj (window.FocusManager.GetFocusedElement())
                cancelSource.Set(Some source)
                // Once it's shown.
                Avalonia.Threading.Dispatcher.UIThread.Post(
                    (fun () ->
                        Avalonia.VisualTree.VisualExtensions.GetVisualDescendants window
                        |> Seq.tryPick (fun (visual: Visual) ->
                            match visual with
                            | :? Button as button when (button.Tag :? string) && string button.Tag = "cancel-work" -> Some button
                            | _ -> None)
                        |> Option.iter (fun button -> button.Focus() |> ignore)),
                    Avalonia.Threading.DispatcherPriority.Background
                )
                source

            /// What source stops is done (or stopped): Cancel goes, and the focus goes back where it
            /// was, if that can take it again.
            let finished (source: CancellationTokenSource) =
                source.Dispose()
                // (Once: not again, after something else has the focus, if it's said twice.)
                match cancelSource.Current with
                | Some current when obj.ReferenceEquals(current, source) ->
                    cancelSource.Set None
                    let before = focusBefore.Value
                    focusBefore.Value <- None
                    // Once what was turned off while busy is turned on again.
                    Avalonia.Threading.Dispatcher.UIThread.Post(
                        (fun () ->
                            match before with
                            | Some(:? InputElement as element) when element.IsEffectivelyEnabled && element.IsEffectivelyVisible -> element.Focus() |> ignore
                            | _ -> ()),
                        Avalonia.Threading.DispatcherPriority.Background
                    )
                | _ -> ()


            let report =
                match outcome.Current with
                | Validated(report, _) -> Some report
                | _ -> None

            /// Whether the PDF has no structure tree: it's tagged from a suggested structure.
            let untagged () = report |> Option.exists (fun r -> r.FixableCount PdfFixOptions.TagFromLayout > 0)

            // For a PDF that passes: its checklist (once what it shows is read), and the steps ticked
            // for this very file (see DocumentReview.CheckedFor).
            let checklist () : (ChecklistStep list * Set<string>) option =
                match report, passFacts.Current, selectedFile.Current with
                | Some report, Some facts, Some path when report.Passes ->
                    let review = layoutReviews.For path
                    let ticked = review.Validation |> Option.map (fun v -> review.CheckedFor v.Hash) |> Option.defaultValue Set.empty
                    // With those checked in the copy it is, if it's one.
                    Some(Checklist.steps facts, Set.union ticked review.CarriedChecked)
                | _ -> None
            /// The steps ticked for this file that aren't in it yet (not carried inside it): what
            /// Export writes into a copy, when that's all it writes.
            let ticksToCarry () =
                match report, selectedFile.Current with
                | Some report, Some path when report.Passes ->
                    let review = layoutReviews.For path
                    match review.Validation with
                    | Some validation -> Set.difference (review.CheckedFor validation.Hash) review.CarriedChecked
                    | None -> Set.empty
                | _ -> Set.empty
            // What it passes: PDF/UA, and WCAG 2.2's machine checks, which run with every validation.
            let profileName =
                report |> Option.map (fun r -> $"{PdfUaTarget.displayName r.Profile} and {WcagSpecification.Name}") |> Option.defaultValue "PDF/UA"
            let passProgress =
                match report, checklist () with
                | Some report, _ when not report.Passes -> None
                | Some _, Some(steps, ticked) -> Some(Checklist.progress profileName steps ticked)
                | Some _, None -> Some $"Passes {profileName}'s automated checks"
                | None, _ -> None
            // Ticks one of the checklist's steps, or unticks it: a change to the review Save keeps.
            let toggleStep (id: string) =
                match selectedFile.Current with
                | Some path ->
                    let review = layoutReviews.For path
                    match review.Validation with
                    // Those checked in the copy it is stay checked.
                    | Some validation when not (review.CarriedChecked.Contains id) ->
                        let ticked = review.CheckedFor validation.Hash
                        review.Checklist <- Some(validation.Hash, (if ticked.Contains id then ticked.Remove id else ticked.Add id))
                        review.ChecklistAt <- Some DateTime.Now
                        edited.Set review.IsEdited
                        checklistTicks.Set(checklistTicks.Current + 1)
                    | _ -> ()
                | None -> ()
            /// Whether it's worth offering fixes right now. The current fixers target known
            /// validation failures, so hide them once the file is already compliant.
            let canAttemptFix = report |> Option.exists (fun r -> not r.Passes || r.WarningCount > 0)

            let clearResults () =
                outcome.Set NotValidated
                selectedRow.Set None
                selectedRule.Set None
                ruleExpanded.Set Map.empty
                checkedAt.Set None
                passSelected.Set false
                nextSelected.Set false
                passFacts.Set None
                passFactsFailed.Set None
                preview.Set noPreview
                previewVersion.Set(previewVersion.Current + 1)

            /// Selects a finding (its rule opened, so it's in the list), or, with asRule, the rule's
            /// row, showing its first finding's page; and shows its page.
            let selectFinding (row: RuleViolationRow option) (asRule: RuleGroup option) =
                task {
                    previewVersion.Set(previewVersion.Current + 1)
                    let version = previewVersion.Current
                    selectedRule.Set asRule
                    passSelected.Set false
                    nextSelected.Set false
                    match row, asRule with
                    | Some row, None -> ruleExpanded.Set(Map.add $"{row.Violation.RuleId}: {row.Message}" true ruleExpanded.Current)
                    | _ -> ()
                    selectedRow.Set row
                    preview.Set noPreview

                    match row with
                    | None -> ()
                    | Some row ->
                        let render =
                            match row.Violation.Geometry, row.Violation.Location with
                            | NonNull geometry, _ -> Some(fun filePath -> renderer.RenderPageWithHighlight(filePath, geometry))
                            | Null, NonNull location -> Some(fun filePath -> renderer.RenderPageWithHighlight(filePath, location))
                            | Null, Null -> None

                        match render, selectedFile.Current with
                        | Some render, Some filePath ->
                            preview.Set { noPreview with Loading = true }
                            try
                                // Decode the PNG off the UI thread too; a full page is a large image.
                                let! result, image = Task.Run(fun () -> let result = render filePath in result, createBitmap result.PngBytes)
                                if version = previewVersion.Current then
                                    preview.Set
                                        { Image = Some image
                                          Note = if result.HasPreciseRegion then "" else result.Note
                                          Loading = false }
                                else
                                    image.Dispose()
                            with :? InvalidOperationException | :? IOException as ex ->
                                if version = previewVersion.Current then
                                    preview.Set { noPreview with Note = $"The page preview could not be rendered: {ex.Message}" }
                        | _ -> preview.Set { noPreview with Note = "This finding is not associated with a specific PDF page." }
                }
                |> ignore

            let selectRow (row: RuleViolationRow option) = selectFinding row None

            /// Selects the Pass row, whose details are the checklist.
            let selectPass () =
                let selecting = selectFinding None None
                passSelected.Set true
                selecting

            /// Selects the Next step row, whose details are what to do next.
            let selectNext () =
                let selecting = selectFinding None None
                nextSelected.Set true
                selecting

            /// Selects what the list starts with: for a PDF that passes, the Pass row; for one that
            /// fails, the Next step row; else the first rule's first finding, or the rule's row if it
            /// starts collapsed.
            let selectFirst (rows: RuleViolationRow list) =
                let passes = match outcome.Current with Validated(report, _) -> report.Passes | _ -> false
                if passes then
                    selectPass ()
                elif rows |> List.exists (fun row -> row.Violation.Severity = FindingSeverity.Failure) then
                    selectNext ()
                else
                    match RuleGroup.OfRows(rows |> List.filter (fun row -> showDismissed.Current || not row.Dismissed)) with
                    | group :: _ when not group.ExpandedAtFirst -> selectFinding (Some group.First) (Some group)
                    | group :: _ -> selectRow (Some group.First)
                    | [] -> selectRow None

            /// Counts the user's changes Save would write for the selected file: for an untagged PDF,
            /// its edits on the suggested structure (regions merged into their neighbours, other edits, a title); otherwise its
            /// edits, and the title and role map given in Structure…, that aren't in it yet, which means reading it, off the UI thread.
            let refreshPending () =
                task {
                    selectedFile.Current |> Option.iter (fun path -> edited.Set (layoutReviews.For path).IsEdited)
                    match selectedFile.Current, outcome.Current with
                    | Some path, Validated(report, _) when report.FixableCount PdfFixOptions.TagFromLayout > 0 ->
                        let review = layoutReviews.For path
                        let changes = review.Edits.Length + (if review.Title.IsSome then 1 else 0) + (if review.Language.IsSome then 1 else 0)
                        // Tagged from on Export once it's been suggested (Suggest Structure… opened, or
                        // a layout kept from before), or there are edits on it.
                        pending.Set { Changes = changes; TagsFromReview = review.Pages.Count > 0 || changes > 0 }
                    | Some path, Validated _ ->
                        let! count =
                            Task.Run(fun () ->
                                try
                                    let review = layoutReviews.For path
                                    let edits = RegionEdits.toElementEdits review.Edits
                                    let title = review.Title |> Option.filter (String.IsNullOrWhiteSpace >> not)
                                    let language = review.Language
                                    let written = File.GetLastWriteTimeUtc path
                                    let roles = review.RoleMap
                                    let attached = review.Attachments
                                    match pendingCount.Value with
                                    | Some(p, w, e, t, l, r, a, count) when p = path && w = written && e = edits && t = title && l = language && r = roles && a = attached -> count
                                    | _ ->
                                        // Its edits, and the title, language and role map given in Structure…, that
                                        // aren't in it yet.
                                        let count =
                                            ElementEdits.pending path edits
                                            + (match title with Some title when ElementEdits.titlePending path title -> 1 | _ -> 0)
                                            + (match language with Some language when fst (LanguagePacks.ofFile path) <> Some language -> 1 | _ -> 0)
                                            + RoleMaps.pending path roles
                                            + Attachments.pending path attached
                                        pendingCount.Value <- Some(path, written, edits, title, language, roles, attached, count)
                                        count
                                with
                                | :? iText.Commons.Exceptions.ITextException
                                | :? IOException
                                | :? UnauthorizedAccessException -> 0)
                        if selectedFile.Current = Some path then
                            pending.Set { Changes = count; TagsFromReview = false }
                    | _ -> pending.Set noChanges
                }
                |> ignore

            // Edits are made in other windows; count them again whenever this one is back in front.
            ctx.useEffect ((fun () -> window.Activated.Add(fun _ -> refreshPending ())), [ EffectTrigger.AfterInit ])

            /// Validates the selected file, returning its report (None if it couldn't be validated).
            /// activity names it in the progress row ("Validating", "Validating the fixed copy").
            /// Validates the selected file, or, unless fresh, shows the result its review saved for
            /// these exact bytes (see SavedResults). A validation run is a change to the review, as an
            /// edit is: Save keeps it, so the file opens with it next time.
            let runValidation (activity: string) (fresh: bool) : Task<ValidationReport option> =
                task {
                    match selectedFile.Current with
                    | Some filePath when not isBusy.Current ->
                        isBusy.Set true
                        clearResults ()
                        // Said from the start, not a bare bar until veraPDF's first report.
                        busyProgress.Set(
                            Some(
                                Validating(
                                    activity,
                                    { Stage = ValidationStage.Preparing
                                      Page = 0
                                      PageCount = 0
                                      Checks = 0
                                      FailedRules = 0
                                      ProcessedObjects = 0
                                      QueuedObjects = 0
                                      Elapsed = TimeSpan.Zero }
                                )
                            )
                        )
                        let review = layoutReviews.For filePath
                        let saved = if fresh then None else review.Validation
                        // Created on the UI thread, so its reports arrive there too.
                        let progress = Progress<ValidationProgress>(fun p -> busyProgress.Set(Some(Validating(activity, p))))
                        let source = cancellable ()
                        try
                            try
                                // Against the PDF/UA its PDF version calls for (see PdfUaTarget).
                                let! report, hash, fromSaved =
                                    Task.Run(fun () ->
                                        let profile, _ = PdfUaTarget.detect filePath
                                        let hash = SavedResults.hashOf filePath
                                        match saved with
                                        | Some saved when SavedResults.isCurrent saved hash profile ->
                                            SavedResults.toReport saved filePath, hash, Some saved.CheckedAt
                                        | _ -> { validator.Validate(filePath, profile, progress, cancel = source.Token) with ProfileDetected = true }, hash, None)
                                checkedAt.Set fromSaved
                                let rows =
                                    report.Findings
                                    |> Seq.map RuleViolationRow.Of
                                    // Unlocated failures (no page number) sort to the end, after page-anchored ones in page order.
                                    |> Seq.sortBy (fun row -> row.PageNumber |> Option.defaultValue Int32.MaxValue, row.Clause, row.TestNumber)
                                    |> Seq.map (fun row -> { row with Dismissed = Dismissals.isDismissed dismissed.Current row.Violation })
                                    |> List.ofSeq
                                let rows =
                                    let counts = rows |> List.countBy (fun r -> r.Clause, r.TestNumber, r.Message) |> Map.ofList
                                    rows |> List.map (fun r -> { r with Occurrences = counts[(r.Clause, r.TestNumber, r.Message)] })
                                match fromSaved with
                                | Some _ -> ()
                                | None ->
                                    review.Validation <- Some(SavedResults.ofReport report hash DateTime.Now)
                                    // A PDF that passes with every warning dismissed (or none) has
                                    // nothing left to do: its result alone isn't a change to keep.
                                    if report.Passes && rows |> List.forall _.Dismissed then review.TakeValidationAsSaved()
                                    edited.Set review.IsEdited
                                outcome.Set(Validated(report, rows))
                                refreshPending ()
                                selectFirst rows
                                // What a person checks for a PDF that passes: read in the background.
                                if report.Passes then
                                    Task.Run(fun () -> try Ok(DocumentFacts.read filePath) with ex -> Error(Dialogs.failureMessage ex))
                                        .ContinueWith((fun (t: Task<Result<DocumentFacts.Facts, string>>) ->
                                            if selectedFile.Current = Some filePath then
                                                match t.Result with
                                                | Ok facts -> passFacts.Set(Some facts)
                                                | Error message -> passFactsFailed.Set(Some message)),
                                            TaskScheduler.FromCurrentSynchronizationContext())
                                    |> ignore
                                return Some report
                            with
                            | :? OperationCanceledException when source.IsCancellationRequested ->
                                outcome.Set NotValidated
                                notice.Set "Cancelled: it hasn't been checked. Check Again checks it."
                                return None
                            | :? VeraPdfValidationException as ex ->
                                outcome.Set(ValidationFailed ex.Message)
                                return None
                            // The file locked or gone while it's read, say: said, not an empty window.
                            | ex ->
                                outcome.Set(ValidationFailed $"Legibyl couldn't check it: {Dialogs.failureMessage ex}")
                                return None
                        finally
                            isBusy.Set false
                            finished source
                    | _ -> return None
                }

            /// Opens a file and validates it with the current profile, naming that activity.
            let openFileAs (activity: string) (path: string) : Task<ValidationReport option> =
                task {
                    selectedFile.Set(Some path)
                    opened path
                    clearResults ()
                    pending.Set noChanges
                    // Its review read in the background: the review file, the copy's own, and the
                    // layout kept with it (each page's text read to see which haven't changed).
                    isBusy.Set true
                    busyProgress.Set(Some(Working $"Opening {Path.GetFileName path}…"))
                    let! review = Task.Run(fun () -> try Ok(layoutReviews.For path) with ex -> Error(Dialogs.failureMessage ex))
                    busyProgress.Set None
                    isBusy.Set false
                    showDismissed.Set false
                    match review with
                    | Ok review ->
                        autoFixes.Set review.AutoFixes
                        dismissed.Set review.Dismissed
                        edited.Set review.IsEdited
                        return! runValidation activity false
                    // Said, rather than reading it again on the UI thread to fail the same way.
                    | Error message ->
                        outcome.Set(ValidationFailed $"Legibyl couldn't open it: {message}")
                        return None
                }

            /// Keeps the selected file's review in the file next to it; false if it couldn't be
            /// written (and says why).
            let saveReview () =
                task {
                    match selectedFile.Current with
                    | Some path ->
                        try
                            // (Recording the layout reads each page's text: off the UI thread.)
                            let review = layoutReviews.For path
                            let wasBusy = isBusy.Current
                            isBusy.Set true
                            busyProgress.Set(Some(Working "Saving the review…"))
                            try
                                do! Task.Run(fun () -> review.Save path)
                            finally
                                busyProgress.Set None
                                isBusy.Set wasBusy
                            edited.Set false
                            notice.Set $"Saved the review in {Path.GetFileName(ReviewFile.filePath path)}."
                            return true
                        with :? IOException | :? UnauthorizedAccessException as ex ->
                            do! Dialogs.showMessage window $"Couldn't save the review next to the PDF: {ex.Message}" "Save"
                            return false
                    | None -> return true
                }

            /// Whether the selected file's review may be left: it has no changes not saved, or the
            /// user saves them, or chooses not to (and they're dropped).
            let mayLeave () =
                task {
                    match selectedFile.Current with
                    | Some path when (layoutReviews.For path).NeedsSaving ->
                        match! Dialogs.askToSave window (string (Path.GetFileName path)) with
                        | Some true -> return! saveReview ()
                        | Some false ->
                            layoutReviews.Forget path
                            edited.Set false
                            return true
                        | None -> return false
                    | _ -> return true
                }

            /// Opens another file, after asking about the selected one's changes not saved.
            let switchTo (activity: string) (path: string) =
                task {
                    let same = selectedFile.Current |> Option.exists (fun current -> Path.GetFullPath current = Path.GetFullPath path)
                    let! leave = if same then Task.FromResult true else mayLeave ()
                    if leave then return! openFileAs activity path else return None
                }

            /// Opens a PDF picked or dropped: here if this window has none yet (or has it), else in
            /// its own window.
            let setSelectedFile (path: string) =
                match selectedFile.Current with
                | Some current when Path.GetFullPath current <> Path.GetFullPath path -> openElsewhere path false |> ignore
                | _ -> openFileAs "Validating" path |> ignore

            // Closing with changes not saved asks about them first.
            let closing = ctx.useState (false, false)
            ctx.useEffect (
                (fun () ->
                    window.Closing.Add(fun e ->
                        let asks = selectedFile.Current |> Option.exists (fun path -> (layoutReviews.For path).NeedsSaving)
                        if asks && not closing.Current then
                            e.Cancel <- true
                            task {
                                let! leave = mayLeave ()
                                if leave then
                                    closing.Set true
                                    window.Close()
                            }
                            |> ignore)),
                [ EffectTrigger.AfterInit ]
            )

            // PDFs this window is asked to open (a new window's, or the headless harness's).
            ctx.useEffect (
                (fun () ->
                    match openRequests.Current with
                    | Some request ->
                        task {
                            let! report = switchTo "Validating" request.Path
                            request.Opened.TrySetResult report |> ignore
                        }
                        |> ignore
                    | None -> ()),
                [ EffectTrigger.AfterChange openRequests ]
            )

            let browse _ =
                task {
                    let! files =
                        window.StorageProvider.OpenFilePickerAsync(
                            FilePickerOpenOptions(
                                Title = "Select a PDF to validate",
                                AllowMultiple = false,
                                FileTypeFilter = [ pdfFileType; FilePickerFileTypes.All ]
                            )
                        )
                    if files.Count > 0 then
                        match files[0].TryGetLocalPath() with
                        | NonNull path -> setSelectedFile path
                        | Null -> ()
                }
                |> ignore

            let onDrop (e: DragEventArgs) =
                match e.DataTransfer.TryGetFiles() with
                | NonNull files when files.Length > 0 ->
                    match files[0].TryGetLocalPath() with
                    | NonNull path -> setSelectedFile path
                    | Null -> ()
                | _ -> ()

            /// Selects a finding in the results list, opening its rule and scrolling to it, and
            /// brings this window forward: from a marker in the Structure window.
            let showFinding (violation: RuleViolation) =
                match outcome.Current with
                | Validated(_, rows) ->
                    match rows |> List.tryFind (fun row -> obj.ReferenceEquals(row.Violation, violation)) with
                    | Some row ->
                        // Its rule is opened, and the list scrolls to it as it's selected.
                        selectRow (Some row) |> ignore
                        window.Activate()
                    | None -> ()
                | _ -> ()

            /// The failures and warnings the Structure window marks on its blocks: those not dismissed.
            let findingsToMark () : FindingMarkers.Findings =
                match outcome.Current with
                | Validated(_, rows) ->
                    { Items = rows |> List.filter (fun row -> not row.Dismissed) |> List.map _.Violation
                      Show = showFinding }
                | _ -> FindingMarkers.none

            let viewStructure _ =
                task {
                    // One Structure window per PDF: asked again, the open one's brought forward. (A
                    // second click while it's still being read or opened does nothing.)
                    let focusOpen filePath =
                        match structureWindows.TryGetValue filePath with
                        | true, (open': Window) ->
                            if open'.WindowState = WindowState.Minimized then open'.WindowState <- WindowState.Normal
                            open'.Activate()
                            true
                        | _ -> false
                    let open' (filePath: string) (structureWindow: Window) =
                        structureWindows[filePath] <- structureWindow
                        structureWindow.Closed.Add(fun _ ->
                            match structureWindows.TryGetValue filePath with
                            | true, w when obj.ReferenceEquals(w, structureWindow) -> structureWindows.Remove filePath |> ignore
                            | _ -> ())
                        structureWindow.Show window
                    let show (structure: StructureNode | null) (filePath: string) =
                        task {
                            match structure with
                            | null ->
                                // Untagged: the button says Suggest Structure…, and that's what it does.
                                let review = layoutReviews.For filePath
                                // What finds its pages' layouts reads the PDF: off the UI thread.
                                isBusy.Set true
                                busyProgress.Set(Some(Working "Opening the suggested structure…"))
                                try
                                    let! suggester = Task.Run(fun () -> new LayoutSuggester(filePath, review.Language))
                                    open' filePath (StructureTreeWindow(Suggested, filePath, review, refreshPending, FindingMarkers.none, Some suggester))
                                finally
                                    busyProgress.Set None
                                    isBusy.Set false
                            | structure ->
                                open' filePath (StructureTreeWindow(Tagged structure, filePath, layoutReviews.For filePath, refreshPending, findingsToMark ()))
                        }

                    match selectedFile.Current with
                    | Some filePath when focusOpen filePath || structureOpening.Contains filePath -> ()
                    | Some filePath ->
                        structureOpening.Add filePath |> ignore
                        try
                            try
                                match structureCache.TryFind filePath with
                                | Some structure -> do! show structure filePath
                                | None ->
                                    isBusy.Set true
                                    busyProgress.Set(Some(Working "Reading the structure…"))
                                    // Created on the UI thread, so its reports arrive there too.
                                    let progress = Progress<StructureReadProgress>(fun p -> busyProgress.Set(Some(ReadingStructure p)))
                                    let stamp = structureCache.Stamp filePath
                                    let source = cancellable ()
                                    try
                                        try
                                            let! structure = Task.Run(fun () -> validator.ReadStructureTree(filePath, progress, source.Token))
                                            structureCache.Remember(stamp, structure)
                                            isBusy.Set false
                                            finished source
                                            do! show structure filePath
                                        with
                                        | :? OperationCanceledException when source.IsCancellationRequested ->
                                            notice.Set "Cancelled: Structure… wasn't opened."
                                        | :? VeraPdfValidationException as ex ->
                                            do! Dialogs.showMessage window ex.Message "Could not read structure"
                                    finally
                                        isBusy.Set false
                                        finished source
                            // Reading it, or opening its window, failing some other way: said.
                            with ex ->
                                do! Dialogs.showMessage window $"Structure… couldn't open it: {Dialogs.failureMessage ex}" "Structure"
                        finally
                            structureOpening.Remove filePath |> ignore
                    | None -> ()
                }
                |> ignore

            /// Whether a title's been typed for the file in Structure…: Export… writes it before the auto fixes.
            let titleGiven path = (layoutReviews.For path).Title |> Option.exists (String.IsNullOrWhiteSpace >> not)

            /// Chooses the fixes Export applies (it doesn't export).
            let chooseAutoFixes _ =
                task {
                    match selectedFile.Current, report with
                    | Some sourcePath, Some report ->
                        let review = layoutReviews.For sourcePath
                        // An untagged PDF is tagged from its suggested structure, not by a fix.
                        let reviewNote =
                            if untagged () then
                                Some "This PDF has no tags: Export… tags a copy from its suggested structure (Suggest Structure…), with your edits on it."
                            else
                                None
                        match! Dialogs.chooseFixOptions window report reviewNote (titleGiven sourcePath) autoFixes.Current with
                        | Some options ->
                            // Part of the review: Save keeps them.
                            review.AutoFixes <- Some options
                            autoFixes.Set review.AutoFixes
                            edited.Set review.IsEdited
                        | None -> ()
                    | _ -> ()
                }
                |> ignore

            /// What Export writes: the chosen auto fixes, the user's edits not in the PDF yet, and for
            /// an untagged PDF with edits on its suggested structure, tags from them.
            let saveOptions () =
                // Tagging isn't an auto fix here: an untagged PDF is tagged from its suggested
                // structure, and only once that's been suggested.
                let chosen = autoFixes.Current |> Option.defaultValue PdfFixOptions.None &&& ~~~PdfFixOptions.TagFromLayout
                // A saved region may no longer pass its safety guards. Still run ApplyEdits
                // so Export explains the refusal instead of silently omitting the decision.
                let hasFigureRegions =
                    selectedFile.Current
                    |> Option.exists (fun path -> (layoutReviews.For path).Edits |> List.exists (fun e -> e.Tag = "FigureRegion"))
                if untagged () && not pending.Current.TagsFromReview then
                    PdfFixOptions.None
                else
                    chosen
                    ||| (if pending.Current.TagsFromReview then PdfFixOptions.TagFromLayout else PdfFixOptions.None)
                    ||| (if (pending.Current.Changes > 0 || hasFigureRegions) && not pending.Current.TagsFromReview then PdfFixOptions.ApplyEdits else PdfFixOptions.None)

            /// Writes the next version of the PDF with saveOptions (its review, saved or not, goes in
            /// it too), then opens it.
            let export _ =
                task {
                    match selectedFile.Current with
                    // Only steps a person checked: a copy of the PDF as it is, carrying them.
                    | Some sourcePath when saveOptions () = PdfFixOptions.None && not (ticksToCarry ()).IsEmpty ->
                        let review = layoutReviews.For sourcePath
                        let! outputPath =
                            Dialogs.pickSavePath
                                window
                                (FilePickerSaveOptions(
                                    Title = "Export a copy carrying the steps checked",
                                    SuggestedFileName = CopyNames.nextVersion sourcePath,
                                    DefaultExtension = "pdf",
                                    FileTypeChoices = [ pdfFileType; FilePickerFileTypes.All ]
                                ))
                                sourcePath
                        match outputPath, review.Validation with
                        | Some outputPath, Some validation ->
                            isBusy.Set true
                            busyProgress.Set(Some(Working "Exporting a copy carrying the steps checked…"))
                            let! written =
                                task {
                                    try
                                        try
                                            // The review as the copy reads it: leaving it then doesn't ask to save.
                                            let! exported =
                                                Task.Run(
                                                    Func<unit -> unit>(fun () ->
                                                        let exported = review.Exporting true
                                                        review.WriteCheckedCopy(sourcePath, outputPath, validation.Hash)
                                                        exported)
                                                )
                                            return Ok exported
                                        with
                                        | :? IOException
                                        | :? UnauthorizedAccessException as ex -> return Error ex.Message
                                        | :? iText.Commons.Exceptions.ITextException as ex -> return Error ex.Message
                                    finally
                                        busyProgress.Set None
                                        isBusy.Set false
                                }
                            match written with
                            | Ok exported ->
                                exported ()
                                notice.Set $"Exported {Path.GetFileName outputPath}, carrying the steps checked."
                                do! openElsewhere outputPath true :> Task
                            | Error message -> do! Dialogs.showMessage window $"The copy couldn't be written: {message}" "Export"
                        | _ -> ()
                    | Some sourcePath when saveOptions () <> PdfFixOptions.None ->
                        let options = saveOptions ()
                        let review = layoutReviews.For sourcePath
                        let! outputPath =
                            Dialogs.pickSavePath
                                window
                                (FilePickerSaveOptions(
                                    Title = "Export a copy with your changes",
                                    SuggestedFileName = CopyNames.nextVersion sourcePath,
                                    DefaultExtension = "pdf",
                                    FileTypeChoices = [ pdfFileType; FilePickerFileTypes.All ]
                                ))
                                sourcePath

                        match outputPath with
                        | None -> ()
                        | Some outputPath ->
                            // The review as it goes into the copy (taken as the copy reads it): leaving
                            // it then doesn't ask to save.
                            let exported = ref ignore
                            // The standard it was checked against, so the metadata fix writes its claim.
                            let profile =
                                match outcome.Current with
                                | Validated(report, _) -> report.Profile
                                | _ -> fst (PdfUaTarget.detect sourcePath)
                            isBusy.Set true
                            busyProgress.Set(Some(Working "Exporting…"))
                            let cancel = cancellable ()
                            let cancelled = ref false
                            let! fixOutcome =
                                task {
                                    // Tagging from the layout and joining split paragraphs run the layout
                                    // detector page by page, after downloading its model the first time.
                                    let usesLayout =
                                        options.HasFlag PdfFixOptions.TagFromLayout || options.HasFlag PdfFixOptions.JoinSplitParagraphs
                                    // The suggestions with the edits on them (all of them if
                                    // it wasn't opened), reusing any pages it found. (Made inside
                                    // the try: reading the PDF for them can fail too.)
                                    let source = ref None
                                    try
                                        try
                                            if usesLayout then
                                                // (It reads the PDF: off the UI thread.)
                                                let! suggester = Task.Run(fun () -> new LayoutSuggester(sourcePath, review.Language))
                                                source.Value <- Some(suggester, ReviewedLayout(LayoutPrefetcher(suggester, review.Pages), review))
                                            if usesLayout && not (LayoutModel.isDownloaded ()) then
                                                do!
                                                    LayoutModel.download
                                                        (Progress<float>(fun fraction -> busyProgress.Set(Some(DownloadingLayoutModel fraction))))
                                                        cancel.Token
                                            source.Value |> Option.iter (fun (_, reviewed) -> validator.LayoutSource <- reviewed)
                                            let progress = Progress<FixProgress>(fun p -> busyProgress.Set(Some(Fixing p)))
                                            // The report shown, validated here (a saved one may not keep all a
                                            // fix reads), of the file as it is still: not validated again.
                                            let shown =
                                                match report, checkedAt.Current, review.Validation with
                                                | Some report, None, Some validation -> Some(report, validation.Hash)
                                                | _ -> None
                                            let! fix =
                                                Task.Run(fun () ->
                                                    // The review, as it is now, goes in the fixed copy. Recording
                                                    // its layout reads the PDF, so it's done here too.
                                                    exported.Value <- review.Exporting false
                                                    validator.Review <- review.ForCopy sourcePath
                                                    validator.Edits <- RegionEdits.toElementEdits review.Edits
                                                    validator.Title <- (review.Title |> Option.filter (String.IsNullOrWhiteSpace >> not) |> Option.toObj)
                                                    validator.Language <- Option.toObj review.Language
                                                    validator.Validated <-
                                                        shown
                                                        |> Option.filter (fun (_, hash) -> (try SavedResults.hashOf sourcePath with _ -> "") = hash)
                                                        |> Option.map fst
                                                    validator.RoleMap <- Collections.Generic.Dictionary<string, string>(review.RoleMap)
                                                    validator.Attachments <- Collections.Generic.Dictionary<string, Attachments.AttachmentEdit>(review.Attachments)
                                                    validator.Fix(sourcePath, profile, outputPath, options, progress, cancel.Token))
                                            return Ok fix
                                        with
                                        // (Before the download's timing out, which is a cancellation too.)
                                        | :? OperationCanceledException when cancel.IsCancellationRequested ->
                                            cancelled.Value <- true
                                            return Error "Cancelled."
                                        | :? VeraPdfValidationException as ex -> return Error ex.Message
                                        | :? Net.Http.HttpRequestException as ex -> return Error $"Couldn't download the layout model: {ex.Message}"
                                        | :? TaskCanceledException -> return Error "Couldn't download the layout model: the connection timed out."
                                        // Anything else is said too, rather than leaving nothing happened.
                                        | ex -> return Error $"Couldn't export it: {Dialogs.failureMessage ex}"
                                    finally
                                        // The busy state ends first, whatever letting go of the
                                        // suggester does after.
                                        busyProgress.Set None
                                        isBusy.Set false
                                        finished cancel
                                        validator.LayoutSource <- null
                                        validator.Review <- null
                                        validator.Edits <- []
                                        validator.Validated <- None
                                        try
                                            source.Value |> Option.iter (fun (suggester, _) -> (suggester :> IDisposable).Dispose())
                                        with _ ->
                                            ()
                                }

                            // Open the exported copy in its own window, where it's validated, to show
                            // what improved; this one stays on the original.
                            let! shownIn, fixedReport =
                                match fixOutcome with
                                | Ok fix when fix.WasFixed ->
                                    exported.Value ()
                                    notice.Set $"Exported {Path.GetFileName outputPath}."
                                    match fix.OutputPath with
                                    // It may be open already, from an earlier export to the same file.
                                    | NonNull path -> openElsewhere path true
                                    | Null -> Task.FromResult(window, None)
                                | _ -> Task.FromResult(window, None)

                            if cancelled.Value then notice.Set "Cancelled: Export… wrote nothing."
                            else do! Dialogs.showFixResult shownIn fixOutcome fixedReport
                    | _ -> ()
                }
                |> ignore

            /// File › Append PDFs…: joins the PDFs chosen (the one open first, if any) into a file
            /// chosen, then opens it, and says how it went.
            let appendPdfs () =
                task {
                    if not isBusy.Current then
                        match! AppendDialog.choose window selectedFile.Current with
                        | None -> ()
                        | Some choice ->
                            let first = choice.Files.Head
                            let! outputPath =
                                Dialogs.pickSavePath
                                    window
                                    (FilePickerSaveOptions(
                                        Title = "Save the joined PDF",
                                        SuggestedFileName = CopyNames.nextVersion first,
                                        DefaultExtension = "pdf",
                                        FileTypeChoices = [ pdfFileType; FilePickerFileTypes.All ]
                                    ))
                                    first
                            match outputPath with
                            | None -> ()
                            | Some outputPath ->
                                isBusy.Set true
                                busyProgress.Set(Some(Appending(Append.Step.Of "starting")))
                                let stage (step: Append.Step) = Avalonia.Threading.Dispatcher.UIThread.Post(fun () -> busyProgress.Set(Some(Appending step)))
                                let source = cancellable ()
                                let! outcome =
                                    task {
                                        try
                                            try
                                                let! outcome =
                                                    Task.Run(fun () ->
                                                        Append.run validator choice.Files outputPath { Append.Options.Default with Title = choice.Title } stage source.Token)
                                                return Some(Ok outcome)
                                            with
                                            | :? OperationCanceledException when source.IsCancellationRequested -> return None
                                            | :? VeraPdfValidationException
                                            | :? IOException
                                            | :? UnauthorizedAccessException
                                            | :? InvalidOperationException as ex -> return Some(Error ex.Message)
                                            | :? iText.Commons.Exceptions.ITextException as ex -> return Some(Error ex.Message)
                                        finally
                                            busyProgress.Set None
                                            isBusy.Set false
                                            finished source
                                    }
                                match outcome with
                                | None -> notice.Set "Cancelled: Append PDFs… wrote nothing."
                                | Some(Error message) -> do! Dialogs.showMessage window $"The PDFs couldn't be joined: {message}" "Append PDFs"
                                | Some(Ok outcome) when outcome.Joined.IsNone -> do! Dialogs.showMessage window (AppendDialog.summary outcome) "Append PDFs"
                                | Some(Ok outcome) ->
                                    let! shownIn, _ = openElsewhere outputPath true
                                    do! Dialogs.showMessage shownIn (AppendDialog.summary outcome) "Append PDFs"
                }
                |> ignore

            // The menus (see MainMenu), once: the File menu's Save and Export… follow the buttons.
            let menuItems = ctx.useState ((None: (NativeMenuItem * NativeMenuItem) option), false)
            ctx.useEffect (
                (fun () ->
                    menuItems.Set(
                        Some(
                            MainMenu.install
                                window
                                { Open = browse
                                  OpenPath = setSelectedFile
                                  CanSave = fun () -> not isBusy.Current && edited.Current
                                  Save = fun () -> saveReview () |> ignore
                                  CanExport = fun () -> not isBusy.Current
                                  Export = fun () -> export ()
                                  Append = appendPdfs }
                        )
                    )),
                [ EffectTrigger.AfterInit ]
            )

            // The window is titled with the PDF, and whether its review has changes not saved.
            window.Title <-
                match selectedFile.Current with
                | Some path when edited.Current -> $"{Path.GetFileName path} — Edited — Legibyl"
                | Some path -> $"{Path.GetFileName path} — Legibyl"
                | None -> "Legibyl — PDF/UA Validator"

            let copyText (text: string) =
                match window.Clipboard with
                | NonNull clipboard -> clipboard.SetTextAsync text |> ignore
                | Null -> ()

            /// Dismisses or restores a warning (key), or every warning of its rule (ruleKey),
            /// keeping the selection on it, or moving it on to the next shown finding if it's now hidden.
            let setDismissed (row: RuleViolationRow) (key: RuleViolation -> Dismissals.Key) (isDismissed: bool) =
              match selectedFile.Current with
              | None -> ()
              | Some path ->
                let review = layoutReviews.For path
                review.Dismissed <-
                    if isDismissed then review.Dismissed.Add(key row.Violation, row.Message)
                    else review.Dismissed.Remove(key row.Violation)
                edited.Set true
                let next = review.Dismissed
                dismissed.Set next
                match outcome.Current with
                | Validated(report, rows) ->
                    let updated = rows |> List.map (fun r -> { r with Dismissed = Dismissals.isDismissed next r.Violation })
                    outcome.Set(Validated(report, updated))
                    let index = rows |> List.findIndex (fun r -> obj.ReferenceEquals(r, row))
                    let same = updated[index]
                    if showDismissed.Current || not same.Dismissed then
                        selectedRow.Set(Some same)
                    else
                        updated
                        |> List.indexed
                        |> List.tryFind (fun (i, r) -> i > index && not r.Dismissed)
                        |> Option.orElse (updated |> List.indexed |> List.rev |> List.tryFind (fun (i, r) -> i < index && not r.Dismissed))
                        |> Option.map snd
                        |> selectRow
                | _ -> ()

            /// The report without the warnings the user dismissed, for the banner and reports.
            let withoutDismissed (report: ValidationReport) =
                { report with Assertions = report.Assertions |> Seq.filter (not << Dismissals.isDismissed dismissed.Current) |> Array.ofSeq }

            /// The report as Copy Summary and Save Report give it: for a PDF that passes, led by its
            /// checklist and what a person has checked of it.
            let reportText (report: ValidationReport) =
                let checks =
                    match checklist () with
                    | Some(steps, ticked) when report.Passes -> Checklist.text profileName steps ticked
                    | _ -> ""
                checks + ValidationReportTextFormatter.BuildReport(withoutDismissed report)

            let saveReport _ =
                task {
                    match report with
                    | Some report ->
                        let! path =
                            Dialogs.pickSavePath
                                window
                                (FilePickerSaveOptions(
                                    Title = "Save validation report",
                                    SuggestedFileName = $"{Path.GetFileNameWithoutExtension report.FilePath}-validation-report.txt",
                                    DefaultExtension = "txt",
                                    FileTypeChoices =
                                        [ FilePickerFileType("Text report", Patterns = [ "*.txt" ], MimeTypes = [ "text/plain" ])
                                          FilePickerFileTypes.All ]
                                ))
                                report.FilePath
                        match path with
                        | Some path ->
                            try
                                File.WriteAllText(path, reportText report)
                            with ex ->
                                do! Dialogs.showMessage window $"The report couldn't be written to {Path.GetFileName path}: {Dialogs.failureMessage ex}" "Save Report"
                        | None -> ()
                    | None -> ()
                }
                |> ignore

            let busy = isBusy.Current

            let fileRow =
                Grid.create [
                    Grid.margin (0.0, 0.0, 0.0, 8.0)
                    Grid.columnDefinitions "*,Auto,Auto"
                    Grid.children [
                        TextBox.create [
                            TextBox.margin (0.0, 0.0, 8.0, 0.0)
                            TextBox.verticalContentAlignment VerticalAlignment.Center
                            TextBox.isReadOnly true
                            AttrBuilder<TextBox>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, "PDF file", ValueNone)
                            TextBox.text (selectedFile.Current |> Option.defaultValue "Drop a PDF here or click Browse…")
                        ]
                        Button.create [
                            Grid.column 1
                            Button.content "Browse…"
                            Button.padding (12.0, 4.0)
                            Button.margin (0.0, 0.0, 8.0, 0.0)
                            Button.isEnabled (not busy)
                            Button.onClick browse
                        ]
                        Button.create [
                            Grid.column 2
                            // Opening a PDF checks it; this checks it again, after it's changed on disk.
                            Button.content "Check Again"
                            ToolTip.tip "Check this PDF again, after it's changed on disk."
                            Button.padding (16.0, 4.0)
                            Button.isEnabled (not busy && selectedFile.Current.IsSome)
                            // Always a fresh check, whatever the review saved.
                            Button.onClick (fun _ -> runValidation "Validating" true |> ignore)
                        ]
                    ]
                ]

            let statusBanner =
                let banner (backgroundToken: string) (text: string) =
                    Border.create [
                        Border.padding (12.0, 8.0)
                        Border.cornerRadius 4.0
                        Border.margin (0.0, 0.0, 0.0, 8.0)
                        Theme.resources [ Border.BackgroundProperty, backgroundToken ]
                        Border.child (
                            TextBlock.create [
                                TextBlock.text text
                                TextBlock.fontWeight FontWeight.Bold
                                TextBlock.textWrapping TextWrapping.Wrap
                                // Black: 4.5:1 or more on the banner's colours, light and dark (see AccessibilityTests).
                                TextBlock.foreground Brushes.Black
                                // A validation's result (PASS, FAIL, an error) is announced as it comes.
                                AttrBuilder<TextBlock>.CreateProperty<Automation.AutomationLiveSetting>(Automation.AutomationProperties.LiveSettingProperty, Automation.AutomationLiveSetting.Polite, ValueNone)
                            ]
                        )
                    ]
                    :> IView

                match outcome.Current with
                | NotValidated -> None
                | ValidationFailed message -> Some(banner Theme.Tokens.Warning $"Error: {message}")
                | Validated(report, _) ->
                    // The standard, and why it's the one: "PDF/UA-2 (a PDF 2.0 file".
                    let flavour = PdfUaTarget.displayName report.Profile
                    let why =
                        match Option.ofObj report.PdfVersion with
                        | Some version when report.ProfileDetected -> $"a PDF {version} file; "
                        | _ -> ""
                    let shownWarnings = (withoutDismissed report).WarningCount
                    let warnings =
                        match report.WarningCount - shownWarnings with
                        | 0 when shownWarnings = 1 -> "1 warning"
                        | 0 -> $"{shownWarnings} warnings"
                        | hidden when shownWarnings = 1 -> $"1 warning, {hidden} dismissed"
                        | hidden -> $"{shownWarnings} warnings, {hidden} dismissed"
                    // Shown from the review's saved result: when it was checked.
                    let asChecked =
                        match checkedAt.Current with
                        | Some date -> ", as checked " + date.ToString("d MMM yyyy", Globalization.CultureInfo.InvariantCulture)
                        | None -> ""
                    // And for a PDF that passes, once a person has worked through its checklist.
                    let asChecked = if passProgress = Some "Checked by a person" then asChecked + ", checked by a person" else asChecked
                    let standing =
                        if report.Passes then
                            banner
                                Theme.Tokens.Success
                                $"PASS — compliant with {flavour} and {WcagSpecification.Name} ({why}{report.TotalAssertions} checks, {warnings}){asChecked}"
                        else
                            // Which standard's checks failed: PDF/UA's, WCAG 2.2's, or both.
                            let wcag = Seq.length report.WcagFailures
                            let failed =
                                [ if report.FailedCount > wcag then $"{report.FailedCount - wcag} {flavour}"
                                  if wcag > 0 then $"{wcag} {WcagSpecification.Name}" ]
                            let checks = if report.FailedCount = 1 then "check" else "checks"
                            // WCAG 2.2's checks that couldn't run: said, with what did fail.
                            let notChecked = if isNull report.WcagError then "" else $"{WcagSpecification.Name} couldn't be checked; "
                            let counted =
                                match failed with
                                | [] -> $"{report.TotalAssertions} checks"
                                | failed -> $"""{String.Join(" and ", failed)} {checks} failed, of {report.TotalAssertions}"""
                            banner Theme.Tokens.Danger $"FAIL — {notChecked}{counted} ({why}{warnings}){asChecked}"
                    Some(
                        match List.ofSeq report.Notes with
                        | [] -> standing
                        | notes ->
                            // What veraPDF logged while it read the file: not findings, but kept.
                            let things = if notes.Length = 1 then "1 thing" else $"{notes.Length} things"
                            StackPanel.create [
                                StackPanel.children [
                                    standing
                                    Expander.create [
                                        Expander.margin (0.0, 0.0, 0.0, 8.0)
                                        Expander.horizontalAlignment HorizontalAlignment.Stretch
                                        Expander.header $"veraPDF noted {things} while reading the file"
                                        Expander.content (
                                            SelectableTextBlock.create [
                                                SelectableTextBlock.textWrapping TextWrapping.Wrap
                                                SelectableTextBlock.text (
                                                    [ for note in notes ->
                                                          let times = if note.Count > 1 then $" (×{note.Count})" else ""
                                                          $"{note.Message}{times} — {note.Level}, {note.Logger}" ]
                                                    |> String.concat "\n"
                                                )
                                            ]
                                        )
                                    ]
                                ]
                            ]
                    )

            let allRows =
                match outcome.Current with
                | Validated(_, rows) -> rows
                | _ -> []

            let dismissedCount = allRows |> List.filter _.Dismissed |> List.length

            // Dismissed warnings are hidden unless the user asks to see them.
            let rows = if showDismissed.Current then allRows else allRows |> List.filter (not << _.Dismissed)

            // What to do next, from its findings and what's been done in its review: for a PDF that
            // fails, and for one that passes with warnings not dismissed.
            let nextStep =
                match report with
                // WCAG 2.2 not checked, and nothing failed: not "it passes".
                | Some report when not (isNull report.WcagError) && allRows |> List.forall (fun r -> r.Violation.Severity <> FindingSeverity.Failure) ->
                    Some
                        { Title = $"Check {WcagSpecification.Name} again"
                          Details =
                            [ $"{WcagSpecification.Name}'s checks couldn't run ({report.WcagError}), so it isn't counted as passing them, though PDF/UA's checks ran."
                              "Choose Check Again. If it happens again, report it, with the PDF if you can share it." ]
                          TopRules = [] }
                | Some _ ->
                    NextStep.decide
                        allRows
                        { StructureSuggested = pending.Current.TagsFromReview
                          EditsPending = pending.Current.Changes
                          AutoFixesChosen = autoFixes.Current
                          TitleGiven = selectedFile.Current |> Option.exists titleGiven }
                | _ -> None

            /// Right-click menu for the selected finding (the grid selects a row when it's right-clicked).
            let findingMenu =
                let selected = selectedRow.Current
                let canDismiss = selected |> Option.exists (fun row -> Dismissals.canDismiss row.Violation)
                ContextMenu.create [
                    ContextMenu.viewItems [
                        match selectedRule.Current, selected with
                        // The pass: nothing to dismiss.
                        | _ when passSelected.Current ->
                            MenuItem.create [ MenuItem.header "Nothing to dismiss: the PDF passes"; MenuItem.isEnabled false ]
                        // The next step: advice, not a finding.
                        | _ when nextSelected.Current ->
                            MenuItem.create [ MenuItem.header "Nothing to dismiss: this is the next step"; MenuItem.isEnabled false ]
                        // A rule's row: what applies to all of it.
                        | Some group, _ ->
                            let canDismissAll = Dismissals.canDismiss group.First.Violation
                            MenuItem.create [
                                MenuItem.header (
                                    if canDismissAll then $"Dismiss all {group.RuleId} warnings" else "Dismiss all warnings of this type (only warnings can be dismissed)"
                                )
                                MenuItem.isEnabled canDismissAll
                                ToolTip.tip "Hides every warning of this type in this PDF, wherever it is."
                                MenuItem.onClick (fun _ -> setDismissed group.First Dismissals.ruleKeyOf true)
                            ]
                        | None, Some row when row.Dismissed ->
                            let restore (header: string) (key: RuleViolation -> Dismissals.Key) =
                                MenuItem.create [ MenuItem.header header; MenuItem.onClick (fun _ -> setDismissed row key false) ]
                            if dismissed.Current.ContainsKey(Dismissals.keyOf row.Violation) then
                                restore "Restore warning" Dismissals.keyOf
                            if Dismissals.isRuleDismissed dismissed.Current row.Violation then
                                restore $"Restore all {row.Violation.RuleId} warnings" Dismissals.ruleKeyOf
                        | None, _ ->
                            MenuItem.create [
                                MenuItem.header (if canDismiss || selected.IsNone then "Dismiss warning" else "Dismiss warning (only warnings can be dismissed)")
                                MenuItem.isEnabled canDismiss
                                MenuItem.onClick (fun _ -> selected |> Option.iter (fun row -> setDismissed row Dismissals.keyOf true))
                            ]
                            MenuItem.create [
                                MenuItem.header (
                                    match selected with
                                    | Some row when canDismiss -> $"Dismiss all {row.Violation.RuleId} warnings"
                                    | _ -> "Dismiss all warnings of this type"
                                )
                                MenuItem.isEnabled canDismiss
                                ToolTip.tip "Hides every warning of this type in this PDF, wherever it is."
                                MenuItem.onClick (fun _ -> selected |> Option.iter (fun row -> setDismissed row Dismissals.ruleKeyOf true))
                            ]
                    ]
                ]

            let gridFontSize = 12.0

            // The findings as the list shows them: each rule's row, and under it its findings if it's
            // opened. Made again only when the findings or what's opened change (a new list would
            // reset the list's scrolling).
            let isExpanded (group: RuleGroup) = ruleExpanded.Current |> Map.tryFind group.Key |> Option.defaultValue group.ExpandedAtFirst
            let groupsCache = ctx.useState ((None: (RuleViolationRow list * RuleGroup list) option), false)
            let groups =
                match groupsCache.Current with
                | Some(cached, groups) when cached.Length = rows.Length && List.forall2 (fun a b -> obj.ReferenceEquals(a, b)) cached rows -> groups
                | _ ->
                    let groups = RuleGroup.OfRows rows
                    groupsCache.Set(Some(rows, groups))
                    groups
            let itemsCache = ctx.useState ((None: (RuleGroup list * Set<string> * string option * NextStep option * ResultItem list) option), false)
            let items =
                let opened = groups |> List.filter isExpanded |> List.map _.Key |> Set.ofList
                match itemsCache.Current with
                | Some(cached, cachedOpened, cachedPass, cachedNext, items) when
                    obj.ReferenceEquals(cached, groups) && cachedOpened = opened && cachedPass = passProgress && cachedNext = nextStep
                    ->
                    items
                | _ ->
                    let items =
                        [ match passProgress with
                          | Some progress -> yield PassItem progress
                          | None -> ()
                          match nextStep with
                          | Some step -> yield NextStepItem step
                          | None -> ()
                          for group in groups do
                              let expanded = opened.Contains group.Key
                              yield RuleItem(group, expanded)
                              if expanded then
                                  yield! [ for row in group.Findings -> FindingItem row ] ]
                    itemsCache.Set(Some(groups, opened, passProgress, nextStep, items))
                    items
            let toggle (group: RuleGroup) = ruleExpanded.Set(Map.add group.Key (not (isExpanded group)) ruleExpanded.Current)

            /// A count as a rounded badge, in a colour.
            let pill (color: string) (text: string) =
                Border.create [
                    Border.padding (7.0, 1.0)
                    Border.cornerRadius 9.0
                    Border.verticalAlignment VerticalAlignment.Center
                    Theme.resources [ Border.BackgroundProperty, color ]
                    Border.child (
                        TextBlock.create [
                            TextBlock.text text
                            TextBlock.fontSize 11.0
                            TextBlock.fontWeight FontWeight.SemiBold
                            // Black: 4.5:1 or more on the failure and warning colours, light and dark.
                            TextBlock.foreground Brushes.Black
                        ]
                    )
                ]

            /// A rule's row: its colour down its edge, whether it's opened, its severity, how it's
            /// fixed (if its findings are), how many, and the rule.
            let ruleRow (group: RuleGroup) (expanded: bool) : IView =
                let color = if group.Severity = FindingSeverity.Warning then Theme.Tokens.Warning else Theme.Tokens.Danger
                let fixable = group.Fixable
                let dismissedHere = group.Findings |> List.filter _.Dismissed |> List.length
                Border.create [
                    Border.borderThickness (4.0, 0.0, 0.0, 0.0)
                    Border.padding (4.0, 4.0, 6.0, 4.0)
                    Border.tag "rule-row"
                    Theme.resources [ Border.BorderBrushProperty, color ]
                    Border.child (
                        Grid.create [
                            Grid.columnDefinitions "Auto,Auto,Auto,Auto,*"
                            Grid.children [
                                Button.create [
                                    Button.content (if expanded then "▾" else "▸")
                                    Button.tag "rule-expander"
                                    Button.padding (4.0, 0.0)
                                    Button.minHeight 0.0
                                    Button.background Brushes.Transparent
                                    Button.borderThickness 0.0
                                    Button.verticalAlignment VerticalAlignment.Top
                                    AttrBuilder<Button>.CreateProperty<string | null>(
                                        Automation.AutomationProperties.NameProperty, (if expanded then $"Hide {group.RuleId}'s findings" else $"Show {group.RuleId}'s findings"), ValueNone)
                                    Button.onClick ((fun e -> e.Handled <- true; toggle group), SubPatchOptions.Always)
                                ]
                                Border.create [
                                    Grid.column 1
                                    Border.margin (2.0, 2.0, 4.0, 0.0)
                                    Border.verticalAlignment VerticalAlignment.Top
                                    Border.child (severityIcon group.First.Violation)
                                ]
                                Border.create [
                                    Grid.column 2
                                    Border.margin (0.0, 2.0, 4.0, 0.0)
                                    Border.verticalAlignment VerticalAlignment.Top
                                    match fixable with
                                    | first :: _ when fixable.Length < group.Findings.Length ->
                                        ToolTip.tip $"{fixedBy first.Violation}, {fixable.Length} of {group.Findings.Length} places: {first.Fixable}"
                                    | _ -> ()
                                    Border.child (
                                        match fixable with
                                        | [] -> TextBlock.create [] :> IView
                                        | first :: _ -> fixableIcon first
                                    )
                                ]
                                StackPanel.create [
                                    Grid.column 3
                                    StackPanel.orientation Orientation.Horizontal
                                    StackPanel.verticalAlignment VerticalAlignment.Top
                                    StackPanel.margin (0.0, 1.0, 6.0, 0.0)
                                    StackPanel.spacing 4.0
                                    StackPanel.children [
                                        if group.Findings.Length > 1 then pill color (string group.Findings.Length)
                                        if dismissedHere > 0 then
                                            TextBlock.create [
                                                TextBlock.text $"{dismissedHere} dismissed"
                                                TextBlock.fontSize 11.0
                                                TextBlock.verticalAlignment VerticalAlignment.Center
                                                Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                            ]
                                    ]
                                ]
                                TextBlock.create [
                                    Grid.column 4
                                    TextBlock.text $"{group.RuleId}  {group.Message}"
                                    TextBlock.fontSize gridFontSize
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    TextBlock.maxLines 2
                                    TextBlock.textTrimming TextTrimming.CharacterEllipsis
                                    ToolTip.tip group.Message
                                ]
                            ]
                        ]
                    )
                ]

            /// A finding's row, under its rule: its page and where it is. How it's fixed only when
            /// its rule's findings differ in that.
            let findingRow (row: RuleViolationRow) : IView =
                let mixed =
                    groups
                    |> List.tryFind (fun g -> g.Findings |> List.exists (fun r -> obj.ReferenceEquals(r, row)))
                    |> Option.exists (fun g -> g.Fixable.Length > 0 && g.Fixable.Length < g.Findings.Length)
                Border.create [
                    Border.padding (30.0, 3.0, 6.0, 3.0)
                    Border.tag "finding-row"
                    Border.opacity (if row.Dismissed then 0.5 else 1.0)
                    Border.child (
                        Grid.create [
                            Grid.columnDefinitions "Auto,64,*"
                            Grid.children [
                                Border.create [
                                    Border.width 20.0
                                    Border.verticalAlignment VerticalAlignment.Top
                                    Border.child (if mixed then fixableIcon row else TextBlock.create [])
                                ]
                                TextBlock.create [
                                    Grid.column 1
                                    TextBlock.text row.PageDisplay
                                    TextBlock.fontSize gridFontSize
                                    Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                ]
                                TextBlock.create [
                                    Grid.column 2
                                    TextBlock.text (if row.Dismissed then $"{row.FriendlyLocation} (dismissed)" else row.FriendlyLocation)
                                    TextBlock.fontSize gridFontSize
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    ToolTip.tip row.Message
                                ]
                            ]
                        ]
                    )
                ]

            /// The Pass row, first for a PDF that passes: green, a tick, and how far it's checked.
            let passRow (progress: string) : IView =
                Border.create [
                    Border.borderThickness (4.0, 0.0, 0.0, 0.0)
                    Border.padding (26.0, 4.0, 6.0, 4.0)
                    Border.tag "pass-row"
                    Theme.resources [ Border.BorderBrushProperty, Theme.Tokens.Success ]
                    Border.child (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.spacing 6.0
                            StackPanel.children [
                                gridIcon Icons.pass Theme.Tokens.Success "Pass"
                                TextBlock.create [
                                    TextBlock.text "PASS"
                                    TextBlock.fontSize gridFontSize
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                ]
                                TextBlock.create [
                                    TextBlock.text progress
                                    TextBlock.fontSize gridFontSize
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                ]
                            ]
                        ]
                    )
                ]

            /// The Next step row, first for a PDF that fails: an "i", and what to do next.
            let nextStepRow (step: NextStep) : IView =
                Border.create [
                    Border.borderThickness (4.0, 0.0, 0.0, 0.0)
                    Border.padding (26.0, 4.0, 6.0, 4.0)
                    Border.tag "next-step-row"
                    Theme.resources [ Border.BorderBrushProperty, Theme.Tokens.Primary ]
                    Border.child (
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.spacing 6.0
                            StackPanel.children [
                                gridIcon Icons.info Theme.Tokens.Primary "Next step"
                                TextBlock.create [
                                    TextBlock.text "NEXT STEP"
                                    TextBlock.fontSize gridFontSize
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                ]
                                TextBlock.create [
                                    TextBlock.text step.Title
                                    TextBlock.fontSize gridFontSize
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                ]
                            ]
                        ]
                    )
                ]

            let selectedItem =
                match passProgress, nextStep with
                | Some progress, _ when passSelected.Current -> Some(PassItem progress)
                | _, Some step when nextSelected.Current -> Some(NextStepItem step)
                | _ ->
                match selectedRule.Current, selectedRow.Current with
                | Some group, _ ->
                    let shown = groups |> List.tryFind ((=) group) |> Option.defaultValue group
                    Some(RuleItem(shown, isExpanded shown))
                | None, Some row -> Some(FindingItem row)
                | None, None -> None

            let resultsList =
                ListBox.create [
                    ListBox.name "Results"
                    Theme.resources [ ListBox.BorderBrushProperty, Theme.Tokens.Border ]
                    ListBox.borderThickness 1.0
                    ListBox.contextMenu findingMenu
                    ListBox.dataItems items
                    ListBox.itemTemplate (
                        DataTemplateView<ResultItem>.create (fun item ->
                            match item with
                            | RuleItem(group, expanded) -> ruleRow group expanded
                            | FindingItem row -> findingRow row
                            | PassItem progress -> passRow progress
                            | NextStepItem step -> nextStepRow step)
                    )
                    ListBox.onSelectedItemChanged (
                        (fun item ->
                            match item with
                            | :? ResultItem as RuleItem(group, _) when selectedRule.Current <> Some group ->
                                selectFinding (Some group.First) (Some group) |> ignore
                            | :? ResultItem as PassItem _ when not passSelected.Current -> selectPass () |> ignore
                            | :? ResultItem as NextStepItem _ when not nextSelected.Current -> selectNext () |> ignore
                            | :? ResultItem as FindingItem row when
                                selectedRule.Current.IsSome || not (selectedRow.Current |> Option.exists (fun current -> obj.ReferenceEquals(current, row)))
                                ->
                                selectRow (Some row) |> ignore
                            | _ -> ()),
                        SubPatchOptions.Always
                    )
                    // Right and left open and close the selected rule; left on a finding goes to its rule.
                    ListBox.onKeyDown (
                        (fun e ->
                            match e.Key, selectedRule.Current, selectedRow.Current with
                            | Key.Right, Some group, _ when not (isExpanded group) ->
                                toggle group
                                e.Handled <- true
                            | Key.Left, Some group, _ when isExpanded group ->
                                toggle group
                                e.Handled <- true
                            | Key.Left, None, Some row ->
                                groups
                                |> List.tryFind (fun g -> g.Findings |> List.exists (fun r -> obj.ReferenceEquals(r, row)))
                                |> Option.iter (fun group -> selectFinding (Some group.First) (Some group) |> ignore)
                                e.Handled <- true
                            | _ -> ()),
                        SubPatchOptions.Always
                    )
                    match selectedItem with
                    | Some item -> AttrBuilder<ListBox>.CreateProperty<objnull>(ListBox.SelectedItemProperty, box item, ValueNone)
                    | None -> ListBox.selectedIndex -1
                ]

            /// Above the list: how many failures and warnings, and how many are fixed how.
            let summary =
                let shownRows = rows |> List.filter (not << _.Dismissed)
                let failures = shownRows |> List.filter (fun r -> r.Violation.Severity = FindingSeverity.Failure) |> List.length
                let warnings = shownRows.Length - failures
                let byTagging = shownRows |> List.filter (fun r -> r.Violation.FixOptions.HasFlag PdfFixOptions.TagFromLayout) |> List.length
                let fixable = (shownRows |> List.filter _.Violation.IsFixable |> List.length) - byTagging
                let plural (n: int) (what: string) = if n = 1 then $"1 {what}" else $"{n} {what}s"
                WrapPanel.create [
                    WrapPanel.margin (0.0, 0.0, 0.0, 6.0)
                    WrapPanel.isVisible (not allRows.IsEmpty)
                    WrapPanel.children [
                        for view in
                            [ if failures > 0 then pill Theme.Tokens.Danger (plural failures "failure") :> IView
                              if warnings > 0 then pill Theme.Tokens.Warning (plural warnings "warning")
                              if fixable > 0 then badged autoFixesIcon $"{fixable} by auto fix" 0
                              if byTagging > 0 then badged (Some(wandIcon, Theme.Tokens.Primary)) $"{byTagging} by Suggest Structure" 0 ] ->
                            Border.create [ Border.margin (0.0, 0.0, 10.0, 0.0); Border.child view ] ]
                ]

            /// What the details pane says when no finding is selected, in an empty window (see Guidance).
            let guidance () : IView option =
                match selectedFile.Current, outcome.Current with
                | None, _ when not busy -> Some(Guidance.emptyWindow window)
                | _ -> None

            /// Details above the page preview: a finding's, or a rule's (with its first finding's page).
            let withPreview (top: IView) =
                Grid.create [
                    Grid.column 2
                    Grid.margin (12.0, 0.0, 0.0, 0.0)
                    Grid.rowDefinitions "Auto,Auto,Auto,*"
                    Grid.children [
                        top
                        ProgressBar.create [
                            Grid.row 1
                            ProgressBar.height 4.0
                            ProgressBar.minHeight 4.0
                            ProgressBar.isIndeterminate true
                            ProgressBar.isVisible preview.Current.Loading
                            ProgressBar.margin (0.0, 0.0, 0.0, 8.0)
                        ]
                        TextBlock.create [
                            Grid.row 2
                            TextBlock.text preview.Current.Note
                            TextBlock.textWrapping TextWrapping.Wrap
                            TextBlock.margin (0.0, 0.0, 0.0, 8.0)
                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                        ]
                        Border.create [
                            Grid.row 3
                            Theme.resources
                                [ Border.BorderBrushProperty, Theme.Tokens.Border
                                  Border.BackgroundProperty, Theme.Tokens.Fill ]
                            Border.borderThickness 1.0
                            Border.padding 6.0
                            Border.child (
                                Image.create [
                                    match preview.Current.Image with
                                    | Some image -> Image.source image
                                    | None -> ()
                                    Image.stretch Stretch.Uniform
                                ]
                            )
                        ]
                    ]
                ]

            let sectionHeading (text: string) =
                TextBlock.create [
                    TextBlock.text text
                    // A heading a screen reader can jump between.
                    AttrBuilder<TextBlock>.CreateProperty<int>(Automation.AutomationProperties.HeadingLevelProperty, 3, ValueNone)
                    TextBlock.fontWeight FontWeight.SemiBold
                    TextBlock.margin (0.0, 8.0, 0.0, 0.0)
                ]

            /// A rule's details: what it says and means, how many places it's found and where, and
            /// what fixes it; its first finding's page below.
            let ruleDetails (group: RuleGroup) =
                let first = group.First.Violation
                let places =
                    let count = if group.Findings.Length = 1 then "1 place" else $"{group.Findings.Length} places"
                    match group.Pages with
                    | [] -> count
                    | [ page ] -> $"{count}, on page {page}"
                    | pages -> $"{count}, on pages {List.head pages}–{List.last pages}"
                let title =
                    if first.Specification = "Legibyl" then $"Legibyl warning: {group.RuleId} — {places}"
                    else
                        let standard = first.Specification.Split(':').[0]
                        $"{first.Severity}: {standard} {first.Clause}, test {first.TestNumber} — {places}"
                withPreview (
                    StackPanel.create [
                        StackPanel.margin (0.0, 0.0, 0.0, 10.0)
                        StackPanel.children [
                            detailTitle
                                first
                                (TextBlock.create [
                                    TextBlock.text title
                                    TextBlock.fontSize 16.0
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.textWrapping TextWrapping.Wrap
                                ])
                            |> View.withAttrs [ Control.margin (0.0, 0.0, 0.0, 8.0) ]
                            TextBlock.create [ TextBlock.text "Message"; TextBlock.fontWeight FontWeight.SemiBold ]
                            detailText "Message" group.Message false [ TextBox.maxHeight 130.0 ]
                            match RuleGuide.forFinding first with
                            | Some guide ->
                                sectionHeading "What it means"
                                detailText "What it means" guide.Meaning false []
                                if group.Fixable.IsEmpty then
                                    sectionHeading "What to do"
                                    detailText "What to do" guide.WhatToDo false []
                            | None -> ()
                            match group.Fixable with
                            | [] -> ()
                            | fixable ->
                                fixableHeading fixable.Head.Violation
                                |> View.withAttrs [ Control.margin (0.0, 8.0, 0.0, 0.0) ]
                                let how = if fixable.Length = group.Findings.Length then "" else $"{fixable.Length} of {group.Findings.Length} places: "
                                // Each way its findings are fixed, as some rules' are more than one.
                                let ways = fixable |> List.map _.Fixable |> List.distinct |> String.concat "; "
                                detailText (fixedBy fixable.Head.Violation) $"{how}{ways}." false []
                            TextBlock.create [
                                TextBlock.text "Select a place in the list for its details; its first is shown below."
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.margin (0.0, 8.0, 0.0, 0.0)
                                Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                            ]
                        ]
                    ]
                )

            /// The Pass row's details: the automated checks passed, and the steps a person checks,
            /// each ticked off as it's done (kept with the review, for this very file).
            let passDetails () : IView =
                let shownWarnings = rows |> List.filter (fun r -> not r.Dismissed && r.Violation.Severity = FindingSeverity.Warning) |> List.length
                let secondary (text: string) =
                    TextBlock.create [
                        TextBlock.text text
                        TextBlock.textWrapping TextWrapping.Wrap
                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                    ]
                    :> IView
                ScrollViewer.create [
                    Grid.column 2
                    ScrollViewer.margin (12.0, 0.0, 0.0, 0.0)
                    ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Disabled
                    ScrollViewer.content (
                        StackPanel.create [
                            StackPanel.maxWidth 560.0
                            StackPanel.horizontalAlignment HorizontalAlignment.Left
                            StackPanel.spacing 8.0
                            StackPanel.children [
                                withIcon
                                    (decorativeIcon Icons.pass Theme.Tokens.Success 18.0)
                                    (TextBlock.create [
                                        TextBlock.text $"Passes {profileName}'s automated checks. PDF/UA also needs a person to check:"
                                        TextBlock.fontSize 16.0
                                        TextBlock.fontWeight FontWeight.SemiBold
                                        TextBlock.textWrapping TextWrapping.Wrap
                                    ])
                                if shownWarnings > 0 then
                                    let what = if shownWarnings = 1 then "1 warning below is" else $"{shownWarnings} warnings below are"
                                    secondary $"{what} worth a look: what Legibyl noticed that PDF/UA doesn't require, and WCAG 2.2's advisory checks."
                                match checklist () with
                                | None ->
                                    match passFactsFailed.Current with
                                    | Some message -> secondary $"Couldn't read the document for its checklist: {message}"
                                    | None -> secondary "Reading the document for its checklist…"
                                | Some(steps, ticked) ->
                                    for i, step in List.indexed steps do
                                        Border.create [
                                            Border.padding (10.0, 8.0)
                                            Border.cornerRadius 4.0
                                            Border.borderThickness 1.0
                                            Border.tag $"step-{step.Id}"
                                            Theme.resources [ Border.BorderBrushProperty, Theme.Tokens.Border ]
                                            Border.child (
                                                StackPanel.create [
                                                    StackPanel.spacing 4.0
                                                    StackPanel.children [
                                                        DockPanel.create [
                                                            DockPanel.children [
                                                                CheckBox.create [
                                                                    DockPanel.dock Dock.Right
                                                                    CheckBox.content "Checked"
                                                                    CheckBox.isChecked (ticked.Contains step.Id)
                                                                    // Checked in the copy it is: read-only.
                                                                    CheckBox.isEnabled (
                                                                        not (selectedFile.Current |> Option.exists (fun path -> (layoutReviews.For path).CarriedChecked.Contains step.Id))
                                                                    )
                                                                    AttrBuilder<CheckBox>.CreateProperty<string | null>(
                                                                        Automation.AutomationProperties.NameProperty, $"{step.Title}: checked", ValueNone)
                                                                    CheckBox.onIsCheckedChanged (
                                                                        (fun e ->
                                                                            match e.Source with
                                                                            | :? CheckBox as box when box.IsChecked.GetValueOrDefault() <> ticked.Contains step.Id ->
                                                                                toggleStep step.Id
                                                                            | _ -> ()),
                                                                        SubPatchOptions.Always
                                                                    )
                                                                ]
                                                                TextBlock.create [
                                                                    TextBlock.text $"{i + 1}. {step.Title}"
                                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                                                ]
                                                            ]
                                                        ]
                                                        match step.Shown with
                                                        | Some shown -> secondary shown
                                                        | None -> ()
                                                        TextBlock.create [ TextBlock.text step.What; TextBlock.textWrapping TextWrapping.Wrap ]
                                                        if step.InStructure then
                                                            Button.create [
                                                                Button.content "Structure…"
                                                                Button.padding (10.0, 2.0)
                                                                Button.horizontalAlignment HorizontalAlignment.Left
                                                                Button.onClick ((fun _ -> viewStructure () |> ignore), SubPatchOptions.Always)
                                                            ]
                                                    ]
                                                ]
                                            )
                                        ]
                                    // What's checked in the copy it is, and what Export does with the rest.
                                    let review = selectedFile.Current |> Option.map layoutReviews.For
                                    match review |> Option.bind _.CarriedChecklist with
                                    | Some carried ->
                                        let count = review.Value.CarriedChecked.Count
                                        let what = if count = 1 then "1 step" else $"{count} steps"
                                        let date =
                                            carried.CheckedAt
                                            |> Option.ofNullable
                                            |> Option.map (fun at -> " on " + at.ToString("d MMM yyyy", Globalization.CultureInfo.InvariantCulture))
                                            |> Option.defaultValue ""
                                        secondary $"{what} checked by a person{date}, in this copy."
                                    | None -> ()
                                    if not (ticksToCarry ()).IsEmpty && ticked.Count < steps.Length then
                                        secondary $"{ticked.Count} of {steps.Length} steps checked. Export… writes a copy that carries them."
                                    elif not (ticksToCarry ()).IsEmpty then
                                        secondary "Checked by a person. Export… writes a copy that carries it."
                            ]
                        ]
                    )
                ]

            /// The Next step row's details: what to do next and why; and, where failures need fixing
            /// by hand, the rules with the most of them.
            let nextStepDetails (step: NextStep) : IView =
                let paragraph (text: string) =
                    TextBlock.create [ TextBlock.text text; TextBlock.textWrapping TextWrapping.Wrap ] :> IView
                ScrollViewer.create [
                    Grid.column 2
                    ScrollViewer.margin (12.0, 0.0, 0.0, 0.0)
                    ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Disabled
                    ScrollViewer.content (
                        StackPanel.create [
                            StackPanel.maxWidth 560.0
                            StackPanel.horizontalAlignment HorizontalAlignment.Left
                            StackPanel.spacing 8.0
                            StackPanel.children [
                                withIcon
                                    (decorativeIcon Icons.info Theme.Tokens.Primary 18.0)
                                    (TextBlock.create [
                                        TextBlock.text $"Next step: {step.Title}"
                                        TextBlock.fontSize 16.0
                                        TextBlock.fontWeight FontWeight.SemiBold
                                        TextBlock.textWrapping TextWrapping.Wrap
                                    ])
                                for text in step.Details do
                                    paragraph text
                                for group in step.TopRules do
                                    let places = if group.Findings.Length = 1 then "1 place" else $"{group.Findings.Length} places"
                                    Border.create [
                                        Border.padding (10.0, 8.0)
                                        Border.cornerRadius 4.0
                                        Border.borderThickness 1.0
                                        Border.tag $"next-rule-{group.RuleId}"
                                        Theme.resources [ Border.BorderBrushProperty, Theme.Tokens.Border ]
                                        Border.child (
                                            StackPanel.create [
                                                StackPanel.spacing 4.0
                                                StackPanel.children [
                                                    TextBlock.create [
                                                        TextBlock.text $"{group.RuleId} — {places}"
                                                        TextBlock.fontWeight FontWeight.SemiBold
                                                    ]
                                                    paragraph group.Message
                                                    match group.Fixable with
                                                    | first :: _ when not (NextStep.isSafeFix first) ->
                                                        TextBlock.create [
                                                            TextBlock.text $"Auto fix (guesses): {first.Fixable}"
                                                            TextBlock.textWrapping TextWrapping.Wrap
                                                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                                        ]
                                                    | _ -> ()
                                                ]
                                            ]
                                        )
                                    ]
                            ]
                        ]
                    )
                ]

            let findingDetails (row: RuleViolationRow) =
                withPreview (
                        Grid.create [
                            Grid.margin (0.0, 0.0, 0.0, 10.0)
                            Grid.columnDefinitions "*,Auto"
                            Grid.children [
                                StackPanel.create [
                                    StackPanel.children [
                                        detailTitle
                                            row.Violation
                                            (TextBlock.create [
                                                // veraPDF's rules by the standard's clause; Legibyl's own by its check.
                                                TextBlock.text (
                                                    if row.Violation.Specification = "Legibyl" then
                                                        $"Legibyl warning: {row.Clause}-{row.TestNumber} — {row.PageDisplay}"
                                                    else
                                                        let standard = row.Violation.Specification.Split(':').[0]
                                                        $"{row.Severity}: {standard} {row.Clause}, test {row.TestNumber} — {row.PageDisplay}"
                                                )
                                                TextBlock.fontSize 16.0
                                                TextBlock.fontWeight FontWeight.SemiBold
                                                TextBlock.textWrapping TextWrapping.Wrap
                                            ])
                                        |> View.withAttrs [ Control.margin (0.0, 0.0, 0.0, 8.0) ]
                                        TextBlock.create [ TextBlock.text "Message"; TextBlock.fontWeight FontWeight.SemiBold ]
                                        detailText "Message" row.Message false [ TextBox.maxHeight 130.0 ]
                                        // In plain language, and what to do when no auto fix fixes it.
                                        match RuleGuide.forFinding row.Violation with
                                        | Some guide ->
                                            TextBlock.create [
                                                TextBlock.text "What it means"
                                                TextBlock.fontWeight FontWeight.SemiBold
                                                TextBlock.margin (0.0, 8.0, 0.0, 0.0)
                                            ]
                                            detailText "What it means" guide.Meaning false []
                                            if not row.Violation.IsFixable then
                                                TextBlock.create [
                                                    TextBlock.text "What to do"
                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                    TextBlock.margin (0.0, 8.0, 0.0, 0.0)
                                                ]
                                                detailText "What to do" guide.WhatToDo false []
                                        | None -> ()
                                        if row.Violation.IsFixable then
                                            fixableHeading row.Violation
                                            |> View.withAttrs [ Control.margin (0.0, 8.0, 0.0, 0.0) ]
                                            // Tagging from the layout takes a detector's guesses: they're
                                            // worth checking first.
                                            let next =
                                                if row.Violation.FixOptions.HasFlag PdfFixOptions.TagFromLayout then
                                                    "Open Suggest Structure… to see the headings, figures and reading order suggested for each page; Export… then tags a copy from it."
                                                else
                                                    "Choose it in Auto Fixes…, then Export… a copy of this PDF."
                                            detailText (fixedBy row.Violation) $"{row.Fixable}. {next}" false []
                                        TextBlock.create [
                                            TextBlock.text "Location"
                                            TextBlock.fontWeight FontWeight.SemiBold
                                            TextBlock.margin (0.0, 8.0, 0.0, 0.0)
                                        ]
                                        detailText "Location" row.FriendlyLocation true []
                                        TextBlock.create [
                                            TextBlock.text "Raw location"
                                            TextBlock.fontWeight FontWeight.SemiBold
                                            TextBlock.margin (0.0, 8.0, 0.0, 0.0)
                                        ]
                                        detailText
                                            "Raw location"
                                            (row.LocationContext |> Option.ofObj |> Option.defaultValue "Not available")
                                            true
                                            [ TextBox.maxHeight 90.0
                                              TextBox.fontFamily Fonts.Monospace
                                              TextBox.fontSize 12.0 ]
                                    ]
                                ]
                                Button.create [
                                    Grid.column 1
                                    Button.content "Copy Finding"
                                    Button.padding (12.0, 4.0)
                                    Button.margin (12.0, 0.0, 0.0, 0.0)
                                    Button.verticalAlignment VerticalAlignment.Top
                                    Button.onClick (fun _ -> copyText (ValidationReportTextFormatter.BuildFinding row.Violation))
                                ]
                            ]
                        ]
                )

            // Kept in step with the buttons.
            match menuItems.Current with
            | Some(save, exportItem) ->
                save.IsEnabled <- not busy && edited.Current
                exportItem.IsEnabled <- not busy && selectedFile.Current.IsSome && (saveOptions () <> PdfFixOptions.None || not (ticksToCarry ()).IsEmpty)
            | None -> ()

            DockPanel.create [
              DockPanel.children [
                // The File menu, where the platform has no menu bar of its own (not macOS).
                ContentControl.create [ DockPanel.dock Dock.Top; ContentControl.content (NativeMenuBar()) ]
                Grid.create [
                    Grid.margin 12.0
                    // A background makes the whole window area a drop target.
                    Grid.background "Transparent"
                    Grid.allowDrop true
                    Grid.onDragOver (fun e ->
                        e.DragEffects <- if e.DataTransfer.Contains DataFormat.File then DragDropEffects.Copy else DragDropEffects.None
                        e.Handled <- true)
                    Grid.onDrop onDrop
                    Grid.rowDefinitions "Auto,Auto,Auto,*,Auto"
                    Grid.children [
                        fileRow
                        StackPanel.create [
                            Grid.row 1
                            StackPanel.isVisible busy
                            StackPanel.margin (0.0, 0.0, 0.0, 8.0)
                            StackPanel.children [
                                ProgressBar.create [
                                    ProgressBar.height 4.0
                                    ProgressBar.minHeight 4.0
                                    yield! progressBarState busyProgress.Current
                                    // Named for what it's showing.
                                    AttrBuilder<ProgressBar>.CreateProperty<string | null>(
                                        Automation.AutomationProperties.NameProperty,
                                        (match busyProgress.Current with Some progress -> statusText progress | None -> "Working"),
                                        ValueNone
                                    )
                                ]
                                DockPanel.create [
                                    DockPanel.lastChildFill true
                                    DockPanel.children [
                                        match cancelSource.Current with
                                        | Some source when busy ->
                                            Button.create [
                                                DockPanel.dock Dock.Right
                                                Button.tag "cancel-work"
                                                Button.content "Cancel"
                                                Button.margin (8.0, 4.0, 0.0, 0.0)
                                                // Escape stops it too.
                                                Button.isCancel true
                                                ToolTip.tip "Stop it (Escape); nothing is written."
                                                Button.onClick (fun _ -> try source.Cancel() with :? ObjectDisposedException -> ())
                                            ]
                                        | _ -> ()
                                        match busyProgress.Current with
                                        | Some progress when busy -> progressLine progress
                                        | _ -> ()
                                    ]
                                ]
                            ]
                        ]
                        // What a screen reader hears while it runs: the activity, and a tenth of the way
                        // at a time (the line above changes too often to hear). Outside the row, which is
                        // hidden while idle, so it's there, empty, before the first thing it says.
                        Border.create [
                            Grid.row 1
                            Border.isHitTestVisible false
                            Border.child (Dialogs.announcer (match busyProgress.Current with Some progress when busy -> announcement progress | _ -> ""))
                        ]
                        match statusBanner with
                        | Some banner -> Border.create [ Grid.row 2; Border.child banner ]
                        | None -> ()
                        Grid.create [
                            Grid.row 3
                            Grid.columnDefinitions [
                                ColumnDefinition(GridLength(5.0, GridUnitType.Star), MinWidth = 360.0)
                                ColumnDefinition(GridLength 6.0)
                                ColumnDefinition(GridLength(7.0, GridUnitType.Star), MinWidth = 420.0)
                            ]
                            Grid.children [
                                Grid.create [
                                    Grid.rowDefinitions "Auto,*,Auto"
                                    Grid.children [
                                        summary
                                        Border.create [ Grid.row 1; Border.child resultsList ]
                                        DockPanel.create [
                                          Grid.row 2
                                          DockPanel.margin (0.0, 8.0, 0.0, 0.0)
                                          DockPanel.children [
                                            if dismissedCount > 0 then
                                                Button.create [
                                                    DockPanel.dock Dock.Left
                                                    Button.verticalAlignment VerticalAlignment.Center
                                                    Button.padding (8.0, 4.0)
                                                    Button.content (
                                                        if showDismissed.Current then $"Hide {dismissedCount} dismissed"
                                                        else $"Show {dismissedCount} dismissed"
                                                    )
                                                    Button.onClick (fun _ -> showDismissed.Set(not showDismissed.Current))
                                                ]
                                            StackPanel.create [
                                            DockPanel.dock Dock.Right
                                            StackPanel.orientation Orientation.Horizontal
                                            StackPanel.horizontalAlignment HorizontalAlignment.Right
                                            StackPanel.children [
                                                // What was just done, announced as it's done.
                                                TextBlock.create [
                                                    TextBlock.text notice.Current
                                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                                    TextBlock.margin (0.0, 0.0, 12.0, 0.0)
                                                    Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                                    AttrBuilder<TextBlock>.CreateProperty<Automation.AutomationLiveSetting>(Automation.AutomationProperties.LiveSettingProperty, Automation.AutomationLiveSetting.Polite, ValueNone)
                                                ]
                                                Button.create [
                                                    Button.content "Copy Summary"
                                                    Button.padding (12.0, 4.0)
                                                    Button.margin (0.0, 0.0, 8.0, 0.0)
                                                    Button.isEnabled report.IsSome
                                                    Button.onClick (fun _ -> report |> Option.iter (reportText >> copyText))
                                                ]
                                                Button.create [
                                                    Button.content "Save Report…"
                                                    Button.padding (12.0, 4.0)
                                                    Button.isEnabled report.IsSome
                                                    Button.onClick saveReport
                                                ]
                                            ]
                                            ]
                                          ]
                                        ]
                                    ]
                                ]
                                GridSplitter.create [
                                    Grid.column 1
                                    GridSplitter.width 6.0
                                    GridSplitter.horizontalAlignment HorizontalAlignment.Stretch
                                    Theme.resources [ GridSplitter.BackgroundProperty, Theme.Tokens.Border ]
                                    GridSplitter.resizeDirection GridResizeDirection.Columns
                                ]
                                match selectedRule.Current, selectedRow.Current, guidance () with
                                | _ when passSelected.Current && passProgress.IsSome -> passDetails ()
                                | _ when nextSelected.Current && nextStep.IsSome -> nextStepDetails nextStep.Value
                                | Some group, _, _ -> ruleDetails (groups |> List.tryFind ((=) group) |> Option.defaultValue group)
                                | None, Some row, _ -> findingDetails row
                                | None, None, Some panel -> panel
                                | None, None, None -> ()
                            ]
                        ]
                        StackPanel.create [
                            Grid.row 4
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.horizontalAlignment HorizontalAlignment.Right
                            StackPanel.margin (0.0, 8.0, 0.0, 0.0)
                            StackPanel.children [
                                let changes = pending.Current.Changes
                                let chosen =
                                    autoFixes.Current
                                    |> Option.map (fun options -> Enum.GetValues<PdfFixOptions>() |> Seq.filter (fun o -> o <> PdfFixOptions.None && o <> PdfFixOptions.All && options.HasFlag o) |> Seq.length)
                                    |> Option.defaultValue 0
                                // The counts are part of what a screen reader says for the buttons.
                                // For an untagged PDF, suggesting its structure is the next step, and what
                                // Export waits for: said, and shown in the warning colour.
                                let suggest = untagged ()
                                let structureLabel = if suggest then "Suggest Structure…" else "Structure…"
                                // An untagged PDF's suggested structure, which Export tags a copy from,
                                // counts as a change of its own: the biggest one.
                                let suggested = suggest && pending.Current.TagsFromReview
                                let edits = if changes = 1 then "1 edit" else $"{changes} edits"
                                let structureName =
                                    match suggested, changes with
                                    | true, 0 -> $"{structureLabel}, suggested structure not in the PDF yet"
                                    | true, _ -> $"{structureLabel}, suggested structure and {edits} not in the PDF yet"
                                    | false, 0 -> structureLabel
                                    | false, _ -> $"{structureLabel}, {edits} not in the PDF yet"
                                let autoFixesName = if chosen = 0 then "Auto Fixes…" else $"Auto Fixes…, {chosen} chosen"
                                Button.create [
                                    Button.content (badged (if suggest then suggestIcon else None) structureLabel (changes + (if suggested then 1 else 0)))
                                    AttrBuilder<Button>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, structureName, ValueNone)
                                    Button.padding (12.0, 4.0)
                                    Button.margin (0.0, 0.0, 8.0, 0.0)
                                    Button.isEnabled (not busy && selectedFile.Current.IsSome)
                                    // Semi's filled button, in its warning colour.
                                    if suggest then
                                        Button.classes [ "Warning" ]
                                        Theme.resources [ Button.ThemeProperty, "SolidButton" ]
                                    ToolTip.tip (
                                        if suggested then
                                            "The suggested structure goes into the copy on Export…, which tags it from it. The count is the suggestion and your edits on it. Suggest Structure… shows it again, to check and edit."
                                        elif suggest then
                                            "This PDF has no tags. Suggest Structure… shows the structure a layout detector finds on each page (headings, paragraphs, figures, reading order), to check and edit; Export… then tags a copy from it."
                                        else
                                            "Shows the PDF's structure (the heading/paragraph/list/table/link hierarchy a screen reader uses), where you edit its elements. The count is your edits not in it yet, which Export writes."
                                    )
                                    Button.onClick viewStructure
                                ]
                                Button.create [
                                    Button.content (badged autoFixesIcon "Auto Fixes…" chosen)
                                    AttrBuilder<Button>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, autoFixesName, ValueNone)
                                    Button.padding (12.0, 4.0)
                                    Button.margin (0.0, 0.0, 8.0, 0.0)
                                    Button.isEnabled (not busy && canAttemptFix)
                                    ToolTip.tip
                                        "Choose the fixes Export applies, like the document's language, title and metadata; it lists the ones this PDF needs. Disabled when it passes with no warnings, since there's nothing left to fix."
                                    Button.onClick chooseAutoFixes
                                ]
                                // Badged with the steps a person checked that the copy will carry: the one
                                // thing no other button counts.
                                let ticks = (ticksToCarry ()).Count
                                let steps = if ticks = 1 then "1 step" else $"{ticks} steps"
                                Button.create [
                                    Button.content (badged None "Export…" ticks)
                                    AttrBuilder<Button>.CreateProperty<string | null>(
                                        Automation.AutomationProperties.NameProperty,
                                        (if ticks > 0 then $"Export…, {steps} checked to go into the copy" else "Export…"),
                                        ValueNone)
                                    Button.padding (12.0, 4.0)
                                    Button.margin (0.0, 0.0, 8.0, 0.0)
                                    Button.isEnabled (not busy && selectedFile.Current.IsSome && (saveOptions () <> PdfFixOptions.None || ticks > 0))
                                    ToolTip.tip (
                                        if ticks > 0 && saveOptions () = PdfFixOptions.None then
                                            $"{steps} checked, to go into the copy: writes the next version of the PDF (flyer.v2.pdf), unchanged, carrying them, and opens it. The original isn't changed."
                                        elif ticks > 0 then
                                            $"Writes the next version of the PDF (flyer.v2.pdf) with your edits and the chosen auto fixes, and opens it. The {steps} checked don't go into it: its content changes, and they were checks of this one."
                                        else
                                            "Writes the next version of the PDF (flyer.v2.pdf) with your edits and the chosen auto fixes, and opens it. The original isn't changed."
                                    )
                                    Button.onClick export
                                ]
                            ]
                        ]
                    ]
                ]
              ]
            ])

open MainView

/// A document window: one PDF, validated, with its review. Opening a PDF brings forward the
/// window showing it, or opens it in a new one (see Open).
type MainWindow(shell: DocumentShell) as this =
    inherit HostWindow(Title = "Legibyl — PDF/UA Validator", Width = 1200.0, Height = 720.0, MinWidth = 900.0, MinHeight = 560.0)

    static let windows = ResizeArray<MainWindow>()

    let openRequests: IWritable<OpenRequest option> = new State<_>(None)
    let mutable filePath: string option = None
    // A PDF asked for before the window was shown (as Finder can, launching the app), opened once it is.
    let mutable shown = false
    let mutable waiting: OpenRequest option = None

    do
        windows.Add this
        this.Closed.Add(fun _ -> windows.Remove this |> ignore)
        this.Opened.Add(fun _ ->
            shown <- true
            waiting |> Option.iter (fun request -> openRequests.Set(Some request))
            waiting <- None)
        let openElsewhere path changed =
            task {
                let! window, report = MainWindow.Open(shell, path, this, changed)
                return (window :> Window), report
            }
        let opened path =
            filePath <- Some path
            RecentFiles.add path
        this.Content <- view this shell openRequests opened openElsewhere

    /// A window of the app's, not showing a PDF yet.
    new() = new MainWindow(DocumentShell.Shared)

    /// The PDF it shows, if any.
    member _.FilePath = filePath

    /// Opens and validates a PDF in this window, as if it had been picked or dropped.
    member this.OpenFile(path: string) = this.OpenFileAsync path |> ignore

    /// Opens and validates a PDF in this window; its report once validated (None if it couldn't be).
    member _.OpenFileAsync(path: string) : Task<ValidationReport option> =
        let request = { Path = path; Opened = TaskCompletionSource<ValidationReport option>() }
        if shown then openRequests.Set(Some request) else waiting <- Some request
        request.Opened.Task

    /// The document windows open.
    static member Windows = List.ofSeq windows

    /// Opens path: brings forward the window showing it (validating it again if changed, as when
    /// it's been written again since), else opens it in a window showing no PDF yet, else in a new
    /// window (a little below and right of from, if given). That window, and its report once
    /// validated (None if it was open already and not changed).
    static member Open(shell: DocumentShell, path: string, ?from: Window, ?changed: bool) : Task<MainWindow * ValidationReport option> =
        let comparison = if OperatingSystem.IsLinux() then StringComparison.Ordinal else StringComparison.OrdinalIgnoreCase
        let showing (window: MainWindow) =
            window.FilePath |> Option.exists (fun shown -> String.Equals(Path.GetFullPath shown, Path.GetFullPath path, comparison))
        match windows |> Seq.tryFind showing, windows |> Seq.tryFind (fun window -> window.FilePath.IsNone) with
        | Some window, _ when defaultArg changed false ->
            window.Activate()
            task {
                let! report = window.OpenFileAsync path
                return window, report
            }
        | Some window, _ ->
            window.Activate()
            Task.FromResult((window, None))
        | None, Some empty ->
            empty.Activate()
            task {
                let! report = empty.OpenFileAsync path
                return empty, report
            }
        | None, None ->
            let window = new MainWindow(shell)
            match from with
            | Some from ->
                window.WindowStartupLocation <- WindowStartupLocation.Manual
                window.Position <- PixelPoint(from.Position.X + 28, from.Position.Y + 28)
            | None -> ()
            window.Show()
            task {
                let! report = window.OpenFileAsync path
                return window, report
            }
