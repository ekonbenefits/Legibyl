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

/// Flattened, display-friendly view of a RuleViolation for the results grid.
type RuleViolationRow =
    { Violation: RuleViolation
      Clause: string
      TestNumber: int
      Severity: string
      /// What an auto fix can do about this finding, e.g. "Fix XMP metadata"; empty if nothing.
      Fixable: string
      Message: string
      LocationContext: string | null
      /// 1-based page number, if the failure is anchored to a page.
      PageNumber: int option
      /// "Page 3" or "—" if no page is known — bound directly to the grid's Page column.
      PageDisplay: string
      /// Plain-English breadcrumb, e.g. "Document → Page 2 → Link annotation". Falls back to the raw context.
      FriendlyLocation: string
      /// A warning the user has dismissed (see Dismissals).
      Dismissed: bool
      /// How many findings break the same rule, it included.
      Occurrences: int }

    /// The rule it breaks, as the results group it, how many first (a long message is cut off at
    /// the pane's edge): "6 × 7.1-3: Content shall be marked as…".
    member row.Rule =
        let count = if row.Occurrences > 1 then $"{row.Occurrences} × " else ""
        $"{count}{row.Clause}-{row.TestNumber}: {row.Message}"

    static member Of(violation: RuleViolation) =
        let location = violation.Location
        let pageNumber =
            match location, violation.Geometry with
            | NonNull location, _ when location.PageNumber.HasValue -> Some location.PageNumber.Value
            | _, NonNull geometry -> Some geometry.PageNumber
            | _ -> None

        { Violation = violation
          Clause = violation.Clause
          TestNumber = violation.TestNumber
          Severity = violation.Severity.ToString()
          Fixable = violation.FixDescription
          Message = violation.Message
          LocationContext = violation.LocationContext
          PageNumber = pageNumber
          PageDisplay =
            match pageNumber with
            | Some page -> $"Page {page}"
            | None -> "—"
          FriendlyLocation =
            match location, violation.LocationContext with
            | NonNull location, _ -> location.Breadcrumb
            | Null, NonNull context -> context
            | Null, Null -> ""
          Dismissed = false
          Occurrences = 1 }

module private MainView =
    let profiles =
        [ "PDF/UA-1", PdfProfile.PdfUa1
          "PDF/UA-2", PdfProfile.PdfUa2 ]

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

    /// A menu item's command: runs action when the item's chosen (from the menu bar, the in-window
    /// menu, or a test).
    let command (action: unit -> unit) =
        let changed = Event<EventHandler, EventArgs>()
        { new System.Windows.Input.ICommand with
            member _.CanExecute _ = true
            member _.Execute _ = action ()
            [<CLIEvent>]
            member _.CanExecuteChanged = changed.Publish }

    let createBitmap (pngBytes: byte array) =
        use stream = new MemoryStream(pngBytes)
        new Bitmap(stream)

    // 16x16 icons; the even-odd fill cuts the "!" and "×" out of the shapes.
    // The wrench is a single outline (head, notch, handle) rotated 45°.
    let private warningIcon =
        Geometry.Parse "F0 M8,1 L15.5,14.5 L0.5,14.5 Z M7.2,5.5 L8.8,5.5 L8.6,10 L7.4,10 Z M7.1,11.2 L8.9,11.2 L8.9,13 L7.1,13 Z"

    let private failureIcon =
        Geometry.Parse
            "F0 M8,0.5 A7.5,7.5 0 1 1 7.99,0.5 Z M4.9,5.9 L5.9,4.9 L8,7 L10.1,4.9 L11.1,5.9 L9,8 L11.1,10.1 L10.1,11.1 L8,9 L5.9,11.1 L4.9,10.1 L7,8 Z"

    let private fixIcon =
        Geometry.Parse
            "F1 M10.68,8.56 L17.25,15.13 A1.5,1.5 0 0 1 15.13,17.25 L8.56,10.68 A4,4 0 0 1 3.45,5.15 L5.44,7.14 L7.14,5.44 L5.15,3.45 A4,4 0 0 1 10.68,8.56 Z"

    /// A magic wand (a stick, and two sparkles): for what suggesting the structure fixes.
    let private wandIcon =
        Geometry.Parse
            "F1 M1.5,13.4 L9.9,5 L11,6.1 L2.6,14.5 Z M12.5,0.5 L13.3,2.7 L15.5,3.5 L13.3,4.3 L12.5,6.5 L11.7,4.3 L9.5,3.5 L11.7,2.7 Z M5,1.5 L5.5,2.5 L6.5,3 L5.5,3.5 L5,4.5 L4.5,3.5 L3.5,3 L4.5,2.5 Z"

    let private iconPath (icon: Geometry) (color: string) (size: float) (attrs: IAttr<Shapes.Path> list) =
        // Qualified: System.IO also defines Path.
        Avalonia.FuncUI.DSL.Path.create [
            AttrBuilder<Shapes.Path>.CreateProperty<Geometry | null>(Shapes.Path.DataProperty, icon, ValueNone)
            Shapes.Shape.stretch Stretch.Uniform
            Control.width size
            Control.height size
            Control.verticalAlignment VerticalAlignment.Center
            Theme.resources [ Shapes.Shape.FillProperty, color ]
            yield! attrs
        ]

    /// A small results-grid icon, with name as its tooltip and screen-reader text.
    let private gridIcon (icon: Geometry) (color: string) (name: string) =
        iconPath icon color 14.0 [
            Control.horizontalAlignment HorizontalAlignment.Center
            ToolTip.tip name
            AttrBuilder<Shapes.Path>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, name, ValueNone)
        ]

    /// An icon beside text that already says the same thing, so screen readers skip it.
    let private decorativeIcon (icon: Geometry) (color: string) (size: float) =
        iconPath icon color size [
            Control.margin (0.0, 0.0, 6.0, 0.0)
            AttrBuilder<Shapes.Path>.CreateProperty<Automation.AccessibilityView>(
                Automation.AutomationProperties.AccessibilityViewProperty, Automation.AccessibilityView.Raw, ValueNone)
        ]

    /// Puts a decorative icon in front of content, keeping the content's own wrapping.
    let private withIcon (icon: IView) (content: IView) =
        Grid.create [
            Grid.columnDefinitions "Auto,*"
            Grid.children [
                icon
                Border.create [ Grid.column 1; Border.child content ]
            ]
        ]

    let private severityIconAndColor (violation: RuleViolation) =
        match violation.Severity with
        | FindingSeverity.Warning -> warningIcon, Theme.Tokens.Warning
        | _ -> failureIcon, Theme.Tokens.Danger

    let severityIcon (violation: RuleViolation) =
        let icon, color = severityIconAndColor violation
        gridIcon icon color (string violation.Severity)

    /// The details pane's title, anchored by the severity icon.
    let detailTitle (violation: RuleViolation) (title: IView) =
        let icon, color = severityIconAndColor violation
        withIcon (decorativeIcon icon color 18.0) title

    /// The icon for how a finding is fixed: a wand for tagging from the suggested structure, a
    /// wrench for an auto fix.
    let private fixIconFor (violation: RuleViolation) =
        if violation.FixOptions.HasFlag PdfFixOptions.TagFromLayout then wandIcon else fixIcon

    /// The details pane's "Fixable" heading, anchored by how it's fixed.
    let fixableHeading (violation: RuleViolation) (heading: IView) =
        withIcon (decorativeIcon (fixIconFor violation) Theme.Tokens.Primary 14.0) heading

    /// A wand for findings suggesting the structure fixes, a wrench for those an auto fix
    /// repairs; nothing otherwise.
    let fixableIcon (row: RuleViolationRow) : IView =
        if row.Violation.IsFixable then
            let how = if row.Violation.FixOptions.HasFlag PdfFixOptions.TagFromLayout then "Fixed by Suggest Structure…" else "Fixable"
            gridIcon (fixIconFor row.Violation) Theme.Tokens.Primary $"{how}: {row.Fixable}"
        else
            TextBlock.create []

    /// The Auto Fixes button's icon: the wrench its fixes have in the results.
    let autoFixesIcon = Some(fixIcon, Theme.Tokens.Primary)

    /// The Suggest Structure button's icon: the wand its fixes have, white on its filled colour.
    let suggestIcon = Some(wandIcon, "SemiColorWhite")

    /// A button's label, after an icon (its geometry and colour token) if given, with a count
    /// beside it (none when it's 0).
    let badged (icon: (Geometry * string) option) (label: string) (count: int) : IView =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                match icon with
                | Some(geometry, color) -> decorativeIcon geometry color 14.0
                | None -> ()
                TextBlock.create [ TextBlock.text label; TextBlock.verticalAlignment VerticalAlignment.Center ]
                if count > 0 then
                    Border.create [
                        Border.margin (6.0, 0.0, 0.0, 0.0)
                        Border.padding (6.0, 0.0)
                        Border.cornerRadius 8.0
                        Border.verticalAlignment VerticalAlignment.Center
                        Theme.resources [ Border.BackgroundProperty, Theme.Tokens.Primary ]
                        Border.child (
                            TextBlock.create [
                                TextBlock.text (string count)
                                TextBlock.fontSize 11.0
                                TextBlock.fontWeight FontWeight.SemiBold
                                TextBlock.foreground Brushes.White
                            ]
                        )
                    ]
            ]
        ]

    let gridLinesVisibility (value: DataGridGridLinesVisibility) : IAttr<DataGrid> =
        AttrBuilder<DataGrid>.CreateProperty<DataGridGridLinesVisibility>(DataGrid.GridLinesVisibilityProperty, value, ValueNone)

    let detailText (text: string) (secondary: bool) (attrs: IAttr<TextBox> list) =
        TextBox.create [
            if secondary then
                Theme.resources [ TextBox.ForegroundProperty, Theme.Tokens.SecondaryText ]
            TextBox.text text
            TextBox.isReadOnly true
            TextBox.textWrapping TextWrapping.Wrap
            TextBox.borderThickness 0.0
            TextBox.background "Transparent"
            TextBox.padding (0.0, 2.0)
            TextBox.minHeight 0.0
            yield! attrs
        ]

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
            let profileIndex = ctx.useState 0
            let isBusy = ctx.useState false
            let busyProgress = ctx.useState (None: BusyProgress option)
            let outcome = ctx.useState NotValidated
            let selectedRow = ctx.useState (None: RuleViolationRow option)
            let preview = ctx.useState noPreview
            // Bumped on every selection change so a slow page render for an earlier
            // selection can't overwrite the preview for the current one.
            let previewVersion = ctx.useState (0, false)
            // Warnings dismissed for the selected file (in its review, and any carried inside it),
            // whether the results list shows them, and whether its review has changes not saved.
            let dismissed = ctx.useState (Map.empty: Dismissals.Dismissed)
            let showDismissed = ctx.useState false
            let edited = ctx.useState false
            // The repairs chosen in Auto Fixes for the selected file, as its review has them (None
            // until they're chosen), and the user's changes Export writes too.
            let autoFixes = ctx.useState (None: PdfFixOptions option)
            let pending = ctx.useState noChanges
            // The last count of edits not in a tagged PDF: (path, its last write, edits, count).
            // Counting reads the PDF, so it's only done again when one of those changes.
            let pendingCount = ctx.useStateLazy(fun () -> ref (None: (string * DateTime * ElementEdit list * int) option)).Current

            let currentProfile () = snd profiles[profileIndex.Current]

            let report =
                match outcome.Current with
                | Validated(report, _) -> Some report
                | _ -> None

            /// Whether the PDF has no structure tree: it's tagged from a suggested structure.
            let untagged () = report |> Option.exists (fun r -> r.FixableCount PdfFixOptions.TagFromLayout > 0)

            /// Whether it's worth offering fixes right now. The current fixers target known
            /// validation failures, so hide them once the file is already compliant.
            let canAttemptFix = report |> Option.exists (fun r -> not r.IsCompliant || r.WarningCount > 0)

            let clearResults () =
                outcome.Set NotValidated
                selectedRow.Set None
                preview.Set noPreview
                previewVersion.Set(previewVersion.Current + 1)

            let selectRow (row: RuleViolationRow option) =
                task {
                    previewVersion.Set(previewVersion.Current + 1)
                    let version = previewVersion.Current
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

            /// Counts the user's changes Save would write for the selected file: for an untagged PDF,
            /// its edits on the suggested structure (regions merged into their neighbours, other edits, a title); otherwise its
            /// edits that aren't in it yet, which means reading it, off the UI thread.
            let refreshPending () =
                task {
                    selectedFile.Current |> Option.iter (fun path -> edited.Set (layoutReviews.For path).IsEdited)
                    match selectedFile.Current, outcome.Current with
                    | Some path, Validated(report, _) when report.FixableCount PdfFixOptions.TagFromLayout > 0 ->
                        let review = layoutReviews.For path
                        let changes = review.Edits.Length + (if review.Title.IsSome then 1 else 0)
                        // Tagged from on Export once it's been suggested (Suggest Structure… opened, or
                        // a layout kept from before), or there are edits on it.
                        pending.Set { Changes = changes; TagsFromReview = review.Pages.Count > 0 || changes > 0 }
                    | Some path, Validated _ ->
                        let! count =
                            Task.Run(fun () ->
                                try
                                    let edits = RegionEdits.toElementEdits (layoutReviews.For path).Edits
                                    let written = File.GetLastWriteTimeUtc path
                                    match pendingCount.Value with
                                    | Some(p, w, e, count) when p = path && w = written && e = edits -> count
                                    | _ ->
                                        let count = ElementEdits.pending path edits
                                        pendingCount.Value <- Some(path, written, edits, count)
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
            /// activity names it in the progress row ("Validating", "Validating the repaired copy").
            let runValidation (activity: string) : Task<ValidationReport option> =
                task {
                    match selectedFile.Current with
                    | Some filePath when not isBusy.Current ->
                        let profile = currentProfile ()
                        isBusy.Set true
                        clearResults ()
                        busyProgress.Set None
                        // Created on the UI thread, so its reports arrive there too.
                        let progress = Progress<ValidationProgress>(fun p -> busyProgress.Set(Some(Validating(activity, p))))
                        try
                            try
                                let! report = Task.Run(fun () -> validator.Validate(filePath, profile, progress))
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
                                outcome.Set(Validated(report, rows))
                                refreshPending ()
                                selectRow (rows |> List.tryFind (fun row -> showDismissed.Current || not row.Dismissed))
                                return Some report
                            with :? VeraPdfValidationException as ex ->
                                outcome.Set(ValidationFailed ex.Message)
                                return None
                        finally
                            isBusy.Set false
                    | _ -> return None
                }

            /// Opens a file and validates it with the current profile, naming that activity.
            let openFileAs (activity: string) (path: string) =
                selectedFile.Set(Some path)
                opened path
                clearResults ()
                pending.Set noChanges
                let review = layoutReviews.For path
                autoFixes.Set review.AutoFixes
                dismissed.Set review.Dismissed
                edited.Set review.IsEdited
                showDismissed.Set false
                runValidation activity

            /// Keeps the selected file's review in the file next to it; false if it couldn't be
            /// written (and says why).
            let saveReview () =
                task {
                    match selectedFile.Current with
                    | Some path ->
                        try
                            (layoutReviews.For path).Save path
                            edited.Set false
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
                    | Some path when (layoutReviews.For path).IsEdited ->
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
                        let edited = selectedFile.Current |> Option.exists (fun path -> (layoutReviews.For path).IsEdited)
                        if edited && not closing.Current then
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

            let viewStructure _ =
                task {
                    let show (structure: StructureNode | null) (filePath: string) =
                        task {
                            match structure with
                            | null ->
                                // Untagged: the button says Suggest Structure…, and that's what it does.
                                let review = layoutReviews.For filePath
                                // Offer to read the titles of pages links go to, before the window opens.
                                do! LinkTitleOffer.offer window filePath review (LinkTitles.WebPageTitles())
                                StructureTreeWindow(Suggested, filePath, review, refreshPending).Show window
                            | structure -> StructureTreeWindow(Tagged structure, filePath, layoutReviews.For filePath, refreshPending).Show window
                        }

                    match selectedFile.Current with
                    | Some filePath ->
                        match structureCache.TryFind filePath with
                        | Some structure -> do! show structure filePath
                        | None ->
                            isBusy.Set true
                            busyProgress.Set None
                            // Created on the UI thread, so its reports arrive there too.
                            let progress = Progress<StructureReadProgress>(fun p -> busyProgress.Set(Some(ReadingStructure p)))
                            let stamp = structureCache.Stamp filePath
                            try
                                try
                                    let! structure = Task.Run(fun () -> validator.ReadStructureTree(filePath, progress))
                                    structureCache.Remember(stamp, structure)
                                    isBusy.Set false
                                    do! show structure filePath
                                with :? VeraPdfValidationException as ex ->
                                    do! Dialogs.showMessage window ex.Message "Could not read structure"
                            finally
                                isBusy.Set false
                    | None -> ()
                }
                |> ignore

            /// Chooses the repairs Export applies (it doesn't export).
            let chooseAutoFixes _ =
                task {
                    match selectedFile.Current, report with
                    | Some sourcePath, Some report ->
                        let review = layoutReviews.For sourcePath
                        // An untagged PDF is tagged from its suggested structure, not by a repair.
                        let reviewNote =
                            if untagged () then
                                Some "This PDF has no tags: Export… tags a copy from its suggested structure (Suggest Structure…), with your edits on it."
                            else
                                None
                        match! Dialogs.chooseFixOptions window report reviewNote autoFixes.Current with
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
                if untagged () && not pending.Current.TagsFromReview then
                    PdfFixOptions.None
                else
                    chosen
                    ||| (if pending.Current.TagsFromReview then PdfFixOptions.TagFromLayout else PdfFixOptions.None)
                    ||| (if pending.Current.Changes > 0 && not pending.Current.TagsFromReview then PdfFixOptions.ApplyEdits else PdfFixOptions.None)

            /// Writes the next version of the PDF with saveOptions (its review, saved or not, goes in
            /// it too), then opens it.
            let export _ =
                task {
                    match selectedFile.Current with
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
                            let profile = currentProfile ()
                            isBusy.Set true
                            busyProgress.Set None
                            let! fixOutcome =
                                task {
                                    // Tagging from the layout and joining split paragraphs run the layout
                                    // detector page by page, after downloading its model the first time.
                                    let usesLayout =
                                        options.HasFlag PdfFixOptions.TagFromLayout || options.HasFlag PdfFixOptions.JoinSplitParagraphs
                                    // The suggestions with the edits on them (all of them if
                                    // it wasn't opened), reusing any pages it found.
                                    let source =
                                        if usesLayout then
                                            let suggester = new LayoutSuggester(sourcePath)
                                            Some(suggester, ReviewedLayout(LayoutPrefetcher(suggester, review.Pages), review))
                                        else
                                            None
                                    try
                                        try
                                            if usesLayout && not (LayoutModel.isDownloaded ()) then
                                                do!
                                                    LayoutModel.download
                                                        (Progress<float>(fun fraction -> busyProgress.Set(Some(DownloadingLayoutModel fraction))))
                                                        CancellationToken.None
                                            source |> Option.iter (fun (_, reviewed) -> validator.LayoutSource <- reviewed)
                                            let progress = Progress<FixProgress>(fun p -> busyProgress.Set(Some(Fixing p)))
                                            let! fix =
                                                Task.Run(fun () ->
                                                    // The review, as it is now, goes in the repaired copy. Recording
                                                    // its layout reads the PDF, so it's done here too.
                                                    validator.Review <- review.ForCopy sourcePath
                                                    validator.Edits <- RegionEdits.toElementEdits review.Edits
                                                    validator.Fix(sourcePath, profile, outputPath, options, progress))
                                            return Ok fix
                                        with
                                        | :? VeraPdfValidationException as ex -> return Error ex.Message
                                        | :? Net.Http.HttpRequestException as ex -> return Error $"Couldn't download the layout model: {ex.Message}"
                                    finally
                                        validator.LayoutSource <- null
                                        validator.Review <- null
                                        validator.Edits <- []
                                        source |> Option.iter (fun (suggester, _) -> (suggester :> IDisposable).Dispose())
                                        busyProgress.Set None
                                        isBusy.Set false
                                }

                            // Open the exported copy in its own window, where it's validated, to show
                            // what improved; this one stays on the original.
                            let! shownIn, repairedReport =
                                match fixOutcome with
                                | Ok fix when fix.WasFixed ->
                                    match fix.OutputPath with
                                    // It may be open already, from an earlier export to the same file.
                                    | NonNull path -> openElsewhere path true
                                    | Null -> Task.FromResult(window, None)
                                | _ -> Task.FromResult(window, None)

                            do! Dialogs.showFixResult shownIn fixOutcome repairedReport
                    | _ -> ()
                }
                |> ignore

            // The File menu: in the menu bar on macOS (with its ⌘ shortcuts), and at the top of the
            // window elsewhere (where the keys below are Ctrl+O, Ctrl+W, Ctrl+S and Ctrl+E).
            let menuItems = ctx.useState ((None: (NativeMenuItem * NativeMenuItem) option), false)
            ctx.useEffect (
                (fun () ->
                    let mac = OperatingSystem.IsMacOS()
                    let item (header: string) (key: Key) (action: unit -> unit) =
                        let item = NativeMenuItem(header, Command = command action)
                        if mac then item.Gesture <- KeyGesture(key, KeyModifiers.Meta)
                        item
                    let save = item "Save" Key.S (fun () -> if not isBusy.Current && edited.Current then saveReview () |> ignore)
                    save.ToolTip <-
                        "Keeps your review (edits, dismissed warnings, the title, the auto fixes chosen) in a .legibyl.json file next to the PDF, to pick up where you left off. The PDF isn't changed. Closing the window asks about changes not saved."
                    let exportItem = item "Export…" Key.E (fun () -> if not isBusy.Current then export ())
                    // The PDFs opened lately, each opened as File › Open… would.
                    let recent = NativeMenu()
                    let fillRecent () =
                        recent.Items.Clear()
                        let paths = RecentFiles.read ()
                        for path, label in List.zip paths (RecentFiles.labels paths) do
                            let entry = NativeMenuItem(label, ToolTip = path)
                            entry.Command <- command (fun () -> setSelectedFile path)
                            recent.Items.Add entry
                        if not paths.IsEmpty then recent.Items.Add(NativeMenuItemSeparator())
                        let clear = NativeMenuItem("Clear Menu", IsEnabled = not paths.IsEmpty)
                        clear.Command <- command RecentFiles.clear
                        recent.Items.Add clear
                    fillRecent ()
                    let refill = RecentFiles.Changed.Subscribe(fun () -> Avalonia.Threading.Dispatcher.UIThread.Post fillRecent)
                    window.Closed.Add(fun _ -> refill.Dispose())
                    let file = NativeMenu()
                    file.Items.Add(item "Open…" Key.O (fun () -> browse ()))
                    file.Items.Add(NativeMenuItem("Open Recent", Menu = recent))
                    file.Items.Add(NativeMenuItemSeparator())
                    file.Items.Add(item "Close Window" Key.W (fun () -> window.Close()))
                    file.Items.Add(save)
                    file.Items.Add(exportItem)
                    let menu = NativeMenu()
                    menu.Items.Add(NativeMenuItem("File", Menu = file))
                    NativeMenu.SetMenu(window, menu)
                    menuItems.Set(Some(save, exportItem))
                    if not mac then
                        window.KeyDown.Add(fun e ->
                            if e.KeyModifiers.HasFlag KeyModifiers.Control then
                                match e.Key with
                                | Key.O -> e.Handled <- true; browse ()
                                | Key.W -> e.Handled <- true; window.Close()
                                | Key.S when not isBusy.Current && edited.Current ->
                                    e.Handled <- true
                                    saveReview () |> ignore
                                | Key.E when not isBusy.Current ->
                                    e.Handled <- true
                                    export ()
                                | _ -> ())),
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
                        path |> Option.iter (fun path -> File.WriteAllText(path, ValidationReportTextFormatter.BuildReport(withoutDismissed report)))
                    | None -> ()
                }
                |> ignore

            let busy = isBusy.Current

            let fileRow =
                Grid.create [
                    Grid.margin (0.0, 0.0, 0.0, 8.0)
                    Grid.columnDefinitions "*,Auto,Auto,Auto"
                    Grid.children [
                        TextBox.create [
                            TextBox.margin (0.0, 0.0, 8.0, 0.0)
                            TextBox.verticalContentAlignment VerticalAlignment.Center
                            TextBox.isReadOnly true
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
                        ComboBox.create [
                            Grid.column 2
                            ComboBox.width 160.0
                            ComboBox.margin (0.0, 0.0, 8.0, 0.0)
                            ComboBox.isEnabled (not busy)
                            ComboBox.dataItems (List.map fst profiles)
                            ComboBox.selectedIndex profileIndex.Current
                            ComboBox.onSelectedIndexChanged (fun index ->
                                if index >= 0 && index <> profileIndex.Current then
                                    profileIndex.Set index
                                    // Re-validate automatically when the profile changes, if a file is selected.
                                    runValidation "Validating" |> ignore)
                        ]
                        Button.create [
                            Grid.column 3
                            Button.content "Validate"
                            Button.padding (16.0, 4.0)
                            Button.isEnabled (not busy && selectedFile.Current.IsSome)
                            Button.onClick (fun _ -> runValidation "Validating" |> ignore)
                        ]
                    ]
                ]

            let statusBanner =
                let banner (backgroundToken: string) (text: string) =
                    Border.create [
                        Grid.row 2
                        Border.padding (12.0, 8.0)
                        Border.cornerRadius 4.0
                        Border.margin (0.0, 0.0, 0.0, 8.0)
                        Theme.resources [ Border.BackgroundProperty, backgroundToken ]
                        Border.child (
                            TextBlock.create [
                                TextBlock.text text
                                TextBlock.fontWeight FontWeight.Bold
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.foreground Brushes.White
                            ]
                        )
                    ]
                    :> IView

                match outcome.Current with
                | NotValidated -> None
                | ValidationFailed message -> Some(banner Theme.Tokens.Warning $"Error: {message}")
                | Validated(report, _) ->
                    // As the profile picker names it ("PDF/UA-1"), not its id ("UA1").
                    let flavour =
                        profiles |> List.tryFind (fun (_, profile) -> profile = report.Profile) |> Option.map fst
                        |> Option.defaultValue (report.FlavourId.ToUpperInvariant())
                    let shownWarnings = (withoutDismissed report).WarningCount
                    let warnings =
                        match report.WarningCount - shownWarnings with
                        | 0 -> $"{shownWarnings} warnings"
                        | hidden -> $"{shownWarnings} warnings, {hidden} dismissed"
                    Some(
                        if report.IsCompliant then
                            banner Theme.Tokens.Success $"PASS — compliant with {flavour} ({report.TotalAssertions} checks, {warnings})"
                        else
                            banner
                                Theme.Tokens.Danger
                                $"FAIL — {report.FailedCount} of {report.TotalAssertions} checks failed against {flavour} ({warnings})"
                    )

            let allRows =
                match outcome.Current with
                | Validated(_, rows) -> rows
                | _ -> []

            let dismissedCount = allRows |> List.filter _.Dismissed |> List.length

            // Dismissed warnings are hidden unless the user asks to see them.
            let rows = if showDismissed.Current then allRows else allRows |> List.filter (not << _.Dismissed)

            /// Right-click menu for the selected finding (the grid selects a row when it's right-clicked).
            let findingMenu =
                let selected = selectedRow.Current
                let canDismiss = selected |> Option.exists (fun row -> Dismissals.canDismiss row.Violation)
                ContextMenu.create [
                    ContextMenu.viewItems [
                        match selected with
                        | Some row when row.Dismissed ->
                            let restore (header: string) (key: RuleViolation -> Dismissals.Key) =
                                MenuItem.create [ MenuItem.header header; MenuItem.onClick (fun _ -> setDismissed row key false) ]
                            if dismissed.Current.ContainsKey(Dismissals.keyOf row.Violation) then
                                restore "Restore warning" Dismissals.keyOf
                            if Dismissals.isRuleIgnored dismissed.Current row.Violation then
                                restore $"Stop ignoring {row.Violation.RuleId} warnings" Dismissals.ruleKeyOf
                        | _ ->
                            MenuItem.create [
                                MenuItem.header (if canDismiss || selected.IsNone then "Dismiss warning" else "Dismiss warning (only warnings can be dismissed)")
                                MenuItem.isEnabled canDismiss
                                MenuItem.onClick (fun _ -> selected |> Option.iter (fun row -> setDismissed row Dismissals.keyOf true))
                            ]
                            MenuItem.create [
                                MenuItem.header (
                                    match selected with
                                    | Some row when canDismiss -> $"Ignore all {row.Violation.RuleId} warnings"
                                    | _ -> "Ignore this type of warning"
                                )
                                MenuItem.isEnabled canDismiss
                                ToolTip.tip "Hides every warning of this type in this PDF, wherever it is."
                                MenuItem.onClick (fun _ -> selected |> Option.iter (fun row -> setDismissed row Dismissals.ruleKeyOf true))
                            ]
                    ]
                ]

            let gridFontSize = 12.0

            // The findings grouped by rule, under a header saying how many; made again only when
            // the rows are (a new view would reset the grid).
            let groupedCache = ctx.useState ((None: (RuleViolationRow list * Avalonia.Collections.DataGridCollectionView) option), false)
            let groupedRows (rows: RuleViolationRow list) =
                match groupedCache.Current with
                | Some(cached, view) when cached.Length = rows.Length && List.forall2 (fun a b -> obj.ReferenceEquals(a, b)) cached rows -> view
                | _ ->
                    let view = Avalonia.Collections.DataGridCollectionView(rows)
                    view.GroupDescriptions.Add(Avalonia.Collections.DataGridPathGroupDescription "Rule")
                    groupedCache.Set(Some(rows, view))
                    view

            let resultsGrid =
                DataGrid.create [
                    DataGrid.isReadOnly true
                    DataGrid.canUserResizeColumns true
                    DataGrid.canUserSortColumns false
                    gridLinesVisibility DataGridGridLinesVisibility.Horizontal
                    Theme.resources [ DataGrid.BorderBrushProperty, Theme.Tokens.Border ]
                    DataGrid.borderThickness 1.0
                    DataGrid.contextMenu findingMenu
                    DataGrid.init (fun grid ->
                        // A rule found many times starts collapsed to its header (with its count); its
                        // first finding's details show beside the list, and the header expands it.
                        grid.LoadingRowGroup.Add(fun e ->
                            match e.RowGroupHeader.DataContext with
                            | :? Avalonia.Collections.DataGridCollectionViewGroup as group when group.ItemCount > 3 ->
                                Avalonia.Threading.Dispatcher.UIThread.Post(fun () -> grid.CollapseRowGroup(group, false))
                            | _ -> ())
                        // Shown dismissed warnings are dimmed (rows are rebuilt when that changes).
                        grid.LoadingRow.Add(fun e ->
                            e.Row.Opacity <-
                                match e.Row.DataContext with
                                | :? RuleViolationRow as row when row.Dismissed -> 0.5
                                | _ -> 1.0))
                    DataGrid.onSelectedItemChanged (fun item ->
                        match item with
                        | :? RuleViolationRow as row when
                            not (selectedRow.Current |> Option.exists (fun current -> obj.ReferenceEquals(current, row)))
                            ->
                            selectRow (Some row)
                        | _ -> ())
                    DataGrid.columns [
                        let textColumn (header: string) (property: string) (width: float) =
                            DataGridTextColumn.create [
                                DataGridTextColumn.header header
                                DataGridTextColumn.binding (Binding property)
                                DataGridTextColumn.width (DataGridLength width)
                                DataGridTextColumn.fontSize gridFontSize
                            ]
                        DataGridTemplateColumn.create [
                            DataGridTemplateColumn.header "Type"
                            DataGridTemplateColumn.width (DataGridLength 64.0)
                            DataGridTemplateColumn.cellTemplate (
                                DataTemplateView<RuleViolationRow>.create (fun row -> severityIcon row.Violation)
                            )
                        ]
                        DataGridTemplateColumn.create [
                            DataGridTemplateColumn.header "Fix"
                            DataGridTemplateColumn.width (DataGridLength 52.0)
                            DataGridTemplateColumn.cellTemplate (
                                DataTemplateView<RuleViolationRow>.create (fun row -> fixableIcon row)
                            )
                        ]
                        textColumn "Clause" "Clause" 72.0
                        textColumn "Test" "TestNumber" 60.0
                        textColumn "Page" "PageDisplay" 72.0
                        DataGridTemplateColumn.create [
                            DataGridTemplateColumn.header "Location"
                            DataGridTemplateColumn.width (DataGridLength(1.0, DataGridLengthUnitType.Star))
                            DataGridTemplateColumn.init (fun column -> column.MinWidth <- 140.0)
                            DataGridTemplateColumn.cellTemplate (
                                DataTemplateView<RuleViolationRow>.create (fun row ->
                                    TextBlock.create [
                                        TextBlock.text (if row.Dismissed then $"{row.FriendlyLocation} (dismissed)" else row.FriendlyLocation)
                                        TextBlock.fontSize gridFontSize
                                        TextBlock.textWrapping TextWrapping.Wrap
                                        TextBlock.margin (8.0, 3.0)
                                        TextBlock.verticalAlignment VerticalAlignment.Center
                                        ToolTip.tip row.Message
                                    ])
                            )
                        ]
                    ]
                    DataGrid.items (groupedRows rows)
                    // By item: grouping orders them by rule.
                    match selectedRow.Current with
                    | Some row -> AttrBuilder<DataGrid>.CreateProperty<objnull>(DataGrid.SelectedItemProperty, box row, ValueNone)
                    | None -> DataGrid.selectedIndex -1
                ]

            /// What the details pane says when no finding is selected: how to start, in an empty window;
            /// for a PDF that passes, what automated checks can't judge.
            let guidance () : IView option =
                let heading (text: string) =
                    TextBlock.create [
                        TextBlock.text text
                        TextBlock.fontSize 16.0
                        TextBlock.fontWeight FontWeight.SemiBold
                        TextBlock.margin (0.0, 0.0, 0.0, 10.0)
                        TextBlock.textWrapping TextWrapping.Wrap
                    ]
                    :> IView
                let line (marker: string) (text: string) =
                    Grid.create [
                        Grid.columnDefinitions "Auto,*"
                        Grid.margin (0.0, 0.0, 0.0, 8.0)
                        Grid.children [
                            TextBlock.create [
                                TextBlock.text marker
                                TextBlock.width 22.0
                                Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                            ]
                            TextBlock.create [ Grid.column 1; TextBlock.text text; TextBlock.textWrapping TextWrapping.Wrap ]
                        ]
                    ]
                    :> IView
                let note (text: string) =
                    TextBlock.create [
                        TextBlock.text text
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.margin (0.0, 6.0, 0.0, 0.0)
                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                    ]
                    :> IView
                let panel (children: IView list) : IView =
                    ScrollViewer.create [
                        Grid.column 2
                        ScrollViewer.margin (12.0, 0.0, 0.0, 0.0)
                        ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Disabled
                        ScrollViewer.content (StackPanel.create [ StackPanel.maxWidth 560.0; StackPanel.horizontalAlignment HorizontalAlignment.Left; StackPanel.children children ])
                    ]
                match selectedFile.Current, outcome.Current with
                | None, _ when not busy ->
                    Some(
                        panel [
                            heading "Making a PDF accessible"
                            line "1." "Open a PDF: Browse…, drop it on this window, or File › Open…."
                            line "2." "Read what's found. Failures (red) are what PDF/UA requires; warnings (orange) are worth a look. A wrench marks what an auto fix can repair; a wand, what suggesting the structure fixes."
                            line "3." "Structure… shows the tags a screen reader uses. Check the headings, the figures' alt text and the reading order, and edit them there. A PDF with no tags gets Suggest Structure… instead."
                            line "4." "Auto Fixes… chooses repairs, like the document's language and title."
                            line "5." "Export… writes a fixed copy and opens it, to check again. The PDF you opened isn't changed. File › Save keeps your review (edits, dismissed warnings, chosen fixes) to pick up later."
                            note "PDF/UA is the standard for PDFs that screen readers and other assistive technology can use."
                        ]
                    )
                | Some _, Validated(report, _) when report.IsCompliant ->
                    let profile = fst profiles[profileIndex.Current]
                    Some(
                        panel [
                            heading $"Passes {profile}'s automated checks"
                            note "Automated checks can't judge everything a person needs. Worth checking yourself:"
                            line "•" "Alt text: does each figure's say what it's for here, not only what it shows? Decorative images shouldn't be read at all."
                            line "•" "Headings: is each one a heading, at the right level, and does every section have one?"
                            line "•" "Reading order: does the content read in a sensible order?"
                            line "•" "Tables: are their header cells marked as headers?"
                            line "•" "Links: does each one's text or description say where it goes?"
                            note "Structure… shows the tags and their alt text, and numbers each page's elements in reading order."
                        ]
                    )
                | _ -> None

            let findingDetails (row: RuleViolationRow) =
                Grid.create [
                    Grid.column 2
                    Grid.margin (12.0, 0.0, 0.0, 0.0)
                    Grid.rowDefinitions "Auto,Auto,Auto,*"
                    Grid.children [
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
                                        detailText row.Message false [ TextBox.maxHeight 130.0 ]
                                        // In plain language, and what to do when no auto fix repairs it.
                                        match RuleGuide.forFinding row.Violation with
                                        | Some guide ->
                                            TextBlock.create [
                                                TextBlock.text "What it means"
                                                TextBlock.fontWeight FontWeight.SemiBold
                                                TextBlock.margin (0.0, 8.0, 0.0, 0.0)
                                            ]
                                            detailText guide.Meaning false []
                                            if not row.Violation.IsFixable then
                                                TextBlock.create [
                                                    TextBlock.text "What to do"
                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                    TextBlock.margin (0.0, 8.0, 0.0, 0.0)
                                                ]
                                                detailText guide.WhatToDo false []
                                        | None -> ()
                                        if row.Violation.IsFixable then
                                            fixableHeading row.Violation (TextBlock.create [ TextBlock.text "Fixable"; TextBlock.fontWeight FontWeight.SemiBold ])
                                            |> View.withAttrs [ Control.margin (0.0, 8.0, 0.0, 0.0) ]
                                            // Tagging from the layout takes a detector's guesses: they're
                                            // worth checking first.
                                            let next =
                                                if row.Violation.FixOptions.HasFlag PdfFixOptions.TagFromLayout then
                                                    "Open Suggest Structure… to see the headings, figures and reading order suggested for each page; Export… then tags a copy from it."
                                                else
                                                    "Choose it in Auto Fixes…, then Export… a copy of this PDF."
                                            detailText $"{row.Fixable}. {next}" false []
                                        TextBlock.create [
                                            TextBlock.text "Location"
                                            TextBlock.fontWeight FontWeight.SemiBold
                                            TextBlock.margin (0.0, 8.0, 0.0, 0.0)
                                        ]
                                        detailText row.FriendlyLocation true []
                                        TextBlock.create [
                                            TextBlock.text "Raw location"
                                            TextBlock.fontWeight FontWeight.SemiBold
                                            TextBlock.margin (0.0, 8.0, 0.0, 0.0)
                                        ]
                                        detailText
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

            // Kept in step with the buttons.
            match menuItems.Current with
            | Some(save, exportItem) ->
                save.IsEnabled <- not busy && edited.Current
                exportItem.IsEnabled <- not busy && selectedFile.Current.IsSome && saveOptions () <> PdfFixOptions.None
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
                                ]
                                match busyProgress.Current with
                                | Some progress when busy -> progressLine progress
                                | _ -> ()
                            ]
                        ]
                        match statusBanner with
                        | Some banner -> banner
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
                                    Grid.rowDefinitions "*,Auto"
                                    Grid.children [
                                        resultsGrid
                                        DockPanel.create [
                                          Grid.row 1
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
                                                Button.create [
                                                    Button.content "Copy Summary"
                                                    Button.padding (12.0, 4.0)
                                                    Button.margin (0.0, 0.0, 8.0, 0.0)
                                                    Button.isEnabled report.IsSome
                                                    Button.onClick (fun _ ->
                                                        report |> Option.iter (withoutDismissed >> ValidationReportTextFormatter.BuildReport >> copyText))
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
                                match selectedRow.Current, guidance () with
                                | Some row, _ -> findingDetails row
                                | None, Some panel -> panel
                                | None, None -> ()
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
                                let structureName =
                                    match changes with
                                    | 0 -> structureLabel
                                    | 1 -> $"{structureLabel}, 1 edit not in the PDF yet"
                                    | n -> $"{structureLabel}, {n} edits not in the PDF yet"
                                let autoFixesName = if chosen = 0 then "Auto Fixes…" else $"Auto Fixes…, {chosen} chosen"
                                Button.create [
                                    Button.content (badged (if suggest then suggestIcon else None) structureLabel changes)
                                    AttrBuilder<Button>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, structureName, ValueNone)
                                    Button.padding (12.0, 4.0)
                                    Button.margin (0.0, 0.0, 8.0, 0.0)
                                    Button.isEnabled (not busy && selectedFile.Current.IsSome)
                                    // Semi's filled button, in its warning colour.
                                    if suggest then
                                        Button.classes [ "Warning" ]
                                        Theme.resources [ Button.ThemeProperty, "SolidButton" ]
                                    ToolTip.tip (
                                        if suggest then
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
                                        "Choose the repairs Export applies, like the document's language, title and metadata; it lists the ones this PDF needs. Disabled when it passes with no warnings, since there's nothing left to fix."
                                    Button.onClick chooseAutoFixes
                                ]
                                Button.create [
                                    Button.content "Export…"
                                    Button.padding (12.0, 4.0)
                                    Button.margin (0.0, 0.0, 8.0, 0.0)
                                    Button.isEnabled (not busy && selectedFile.Current.IsSome && saveOptions () <> PdfFixOptions.None)
                                    ToolTip.tip
                                        "Writes the next version of the PDF (flyer.v2.pdf) with your edits and the chosen auto fixes, and opens it. The original isn't changed."
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
        // Semi's column headers are larger than the 12pt cells; match them so short
        // headers aren't clipped in the narrow results pane.
        let headerStyle = Style(fun selector -> selector.OfType<DataGridColumnHeader>())
        headerStyle.Setters.Add(Setter(DataGridColumnHeader.FontSizeProperty, 12.0))
        this.Styles.Add headerStyle
        // A group of findings is headed by its rule and how many there are, not "Rule:".
        let groupStyle = Style(fun selector -> selector.OfType<DataGridRowGroupHeader>())
        groupStyle.Setters.Add(Setter(DataGridRowGroupHeader.IsPropertyNameVisibleProperty, false))
        // The count leads the header's text instead (see RuleViolationRow.Rule).
        groupStyle.Setters.Add(Setter(DataGridRowGroupHeader.IsItemCountVisibleProperty, false))
        groupStyle.Setters.Add(Setter(DataGridRowGroupHeader.FontSizeProperty, 12.0))
        this.Styles.Add groupStyle
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
