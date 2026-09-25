namespace Legibyl.Gui

open System
open System.IO
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
open Legibyl.Rendering
open Legibyl.Validation

/// Flattened, display-friendly view of a RuleViolation for the results grid.
type RuleViolationRow =
    { Violation: RuleViolation
      Clause: string
      TestNumber: int
      Severity: string
      /// What the Fix dialog can do about this finding, e.g. "Fix XMP metadata"; empty if nothing.
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
      Dismissed: bool }

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
          Dismissed = false }

module private MainView =
    let profiles =
        [ "PDF/UA-1", PdfProfile.PdfUa1
          "PDF/A-1b", PdfProfile.PdfA1B
          "PDF/A-2b", PdfProfile.PdfA2B
          "PDF/A-2u", PdfProfile.PdfA2U
          "PDF/A-3b", PdfProfile.PdfA3B
          "PDF/A-4", PdfProfile.PdfA4 ]

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

    /// The details pane's "Fixable" heading, anchored by the wrench.
    let fixableHeading (heading: IView) =
        withIcon (decorativeIcon fixIcon Theme.Tokens.Primary 14.0) heading

    /// A wrench for findings the Fix dialog can repair; nothing otherwise.
    let fixableIcon (row: RuleViolationRow) : IView =
        if row.Violation.IsFixable then
            gridIcon fixIcon Theme.Tokens.Primary $"Fixable: {row.Fixable}"
        else
            TextBlock.create []

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

    /// What the progress row under the file row is showing.
    type BusyProgress =
        | Validating of ValidationProgress
        | ReadingStructure of StructureReadProgress

    let private formatElapsed (elapsed: TimeSpan) =
        if elapsed.TotalHours >= 1.0 then elapsed.ToString(@"h\:mm\:ss") else elapsed.ToString(@"m\:ss")

    /// e.g. "Page 57 of 675   Checks 394,185   Rules failed 1   Elapsed 0:07": each value sits
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
        let status, stats =
            match progress with
            | Validating progress ->
                let status =
                    match progress.Stage with
                    | ValidationStage.CheckingPages -> $"Page {progress.Page:N0} of {progress.PageCount:N0}"
                    | ValidationStage.Finishing -> "Finishing up…"
                    | _ -> "Reading the PDF…"
                status,
                [ stat "Checks" 80.0 (progress.Checks.ToString "N0")
                  stat "Rules failed" 32.0 (progress.FailedRules.ToString "N0")
                  stat "Elapsed" 48.0 (formatElapsed progress.Elapsed) ]
            | ReadingStructure progress ->
                let status =
                    match progress.Stage with
                    | StructureReadStage.ReadingText -> $"Reading page text {progress.Page:N0} of {progress.PageCount:N0}"
                    | StructureReadStage.ReadingLinks -> "Reading links…"
                    | _ -> "Building the tree…"
                status,
                [ stat "Elements" 64.0 (progress.Elements.ToString "N0")
                  stat "Elapsed" 48.0 (formatElapsed progress.Elapsed) ]
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
        | Some(Validating progress) when progress.Stage = ValidationStage.CheckingPages && progress.PageCount > 0 ->
            fraction progress.Page progress.PageCount
        | Some(Validating progress) when progress.Stage = ValidationStage.Finishing -> full
        | Some(ReadingStructure progress) when progress.Stage = StructureReadStage.ReadingText && progress.PageCount > 0 ->
            fraction progress.Page progress.PageCount
        | Some(ReadingStructure _) -> full
        | _ -> [ ProgressBar.isIndeterminate true ]

    let view (window: Window) (openRequests: IReadable<string option>) =
        Component(fun ctx ->
            let validator = ctx.useStateLazy(fun () -> VeraPdfValidator()).Current
            let renderer = ctx.useStateLazy(fun () -> new PdfRegionRenderer()).Current
            let structureCache = ctx.useStateLazy(fun () -> StructureCache()).Current
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
            // Warnings dismissed for the selected file, whether they're saved next to it, and
            // whether the results list shows them.
            let dismissed = ctx.useState (Map.empty: Dismissals.Dismissed)
            let rememberDismissals = ctx.useState false
            let showDismissed = ctx.useState false

            let currentProfile () = snd profiles[profileIndex.Current]

            let report =
                match outcome.Current with
                | Validated(report, _) -> Some report
                | _ -> None

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

            /// Validates the selected file, returning its report (None if it couldn't be validated).
            let runValidation () : Task<ValidationReport option> =
                task {
                    match selectedFile.Current with
                    | Some filePath when not isBusy.Current ->
                        let profile = currentProfile ()
                        isBusy.Set true
                        clearResults ()
                        busyProgress.Set None
                        // Created on the UI thread, so its reports arrive there too.
                        let progress = Progress<ValidationProgress>(fun p -> busyProgress.Set(Some(Validating p)))
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
                                outcome.Set(Validated(report, rows))
                                selectRow (rows |> List.tryFind (fun row -> showDismissed.Current || not row.Dismissed))
                                return Some report
                            with :? VeraPdfValidationException as ex ->
                                outcome.Set(ValidationFailed ex.Message)
                                return None
                        finally
                            isBusy.Set false
                    | _ -> return None
                }

            /// Opens a file and validates it with the current profile.
            let openFile (path: string) =
                selectedFile.Set(Some path)
                clearResults ()
                match Dismissals.load path with
                | Some saved ->
                    dismissed.Set saved
                    rememberDismissals.Set true
                | None ->
                    dismissed.Set Map.empty
                    rememberDismissals.Set false
                showDismissed.Set false
                runValidation ()

            // Auto-validate as soon as a file is picked/dropped.
            let setSelectedFile (path: string) = openFile path |> ignore

            // Files opened from outside the view (e.g. by the headless screenshot harness).
            ctx.useEffect (
                (fun () ->
                    match openRequests.Current with
                    | Some path -> setSelectedFile path
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
                                do!
                                    Dialogs.showMessage
                                        window
                                        ("This PDF has no structure tree at all — it is completely untagged, so there is no "
                                         + "semantic hierarchy for a screen reader to use.")
                                        "Document Structure"
                            | structure -> StructureTreeWindow(structure, filePath).Show window
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

            let fix _ =
                task {
                    match selectedFile.Current, report with
                    | Some sourcePath, Some report ->
                        match! Dialogs.chooseFixOptions window report with
                        | None -> ()
                        | Some options ->
                            let! outputPath =
                                Dialogs.pickSavePath
                                    window
                                    (FilePickerSaveOptions(
                                        Title = "Save repaired PDF",
                                        SuggestedFileName =
                                            $"{Path.GetFileNameWithoutExtension sourcePath}.fixed{Path.GetExtension sourcePath}",
                                        DefaultExtension = "pdf",
                                        FileTypeChoices = [ pdfFileType; FilePickerFileTypes.All ]
                                    ))
                                    sourcePath

                            match outputPath with
                            | None -> ()
                            | Some outputPath ->
                                let profile = currentProfile ()
                                isBusy.Set true
                                let! fixOutcome =
                                    task {
                                        try
                                            try
                                                let! fix = Task.Run(fun () -> validator.Fix(sourcePath, profile, outputPath, options))
                                                return Ok fix
                                            with :? VeraPdfValidationException as ex ->
                                                return Error ex.Message
                                        finally
                                            isBusy.Set false
                                    }

                                // Switch to the repaired copy and validate it (after clearing the busy
                                // flag, so the validation can start) to show what improved.
                                let! repairedReport =
                                    match fixOutcome with
                                    | Ok fix when fix.WasFixed ->
                                        match fix.OutputPath with
                                        | NonNull path -> openFile path
                                        | Null -> Task.FromResult None
                                    | _ -> Task.FromResult None

                                do! Dialogs.showFixResult window fixOutcome repairedReport
                    | _ -> ()
                }
                |> ignore

            let copyText (text: string) =
                match window.Clipboard with
                | NonNull clipboard -> clipboard.SetTextAsync text |> ignore
                | Null -> ()

            /// Writes the dismissals next to the PDF when the user has asked to remember them.
            /// If that fails, stops remembering and says why.
            let saveDismissals (next: Dismissals.Dismissed) =
                task {
                    match selectedFile.Current with
                    | Some filePath when rememberDismissals.Current ->
                        try
                            Dismissals.save filePath next
                        with :? IOException | :? UnauthorizedAccessException as ex ->
                            rememberDismissals.Set false
                            do! Dialogs.showMessage window $"Couldn't save dismissals next to the PDF: {ex.Message}" "Dismissed warnings"
                    | _ -> ()
                }

            /// Dismisses or restores a warning, keeping the selection on it, or moving it on to
            /// the next shown finding if it's now hidden.
            let setDismissed (row: RuleViolationRow) (isDismissed: bool) =
                let next =
                    if isDismissed then dismissed.Current.Add(Dismissals.keyOf row.Violation, row.Message)
                    else dismissed.Current.Remove(Dismissals.keyOf row.Violation)
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
                saveDismissals next |> ignore

            /// Starts or stops keeping this PDF's dismissals in a file next to it.
            let toggleRememberDismissals () =
                task {
                    match selectedFile.Current with
                    | Some filePath when rememberDismissals.Current ->
                        try
                            Dismissals.forget filePath
                            rememberDismissals.Set false
                        with :? IOException | :? UnauthorizedAccessException as ex ->
                            do! Dialogs.showMessage window $"Couldn't delete {Dismissals.filePath filePath}: {ex.Message}" "Dismissed warnings"
                    | Some _ ->
                        rememberDismissals.Set true
                        do! saveDismissals dismissed.Current
                    | None -> ()
                }
                |> ignore

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
                            TextBox.text (selectedFile.Current |> Option.defaultValue "Drop a PDF here or click Browse...")
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
                                    runValidation () |> ignore)
                        ]
                        Button.create [
                            Grid.column 3
                            Button.content "Validate"
                            Button.padding (16.0, 4.0)
                            Button.isEnabled (not busy && selectedFile.Current.IsSome)
                            Button.onClick (fun _ -> runValidation () |> ignore)
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
                    let flavour = report.FlavourId.ToUpperInvariant()
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
                            MenuItem.create [
                                MenuItem.header "Restore warning"
                                MenuItem.onClick (fun _ -> setDismissed row false)
                            ]
                        | _ ->
                            MenuItem.create [
                                MenuItem.header (if canDismiss || selected.IsNone then "Dismiss warning" else "Dismiss warning (only warnings can be dismissed)")
                                MenuItem.isEnabled canDismiss
                                MenuItem.onClick (fun _ -> selected |> Option.iter (fun row -> setDismissed row true))
                            ]
                        Separator.create []
                        MenuItem.create [
                            MenuItem.header "Remember dismissals for this PDF"
                            MenuItem.toggleType MenuItemToggleType.CheckBox
                            MenuItem.isChecked rememberDismissals.Current
                            MenuItem.isEnabled selectedFile.Current.IsSome
                            ToolTip.tip (
                                match selectedFile.Current with
                                | Some filePath -> $"Keeps them in {Path.GetFileName(Dismissals.filePath filePath)} next to the PDF."
                                | None -> "Open a PDF first."
                            )
                            MenuItem.onClick (fun _ -> toggleRememberDismissals ())
                        ]
                    ]
                ]

            let gridFontSize = 12.0

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
                    DataGrid.items rows
                    DataGrid.selectedIndex (
                        match selectedRow.Current with
                        | Some selected -> rows |> List.tryFindIndex (fun row -> obj.ReferenceEquals(row, selected)) |> Option.defaultValue -1
                        | None -> -1
                    )
                ]

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
                                                TextBlock.text $"{row.Severity}: clause {row.Clause}, test {row.TestNumber} — {row.PageDisplay}"
                                                TextBlock.fontSize 16.0
                                                TextBlock.fontWeight FontWeight.SemiBold
                                                TextBlock.textWrapping TextWrapping.Wrap
                                            ])
                                        |> View.withAttrs [ Control.margin (0.0, 0.0, 0.0, 8.0) ]
                                        TextBlock.create [ TextBlock.text "Message"; TextBlock.fontWeight FontWeight.SemiBold ]
                                        detailText row.Message false [ TextBox.maxHeight 130.0 ]
                                        if row.Violation.IsFixable then
                                            fixableHeading (TextBlock.create [ TextBlock.text "Fixable"; TextBlock.fontWeight FontWeight.SemiBold ])
                                            |> View.withAttrs [ Control.margin (0.0, 8.0, 0.0, 0.0) ]
                                            detailText $"{row.Fixable}. Use Fix… to repair a copy of this PDF." false []
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
                            match selectedRow.Current with
                            | Some row -> findingDetails row
                            | None -> ()
                        ]
                    ]
                    StackPanel.create [
                        Grid.row 4
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.horizontalAlignment HorizontalAlignment.Right
                        StackPanel.margin (0.0, 8.0, 0.0, 0.0)
                        StackPanel.children [
                            Button.create [
                                Button.content "View Structure…"
                                Button.padding (12.0, 4.0)
                                Button.margin (0.0, 0.0, 8.0, 0.0)
                                Button.isEnabled (not busy && selectedFile.Current.IsSome)
                                ToolTip.tip
                                    "Shows the tagged logical structure tree (the semantic heading/paragraph/list/table/link hierarchy a screen reader uses) and lets you export it as JSON."
                                Button.onClick viewStructure
                            ]
                            Button.create [
                                Button.content "Fix…"
                                Button.padding (12.0, 4.0)
                                Button.margin (0.0, 0.0, 8.0, 0.0)
                                Button.isEnabled (not busy && canAttemptFix)
                                ToolTip.tip
                                    "Opens a dialog of selectable best-effort repairs, including XMP metadata, page annotation tab order, and empty paragraph tags. Disabled once the file is already fully compliant, since there'd be nothing left to fix."
                                Button.onClick fix
                            ]
                        ]
                    ]
                ]
            ])

open MainView

type MainWindow() as this =
    inherit HostWindow(Title = "Legibyl — PDF/UA Validator", Width = 1200.0, Height = 720.0, MinWidth = 900.0, MinHeight = 560.0)

    let openRequests: IWritable<string option> = new State<_>(None)

    do
        // Semi's column headers are larger than the 12pt cells; match them so short
        // headers aren't clipped in the narrow results pane.
        let headerStyle = Style(fun selector -> selector.OfType<DataGridColumnHeader>())
        headerStyle.Setters.Add(Setter(DataGridColumnHeader.FontSizeProperty, 12.0))
        this.Styles.Add headerStyle
        this.Content <- view this openRequests

    /// Opens and validates a PDF, as if it had been picked or dropped.
    member _.OpenFile(path: string) = openRequests.Set(Some path)
