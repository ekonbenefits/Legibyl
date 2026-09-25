module Legibyl.Gui.Dialogs

open System.IO
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Platform.Storage
open Legibyl.Validation

/// Shows a save dialog starting in nearPath's folder and returns the chosen local
/// path, or None if the user cancelled.
let pickSavePath (topLevel: TopLevel) (options: FilePickerSaveOptions) (nearPath: string) : Task<string option> =
    task {
        match Path.GetDirectoryName nearPath with
        | NonNull folder when folder <> "" ->
            let! start = topLevel.StorageProvider.TryGetFolderFromPathAsync folder
            options.SuggestedStartLocation <- start
        | _ -> ()

        let! file = topLevel.StorageProvider.SaveFilePickerAsync options
        return file |> Option.ofObj |> Option.bind (fun f -> Option.ofObj (f.TryGetLocalPath()))
    }

let private dialogWindow (title: string) (width: float) =
    HostWindow(
        Title = title,
        Width = width,
        SizeToContent = SizeToContent.Height,
        CanResize = false,
        ShowInTaskbar = false,
        WindowStartupLocation = WindowStartupLocation.CenterOwner
    )

/// Minimal modal OK-only message box. Avalonia has no built-in equivalent of WPF's
/// MessageBox, and this is all the app needs.
let showMessage (owner: Window) (message: string) (title: string) : Task =
    let window = dialogWindow title 460.0
    window.Content <-
        Component(fun _ ->
            StackPanel.create [
                StackPanel.margin 16.0
                StackPanel.spacing 16.0
                StackPanel.children [
                    SelectableTextBlock.create [
                        SelectableTextBlock.text message
                        SelectableTextBlock.textWrapping TextWrapping.Wrap
                    ]
                    Button.create [
                        Button.content "OK"
                        Button.minWidth 82.0
                        Button.isDefault true
                        Button.isCancel true
                        Button.horizontalAlignment HorizontalAlignment.Right
                        Button.horizontalContentAlignment HorizontalAlignment.Center
                        Button.onClick (fun _ -> window.Close())
                    ]
                ]
            ])
    window.ShowDialog owner

/// Lets the user pick which repairs to run. Returns None if the user cancelled.
let chooseFixOptions (owner: Window) : Task<PdfFixOptions option> =
    let window = dialogWindow "Fix PDF" 520.0
    window.Content <-
        Component(fun ctx ->
            let metadata = ctx.useState true
            let annotationTabs = ctx.useState true
            let emptyParagraphs = ctx.useState false
            let linkContents = ctx.useState true
            let documentTitle = ctx.useState true

            let checkBox (state: IWritable<bool>) (content: IView) =
                CheckBox.create [
                    CheckBox.margin (0.0, 0.0, 0.0, 10.0)
                    CheckBox.isChecked state.Current
                    CheckBox.onIsCheckedChanged (fun e ->
                        match e.Source with
                        | :? CheckBox as checkBox -> state.Set(checkBox.IsChecked.GetValueOrDefault())
                        | _ -> ())
                    CheckBox.content content
                ]

            let label (text: string) =
                TextBlock.create [ TextBlock.text text; TextBlock.textWrapping TextWrapping.Wrap ]

            let onContinue _ =
                let options =
                    [ metadata.Current, PdfFixOptions.Metadata
                      annotationTabs.Current, PdfFixOptions.AnnotationTabs
                      emptyParagraphs.Current, PdfFixOptions.RemoveEmptyParagraphs
                      linkContents.Current, PdfFixOptions.LinkContents
                      documentTitle.Current, PdfFixOptions.DocumentTitle ]
                    |> List.fold (fun all (isChecked, option) -> if isChecked then all ||| option else all) PdfFixOptions.None

                if options = PdfFixOptions.None then
                    showMessage window "Select at least one fix to apply." "Fix PDF" |> ignore
                else
                    window.Close(Some options)

            Grid.create [
                Grid.margin 14.0
                Grid.rowDefinitions "Auto,*,Auto"
                Grid.children [
                    TextBlock.create [
                        TextBlock.text "Choose the repairs to apply to a copy of this PDF:"
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.margin (0.0, 0.0, 0.0, 12.0)
                    ]
                    StackPanel.create [
                        Grid.row 1
                        StackPanel.children [
                            checkBox metadata (label "Fix XMP metadata using veraPDF (metadata stream, PDF/UA identifier, Info sync)")
                            checkBox annotationTabs (label "Set /Tabs /S on pages that contain annotations")
                            checkBox linkContents (label "Describe table-of-contents links that have no description, using the entry's own text (/Contents)")
                            checkBox documentTitle (label "Give the document a title, and show it in place of the file name (/DisplayDocTitle)")
                            checkBox emptyParagraphs (label "Remove empty semantic /P structure elements (optional cleanup)")
                            TextBlock.create [
                                Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.margin (0.0, 8.0, 0.0, 16.0)
                                TextBlock.text
                                    "These repairs are best-effort structural cleanup. They do not add missing alt text, retag untagged content, or infer table headers."
                            ]
                        ]
                    ]
                    StackPanel.create [
                        Grid.row 2
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.horizontalAlignment HorizontalAlignment.Right
                        StackPanel.children [
                            Button.create [
                                Button.content "Cancel"
                                Button.minWidth 82.0
                                Button.margin (0.0, 0.0, 8.0, 0.0)
                                Button.isCancel true
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.onClick (fun _ -> window.Close(None: PdfFixOptions option))
                            ]
                            Button.create [
                                Button.content "Continue…"
                                Button.minWidth 96.0
                                Button.isDefault true
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.onClick onContinue
                            ]
                        ]
                    ]
                ]
            ])

    // Closing with the title-bar button returns null, which is None.
    window.ShowDialog<PdfFixOptions option> owner

type private StatusStyle =
    { Title: string
      Detail: string
      Glyph: string
      ColorToken: string }

let private statusStyle (outcome: Result<FixResult, string>) =
    match outcome with
    | Error message ->
        { Title = "Repair failed"; Detail = message; Glyph = "✕"; ColorToken = Theme.Tokens.Danger }
    | Ok fix ->
        match fix.Status with
        | RepairStatus.Success ->
            { Title = "Repaired"
              Detail = "The selected fixes changed the file, and a repaired copy was saved."
              Glyph = "✓"
              ColorToken = Theme.Tokens.Success }
        | RepairStatus.IdRemoved ->
            { Title = "Conformance claim removed"
              Detail =
                "The file claimed PDF/A or PDF/UA conformance it doesn't meet, so veraPDF removed that claim. "
                + "Without it, the check that requires a conformance claim now fails too, "
                + "so the repaired copy can show one more failure."
              Glyph = "!"
              ColorToken = Theme.Tokens.Warning }
        | RepairStatus.NoAction ->
            { Title = "Nothing to repair"
              Detail = "The selected fixes had nothing to change in this file, so no copy was saved."
              Glyph = "i"
              ColorToken = Theme.Tokens.Primary }
        | RepairStatus.WontFix ->
            { Title = "Couldn't repair"
              Detail = "veraPDF found no way to fix these problems with the selected repairs, so no copy was saved."
              Glyph = "!"
              ColorToken = Theme.Tokens.Warning }
        | _ ->
            { Title = "Repair failed"
              Detail = "veraPDF tried to repair the metadata but failed, so no copy was saved."
              Glyph = "✕"
              ColorToken = Theme.Tokens.Danger }

let private sectionHeading (text: string) : IView =
    TextBlock.create [
        TextBlock.text text
        TextBlock.fontWeight FontWeight.SemiBold
        TextBlock.margin (0.0, 16.0, 0.0, 6.0)
        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
    ]

/// One before → after row. `better` says whether the change is an improvement
/// (Some true), a regression (Some false) or neither (None).
let private comparisonCells (row: int) (label: string) (before: string) (after: string) (better: bool option) : IView list =
    let cell column (text: string) (attrs: IAttr<TextBlock> list) : IView =
        TextBlock.create [
            Grid.row row
            Grid.column column
            TextBlock.text text
            TextBlock.margin (0.0, 2.0, 16.0, 2.0)
            yield! attrs
        ]

    [ cell 0 label []
      cell 1 before [ Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ] ]
      cell 2 "→" [ Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ] ]
      cell 3 after [
          TextBlock.fontWeight FontWeight.SemiBold
          match better with
          | Some true -> Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.Success ]
          | Some false -> Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.Danger ]
          | None -> ()
      ] ]

let private comparison (before: ValidationReport) (after: ValidationReport) : IView =
    let count label (b: int) (a: int) = label, string b, string a, (if a < b then Some true elif a > b then Some false else None)
    let yesNo value = if value then "Yes" else "No"
    let rows =
        [ count "Failed checks" before.FailedCount after.FailedCount
          count "Warnings" before.WarningCount after.WarningCount
          $"Compliant with {after.FlavourId.ToUpperInvariant()}",
          yesNo before.IsCompliant,
          yesNo after.IsCompliant,
          (if after.IsCompliant = before.IsCompliant then None else Some after.IsCompliant) ]

    Grid.create [
        Grid.columnDefinitions "Auto,Auto,Auto,Auto"
        Grid.rowDefinitions (String.replicate rows.Length "Auto," |> fun s -> s.TrimEnd ',')
        Grid.children [
            for index, (label, b, a, better) in List.indexed rows do
                yield! comparisonCells index label b a better
        ]
    ]

/// Shows the outcome of a Fix: a plain-language status, what changed, how the
/// repaired copy validates compared with the original, and where it was saved.
let showFixResult (owner: Window) (outcome: Result<FixResult, string>) (after: ValidationReport option) : Task =
    let window = dialogWindow "Fix PDF" 540.0
    let style = statusStyle outcome

    let savedPath =
        match outcome with
        | Ok fix when fix.WasFixed -> Option.ofObj fix.OutputPath
        | _ -> None

    let openFolder (folder: string) =
        window.Launcher.LaunchDirectoryInfoAsync(DirectoryInfo folder) |> ignore

    window.Content <-
        Component(fun _ ->
            StackPanel.create [
                StackPanel.margin 20.0
                StackPanel.children [
                    // Status headline
                    Grid.create [
                        Grid.columnDefinitions "Auto,*"
                        Grid.children [
                            Border.create [
                                Border.width 36.0
                                Border.height 36.0
                                Border.cornerRadius 18.0
                                Border.margin (0.0, 0.0, 14.0, 0.0)
                                Border.verticalAlignment VerticalAlignment.Top
                                Theme.resources [ Border.BackgroundProperty, style.ColorToken ]
                                Border.child (
                                    TextBlock.create [
                                        TextBlock.text style.Glyph
                                        TextBlock.fontSize 18.0
                                        TextBlock.fontWeight FontWeight.Bold
                                        TextBlock.foreground Brushes.White
                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                        TextBlock.verticalAlignment VerticalAlignment.Center
                                    ]
                                )
                            ]
                            StackPanel.create [
                                Grid.column 1
                                StackPanel.children [
                                    TextBlock.create [
                                        TextBlock.text style.Title
                                        TextBlock.fontSize 18.0
                                        TextBlock.fontWeight FontWeight.SemiBold
                                    ]
                                    SelectableTextBlock.create [
                                        SelectableTextBlock.text style.Detail
                                        SelectableTextBlock.textWrapping TextWrapping.Wrap
                                        SelectableTextBlock.margin (0.0, 4.0, 0.0, 0.0)
                                        Theme.resources [ SelectableTextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                    ]
                                ]
                            ]
                        ]
                    ]

                    // Before → after
                    match outcome, after with
                    | Ok fix, Some after ->
                        sectionHeading "Result (original → repaired copy)"
                        comparison fix.OriginalReport after
                    | _ -> ()

                    // Changes made
                    match outcome with
                    | Ok fix when fix.AppliedFixes.Count > 0 ->
                        sectionHeading "Changes made"
                        StackPanel.create [
                            StackPanel.spacing 4.0
                            StackPanel.children [
                                for change in fix.AppliedFixes ->
                                    Grid.create [
                                        Grid.columnDefinitions "Auto,*"
                                        Grid.children [
                                            TextBlock.create [
                                                TextBlock.text "✓"
                                                TextBlock.margin (0.0, 0.0, 8.0, 0.0)
                                                Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.Success ]
                                            ]
                                            TextBlock.create [
                                                Grid.column 1
                                                TextBlock.text change
                                                TextBlock.textWrapping TextWrapping.Wrap
                                            ]
                                        ]
                                    ]
                            ]
                        ]
                    | _ -> ()

                    // Saved as
                    match savedPath with
                    | Some path ->
                        let folder = Path.GetDirectoryName path |> Option.ofObj |> Option.defaultValue ""
                        sectionHeading "Saved as"
                        Grid.create [
                            Grid.columnDefinitions "*,Auto"
                            Grid.children [
                                StackPanel.create [
                                    StackPanel.children [
                                        SelectableTextBlock.create [
                                            SelectableTextBlock.text (match Path.GetFileName path with NonNull name -> name | Null -> path)
                                            SelectableTextBlock.fontWeight FontWeight.SemiBold
                                        ]
                                        SelectableTextBlock.create [
                                            SelectableTextBlock.text folder
                                            SelectableTextBlock.textWrapping TextWrapping.Wrap
                                            Theme.resources [ SelectableTextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                        ]
                                        TextBlock.create [
                                            TextBlock.text "Legibyl is now showing this copy."
                                            TextBlock.margin (0.0, 4.0, 0.0, 0.0)
                                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                        ]
                                    ]
                                ]
                                Button.create [
                                    Grid.column 1
                                    Button.content "Open folder"
                                    Button.margin (12.0, 0.0, 0.0, 0.0)
                                    Button.verticalAlignment VerticalAlignment.Top
                                    Button.isEnabled (folder <> "")
                                    Button.onClick (fun _ -> openFolder folder)
                                ]
                            ]
                        ]
                    | None -> ()

                    // Footnote and OK
                    Grid.create [
                        Grid.margin (0.0, 20.0, 0.0, 0.0)
                        Grid.columnDefinitions "*,Auto"
                        Grid.children [
                            match outcome with
                            | Ok _ ->
                                TextBlock.create [
                                    TextBlock.text
                                        "Fixes are best-effort and limited to the repairs you selected. They don't add missing alt text, retag untagged content, or infer table headers."
                                    TextBlock.fontSize 12.0
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    TextBlock.margin (0.0, 0.0, 16.0, 0.0)
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                    Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                ]
                            | Error _ -> ()
                            Button.create [
                                Grid.column 1
                                Button.content "OK"
                                Button.minWidth 82.0
                                Button.isDefault true
                                Button.isCancel true
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.verticalAlignment VerticalAlignment.Bottom
                                Button.onClick (fun _ -> window.Close())
                            ]
                        ]
                    ]
                ]
            ])

    window.ShowDialog owner
