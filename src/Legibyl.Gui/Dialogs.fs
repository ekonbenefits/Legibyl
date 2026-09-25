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
                      emptyParagraphs.Current, PdfFixOptions.RemoveEmptyParagraphs ]
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
