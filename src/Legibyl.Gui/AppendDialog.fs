/// File › Append PDFs…: choosing the PDFs to join into one (see Append), in order, and what to
/// call the joined file; and saying how the join went.
module Legibyl.Gui.AppendDialog

open System
open System.IO
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Platform.Storage
open Legibyl.Validation

/// What's chosen to join: the files, in order, and the joined file's title (None for the first
/// file's).
type Choice = { Files: string list; Title: string option }

/// The files moved: the one at index by offset (-1 up, 1 down), if it can be.
let move (files: string list) (index: int) (offset: int) =
    let target = index + offset
    if index < 0 || index >= files.Length || target < 0 || target >= files.Length then
        files
    else
        files |> List.mapi (fun i file -> if i = index then files[target] elif i = target then files[index] else file)

/// Asks for the PDFs to join, starting with first (the one open, if any): Some choice on Join,
/// None on Cancel.
let choose (owner: Window) (first: string option) : Task<Choice option> =
    let window = Dialogs.dialogWindow "Append PDFs" 560.0
    let mutable chosen = None
    let pdfs = FilePickerFileType("PDF documents", Patterns = [ "*.pdf" ])
    window.Content <-
        Component(fun ctx ->
            let files = ctx.useState (Option.toList first)
            let selected = ctx.useState (if first.IsSome then 0 else -1)
            let title = ctx.useState ""
            let add _ =
                task {
                    let! picked =
                        window.StorageProvider.OpenFilePickerAsync(
                            FilePickerOpenOptions(Title = "Choose PDFs to append", AllowMultiple = true, FileTypeFilter = [ pdfs; FilePickerFileTypes.All ])
                        )
                    let paths = picked |> Seq.choose (fun file -> Option.ofObj (file.TryGetLocalPath())) |> List.ofSeq
                    if not paths.IsEmpty then
                        files.Set(files.Current @ paths)
                        selected.Set(files.Current.Length - 1)
                }
                |> ignore
            let moveSelected offset _ =
                let index = selected.Current
                files.Set(move files.Current index offset)
                selected.Set(max 0 (min (files.Current.Length - 1) (index + offset)))
            let remove _ =
                let index = selected.Current
                if index >= 0 && index < files.Current.Length then
                    files.Set(files.Current |> List.removeAt index)
                    selected.Set(min index (files.Current.Length - 1))
            let button (label: string) (enabled: bool) (onClick: Interactivity.RoutedEventArgs -> unit) : IView =
                Button.create [
                    Button.content label
                    Button.minWidth 82.0
                    Button.isEnabled enabled
                    Button.horizontalContentAlignment HorizontalAlignment.Center
                    Button.onClick onClick
                ]
            let index = selected.Current
            StackPanel.create [
                StackPanel.margin 16.0
                StackPanel.spacing 12.0
                StackPanel.children [
                    TextBlock.create [
                        TextBlock.text
                            "Join PDFs that pass PDF/UA into one, in this order, keeping each one's tags as they are: each becomes a part of the joined file. Each is validated first; if any fails, or can't be joined (inconsistent tags, a form, layers, attached files), nothing is written. No fixes or reading-order guesses are made. The joined file is then checked, and opened."
                        TextBlock.textWrapping TextWrapping.Wrap
                    ]
                    DockPanel.create [
                        DockPanel.children [
                            StackPanel.create [
                                DockPanel.dock Dock.Right
                                StackPanel.margin (Thickness(8.0, 0.0, 0.0, 0.0))
                                StackPanel.spacing 6.0
                                StackPanel.children [
                                    button "Add…" true add
                                    button "Remove" (index >= 0 && index < files.Current.Length) remove
                                    button "Move Up" (index > 0) (moveSelected -1)
                                    button "Move Down" (index >= 0 && index < files.Current.Length - 1) (moveSelected 1)
                                ]
                            ]
                            ListBox.create [
                                AttrBuilder<ListBox>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, "PDFs to join, in order", ValueNone)
                                ListBox.height 180.0
                                ListBox.dataItems (files.Current |> List.mapi (fun i file -> $"{i + 1}. {Path.GetFileName file}"))
                                ListBox.selectedIndex index
                                ListBox.onSelectedIndexChanged (fun i -> if i >= 0 then selected.Set i)
                            ]
                        ]
                    ]
                    TextBlock.create [ TextBlock.text "Title of the joined file" ]
                    TextBox.create [
                        AttrBuilder<TextBox>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, "Title of the joined file", ValueNone)
                        TextBox.text title.Current
                        TextBox.placeHolderText "The first file's title"
                        TextBox.onTextChanged title.Set
                    ]
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.horizontalAlignment HorizontalAlignment.Right
                        StackPanel.spacing 8.0
                        StackPanel.children [
                            Button.create [
                                Button.content "Join…"
                                Button.minWidth 82.0
                                Button.isDefault true
                                Button.isEnabled (files.Current.Length >= 2)
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.onClick (fun _ ->
                                    chosen <-
                                        Some
                                            { Files = files.Current
                                              Title = if String.IsNullOrWhiteSpace title.Current then None else Some(title.Current.Trim()) }
                                    window.Close())
                            ]
                            Button.create [
                                Button.content "Cancel"
                                Button.minWidth 82.0
                                Button.isCancel true
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.onClick (fun _ -> window.Close())
                            ]
                        ]
                    ]
                ]
            ])
    task {
        do! window.ShowDialog owner
        return chosen
    }

/// How a join went, to say once it's done: why nothing was written, or what was, how it checks
/// out (beside the window's own report on it), and what was renamed or left out.
let summary (outcome: Append.Outcome) : string =
    let lines (heading: string) (items: string list) =
        if items.IsEmpty then [] else "" :: heading :: [ for item in items -> $"• {item}" ]
    match outcome.Joined with
    | None ->
        let reasons =
            [ for refusal in outcome.Refusals ->
                  match refusal.File with
                  | NonNull file -> $"{Path.GetFileName file}: {refusal.Reason}"
                  | Null -> refusal.Reason ]
        String.Join(Environment.NewLine, "Nothing was written." :: lines "Why:" reasons)
    | Some joined ->
        let standard = PdfUaTarget.displayName joined.Profile
        let passes = outcome.After |> Option.exists _.IsCompliant
        let headline =
            match passes, outcome.Differences with
            | true, [] -> $"Joined {joined.Parts.Length} files into {Path.GetFileName joined.Output}. It passes {standard}, and each part reads as its file did."
            | false, _ -> $"Joined {joined.Parts.Length} files into {Path.GetFileName joined.Output}, but it doesn't pass {standard}: its report says why."
            | true, _ -> $"Joined {joined.Parts.Length} files into {Path.GetFileName joined.Output}. It passes {standard}, but some of it doesn't read as its file did."
        String.Join(
            Environment.NewLine,
            headline :: lines "Read differently:" outcome.Differences @ lines "Changed to join them:" joined.Notes
        )
