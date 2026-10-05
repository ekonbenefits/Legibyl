/// Structure…'s Attachments tab: the files attached to the PDF and its media clips, which aren't in
/// the page's tags, so have no block to edit: each with what PDF/UA asks of it (a file's name and
/// description, a media clip's content type and description), what it has, and edits that give
/// it, which Export… writes (see Attachments).
module internal Legibyl.Gui.StructureAttachmentsTab

open System
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open Legibyl.Validation
open Legibyl.Validation.Attachments

/// What the tab needs: the attachments found, the review its edits go in, the window its prompts
/// belong to, and what to call when one's made.
type Needs =
    { Attachments: Attachment list
      Review: DocumentReview
      Window: Window
      OnChanged: unit -> unit }

let private none: AttachmentEdit = { FileName = null; Description = null; ContentType = null }

/// What's said of an attachment: what it has, and what's missing.
let private said (attachment: Attachment) (edit: AttachmentEdit) =
    let yours (value: string | null) = match value with NonNull value when value <> "" -> Some value | _ -> None
    [ match attachment.Kind with
      | File ->
          match yours edit.FileName, attachment.FileName with
          | Some name, _ -> $"File name: \"{name}\" (your edit, not in the PDF yet)"
          | None, Some name when attachment.Named -> $"File name: \"{name}\""
          | None, Some name -> $"File name: \"{name}\", not written as PDF/UA asks (as both /F and /UF)"
          | None, None -> "No file name"
      | Media ->
          match yours edit.ContentType, attachment.ContentType with
          | Some contentType, _ -> $"Content type: {contentType} (your edit, not in the PDF yet)"
          | None, Some contentType -> $"Content type: {contentType}"
          | None, None -> "No content type (what kind of media it is)"
      match yours edit.Description, attachment.Description with
      | Some description, _ -> $"Description: \"{description}\" (your edit, not in the PDF yet)"
      | None, Some description -> $"Description: \"{description}\""
      | None, None -> "No description: a screen reader can only say its file name" ]

let view (needs: Needs) : IView =
    Component.create (
        "attachments",
        fun ctx ->
            let edits = ctx.useState needs.Review.Attachments
            let save (key: string) (change: AttachmentEdit -> AttachmentEdit) =
                let next =
                    let edited = change (edits.Current.TryFind key |> Option.defaultValue none)
                    if edited = none then edits.Current.Remove key else edits.Current.Add(key, edited)
                if next <> edits.Current then
                    needs.Review.Attachments <- next
                    edits.Set next
                    needs.OnChanged()
            // Asks for a value, prefilled, and keeps it (empty drops it).
            let ask (title: string) (prompt: string) (current: string) (placeholder: string) (set: string | null -> AttachmentEdit -> AttachmentEdit) (key: string) =
                task {
                    match! Dialogs.promptText needs.Window title prompt current placeholder with
                    | Some text -> save key (set (if String.IsNullOrWhiteSpace text then null else text.Trim()))
                    | None -> ()
                }
                |> ignore
            ScrollViewer.create [
                ScrollViewer.content (
                    StackPanel.create [
                        StackPanel.margin 8.0
                        StackPanel.children [
                            yield
                                TextBlock.create [
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    TextBlock.margin (0.0, 0.0, 0.0, 10.0)
                                    Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                    TextBlock.text
                                        "The files attached to the PDF, and its media (a video or a sound), which aren't in the page's tags. PDF/UA asks for a file's name and a description of it, and a media clip's content type and a description; Export… writes what you give them."
                                ]
                            for attachment in needs.Attachments ->
                                let edit = edits.Current.TryFind attachment.Key |> Option.defaultValue none
                                let kind = match attachment.Kind with File -> "Attached file" | Media -> "Media"
                                let named (value: string | null) (fallback: string option) =
                                    match value with
                                    | NonNull value when value <> "" -> value
                                    | _ -> fallback |> Option.defaultValue ""
                                StackPanel.create [
                                    StackPanel.margin (0.0, 0.0, 0.0, 12.0)
                                    StackPanel.children [
                                        yield
                                            TextBlock.create [
                                                TextBlock.text $"{attachment.Key}  ({kind}, {attachment.Where})"
                                                TextBlock.fontWeight FontWeight.SemiBold
                                            ]
                                        for line in said attachment edit ->
                                            TextBlock.create [
                                                TextBlock.text line
                                                TextBlock.textWrapping TextWrapping.Wrap
                                                Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                            ]
                                        yield
                                            WrapPanel.create [
                                                WrapPanel.margin (0.0, 4.0, 0.0, 0.0)
                                                WrapPanel.children [
                                                    match attachment.Kind with
                                                    | File ->
                                                        StructureActions.editButton "Edit File Name…" [
                                                            Button.margin (0.0, 0.0, 6.0, 0.0)
                                                            ToolTip.tip "The file's name, which a screen reader says for it (written as its /F and /UF)."
                                                            Button.onClick (
                                                                (fun _ ->
                                                                    ask
                                                                        "File Name"
                                                                        $"The name of the file attached as \"{attachment.Key}\", e.g. \"budget-2026.xlsx\"."
                                                                        (named edit.FileName (attachment.FileName |> Option.orElse (Some attachment.Key)))
                                                                        "The file's name"
                                                                        (fun value e -> { e with FileName = value })
                                                                        attachment.Key),
                                                                SubPatchOptions.Always
                                                            )
                                                        ]
                                                    | Media ->
                                                        StructureActions.editButton "Edit Content Type…" [
                                                            Button.margin (0.0, 0.0, 6.0, 0.0)
                                                            ToolTip.tip "What kind of media it is, as a MIME type, e.g. video/mp4 (written as its /CT)."
                                                            Button.onClick (
                                                                (fun _ ->
                                                                    let guess = attachment.FileName |> Option.bind contentTypeFor
                                                                    ask
                                                                        "Content Type"
                                                                        $"What kind of media \"{attachment.Key}\" is, as a MIME type: video/mp4, audio/mpeg…"
                                                                        (named edit.ContentType (attachment.ContentType |> Option.orElse guess))
                                                                        "e.g. video/mp4"
                                                                        (fun value e -> { e with ContentType = value })
                                                                        attachment.Key),
                                                                SubPatchOptions.Always
                                                            )
                                                        ]
                                                    StructureActions.editButton "Edit Description…" [
                                                        ToolTip.tip "What it is, which a screen reader says for it (an attached file's /Desc, a media clip's /Alt)."
                                                        Button.onClick (
                                                            (fun _ ->
                                                                ask
                                                                    "Description"
                                                                    $"What \"{attachment.Key}\" is, for someone who can't open it, e.g. \"The budget, as a spreadsheet\"."
                                                                    (named edit.Description attachment.Description)
                                                                    "What it is"
                                                                    (fun value e -> { e with Description = value })
                                                                    attachment.Key),
                                                            SubPatchOptions.Always
                                                        )
                                                    ]
                                                ]
                                            ]
                                    ]
                                ]
                        ]
                    ]
                )
            ]
    )
