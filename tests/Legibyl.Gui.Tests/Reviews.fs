/// Reviews saved and read as the app does (see DocumentReview), to set up and check tests.
module Legibyl.Gui.Tests.Reviews

open Avalonia.Controls
open Legibyl.Gui

/// pdf's review as it's read when the PDF is opened: from the file next to it (and any carried
/// inside it).
let read (pdf: string) = DocumentReviews().For pdf

/// Saves pdf's review with change made to it, as Save does.
let save (pdf: string) (change: DocumentReview -> unit) =
    let review = read pdf
    change review
    review.Save pdf

/// Saves edits as pdf's review's edits.
let saveEdits (pdf: string) (edits: RegionEdits.Edit list) = save pdf (fun review -> review.Edits <- edits)

/// Chooses the window's File › header menu item (as the menu bar, or the menu at the top of the
/// window, does).
let chooseFromFileMenu (window: Window) (header: string) =
    let file = NativeMenu.GetMenu window |> nonNull |> _.Items |> Seq.cast<NativeMenuItem> |> Seq.head
    let item =
        (nonNull file.Menu).Items
        |> Seq.pick (function
            | :? NativeMenuItem as item when string item.Header = header -> Some item
            | _ -> None)
    // Whether it could be chosen: as it was, not once it's done.
    let enabled = item.IsEnabled
    if enabled then
        (nonNull item.Command).Execute null
        // Saving is done in the background: until the window no longer says it's edited.
        if header = "Save" then
            Legibyl.Screenshots.HeadlessWindow.pumpUntil (System.TimeSpan.FromSeconds 10.0) (fun () -> not (window.Title.Contains "Edited")) |> ignore
    enabled
