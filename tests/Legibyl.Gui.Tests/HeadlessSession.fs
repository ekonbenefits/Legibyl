/// One headless Avalonia session (and UI thread) for the whole test run: Avalonia allows
/// only one application per process, so every test module shares it.
module Legibyl.Gui.Tests.HeadlessSession

open System
open System.Threading
open Legibyl.Screenshots

let private session =
    lazy (Avalonia.Headless.HeadlessUnitTestSession.StartNew(typeof<HeadlessWindow.HeadlessApp>))

let onUiThread (test: unit -> unit) =
    session.Value.Dispatch(Action test, CancellationToken.None).GetAwaiter().GetResult()

/// A block's right-click menu as it opens (asked for, as a right-click or the menu key does: it's
/// filled then, for the block): its items (with separators left out), the menu closed again.
/// Opening it selects the block.
let blockMenu (block: Avalonia.Controls.Border) : Avalonia.Controls.MenuItem list =
    match block.ContextMenu with
    | null -> []
    | menu ->
        block.RaiseEvent(Avalonia.Input.ContextRequestedEventArgs())
        let items = menu.Items |> Seq.choose (function :? Avalonia.Controls.MenuItem as item -> Some item | _ -> None) |> List.ofSeq
        menu.Close()
        items
