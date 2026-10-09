/// What went wrong is said, not left as an empty or stuck window (see #322).
module Legibyl.Gui.Tests.FailureReportingTests

open System
open System.IO
open Avalonia.Controls
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private texts (root: Avalonia.Visual) =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? TextBlock as t -> Some(string t.Text) | _ -> None) |> List.ofSeq

[<Fact>]
let ``a failure inside a task is said by its own message`` () =
    let inner = IOException "The file is locked."
    Dialogs.failureMessage (AggregateException(inner)) |> should equal "The file is locked."
    Dialogs.failureMessage (AggregateException(AggregateException(inner))) |> should equal "The file is locked."
    Dialogs.failureMessage inner |> should equal "The file is locked."

[<Fact>]
let ``a PDF gone by the time it's checked says why, rather than leaving the window empty`` () =
    onUiThread (fun () ->
        let gone = Path.Combine(Path.GetTempPath(), $"legibyl-gone-{Guid.NewGuid():N}.pdf")
        let window = HeadlessWindow.show 1200.0 720.0
        try
            HeadlessWindow.openPdf window gone
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) (fun () -> texts window |> List.exists (fun t -> t.Contains "Legibyl couldn't check it")) |> ignore
            texts window |> List.exists (fun t -> t.StartsWith "Error: Legibyl couldn't check it:") |> should be True
        finally
            window.Close())
