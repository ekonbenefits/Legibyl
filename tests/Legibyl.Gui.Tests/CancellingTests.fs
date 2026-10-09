/// What runs long can be stopped from the progress row (see #329).
module Legibyl.Gui.Tests.CancellingTests

open System
open System.IO
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private texts (root: Avalonia.Visual) =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? TextBlock as t -> Some(string t.Text) | _ -> None) |> List.ofSeq

let private cancelButton (root: Avalonia.Visual) =
    root.GetVisualDescendants()
    |> Seq.tryPick (fun v ->
        match box v with
        | :? Button as button when string button.Tag = "cancel-work" -> Some button
        | _ -> None)

/// A tagged PDF long enough that validating it takes a while.
let private longPdf () =
    let pdf = Path.Combine(Path.GetTempPath(), $"legibyl-long-{Guid.NewGuid():N}.pdf")
    use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfWriter(pdf))
    document.SetTagged() |> ignore
    use layout = new iText.Layout.Document(document)
    for page in 1..150 do
        for line in 1..30 do
            layout.Add(iText.Layout.Element.Paragraph $"Page {page}, line {line}: text for veraPDF to check.") |> ignore
        if page < 150 then layout.Add(iText.Layout.Element.AreaBreak()) |> ignore
    pdf

[<Fact>]
let ``validating can be cancelled from the progress row, whose Cancel has the focus while it runs`` () =
    let pdf = longPdf ()
    try
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            try
                // (Not waiting for it to finish, as HeadlessWindow.openPdf does.)
                window.OpenFile pdf
                HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 20.0) (fun () -> (cancelButton window).IsSome) |> ignore
                let cancel = (cancelButton window).Value
                // The keyboard's on it, so Enter or Space stops it.
                HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 5.0) (fun () -> cancel.IsFocused) |> ignore
                cancel.IsFocused |> should be True

                cancel.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
                HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 20.0) (fun () -> texts window |> List.exists (fun t -> t.StartsWith "Cancelled")) |> ignore

                texts window |> should contain "Cancelled: it hasn't been checked. Check Again checks it."
                // No result, and nothing left running.
                texts window |> List.exists (fun t -> t.StartsWith "PASS" || t.StartsWith "FAIL") |> should be False
                cancelButton window |> should equal None

                // Started from Check Again, which has the focus: given back once it's stopped.
                let checkAgain =
                    window.GetVisualDescendants() |> Seq.pick (fun v -> match box v with :? Button as b when string b.Content = "Check Again" -> Some b | _ -> None)
                HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 5.0) (fun () -> checkAgain.IsEffectivelyEnabled) |> ignore
                checkAgain.Focus() |> ignore
                checkAgain.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
                HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 20.0) (fun () -> (cancelButton window).IsSome && (cancelButton window).Value.IsFocused) |> ignore
                (cancelButton window).Value.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
                HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 20.0) (fun () -> (cancelButton window).IsNone && checkAgain.IsFocused) |> ignore
                checkAgain.IsFocused |> should be True
            finally
                window.Close())
    finally
        File.Delete pdf
