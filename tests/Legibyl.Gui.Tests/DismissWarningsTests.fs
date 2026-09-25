module Legibyl.Gui.Tests.DismissWarningsTests

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

/// A copy of a sample PDF in its own temporary folder, so dismissals saved next to it don't
/// touch the corpus.
let private withSampleCopy name (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, name)
    File.Copy(Path.Combine(AppContext.BaseDirectory, "SampleCorpus", name), pdf)
    try test pdf finally Directory.Delete(folder, true)

let private rows (window: Window) =
    (HeadlessWindow.resultsGrid window).ItemsSource |> Seq.cast<RuleViolationRow> |> List.ofSeq

let private texts (window: Window) = descendants<TextBlock> window |> List.map (fun t -> string t.Text)

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

/// Right-clicks the results row at index, as a user would to open its menu.
let private rightClickRow (window: Window) index =
    let row = descendants<DataGridRow> (HeadlessWindow.resultsGrid window) |> List.find (fun r -> r.Index = index)
    let point = (row.TranslatePoint(Point(20.0, row.Bounds.Height / 2.0), window)).Value
    window.MouseDown(point, MouseButton.Right)
    window.MouseUp(point, MouseButton.Right)
    pump ()

let private menuItem (window: Window) (header: string) =
    (HeadlessWindow.resultsGrid window).ContextMenu
    |> nonNull
    |> _.Items
    |> Seq.cast<obj>
    |> Seq.pick (function
        | :? MenuItem as item when (string item.Header).StartsWith header -> Some item
        | _ -> None)

let private click (item: MenuItem) =
    item.RaiseEvent(RoutedEventArgs(MenuItem.ClickEvent))
    pump ()

let private clickButton (window: Window) (text: string) =
    let b = descendants<Button> window |> List.find (fun b -> string b.Content = text)
    b.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
    pump ()

let private openPdf (pdf: string) =
    let window = HeadlessWindow.show 1200.0 720.0
    HeadlessWindow.openPdf window pdf
    window

let private dismissFirstWarning (window: Window) =
    rightClickRow window (rows window |> List.findIndex (fun r -> r.Severity = "Warning"))
    click (menuItem window "Dismiss warning")

[<Fact>]
let ``a dismissed warning is hidden, counted, and can be shown and restored`` () =
    withSampleCopy "libreoffice-empty-paragraph.pdf" (fun pdf ->
        onUiThread (fun () ->
            let window = openPdf pdf
            let before = rows window

            dismissFirstWarning window

            rows window |> should haveLength (before.Length - 1)
            rows window |> List.exists _.Dismissed |> should be False
            texts window |> List.exists (fun t -> t.Contains "1 dismissed") |> should be True

            clickButton window "Show 1 dismissed"
            let shown = rows window
            shown |> should haveLength before.Length
            shown |> List.filter _.Dismissed |> should haveLength 1
            texts window |> List.exists (fun t -> t.EndsWith "(dismissed)") |> should be True
            descendants<DataGridRow> (HeadlessWindow.resultsGrid window)
            |> List.filter (fun row -> match row.DataContext with :? RuleViolationRow as r -> r.Dismissed | _ -> false)
            |> List.map _.Opacity
            |> should equal [ 0.5 ]

            rightClickRow window (shown |> List.findIndex _.Dismissed)
            click (menuItem window "Restore warning")
            let restored = rows window
            window.Close()
            restored |> List.exists _.Dismissed |> should be False
            File.Exists(Dismissals.filePath pdf) |> should be False))

[<Fact>]
let ``remembered dismissals are saved next to the PDF and reloaded`` () =
    withSampleCopy "libreoffice-empty-paragraph.pdf" (fun pdf ->
        onUiThread (fun () ->
            let window = openPdf pdf
            let before = rows window
            dismissFirstWarning window
            click (menuItem window "Remember dismissals")
            window.Close()
            File.Exists(Dismissals.filePath pdf) |> should be True

            let reopened = openPdf pdf
            let reloaded = rows reopened
            let remembering = (menuItem reopened "Remember dismissals").IsChecked
            reopened.Close()
            reloaded |> should haveLength (before.Length - 1)
            remembering |> should be True))

[<Fact>]
let ``failures can't be dismissed`` () =
    withSampleCopy "pages-clipped-paragraphs.pdf" (fun pdf ->
        onUiThread (fun () ->
            let window = openPdf pdf
            rightClickRow window (rows window |> List.findIndex (fun r -> r.Severity = "Failure"))
            let dismiss = menuItem window "Dismiss warning"
            window.Close()
            dismiss.IsEnabled |> should be False))

[<Fact>]
let ``right-clicking a row selects it, so its menu acts on it`` () =
    withSampleCopy "pages-clipped-paragraphs.pdf" (fun pdf ->
        onUiThread (fun () ->
            let window = openPdf pdf
            let grid = HeadlessWindow.resultsGrid window
            let selectedBefore = grid.SelectedIndex
            rightClickRow window 1
            let selectedAfter = grid.SelectedIndex
            window.Close()
            selectedBefore |> should equal 0
            selectedAfter |> should equal 1))
