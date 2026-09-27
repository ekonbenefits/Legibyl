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
    HeadlessWindow.expandGroups window
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

/// Closes window, not saving its review if it asks.
let private close (window: Window) =
    window.Close()
    pump ()
    for asked in List.ofSeq window.OwnedWindows do
        if asked.Title = "Save Your Review?" then clickButton asked "Don't Save"

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
            close window
            restored |> List.exists _.Dismissed |> should be False
            File.Exists(Dismissals.filePath pdf) |> should be False))

[<Fact>]
let ``dismissals are kept next to the PDF once saved, and reloaded`` () =
    withSampleCopy "libreoffice-empty-paragraph.pdf" (fun pdf ->
        onUiThread (fun () ->
            let window = openPdf pdf
            let before = rows window
            dismissFirstWarning window
            let editedTitle = window.Title
            let unsaved = File.Exists(Dismissals.filePath pdf)
            let canSave = Reviews.chooseFromFileMenu window "Save"
            pump ()
            let savedTitle = window.Title
            close window

            let reopened = openPdf pdf
            let reloaded = rows reopened
            close reopened

            editedTitle |> should equal "libreoffice-empty-paragraph.pdf — Edited — Legibyl"
            canSave |> should be True
            unsaved |> should be False
            savedTitle |> should equal "libreoffice-empty-paragraph.pdf — Legibyl"
            reloaded |> should haveLength (before.Length - 1)))

[<Fact>]
let ``closing with a dismissal not saved asks, and Don't Save drops it`` () =
    withSampleCopy "libreoffice-empty-paragraph.pdf" (fun pdf ->
        onUiThread (fun () ->
            let window = openPdf pdf
            let before = rows window
            dismissFirstWarning window
            window.Close()
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 5.0) (fun () -> window.OwnedWindows.Count = 1) |> ignore
            let asked = window.OwnedWindows |> Seq.exactlyOne
            let question = asked.Title
            clickButton asked "Don't Save"
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 5.0) (fun () -> not window.IsVisible) |> ignore
            let closed = not window.IsVisible

            let reopened = openPdf pdf
            let reloaded = rows reopened
            reopened.Close()

            question |> should equal "Save Your Review?"
            closed |> should be True
            File.Exists(Dismissals.filePath pdf) |> should be False
            reloaded |> should haveLength before.Length))

[<Fact>]
let ``a warning dismissed in the review inside a saved copy stays dismissed, until restored and saved there`` () =
    withSampleCopy "libreoffice-empty-paragraph.pdf" (fun pdf ->
        onUiThread (fun () ->
            let window = openPdf pdf
            dismissFirstWarning window
            Reviews.chooseFromFileMenu window "Save" |> ignore
            pump ()
            close window
            // A copy carrying that review, as Export writes one.
            let copy = Path.ChangeExtension(pdf, ".v2.pdf") |> nonNull
            Legibyl.Validation.VeraPdfValidator(Review = (Reviews.read pdf).ForCopy pdf)
                .Fix(pdf, Legibyl.Validation.PdfProfile.PdfUa1, copy, Legibyl.Validation.PdfFixOptions.Metadata)
            |> ignore

            let opened = openPdf copy
            let dismissedThere = texts opened |> List.exists (fun t -> t.Contains "1 dismissed")
            clickButton opened "Show 1 dismissed"
            rightClickRow opened (rows opened |> List.findIndex _.Dismissed)
            click (menuItem opened "Restore warning")
            Reviews.chooseFromFileMenu opened "Save" |> ignore
            pump ()
            close opened

            let reopened = openPdf copy
            let dismissedAfter = rows reopened |> List.exists _.Dismissed || texts reopened |> List.exists (fun t -> t.Contains "dismissed")
            close reopened

            dismissedThere |> should be True
            // The file next to the copy says none, over the review inside it.
            File.ReadAllText(Dismissals.filePath copy) |> should haveSubstring "\"dismissedWarnings\": []"
            dismissedAfter |> should be False))

[<Fact>]
let ``ignoring a type of warning hides all of them until it's stopped`` () =
    // Every warning of the rule, wherever it is, is covered by DismissalsTests; this is the menu.
    withSampleCopy "libreoffice-empty-paragraph.pdf" (fun pdf ->
        onUiThread (fun () ->
            let window = openPdf pdf
            let before = rows window
            let warning = before |> List.find (fun r -> r.Severity = "Warning")

            rightClickRow window (before |> List.findIndex (fun r -> obj.ReferenceEquals(r, warning)))
            click (menuItem window $"Ignore all {warning.Violation.RuleId} warnings")
            let ignored = rows window

            clickButton window "Show 1 dismissed"
            rightClickRow window (rows window |> List.findIndex _.Dismissed)
            // Nothing to restore on its own: it's hidden with the rest of its type.
            let headers =
                (HeadlessWindow.resultsGrid window).ContextMenu |> nonNull |> _.Items |> Seq.cast<obj> |> Seq.choose (function :? MenuItem as m -> Some(string m.Header) | _ -> None) |> List.ofSeq
            click (menuItem window "Stop ignoring")
            let restored = rows window
            close window
            ignored |> should haveLength (before.Length - 1)
            headers |> should contain $"Stop ignoring {warning.Violation.RuleId} warnings"
            headers |> should not' (contain "Restore warning")
            restored |> should haveLength before.Length
            restored |> List.exists _.Dismissed |> should be False))

[<Fact>]
let ``failures can't be dismissed`` () =
    withSampleCopy "pages-clipped-paragraphs.pdf" (fun pdf ->
        onUiThread (fun () ->
            let window = openPdf pdf
            rightClickRow window (rows window |> List.findIndex (fun r -> r.Severity = "Failure"))
            let dismiss = menuItem window "Dismiss warning"
            close window
            dismiss.IsEnabled |> should be False))

[<Fact>]
let ``right-clicking a row selects it, so its menu acts on it`` () =
    withSampleCopy "pages-clipped-paragraphs.pdf" (fun pdf ->
        onUiThread (fun () ->
            let window = openPdf pdf
            let grid = HeadlessWindow.resultsGrid window
            rightClickRow window 1
            let selectedAfter = grid.SelectedIndex
            close window
            selectedAfter |> should equal 1))
