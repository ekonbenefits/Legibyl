module Legibyl.Gui.Tests.DocumentWindowsTests

open System
open System.IO
open Avalonia.Controls
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

/// Copies of two sample PDFs in a fresh folder.
let private withTwoPdfs (test: string -> string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let copy name =
        let path = Path.Combine(folder, name)
        File.Copy(Path.Combine(AppContext.BaseDirectory, "SampleCorpus", name), path)
        path
    try test (copy "pdfua1-pass.pdf") (copy "libreoffice-empty-paragraph.pdf") finally Directory.Delete(folder, true)

/// Waits for task, pumping the UI.
let private wait (task: Threading.Tasks.Task<'T>) =
    HeadlessWindow.pumpUntil (TimeSpan.FromMinutes 1.0) (fun () -> task.IsCompleted) |> ignore
    task.Result

let private fileMenu (window: Window) =
    let menu = NativeMenu.GetMenu window |> nonNull
    let file = menu.Items |> Seq.cast<NativeMenuItem> |> Seq.exactlyOne
    [ for item in (nonNull file.Menu).Items do
          match item with
          | :? NativeMenuItemSeparator -> ()
          | :? NativeMenuItem as item -> item.Header |> string, item
          | _ -> () ]

[<Fact>]
let ``another PDF opens in its own window, and one already open is brought forward`` () =
    withTwoPdfs (fun first second ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window first

            let other, report = wait (MainWindow.Open(DocumentShell.Shared, second, window))
            let again, reportAgain = wait (MainWindow.Open(DocumentShell.Shared, first, other))
            let shown = [ window.FilePath; other.FilePath ]
            let open' = MainWindow.Windows |> List.filter (fun w -> obj.ReferenceEquals(w, window) || obj.ReferenceEquals(w, other))
            other.Close()
            let afterClosingOne = window.IsVisible
            window.Close()

            obj.ReferenceEquals(other, window) |> should be False
            shown |> should equal [ Some first; Some second ]
            report.IsSome |> should be True
            obj.ReferenceEquals(again, window) |> should be True
            reportAgain |> should equal None
            open' |> should haveLength 2
            afterClosingOne |> should be True))

[<Fact>]
let ``the File menu opens, closes, saves and exports, in step with the buttons`` () =
    withTwoPdfs (fun first _ ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window first
            let items = fileMenu window
            let enabled name = (items |> List.find (fst >> (=) name) |> snd).IsEnabled
            let saveAtFirst, exportAtFirst = enabled "Save", enabled "Export…"
            window.Close()

            items |> List.map fst |> should equal [ "Open…"; "Open Recent"; "Close Window"; "Save"; "Export…" ]
            // Nothing to save or export until there's a change or a fix chosen.
            saveAtFirst |> should be False
            exportAtFirst |> should be False))

[<Fact>]
let ``a PDF asked for before the window is shown opens once it is, in an empty window first`` () =
    withTwoPdfs (fun first second ->
        onUiThread (fun () ->
            // As Finder can ask, launching the app: before its window is up.
            let window = MainWindow(Width = 1200.0, Height = 720.0)
            let opening = window.OpenFileAsync first
            window.Show()
            let report = wait opening
            // An empty window is used before a new one is made.
            let empty = HeadlessWindow.show 1200.0 720.0
            let shownIn, _ = wait (MainWindow.Open(DocumentShell.Shared, second, window))
            let reused = obj.ReferenceEquals(shownIn, empty)
            window.Close()
            empty.Close()

            report.IsSome |> should be True
            window.FilePath |> should equal (Some first)
            reused |> should be True))

[<Fact>]
let ``opened PDFs are listed under Open Recent, newest first`` () =
    withTwoPdfs (fun first second ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window first
            let other, _ = wait (MainWindow.Open(DocumentShell.Shared, second, window))
            HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
            let recent = (fileMenu window |> List.find (fst >> (=) "Open Recent") |> snd).Menu |> nonNull
            let entries =
                [ for item in recent.Items do
                      match item with
                      | :? NativeMenuItemSeparator -> ()
                      | :? NativeMenuItem as item -> string item.Header, string item.ToolTip
                      | _ -> () ]
            other.Close()
            window.Close()

            // Each entry's full path is its tooltip. Other tests (sharing the UI thread) open
            // PDFs too, so only the order of these two is certain.
            let at (pdf: string) = entries |> List.findIndex (fun (_, path) -> path = Path.GetFullPath pdf)
            at second |> should be (lessThan (at first))
            entries |> List.last |> fst |> should equal "Clear Menu"))

[<Fact>]
let ``a PDF written again while it's open, as an export can, is validated again there`` () =
    withTwoPdfs (fun first second ->
        onUiThread (fun () ->
            let rows (window: MainWindow) = (HeadlessWindow.resultsGrid window).ItemsSource |> Seq.cast<obj> |> Seq.length
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window first
            let other, _ = wait (MainWindow.Open(DocumentShell.Shared, second, window))
            let before = rows other
            // The copy exported again: here, another PDF written over it.
            File.Copy(first, second, true)

            let brought, notValidated = wait (MainWindow.Open(DocumentShell.Shared, second, window))
            let again, report = wait (MainWindow.Open(DocumentShell.Shared, second, window, true))
            let after = rows other
            other.Close()
            window.Close()

            // Only brought forward, unless it's changed.
            obj.ReferenceEquals(brought, other) |> should be True
            notValidated |> should equal None
            obj.ReferenceEquals(again, other) |> should be True
            report.IsSome |> should be True
            after |> should not' (equal before)
            after |> should equal (rows window)))
