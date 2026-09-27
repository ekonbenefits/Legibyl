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

            items |> List.map fst |> should equal [ "Open…"; "Close Window"; "Save"; "Export…" ]
            // Nothing to save or export until there's a change or a fix chosen.
            saveAtFirst |> should be False
            exportAtFirst |> should be False))
