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
    // (Beside it, on Windows and Linux, a Help menu.)
    let file = menu.Items |> Seq.cast<NativeMenuItem> |> Seq.find (fun item -> string item.Header = "File")
    [ for item in (nonNull file.Menu).Items do
          match item with
          | :? NativeMenuItemSeparator -> ()
          | :? NativeMenuItem as item -> item.Header |> string, item
          | _ -> () ]

let private descendants<'T when 'T :> Avalonia.Visual> (root: Avalonia.Visual) : 'T list =
    Avalonia.VisualTree.VisualExtensions.GetVisualDescendants root
    |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None)
    |> List.ofSeq

/// Closes window, answering Don't Save if it asks, and waits until it's gone; whether it asked.
let private closeWithoutSaving (window: Window) =
    window.Close()
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 2.0) (fun () -> window.OwnedWindows.Count > 0 || not window.IsVisible) |> ignore
    let asked = window.OwnedWindows |> Seq.exists (fun owned -> owned.Title = "Save Your Review?")
    for owned in List.ofSeq window.OwnedWindows do
        if owned.Title = "Save Your Review?" then
            (descendants<Button> owned |> List.find (fun b -> string b.Content = "Don't Save"))
                .RaiseEvent(Avalonia.Interactivity.RoutedEventArgs Button.ClickEvent)
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 5.0) (fun () -> not window.IsVisible) |> ignore
    asked

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
    withTwoPdfs (fun passing failing ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window failing
            let items = fileMenu window
            let enabled name = (items |> List.find (fst >> (=) name) |> snd).IsEnabled
            let saveAtFirst, exportAtFirst = enabled "Save", enabled "Export…"
            closeWithoutSaving window |> ignore
            let closed = not window.IsVisible

            items |> List.map fst |> should equal [ "Open…"; "Open Recent"; "Append PDFs…"; "Close Window"; "Save"; "Export…" ]
            // The validation just run, of a PDF with things to do, is kept by Save (see
            // SavedResults); nothing to export until there's a change or a fix chosen.
            saveAtFirst |> should be True
            exportAtFirst |> should be False
            closed |> should be True))

[<Fact>]
let ``a PDF that passes with nothing to do opens with nothing to save, and closes without asking`` () =
    withTwoPdfs (fun passing _ ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window passing
            let save = (fileMenu window |> List.find (fst >> (=) "Save") |> snd).IsEnabled
            let title = window.Title
            let asked = closeWithoutSaving window
            title |> should equal "pdfua1-pass.pdf — Legibyl"
            save |> should be False
            asked |> should be False
            window.IsVisible |> should be False))

/// A PDF/UA-1 file (iText's PDF/UA support makes it so): a heading and a paragraph, and no
/// bookmarks, which Legibyl warns about (Structure-7).
let private createPassingWithWarning (pdf: string) =
    use document =
        new iText.Pdfua.PdfUADocument(
            new iText.Kernel.Pdf.PdfWriter(pdf),
            iText.Pdfua.PdfUAConfig(iText.Kernel.Pdf.PdfUAConformance.PDF_UA_1, "The Community Garden", "en-US")
        )
    let font =
        iText.Kernel.Font.PdfFontFactory.CreateFont(
            Path.Combine(AppContext.BaseDirectory, "SampleCorpus", "fonts", "Lato-Regular.ttf"),
            iText.IO.Font.PdfEncodings.WINANSI,
            iText.Kernel.Font.PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED
        )
    use layout = new iText.Layout.Document(document)
    layout.SetFont font |> ignore
    let heading = iText.Layout.Element.Paragraph "The Community Garden"
    heading.GetAccessibilityProperties().SetRole(iText.Kernel.Pdf.Tagging.StandardRoles.H1) |> ignore
    layout.Add heading |> ignore
    layout.Add(iText.Layout.Element.Paragraph "Open to every resident of the neighbourhood, whatever they grow.") |> ignore

[<Fact>]
let ``a PDF that passes with its warnings dismissed opens with nothing to save, until a person changes something`` () =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try
        let pdf = Path.Combine(folder, "garden.pdf")
        createPassingWithWarning pdf
        // Its warning dismissed, and saved, with no validation kept: opening it validates it.
        Reviews.save pdf (fun review ->
            review.Dismissed <- Map [ { Dismissals.Rule = "Legibyl Structure-7"; Dismissals.Location = Dismissals.AnyLocation }, "No bookmarks" ])
        onUiThread (fun () ->
            DocumentShell.Shared.Reviews.Forget pdf
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window pdf
            let save () = (fileMenu window |> List.find (fst >> (=) "Save") |> snd).IsEnabled
            let titleAtFirst, saveAtFirst = window.Title, save ()
            // A step of the checklist ticked: a change a person made.
            let step = descendants<Border> window |> List.find (fun b -> (b.Tag :? string) && string b.Tag = "step-title")
            (descendants<CheckBox> step |> List.exactlyOne).IsChecked <- true
            HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
            let titleAfter, saveAfter = window.Title, save ()
            let asked = closeWithoutSaving window

            titleAtFirst |> should equal "garden.pdf — Legibyl"
            saveAtFirst |> should be False
            titleAfter |> should equal "garden.pdf — Edited — Legibyl"
            saveAfter |> should be True
            asked |> should be True
            window.IsVisible |> should be False)
    finally
        Directory.Delete(folder, true)

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
            let rows (window: MainWindow) = HeadlessWindow.findings window |> List.length
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
