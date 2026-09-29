/// Drives the real Legibyl MainWindow on Avalonia's headless platform (rendered with Skia,
/// no display needed). Shared by the screenshot tool and tests/Legibyl.Gui.Tests.
module Legibyl.Screenshots.HeadlessWindow

open System
open System.Diagnostics
open System.Threading
open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Threading
open Avalonia.VisualTree
open Legibyl.Gui

/// The app configured for headless rendering. Also the entry point that
/// HeadlessUnitTestSession looks for (a static BuildAvaloniaApp).
type HeadlessApp =
    static member BuildAvaloniaApp() : AppBuilder =
        // PDFs opened here aren't a person's: they go in a list of their own.
        RecentFiles.FilePath <- IO.Path.Combine(IO.Path.GetTempPath(), $"legibyl-recent-{Guid.NewGuid():N}.json")
        AppBuilder
            .Configure<App>()
            .UseSkia()
            .UseHeadless(AvaloniaHeadlessPlatformOptions(UseHeadlessDrawing = false))

/// Runs pending UI work and renders a frame, repeatedly, until condition holds.
/// Returns false if timeout passes first.
let pumpUntil (timeout: TimeSpan) (condition: unit -> bool) =
    let stopwatch = Stopwatch.StartNew()
    let mutable met = condition ()
    while not met && stopwatch.Elapsed < timeout do
        Dispatcher.UIThread.RunJobs()
        AvaloniaHeadlessPlatform.ForceRenderTimerTick()
        Thread.Sleep 20
        met <- condition ()
    Dispatcher.UIThread.RunJobs()
    AvaloniaHeadlessPlatform.ForceRenderTimerTick()
    met

let private descendants<'T when 'T :> Visual> (window: Window) : 'T seq =
    window.GetVisualDescendants() |> Seq.choose (fun visual -> match box visual with :? 'T as control -> Some control | _ -> None)

let private isBusy (window: Window) =
    descendants<ProgressBar> window |> Seq.exists (fun bar -> bar.IsEffectivelyVisible)

/// Waits for work the window has just started (validation, a page preview) to finish.
let private waitForIdle (window: Window) =
    // The progress bar may take a moment to appear, and never appears if the work
    // completes within one frame, so a short first wait isn't an error.
    pumpUntil (TimeSpan.FromSeconds 2.0) (fun () -> isBusy window) |> ignore
    if not (pumpUntil (TimeSpan.FromMinutes 2.0) (fun () -> not (isBusy window))) then
        raise (TimeoutException "The window was still busy after two minutes.")

/// The results list: each rule's row, and its findings under it where it's opened.
let resultsList (window: Window) : ListBox = descendants<ListBox> window |> Seq.find (fun list -> list.Name = "Results")

/// The list's items, as shown.
let resultItems (window: Window) : ResultItem list =
    match (resultsList window).ItemsSource with
    | null -> []
    | items -> items |> Seq.cast<ResultItem> |> List.ofSeq

/// Every finding, in the list's order (each rule's, whether it's opened or not).
let findings (window: Window) : RuleViolationRow list =
    [ for item in resultItems window do
          match item with
          | RuleItem(group, _) -> yield! group.Findings
          | FindingItem _
          | PassItem _
          | NextStepItem _ -> () ]

/// Shows a new MainWindow at the given size.
let show (width: float) (height: float) =
    let window = MainWindow(Width = width, Height = height)
    window.Show()
    pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
    window

/// Opens and validates pdfPath, waiting until the results (and the first row's
/// preview) are shown.
let openPdf (window: MainWindow) (pdfPath: string) =
    window.OpenFile pdfPath
    waitForIdle window

/// Opens every rule's findings (a rule found many times starts collapsed), as clicking their
/// rows' arrows does.
let expandGroups (window: Window) =
    let collapsed () =
        descendants<Button> window
        |> Seq.tryFind (fun button -> (button.Tag :? string) && string button.Tag = "rule-expander" && string button.Content = "▸")
    let mutable next = collapsed ()
    let mutable guard = 0
    while next.IsSome && guard < 1000 do
        next.Value.RaiseEvent(Interactivity.RoutedEventArgs(Button.ClickEvent))
        pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
        next <- collapsed ()
        guard <- guard + 1
    pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

/// Selects a finding (0-based, in the order of findings) and waits for its preview.
let selectRow (window: MainWindow) (index: int) =
    let all = findings window
    if index < 0 || index >= all.Length then
        raise (ArgumentOutOfRangeException(nameof index, $"Finding {index} doesn't exist; there are {all.Length}."))
    expandGroups window
    (resultsList window).SelectedItem <- FindingItem all[index]
    waitForIdle window

/// The list's row for a finding, once it's shown (see expandGroups).
let findingContainer (window: Window) (row: RuleViolationRow) : ListBoxItem =
    descendants<ListBoxItem> window
    |> Seq.find (fun item ->
        match item.DataContext with
        | :? ResultItem as FindingItem shown -> obj.ReferenceEquals(shown, row)
        | _ -> false)

/// Saves the window's current rendered frame as a PNG.
let savePng (window: Window) (path: string) =
    match window.CaptureRenderedFrame() with
    | null -> raise (InvalidOperationException "The headless platform produced no frame.")
    | frame -> frame.Save(path, Media.Imaging.PngBitmapEncoderOptions.Default)
