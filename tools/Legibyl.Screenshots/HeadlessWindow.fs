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

/// The results grid.
let resultsGrid (window: Window) : DataGrid = descendants<DataGrid> window |> Seq.exactlyOne

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

/// Selects a results row (0-based) and waits for its preview.
let selectRow (window: MainWindow) (index: int) =
    let grid = resultsGrid window
    let count = grid.ItemsSource |> Seq.cast<obj> |> Seq.length
    if index < 0 || index >= count then
        raise (ArgumentOutOfRangeException(nameof index, $"Row {index} doesn't exist; there are {count} results."))
    grid.SelectedIndex <- index
    waitForIdle window

/// Saves the window's current rendered frame as a PNG.
let savePng (window: Window) (path: string) =
    match window.CaptureRenderedFrame() with
    | null -> raise (InvalidOperationException "The headless platform produced no frame.")
    | frame -> frame.Save(path, Media.Imaging.PngBitmapEncoderOptions.Default)
