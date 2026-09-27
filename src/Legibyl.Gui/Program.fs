namespace Legibyl.Gui

open System
open System.IO
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Styling
open Semi.Avalonia
open Semi.Avalonia.DataGrid

type App() =
    inherit Application()

    override this.Initialize() =
        // Shown as the macOS app menu's title.
        this.Name <- "Legibyl"
        // Follow the OS light/dark setting.
        this.RequestedThemeVariant <- ThemeVariant.Default
        this.Styles.Add(SemiTheme())
        this.Styles.Add(DataGridSemiTheme())

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            let first = MainWindow()
            desktop.MainWindow <- first
            // PDFs named on the command line: the first in this window, each other in its own.
            let pdfs = desktop.Args |> Option.ofObj |> Option.defaultValue [||] |> Array.filter File.Exists |> List.ofArray
            first.Opened.Add(fun _ ->
                match pdfs with
                | head :: rest ->
                    first.OpenFile head
                    for pdf in rest do
                        MainWindow.Open(DocumentShell.Shared, pdf, first) |> ignore
                | [] -> ())
        | _ -> ()

        base.OnFrameworkInitializationCompleted()

module Program =
    [<EntryPoint; STAThread>]
    let main args =
        AppBuilder.Configure<App>().UsePlatformDetect().LogToTrace().StartWithClassicDesktopLifetime args
