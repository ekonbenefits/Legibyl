namespace Legibyl.Gui

open System
open System.IO
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Platform.Storage
open Avalonia.Styling
open Semi.Avalonia

type App() =
    inherit Application()

    override this.Initialize() =
        // Shown as the macOS app menu's title.
        this.Name <- "Legibyl"
        // Follow the OS light/dark setting.
        this.RequestedThemeVariant <- ThemeVariant.Default
        this.Styles.Add(SemiTheme())

    override this.OnFrameworkInitializationCompleted() =
        // veraPDF's one-time setup takes a couple of seconds: done in the background now, while a
        // PDF is being picked, rather than when the first one is validated.
        Threading.Tasks.Task.Run(fun () ->
            try
                Legibyl.Validation.VeraPdfValidator.Warm()
            with _ ->
                // A real validation reports what's wrong; this was only a head start.
                ())
        |> ignore
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            desktop.MainWindow <- MainWindow()
            let openPdf (path: string) =
                if File.Exists path then MainWindow.Open(DocumentShell.Shared, path) |> ignore
            // PDFs named on the command line (as Windows' Open With passes them): the first in the
            // window just made, each other in its own.
            desktop.Args |> Option.ofObj |> Option.defaultValue [||] |> Array.iter openPdf
            // PDFs macOS asks the app to open (Finder's Open With, or a PDF dropped on its icon),
            // whether it's starting or running.
            match this.TryGetFeature<IActivatableLifetime>() with
            | NonNull activatable ->
                activatable.Activated.Add(fun e ->
                    match e with
                    | :? FileActivatedEventArgs as files ->
                        for file in files.Files do
                            match file.TryGetLocalPath() with
                            | NonNull path -> openPdf path
                            | Null -> ()
                    | _ -> ())
            | Null -> ()
        | _ -> ()

        base.OnFrameworkInitializationCompleted()

module Program =
    [<EntryPoint; STAThread>]
    let main args =
        AppBuilder.Configure<App>().UsePlatformDetect().LogToTrace().StartWithClassicDesktopLifetime args
