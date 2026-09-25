namespace Legibyl.Gui

open System
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Styling
open Semi.Avalonia
open Semi.Avalonia.DataGrid

type App() =
    inherit Application()

    override this.Initialize() =
        // Follow the OS light/dark setting.
        this.RequestedThemeVariant <- ThemeVariant.Default
        this.Styles.Add(SemiTheme())
        this.Styles.Add(DataGridSemiTheme())

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop -> desktop.MainWindow <- MainWindow()
        | _ -> ()

        base.OnFrameworkInitializationCompleted()

module Program =
    [<EntryPoint; STAThread>]
    let main args =
        AppBuilder.Configure<App>().UsePlatformDetect().LogToTrace().StartWithClassicDesktopLifetime args
