/// The menus: File (Open…, Open Recent, Append PDFs…, Close Window, Save, Export…), in the menu bar on macOS with
/// its ⌘ shortcuts, and at the top of the window elsewhere, with Ctrl shortcuts and a Help menu
/// (About; on macOS it's in the application menu, see App).
module Legibyl.Gui.MainMenu

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Data
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Input.Platform
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform.Storage
open Avalonia.Styling
open Legibyl.Layout
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Gui.BusyStatus

/// A menu item's command: runs action when the item's chosen (from the menu bar, the in-window
/// menu, or a test).
let command (action: unit -> unit) =
    let changed = Event<EventHandler, EventArgs>()
    { new System.Windows.Input.ICommand with
        member _.CanExecute _ = true
        member _.Execute _ = action ()
        [<CLIEvent>]
        member _.CanExecuteChanged = changed.Publish }

/// What the menus do, from the window.
type Actions =
    { Open: unit -> unit
      /// Opens a PDF opened lately.
      OpenPath: string -> unit
      /// Whether Save would keep anything now.
      CanSave: unit -> bool
      Save: unit -> unit
      /// Whether Export… can run now (not while busy).
      CanExport: unit -> bool
      Export: unit -> unit
      /// Joins PDFs into one (see AppendDialog), if it can run now (not while busy).
      Append: unit -> unit }

/// Puts the menus on window; returns its Save and Export… items, to enable as the buttons are.
let install (window: Window) (actions: Actions) : NativeMenuItem * NativeMenuItem =
    let mac = OperatingSystem.IsMacOS()
    let item (header: string) (key: Key) (action: unit -> unit) =
        let item = NativeMenuItem(header, Command = command action)
        if mac then item.Gesture <- KeyGesture(key, KeyModifiers.Meta)
        item
    let save = item "Save" Key.S (fun () -> if actions.CanSave () then actions.Save ())
    save.ToolTip <-
        "Keeps your review (edits, dismissed warnings, the title, the auto fixes chosen) in a .legibyl.json file next to the PDF, to pick up where you left off. The PDF isn't changed. Closing the window asks about changes not saved, unless they're all in a copy just exported."
    let exportItem = item "Export…" Key.E (fun () -> if actions.CanExport () then actions.Export ())
    // The PDFs opened lately, each opened as File › Open… would.
    let recent = NativeMenu()
    let fillRecent () =
        recent.Items.Clear()
        let paths = RecentFiles.read ()
        for path, label in List.zip paths (RecentFiles.labels paths) do
            let entry = NativeMenuItem(label, ToolTip = path)
            entry.Command <- command (fun () -> actions.OpenPath path)
            recent.Items.Add entry
        if not paths.IsEmpty then recent.Items.Add(NativeMenuItemSeparator())
        let clear = NativeMenuItem("Clear Menu", IsEnabled = not paths.IsEmpty)
        clear.Command <- command RecentFiles.clear
        recent.Items.Add clear
    fillRecent ()
    let refill = RecentFiles.Changed.Subscribe(fun () -> Avalonia.Threading.Dispatcher.UIThread.Post fillRecent)
    window.Closed.Add(fun _ -> refill.Dispose())
    let file = NativeMenu()
    file.Items.Add(item "Open…" Key.O actions.Open)
    file.Items.Add(NativeMenuItem("Open Recent", Menu = recent))
    let append = NativeMenuItem("Append PDFs…", Command = command actions.Append)
    append.ToolTip <- "Joins PDFs that pass PDF/UA into one, keeping each one's tags, and checks the result."
    file.Items.Add append
    file.Items.Add(NativeMenuItemSeparator())
    file.Items.Add(item "Close Window" Key.W (fun () -> window.Close()))
    file.Items.Add(save)
    file.Items.Add(exportItem)
    let menu = NativeMenu()
    menu.Items.Add(NativeMenuItem("File", Menu = file))
    // About: in the application menu on macOS (see App); a Help menu elsewhere.
    if not mac then
        let help = NativeMenu()
        help.Items.Add(NativeMenuItem("About Legibyl…", Command = command (fun () -> About.show window |> ignore)))
        menu.Items.Add(NativeMenuItem("Help", Menu = help))
    NativeMenu.SetMenu(window, menu)
    // Elsewhere, the keys are Ctrl+O, Ctrl+W, Ctrl+S and Ctrl+E.
    if not mac then
        window.KeyDown.Add(fun e ->
            if e.KeyModifiers.HasFlag KeyModifiers.Control then
                match e.Key with
                | Key.O -> e.Handled <- true; actions.Open ()
                | Key.W -> e.Handled <- true; window.Close()
                | Key.S when actions.CanSave () ->
                    e.Handled <- true
                    actions.Save ()
                | Key.E when actions.CanExport () ->
                    e.Handled <- true
                    actions.Export ()
                | _ -> ())
    save, exportItem
