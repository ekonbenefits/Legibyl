/// Legibyl's own windows, for a screen reader: every control a person can use has a name.
module Legibyl.Gui.Tests.AccessibilityTests

open System
open System.IO
open Avalonia
open Avalonia.Automation.Peers
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Interactivity
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession


/// What a button says: its screen-reader name (an edit button's label, after its pencil), else its text.
let private buttonText (b: Button) =
    match Automation.AutomationProperties.GetName b with
    | null | "" -> string b.Content
    | name -> name

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 1.0) (fun () -> false) |> ignore

let private corpusFile name = Path.Combine(AppContext.BaseDirectory, "SampleCorpus", name)

/// The controls a person uses in root (buttons, pickers, check boxes, text boxes, switches,
/// sliders) that a screen reader has no name for: each as its type and what it shows.
let unnamed (root: Visual) =
    [ for control in descendants<Control> root do
          let usable =
              match control with
              | :? Button | :? ComboBox | :? CheckBox | :? TextBox | :? ToggleSwitch | :? RadioButton | :? Slider -> control.IsVisible
              | _ -> false
          // Parts of another control (a combo box's own toggle, a scroll bar's buttons) are named by it.
          let templated = not (isNull control.TemplatedParent)
          if usable && not templated then
              let peer = ControlAutomationPeer.CreatePeerForElement control
              let name = peer.GetName()
              if String.IsNullOrWhiteSpace name then
                  let shown =
                      match control with
                      | :? ContentControl as c -> string c.Content
                      | :? TextBox as t -> $"text box ({t.PlaceholderText})"
                      | _ -> ""
                  yield $"{control.GetType().Name}: {shown}" ]

/// Opens a failing PDF in a new main window, and runs test on it.
let private withFailing (test: Window -> unit) =
    let window = HeadlessWindow.show 1200.0 720.0
    HeadlessWindow.openPdf window (corpusFile "pdfua1-fail.pdf")
    pump ()
    try test window finally window.Close()

let private button (root: Visual) (name: string) =
    descendants<Button> root |> List.find (fun b -> (string (Automation.AutomationProperties.GetName b)).StartsWith name || buttonText b = name)

let private openOwned (window: Window) (name: string) =
    (button window name).RaiseEvent(RoutedEventArgs Button.ClickEvent)
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) (fun () -> window.OwnedWindows.Count > 0) |> ignore
    pump ()
    window.OwnedWindows |> Seq.last

[<Fact>]
let ``every control in the main window has a name: empty, failing and passing`` () =
    onUiThread (fun () ->
        let window = HeadlessWindow.show 1200.0 720.0
        pump ()
        let empty = unnamed window
        HeadlessWindow.openPdf window (corpusFile "pdfua1-fail.pdf")
        pump ()
        let failing = unnamed window
        HeadlessWindow.openPdf window (corpusFile "pdfua1-pass.pdf")
        pump ()
        let passing = unnamed window
        window.Close()

        empty |> should be Empty
        failing |> should be Empty
        passing |> should be Empty)

[<Fact>]
let ``every control in the Structure window, Auto Fixes, About and Licenses has a name`` () =
    onUiThread (fun () ->
        withFailing (fun window ->
            let structure = openOwned window "Structure…"
            let inStructure = unnamed structure
            structure.Close()
            pump ()
            let fixes = openOwned window "Auto Fixes…"
            let inFixes = unnamed fixes
            fixes.Close()
            pump ()
            About.show window |> ignore
            pump ()
            let about = window.OwnedWindows |> Seq.last
            let inAbout = unnamed about
            (button about "Licenses…").RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let licenses = about.OwnedWindows |> Seq.exactlyOne
            let inLicenses = unnamed licenses
            licenses.Close()
            about.Close()

            inStructure |> should be Empty
            inFixes |> should be Empty
            inAbout |> should be Empty
            inLicenses |> should be Empty))

// ---- Colour ----

/// WCAG's contrast ratio between two colours.
let private contrast (a: Avalonia.Media.Color) (b: Avalonia.Media.Color) =
    let luminance (c: Avalonia.Media.Color) =
        let channel (v: byte) =
            let v = float v / 255.0
            if v <= 0.03928 then v / 12.92 else ((v + 0.055) / 1.055) ** 2.4
        0.2126 * channel c.R + 0.7152 * channel c.G + 0.0722 * channel c.B
    let la, lb = luminance a, luminance b
    (max la lb + 0.05) / (min la lb + 0.05)

/// The blocks' background in a theme (light or dark), as the app's theme gives it.
let private background (variant: Avalonia.Styling.ThemeVariant) =
    match Application.Current |> nonNull |> fun app -> app.TryGetResource(Theme.Tokens.Background, variant) with
    | true, (:? Avalonia.Media.ISolidColorBrush as brush) -> brush.Color
    | true, (:? Avalonia.Media.Color as color) -> color
    | _ -> failwith $"No {Theme.Tokens.Background} in the {variant} theme"

let private ofRgb (rgb: uint32) = Avalonia.Media.Color.FromRgb(byte (rgb >>> 16), byte (rgb >>> 8), byte rgb)

[<Fact>]
let ``each tag's block border stands out 3:1 in light and dark, and its chip's text 4.5:1`` () =
    onUiThread (fun () ->
        let light, dark = background Avalonia.Styling.ThemeVariant.Light, background Avalonia.Styling.ThemeVariant.Dark
        let tags = [ "H1"; "P"; "LI"; "Link"; "Table"; "Figure"; "Artifact"; "Document" ]
        let low =
            [ for tag in tags do
                  let border, chip = StructureColors.colorsFor tag
                  for variant, back in [ "light", light; "dark", dark ] do
                      if contrast border back < 3.0 then yield $"{tag} border on {variant}: {contrast border back:F2}"
                  // The chip's text is black on its pastel, in either theme.
                  if contrast Avalonia.Media.Colors.Black chip < 4.5 then yield $"{tag} chip text: {contrast Avalonia.Media.Colors.Black chip:F2}" ]

        low |> should be Empty)

[<Fact>]
let ``each outline on the page preview stands out 3:1 against the white page`` () =
    let white = Avalonia.Media.Colors.White
    let low =
        [ for tag in [ "H1"; "P"; "LI"; "Figure"; "Table"; "Artifact"; "Document" ] do
              let outline = ofRgb (StructureColors.outlineColor tag)
              if contrast outline white < 3.0 then yield $"{tag}: {contrast outline white:F2}"
          let grey = ofRgb StructureColors.leftOutColor
          if contrast grey white < 3.0 then yield $"left out: {contrast grey white:F2}" ]

    low |> should be Empty

// ---- Announcements and the keyboard ----

let private isLive (text: TextBlock) =
    Automation.AutomationProperties.GetLiveSetting text = Automation.AutomationLiveSetting.Polite

[<Fact>]
let ``the banner and what was just done are announced as they change`` () =
    onUiThread (fun () ->
        withFailing (fun window ->
            let texts = descendants<TextBlock> window
            let banner = texts |> List.find (fun t -> (string t.Text).StartsWith "FAIL")
            // A notice line, empty until something's done.
            let notices = texts |> List.filter (fun t -> isLive t && not (obj.ReferenceEquals(t, banner)))

            isLive banner |> should be True
            notices |> should haveLength 1))

[<Fact>]
let ``a block can be reached, selected and left with the keyboard, and Move Up and Down have shortcuts`` () =
    onUiThread (fun () ->
        withFailing (fun window ->
            let structure = openOwned window "Structure…"
            let blocks =
                descendants<Border> structure |> List.filter (fun b -> (b.Tag :? StructureNode) && b.Focusable)
            let first = blocks.Head
            first.Focus() |> ignore
            first.RaiseEvent(Input.KeyEventArgs(RoutedEvent = Input.InputElement.KeyDownEvent, Key = Input.Key.Down, Source = first))
            pump ()
            let moved = structure.FocusManager.GetFocusedElement() |> Option.ofObj |> Option.map box
            let second = blocks[1]
            second.RaiseEvent(Input.KeyEventArgs(RoutedEvent = Input.InputElement.KeyDownEvent, Key = Input.Key.Enter, Source = second))
            pump ()
            let note = descendants<TextBlock> structure |> List.exists (fun t -> (string t.Text).StartsWith "Page ")
            let named = string (Automation.AutomationProperties.GetName second)
            let moveUp = descendants<Button> structure |> List.find (fun b -> buttonText b = "Move Up")
            // (Read before closing, which takes a button's shortcut off it.)
            let shortcut = moveUp.HotKey |> Option.ofObj |> Option.map (fun g -> g.Key, g.KeyModifiers)
            structure.Close()

            blocks.Length |> should be (greaterThan 1)
            moved |> should equal (Some(box second))
            named |> should equal (StructureBlocks.blockName (nonNull second.Tag :?> StructureNode))
            note |> should be True
            shortcut |> should equal (Some(Input.Key.Up, Input.KeyModifiers.Alt))))



[<Fact>]
let ``the results list's rows, and the structure tree's items, are said as a sentence, not a record`` () =
    onUiThread (fun () ->
        withFailing (fun window ->
            let rows = [ for item in descendants<ListBoxItem> (HeadlessWindow.resultsList window) -> (ControlAutomationPeer.CreatePeerForElement item).GetName() ]
            let structure = openOwned window "Structure…"
            let tabs = descendants<TabControl> structure |> List.head
            tabs.SelectedItem <- tabs.Items |> Seq.cast<TabItem> |> Seq.find (fun t -> string t.Header = "Tree")
            pump ()
            let items = [ for item in descendants<TreeViewItem> structure -> (ControlAutomationPeer.CreatePeerForElement item).GetName() ]
            structure.Close()

            rows |> should not' (be Empty)
            rows.Head |> should equal "Next step: Fix the rules with the most failures"
            rows[1] |> should startWith "Failure 7.1-1: Content marked as Artifact should not be present inside tagged content. 1 place"
            items |> should not' (be Empty)
            // No record's insides ("{ Tag = …"), and short.
            for name in rows @ items do
                name |> should not' (haveSubstring "{")
                name.Length |> should be (lessThan 400)))

/// A theme colour in light or dark.
let private themeColor (key: string) (variant: Avalonia.Styling.ThemeVariant) =
    match Application.Current |> nonNull |> fun app -> app.TryGetResource(key, variant) with
    | true, (:? Avalonia.Media.ISolidColorBrush as brush) -> brush.Color
    | true, (:? Avalonia.Media.Color as color) -> color
    | _ -> failwith $"No {key} in the {variant} theme"

[<Fact>]
let ``text on the banner, the count pills and the badges stands out 4.5:1, light and dark`` () =
    onUiThread (fun () ->
        let low =
            [ for variant in [ Avalonia.Styling.ThemeVariant.Light; Avalonia.Styling.ThemeVariant.Dark ] do
                  // The banner and the pills: black on a status colour.
                  for key in [ Theme.Tokens.Danger; Theme.Tokens.Success; Theme.Tokens.Warning ] do
                      let ratio = contrast Avalonia.Media.Colors.Black (themeColor key variant)
                      if ratio < 4.5 then yield $"black on {key} ({variant}): {ratio:F2}"
                  // A badge's count: the background's colour on the primary one.
                  let ratio = contrast (themeColor Theme.Tokens.Background variant) (themeColor Theme.Tokens.Primary variant)
                  if ratio < 4.5 then yield $"badge ({variant}): {ratio:F2}" ]

        low |> should be Empty)
