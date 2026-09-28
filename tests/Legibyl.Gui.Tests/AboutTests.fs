module Legibyl.Gui.Tests.AboutTests

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private texts (root: Visual) =
    [ for t in descendants<TextBlock> root -> string t.Text ]

let private button (root: Visual) (label: string) =
    descendants<Button> root |> List.find (fun b -> string b.Content = label)

/// Opens About over a main window, runs test on it, and closes both.
let private withAbout (test: Window -> Window -> unit) =
    onUiThread (fun () ->
        let owner = HeadlessWindow.show 1000.0 700.0
        About.show owner |> ignore
        pump ()
        let about = owner.OwnedWindows |> Seq.exactlyOne
        try test owner about
        finally
            for window in List.ofSeq about.OwnedWindows do window.Close()
            about.Close()
            owner.Close())

[<Fact>]
let ``About says the version and the veraPDF it bundles, and what it's built on`` () =
    withAbout (fun _ about ->
        let shown = texts about
        shown |> should contain $"Version {BuildInfo.version} (vendored veraPDF engine {VeraPdfValidator.EngineVersion})"
        // The version from Directory.Build.props: veraPDF's, then the build number.
        BuildInfo.version |> should startWith VeraPdfValidator.EngineVersion
        for name in [ "veraPDF"; "IKVM.NET"; "iText"; "PdfPig"; "PDFium"; "Tesseract tessdata"; "Avalonia" ] do
            shown |> List.exists (fun t -> t.StartsWith(name + " ")) |> should be True
        shown |> List.exists (fun t -> t.Contains "unknown") |> should be False
        shown |> List.exists (fun t -> t.StartsWith "Legibyl is free software under the GNU Affero General Public License") |> should be True)

[<Fact>]
let ``Copy's text starts with the line legibyl --version prints`` () =
    let lines = About.copyText().Split(Environment.NewLine)
    lines[0] |> should equal BuildInfo.versionLine
    lines[0] |> should startWith $"legibyl {BuildInfo.version} (vendored veraPDF engine "
    lines.Length |> should equal (About.components().Length + 1)

[<Fact>]
let ``Licenses… shows the NOTICE, and each license text`` () =
    withAbout (fun _ about ->
        (button about "Licenses…").RaiseEvent(RoutedEventArgs Button.ClickEvent)
        pump ()
        let licenses = about.OwnedWindows |> Seq.exactlyOne
        let shown = texts licenses |> String.concat "\n"
        let names = (descendants<ComboBox> licenses |> List.exactlyOne).Items |> Seq.map string |> List.ofSeq

        shown |> should haveSubstring "# Third-Party Notices"
        names |> List.head |> should equal "NOTICE.md"
        names |> should contain "LICENSE.AGPL")

[<Fact>]
let ``Help › About Legibyl… is in the window's menu where the application has no menu of its own`` () =
    onUiThread (fun () ->
        let window = HeadlessWindow.show 1000.0 700.0
        pump ()
        let headers = NativeMenu.GetMenu window |> nonNull |> _.Items |> Seq.choose (function :? NativeMenuItem as i -> Some i | _ -> None) |> List.ofSeq
        let help = headers |> List.tryFind (fun i -> i.Header = "Help")
        window.Close()

        if OperatingSystem.IsMacOS() then
            help |> should equal None
        else
            help.Value.Menu |> nonNull |> _.Items |> Seq.map (fun i -> (i :?> NativeMenuItem).Header) |> should contain "About Legibyl…")

[<Fact>]
let ``the empty window says the version, with About… beside it`` () =
    onUiThread (fun () ->
        let window = HeadlessWindow.show 1000.0 700.0
        pump ()
        let shown = texts window
        let about = descendants<Button> window |> List.tryFind (fun b -> string b.Content = "About…")
        window.Close()

        shown |> should contain $"Legibyl {BuildInfo.version} · "
        about.IsSome |> should be True)
