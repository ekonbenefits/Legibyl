module Legibyl.Gui.Tests.MainWindowTests

open System
open System.IO
open System.Threading
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Controls.Shapes
open Avalonia.VisualTree
open Xunit
open Legibyl.Screenshots

// One headless Avalonia session (and UI thread) for the whole test run.
let private session =
    lazy (Avalonia.Headless.HeadlessUnitTestSession.StartNew(typeof<HeadlessWindow.HeadlessApp>))

let private onUiThread (test: unit -> unit) =
    session.Value.Dispatch(Action test, CancellationToken.None).GetAwaiter().GetResult()

let private corpusFile name =
    Path.Combine(AppContext.BaseDirectory, "SampleCorpus", name)

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

/// Icons in the finding details (the block that holds the Copy Finding button).
let private detailIcons (window: Window) =
    let copyFinding = descendants<Button> window |> List.find (fun button -> string button.Content = "Copy Finding")
    match copyFinding.GetVisualParent() with
    | null -> []
    | details -> descendants<Path> details

let private isHiddenFromScreenReaders (icon: Path) =
    AutomationProperties.GetAccessibilityView icon = AccessibilityView.Raw

let private openSample name =
    let window = HeadlessWindow.show 1200.0 720.0
    HeadlessWindow.openPdf window (corpusFile name)
    window

[<Fact>]
let ``results grid column headers are not clipped`` () =
    onUiThread (fun () ->
        let window = openSample "libreoffice-empty-paragraph.pdf"
        // Skip the grid's empty filler header after the last column.
        let headers =
            descendants<DataGridColumnHeader> (HeadlessWindow.resultsGrid window)
            |> List.filter (fun header -> not (String.IsNullOrEmpty(string header.Content)))
        Assert.NotEmpty headers
        let clipped =
            [ for header in headers do
                  let available = header.Bounds.Width
                  header.Measure(Size(Double.PositiveInfinity, header.Bounds.Height))
                  if header.DesiredSize.Width > available + 0.5 then
                      $"'{header.Content}' needs {header.DesiredSize.Width:F1}px but has {available:F1}px" ]
        window.Close()
        Assert.True(clipped.IsEmpty, String.Join("; ", clipped)))

[<Fact>]
let ``empty paragraph warning shows its icons, page, fix, and preview`` () =
    onUiThread (fun () ->
        let window = openSample "libreoffice-empty-paragraph.pdf"
        let grid = HeadlessWindow.resultsGrid window
        let firstRow = descendants<DataGridRow> grid |> List.find (fun row -> row.GetIndex() = 0)

        let icons = descendants<Path> firstRow
        let expected = [ "Warning"; "Fixable: Remove empty /P structure elements" ]
        Assert.Equal<string list>(expected, icons |> List.map AutomationProperties.GetName)
        Assert.Equal<string list>(expected, icons |> List.map (fun icon -> string (ToolTip.GetTip icon)))
        Assert.Contains("Page 1", descendants<TextBlock> firstRow |> List.map _.Text)

        // Warning icon by the title and a wrench by "Fixable"; the text says the same,
        // so screen readers skip them.
        let anchors = detailIcons window
        Assert.Equal(2, anchors.Length)
        Assert.All(anchors, fun icon -> Assert.True(isHiddenFromScreenReaders icon))

        let details = descendants<TextBox> window |> List.map _.Text
        Assert.Contains("Remove empty /P structure elements. Use Fix… to repair a copy of this PDF.", details)

        let preview = descendants<Image> window |> List.tryFind (fun image -> not (isNull image.Source))
        Assert.True(preview.IsSome, "The selected warning's page preview wasn't shown.")
        window.Close())

[<Fact>]
let ``findings that can't be fixed show no fix icon or details`` () =
    onUiThread (fun () ->
        let window = openSample "pages-clipped-paragraphs.pdf"
        let rows = descendants<DataGridRow> (HeadlessWindow.resultsGrid window)
        // The first row is untagged content, which no fix addresses.
        let firstRow = rows |> List.find (fun row -> row.GetIndex() = 0)
        let icons = descendants<Path> firstRow |> List.map AutomationProperties.GetName
        let detailHeadings = descendants<TextBlock> window |> List.map _.Text
        let anchors = detailIcons window
        window.Close()
        Assert.Equal(1, anchors.Length)
        Assert.Equal<string list>([ "Failure" ], icons)
        Assert.DoesNotContain("Fixable", detailHeadings))
