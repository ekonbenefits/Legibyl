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
open FsUnit.Xunit
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

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
        headers |> should not' (be Empty)
        let clipped =
            [ for header in headers do
                  let available = header.Bounds.Width
                  header.Measure(Size(Double.PositiveInfinity, header.Bounds.Height))
                  if header.DesiredSize.Width > available + 0.5 then
                      $"'{header.Content}' needs {header.DesiredSize.Width:F1}px but has {available:F1}px" ]
        window.Close()
        clipped |> should be Empty)

[<Fact>]
let ``empty paragraph warning shows its icons, page, fix, and preview`` () =
    onUiThread (fun () ->
        let window = openSample "libreoffice-empty-paragraph.pdf"
        let grid = HeadlessWindow.resultsGrid window
        let firstRow = descendants<DataGridRow> grid |> List.find (fun row -> row.GetIndex() = 0)

        let icons = descendants<Path> firstRow
        let expected = [ "Warning"; "Fixable: Remove empty paragraph tags" ]
        icons |> List.map AutomationProperties.GetName |> should equal expected
        icons |> List.map (fun icon -> string (ToolTip.GetTip icon)) |> should equal expected
        descendants<TextBlock> firstRow |> List.map _.Text |> should contain "Page 1"

        // Warning icon by the title and a wrench by "Fixable"; the text says the same,
        // so screen readers skip them.
        let anchors = detailIcons window
        anchors |> should haveLength 2
        anchors |> List.filter (not << isHiddenFromScreenReaders) |> should be Empty

        let details = descendants<TextBox> window |> List.map _.Text
        details |> should contain "Remove empty paragraph tags. Choose it in Auto Fixes…, then Export… a copy of this PDF."

        let preview = descendants<Image> window |> List.tryFind (fun image -> not (isNull image.Source))
        // The selected warning's page preview is shown.
        preview |> should not' (equal None)
        window.Close())

[<Fact>]
let ``findings that can't be fixed show no fix icon or details`` () =
    onUiThread (fun () ->
        let window = openSample "pages-clipped-paragraphs.pdf"
        HeadlessWindow.expandGroups window
        let rows = descendants<DataGridRow> (HeadlessWindow.resultsGrid window)
        // The first row is untagged content, which no fix addresses.
        let firstRow = rows |> List.find (fun row -> row.GetIndex() = 0)
        let icons = descendants<Path> firstRow |> List.map AutomationProperties.GetName
        let detailHeadings = descendants<TextBlock> window |> List.map _.Text
        let anchors = detailIcons window
        window.Close()
        anchors |> should haveLength 1
        icons |> should equal [ "Failure" ]
        detailHeadings |> should not' (contain "Fixable"))

let private texts (window: Window) = descendants<TextBlock> window |> List.map (fun t -> string t.Text)

[<Fact>]
let ``an empty window says how to start`` () =
    onUiThread (fun () ->
        let window = HeadlessWindow.show 1200.0 720.0
        let shown = texts window
        window.Close()

        shown |> should contain "Making a PDF accessible"
        shown |> List.exists (fun t -> t.StartsWith "Open a PDF: Browse…") |> should be True)

[<Fact>]
let ``a PDF that passes says what automated checks can't judge`` () =
    onUiThread (fun () ->
        let window = openSample "pdfua1-pass.pdf"
        let shown = texts window
        window.Close()

        shown |> should contain "Passes PDF/UA-1's automated checks"
        shown |> List.exists (fun t -> t.StartsWith "Alt text: does each figure's say what it's for here") |> should be True
        // Not the steps for an empty window.
        shown |> should not' (contain "Making a PDF accessible"))

[<Fact>]
let ``a failure no auto fix repairs says what it means, and what to do`` () =
    onUiThread (fun () ->
        let window = openSample "pdfua1-fail.pdf"
        let rows = (HeadlessWindow.resultsGrid window).ItemsSource |> Seq.cast<Legibyl.Gui.RuleViolationRow> |> List.ofSeq
        HeadlessWindow.selectRow window (rows |> List.findIndex (fun r -> r.Violation.Clause = "7.1" && r.Violation.TestNumber = 1))
        let shown = texts window
        let details = descendants<TextBox> window |> List.map (fun t -> string t.Text)
        window.Close()

        shown |> should contain "What it means"
        shown |> should contain "What to do"
        details |> List.exists (fun t -> t.StartsWith "Something marked as decoration") |> should be True)

[<Fact>]
let ``findings are grouped by rule, the count first, a rule found many times collapsed`` () =
    onUiThread (fun () ->
        let window = openSample "pages-clipped-paragraphs.pdf"
        HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 1.0) (fun () -> false) |> ignore
        let grid = HeadlessWindow.resultsGrid window
        let keys =
            descendants<DataGridRowGroupHeader> grid
            |> List.choose (fun h -> match h.DataContext with :? Avalonia.Collections.DataGridCollectionViewGroup as g -> Some(string g.Key, g.ItemCount) | _ -> None)
        let findings = grid.ItemsSource |> Seq.cast<obj> |> Seq.length
        let rowsShown = descendants<DataGridRow> grid |> List.filter _.IsVisible |> List.length
        window.Close()

        keys |> List.exists (fun (key, count) -> count > 3 && key.StartsWith $"{count} × 7.1-3: ") |> should be True
        rowsShown |> should be (lessThan findings))

[<Fact>]
let ``a PDF 2.0 file is checked against PDF/UA-2 without choosing, and the banner says why`` () =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "annual-report.pdf")
    do
        use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfWriter(pdf, iText.Kernel.Pdf.WriterProperties().SetPdfVersion(iText.Kernel.Pdf.PdfVersion.PDF_2_0)))
        iText.Kernel.Pdf.Canvas.PdfCanvas(document.AddNewPage())
            .BeginText().SetFontAndSize(iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA, 12f)
            .MoveText(72.0, 720.0).ShowText("A PDF 2.0 page.").EndText()
        |> ignore
    try
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window pdf
            let shown = texts window
            let pickers = descendants<ComboBox> window
            window.Close()

            shown |> List.exists (fun t -> t.StartsWith "FAIL" && t.Contains "against PDF/UA-2 (a PDF 2.0 file; ") |> should be True
            // No profile picker to choose from.
            pickers |> should be Empty)
    finally
        Directory.Delete(folder, true)

[<Fact>]
let ``Check Again checks the open PDF again, and needs one open`` () =
    onUiThread (fun () ->
        let checkAgain (window: Window) =
            descendants<Button> window |> List.find (fun b -> string b.Content = "Check Again")
        let empty = HeadlessWindow.show 1200.0 720.0
        let enabledEmpty = (checkAgain empty).IsEnabled
        empty.Close()
        let window = openSample "pdfua1-fail.pdf"
        let button = checkAgain window
        let enabledOpen = button.IsEnabled
        let tip = string (ToolTip.GetTip button)
        let before = (HeadlessWindow.resultsGrid window).ItemsSource
        button.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs(Button.ClickEvent))
        // A new report, once it's done.
        let checkedAgain =
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 30.0) (fun () ->
                let now = (HeadlessWindow.resultsGrid window).ItemsSource
                not (isNull now) && not (obj.ReferenceEquals(now, before)) && button.IsEnabled)
        window.Close()

        enabledEmpty |> should be False
        enabledOpen |> should be True
        tip |> should equal "Check this PDF again, after it's changed on disk."
        checkedAgain |> should be True)
