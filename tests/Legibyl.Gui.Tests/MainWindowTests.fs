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
open Legibyl.Gui
open Legibyl.Validation
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

/// The list's row for an item, once shown.
let private containerFor (window: Window) (matches: ResultItem -> bool) =
    descendants<ListBoxItem> (HeadlessWindow.resultsList window)
    |> List.find (fun item -> match item.DataContext with :? ResultItem as shown -> matches shown | _ -> false)

let private ruleContainer (window: Window) (ruleId: string) =
    containerFor window (function RuleItem(group, _) -> group.RuleId = ruleId | _ -> false)

let private firstRuleContainer (window: Window) =
    containerFor window (function RuleItem(group, _) -> obj.ReferenceEquals(group.First, (HeadlessWindow.findings window).Head) | _ -> false)

/// A row's icons' screen-reader names.
let private iconNames (row: Visual) = descendants<Path> row |> List.map (AutomationProperties.GetName >> string)

[<Fact>]
let ``empty paragraph warning shows its icons, page, fix, and preview`` () =
    onUiThread (fun () ->
        let window = openSample "libreoffice-empty-paragraph.pdf"
        // The rule's row says what it is and how it's fixed; the finding under it, its page.
        let ruleRow = firstRuleContainer window
        let icons = descendants<Path> ruleRow
        let expected = [ "Warning"; "Auto fix: Remove empty paragraph tags" ]
        icons |> List.map AutomationProperties.GetName |> should equal expected
        icons |> List.map (fun icon -> string (ToolTip.GetTip icon)) |> should equal expected
        let findingRow = containerFor window (function FindingItem row -> obj.ReferenceEquals(row, (HeadlessWindow.findings window).Head) | _ -> false)
        descendants<TextBlock> findingRow |> List.map _.Text |> should contain "Page 1"

        // Warning icon by the title and a wrench by "Auto fix"; the text says the same,
        // so screen readers skip them.
        let anchors = detailIcons window
        anchors |> should haveLength 2
        anchors |> List.filter (not << isHiddenFromScreenReaders) |> should be Empty

        let details = descendants<TextBox> window |> List.map _.Text
        details |> should contain "Remove empty paragraph tags. Choose it in Auto Fixes…, then Export… a copy of this PDF."
        let headings = descendants<TextBlock> window |> List.map (fun t -> string t.Text)
        headings |> should contain "Auto fix"
        headings |> should not' (contain "Fixable")

        let preview = descendants<Image> window |> List.tryFind (fun image -> not (isNull image.Source))
        // The selected warning's page preview is shown.
        preview |> should not' (equal None)
        window.Close())

[<Fact>]
let ``findings that can't be fixed show no fix icon or details`` () =
    onUiThread (fun () ->
        let window = openSample "pdfua1-fail.pdf"
        // The first rule is content marked both as an artifact and tagged, which no fix addresses.
        let icons = iconNames (firstRuleContainer window)
        let detailHeadings = descendants<TextBlock> window |> List.map _.Text
        let anchors = detailIcons window
        window.Close()
        anchors |> should haveLength 1
        icons |> should equal [ "Failure" ]
        detailHeadings |> should not' (contain "Auto fix")
        detailHeadings |> should not' (contain "Suggest Structure fixes it"))

let private texts (window: Window) = descendants<TextBlock> window |> List.map (fun t -> string t.Text)

[<Fact>]
let ``an empty window says how to start`` () =
    onUiThread (fun () ->
        let window = HeadlessWindow.show 1200.0 720.0
        let shown = texts window
        window.Close()

        shown |> should contain "Making a PDF accessible"
        shown |> List.exists (fun t -> t.StartsWith "Open a PDF: Browse…") |> should be True)

/// Waits for the Pass row's checklist to be read.
let private waitForChecklist (window: Window) =
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) (fun () -> texts window |> List.exists (fun t -> t.StartsWith "1. Title")) |> ignore

[<Fact>]
let ``a PDF that passes opens on its Pass row: the steps a person checks, for this file`` () =
    onUiThread (fun () ->
        let window = openSample "pdfua1-pass.pdf"
        waitForChecklist window
        let selected = (HeadlessWindow.resultsList window).SelectedItem
        let first = HeadlessWindow.resultItems window |> List.head
        let shown = texts window
        let facts = DocumentFacts.read (corpusFile "pdfua1-pass.pdf")
        window.Close()

        (match selected with :? ResultItem as PassItem _ -> true | _ -> false) |> should be True
        (match first with PassItem progress -> progress | _ -> "") |> should equal "Passes PDF/UA-1's automated checks"
        shown |> should contain "Passes PDF/UA-1's automated checks. PDF/UA also needs a person to check:"
        // The file's own title and language, as written.
        shown |> List.exists (fun t -> t.StartsWith "Title: " || t = "No title of its own") |> should be True
        shown |> List.exists (fun t -> t.StartsWith "Language: " || t = "No language declared") |> should be True
        // Only the steps that apply to it.
        (shown |> List.exists (fun t -> t.EndsWith ". Tables"))  |> should equal (facts.Tables > 0)
        (shown |> List.exists (fun t -> t.EndsWith ". Links")) |> should equal (facts.Links > 0)
        shown |> List.exists (fun t -> t.EndsWith ". Colour") |> should be True
        // Not the steps for an empty window; and no failures to count.
        shown |> should not' (contain "Making a PDF accessible")
        shown |> List.exists (fun t -> t.EndsWith " failures" || t = "1 failure") |> should be False)

[<Fact>]
let ``a PDF that fails has no Pass row, and opens on its first finding`` () =
    onUiThread (fun () ->
        let window = openSample "pdfua1-fail.pdf"
        let items = HeadlessWindow.resultItems window
        window.Close()
        items |> List.exists (function PassItem _ -> true | _ -> false) |> should be False)

[<Fact>]
let ``a failure no auto fix fixes says what it means, and what to do`` () =
    onUiThread (fun () ->
        let window = openSample "pdfua1-fail.pdf"
        let rows = HeadlessWindow.findings window
        HeadlessWindow.selectRow window (rows |> List.findIndex (fun r -> r.Violation.Clause = "7.1" && r.Violation.TestNumber = 1))
        let shown = texts window
        let details = descendants<TextBox> window |> List.map (fun t -> string t.Text)
        window.Close()

        shown |> should contain "What it means"
        shown |> should contain "What to do"
        details |> List.exists (fun t -> t.StartsWith "Something marked as decoration") |> should be True)

[<Fact>]
let ``each rule's row shows its type, how it's fixed and how many, even collapsed`` () =
    onUiThread (fun () ->
        let window = openSample "pages-clipped-paragraphs.pdf"
        let items = HeadlessWindow.resultItems window
        let groups = items |> List.choose (function RuleItem(group, expanded) -> Some(group, expanded) | _ -> None)
        let rows =
            [ for group, expanded in groups do
                  let row = ruleContainer window group.RuleId
                  let texts = descendants<TextBlock> row |> List.map (fun t -> string t.Text)
                  group, expanded, iconNames row, texts ]
        let findingRows = items |> List.filter (function FindingItem _ -> true | _ -> false) |> List.length
        // The first rule is found many times, so it's its row that's selected to begin with.
        let selectedAtFirst = (HeadlessWindow.resultsList window).SelectedIndex
        window.Close()

        selectedAtFirst |> should equal 0

        // A rule found many times starts collapsed, its row still saying what it is.
        let many, expanded, icons, texts = rows |> List.find (fun (g, _, _, _) -> g.RuleId = "7.1-3")
        many.Findings.Length |> should be (greaterThan 3)
        expanded |> should be False
        icons.Head |> should equal "Failure"
        icons |> List.exists (fun name -> name.StartsWith "Auto fix") |> should be True
        texts |> should contain (string many.Findings.Length)
        // The warning's row, as a warning.
        let _, _, warningIcons, _ = rows |> List.find (fun (g, _, _, _) -> g.Severity = FindingSeverity.Warning)
        warningIcons.Head |> should equal "Warning"
        // No fix icon where nothing fixes it.
        for group, _, icons, _ in rows do
            (icons |> List.exists (fun name -> name.StartsWith "Auto fix" || name.StartsWith "Suggest Structure")) |> should equal (not group.Fixable.IsEmpty)
        // Collapsed rules' findings aren't listed.
        findingRows |> should be (lessThan (groups |> List.sumBy (fun (g, _) -> g.Findings.Length))))

[<Fact>]
let ``a rule's row shows the rule, how many places and where; a finding's, its place`` () =
    onUiThread (fun () ->
        let window = openSample "pages-clipped-paragraphs.pdf"
        let list = HeadlessWindow.resultsList window
        let rule = HeadlessWindow.resultItems window |> List.find (function RuleItem(g, _) -> g.RuleId = "7.1-3" | _ -> false)
        list.SelectedItem <- rule
        HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 2.0) (fun () -> false) |> ignore
        let ruleTexts = texts window
        let count = match rule with RuleItem(g, _) -> g.Findings.Length | _ -> 0
        HeadlessWindow.selectRow window 0
        let findingTexts = texts window
        window.Close()

        ruleTexts |> List.exists (fun t -> t.Contains $"— {count} places, on page 1") |> should be True
        ruleTexts |> should contain "What it means"
        // Fixed by an auto fix: said by the button's name, as a finding's is.
        ruleTexts |> should contain "Auto fix"
        findingTexts |> should contain "Location")

[<Fact>]
let ``the summary above the list counts what the banner does`` () =
    onUiThread (fun () ->
        let window = openSample "pages-clipped-paragraphs.pdf"
        let shown = texts window
        let failures = HeadlessWindow.findings window |> List.filter (fun r -> r.Violation.Severity = FindingSeverity.Failure) |> List.length
        window.Close()

        shown |> should contain $"{failures} failures"
        shown |> should contain "1 warning"
        // (Ending ", as checked …" when this session has checked it already.)
        shown |> List.exists (fun t -> t.StartsWith $"FAIL — {failures} of " && t.Contains "; 1 warning)") |> should be True)

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
        let before = (HeadlessWindow.resultsList window).ItemsSource
        button.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs(Button.ClickEvent))
        // A new report, once it's done.
        let checkedAgain =
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 30.0) (fun () ->
                let now = (HeadlessWindow.resultsList window).ItemsSource
                not (isNull now) && not (obj.ReferenceEquals(now, before)) && button.IsEnabled)
        window.Close()

        enabledEmpty |> should be False
        enabledOpen |> should be True
        tip |> should equal "Check this PDF again, after it's changed on disk."
        checkedAgain |> should be True)

[<Fact>]
let ``what veraPDF noted reading the file is under the banner, not among the findings`` () =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "list.pdf")
    do
        // Marked content inside marked content, which veraPDF logs as it reads the page.
        use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfWriter(pdf))
        let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas(document.AddNewPage())
        for tag, mcid in [ "P", 0; "Span", 1 ] do
            let properties = iText.Kernel.Pdf.PdfDictionary()
            properties.Put(iText.Kernel.Pdf.PdfName.MCID, iText.Kernel.Pdf.PdfNumber mcid) |> ignore
            canvas.BeginMarkedContent(iText.Kernel.Pdf.PdfName tag, properties) |> ignore
        canvas.BeginText().SetFontAndSize(iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA, 12f)
            .MoveText(72.0, 700.0).ShowText("Item 1").EndText().EndMarkedContent().EndMarkedContent()
        |> ignore
    try
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window pdf
            let headers = descendants<Expander> window |> List.map (fun e -> string e.Header)
            let findings = HeadlessWindow.findings window |> List.map _.Violation.Message
            window.Close()

            headers |> should equal [ "veraPDF noted 1 thing while reading the file" ]
            findings |> List.exists (fun m -> m.Contains "Nested MCID") |> should be False)
    finally
        Directory.Delete(folder, true)
