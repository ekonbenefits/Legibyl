module Legibyl.Gui.Tests.SuggestedStructureTests

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Font
open iText.IO.Font.Constants
open Legibyl.Gui
open Legibyl.Layout
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private button (root: Visual) (text: string) =
    descendants<Button> root |> List.find (fun b -> string b.Content = text)

let private click (b: Button) = b.RaiseEvent(RoutedEventArgs(Button.ClickEvent))

/// An untagged one-page PDF with a line of text, in its own temporary folder.
let private withUntaggedPdf (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let path = Path.Combine(folder, "untagged.pdf")
    do
        use document = new PdfDocument(new PdfWriter(path))
        let canvas = PdfCanvas(document.AddNewPage())
        canvas.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f).MoveText(72.0, 720.0).ShowText("Hello").EndText() |> ignore
    try test path finally Directory.Delete(folder, true)

[<Fact>]
let ``an untagged PDF's structure offers to suggest one`` () =
    withUntaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window pdf
            click (button window "View Structure…")
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 20.0) (fun () -> window.OwnedWindows.Count = 1) |> ignore
            let dialog = window.OwnedWindows |> Seq.exactlyOne
            click (button dialog "Suggest Structure…")
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 5.0) (fun () ->
                window.OwnedWindows |> Seq.exists (fun w -> w :? SuggestedStructureWindow))
            |> ignore
            let opened = window.OwnedWindows |> Seq.filter (fun w -> w :? SuggestedStructureWindow) |> List.ofSeq
            for owned in List.ofSeq window.OwnedWindows do owned.Close()
            window.Close()
            opened |> should haveLength 1))

[<Fact>]
let ``the suggested structure lists each region the detector finds`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    onUiThread (fun () ->
        let window = SuggestedStructureWindow(Path.Combine(AppContext.BaseDirectory, "SampleCorpus", "pdfua1-pass.pdf"))
        window.Show()
        let blocks () =
            descendants<Button> window |> List.filter (fun b -> match b.Content with :? DockPanel -> true | _ -> false)
        let images () = descendants<Image> window |> List.filter (fun image -> not (isNull image.Source))
        HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 60.0) (fun () -> not (blocks().IsEmpty) && not (images().IsEmpty)) |> ignore
        let found = blocks().Length
        let texts = descendants<TextBlock> window |> List.map (fun t -> string t.Text)
        window.Close()
        found |> should be (greaterThan 0)
        texts |> List.exists (fun t -> t.StartsWith "Page 1 of 1") |> should be True
        texts |> List.exists (fun t -> t.StartsWith " 1 ") |> should be True)

[<Fact>]
let ``every page's layout is found in the background, so the next page shows at once`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "two-pages.pdf")
    do
        use document = new PdfDocument(new PdfWriter(pdf))
        let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
        for title in [ "First page"; "Second page" ] do
            let canvas = PdfCanvas(document.AddNewPage())
            canvas.BeginText().SetFontAndSize(font, 24f).MoveText(72.0, 700.0).ShowText(title).EndText() |> ignore
            for line in 0 .. 5 do
                canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 640.0 - 16.0 * float line)
                    .ShowText("A paragraph of ordinary text, long enough to read as a block of body copy.").EndText()
                |> ignore
    try
        onUiThread (fun () ->
            let window = SuggestedStructureWindow(pdf)
            window.Show()
            let texts () = descendants<TextBlock> window |> List.map (fun t -> string t.Text)
            let allFound () = texts () |> List.exists (fun t -> t.EndsWith "every page's layout found")
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 60.0) allFound |> ignore
            let foundBeforeMoving = allFound ()

            click (button window "Next ▶")
            HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
            let afterMoving = texts ()
            window.Close()

            foundBeforeMoving |> should be True
            afterMoving |> List.exists (fun t -> t.StartsWith "Page 2 of 2") |> should be True
            afterMoving |> List.exists (fun t -> t.StartsWith "Finding the layout") |> should be False)
    finally
        Directory.Delete(folder, true)

[<Fact>]
let ``a QR code's alt text is prefilled with where it links, to edit`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "qr.pdf")
    do
        use document = new PdfDocument(new PdfWriter(pdf))
        let canvas = PdfCanvas(document.AddNewPage())
        canvas.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 18f).MoveText(72.0, 720.0).ShowText("Scan to apply").EndText()
        |> ignore
        // A 2-inch code, a point per module, drawn as squares.
        let matrix = ZXing.QrCode.QRCodeWriter().encode("https://example.com/apply", ZXing.BarcodeFormat.QR_CODE, 144, 144)
        for row in 0 .. matrix.Height - 1 do
            for column in 0 .. matrix.Width - 1 do
                if matrix[column, row] then canvas.Rectangle(72.0 + float column, 680.0 - float row, 1.0, 1.0) |> ignore
        canvas.Fill() |> ignore
    try
        onUiThread (fun () ->
            let window = SuggestedStructureWindow(pdf)
            window.Show()
            let boxes () = descendants<TextBox> window |> List.map (fun t -> string t.Text)
            let alt = "QR code linking to https://example.com/apply"
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 60.0) (fun () -> boxes () |> List.contains alt) |> ignore
            let shown = boxes ()
            // Reading titles is offered before the window opens, not in it.
            let checkbox = descendants<CheckBox> window |> List.tryFind (fun c -> string c.Content = "Read link titles from the web")
            window.Close()

            shown |> should contain alt
            checkbox |> should equal None)
    finally
        Directory.Delete(folder, true)

[<Fact>]
let ``suggestions can be discarded and kept again, and discarded ones aren't tagged`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    let pdf = Path.Combine(AppContext.BaseDirectory, "SampleCorpus", "pdfua1-pass.pdf")
    onUiThread (fun () ->
        let window = SuggestedStructureWindow(pdf)
        window.Show()
        let buttons () = descendants<Button> window
        let toggles () = buttons () |> List.filter (fun b -> string b.Content = "Discard" || string b.Content = "Keep")
        HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 60.0) (fun () -> not (toggles().IsEmpty)) |> ignore
        let count = toggles().Length

        click (toggles().Head)
        HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
        let afterDiscarding = toggles () |> List.map (fun b -> string b.Content)
        click (button window "Keep all")
        HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
        let afterKeepingAll = toggles () |> List.map (fun b -> string b.Content)
        click (button window "Discard all")
        HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
        let afterDiscardingAll = toggles () |> List.map (fun b -> string b.Content)
        window.Close()

        count |> should be (greaterThan 0)
        afterDiscarding |> should equal ("Keep" :: List.replicate (count - 1) "Discard")
        afterKeepingAll |> should equal (List.replicate count "Discard")
        afterDiscardingAll |> should equal (List.replicate count "Keep"))

[<Fact>]
let ``the reviewed layout leaves out discarded suggestions`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    use suggester = new LayoutSuggester(Path.Combine(AppContext.BaseDirectory, "SampleCorpus", "pdfua1-pass.pdf"))
    let prefetcher = LayoutPrefetcher(suggester, Collections.Concurrent.ConcurrentDictionary())
    let all = (prefetcher.Find 1).Blocks
    let review = LayoutReview(DateTime.UtcNow, Discarded = set [ 1, all.Head.Order ])
    let reviewed = ReviewedLayout(prefetcher, review) :> Legibyl.Validation.IPageLayoutSource
    let regions = reviewed.PageRegions("unused.pdf", 1)
    regions.Count |> should equal (all.Length - 1)

[<Fact>]
let ``reopening Suggest Structure keeps what was found and discarded`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    let pdf = Path.Combine(AppContext.BaseDirectory, "SampleCorpus", "pdfua1-pass.pdf")
    let reviews = LayoutReviews()
    onUiThread (fun () ->
        let toggles (window: Window) =
            descendants<Button> window |> List.filter (fun b -> string b.Content = "Discard" || string b.Content = "Keep")
        let first = SuggestedStructureWindow(pdf, reviews.For pdf)
        first.Show()
        HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 60.0) (fun () -> not (toggles first).IsEmpty) |> ignore
        click (toggles first).Head
        HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
        first.Close()

        let reopened = SuggestedStructureWindow(pdf, reviews.For pdf)
        reopened.Show()
        // Already found: shown without waiting for the detector.
        HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 2.0) (fun () -> not (toggles reopened).IsEmpty) |> ignore
        let restored = toggles reopened |> List.map (fun b -> string b.Content)
        reopened.Close()

        restored |> should not' (be Empty)
        restored.Head |> should equal "Keep"
        restored.Tail |> List.forall ((=) "Discard") |> should be True
        (reviews.For pdf).Discarded.Count |> should equal 1)

/// A PDF with a heading and a QR code, which the detector finds as a figure.
let private createQrPdf (pdf: string) =
    use document = new PdfDocument(new PdfWriter(pdf))
    let canvas = PdfCanvas(document.AddNewPage())
    canvas.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 18f).MoveText(72.0, 720.0).ShowText("Scan to apply").EndText()
    |> ignore
    let matrix = ZXing.QrCode.QRCodeWriter().encode("https://example.com/apply", ZXing.BarcodeFormat.QR_CODE, 144, 144)
    for row in 0 .. matrix.Height - 1 do
        for column in 0 .. matrix.Width - 1 do
            if matrix[column, row] then canvas.Rectangle(72.0 + float column, 680.0 - float row, 1.0, 1.0) |> ignore
    canvas.Fill() |> ignore

[<Fact>]
let ``alt text typed for a figure is saved next to the PDF, filled in again later, and tagged`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "qr.pdf")
    createQrPdf pdf
    let altBoxes (window: Window) =
        descendants<TextBox> window |> List.filter (fun b -> b.PlaceholderText = "Alt text: what this figure shows, for someone who can't see it")
    try
        onUiThread (fun () ->
            let first = SuggestedStructureWindow(pdf, LayoutReviews().For pdf)
            first.Show()
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 60.0) (fun () -> not (altBoxes first).IsEmpty) |> ignore
            (altBoxes first).Head.Text <- "Scan to apply online"
            HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
            first.Close()

            // A new session: the alt text comes back from the file next to the PDF.
            let review = LayoutReviews().For pdf
            let reopened = SuggestedStructureWindow(pdf, review)
            reopened.Show()
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 60.0) (fun () -> not (altBoxes reopened).IsEmpty) |> ignore
            let filled = (altBoxes reopened).Head.Text
            reopened.Close()

            File.ReadAllText(pdf + ".legibyl.json") |> should haveSubstring "Scan to apply online"
            filled |> should equal "Scan to apply online"
            use suggester = new LayoutSuggester(pdf)
            let regions = (ReviewedLayout(LayoutPrefetcher(suggester, review.Pages), review) :> Legibyl.Validation.IPageLayoutSource).PageRegions(pdf, 1)
            regions |> Seq.choose (fun r -> Option.ofObj r.Alt) |> List.ofSeq |> should equal [ "Scan to apply online" ])
    finally
        Directory.Delete(folder, true)

[<Fact>]
let ``the document title is prefilled from the document, and only a change is saved`` () =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "flyer.pdf")
    do
        use document = new PdfDocument(new PdfWriter(pdf))
        document.AddNewPage() |> ignore
        document.GetDocumentInfo().SetTitle "Design" |> ignore
    try
        onUiThread (fun () ->
            let reviews = LayoutReviews()
            let window = SuggestedStructureWindow(pdf, reviews.For pdf)
            window.Show()
            HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
            let titleBox () = descendants<TextBox> window |> List.find (fun b -> b.PlaceholderText = "What a screen reader and the title bar call this document")
            let prefilled = titleBox().Text
            let savedAtFirst = File.Exists(pdf + ".legibyl.json")
            titleBox().Text <- "Cub Scout Pack 721"
            HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
            let saved = File.ReadAllText(pdf + ".legibyl.json")
            titleBox().Text <- "Design"
            HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
            let removed = not (File.Exists(pdf + ".legibyl.json"))
            window.Close()

            prefilled |> should equal "Design"
            savedAtFirst |> should be False
            saved |> should haveSubstring "\"title\": \"Cub Scout Pack 721\""
            removed |> should be True
            (reviews.For pdf).DocumentTitle |> should equal (Some "Design"))
    finally
        Directory.Delete(folder, true)
