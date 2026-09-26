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
open Legibyl.Validation
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

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private box (l, b, r, t) : StructureGeometry = { PageNumber = 1; Left = l; Bottom = b; Right = r; Top = t }

let private block order tag bounds text : SuggestedBlock =
    { Order = order; Label = "text"; Tag = tag; Score = 0.9; Geometry = box bounds; Text = text
      ListNumbering = None; Recognized = false; QrText = None }

/// A review of pdf with its first page's layout found already (so no model is needed): a
/// heading and a paragraph with a link in it.
let private reviewOf (pdf: string) =
    let review = LayoutReview(File.GetLastWriteTimeUtc pdf)
    review.Pages[1] <-
        { PageNumber = 1
          Blocks = [ block 1 "H" (72.0, 715.0, 300.0, 735.0) "Hello"; block 2 "P" (72.0, 600.0, 400.0, 700.0) "Apply online today." ]
          HasText = true
          Rotated = false }
    review.WebLinks <- [ { Page = 1; Box = box (100.0, 640.0, 200.0, 655.0); Address = "https://example.com/apply" } ]
    review

let private show (pdf: string) (review: LayoutReview) =
    let window = StructureTreeWindow(Suggested review, pdf, ignore)
    window.Show()
    pump ()
    window

let private texts (root: Visual) = descendants<TextBlock> root |> List.map (fun t -> string t.Text)

let private blockFor (window: Window) (tag: string) =
    descendants<Border> window |> List.find (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = tag | _ -> false)

/// Clicks a block, selecting it.
let private select (block: Border) =
    let properties = Input.PointerPointProperties(Input.RawInputModifiers.LeftMouseButton, Input.PointerUpdateKind.LeftButtonPressed)
    block.RaiseEvent(
        Input.PointerPressedEventArgs(block, new Input.Pointer(0, Input.PointerType.Mouse, true), block, Point(4.0, 4.0), 0UL, properties, Input.KeyModifiers.None)
    )
    pump ()

[<Fact>]
let ``a page's suggestions are elements: list items in a list, links in the region they're in`` () =
    let review = LayoutReview(DateTime.UtcNow)
    review.WebLinks <- [ { Page = 1; Box = box (100.0, 640.0, 200.0, 655.0); Address = "https://example.com/apply" } ]
    review.Titles <- Map [ "https://example.com/apply", "Apply" ]
    let page =
        { PageNumber = 1
          Blocks =
            [ block 1 "P" (72.0, 600.0, 400.0, 700.0) "Apply online today."
              block 2 "LI" (72.0, 560.0, 400.0, 580.0) "One"
              block 3 "LI" (72.0, 540.0, 400.0, 558.0) "Two"
              { block 4 "Figure" (72.0, 300.0, 216.0, 444.0) "" with QrText = Some "https://example.com/apply" } ]
          HasText = true
          Rotated = false }

    let nodes = SuggestedTree.pageNodes review page

    nodes |> List.map _.Tag |> should equal [ "P"; "L"; "Figure" ]
    [ for item in nodes[1].Children -> item.Content ] |> should equal [ "One"; "Two" ]
    let link = nodes[0].Children |> Seq.exactlyOne
    // Named by its page's title, and where it is.
    (link.Tag, link.LinkUri, link.LinkDescription) |> should equal ("Link", "https://example.com/apply", "Apply (example.com)")
    nodes[2].AlternateDescription |> should haveSubstring "https://example.com/apply"

[<Fact>]
let ``an untagged PDF's structure offers to suggest one, shown in View Structure`` () =
    withUntaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window pdf
            click (descendants<Button> window |> List.find (fun b -> Automation.AutomationProperties.GetName b = "Structure…"))
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 20.0) (fun () -> window.OwnedWindows.Count = 1) |> ignore
            let dialog = window.OwnedWindows |> Seq.exactlyOne
            click (button dialog "Suggest Structure…")
            let suggested () = window.OwnedWindows |> Seq.filter (fun w -> w :? StructureTreeWindow) |> List.ofSeq
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 5.0) (fun () -> not (suggested ()).IsEmpty) |> ignore
            let opened = suggested () |> List.map _.Title
            for owned in List.ofSeq window.OwnedWindows do owned.Close()
            window.Close()
            opened |> should equal [ "Suggested Structure" ]))

[<Fact>]
let ``the suggestions show as elements, said not to be tagged yet`` () =
    withUntaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let window = show pdf (reviewOf pdf)
            let shown = texts window
            let tags =
                descendants<Border> window |> List.choose (fun b -> match b.Tag with :? StructureNode as n -> Some n.Tag | _ -> None)
            window.Close()

            shown |> should contain "Not tagged yet: this is the structure suggested for it"
            shown |> List.exists (fun t -> t.StartsWith "Every page's layout found.") |> should be True
            tags |> should equal [ "Document"; "H"; "P"; "Link" ]))

[<Fact>]
let ``a region can be left out and kept again, as an edit tagging takes`` () =
    withUntaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let review = reviewOf pdf
            let window = show pdf review
            select (blockFor window "H")
            click (button window "Leave Out")
            pump ()
            let leftOut = texts window
            let saved = File.ReadAllText(pdf + ".legibyl.json")
            use suggester = new LayoutSuggester(pdf)
            let regions = (ReviewedLayout(LayoutPrefetcher(suggester, review.Pages), review) :> IPageLayoutSource).PageRegions(pdf, 1)
            let tagged = [ for region in regions -> region.Tag ]
            click (button window "Keep")
            pump ()
            let kept = review.Edits
            window.Close()

            leftOut |> should contain "✎ left out: not tagged (its text joins the nearest region kept)"
            // With the layout the edit is on.
            saved |> should haveSubstring "\"exclude\": true"
            saved |> should haveSubstring "\"layout\""
            tagged |> should equal [ "P" ]
            kept |> List.exists _.Exclude |> should be False))

[<Fact>]
let ``a region can be given any suggested tag, which is kept and tagged`` () =
    withUntaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let review = reviewOf pdf
            let window = show pdf review
            select (blockFor window "P")
            let picker = descendants<ComboBox> window |> List.exactlyOne
            let choices = picker.Items |> Seq.map string |> List.ofSeq
            picker.SelectedItem <- "Note"
            pump ()
            let shown = texts window
            window.Close()

            choices |> should equal LayoutFile.tags
            shown |> should contain "✎ your tag: \"Note\""
            // An edit on the suggestion, which stays as found; a new session has it.
            let later = LayoutReviews().For pdf
            later.Pages[1].Blocks[1].Tag |> should equal "P"
            later.Edits |> List.map (fun e -> e.Tag, e.Retag) |> should equal [ "P", Some "Note" ]))

[<Fact>]
let ``a web link's description is an edit, which tagging writes`` () =
    withUntaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let review = reviewOf pdf
            let window = show pdf review
            let link = blockFor window "Link"
            let item = link.ContextMenu |> nonNull |> _.Items |> Seq.cast<MenuItem> |> Seq.exactlyOne
            item.RaiseEvent(RoutedEventArgs MenuItem.ClickEvent)
            pump ()
            let prompt = window.OwnedWindows |> Seq.exactlyOne
            (descendants<TextBox> prompt |> List.exactlyOne).Text <- "Apply for membership"
            pump ()
            click (button prompt "OK")
            pump ()
            let shown = texts window
            window.Close()

            string item.Header |> should equal "Edit Link Description…"
            shown |> should contain "✎ your description: \"Apply for membership\""
            review.LinkDescription review.WebLinks.Head |> should equal (Some "Apply for membership")))

[<Fact>]
let ``reopening keeps what was found and left out`` () =
    withUntaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let reviews = LayoutReviews()
            let review = reviews.For pdf
            for KeyValue(page, found) in reviewOf pdf |> _.Pages do review.Pages[page] <- found
            let window = show pdf review
            select (blockFor window "H")
            click (button window "Leave Out")
            window.Close()

            // A new session: the layout and the edit come back from the file next to the PDF.
            let later = LayoutReviews().For pdf
            later.Pages[1].Blocks |> List.map _.Tag |> should equal [ "H"; "P" ]
            later.Edits |> List.filter _.Exclude |> List.length |> should equal 1))

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
            let window = show pdf (reviews.For pdf)
            let titleBox () = descendants<TextBox> window |> List.find (fun b -> b.PlaceholderText = "What a screen reader and the title bar call this document")
            let prefilled = titleBox().Text
            let savedAtFirst = File.Exists(pdf + ".legibyl.json")
            titleBox().Text <- "Cub Scout Pack 721"
            pump ()
            let saved = File.ReadAllText(pdf + ".legibyl.json")
            titleBox().Text <- "Design"
            pump ()
            let removed = not (File.Exists(pdf + ".legibyl.json"))
            window.Close()

            prefilled |> should equal "Design"
            savedAtFirst |> should be False
            saved |> should haveSubstring "\"title\": \"Cub Scout Pack 721\""
            removed |> should be True
            (reviews.For pdf).DocumentTitle |> should equal (Some "Design"))
    finally
        Directory.Delete(folder, true)

[<Fact>]
let ``the detector's regions are found in the background and fill in the structure`` () =
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
            let window = show pdf (LayoutReview(File.GetLastWriteTimeUtc pdf))
            let allFound () = texts window |> List.exists (fun t -> t.StartsWith "Every page's layout found.")
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 90.0) allFound |> ignore
            let found = allFound ()
            let pages =
                descendants<Border> window
                |> List.choose (fun b -> match b.Tag with :? StructureNode as n when n.PageNumber.HasValue -> Some n.PageNumber.Value | _ -> None)
                |> List.distinct
            let tags = descendants<Border> window |> List.choose (fun b -> match b.Tag with :? StructureNode as n -> Some n.Tag | _ -> None)
            let preview = descendants<Image> window |> List.exists (fun image -> not (isNull image.Source))
            window.Close()

            found |> should be True
            pages |> should equal [ 1; 2 ]
            tags |> List.forall (fun tag -> tag = "Document" || tag = "L" || tag = "Link" || List.contains tag LayoutFile.tags) |> should be True
            preview |> should be True)
    finally
        Directory.Delete(folder, true)

[<Fact>]
let ``the reviewed layout leaves out discarded suggestions`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    use suggester = new LayoutSuggester(Path.Combine(AppContext.BaseDirectory, "SampleCorpus", "pdfua1-pass.pdf"))
    let prefetcher = LayoutPrefetcher(suggester, Collections.Concurrent.ConcurrentDictionary())
    let all = (prefetcher.Find 1).Blocks
    let review = LayoutReview(DateTime.UtcNow, Edits = RegionEdits.setExcluded [] 1 all.Head.Tag all.Head.Geometry true)
    let reviewed = ReviewedLayout(prefetcher, review) :> Legibyl.Validation.IPageLayoutSource
    let regions = reviewed.PageRegions("unused.pdf", 1)
    regions.Count |> should equal (all.Length - 1)
