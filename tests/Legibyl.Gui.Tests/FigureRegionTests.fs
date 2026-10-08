module Legibyl.Gui.Tests.FigureRegionTests

open System
open System.IO
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Media.Imaging
open Avalonia.Headless
open Avalonia.Input
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Tagging
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Visual> (window: Window) =
    window.GetVisualDescendants() |> Seq.choose (function :? 'T as control -> Some control | _ -> None) |> List.ofSeq

let private click (window: Window) content =
    let button = descendants<Button> window |> List.find (fun b -> string b.Content = content)
    button.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs(Button.ClickEvent))
    HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private wait predicate =
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 15.0) predicate |> should be True

let private create (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    let page = document.AddNewPage PageSize.LETTER
    let top = PdfStructElem(document, PdfName.Document)
    document.GetStructTreeRoot().AddKid top |> ignore
    let paragraph = PdfStructElem(document, PdfName.P, page)
    top.AddKid paragraph |> ignore
    let mcr = PdfMcrNumber(page, paragraph)
    paragraph.AddKid mcr |> ignore
    let properties = PdfDictionary()
    properties.Put(PdfName.MCID, PdfNumber(mcr.GetMcid())) |> ignore
    let canvas = PdfCanvas page
    canvas.BeginMarkedContent(PdfName.P, properties).BeginText()
        .SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f)
        .MoveText(75.0, 610.0).ShowText("Chart label").EndText().EndMarkedContent() |> ignore
    canvas.BeginMarkedContent(PdfName.Artifact).Rectangle(75.0, 500.0, 80.0, 80.0).Fill().EndMarkedContent() |> ignore

/// Where (x, y) on the page, in PDF points, is on the window, through the preview's current layout.
let private toWindow (image: Image) (x: float) (y: float) =
    let bitmap = nonNull image.Source :?> Bitmap
    let window = TopLevel.GetTopLevel image |> nonNull
    (image.TranslatePoint(StructurePreview.pageToPreview image.Bounds.Size bitmap (x, y), window)).Value

/// The page point under a window point, as the preview maps it now.
let private onPage (image: Image) (point: Avalonia.Point) =
    let bitmap = nonNull image.Source :?> Bitmap
    let window = TopLevel.GetTopLevel image |> nonNull
    StructurePreview.pointOnPage image bitmap (window.TranslatePoint(point, image)).Value |> Option.get

/// Lets the window lay out and render, as it would between real pointer events.
let private pump () =
    HeadlessWindow.pumpUntil (TimeSpan.FromMilliseconds 100.0) (fun () -> false) |> ignore

/// The figure region's outline: the white rule and the dashes over it.
let private outline (window: Window) =
    descendants<Shapes.Rectangle> window |> List.filter (fun r -> string r.Tag = "FigureRegionOutline")

/// The dashed outline's rectangle on the window, along the middle of its stroke.
let private dashedOnWindow (window: Window) =
    let dashed = outline window |> List.filter (fun r -> match r.StrokeDashArray with NonNull dashes -> dashes.Count > 0 | Null -> false) |> List.exactlyOne
    let half = dashed.StrokeThickness / 2.0
    let topLeft = (dashed.TranslatePoint(Avalonia.Point(half, half), window)).Value
    let bottomRight = (dashed.TranslatePoint(Avalonia.Point(dashed.Bounds.Width - half, dashed.Bounds.Height - half), window)).Value
    dashed, Avalonia.Rect(topLeft, bottomRight)

let private near (expected: float) (actual: float) = abs (expected - actual) |> should be (lessThan 0.01)

let private withPdf test =
    let path = Path.GetFullPath($"figure-ui-{Guid.NewGuid():N}.pdf")
    try
        create path
        test path
    finally
        for file in [path; ReviewFile.filePath path] do
            if File.Exists file then File.Delete file

[<Fact>]
let ``figure region reviews save reopen and remove without changing the source PDF`` () =
    withPdf (fun path ->
        let bytes = File.ReadAllBytes path
        let review = DocumentReviews().For path
        let box: StructureGeometry = { PageNumber = 1; Left = 60.0; Bottom = 480.0; Right = 180.0; Top = 650.0 }
        review.Edits <- RegionEdits.replaceWithFigure [] box "A rising bar."
        review.Save path
        let reopened = DocumentReviews().For path
        reopened.Edits |> should equal review.Edits
        let cliEdits = ReviewFile.readFor path |> Option.get |> ReviewFile.edits
        cliEdits.Head.Tag |> should equal "FigureRegion"
        cliEdits.Head.Box |> should equal box
        cliEdits.Head.Alt |> should equal "A rising bar."
        reopened.Edits <- []
        reopened.Save path
        (DocumentReviews().For path).Edits |> should be Empty
        File.ReadAllBytes path |> should equal bytes)

[<Fact>]
let ``drawing a figure region previews asks for alt confirms and allows removal`` () =
    withPdf (fun path ->
        let root = VeraPdfValidator().ReadStructureTree path |> nonNull
        onUiThread (fun () ->
            let review = DocumentReviews().For path
            let window = StructureTreeWindow(Tagged root, path, review, ignore)
            try
                window.Show()
                wait (fun () -> descendants<Image> window |> List.exists (fun image -> not (isNull image.Source)))
                click window "Replace Structure with Figure from Region…"
                descendants<Button> window |> List.exists (fun b -> string b.Content = "Cancel Drawing Figure Region") |> should be True
                let image = descendants<Image> window |> List.find (fun i -> not (isNull i.Source))
                let toWindow = toWindow image
                window.MouseDown(toWindow 60.0 480.0, MouseButton.Left)
                window.MouseMove(toWindow 180.0 650.0)
                window.MouseUp(toWindow 180.0 650.0, MouseButton.Left)
                wait (fun () -> window.OwnedWindows.Count = 1)
                let prompt = window.OwnedWindows |> Seq.exactlyOne
                descendants<TextBlock> prompt |> List.exists (fun t -> (string t.Text).Contains "Chart label") |> should be True
                descendants<TextBlock> prompt |> List.exists (fun t -> (string t.Text).Contains "most of it is inside") |> should be True
                let text = descendants<TextBox> prompt |> List.exactlyOne
                text.Text <- "A rising bar."
                HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
                click prompt "OK"
                wait (fun () -> window.OwnedWindows.Count = 1 && (window.OwnedWindows |> Seq.head |> descendants<Button> |> List.exists (fun b -> string b.Content = "Make It a Figure")))
                review.Edits |> should be Empty
                click (window.OwnedWindows |> Seq.exactlyOne) "Make It a Figure"
                wait (fun () -> review.Edits.Length = 1)
                review.Edits.Head.Tag |> should equal "FigureRegion"
                review.Edits.Head.Alt |> should equal (Some "A rising bar.")
                // The generic Edits tab supplies undo for every pending operation.
                let tab = descendants<TabItem> window |> List.find (fun t -> (string t.Header).StartsWith "Edits")
                tab.IsSelected <- true
                HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
                click window "Remove"
                review.Edits |> should be Empty
            finally
                for owned in List.ofSeq window.OwnedWindows do owned.Close()
                window.Close()))

[<Fact>]
let ``with the keyboard, selected elements are made a Figure the same way`` () =
    withPdf (fun path ->
        let root = VeraPdfValidator().ReadStructureTree path |> nonNull
        onUiThread (fun () ->
            let review = DocumentReviews().For path
            let window = StructureTreeWindow(Tagged root, path, review, ignore)
            try
                window.Show()
                wait (fun () -> descendants<Image> window |> List.exists (fun image -> not (isNull image.Source)))
                // The chart's group (here the Document round its label), selected as Enter on it does:
                // one element holding others. (One paragraph alone isn't offered it.)
                let block =
                    descendants<Border> window
                    |> List.find (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "Document" | _ -> false)
                let label = block.Tag :?> StructureNode
                block.Focus() |> ignore
                block.RaiseEvent(KeyEventArgs(RoutedEvent = InputElement.KeyDownEvent, Key = Key.Enter, Source = block))
                HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
                // An action button, found by its name (its content is an icon and a label).
                let action =
                    descendants<Button> window |> List.find (fun b -> string (AutomationProperties.GetName b) = "Replace with Figure…")
                action.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs(Button.ClickEvent))
                wait (fun () -> window.OwnedWindows.Count = 1)
                let prompt = window.OwnedWindows |> Seq.exactlyOne
                let text = descendants<TextBox> prompt |> List.exactlyOne
                text.Text <- "A rising bar."
                HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
                click prompt "OK"
                wait (fun () -> window.OwnedWindows.Count = 1 && (window.OwnedWindows |> Seq.head |> descendants<Button> |> List.exists (fun b -> string b.Content = "Make It a Figure")))
                click (window.OwnedWindows |> Seq.exactlyOne) "Make It a Figure"
                wait (fun () -> review.Edits.Length = 1)
                review.Edits.Head.Tag |> should equal "FigureRegion"
                // The region is round what the selected element draws.
                review.Edits.Head.Box |> should equal (nonNull label.Geometry)
            finally
                for owned in List.ofSeq window.OwnedWindows do owned.Close()
                window.Close()))

[<Fact>]
let ``a refused saved region still enables Export so its refusal is reported`` () =
    withPdf (fun path ->
        let review = DocumentReviews().For path
        review.Edits <- RegionEdits.replaceWithFigure [] { PageNumber = 1; Left = 400.0; Bottom = 300.0; Right = 500.0; Top = 400.0 } "A rising bar."
        review.Save path
        ElementEdits.pending path (RegionEdits.toElementEdits review.Edits) |> should equal 0
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            try
                HeadlessWindow.openPdf window path
                let export () =
                    descendants<Button> window
                    |> List.tryFind (fun b -> (string (AutomationProperties.GetName b)).StartsWith "Export…" || string b.Content = "Export…")
                wait (fun () -> export () |> Option.exists _.IsEnabled)
                (export () |> Option.get).IsEnabled |> should be True
            finally window.Close()))

[<Theory>]
[<InlineData(60.0, 650.0, 180.0, 480.0)>]
[<InlineData(180.0, 480.0, 60.0, 650.0)>]
[<InlineData(180.0, 650.0, 60.0, 480.0)>]
let ``drawing a figure region on a wide preview keeps the page still and outlines the drag dashed`` (x1: float, y1: float, x2: float, y2: float) =
    withPdf (fun path ->
        let root = VeraPdfValidator().ReadStructureTree path |> nonNull
        onUiThread (fun () ->
            let review = DocumentReviews().For path
            // Wide and tall: the page, scaled to fit, is centred with space either side of it.
            let window = StructureTreeWindow(Tagged root, path, review, ignore, Width = 2400.0, Height = 1400.0)
            try
                window.Show()
                wait (fun () -> descendants<Image> window |> List.exists (fun image -> not (isNull image.Source)))
                click window "Replace Structure with Figure from Region…"
                pump ()
                let image = descendants<Image> window |> List.find (fun i -> not (isNull i.Source))
                let frame = image.GetVisualAncestors() |> Seq.pick (function :? Border as b -> Some b | _ -> None)
                // The image doesn't start at its frame's edge (the case the drag got wrong).
                (image.TranslatePoint(Avalonia.Point(), frame)).Value.X |> should be (greaterThan 20.0)
                let layout () = (image.TranslatePoint(Avalonia.Point(), window)).Value, image.Bounds.Size
                let before = layout ()
                let start = toWindow image x1 y1
                window.MouseDown(start, MouseButton.Left)
                pump ()
                // Dragging through the middle, then to the far corner: each step laid out.
                for x, y in [ (x1 + x2) / 2.0, (y1 + y2) / 2.0; x2, y2 ] do
                    let cursor = toWindow image x y
                    window.MouseMove(cursor, RawInputModifiers.LeftMouseButton)
                    pump ()
                    layout () |> should equal before
                    let dashed, drawn = dashedOnWindow window
                    dashed.IsHitTestVisible |> should be False
                    isNull dashed.Fill |> should be True
                    near (min start.X cursor.X) drawn.Left
                    near (min start.Y cursor.Y) drawn.Top
                    near (max start.X cursor.X) drawn.Right
                    near (max start.Y cursor.Y) drawn.Bottom
                let finish = toWindow image x2 y2
                let expected =
                    let ax, ay = onPage image start
                    let bx, by = onPage image finish
                    { PageNumber = 1; Left = min ax bx; Bottom = min ay by; Right = max ax bx; Top = max ay by }
                for actual, wanted in [ expected.Left, min x1 x2; expected.Bottom, min y1 y2; expected.Right, max x1 x2; expected.Top, max y1 y2 ] do
                    abs (actual - wanted) |> should be (lessThan 1e-6)
                window.MouseUp(finish, MouseButton.Left)
                pump ()
                wait (fun () -> window.OwnedWindows.Count = 1)
                pump ()
                // Confirming: the same dashed outline, still where the drag ended, on the same layout.
                layout () |> should equal before
                let _, confirming = dashedOnWindow window
                near (min start.X finish.X) confirming.Left
                near (min start.Y finish.Y) confirming.Top
                near (max start.X finish.X) confirming.Right
                near (max start.Y finish.Y) confirming.Bottom
                let prompt = window.OwnedWindows |> Seq.exactlyOne
                (descendants<TextBox> prompt |> List.exactlyOne).Text <- "A rising bar."
                HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
                click prompt "OK"
                wait (fun () -> window.OwnedWindows.Count = 1 && (window.OwnedWindows |> Seq.head |> descendants<Button> |> List.exists (fun b -> string b.Content = "Make It a Figure")))
                (dashedOnWindow window |> fst).IsVisible |> should be True
                click (window.OwnedWindows |> Seq.exactlyOne) "Make It a Figure"
                wait (fun () -> review.Edits.Length = 1)
                review.Edits.Head.Tag |> should equal "FigureRegion"
                review.Edits.Head.Box |> should equal expected
                pump ()
                outline window |> should be Empty
            finally
                for owned in List.ofSeq window.OwnedWindows do owned.Close()
                window.Close()))
