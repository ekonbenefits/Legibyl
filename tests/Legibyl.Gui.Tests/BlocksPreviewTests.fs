/// The Blocks tab's page preview: clicking an element on it selects its block.
module Legibyl.Gui.Tests.BlocksPreviewTests

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private rect left bottom right top : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

let private node (tag: string) (geometry: StructureGeometry | null) (children: StructureNode list) =
    { Tag = tag
      Role = null
      AlternateDescription = null
      ActualText = null
      Content = if children.IsEmpty then $"{tag} text" else null
      FormFieldName = null
      FormFieldDescription = null
      FormFieldType = null
      FormFieldValue = null
      LinkUri = null
      LinkDestinationPageNumber = Nullable()
      LinkDestinationDescription = null
      LinkDestination = null
      LinkDescription = null
      Geometry = geometry
      Language = null
      PageNumber = (match geometry with NonNull g -> Nullable g.PageNumber | Null -> Nullable())
      ContentLeafCount = 1
      ColSpan = 1
      RowSpan = 1
      Children = List.toArray children }

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

/// A blank one-page PDF (US Letter) in a fresh folder: the preview renders its page, and the
/// structure's regions are drawn over it.
let private withPdf (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "page.pdf")
    do
        use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfWriter(pdf))
        document.AddNewPage iText.Kernel.Geom.PageSize.LETTER |> ignore
    try test pdf finally Directory.Delete(folder, true)

/// Opens the structure, and waits for the first page's preview.
let private show root pdf =
    let window = StructureTreeWindow(root, pdf, Width = 1000.0, Height = 760.0)
    window.Show()
    let preview () =
        descendants<Image> window |> List.tryFind (fun image -> not (isNull image.Source))
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) (fun () -> (preview ()).IsSome) |> ignore
    window, (preview ()).Value

/// Clicks the preview at (x, y) on the page, in PDF points (the inverse of
/// StructureTreeWindow.pointOnPage), with shift held if extend.
let private clickAt (image: Image) (x: float) (y: float) (extend: bool) =
    // Each selection renders the page again: wait for it.
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) (fun () -> not (isNull image.Source)) |> ignore
    let bitmap = match image.Source with :? Bitmap as bitmap -> bitmap | _ -> failwith "The preview isn't rendered."
    let size = bitmap.Size
    let bounds = image.Bounds.Size
    let fit = min (bounds.Width / size.Width) (bounds.Height / size.Height)
    let pixels = float bitmap.PixelSize.Width / size.Width
    // The preview is rendered at 2 pixels a point (StructureView.previewScale).
    let scale = 2.0
    let point =
        Point(
            (bounds.Width - size.Width * fit) / 2.0 + x * scale / pixels * fit,
            (bounds.Height - size.Height * fit) / 2.0 + (size.Height - y * scale / pixels) * fit
        )
    let modifiers = if extend then Input.KeyModifiers.Shift else Input.KeyModifiers.None
    let properties = Input.PointerPointProperties(Input.RawInputModifiers.LeftMouseButton, Input.PointerUpdateKind.LeftButtonPressed)
    // The event's position is the window's.
    let window = TopLevel.GetTopLevel image |> nonNull
    let inWindow = (image.TranslatePoint(point, window)).Value
    image.RaiseEvent(Input.PointerPressedEventArgs(image, new Input.Pointer(0, Input.PointerType.Mouse, true), window, inWindow, 0UL, properties, modifiers))
    pump ()

/// The nodes whose blocks are highlighted as selected.
let private selected (window: Window) =
    descendants<Border> window
    |> List.choose (fun b ->
        match b.Tag with
        | :? StructureNode as n when
            b.BorderThickness = Thickness 3.0
            && (match b.BorderBrush with :? ISolidColorBrush as brush -> brush.Color = Colors.DodgerBlue | _ -> false)
            ->
            Some n
        | _ -> None)

let private heading = rect 72.0 690.0 400.0 720.0
let private paragraph = rect 72.0 600.0 540.0 660.0
let private link = rect 200.0 620.0 300.0 640.0

let private page =
    node "StructTreeRoot" null [
        node "Document" null [
            node "H1" heading []
            { node "P" paragraph [ node "Link" link [] ] with Content = "Read the guide." }
            node "P" (rect 72.0 500.0 540.0 560.0) []
        ]
    ]

[<Fact>]
let ``clicking an element on the page preview selects its block`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window, image = show page pdf
            clickAt image 100.0 700.0 false
            let afterHeading = selected window |> List.map _.Tag
            // A link in a paragraph: the innermost.
            clickAt image 250.0 630.0 false
            let afterLink = selected window |> List.map _.Tag
            clickAt image 100.0 610.0 false
            let afterParagraph = selected window |> List.map (fun n -> n.Tag, n.Geometry)
            // Off every element: nothing changes.
            clickAt image 580.0 100.0 false
            let afterNothing = selected window |> List.map (fun n -> n.Tag, n.Geometry)
            window.Close()

            afterHeading |> should equal [ "H1" ]
            afterLink |> should equal [ "Link" ]
            afterParagraph |> should equal [ "P", paragraph ]
            afterNothing |> should equal afterParagraph))

[<Fact>]
let ``shift-clicking on the preview extends the selection, as in the list`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window, image = show page pdf
            clickAt image 100.0 700.0 false
            clickAt image 100.0 510.0 true
            let tags = selected window |> List.map _.Tag
            window.Close()

            tags |> should equal [ "H1"; "P"; "P" ]))

[<Fact>]
let ``a block selected on the preview inside collapsed blocks is opened and scrolled to`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            // 2,100 verses under 21 chapters: over the budget, so the verses start collapsed.
            let verse chapter number =
                let left = 20.0 + float number * 5.5
                let bottom = 40.0 + float chapter * 30.0
                node "P" (rect left bottom (left + 5.0) (bottom + 20.0)) []
            let root = node "StructTreeRoot" null [ node "Document" null [ for c in 0 .. 20 -> node "Sect" null [ for v in 0 .. 99 -> verse c v ] ] ]
            let window, image = show root pdf
            let collapsedAtFirst = descendants<Button> window |> List.filter (fun b -> (string b.Content).StartsWith "▸") |> List.length
            // The last chapter's 51st verse.
            clickAt image (20.0 + 50.0 * 5.5 + 2.5) (40.0 + 20.0 * 30.0 + 10.0) false
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 2.0) (fun () -> not (selected window).IsEmpty) |> ignore
            let chosen = selected window
            let border = descendants<Border> window |> List.find (fun b -> match chosen with [ n ] -> obj.ReferenceEquals(b.Tag, n) | _ -> false)
            // The list's, which holds the block.
            let scroller = border.GetVisualAncestors() |> Seq.pick (fun v -> match v with :? ScrollViewer as s -> Some s | _ -> None)
            let top = (border.TranslatePoint(Point(0.0, 0.0), scroller)).Value.Y
            window.Close()

            collapsedAtFirst |> should equal 21
            chosen |> List.map (fun n -> n.Geometry) |> should equal [ rect (20.0 + 50.0 * 5.5) 640.0 (25.0 + 50.0 * 5.5) 660.0 ]
            // In view.
            top |> should be (greaterThanOrEqualTo 0.0)
            top |> should be (lessThan scroller.Viewport.Height)))

let private selectionOnlyBox (window: Window) =
    descendants<CheckBox> window |> List.find (fun c -> string c.Content = "Show selected block only")

/// What the preview's note says, once it's rendered.
let private note (window: Window) =
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) (fun () ->
        descendants<TextBlock> window |> List.exists (fun t -> (string t.Text).StartsWith "Page 1:"))
    |> ignore
    descendants<TextBlock> window |> List.map (fun t -> string t.Text) |> List.find (fun t -> t.StartsWith "Page 1:")

[<Fact>]
let ``with Show selected block only, the preview draws just the selection, and the page can still be clicked`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window, image = show page pdf
            let all = note window
            (selectionOnlyBox window).IsChecked <- true
            pump ()
            let nothingSelected = note window
            clickAt image 100.0 700.0 false
            let heading = note window
            // An element not drawn is still there to click.
            clickAt image 100.0 510.0 false
            let chosen = selected window |> List.map (fun n -> n.Tag, n.Geometry)
            window.Close()
            // The next window this session starts the same way.
            let next, _ = show page pdf
            let stillOn = (selectionOnlyBox next).IsChecked.GetValueOrDefault()
            (selectionOnlyBox next).IsChecked <- false
            pump ()
            next.Close()

            all |> should equal "Page 1: its 3 elements, outlined and numbered in reading order."
            nothingSelected |> should equal "Page 1: select an element (here, or in the list) to show it; 3 elements hidden."
            heading |> should equal "Page 1: the selection only; 2 elements hidden."
            chosen |> should equal [ "P", rect 72.0 500.0 540.0 560.0 ]
            stillOn |> should be True))
