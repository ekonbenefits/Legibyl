module Legibyl.Gui.Tests.FindingMarkersTests

open System
open System.IO
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.VisualTree
open iText.IO.Font.Constants
open iText.IO.Image
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private rect (l, b, r, t) : StructureGeometry = { PageNumber = 1; Left = l; Bottom = b; Right = r; Top = t }

let private node (tag: string) (geometry: StructureGeometry | null) (children: StructureNode list) =
    { Tag = tag
      Role = null
      AlternateDescription = null
      ActualText = null
      Content = null
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

let private finding (severity: FindingSeverity) (clause: string) (geometry: StructureGeometry | null) (element: string | null) =
    { Specification = "ISO 14289-1:2014"
      Clause = clause
      TestNumber = 1
      Passed = false
      Message = $"Rule {clause}"
      ErrorMessage = null
      LocationContext = null
      Severity = severity
      FixOptions = PdfFixOptions.None
      Geometry = geometry
      Element = element }

let private figure = node "Figure" (rect (72.0, 550.0, 172.0, 650.0)) []
let private link = node "Link" (rect (200.0, 700.0, 260.0, 712.0)) []
let private paragraph = node "P" (rect (72.0, 695.0, 400.0, 715.0)) [ link ]
let private section = node "Sect" (rect (60.0, 540.0, 420.0, 720.0)) [ paragraph; figure ]
let private root = node "StructTreeRoot" null [ node "Document" null [ section ] ]

[<Fact>]
let ``each finding goes on the smallest element that holds its region`` () =
    let noAlt = finding FindingSeverity.Failure "7.3" (rect (72.0, 550.0, 172.0, 650.0)) "figure (Figure)"
    let linkWarning = finding FindingSeverity.Warning "7.18.5" (rect (201.0, 701.0, 259.0, 711.0)) null
    let untagged = finding FindingSeverity.Failure "7.1" (rect (80.0, 600.0, 120.0, 620.0)) "content not in the tags \"Hello\""
    let metadata = finding FindingSeverity.Failure "7.1" null null

    let placed = FindingMarkers.place root [ linkWarning; noAlt; untagged; metadata ]

    placed[figure] |> should equal [ noAlt ]
    // Inside the paragraph, but the link's own.
    placed[link] |> should equal [ linkWarning ]
    // Document-wide, or content in no element: not on a block.
    placed.Count |> should equal 2

[<Fact>]
let ``a finding with no region goes on the element its location names: a figure that draws nothing`` () =
    let empty = node "Figure" null []
    let tree = node "StructTreeRoot" null [ node "Document" null [ section; empty ] ]
    let at (context: string) =
        { finding FindingSeverity.Failure "7.3" null "figure (Figure)" with LocationContext = context }
    let noAlt = at "root/document[0]/StructTreeRoot[0](31 0 obj PDStructTreeRoot)/K[0](30 0 obj SEDocument Document)/K[1](49 0 obj SEFigure Figure)"
    // No such element; and a path that's not to an element, but to a page's content.
    let missing = at "root/document[0]/StructTreeRoot[0](31 0 obj PDStructTreeRoot)/K[0](30 0 obj SEDocument Document)/K[7](50 0 obj SEFigure Figure)"
    let content = at "root/document[0]/pages[0](1 0 obj PDPage)/contentStream[0](4 0 obj PDContentStream)/content[3]"

    let placed = FindingMarkers.place tree [ noAlt; missing; content ]

    placed[empty] |> should equal [ noAlt ]
    placed.Count |> should equal 1

[<Fact>]
let ``a block's failures come before its warnings`` () =
    let warning = finding FindingSeverity.Warning "7.3" (rect (72.0, 550.0, 172.0, 650.0)) null
    let failure = finding FindingSeverity.Failure "7.3" (rect (72.0, 550.0, 172.0, 650.0)) null
    (FindingMarkers.place root [ warning; failure ])[figure] |> should equal [ failure; warning ]

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private waitFor (condition: unit -> bool) =
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) condition |> ignore

let private click (b: Button) =
    b.RaiseEvent(RoutedEventArgs Button.ClickEvent)
    pump ()

type private FixedLayout(regions: LayoutRegion list) =
    interface IPageLayoutSource with
        member _.PageRegions(_, _) = regions
        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = null

/// A tagged page: a paragraph, and a figure without alt text (a failure), in a fresh folder.
let private withTaggedPdf (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let untagged = Path.Combine(folder, "garden-untagged.pdf")
    let pdf = Path.Combine(folder, "garden.pdf")
    try
        do
            use document = new PdfDocument(new PdfWriter(untagged))
            let canvas = PdfCanvas(document.AddNewPage())
            canvas.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f).MoveText(72.0, 700.0).ShowText("The garden in spring.").EndText()
            |> ignore
            let pixels = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 255uy; 255uy; 0uy |]
            canvas.AddImageWithTransformationMatrix(ImageDataFactory.Create(2, 2, 3, 8, pixels, null), 100f, 0f, 0f, 100f, 72f, 550f, true) |> ignore
        let region tag bounds : LayoutRegion =
            { Tag = tag; Geometry = rect bounds; ListNumbering = null; ActualText = null; QrText = null; Alt = null }
        let layout = FixedLayout [ region "P" (70.0, 695.0, 300.0, 715.0); region "Figure" (70.0, 548.0, 175.0, 652.0) ]
        VeraPdfValidator(LayoutSource = layout).Fix(untagged, PdfProfile.PdfUa1, pdf, PdfFixOptions.TagFromLayout) |> ignore
        test pdf
    finally
        Directory.Delete(folder, true)

let private markers (window: Window) =
    descendants<Button> window |> List.filter (fun b -> (b.Tag :? string) && string b.Tag = "finding-marker")

/// The block a marker is on, if it's on one (the title's isn't).
let private tryBlockOf (marker: Button) =
    marker.GetVisualAncestors()
    |> Seq.tryPick (fun v ->
        match v with
        | :? Border as b ->
            match b.Tag with
            | :? StructureNode as n -> Some n
            | _ -> None
        | _ -> None)

let private blockOf (marker: Button) = tryBlockOf marker |> Option.get

/// The markers on blocks.
let private blockMarkers (window: Window) = markers window |> List.filter (tryBlockOf >> Option.isSome)

[<Fact>]
let ``a tagged PDF's Structure window marks the blocks with failures, and a marker shows its finding in the results`` () =
    withTaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window pdf
            click (descendants<Button> window |> List.find (fun b -> (string (AutomationProperties.GetName b)).StartsWith "Structure…"))
            let structure () = window.OwnedWindows |> Seq.tryPick (fun w -> match w with :? StructureTreeWindow as s -> Some s | _ -> None)
            waitFor (fun () -> (structure ()).IsSome)
            let structureWindow = structure () |> Option.get
            pump ()
            let marked = blockMarkers structureWindow
            let markedTags = marked |> List.map (fun m -> (blockOf m).Tag)
            let figureMarker = marked |> List.find (fun m -> (blockOf m).Tag = "Figure")
            let tip = string (ToolTip.GetTip figureMarker)
            // Clicking it selects the finding in the main window's results.
            click figureMarker
            let chosen =
                match (HeadlessWindow.resultsList window).SelectedItem with
                | :? ResultItem as FindingItem row -> Some(row.Clause, row.TestNumber)
                | _ -> None
            structureWindow.Close()
            window.Close()

            // The figure without alt text; and the paragraph, whose font isn't embedded.
            markedTags |> should equal [ "P"; "Figure" ]
            tip |> should haveSubstring "Failure: ISO 14289-1 7.3, test 1"
            // What it means, from the rule guide.
            tip |> should haveSubstring "a screen reader can only say it's an image"
            chosen |> should equal (Some("7.3", 1))))

[<Fact>]
let ``clicking Structure again brings its window forward rather than opening another`` () =
    withTaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window pdf
            let structureButton () = descendants<Button> window |> List.find (fun b -> (string (AutomationProperties.GetName b)).StartsWith "Structure…")
            let structures () = window.OwnedWindows |> Seq.filter (fun w -> w :? StructureTreeWindow) |> List.ofSeq
            // Twice while it's still being read, then once it's open.
            click (structureButton ())
            click (structureButton ())
            waitFor (fun () -> not (structures ()).IsEmpty)
            pump ()
            click (structureButton ())
            pump ()
            let whileOpen = structures () |> List.length
            // Closed, it opens again.
            (structures () |> List.exactlyOne).Close()
            pump ()
            click (structureButton ())
            waitFor (fun () -> not (structures ()).IsEmpty)
            let reopened = structures () |> List.length
            for w in structures () do w.Close()
            window.Close()

            whileOpen |> should equal 1
            reopened |> should equal 1))

[<Fact>]
let ``markers stay until the file is validated again: an edit doesn't clear them`` () =
    withTaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window pdf
            click (descendants<Button> window |> List.find (fun b -> (string (AutomationProperties.GetName b)).StartsWith "Structure…"))
            let structure () = window.OwnedWindows |> Seq.tryPick (fun w -> match w with :? StructureTreeWindow as s -> Some s | _ -> None)
            waitFor (fun () -> (structure ()).IsSome)
            let structureWindow = structure () |> Option.get
            pump ()
            // Give the figure alt text.
            let figureBlock =
                descendants<Border> structureWindow |> List.find (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "Figure" | _ -> false)
            let item = blockMenu figureBlock |> List.find (fun i -> string i.Header = "Edit Alt Text…")
            item.RaiseEvent(RoutedEventArgs MenuItem.ClickEvent)
            pump ()
            let prompt = structureWindow.OwnedWindows |> Seq.exactlyOne
            (descendants<TextBox> prompt |> List.exactlyOne).Text <- "Four coloured squares"
            pump ()
            click (descendants<Button> prompt |> List.find (fun b -> string b.Content = "OK"))
            let after = blockMarkers structureWindow |> List.map (fun m -> (blockOf m).Tag)
            structureWindow.Close()
            window.Close()

            after |> should equal [ "P"; "Figure" ]))

[<Fact>]
let ``a suggested structure has no markers: its findings are the untagged file's`` () =
    onUiThread (fun () ->
        let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
        Directory.CreateDirectory folder |> ignore
        try
            let pdf = Path.Combine(folder, "untagged.pdf")
            do
                use document = new PdfDocument(new PdfWriter(pdf))
                document.AddNewPage() |> ignore
            let findings: FindingMarkers.Findings =
                { Items = [ finding FindingSeverity.Failure "7.1" (rect (72.0, 550.0, 172.0, 650.0)) null; finding FindingSeverity.Failure "7.1" null null ]
                  Show = ignore }
            let review = DocumentReviews().For pdf
            let window = StructureTreeWindow(Suggested, pdf, review, ignore, findings)
            window.Show()
            pump ()
            let shownMarkers = markers window
            let rulers = descendants<FindingRuler.Ruler> window
            window.Close()

            shownMarkers |> should be Empty
            rulers |> should be Empty
        finally
            Directory.Delete(folder, true))


/// The Structure window a tagged PDF's Structure… button opens, from the main window.
let private openStructure (window: MainWindow) =
    click (descendants<Button> window |> List.find (fun b -> (string (AutomationProperties.GetName b)).StartsWith "Structure…"))
    let structure () = window.OwnedWindows |> Seq.tryPick (fun w -> match w with :? StructureTreeWindow as s -> Some s | _ -> None)
    waitFor (fun () -> (structure ()).IsSome)
    let structureWindow = structure () |> Option.get
    pump ()
    structureWindow

/// The title row's text box, and the marker beside its label, if any.
let private titleRow (window: Window) =
    let box = descendants<TextBox> window |> List.find (fun t -> string t.PlaceholderText = "What a screen reader and the title bar call this document")
    let marker =
        markers window |> List.tryFind (tryBlockOf >> Option.isNone)
    box, marker

[<Fact>]
let ``a tagged PDF's Structure window has its title, empty when it has none, marked with the title failures`` () =
    withTaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window pdf
            let structureWindow = openStructure window
            let box, marker = titleRow structureWindow
            let text = box.Text
            let tip = marker |> Option.map (fun m -> string (ToolTip.GetTip m)) |> Option.defaultValue ""
            box.Text <- "The Garden in Spring"
            pump ()
            let given = structureWindow.Review.Title
            structureWindow.Close()
            window.Close()

            text |> should equal ""
            tip |> should haveSubstring "7.1, test 10"
            given |> should equal (Some "The Garden in Spring")))

[<Fact>]
let ``a tagged PDF with a title of its own shows it`` () =
    withTaggedPdf (fun pdf ->
        let titled = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "titled.pdf")
        do
            use document = new PdfDocument(new PdfReader(pdf), new PdfWriter(titled))
            document.GetDocumentInfo().SetTitle "The Community Garden" |> ignore
        onUiThread (fun () ->
            let window = StructureTreeWindow(StructureSource.Tagged(VeraPdfValidator().ReadStructureTree titled |> nonNull), titled, DocumentReviews().For titled, ignore)
            window.Show()
            pump ()
            let box, _ = titleRow window
            let text = box.Text
            window.Close()

            text |> should equal "The Community Garden"))

/// The block focused in window, if one is.
let private focusedBlock (window: Window) =
    Some window.FocusManager
    |> Option.bind (fun f -> match f.GetFocusedElement() with :? Border as b -> Some b | _ -> None)
    |> Option.bind (fun b -> match b.Tag with :? StructureNode as n -> Some n.Tag | _ -> None)

[<Fact>]
let ``the blocks with findings are ticked beside the blocks, and Next and Previous (F8) go to them in turn`` () =
    withTaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window pdf
            let structureWindow = openStructure window
            let ruler = descendants<FindingRuler.Ruler> structureWindow |> List.exactlyOne
            waitFor (fun () -> ruler.Ticks.Length = 2)
            let ticks = ruler.Ticks
            let summary =
                descendants<StackPanel> structureWindow
                |> List.map (fun p -> string (AutomationProperties.GetName p))
                |> List.find (fun t -> t.Contains "with failures")
            let button name = descendants<Button> structureWindow |> List.find (fun b -> string (AutomationProperties.GetName b) = name)
            let next, previous = button "Next block with failures or warnings", button "Previous block with failures or warnings"
            let go (b: Button) =
                click b
                waitFor (fun () -> (focusedBlock structureWindow).IsSome)
                focusedBlock structureWindow
            let visited = [ go next; go next; go next; go previous ]
            // Clicking a tick goes to its block.
            ruler.Go (snd ticks[1]).Node
            pump ()
            let clicked = focusedBlock structureWindow
            let keys = next.HotKey, previous.HotKey
            structureWindow.Close()
            window.Close()

            summary |> should equal "2 blocks with failures"
            // In the blocks' order, down the strip.
            ticks |> List.map (fun (_, mark) -> mark.Node.Tag) |> should equal [ "P"; "Figure" ]
            ticks |> List.map fst |> List.pairwise |> List.forall (fun (a, b) -> a < b && a >= 0.0 && b <= 1.0) |> should be True
            ticks |> List.forall (fun (_, mark) -> mark.Failure) |> should be True
            // Round from the last to the first, and back.
            visited |> should equal [ Some "P"; Some "Figure"; Some "P"; Some "Figure" ]
            clicked |> should equal (Some "Figure")
            keys |> should equal (Input.KeyGesture(Input.Key.F8), Input.KeyGesture(Input.Key.F8, Input.KeyModifiers.Shift))))

/// A tagged PDF of three pages, a heading and two paragraphs on each, in a fresh folder.
let private withThreePages (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let untagged = Path.Combine(folder, "garden-untagged.pdf")
    let pdf = Path.Combine(folder, "garden.pdf")
    try
        do
            use document = new PdfDocument(new PdfWriter(untagged))
            for page in 1 .. 3 do
                let canvas = PdfCanvas(document.AddNewPage())
                for i, text in List.indexed [ $"Page {page}"; "Tulips go in before the frost."; "Water them once a week." ] do
                    canvas.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f).MoveText(72.0, 720.0 - float i * 30.0).ShowText(text).EndText()
                    |> ignore
        let region tag bounds : LayoutRegion =
            { Tag = tag; Geometry = rect bounds; ListNumbering = null; ActualText = null; QrText = null; Alt = null }
        let layout = FixedLayout [ for i in 0 .. 2 -> region (if i = 0 then "H1" else "P") (70.0, 715.0 - float i * 30.0, 300.0, 735.0 - float i * 30.0) ]
        VeraPdfValidator(LayoutSource = layout).Fix(untagged, PdfProfile.PdfUa1, pdf, PdfFixOptions.TagFromLayout) |> ignore
        test pdf
    finally
        Directory.Delete(folder, true)

/// The page of the block focused in window, if one is.
let private focusedPage (window: Window) =
    Some window.FocusManager
    |> Option.bind (fun f -> match f.GetFocusedElement() with :? Border as b -> Some b | _ -> None)
    |> Option.bind (fun b -> match b.Tag with :? StructureNode as n -> Option.ofObj n.Geometry | _ -> None)
    |> Option.map _.PageNumber

[<Fact>]
let ``the arrows below the blocks go to the page before and after, and the picker to any page`` () =
    withThreePages (fun pdf ->
        onUiThread (fun () ->
            let source = StructureSource.Tagged(VeraPdfValidator().ReadStructureTree pdf |> nonNull)
            let window = StructureTreeWindow(source, pdf, DocumentReviews().For pdf, ignore, FindingMarkers.none)
            window.Show()
            pump ()
            let button name = descendants<Button> window |> List.find (fun b -> string (AutomationProperties.GetName b) = name)
            let picker () = descendants<ComboBox> window |> List.find (fun c -> string (AutomationProperties.GetName c) = "Go to page")
            let go (name: string) =
                click (button name)
                waitFor (fun () -> (focusedPage window).IsSome)
                pump ()
                focusedPage window
            let pages = (picker ()).Items |> Seq.map unbox<int> |> List.ofSeq
            // The first page is shown to begin with.
            let atFirst = (button "Previous page").IsEnabled
            let first = go "Next page"
            // The page's first block at the top of the blocks, its page after it.
            let atTop =
                Some window.FocusManager
                |> Option.bind (fun f -> match f.GetFocusedElement() with :? Border as b -> Some b | _ -> None)
                |> Option.bind (fun b ->
                    let scroll = descendants<ScrollViewer> window |> List.find (fun s -> s.Name = FindingRuler.blocksName)
                    b.TranslatePoint(Point(0.0, 0.0), scroll) |> Option.ofNullable
                    // Or as near as it goes: scrolled to the end, when the blocks after it don't fill the view.
                    |> Option.map (fun p -> p.Y <= 20.0 || scroll.Offset.Y >= scroll.Extent.Height - scroll.Viewport.Height - 4.0))
            let visited = [ first; go "Next page" ]
            let atLast = (button "Next page").IsEnabled
            let back = go "Previous page"
            let shown = (picker ()).SelectedItem
            (picker ()).SelectedItem <- box 3
            waitFor (fun () -> focusedPage window = Some 3)
            let picked = focusedPage window
            let keys = (button "Previous page").HotKey, (button "Next page").HotKey
            // No findings, so no arrows for them.
            let findingArrows = descendants<Button> window |> List.filter (fun b -> (string (AutomationProperties.GetName b)).Contains "failures or warnings")
            window.Close()

            pages |> should equal [ 1; 2; 3 ]
            atFirst |> should be False
            visited |> should equal [ Some 2; Some 3 ]
            atTop |> should equal (Some true)
            atLast |> should be False
            back |> should equal (Some 2)
            shown |> should equal (box 2)
            picked |> should equal (Some 3)
            keys |> should equal (Input.KeyGesture(Input.Key.PageUp, Input.KeyModifiers.Alt), Input.KeyGesture(Input.Key.PageDown, Input.KeyModifiers.Alt))
            findingArrows |> should be Empty))

[<Fact>]
let ``editing is on the right with a pencil (ordering by layout with the wand), going places on the left without one, and More holds Import and Export`` () =
    withThreePages (fun pdf ->
        onUiThread (fun () ->
            let source = StructureSource.Tagged(VeraPdfValidator().ReadStructureTree pdf |> nonNull)
            let window = StructureTreeWindow(source, pdf, DocumentReviews().For pdf, ignore, FindingMarkers.none)
            window.Show()
            pump ()
            let named name = descendants<Button> window |> List.find (fun b -> string (AutomationProperties.GetName b) = name)
            let splitter = descendants<GridSplitter> window |> List.exactlyOne
            let x (b: Button) = (b.TranslatePoint(Point(0.0, 0.0), window) |> Option.ofNullable |> Option.get).X
            let line = (splitter.TranslatePoint(Point(0.0, 0.0), window) |> Option.ofNullable |> Option.get).X
            let has icon (b: Button) =
                descendants<Avalonia.Controls.Shapes.Path> b |> List.exists (fun p -> obj.ReferenceEquals(p.Data, icon))
            let pencilled = has Icons.pencil
            let edits = [ named "Move Up"; named "Move Down" ]
            // Suggested from the layout: the wand, as Suggest Structure has.
            let order = named "Order Page 1 by Layout"
            let orderRight = x order > (splitter.TranslatePoint(Point(0.0, 0.0), window) |> Option.ofNullable |> Option.get).X && has Icons.wand order && not (pencilled order)
            let goes = [ named "Previous page"; named "Next page" ]
            let editsRight = edits |> List.forall (fun b -> x b > line && pencilled b)
            let goesLeft = goes |> List.forall (fun b -> x b < line && not (pencilled b) && not (isNull b.Theme))
            let more = named "More"
            let items =
                match more.Flyout with
                | :? MenuFlyout as flyout -> flyout.Items |> Seq.choose (function :? MenuItem as i -> Some(string i.Header) | _ -> None) |> List.ofSeq
                | _ -> []
            let footer = descendants<Button> window |> List.filter (fun b -> [ "Import Edits…"; "Export JSON…" ] |> List.contains (string b.Content))
            window.Close()

            editsRight |> should be True
            orderRight |> should be True
            goesLeft |> should be True
            items |> should equal [ "Import Edits…"; "Export Structure as JSON…" ]
            footer |> should be Empty))
