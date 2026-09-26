module Legibyl.Gui.Tests.StructureEditsTests

open System
open System.IO
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

let private rect left bottom right top : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

let private node (tag: string) (geometry: StructureGeometry | null) (children: StructureNode list) =
    { Tag = tag
      Role = null
      AlternateDescription = null
      ActualText = null
      Content = if children.IsEmpty && tag <> "Figure" then $"{tag} text" else null
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
      Children = List.toArray children }

let private logo = rect 72.0 550.0 172.0 650.0
let private scanned = rect 72.0 400.0 300.0 420.0

/// A figure without alt text, a paragraph read by OCR (with replacement text), and a plain one.
let private structure (figureAlt: string | null) =
    node "StructTreeRoot" null [
        node "Document" null [
            { node "Figure" logo [] with AlternateDescription = figureAlt }
            { node "P" scanned [] with ActualText = "Garden opns at 8" ; Content = null }
            node "P" (rect 72.0 700.0 300.0 715.0) []
        ]
    ]

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private blockFor (window: Window) (tag: string) =
    descendants<Border> window |> List.find (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = tag && not (isNull n.Geometry) | _ -> false)

let private menuItems (border: Border) =
    match border.ContextMenu with
    | null -> []
    | menu -> menu.Items |> Seq.cast<MenuItem> |> Seq.map (fun item -> string item.Header, item) |> List.ofSeq

let private texts (root: Visual) =
    descendants<TextBlock> root |> List.map (fun t -> string t.Text)

/// A PDF path in a fresh folder (the PDF needn't exist; its edits are saved next to it).
let private withPdf (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try test (Path.Combine(folder, "garden.pdf")) finally Directory.Delete(folder, true)

let private show root pdf =
    let window = StructureTreeWindow(root, pdf, Width = 900.0, Height = 700.0)
    window.Show()
    pump ()
    window

/// Chooses a block's menu item, types text into the prompt that opens, and presses OK.
let private editVia (window: Window) (tag: string) (item: string) (text: string) =
    (menuItems (blockFor window tag) |> List.find (fst >> (=) item) |> snd).RaiseEvent(RoutedEventArgs MenuItem.ClickEvent)
    pump ()
    let prompt = window.OwnedWindows |> Seq.exactlyOne
    (descendants<TextBox> prompt |> List.exactlyOne).Text <- text
    pump ()
    (descendants<Button> prompt |> List.find (fun b -> string b.Content = "OK")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
    pump ()

let private selectTab (window: Window) (header: string) =
    let tabs = descendants<TabControl> window |> List.head
    tabs.SelectedItem <- tabs.Items |> Seq.cast<TabItem> |> Seq.find (fun t -> (string t.Header).StartsWith header)
    pump ()

[<Fact>]
let ``figures offer alt text, and replacement text only where there is some`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show (structure null) pdf
            let figure = menuItems (blockFor window "Figure") |> List.map fst
            let paragraphs =
                descendants<Border> window
                |> List.filter (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "P" | _ -> false)
                |> List.map (menuItems >> List.map fst)
            window.Close()

            figure |> should equal [ "Edit Alt Text…" ]
            paragraphs |> should equal [ [ "Edit Replacement Text…" ]; [] ]))

[<Fact>]
let ``an edit is saved next to the PDF, shown as not in it yet, and listed on the Edits tab`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show (structure null) pdf
            editVia window "Figure" "Edit Alt Text…" "Tomato seedlings in a tray"
            editVia window "P" "Edit Replacement Text…" "Garden opens at 8"
            let layout = texts window
            selectTab window "Edits"
            let editsTab = texts window
            let header = descendants<TabItem> window |> List.map (fun t -> string t.Header)
            window.Close()

            RegionEdits.load pdf |> List.map (fun e -> e.Tag, e.Alt, e.Text)
            |> should equal [ "Figure", Some "Tomato seedlings in a tray", None; "P", None, Some "Garden opens at 8" ]
            layout |> should contain "✎ your alt, not in the PDF yet: \"Tomato seedlings in a tray\""
            layout |> should contain "✎ your text, not in the PDF yet: \"Garden opens at 8\""
            header |> should contain "Edits (2)"
            editsTab |> List.filter ((=) "Not in this PDF yet: Save… writes it to a copy.") |> List.length |> should equal 2))

[<Fact>]
let ``an edit the PDF already has shows as in it, and can be removed`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            RegionEdits.save pdf (RegionEdits.setAlt [] 1 "Figure" logo "Tomato seedlings in a tray")
            let window = show (structure "Tomato seedlings in a tray") pdf
            let layout = texts window
            selectTab window "Edits"
            let status = texts window
            (descendants<Button> window |> List.find (fun b -> string b.Content = "Remove")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let header = descendants<TabItem> window |> List.map (fun t -> string t.Header)
            window.Close()

            layout |> List.filter (fun t -> t.StartsWith "✎") |> should be Empty
            status |> should contain "In this PDF."
            RegionEdits.load pdf |> should be Empty
            header |> should contain "Edits"))

[<Fact>]
let ``saving an edit tells whoever opened the window, so it can count them`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let mutable told = 0
            let window = StructureTreeWindow(structure null, pdf, (fun () -> told <- told + 1), Width = 900.0, Height = 700.0)
            window.Show()
            pump ()
            editVia window "Figure" "Edit Alt Text…" "Tomato seedlings in a tray"
            window.Close()

            told |> should equal 1))

/// Three paragraphs a line each, and a list with one paragraph in its item's body.
let private lines =
    node "StructTreeRoot" null [
        node "Document" null [
            node "P" (rect 72.0 697.0 238.0 712.0) []
            node "P" (rect 72.0 685.0 227.0 698.0) []
            { node "P" (rect 72.0 669.0 222.0 684.0) [] with ActualText = null }
            node "L" null [ node "LI" null [ node "LBody" null [ node "P" (rect 90.0 640.0 200.0 652.0) [] ] ] ]
        ]
    ]

let private paragraphs (window: Window) =
    descendants<Border> window |> List.filter (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "P" | _ -> false)

/// Clicks a block, with shift held if extend.
let private click (block: Border) (extend: bool) =
    let modifiers = if extend then Input.KeyModifiers.Shift else Input.KeyModifiers.None
    let properties = Input.PointerPointProperties(Input.RawInputModifiers.LeftMouseButton, Input.PointerUpdateKind.LeftButtonPressed)
    block.RaiseEvent(
        Input.PointerPressedEventArgs(block, new Input.Pointer(0, Input.PointerType.Mouse, true), block, Point(4.0, 4.0), 0UL, properties, modifiers)
    )
    pump ()

let private mergeButton (window: Window) =
    descendants<Button> window |> List.tryFind (fun b -> (string b.Content).StartsWith "Merge ")

[<Fact>]
let ``shift-clicking selects the paragraphs between, which can be merged as an edit`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show lines pdf
            let blocks = paragraphs window
            click blocks[0] false
            let hint = texts window
            click blocks[2] true
            let offer = mergeButton window |> Option.map (fun b -> string b.Content)
            (mergeButton window |> Option.get).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let layout = texts window
            selectTab window "Edits"
            let editsTab = texts window
            window.Close()

            hint |> should contain "Shift-click the paragraphs after it to merge them into one."
            offer |> should equal (Some "Merge 3 Paragraphs")
            RegionEdits.load pdf |> List.map (fun e -> e.Tag, e.MergeWith.Length) |> should equal [ "P", 2 ]
            layout |> should contain "✎ merged with the next 2 paragraphs, not in the PDF yet"
            layout |> List.filter ((=) "✎ merged into the paragraph before, not in the PDF yet") |> List.length |> should equal 2
            editsTab |> should contain "merge: 3 paragraphs into one, from \"P text\""
            editsTab |> should contain "Not in this PDF yet: Save… writes it to a copy."))

[<Fact>]
let ``paragraphs already chosen to merge offer to stay apart instead`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show lines pdf
            let blocks = paragraphs window
            click blocks[0] false
            click blocks[1] true
            (mergeButton window |> Option.get).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let undo = descendants<Button> window |> List.find (fun b -> string b.Content = "Don't Merge These")
            undo.RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let offer = mergeButton window |> Option.map (fun b -> string b.Content)
            window.Close()

            RegionEdits.load pdf |> should be Empty
            offer |> should equal (Some "Merge 2 Paragraphs")))

[<Fact>]
let ``a merge isn't offered across parents, or for paragraphs with replacement text`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show lines pdf
            let blocks = paragraphs window
            click blocks[2] false
            // The list's paragraph has another parent: just it is selected.
            click blocks[3] true
            let acrossParents = mergeButton window
            window.Close()
            let window = show (structure null) pdf
            let blocks = paragraphs window
            // The first paragraph has replacement text (it was read by OCR).
            click blocks[0] false
            click blocks[1] true
            let withReplacement = mergeButton window
            window.Close()

            acrossParents |> should equal None
            withReplacement |> should equal None))

let private tagPicker (window: Window) =
    descendants<ComboBox> window
    |> List.tryFind (fun c -> (string (Automation.AutomationProperties.GetName c)).StartsWith "Tag for this")

[<Fact>]
let ``a block of text can be given another tag, which shows as not in the PDF yet`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show lines pdf
            let first = (paragraphs window)[0]
            click first false
            (tagPicker window |> Option.get).SelectedItem <- "H1"
            pump ()
            let layout = texts window
            selectTab window "Edits"
            let editsTab = texts window
            window.Close()

            RegionEdits.load pdf |> List.map (fun e -> e.Tag, e.Retag) |> should equal [ "P", Some "H1" ]
            layout |> should contain "✎ your tag, not in the PDF yet: \"H1\""
            editsTab |> should contain "tag: H1"
            editsTab |> should contain "Not in this PDF yet: Save… writes it to a copy."))

[<Fact>]
let ``a retag the PDF already has shows as in it, and figures get no tag picker`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let paragraph = rect 72.0 697.0 238.0 712.0
            RegionEdits.save pdf (RegionEdits.setRetag [] 1 "P" paragraph "H1")
            let retagged = node "StructTreeRoot" null [ node "Document" null [ node "H1" paragraph []; node "Figure" logo [] ] ]
            let window = show retagged pdf
            let heading = descendants<Border> window |> List.find (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "H1" | _ -> false)
            click heading false
            let picked = tagPicker window |> Option.map (fun c -> string c.SelectedItem)
            click (blockFor window "Figure") false
            let forFigure = tagPicker window
            selectTab window "Edits"
            let editsTab = texts window
            window.Close()

            picked |> should equal (Some "H1")
            forFigure |> should equal None
            editsTab |> should contain "In this PDF."))

[<Fact>]
let ``a link's description can be edited, starting from the one it has`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let link =
                { node "Link" (rect 72.0 690.0 150.0 710.0) [] with
                    LinkUri = "https://example.com"
                    LinkDescription = "click here"
                    Content = "Visit the site" }
            let window = show (node "StructTreeRoot" null [ node "Document" null [ link ] ]) pdf
            let items = menuItems (blockFor window "Link") |> List.map fst
            (menuItems (blockFor window "Link") |> List.find (fst >> (=) "Edit Link Description…") |> snd).RaiseEvent(RoutedEventArgs MenuItem.ClickEvent)
            pump ()
            let prompt = window.OwnedWindows |> Seq.exactlyOne
            let prefilled = (descendants<TextBox> prompt |> List.exactlyOne).Text
            (descendants<TextBox> prompt |> List.exactlyOne).Text <- "Example (example.com)"
            (descendants<Button> prompt |> List.find (fun b -> string b.Content = "OK")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let layout = texts window
            window.Close()

            items |> should equal [ "Edit Link Description…" ]
            prefilled |> should equal "click here"
            layout |> should contain "description: \"click here\""
            layout |> should contain "✎ your description, not in the PDF yet: \"Example (example.com)\""
            RegionEdits.load pdf |> List.map (fun e -> e.Tag, e.Alt) |> should equal [ "Link", Some "Example (example.com)" ]))
