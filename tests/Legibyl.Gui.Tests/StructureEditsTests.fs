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


/// What a button says: its screen-reader name (an edit button's label, after its pencil), else its text.
let private buttonText (b: Button) =
    match Automation.AutomationProperties.GetName b with
    | null | "" -> string b.Content
    | name -> name

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
      ColSpan = 1
      RowSpan = 1
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
    blockMenu border |> List.map (fun item -> string item.Header, item)

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
    (descendants<Button> prompt |> List.find (fun b -> buttonText b = "OK")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
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
            let edits (border: Border) = menuItems border |> List.map fst |> List.filter (fun t -> t.StartsWith "Edit ")
            let figure = edits (blockFor window "Figure")
            let paragraphs =
                descendants<Border> window
                |> List.filter (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "P" | _ -> false)
                |> List.map edits
            window.Close()

            figure |> should equal [ "Edit Alt Text…" ]
            paragraphs |> should equal [ [ "Edit Replacement Text…" ]; [] ]))

/// The selected element's edit buttons, by what they say.
let private editButtons (window: Window) =
    descendants<Button> window |> List.map buttonText |> List.filter (fun t -> t.StartsWith "Edit ")

/// Selects tag's block, as clicking it does.
let private selectBlock (window: Window) (tag: string) =
    let block = blockFor window tag
    block.Focus() |> ignore
    block.RaiseEvent(Input.KeyEventArgs(RoutedEvent = Input.InputElement.KeyDownEvent, Key = Input.Key.Enter, Source = block))
    pump ()

[<Fact>]
let ``the selected element offers its edits beside its tag, only those it has, and F2 opens the first`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show (structure null) pdf
            let before = editButtons window
            selectBlock window "Figure"
            let figure = editButtons window
            // Its button opens the prompt.
            (descendants<Button> window |> List.find (fun b -> buttonText b = "Edit Alt Text…")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let fromButton = window.OwnedWindows |> Seq.map (fun w -> string w.Title) |> List.ofSeq
            for prompt in List.ofSeq window.OwnedWindows do prompt.Close()
            pump ()
            // F2 on the focused block does too.
            let block = blockFor window "Figure"
            block.RaiseEvent(Input.KeyEventArgs(RoutedEvent = Input.InputElement.KeyDownEvent, Key = Input.Key.F2, Source = block))
            pump ()
            let fromKey = window.OwnedWindows.Count
            for prompt in List.ofSeq window.OwnedWindows do prompt.Close()
            pump ()
            let plain =
                descendants<Border> window
                |> List.find (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "P" && isNull n.ActualText | _ -> false)
            plain.Focus() |> ignore
            plain.RaiseEvent(Input.KeyEventArgs(RoutedEvent = Input.InputElement.KeyDownEvent, Key = Input.Key.Enter, Source = plain))
            pump ()
            let paragraph = editButtons window
            window.Close()

            before |> should be Empty
            figure |> should equal [ "Edit Alt Text…" ]
            fromButton |> should haveLength 1
            fromKey |> should equal 1
            // A paragraph with nothing to say instead: nothing to edit.
            paragraph |> should be Empty))

[<Fact>]
let ``a block's right-click menu offers what Selected element does for it, and selects it`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show (structure null) pdf
            let figure = blockFor window "Figure"
            let menu = blockMenu figure
            pump ()
            let selectedThen = editButtons window
            // The right pane's buttons for it (its edits, Move Up and Down), then its Tag picker.
            let pane =
                descendants<Button> window
                |> List.map buttonText
                |> List.filter (fun t -> t.StartsWith "Edit " || t = "Move Up" || t = "Move Down")
            let picker = descendants<ComboBox> window |> List.exists (fun c -> (string (Automation.AutomationProperties.GetName c)).StartsWith "Tag for")
            let tag = menu |> List.find (fun i -> string i.Header = "Tag")
            let tags = tag.Items |> Seq.cast<MenuItem> |> Seq.map (fun i -> string i.Header, i.IsChecked) |> List.ofSeq
            // Choosing one from the menu tags it, as the picker does.
            (tag.Items |> Seq.cast<MenuItem> |> Seq.find (fun i -> string i.Header = "Artifact")).RaiseEvent(RoutedEventArgs MenuItem.ClickEvent)
            pump ()
            let retagged = window.Review.Edits |> List.choose _.Retag
            let moveUp = menu |> List.find (fun i -> string i.Header = "Move Up")
            let shownKeys = moveUp.InputGesture
            window.Close()

            menu |> List.map (fun i -> string i.Header) |> should equal (pane @ [ "Tag" ])
            selectedThen |> should equal [ "Edit Alt Text…" ]
            picker |> should be True
            tags |> should equal [ "Figure", true; "Artifact", false ]
            retagged |> should equal [ "Artifact" ]
            shownKeys |> should equal (Input.KeyGesture(Input.Key.Up, Input.KeyModifiers.Alt))))

[<Fact>]
let ``the alt text prompt says what good alt text is`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show (structure null) pdf
            (menuItems (blockFor window "Figure") |> List.find (fst >> (=) "Edit Alt Text…") |> snd).RaiseEvent(RoutedEventArgs MenuItem.ClickEvent)
            pump ()
            let prompt = window.OwnedWindows |> Seq.exactlyOne
            let said = descendants<TextBlock> prompt |> List.map (fun t -> string t.Text) |> String.concat " "
            let placeholder = (descendants<TextBox> prompt |> List.exactlyOne).PlaceholderText
            prompt.Close()
            pump ()
            window.Close()

            said |> should haveSubstring "Say what it's for here, not only what it shows"
            said |> should haveSubstring "leave out \"image of\""
            placeholder |> should equal "What it's for, in a sentence"))

[<Fact>]
let ``an edit is kept in the review, shown as not in the PDF yet, and listed on the Edits tab`` () =
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

            window.Review.Edits |> List.map (fun e -> e.Tag, e.Alt, e.Text)
            |> should equal [ "Figure", Some "Tomato seedlings in a tray", None; "P", None, Some "Garden opens at 8" ]
            // Not saved until Save in the main window.
            window.Review.IsEdited |> should be True
            File.Exists(pdf + ".legibyl.json") |> should be False
            layout |> should contain "✎ your alt, not in the PDF yet: \"Tomato seedlings in a tray\""
            layout |> should contain "✎ your text, not in the PDF yet: \"Garden opens at 8\""
            header |> should contain "Edits (2)"
            editsTab |> List.filter ((=) "Not in this PDF yet: Export… writes it to a copy.") |> List.length |> should equal 2))

[<Fact>]
let ``an edit the PDF already has shows as in it, and can be removed`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            Reviews.saveEdits pdf (RegionEdits.setAlt [] 1 "Figure" logo "Tomato seedlings in a tray")
            let window = show (structure "Tomato seedlings in a tray") pdf
            let layout = texts window
            selectTab window "Edits"
            let status = texts window
            (descendants<Button> window |> List.find (fun b -> buttonText b = "Remove")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let header = descendants<TabItem> window |> List.map (fun t -> string t.Header)
            window.Close()

            layout |> List.filter (fun t -> t.StartsWith "✎") |> should be Empty
            status |> should contain "In this PDF."
            window.Review.Edits |> should be Empty
            header |> should contain "Edits"))

[<Fact>]
let ``an edit tells whoever opened the window, so it can count them`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let mutable told = 0
            let window =
                StructureTreeWindow(Tagged(structure null), pdf, Reviews.read pdf, (fun () -> told <- told + 1), Width = 900.0, Height = 700.0)
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
    descendants<Button> window |> List.tryFind (fun b -> (buttonText b).StartsWith "Merge ")

let private convertTableButton (window: Window) =
    descendants<Button> window |> List.tryFind (fun b -> (buttonText b).StartsWith "Convert " && (buttonText b).EndsWith " to Table…")

[<Fact>]
let ``consecutive selected paragraphs offer a deliberate table conversion`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show lines pdf
            let blocks = paragraphs window
            click blocks[0] false
            click blocks[1] true
            let offered = convertTableButton window |> Option.map buttonText
            window.Close()

            offered |> should equal (Some "Convert 2 Paragraphs to Table…")))

[<Fact>]
let ``a saved paragraph table conversion can be reviewed or removed`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let selected =
                lines.Children[0].Children
                |> Seq.take 2
                |> Seq.map (fun node -> nonNull node.Geometry)
                |> List.ofSeq
            let review = Reviews.read pdf
            review.Edits <- RegionEdits.setParagraphsAsTable [] selected [ { Tag = "TH"; Box = rect 72.0 685.0 150.0 712.0 } ]
            let window = StructureTreeWindow(Tagged lines, pdf, review, ignore, Width = 900.0, Height = 700.0)
            window.Show()
            pump ()
            let blocks = paragraphs window
            click blocks[0] false
            click blocks[1] true
            let actions = descendants<Button> window |> List.map buttonText
            (descendants<Button> window |> List.find (fun button -> buttonText button = "Don't Convert to Table")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let remaining = window.Review.Edits
            window.Close()

            actions |> should contain "Check Table Conversion…"
            actions |> should contain "Don't Convert to Table"
            remaining |> should be Empty))

[<Fact>]
let ``shift-clicking selects the paragraphs between, which can be merged as an edit`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show lines pdf
            let blocks = paragraphs window
            click blocks[0] false
            let hint = texts window
            click blocks[2] true
            let offer = mergeButton window |> Option.map buttonText
            (mergeButton window |> Option.get).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let layout = texts window
            selectTab window "Edits"
            let editsTab = texts window
            window.Close()

            hint |> should contain "Shift-click the paragraphs after it to merge them into one."
            offer |> should equal (Some "Merge 3 Paragraphs")
            window.Review.Edits |> List.map (fun e -> e.Tag, e.MergeWith.Length) |> should equal [ "P", 2 ]
            layout |> should contain "✎ merged with the next 2 paragraphs, not in the PDF yet"
            layout |> List.filter ((=) "✎ merged into the paragraph before, not in the PDF yet") |> List.length |> should equal 2
            editsTab |> should contain "merge: 3 paragraphs into one, from \"P text\""
            editsTab |> should contain "Not in this PDF yet: Export… writes it to a copy."))

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
            let undo = descendants<Button> window |> List.find (fun b -> buttonText b = "Don't Merge These")
            undo.RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let offer = mergeButton window |> Option.map buttonText
            window.Close()

            window.Review.Edits |> should be Empty
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

            window.Review.Edits |> List.map (fun e -> e.Tag, e.Retag) |> should equal [ "P", Some "H1" ]
            layout |> should contain "✎ your tag, not in the PDF yet: \"H1\""
            editsTab |> should contain "tag: H1"
            editsTab |> should contain "Not in this PDF yet: Export… writes it to a copy."))

[<Fact>]
let ``the tag picker retags the block selected now, not one selected before`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show lines pdf
            let blocks = paragraphs window
            click blocks[0] false
            click blocks[1] false
            (tagPicker window |> Option.get).SelectedItem <- "Caption"
            pump ()
            // Moving on sets the picker to the next one's tag, which retags neither.
            click blocks[2] false
            click blocks[0] false
            window.Close()

            let second = nonNull (nonNull blocks[1].Tag :?> StructureNode).Geometry
            window.Review.Edits |> List.map (fun e -> e.Box, e.Retag) |> should equal [ second, Some "Caption" ]))

[<Fact>]
let ``a retag the PDF already has shows as in it, and figures are only offered Figure or Artifact`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let paragraph = rect 72.0 697.0 238.0 712.0
            Reviews.saveEdits pdf (RegionEdits.setRetag [] 1 "P" paragraph "H1")
            let retagged = node "StructTreeRoot" null [ node "Document" null [ node "H1" paragraph []; node "Figure" logo [] ] ]
            let window = show retagged pdf
            let heading = descendants<Border> window |> List.find (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "H1" | _ -> false)
            click heading false
            let picked = tagPicker window |> Option.map (fun c -> string c.SelectedItem)
            click (blockFor window "Figure") false
            let forFigure = tagPicker window |> Option.map (fun c -> c.Items |> Seq.map string |> List.ofSeq)
            selectTab window "Edits"
            let editsTab = texts window
            window.Close()

            picked |> should equal (Some "H1")
            forFigure |> should equal (Some [ "Figure"; "Artifact" ])
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
            let items = menuItems (blockFor window "Link") |> List.map fst |> List.filter (fun t -> t.StartsWith "Edit ")
            (menuItems (blockFor window "Link") |> List.find (fst >> (=) "Edit Link Description…") |> snd).RaiseEvent(RoutedEventArgs MenuItem.ClickEvent)
            pump ()
            let prompt = window.OwnedWindows |> Seq.exactlyOne
            let prefilled = (descendants<TextBox> prompt |> List.exactlyOne).Text
            (descendants<TextBox> prompt |> List.exactlyOne).Text <- "Example (example.com)"
            (descendants<Button> prompt |> List.find (fun b -> buttonText b = "OK")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let layout = texts window
            window.Close()

            items |> should equal [ "Edit Link Description…" ]
            prefilled |> should equal "click here"
            layout |> should contain "description: \"click here\""
            layout |> should contain "✎ your description, not in the PDF yet: \"Example (example.com)\""
            window.Review.Edits |> List.map (fun e -> e.Tag, e.Alt) |> should equal [ "Link", Some "Example (example.com)" ]))

[<Fact>]
let ``a tagged PDF's figure can be tagged Artifact, not read, and Figure again`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show (structure null) pdf
            click (blockFor window "Figure") false
            let picker () = descendants<ComboBox> window |> List.exactlyOne
            let choices = picker().Items |> Seq.map string |> List.ofSeq
            picker().SelectedItem <- "Artifact"
            pump ()
            let recorded = window.Review.Edits |> List.map (fun e -> e.Tag, e.Retag)
            let header = descendants<TabItem> window |> List.map (fun t -> string t.Header)
            let pendingLine = texts window |> List.exists (fun t -> t = "✎ not read (decoration), not in the PDF yet")
            // No button for it: the Tag picker says it.
            let buttons = descendants<Button> window |> List.map buttonText
            picker().SelectedItem <- "Figure"
            pump ()
            let after = window.Review.Edits
            // A paragraph isn't offered Artifact.
            click (blockFor window "P") false
            let forParagraph = descendants<ComboBox> window |> List.collect (fun c -> c.Items |> Seq.map string |> List.ofSeq)
            window.Close()

            choices |> should equal [ "Figure"; "Artifact" ]
            recorded |> should equal [ "Figure", Some "Artifact" ]
            header |> should contain "Edits (1)"
            pendingLine |> should be True
            buttons |> List.exists (fun b -> b.StartsWith "Not Read" || b.StartsWith "Read It") |> should be False
            after |> should be Empty
            forParagraph |> should not' (contain "Artifact")))

[<Fact>]
let ``a tagged PDF's figure's alt text prompt suggests the tag Artifact for decoration`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show (structure null) pdf
            (menuItems (blockFor window "Figure") |> List.find (fst >> (=) "Edit Alt Text…") |> snd).RaiseEvent(RoutedEventArgs MenuItem.ClickEvent)
            pump ()
            let prompt = window.OwnedWindows |> Seq.exactlyOne
            let said = texts prompt
            prompt.Close()
            window.Close()

            said |> List.exists (fun t -> t.Contains "give it the tag Artifact instead") |> should be True))

/// Two small figures side by side, the second with alt text if given.
let private twoFigures (secondAlt: string | null) =
    node "StructTreeRoot" null [
        node "Document" null [
            node "Figure" (rect 72.0 60.0 81.0 71.0) []
            { node "Figure" (rect 82.0 60.0 91.0 71.0) [] with AlternateDescription = secondAlt }
        ]
    ]

[<Fact>]
let ``neighbouring figures can be merged as an edit, but not ones described differently`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let figureBlocks (window: Window) =
                descendants<Border> window |> List.filter (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "Figure" | _ -> false)
            let window = show (twoFigures null) pdf
            let blocks = figureBlocks window
            click blocks[0] false
            click blocks[1] true
            let mergeButton = descendants<Button> window |> List.tryFind (fun b -> buttonText b = "Merge 2 Figures")
            mergeButton.Value.RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let recorded = window.Review.Edits |> List.map (fun e -> e.Tag, e.MergeWith.Length)
            let line = texts window |> List.exists (fun t -> t = "✎ merged with the next figure, not in the PDF yet")
            window.Close()

            let described = show (twoFigures "A pencil") pdf
            let blocks = figureBlocks described
            click blocks[0] false
            click blocks[1] true
            let offered = descendants<Button> described |> List.exists (fun b -> (buttonText b).StartsWith "Merge ")
            described.Close()

            recorded |> should equal [ "Figure", 1 ]
            line |> should be True
            offered |> should be False))

[<Fact>]
let ``several figures are tagged Artifact at once`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show (twoFigures null) pdf
            let figures = descendants<Border> window |> List.filter (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "Figure" | _ -> false)
            click figures[0] false
            click figures[1] true
            let picker = descendants<ComboBox> window |> List.exactlyOne
            let named = string (Automation.AutomationProperties.GetName picker)
            picker.SelectedItem <- "Artifact"
            pump ()
            let recorded = window.Review.Edits |> List.map (fun e -> e.Tag, e.Retag)
            window.Close()

            named |> should equal "Tag for these 2 elements"
            recorded |> should equal [ "Figure", Some "Artifact"; "Figure", Some "Artifact" ]))
/// A page's masthead, left column and right column, tagged in that order, and a paragraph on page 2.
let private columns =
    let paragraph text geometry = { node "P" geometry [] with Content = text }
    node "StructTreeRoot" null [
        node "Document" null [
            paragraph "Masthead" (rect 72.0 700.0 540.0 740.0)
            paragraph "Left column" (rect 72.0 400.0 290.0 680.0)
            paragraph "Right column" (rect 310.0 400.0 540.0 680.0)
            paragraph "Page two" { rect 72.0 600.0 540.0 700.0 with PageNumber = 2 }
        ]
    ]

/// The paragraphs' text, in the order the layout shows them.
let private shownOrder (window: Window) =
    paragraphs window |> List.map (fun b -> (nonNull b.Tag :?> StructureNode).Content |> string)

let private moveButton (window: Window) (label: string) =
    descendants<Button> window |> List.find (fun b -> buttonText b = label)

[<Fact>]
let ``Move Up reads an element one place earlier, as an edit; the Edits tab says so, and Remove puts it back`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show columns pdf
            let right = (paragraphs window)[2]
            click right false
            (moveButton window "Move Up").RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let moved = shownOrder window
            let edits = window.Review.Edits |> List.map (fun e -> string e.Tag, e.Move.IsSome)
            let marked = texts window |> List.filter ((=) "✎ moved in the reading order, not in the PDF yet") |> List.length
            selectTab window "Edits"
            let editsTab = texts window
            for remove in descendants<Button> window |> List.filter (fun b -> buttonText b = "Remove") do
                remove.RaiseEvent(RoutedEventArgs Button.ClickEvent)
                pump ()
            selectTab window "Blocks"
            let restored = shownOrder window
            window.Close()

            moved |> should equal [ "Masthead"; "Right column"; "Left column"; "Page two" ]
            // The fewest moves that give it: the right column after the masthead.
            edits |> should equal [ "P", true ]
            marked |> should equal 1
            editsTab |> should contain "moved: after \"Masthead\""
            editsTab |> should contain "Not in this PDF yet: Export… writes it to a copy."
            restored |> should equal [ "Masthead"; "Left column"; "Right column"; "Page two" ]))

[<Fact>]
let ``an element moves only among its siblings on its page`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show columns pdf
            let masthead = (paragraphs window)[0]
            click masthead false
            let first = (moveButton window "Move Up").IsEnabled, (moveButton window "Move Down").IsEnabled
            let right = (paragraphs window)[2]
            click right false
            let last = (moveButton window "Move Up").IsEnabled, (moveButton window "Move Down").IsEnabled
            (moveButton window "Move Down").RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let order = shownOrder window
            window.Close()

            first |> should equal (false, true)
            // The next is on page 2.
            last |> should equal (true, false)
            order |> should equal [ "Masthead"; "Left column"; "Right column"; "Page two" ]))

[<Fact>]
let ``an element moves past a sibling that has no place on a page, which stays where it is`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let paragraph text geometry = { node "P" geometry [] with Content = text }
            // A figure that draws nothing (no box) between the left column and the masthead.
            let tree =
                node "StructTreeRoot" null [
                    node "Document" null [
                        paragraph "Left column" (rect 72.0 400.0 290.0 680.0)
                        { node "Figure" null [] with Content = "Empty figure" }
                        paragraph "Masthead" (rect 72.0 700.0 540.0 740.0)
                    ]
                ]
            let window = show tree pdf
            let masthead = (paragraphs window)[1]
            click masthead false
            let enabled = (moveButton window "Move Up").IsEnabled
            (moveButton window "Move Up").RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let order =
                descendants<Border> window
                |> List.choose (fun b -> match b.Tag with :? StructureNode as n when n.Tag <> "Document" -> Some(string n.Content) | _ -> None)
            let edits = window.Review.Edits |> List.map (fun e -> e.Tag, e.Move)
            window.Close()

            enabled |> should be True
            order |> should equal [ "Masthead"; "Left column"; "Empty figure" ]
            edits |> should equal [ "P", Some First ]))
