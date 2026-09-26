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
            editsTab |> List.filter ((=) "Not in this PDF yet: Fix → Apply your edits writes it.") |> List.length |> should equal 2))

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
