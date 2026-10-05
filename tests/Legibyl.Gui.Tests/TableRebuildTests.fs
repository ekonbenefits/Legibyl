module Legibyl.Gui.Tests.TableRebuildTests

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

let private node (tag: string) (content: string | null) (geometry: StructureGeometry | null) (children: StructureNode list) =
    { Tag = tag
      Role = null
      AlternateDescription = null
      ActualText = null
      Content = content
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

let private tableBox = rect 72.0 648.0 372.0 696.0

/// A table tagged wrongly: its four texts as TDs right in it, no rows.
let private structure =
    node "StructTreeRoot" null null [
        node "Document" null null [
            node "Table" null tableBox [
                node "TD" "Plant" (rect 80.0 676.0 110.0 688.0) []
                node "TD" "Sow" (rect 230.0 676.0 250.0 688.0) []
                node "TD" "Tomato" (rect 80.0 652.0 120.0 664.0) []
                node "TD" "March" (rect 230.0 652.0 265.0 664.0) []
                // Drawn outside every cell found: the nearest one's.
                node "TD" "(dry)" (rect 380.0 652.0 400.0 664.0) []
            ]
        ]
    ]

let private cells =
    [ { Tag = "TD"; Box = rect 72.0 672.0 222.0 696.0 }
      { Tag = "TD"; Box = rect 222.0 672.0 372.0 696.0 }
      { Tag = "TD"; Box = rect 72.0 648.0 222.0 672.0 }
      { Tag = "TD"; Box = rect 222.0 648.0 372.0 672.0 } ]

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private withPdf (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try test (Path.Combine(folder, "garden.pdf")) finally Directory.Delete(folder, true)

let private selectTable (window: Window) =
    let block = descendants<Border> window |> List.find (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "Table" | _ -> false)
    block.Focus() |> ignore
    block.RaiseEvent(Input.KeyEventArgs(RoutedEvent = Input.InputElement.KeyDownEvent, Key = Input.Key.Enter, Source = block))
    pump ()

let private buttonNamed (window: Window) (name: string) =
    descendants<Button> window |> List.tryFind (fun b -> string (Automation.AutomationProperties.GetName b) = name)

[<Fact>]
let ``a tagged table offers its rows and cells rebuilt; one rebuilt shows its grid to check, and keeps the cells checked`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = StructureTreeWindow(structure, pdf, Width = 900.0, Height = 700.0)
            window.Show()
            pump ()
            selectTable window
            let offered = (buttonNamed window "Rebuild Rows and Cells from the Page…").IsSome
            window.Close()
            // A rebuild kept from before (as the model would have found it, all data cells).
            let window = StructureTreeWindow(structure, pdf, Width = 900.0, Height = 700.0)
            let review = window.Review
            review.Edits <- RegionEdits.setCells [] 1 "Table" tableBox cells
            window.Show()
            pump ()
            selectTable window
            (buttonNamed window "Check Rebuilt Rows and Cells…" |> Option.get).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let dialog = window.OwnedWindows |> Seq.exactlyOne
            let shown = descendants<TextBlock> dialog |> List.map (fun t -> string t.Text)
            // The first row's cells made headers.
            for name in [ "Cell in row 1, column 1"; "Cell in row 1, column 2" ] do
                (descendants<ComboBox> dialog |> List.find (fun c -> string (Automation.AutomationProperties.GetName c) = name)).SelectedItem <- "TH"
                pump ()
            (descendants<Button> dialog |> List.find (fun b -> string b.Content = "OK")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let kept = review.Edits |> List.collect _.Cells |> List.map _.Tag
            let tabs = descendants<TabControl> window |> List.head
            tabs.SelectedItem <- tabs.Items |> Seq.cast<TabItem> |> Seq.find (fun t -> (string t.Header).StartsWith "Edits")
            pump ()
            let editsTab = descendants<TextBlock> window |> List.map (fun t -> string t.Text)
            window.Close()

            offered |> should be True
            shown |> should contain "Plant"
            shown |> should contain "March (dry)"
            kept |> should equal [ "TH"; "TH"; "TD"; "TD" ]
            editsTab |> List.exists (fun t -> t.Contains "rows and cells: rebuilt from the page, 4 cells (2 header)") |> should be True))

[<Fact>]
let ``a rebuild that fails says so, rather than doing nothing`` () =
    if not (Legibyl.Layout.TableModel.isDownloaded ()) then
        Assert.Skip "The table model isn't downloaded; open Suggest Structure in the app to fetch it."
    onUiThread (fun () ->
        // No PDF there to find the table's cells in.
        withPdf (fun pdf ->
            let window = StructureTreeWindow(structure, pdf, Width = 900.0, Height = 700.0)
            window.Show()
            pump ()
            selectTable window
            (buttonNamed window "Rebuild Rows and Cells from the Page…" |> Option.get).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) (fun () -> not (Seq.isEmpty window.OwnedWindows)) |> ignore
            let message = window.OwnedWindows |> Seq.tryHead |> Option.map (fun w -> descendants<TextBlock> w |> List.map (fun t -> string t.Text) |> String.concat " ")
            for owned in List.ofSeq window.OwnedWindows do owned.Close()
            window.Close()

            message |> Option.exists (fun text -> text.Contains "The table's rows and cells couldn't be found") |> should be True))
