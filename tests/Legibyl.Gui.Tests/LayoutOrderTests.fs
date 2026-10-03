module Legibyl.Gui.Tests.LayoutOrderTests

open System
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation

/// What a button says: its screen-reader name (an edit button's label, after its pencil), else its text.
let private buttonText (b: Avalonia.Controls.Button) =
    match Avalonia.Automation.AutomationProperties.GetName b with
    | null | "" -> string b.Content
    | name -> name

let private rect left bottom right top : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

let internal node (tag: string) (content: string | null) (geometry: StructureGeometry | null) (children: StructureNode list) =
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
      ContentLeafCount = (if children.IsEmpty then 1 else 0)
      ColSpan = 1
      RowSpan = 1
      Children = List.toArray children }

// A two-column page, as a newsletter template exports it: its running header and footer first,
// then the left column (a paragraph, a list, a photo), and the masthead, which spans the page,
// only after that; then the right column. And a paragraph on page 2.
let internal header = node "P" "Author or Publication Name" (rect 60.0 745.0 190.0 758.0) []
let internal footer = node "P" "Page 1" (rect 290.0 30.0 320.0 42.0) []
let internal left = node "P" "Left column" (rect 60.0 400.0 290.0 560.0) []
let internal list =
    node "L" null (rect 60.0 300.0 290.0 390.0) [
        node "LI" "One" (rect 60.0 350.0 290.0 390.0) []
        node "LI" "Two" (rect 60.0 300.0 290.0 340.0) [] ]
let internal photo = node "Figure" null (rect 60.0 100.0 290.0 280.0) []
let internal masthead = node "H1" "Today's news" (rect 60.0 600.0 550.0 720.0) []
let internal right = node "P" "Right column" (rect 310.0 100.0 550.0 560.0) []
let internal nextPage = node "P" "Page two" { rect 60.0 600.0 550.0 700.0 with PageNumber = 2 } []

let internal children = [ header; footer; left; list; photo; masthead; right; nextPage ]

/// The detector's regions for page 1, in its reading order: the header, the masthead, the left
/// column's paragraph and list items, the right column, the footer. (None for the photo.)
let internal regions =
    [ rect 60.0 745.0 190.0 758.0
      rect 62.0 605.0 548.0 715.0
      rect 60.0 400.0 290.0 560.0
      rect 60.0 350.0 290.0 390.0
      rect 60.0 300.0 290.0 340.0
      rect 310.0 100.0 550.0 560.0
      rect 290.0 30.0 320.0 42.0 ]

let private contents (nodes: StructureNode list) = nodes |> List.map (fun n -> match n.Content with NonNull content -> content | Null -> n.Tag)

[<Fact>]
let ``the page's layout puts the masthead first and the columns in order; the list moves as one`` () =
    LayoutOrder.propose children 1 regions
    |> contents
    |> should
        equal
        [ "Author or Publication Name"
          "Today's news"
          "Left column"
          "L"
          // No region: it stays after what it follows now.
          "Figure"
          "Right column"
          "Page 1"
          "Page two" ]

[<Fact>]
let ``elements on other pages keep their places`` () =
    LayoutOrder.propose children 2 [ rect 60.0 600.0 550.0 700.0 ] |> should equal children

[<Fact>]
let ``a page already in its layout's order stays as it is`` () =
    let inOrder = LayoutOrder.propose children 1 regions
    LayoutOrder.propose inOrder 1 regions |> should equal inOrder

// ---- In the Structure window ----

open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.VisualTree
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private button (window: Window) (label: string) =
    descendants<Button> window |> List.tryFind (fun b -> buttonText b = label)

let private press (window: Window) (label: string) =
    (button window label).Value.RaiseEvent(RoutedEventArgs Button.ClickEvent)
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 2.0) (fun () -> false) |> ignore

/// The top-level blocks' text, in the order the layout shows them.
let private shown (window: Window) =
    descendants<Border> window
    |> List.choose (fun b ->
        match b.Tag with
        | :? StructureNode as n when List.exists (fun c -> obj.ReferenceEquals(c, n)) children -> Some(match n.Content with NonNull content -> content | Null -> n.Tag)
        | _ -> None)

[<Fact>]
let ``Order Page 1 by Layout reorders the blocks at once, as moves only for what moves; Restore puts it back`` () =
    onUiThread (fun () ->
        let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
        Directory.CreateDirectory folder |> ignore
        try
            let root = node "StructTreeRoot" null null [ node "Document" null null children ]
            let window = StructureTreeWindow(root, Path.Combine(folder, "news.pdf"), Width = 1000.0, Height = 760.0)
            window.LayoutRegions <- Some(fun page -> if page = 1 then regions else [])
            window.Show()
            pump ()
            let before = shown window
            let restoreAtFirst = (button window "Restore Page 1 Original Order").IsSome
            // The first page is shown to begin with: the button names it.
            press window "Order Page 1 by Layout"
            let after = shown window
            let moved = window.Review.Edits |> List.map (fun e -> e.Tag, e.Move.IsSome)
            press window "Restore Page 1 Original Order"
            let restored = shown window
            let left = window.Review.Edits
            window.Close()

            before |> should equal [ "Author or Publication Name"; "Page 1"; "Left column"; "L"; "Figure"; "Today's news"; "Right column"; "Page two" ]
            restoreAtFirst |> should be False
            after |> should equal [ "Author or Publication Name"; "Today's news"; "Left column"; "L"; "Figure"; "Right column"; "Page 1"; "Page two" ]
            // The masthead first after the header, the footer after the right column: the rest
            // follow on their own.
            moved |> List.sort |> should equal [ "H1", true; "P", true ]
            restored |> should equal before
            left |> should be Empty
        finally
            Directory.Delete(folder, true))
