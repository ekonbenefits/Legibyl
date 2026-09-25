module Legibyl.Gui.Tests.StructureTreeWindowTests

open System
open Avalonia.Controls
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private node (tag: string) (children: StructureNode list) =
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
      Geometry = null
      Language = null
      PageNumber = Nullable()
      ContentLeafCount = 0
      Children = List.toArray children }

/// Document > chapters > verses > one span each.
let private book chapters verses =
    node "StructTreeRoot" [ node "Document" [ for _ in 1 .. chapters -> node "Chapter" [ for _ in 1 .. verses -> node "verse" [ node "Span" [] ] ] ] ]

let private descendants<'T when 'T :> Avalonia.Visual> (window: Window) : 'T list =
    window.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private blocks window =
    descendants<Border> window |> List.filter (fun border -> border.Tag :? StructureNode)

let private expandButtons window =
    descendants<Button> window |> List.filter (fun button -> (string button.Content).StartsWith "▸")

let private show root =
    let window = StructureTreeWindow(root, "unused.pdf")
    window.Show()
    HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
    window

[<Fact>]
let ``small structures are shown in full`` () =
    onUiThread (fun () ->
        let window = show (book 3 4)
        let shown = blocks window |> List.length
        let buttons = expandButtons window
        window.Close()
        buttons |> should be Empty
        // Document + 3 chapters + 12 verses + 12 spans.
        shown |> should equal 28)

[<Fact>]
let ``large structures start collapsed and expand on click`` () =
    onUiThread (fun () ->
        // 10,050 elements: Document, 50 chapters, then 100 verses and spans per chapter.
        let window = show (book 50 100)
        let shownAtFirst = blocks window |> List.length
        let buttons = expandButtons window
        let label = string buttons.Head.Content
        buttons.Head.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs(Button.ClickEvent))
        HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
        let shownAfterExpanding = blocks window |> List.length
        window.Close()
        // Document and chapters fit the budget; adding the 5,000 verses wouldn't.
        shownAtFirst |> should equal 51
        buttons |> should haveLength 50
        label |> should equal "▸ 100 child elements (200 in all)"
        // One chapter's 100 verses and their spans fit the budget together.
        shownAfterExpanding |> should equal (51 + 200))
