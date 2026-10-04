module Legibyl.Gui.Tests.RoleMapTabTests

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private rect top : StructureGeometry =
    { PageNumber = 1; Left = 72.0; Bottom = top - 15.0; Right = 300.0; Top = top }

let private node (tag: string) (role: string | null) (geometry: StructureGeometry | null) (children: StructureNode list) =
    { Tag = tag
      Role = role
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

/// A document of the PDF's own tag names: Heading1 (mapped to H1), Body Text twice (not mapped),
/// and Zorp (not mapped, saying nothing), with a standard P.
let private structure =
    node "StructTreeRoot" null null [
        node "Document" null null [
            node "Heading1" "H1" (rect 720.0) []
            node "Body Text" null (rect 700.0) []
            node "Body Text" null (rect 680.0) []
            node "Zorp" null (rect 660.0) []
            node "P" null (rect 640.0) []
        ]
    ]

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private texts (root: Visual) =
    descendants<TextBlock> root |> List.map (fun t -> string t.Text)

let private withPdf (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try test (Path.Combine(folder, "garden.pdf")) finally Directory.Delete(folder, true)

let private show root pdf =
    let window = StructureTreeWindow(root, pdf, Width = 900.0, Height = 700.0)
    window.Show()
    pump ()
    window

let private selectTab (window: Window) (header: string) =
    let tabs = descendants<TabControl> window |> List.head
    tabs.SelectedItem <- tabs.Items |> Seq.cast<TabItem> |> Seq.find (fun t -> (string t.Header).StartsWith header)
    pump ()

let private picker (window: Window) (name: string) =
    descendants<ComboBox> window |> List.find (fun c -> string (Automation.AutomationProperties.GetName c) = name)

[<Fact>]
let ``the Role map tab lists the PDF's own tag names, what they're mapped to, and takes a choice`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show structure pdf
            selectTab window "Role map"
            let shown = texts window
            let zorp = picker window "Standard tag for Zorp"
            let first = string zorp.SelectedItem
            zorp.SelectedItem <- "P"
            pump ()
            let chosen = window.Review.RoleMap
            // Back to not chosen.
            zorp.SelectedItem <- "Not chosen"
            pump ()
            let cleared = window.Review.RoleMap
            window.Close()

            shown |> should contain "Heading1  (1 element)"
            shown |> should contain "The PDF maps it to H1."
            shown |> should contain "Body Text  (2 elements)"
            shown |> should contain "Not mapped. Map the PDF's own tag names, in Auto Fixes…, would map it to P."
            shown |> should contain "Not mapped, and its name doesn't say what it is: choose the tag a screen reader should hear."
            first |> should equal "Not chosen"
            chosen |> should equal (Map.ofList [ "Zorp", "P" ])
            cleared.IsEmpty |> should be True))

[<Fact>]
let ``a PDF that uses only standard tags has no Role map tab`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show (node "StructTreeRoot" null null [ node "Document" null null [ node "P" null (rect 700.0) [] ] ]) pdf
            let headers = descendants<TabItem> window |> List.map (fun t -> string t.Header)
            window.Close()

            headers |> should not' (contain "Role map")))

[<Fact>]
let ``the Tag picker shows an element of the PDF's own tag name with what it's mapped to`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show structure pdf
            let block =
                descendants<Border> window |> List.find (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "Heading1" | _ -> false)
            block.Focus() |> ignore
            block.RaiseEvent(Input.KeyEventArgs(RoutedEvent = Input.InputElement.KeyDownEvent, Key = Input.Key.Enter, Source = block))
            pump ()
            let tag = picker window "Tag for this <Heading1>"
            let shown = string tag.SelectedItem
            window.Close()

            shown |> should equal "Heading1 → H1"))
