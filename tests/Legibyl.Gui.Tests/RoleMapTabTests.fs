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
      Kind = (if tag = "StructTreeRoot" then StructureNodeKind.Root else StructureNodeKind.Element)
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
let ``a PDF's own tag named like one of Legibyl's own nodes is listed, and Legibyl's own nodes aren't`` () =
    let outside =
        { node "NotInTags" null null [ { node "Untagged" null (rect 500.0) [] with Kind = StructureNodeKind.Untagged } ] with
            Kind = StructureNodeKind.NotInTags }
    let tree =
        node "StructTreeRoot" null null [ node "Document" null null [ node "Untagged" null (rect 700.0) []; node "NotInTags" null (rect 680.0) [] ]; outside ]
    StructureRoleMapTab.names (fun _ -> true) tree |> should equal [ "Untagged", 1, (None: string option); "NotInTags", 1, None ]

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

/// Selects the block of the element with tag, as Enter on it does.
let private selectBlock (window: Window) (tag: string) =
    let block = descendants<Border> window |> List.find (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = tag | _ -> false)
    block.Focus() |> ignore
    block.RaiseEvent(Input.KeyEventArgs(RoutedEvent = Input.InputElement.KeyDownEvent, Key = Input.Key.Enter, Source = block))
    pump ()

[<Fact>]
let ``a tag chosen in the Role map tab gives its elements the Tag picker, as what they'll be heard as`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show structure pdf
            selectBlock window "Zorp"
            let before = descendants<ComboBox> window |> List.exists (fun c -> string (Automation.AutomationProperties.GetName c) = "Tag for this <Zorp>")
            selectTab window "Role map"
            (picker window "Standard tag for Zorp").SelectedItem <- "P"
            pump ()
            selectTab window "Blocks"
            selectBlock window "Zorp"
            let shown = string (picker window "Tag for this <Zorp>").SelectedItem
            window.Close()

            before |> should be False
            shown |> should equal "Zorp → P"))

[<Fact>]
let ``an element mapped to Figure is a figure: Figure or Artifact, and alt text`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show (node "StructTreeRoot" null null [ node "Document" null null [ node "Image" "Figure" (rect 700.0) [] ] ]) pdf
            selectBlock window "Image"
            let tag = picker window "Tag for this <Image>"
            let choices = tag.Items |> Seq.map string |> List.ofSeq
            let buttons =
                descendants<Button> window
                |> List.map (fun b -> match Automation.AutomationProperties.GetName b with null | "" -> string b.Content | name -> name)
            window.Close()

            choices |> should equal [ "Image → Figure"; "Figure"; "Artifact" ]
            buttons |> should contain "Edit Alt Text…"))

[<Fact>]
let ``a review's choice of a tag that isn't standard shows as not chosen`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = show structure pdf
            window.Review.RoleMap <- Map.ofList [ "Zorp", "Paragraph" ]
            selectTab window "Role map"
            let shown = string (picker window "Standard tag for Zorp").SelectedItem
            window.Close()

            shown |> should equal "Not chosen"))

[<Fact>]
let ``only names the role map is for are listed`` () =
    StructureRoleMapTab.names (fun name -> name <> "Zorp") structure
    |> List.map (fun (name, uses, _) -> name, uses)
    |> should equal [ "Heading1", 1; "Body Text", 2 ]
