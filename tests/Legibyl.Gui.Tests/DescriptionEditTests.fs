module Legibyl.Gui.Tests.DescriptionEditTests

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

let private rect top : StructureGeometry =
    { PageNumber = 1; Left = 72.0; Bottom = top - 20.0; Right = 272.0; Top = top }

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

/// A form's field (first_name, no tooltip) and a sticky note (no description).
let private structure =
    node "StructTreeRoot" null [
        node "Document" null [
            { node "Form" (rect 700.0) [] with FormFieldName = "topmostSubform[0].Page1[0].first_name[0]"; FormFieldType = "Tx" }
            node "Annot" (rect 650.0) []
        ]
    ]

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private withPdf (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try test (Path.Combine(folder, "form.pdf")) finally Directory.Delete(folder, true)

let private blockFor (window: Window) (tag: string) =
    descendants<Border> window |> List.find (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = tag | _ -> false)

let private menuItems (border: Border) = blockMenu border |> List.map (fun item -> string item.Header, item)

[<Fact>]
let ``a field's tooltip and an annotation's description are edited like a link's description`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let window = StructureTreeWindow(structure, pdf, Width = 900.0, Height = 700.0)
            window.Show()
            pump ()
            let fieldItems = menuItems (blockFor window "Form") |> List.map fst |> List.filter (fun t -> t.StartsWith "Edit ")
            let noteItems = menuItems (blockFor window "Annot") |> List.map fst |> List.filter (fun t -> t.StartsWith "Edit ")
            (menuItems (blockFor window "Form") |> List.find (fst >> (=) "Edit Tooltip…") |> snd).RaiseEvent(RoutedEventArgs MenuItem.ClickEvent)
            pump ()
            let prompt = window.OwnedWindows |> Seq.exactlyOne
            let prefilled = (descendants<TextBox> prompt |> List.exactlyOne).Text
            (descendants<Button> prompt |> List.find (fun b -> string b.Content = "OK")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let edits = window.Review.Edits |> List.map (fun e -> e.Tag, e.Alt)
            window.Close()

            fieldItems |> should equal [ "Edit Tooltip…" ]
            noteItems |> should equal [ "Edit Description…" ]
            // Prefilled from the field's name.
            prefilled |> should equal "First name"
            edits |> should equal [ "Form", Some "First name" ]))

[<Fact>]
let ``a field's name is said as a tooltip would`` () =
    [ "first_name"; "EmailAddress"; "topmostSubform[0].Page1[0].dateOfBirth[0]"; "Phone-2" ]
    |> List.map StructureActions.fieldNameText
    |> should equal [ "First name"; "Email address"; "Date of birth"; "Phone 2" ]

[<Fact>]
let ``text with icon characters offers replacement text, prefilled without them, and saves none`` () =
    onUiThread (fun () ->
        withPdf (fun pdf ->
            let icons =
                node "StructTreeRoot" null [
                    node "Document" null [
                        { node "P" (rect 700.0) [] with Content = "Call  555-0100" }
                        { node "P" (rect 650.0) [] with Content = "Plain text" }
                    ]
                ]
            let window = StructureTreeWindow(icons, pdf, Width = 900.0, Height = 700.0)
            window.Show()
            pump ()
            let blocks = descendants<Border> window |> List.filter (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "P" | _ -> false)
            let items (block: Border) = menuItems block |> List.map fst |> List.filter (fun t -> t.StartsWith "Edit ")
            let withIcons, plain = items blocks[0], items blocks[1]
            (menuItems blocks[0] |> List.find (fst >> (=) "Edit Replacement Text…") |> snd).RaiseEvent(RoutedEventArgs MenuItem.ClickEvent)
            pump ()
            let prompt = window.OwnedWindows |> Seq.exactlyOne
            let box = descendants<TextBox> prompt |> List.exactlyOne
            let prefilled = box.Text
            box.Text <- " Phone: 555-0100"
            pump ()
            (descendants<Button> prompt |> List.find (fun b -> string b.Content = "OK")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()
            let edits = window.Review.Edits |> List.map (fun e -> e.Tag, e.Text)
            window.Close()

            withIcons |> should equal [ "Edit Replacement Text…" ]
            plain |> should be Empty
            prefilled |> should equal "Call 555-0100"
            edits |> should equal [ "P", Some "Phone: 555-0100" ]))

[<Fact>]
let ``a lone surrogate takes nothing else with it`` () =
    // (A lone surrogate can't be a literal: the compiler makes it U+FFFD.)
    [ "Call \uE001 555"; "A \U000F0001 B"; String([| 'A'; ' '; char 0xDB80; 'B' |]) ]
    |> List.map StructureActions.withoutIconCharacters
    |> should equal [ "Call 555"; "A B"; "A B" ]
