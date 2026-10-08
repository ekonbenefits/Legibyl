/// Structure…'s Role map tab: a tagged PDF's own tag names that its elements use, how many
/// use each, what its role map says each is (or that it says nothing), and a picker of standard
/// tags to choose for each, which Export… writes to the role map (see RoleMaps.choose). For the
/// names the Map the PDF's own tag names fix can't guess, this is the way to map them.
module internal Legibyl.Gui.StructureRoleMapTab

open System
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open Legibyl.Validation


/// A tagged PDF's own tag names its elements use, in the order first used, of those inRoleMap
/// says its role map is for (not a PDF 2.0 file's names in another namespace, which its own map
/// is for): each with how many use it, and the tag its role map gives it (null: none).
let names (inRoleMap: string -> bool) (root: StructureNode) : (string * int * string option) list =
    let found = Collections.Generic.Dictionary<string, int * string option>()
    let order = ResizeArray<string>()
    let rec walk (node: StructureNode) =
        // Legibyl's own nodes aren't tags; "?" is an element with no tag at all.
        if node.Kind = StructureNodeKind.Element && node.Tag <> "?" && not (Tags.standard.Contains node.Tag) && inRoleMap node.Tag then
            match found.TryGetValue node.Tag with
            | true, (uses, role) -> found[node.Tag] <- (uses + 1, role |> Option.orElse (Option.ofObj node.Role))
            | _ ->
                found[node.Tag] <- (1, Option.ofObj node.Role)
                order.Add node.Tag
        for child in node.Children do
            walk child
    walk root
    [ for name in order -> let uses, role = found[name] in name, uses, role ]

/// The standard tags offered, the commonest first.
let tags =
    let common =
        [ Tags.H1; Tags.H2; Tags.H3; Tags.H4; Tags.H5; Tags.H6; Tags.H; Tags.P; Tags.L; Tags.LI; Tags.Lbl; Tags.LBody; Tags.Caption; Tags.Note; Tags.Figure; Tags.Formula
          Tags.Table; Tags.TR; Tags.TH; Tags.TD; Tags.THead; Tags.TBody; Tags.TFoot; Tags.Link; Tags.Span; Tags.Quote; Tags.BlockQuote; Tags.Code
          Tags.TOC; Tags.TOCI; Tags.Index; Tags.Sect; Tags.Div; Tags.Art; Tags.Part; Tags.Document; Tags.NonStruct ]
    common @ (Tags.standard |> Set.toList |> List.filter (fun tag -> not (List.contains tag common)))

/// What the tab needs: the structure, the review its choices go in, and what to call when one's
/// made.
type Needs =
    { Root: StructureNode
      /// Whether a name is one the PDF's role map is for (see names).
      InRoleMap: string -> bool
      Review: DocumentReview
      OnChanged: unit -> unit }

/// What's said of a name: what the PDF maps it to, or what the fix would, or that it can't tell.
let private said (role: string option) (name: string) =
    match role, RoleMaps.guess name with
    | Some role, _ -> $"The PDF maps it to {role}."
    | None, Some guess -> $"Not mapped. Map the PDF's own tag names, in Auto Fixes…, would map it to {guess}."
    | None, None -> "Not mapped, and its name doesn't say what it is: choose the tag a screen reader should hear."

let view (needs: Needs) : IView =
    Component.create (
        "role-map",
        fun ctx ->
            let chosen = ctx.useState needs.Review.RoleMap
            let choose (name: string) (tag: string option) =
                let next =
                    match tag with
                    | Some tag -> chosen.Current.Add(name, tag)
                    | None -> chosen.Current.Remove name
                if next <> chosen.Current then
                    needs.Review.RoleMap <- next
                    chosen.Set next
                    needs.OnChanged()
            let rows = names needs.InRoleMap needs.Root
            ScrollViewer.create [
                ScrollViewer.content (
                    StackPanel.create [
                        StackPanel.margin 8.0
                        StackPanel.children [
                            yield
                                TextBlock.create [
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    TextBlock.margin (0.0, 0.0, 0.0, 10.0)
                                    Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                    TextBlock.text
                                        "The PDF's own tag names, as its elements use them. A screen reader hears each as the standard tag the role map gives it, so each needs one. Choose one here, and Export… writes it; or Map the PDF's own tag names, in Auto Fixes…, maps those whose names say what they are."
                                ]
                            for name, uses, role in rows ->
                                let current = chosen.Current.TryFind name
                                let first = match role with Some role -> $"As in the PDF ({role})" | None -> "Not chosen"
                                let items = first :: tags
                                Grid.create [
                                    Grid.columnDefinitions "*,Auto"
                                    Grid.margin (0.0, 0.0, 0.0, 10.0)
                                    Grid.children [
                                        StackPanel.create [
                                            StackPanel.children [
                                                TextBlock.create [
                                                    TextBlock.text (if uses = 1 then $"{name}  (1 element)" else $"{name}  ({uses} elements)")
                                                    TextBlock.fontWeight FontWeight.SemiBold
                                                ]
                                                TextBlock.create [
                                                    TextBlock.text (said role name)
                                                    TextBlock.textWrapping TextWrapping.Wrap
                                                    Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                                ]
                                            ]
                                        ]
                                        ComboBox.create [
                                            Grid.column 1
                                            ComboBox.minWidth 160.0
                                            ComboBox.verticalAlignment VerticalAlignment.Top
                                            ComboBox.dataItems items
                                            // (A hand-edited review's tag that isn't standard shows as not chosen.)
                                            ComboBox.selectedIndex (
                                                match current |> Option.bind (fun tag -> List.tryFindIndex ((=) tag) tags) with
                                                | Some index -> 1 + index
                                                | None -> 0
                                            )
                                            AttrBuilder<ComboBox>.CreateProperty<string | null>(
                                                Automation.AutomationProperties.NameProperty,
                                                $"Standard tag for {name}",
                                                ValueNone
                                            )
                                            ToolTip.tip "The standard tag a screen reader should hear this name as. Export… writes it to the role map."
                                            ComboBox.onSelectedItemChanged (
                                                (fun item ->
                                                    match item with
                                                    | :? string as picked when picked = first -> choose name None
                                                    | :? string as picked -> choose name (Some picked)
                                                    | _ -> ()),
                                                SubPatchOptions.Always
                                            )
                                        ]
                                    ]
                                ]
                        ]
                    ]
                )
            ]
    )
