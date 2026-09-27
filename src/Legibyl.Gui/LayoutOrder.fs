/// The reading order a page's layout suggests for a tagged PDF's elements (Structure's Order by
/// Page Layout): the layout detector's regions come in the page's reading order, and each element
/// takes the place of the first region it holds. A suggestion for a person to accept, page by page;
/// nothing is reordered on its own.
module Legibyl.Gui.LayoutOrder

open Legibyl.Layout
open Legibyl.Validation

/// A page's regions, in its reading order, from the layout detector (whose model must be
/// downloaded; see LayoutModel). Several seconds.
let detect (pdfPath: string) (page: int) : StructureGeometry list =
    use suggester = new LayoutSuggester(pdfPath)
    (suggester.Suggest page).Blocks |> List.sortBy _.Order |> List.map _.Geometry

let private area (box: StructureGeometry) = max 0.0 (box.Right - box.Left) * max 0.0 (box.Top - box.Bottom)

/// How much of region lies in box, 0 to 1.
let private coverage (box: StructureGeometry) (region: StructureGeometry) =
    let box, region = Boxes.visible box, Boxes.visible region
    let width = min box.Right region.Right - max box.Left region.Left
    let height = min box.Top region.Top - max box.Bottom region.Bottom
    if area region <= 0.0 then 0.0 else max 0.0 width * max 0.0 height / area region

/// children (a parent's, in their order now) in the order page's regions give those with content
/// on page: each region, in reading order, goes to the element that holds most of it (half, at
/// least), and each element takes the place of the first it holds. A container (a list, a table)
/// moves as one, by its own box. One that holds no region stays right after the element it
/// follows now. Children on other pages keep their places.
let propose (children: StructureNode list) (page: int) (regions: StructureGeometry list) : StructureNode list =
    let onPage (node: StructureNode) =
        match node.Geometry with
        | NonNull box -> box.PageNumber = page
        | Null -> false
    let here = children |> List.filter onPage
    // The first region (in reading order) each element holds.
    let first = System.Collections.Generic.Dictionary<StructureNode, int>(HashIdentity.Reference)
    for index, region in List.indexed regions do
        let holder =
            here
            |> List.map (fun node -> node, coverage (nonNull node.Geometry) { region with PageNumber = page })
            |> List.filter (fun (_, c) -> c >= 0.5)
            |> List.sortByDescending snd
            |> List.tryHead
        match holder with
        | Some(node, _) when not (first.ContainsKey node) -> first[node] <- index
        | _ -> ()
    // Each element that holds a region, with those after it that hold none; those before any.
    let leading = here |> List.takeWhile (first.ContainsKey >> not)
    let rec groups (nodes: StructureNode list) =
        match nodes with
        | [] -> []
        | head :: rest ->
            let following = rest |> List.takeWhile (first.ContainsKey >> not)
            (head :: following) :: groups (rest |> List.skip following.Length)
    let ordered =
        leading
        @ (groups (here |> List.skip leading.Length) |> List.sortBy (fun group -> first[group.Head]) |> List.concat)
    // Back into the places the page's elements had among all of them.
    let mutable next = ordered
    [ for child in children ->
          if onPage child then
              let placed = next.Head
              next <- next.Tail
              placed
          else
              child ]
