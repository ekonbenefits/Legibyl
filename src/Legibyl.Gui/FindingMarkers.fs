/// Which of the validation's findings are about which of the Structure window's blocks, so each
/// block can say it has failures or warnings. They're the file's as validated: an edit that would
/// fix one leaves it marked until the copy with it is checked.
module Legibyl.Gui.FindingMarkers

open System.Collections.Generic
open System.Text.RegularExpressions
open Legibyl.Validation

/// The findings to mark, and how to show one in the main window's results.
type Findings =
    { /// Failures and warnings, those dismissed left out.
      Items: RuleViolation list
      /// Selects it in the main window's results list.
      Show: RuleViolation -> unit }

/// No findings to mark: a structure opened on its own, or a suggested one (its findings are about
/// the untagged file, not the suggested blocks).
let none = { Items = []; Show = ignore }

let private area (g: StructureGeometry) = max 0.0 (g.Right - g.Left) * max 0.0 (g.Top - g.Bottom)

/// How much of region lies in box, 0 to 1; for a region with no area, whether its centre does.
let private coverage (box: StructureGeometry) (region: StructureGeometry) =
    if box.PageNumber <> region.PageNumber then
        0.0
    elif area region <= 0.0 then
        let x = (region.Left + region.Right) / 2.0
        let y = (region.Bottom + region.Top) / 2.0
        if x >= box.Left && x <= box.Right && y >= box.Bottom && y <= box.Top then 1.0 else 0.0
    else
        let width = min box.Right region.Right - max box.Left region.Left
        let height = min box.Top region.Top - max box.Bottom region.Bottom
        max 0.0 width * max 0.0 height / area region

/// Whether a finding is about content that's in no element: it goes on the piece drawn outside
/// the tags there (an "Untagged" node; see UntaggedContent), not on an element.
let private aboutUntagged (finding: RuleViolation) =
    finding.Element |> Option.ofObj |> Option.exists (fun e -> e.StartsWith("content not in the tags", System.StringComparison.Ordinal))

/// veraPDF's path to a structure element ("root/document[0]/StructTreeRoot[0](…)/K[0](…)/K[11](49 0
/// obj SEFigure Figure)"): each element's index among its parent's elements, from the root's.
let private elementPattern =
    Regex(@"^root/document\[0\]/StructTreeRoot\[0\](?:\([^)]*\))?((?:/K\[\d+\](?:\([^)]*\))?)+)$", RegexOptions.Compiled)

/// The element a finding's location names, if it names one there: for one with nowhere on a page
/// (a figure that draws nothing, say), so it's marked all the same.
let private elementNamed (root: StructureNode) (finding: RuleViolation) : StructureNode option =
    match finding.LocationContext with
    | NonNull context ->
        let path = elementPattern.Match context
        if not path.Success then None
        else
            Regex.Matches(path.Groups[1].Value, @"/K\[(\d+)\]")
            |> Seq.map (fun m -> int m.Groups[1].Value)
            |> Seq.fold
                (fun (node: StructureNode option) index ->
                    node |> Option.bind (fun node -> if index < node.Children.Count then Some node.Children[index] else None))
                (Some root)
    | Null -> None

/// The findings on each block, failures first: each on the smallest element whose region holds
/// most of the finding's (a figure's own, rather than the section it's in); one about content
/// not in the tags, on the piece drawn outside them there; one with no region, on the element its
/// location names. Those on no block (document-wide, like the metadata) are left out: the main
/// window's results list them.
let place (root: StructureNode) (findings: RuleViolation list) : IReadOnlyDictionary<StructureNode, RuleViolation list> =
    let rec all (node: StructureNode) : StructureNode seq =
        seq {
            for child in node.Children do
                yield child
                yield! all child
        }
    let nodes = all root |> Seq.filter (fun n -> not (isNull n.Geometry)) |> List.ofSeq
    let onNodes = Dictionary<StructureNode, RuleViolation list>(HashIdentity.Reference)
    for finding in findings do
        let target =
            match finding.Geometry with
            | NonNull region ->
                let untagged = aboutUntagged finding
                nodes
                |> List.filter (fun n -> (n.Tag = "Untagged") = untagged && coverage (nonNull n.Geometry) region >= 0.5)
                |> List.sortBy (fun n -> area (nonNull n.Geometry))
                |> List.tryHead
            | Null -> elementNamed root finding
        match target with
        | Some node ->
            let existing = match onNodes.TryGetValue node with | true, list -> list | _ -> []
            onNodes[node] <- existing @ [ finding ]
        | None -> ()
    for node in List.ofSeq onNodes.Keys do
        onNodes[node] <- onNodes[node] |> List.sortBy (fun f -> if f.Severity = FindingSeverity.Failure then 0 else 1)
    onNodes
