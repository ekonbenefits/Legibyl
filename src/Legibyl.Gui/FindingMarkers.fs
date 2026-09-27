/// Which of the validation's findings are about which of the Structure window's blocks, so each
/// block can say it has failures or warnings. They're the file's as validated: an edit that would
/// fix one leaves it marked until the copy with it is checked.
module Legibyl.Gui.FindingMarkers

open System.Collections.Generic
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

/// Whether a finding can be about a block: it has a place on a page, and isn't about content
/// that's in no element.
let private aboutAnElement (finding: RuleViolation) =
    not (isNull finding.Geometry)
    && not (finding.Element |> Option.ofObj |> Option.exists (fun e -> e.StartsWith("content not in the tags", System.StringComparison.Ordinal)))

/// The findings on each block, failures first: each on the smallest element whose region holds
/// most of the finding's (a figure's own, rather than the section it's in). Those on no block
/// (document-wide, like the metadata, or on content not in the tags) are left out: the main
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
            if aboutAnElement finding then
                let region = nonNull finding.Geometry
                nodes
                |> List.filter (fun n -> coverage (nonNull n.Geometry) region >= 0.5)
                |> List.sortBy (fun n -> area (nonNull n.Geometry))
                |> List.tryHead
            else
                None
        match target with
        | Some node ->
            let existing = match onNodes.TryGetValue node with | true, list -> list | _ -> []
            onNodes[node] <- existing @ [ finding ]
        | None -> ()
    for node in List.ofSeq onNodes.Keys do
        onNodes[node] <- onNodes[node] |> List.sortBy (fun f -> if f.Severity = FindingSeverity.Failure then 0 else 1)
    onNodes
