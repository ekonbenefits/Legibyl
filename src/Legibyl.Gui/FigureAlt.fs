/// Alt text written for figures in Suggest Structure, kept in the "<name>.pdf.legibyl.json" file
/// next to the PDF (with any remembered dismissals). A figure is found again by its page, its
/// suggested tag and its box, so the text still applies when the layout is suggested again,
/// even for a regenerated PDF, as long as the figure is in about the same place.
module Legibyl.Gui.FigureAlt

open System
open Legibyl.Validation
open Legibyl.Gui.Dismissals

type Figure =
    { Page: int
      Tag: string
      Box: StructureGeometry
      Alt: string }

/// How much two boxes must overlap to be the same figure: their intersection over their
/// union. A regenerated PDF can move things a little; different figures rarely overlap this much.
[<Literal>]
let SameFigureOverlap = 0.6

let private area (box: StructureGeometry) = max 0.0 (box.Right - box.Left) * max 0.0 (box.Top - box.Bottom)

/// The intersection over union of two boxes.
let overlap (a: StructureGeometry) (b: StructureGeometry) =
    let width = min a.Right b.Right - max a.Left b.Left
    let height = min a.Top b.Top - max a.Bottom b.Bottom
    let intersection = max 0.0 width * max 0.0 height
    let union = area a + area b - intersection
    if union <= 0.0 then 0.0 else intersection / union

/// The saved figure that's this one: same page and tag, overlapping most.
let find (figures: Figure list) (page: int) (tag: string) (box: StructureGeometry) : Figure option =
    figures
    |> List.filter (fun f -> f.Page = page && f.Tag = tag)
    |> List.map (fun f -> f, overlap f.Box box)
    |> List.filter (fun (_, o) -> o >= SameFigureOverlap)
    |> List.sortByDescending snd
    |> List.tryHead
    |> Option.map fst

/// figures with this figure's alt text set (replacing its entry, if it has one), or removed
/// if alt is blank.
let set (figures: Figure list) (page: int) (tag: string) (box: StructureGeometry) (alt: string) : Figure list =
    let others =
        match find figures page tag box with
        | Some existing -> figures |> List.filter (fun f -> not (obj.ReferenceEquals(f, existing)))
        | None -> figures
    if String.IsNullOrWhiteSpace alt then
        others
    else
        let figure: Figure = { Page = page; Tag = tag; Box = box; Alt = alt }
        others @ [ figure ]

/// Adds imported figures that aren't already among figures; returns the result and how many
/// were added.
let merge (figures: Figure list) (imported: Figure list) : Figure list * int =
    let added = imported |> List.filter (fun f -> (find figures f.Page f.Tag f.Box).IsNone)
    figures @ added, added.Length

let private ofSaved (saved: SavedEdit) : Figure option =
    match saved.Box with
    | [| left; bottom; right; top |] when not (String.IsNullOrWhiteSpace saved.Alt) ->
        let figure: Figure =
            { Page = saved.Page
              Tag = saved.Tag
              Box = { PageNumber = saved.Page; Left = left; Bottom = bottom; Right = right; Top = top }
              Alt = saved.Alt }
        Some figure
    | _ -> None

/// The figures in a .legibyl.json file, e.g. one saved next to an earlier version of the PDF.
let fromFile (jsonPath: string) : Figure list =
    match readFile jsonPath with
    | Some saved -> saved.Edits |> Option.ofObj |> Option.map (Array.choose ofSaved >> List.ofArray) |> Option.defaultValue []
    | None -> []

/// The figures saved next to pdfPath.
let load (pdfPath: string) = fromFile (filePath pdfPath)

/// Saves figures next to pdfPath, keeping any dismissals in the file. Throws
/// IOException/UnauthorizedAccessException if it can't be written.
let save (pdfPath: string) (figures: Figure list) =
    let toSaved (f: Figure) : SavedEdit =
        { Page = f.Page
          Tag = f.Tag
          Box = [| f.Box.Left; f.Box.Bottom; f.Box.Right; f.Box.Top |] |> Array.map (fun v -> Math.Round(v, 1))
          Alt = f.Alt }
    let saved = figures |> List.map toSaved |> Array.ofList
    writeFile pdfPath { readOrEmpty pdfPath with Version = 1; Edits = (if saved.Length = 0 then null else saved) }
