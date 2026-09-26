/// Changes made to suggested regions in Suggest Structure: a figure's alt text, and corrected
/// OCR text. Kept in the "edits" section of the "<name>.pdf.legibyl.json" file next to the PDF
/// (with any remembered dismissals). A region is found again by its page, its suggested tag and
/// its box, so an edit still applies when the layout is suggested again, even for a regenerated
/// PDF, as long as the region is in about the same place.
module Legibyl.Gui.RegionEdits

open System
open Legibyl.Layout
open Legibyl.Validation
open Legibyl.Gui.Dismissals

type Edit =
    { Page: int
      Tag: string
      Box: StructureGeometry
      /// A figure's alt text.
      Alt: string option
      /// A region's OCR text, corrected; "" for nothing to read.
      Text: string option }

/// How much two boxes must overlap to be the same region: their intersection over their
/// union. A regenerated PDF can move things a little; different regions rarely overlap this much.
[<Literal>]
let SameRegionOverlap = 0.6

let private area (box: StructureGeometry) = max 0.0 (box.Right - box.Left) * max 0.0 (box.Top - box.Bottom)

/// The intersection over union of two boxes.
let overlap (a: StructureGeometry) (b: StructureGeometry) =
    let width = min a.Right b.Right - max a.Left b.Left
    let height = min a.Top b.Top - max a.Bottom b.Bottom
    let intersection = max 0.0 width * max 0.0 height
    let union = area a + area b - intersection
    if union <= 0.0 then 0.0 else intersection / union

/// The edit for this region: same page and tag, overlapping most.
let find (edits: Edit list) (page: int) (tag: string) (box: StructureGeometry) : Edit option =
    edits
    |> List.filter (fun e -> e.Page = page && e.Tag = tag)
    |> List.map (fun e -> e, overlap e.Box box)
    |> List.filter (fun (_, o) -> o >= SameRegionOverlap)
    |> List.sortByDescending snd
    |> List.tryHead
    |> Option.map fst

/// edits with this region's edit changed by change (from its current one, or an empty one),
/// dropping it if that leaves nothing changed.
let private update (edits: Edit list) page tag box (change: Edit -> Edit) : Edit list =
    let existing = find edits page tag box
    let others =
        match existing with
        | Some e -> edits |> List.filter (fun other -> not (obj.ReferenceEquals(other, e)))
        | None -> edits
    let edited =
        change (existing |> Option.defaultValue { Page = page; Tag = tag; Box = box; Alt = None; Text = None })
    if edited.Alt.IsNone && edited.Text.IsNone then others else others @ [ { edited with Box = box } ]

/// A figure's alt text set, or removed if blank.
let setAlt edits page tag box (alt: string) =
    update edits page tag box (fun e -> { e with Alt = (if String.IsNullOrWhiteSpace alt then None else Some alt) })

/// Alt text for something prefilled (a QR code's description, a link's from its page's title),
/// recorded only if it differs from what was prefilled; "" if cleared.
let setAltAgainst edits page tag box (prefilled: string) (alt: string) =
    update edits page tag box (fun e -> { e with Alt = (if alt = prefilled then None else Some alt) })

/// A region's OCR text corrected, recorded only if it differs from what OCR read.
let setText edits page tag box (ocr: string) (text: string) =
    update edits page tag box (fun e -> { e with Text = (if text = ocr then None else Some text) })

/// The figure's alt text, if written.
let altFor edits page (block: SuggestedBlock) =
    find edits page block.Tag block.Geometry |> Option.bind _.Alt

/// The region's text: corrected if it was, else what OCR read (only for regions OCR read).
let textFor edits page (block: SuggestedBlock) =
    if not block.Recognized then None
    else find edits page block.Tag block.Geometry |> Option.bind _.Text |> Option.orElse (Some block.Text)

/// A suggested region as tagging writes it, with its edits: a figure's alt text, and corrected
/// OCR text as its ActualText ("" meaning there's nothing to read, so none).
let apply (edits: Edit list) (page: int) (block: SuggestedBlock) (region: LayoutRegion) : LayoutRegion =
    let region =
        match altFor edits page block with
        | Some alt when block.Tag = "Figure" && not (String.IsNullOrWhiteSpace alt) -> { region with Alt = alt }
        | _ -> region
    match find edits page block.Tag block.Geometry |> Option.bind _.Text with
    | Some text when block.Recognized -> { region with ActualText = (if String.IsNullOrWhiteSpace text then null else text) }
    | _ -> region

/// Adds imported edits for regions that have none here; returns the result and how many were added.
let merge (edits: Edit list) (imported: Edit list) : Edit list * int =
    let added = imported |> List.filter (fun e -> (find edits e.Page e.Tag e.Box).IsNone)
    edits @ added, added.Length

let private ofSaved (saved: SavedEdit) : Edit option =
    let value (text: string | null) = Option.ofObj text
    match saved.Box with
    | [| left; bottom; right; top |] when not (isNull saved.Alt && isNull saved.Text) ->
        let edit: Edit =
            { Page = saved.Page
              Tag = saved.Tag
              Box = { PageNumber = saved.Page; Left = left; Bottom = bottom; Right = right; Top = top }
              Alt = value saved.Alt
              Text = value saved.Text }
        if edit.Alt.IsNone && edit.Text.IsNone then None else Some edit
    | _ -> None

/// The edits in a .legibyl.json file, e.g. one saved next to an earlier version of the PDF.
let fromFile (jsonPath: string) : Edit list =
    match readFile jsonPath with
    | Some saved -> saved.Edits |> Option.ofObj |> Option.map (Array.choose ofSaved >> List.ofArray) |> Option.defaultValue []
    | None -> []

/// The edits saved next to pdfPath.
let load (pdfPath: string) = fromFile (filePath pdfPath)

/// Saves edits next to pdfPath, keeping any dismissals in the file. Throws
/// IOException/UnauthorizedAccessException if it can't be written.
let save (pdfPath: string) (edits: Edit list) =
    let toSaved (e: Edit) : SavedEdit =
        { Page = e.Page
          Tag = e.Tag
          Box = [| e.Box.Left; e.Box.Bottom; e.Box.Right; e.Box.Top |] |> Array.map (fun v -> Math.Round(v, 1))
          Alt = Option.toObj e.Alt
          Text = Option.toObj e.Text }
    let saved = edits |> List.map toSaved |> Array.ofList
    writeFile pdfPath { readOrEmpty pdfPath with Version = 1; Edits = (if saved.Length = 0 then null else saved) }

/// The document title set in Suggest Structure and saved next to pdfPath, if any.
let loadTitle (pdfPath: string) : string option =
    readFile (filePath pdfPath) |> Option.bind (fun saved -> Option.ofObj saved.Title)

/// Saves (or, with None, removes) the document title next to pdfPath, keeping the rest of the file.
let saveTitle (pdfPath: string) (title: string option) =
    writeFile pdfPath { readOrEmpty pdfPath with Version = 1; Title = Option.toObj title }
