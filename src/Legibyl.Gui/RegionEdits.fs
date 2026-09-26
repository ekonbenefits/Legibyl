/// Changes made to suggested regions in Suggest Structure: a figure's alt text, and corrected
/// OCR text; and the same, plus paragraphs merged, to elements in View Structure. Kept in the
/// "edits" section of the "<name>.pdf.legibyl.json" file next to the PDF
/// (with any remembered dismissals). A region is found again by its page, its suggested tag and
/// its box, so an edit still applies when the layout is suggested again, even for a regenerated
/// PDF, as long as the region is in about the same place.
module Legibyl.Gui.RegionEdits

open System
open System.IO
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
      Text: string option
      /// Paragraphs merged into this one (a P), by box: its neighbours after it, in order.
      MergeWith: StructureGeometry list }

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

/// The edit for this region (its alt text or text, not a merge): same page and tag, overlapping most.
let find (edits: Edit list) (page: int) (tag: string) (box: StructureGeometry) : Edit option =
    edits
    |> List.filter (fun e -> e.Page = page && e.Tag = tag && e.MergeWith.IsEmpty)
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
        change (existing |> Option.defaultValue { Page = page; Tag = tag; Box = box; Alt = None; Text = None; MergeWith = [] })
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

/// The merge of paragraphs (boxes, first to last) on page, added to edits. Merges it overlaps
/// are replaced: a paragraph is merged once.
let mergeParagraphs (edits: Edit list) (page: int) (boxes: StructureGeometry list) : Edit list =
    let same (a: StructureGeometry) (b: StructureGeometry) = overlap a b >= SameRegionOverlap
    let involves (e: Edit) =
        e.Page = page && not e.MergeWith.IsEmpty && (e.Box :: e.MergeWith) |> List.exists (fun box -> boxes |> List.exists (same box))
    (edits |> List.filter (not << involves))
    @ [ { Page = page; Tag = "P"; Box = boxes.Head; Alt = None; Text = None; MergeWith = boxes.Tail } ]

/// The merge that starts at box on page, if any.
let findMerge (edits: Edit list) (page: int) (box: StructureGeometry) : Edit option =
    edits |> List.tryFind (fun e -> e.Page = page && not e.MergeWith.IsEmpty && overlap e.Box box >= SameRegionOverlap)

/// Adds imported edits for regions that have none here; returns the result and how many were added.
let merge (edits: Edit list) (imported: Edit list) : Edit list * int =
    let isNew (e: Edit) =
        if e.MergeWith.IsEmpty then (find edits e.Page e.Tag e.Box).IsNone else (findMerge edits e.Page e.Box).IsNone
    let added = imported |> List.filter isNew
    edits @ added, added.Length

let private ofSaved (saved: SavedEdit) : Edit option =
    let value (text: string | null) = Option.ofObj text
    let geometry (box: float array) =
        match box with
        | [| left; bottom; right; top |] -> Some { PageNumber = saved.Page; Left = left; Bottom = bottom; Right = right; Top = top }
        | _ -> None
    match geometry saved.Box with
    | Some box ->
        let mergeWith =
            match saved.MergeWith with
            | NonNull boxes -> boxes |> Array.choose geometry |> List.ofArray
            | Null -> []
        let edit: Edit =
            { Page = saved.Page
              Tag = saved.Tag
              Box = box
              Alt = value saved.Alt
              Text = value saved.Text
              MergeWith = mergeWith }
        if edit.Alt.IsNone && edit.Text.IsNone && edit.MergeWith.IsEmpty then None else Some edit
    | None -> None

let private editsOf (saved: SavedFile option) : Edit list =
    match saved with
    | Some saved -> saved.Edits |> Option.ofObj |> Option.map (Array.choose ofSaved >> List.ofArray) |> Option.defaultValue []
    | None -> []

/// The edits in a .legibyl.json file, e.g. one saved next to an earlier version of the PDF.
let fromFile (jsonPath: string) : Edit list = editsOf (readFile jsonPath)

/// The edits as the ApplyEdits fix takes them.
let toElementEdits (edits: Edit list) : ElementEdit list =
    [ for e in edits ->
          ({ Page = e.Page
             Tag = e.Tag
             Box = e.Box
             Alt = Option.toObj e.Alt
             Text = Option.toObj e.Text
             MergeWith = e.MergeWith }
          : ElementEdit) ]

/// The edits saved for pdfPath (see Dismissals.readFor).
let load (pdfPath: string) = editsOf (readFor pdfPath)

/// The edits to import from path: a .legibyl.json file, or a PDF, whose own edits are those
/// saved for it (in the file next to it, or inside it; see load).
let fromFileOrPdf (path: string) : Edit list =
    if String.Equals(Path.GetExtension path, ".pdf", StringComparison.OrdinalIgnoreCase) then load path
    else fromFile path

/// The edits and document title saved for pdfPath, read once (reading a review embedded in the
/// PDF opens it).
let loadWithTitle (pdfPath: string) : Edit list * string option =
    let saved = readFor pdfPath
    editsOf saved, saved |> Option.bind (fun saved -> Option.ofObj saved.Title)

let private toSaved (e: Edit) : SavedEdit =
    let saved (box: StructureGeometry) = [| box.Left; box.Bottom; box.Right; box.Top |] |> Array.map (fun v -> Math.Round(v, 1))
    { Page = e.Page
      Tag = e.Tag
      Box = saved e.Box
      Alt = Option.toObj e.Alt
      Text = Option.toObj e.Text
      MergeWith = if e.MergeWith.IsEmpty then null else e.MergeWith |> List.map saved |> Array.ofList }

/// Saves edits next to pdfPath, keeping any dismissals in the file. Throws
/// IOException/UnauthorizedAccessException if it can't be written.
let save (pdfPath: string) (edits: Edit list) =
    let saved = edits |> List.map toSaved |> Array.ofList
    writeFile pdfPath { readOrEmpty pdfPath with Version = 1; Edits = (if saved.Length = 0 then null else saved) }

/// edits moved from the regions they were made for to the elements tagging made of them (the
/// tag it wrote, the box of what the element holds), so in the tagged copy they're edits of its
/// elements. Edits for regions that weren't tagged stay as they are.
let rekey (edits: Edit list) (tagged: TaggedRegion seq) : Edit list =
    let tagged = List.ofSeq tagged
    [ for e in edits ->
          tagged
          |> List.choose (fun t ->
              match t.Box with
              | NonNull box when t.Region.Tag = e.Tag && t.Region.Geometry.PageNumber = e.Page ->
                  Some(t.Tag, box, overlap t.Region.Geometry e.Box)
              | _ -> None)
          |> List.filter (fun (_, _, o) -> o >= SameRegionOverlap)
          |> List.sortByDescending (fun (_, _, o) -> o)
          |> List.tryHead
          |> Option.map (fun (tag, box, _) -> { e with Tag = tag; Box = box })
          |> Option.defaultValue e ]

/// Builds the review a copy of pdfPath carries (see VeraPdfValidator.Review), with dismissed in
/// place of its saved dismissals if given, and its edits moved to the elements tagging made.
let reviewFor (pdfPath: string) (dismissed: Dismissed option) =
    Func<Collections.Generic.IReadOnlyList<TaggedRegion>, string | null>(fun tagged ->
        match reviewFile pdfPath dismissed with
        | Some saved ->
            let moved = rekey (editsOf (Some saved)) tagged
            toJson { saved with Edits = (if moved.IsEmpty then saved.Edits else moved |> List.map toSaved |> Array.ofList) }
        | None -> null)

/// The document title set in Suggest Structure and saved for pdfPath, if any.
let loadTitle (pdfPath: string) : string option =
    readFor pdfPath |> Option.bind (fun saved -> Option.ofObj saved.Title)

/// Saves (or, with None, removes) the document title next to pdfPath, keeping the rest of the file.
let saveTitle (pdfPath: string) (title: string option) =
    writeFile pdfPath { readOrEmpty pdfPath with Version = 1; Title = Option.toObj title }
