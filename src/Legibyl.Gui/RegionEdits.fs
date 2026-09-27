/// Changes made to suggested regions (an untagged PDF's structure in View Structure): a figure's alt text, and corrected
/// OCR text; and the same, plus paragraphs merged, to elements in View Structure. Part of the
/// PDF's review (see DocumentReview), which Save keeps in the "edits" section of the
/// "<name>.pdf.legibyl.json" file next to the PDF. A region is found again by its page, its suggested tag and
/// its box, so an edit still applies when the layout is suggested again, even for a regenerated
/// PDF, as long as the region is in about the same place.
module Legibyl.Gui.RegionEdits

open System
open System.IO
open Legibyl.Layout
open Legibyl.Validation
open Legibyl.Validation.ReviewFile

type Edit =
    { Page: int
      Tag: string
      Box: StructureGeometry
      /// A figure's alt text.
      Alt: string option
      /// A region's OCR text, corrected; "" for nothing to read.
      Text: string option
      /// Paragraphs merged into this one (a P), by box: its neighbours after it, in order.
      MergeWith: StructureGeometry list
      /// The tag to give it instead of the one it was found with.
      Retag: string option
      /// For a suggested region: merge it into its neighbour (its text joins the nearest region kept).
      Exclude: bool }

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

/// The edit for this region or element (not a merge): same page and tag (the one it was found
/// with, or, once retagged, the one it was given), overlapping most.
let find (edits: Edit list) (page: int) (tag: string) (box: StructureGeometry) : Edit option =
    edits
    |> List.filter (fun e -> e.Page = page && (e.Tag = tag || e.Retag = Some tag) && e.MergeWith.IsEmpty)
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
        change (
            existing
            |> Option.defaultValue { Page = page; Tag = tag; Box = box; Alt = None; Text = None; MergeWith = []; Retag = None; Exclude = false }
        )
    if edited.Alt.IsNone && edited.Text.IsNone && edited.Retag.IsNone && not edited.Exclude then others
    else others @ [ { edited with Box = box } ]

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

/// A suggested region merged into its neighbour, or kept as its own again.
let setExcluded edits page tag box (exclude: bool) =
    update edits page tag box (fun e -> { e with Exclude = exclude })

/// A suggested region or an element (found as tag) given another tag, or its own again.
let setRetag edits page (tag: string) box (newTag: string) =
    update edits page tag box (fun e -> { e with Retag = (if newTag = e.Tag then None else Some newTag) })

/// Whether the suggestion is merged into its neighbour.
let isExcluded edits page (block: SuggestedBlock) =
    find edits page block.Tag block.Geometry |> Option.exists _.Exclude

/// The tag the suggestion gets: the one it's given, else the one it was found with.
let tagOf edits page (block: SuggestedBlock) =
    find edits page block.Tag block.Geometry |> Option.bind _.Retag |> Option.defaultValue block.Tag

/// The figure's alt text, if written.
let altFor edits page (block: SuggestedBlock) =
    find edits page block.Tag block.Geometry |> Option.bind _.Alt

/// The region's text: corrected if it was, else what OCR read (only for regions OCR read).
let textFor edits page (block: SuggestedBlock) =
    if not block.Recognized then None
    else find edits page block.Tag block.Geometry |> Option.bind _.Text |> Option.orElse (Some block.Text)

/// The edits as the ApplyEdits fix takes them.
let toElementEdits (edits: Edit list) : ElementEdit list =
    [ for e in edits ->
          ({ Page = e.Page
             Tag = e.Tag
             Box = e.Box
             Alt = Option.toObj e.Alt
             Text = Option.toObj e.Text
             MergeWith = e.MergeWith
             Retag = Option.toObj e.Retag
             Exclude = e.Exclude }
          : ElementEdit) ]

/// A page's suggestions as tagging takes them: with the edits layered on (see
/// ElementEdits.onRegions).
let onRegions (edits: Edit list) (blocks: SuggestedBlock list) : LayoutRegion list =
    ElementEdits.onRegions (toElementEdits edits) (LayoutSuggestions.toRegions blocks)

/// The merge of paragraphs (boxes, first to last) on page, added to edits. Merges it overlaps
/// are replaced: a paragraph is merged once.
let mergeParagraphs (edits: Edit list) (page: int) (boxes: StructureGeometry list) : Edit list =
    let same (a: StructureGeometry) (b: StructureGeometry) = overlap a b >= SameRegionOverlap
    let involves (e: Edit) =
        e.Page = page && not e.MergeWith.IsEmpty && (e.Box :: e.MergeWith) |> List.exists (fun box -> boxes |> List.exists (same box))
    (edits |> List.filter (not << involves))
    @ [ { Page = page; Tag = "P"; Box = boxes.Head; Alt = None; Text = None; MergeWith = boxes.Tail; Retag = None; Exclude = false } ]

/// The merge that starts at box on page, if any.
let findMerge (edits: Edit list) (page: int) (box: StructureGeometry) : Edit option =
    edits |> List.tryFind (fun e -> e.Page = page && not e.MergeWith.IsEmpty && overlap e.Box box >= SameRegionOverlap)

/// Adds imported edits for regions that have none here; returns the result and how many were added.
let merge (edits: Edit list) (imported: Edit list) : Edit list * int =
    let isNew (e: Edit) =
        if e.MergeWith.IsEmpty then (find edits e.Page e.Tag e.Box).IsNone else (findMerge edits e.Page e.Box).IsNone
    let added = imported |> List.filter isNew
    edits @ added, added.Length

/// An edit as the review file and the repairs have it (see ElementEdit), for the window.
let ofElementEdit (e: ElementEdit) : Edit =
    { Page = e.Page
      Tag = e.Tag
      Box = e.Box
      Alt = Option.ofObj e.Alt
      Text = Option.ofObj e.Text
      MergeWith = e.MergeWith
      Retag = Option.ofObj e.Retag
      Exclude = e.Exclude }

/// The edits in a review.
let editsIn (saved: SavedFile) : Edit list = ReviewFile.edits saved |> List.map ofElementEdit

let private editsOf (saved: SavedFile option) : Edit list = saved |> Option.map editsIn |> Option.defaultValue []

/// The edits in a .legibyl.json file, e.g. one saved next to an earlier version of the PDF.
let fromFile (jsonPath: string) : Edit list = editsOf (readFile jsonPath)

/// The edits saved for pdfPath: in the file next to it, or else inside it (a copy Legibyl saved).
let load (pdfPath: string) = editsOf (ReviewFile.readFor pdfPath)

/// The edits to import from path: a .legibyl.json file, or a PDF, whose own edits are those
/// saved for it (in the file next to it, or inside it; see load).
let fromFileOrPdf (path: string) : Edit list =
    if String.Equals(Path.GetExtension path, ".pdf", StringComparison.OrdinalIgnoreCase) then load path
    else fromFile path

