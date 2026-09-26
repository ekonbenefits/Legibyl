/// A document's suggested layout as JSON, to look over and change (a person or an agent: retag
/// a region, drop it, reorder, write alt text, correct OCR'd text) and then tag the document from.
/// The CLI writes it with --suggest-layout and tags from it with --apply-layout.
module Legibyl.Layout.LayoutFile

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open Legibyl.Validation

// CLIMutable gives System.Text.Json the parameterless constructors it needs.

/// A region to tag. Only tag and box are needed; the rest is what the detector found.
[<CLIMutable>]
type SavedRegion =
    { /// Its place in the page's reading order (the regions are tagged in this order).
      [<JsonPropertyName "order">] Order: int
      /// The tag to give it: H1, H (a heading below the title), P, LI, Figure, Table, Caption,
      /// Note, Formula, Lbl, or Artifact (page furniture, not read).
      [<JsonPropertyName "tag">] Tag: string
      /// Left, bottom, right, top, in PDF points.
      [<JsonPropertyName "box">] Box: float array
      /// The detector's name for it, e.g. "paragraph_title" (for information).
      [<JsonPropertyName "label">] Label: string | null
      /// The detector's confidence, 0-1 (for information).
      [<JsonPropertyName "score">] Score: float
      /// The page's text inside it; read by OCR when recognized.
      [<JsonPropertyName "text">] Text: string | null
      /// True when text was read by OCR (it's drawn as outlines or pixels): it's written as the
      /// element's replacement text, so correct it here ("" for nothing to read).
      [<JsonPropertyName "recognized">] Recognized: bool
      /// For an LI: its list's /ListNumbering, e.g. "Disc" or "Decimal".
      [<JsonPropertyName "listNumbering">] ListNumbering: string | null
      /// For a Figure that's a QR code: what it encodes.
      [<JsonPropertyName "qrText">] QrText: string | null
      /// For a Figure: its alt text, to write.
      [<JsonPropertyName "alt">] Alt: string | null }

[<CLIMutable>]
type SavedPage =
    { [<JsonPropertyName "page">] Page: int
      /// False for a page with no text to tag (a scan, say): it needs OCR first.
      [<JsonPropertyName "hasText">] HasText: bool
      /// A rotated page isn't detected yet, so it has no regions.
      [<JsonPropertyName "rotated">] Rotated: bool
      [<JsonPropertyName "regions">] Regions: SavedRegion array }

[<CLIMutable>]
type SavedLayout =
    { [<JsonPropertyName "version">] Version: int
      /// The PDF it was suggested for (its file name).
      [<JsonPropertyName "file">] File: string
      /// The title to give the document; null to leave its title as it is.
      [<JsonPropertyName "title">] Title: string | null
      [<JsonPropertyName "pages">] Pages: SavedPage array }

/// The tags a region can have.
let tags = [ "H1"; "H"; "P"; "LI"; "Figure"; "Table"; "Caption"; "Note"; "Formula"; "Lbl"; "Artifact" ]

let private boxOf (geometry: StructureGeometry) =
    [| geometry.Left; geometry.Bottom; geometry.Right; geometry.Top |] |> Array.map (fun v -> Math.Round(v, 1))

/// The layout suggested for pdfPath's pages, with title (e.g. its own, or its first H1's text).
let ofSuggestions (pdfPath: string) (pages: PageSuggestion list) (title: string option) : SavedLayout =
    { Version = 1
      File = string (Path.GetFileName pdfPath)
      Title = Option.toObj title
      Pages =
        [| for page in pages ->
               { Page = page.PageNumber
                 HasText = page.HasText
                 Rotated = page.Rotated
                 Regions =
                   [| for block in page.Blocks ->
                          { Order = block.Order
                            Tag = block.Tag
                            Box = boxOf block.Geometry
                            Label = block.Label
                            Score = Math.Round(block.Score, 3)
                            Text = block.Text
                            Recognized = block.Recognized
                            ListNumbering = Option.toObj block.ListNumbering
                            QrText = Option.toObj block.QrText
                            Alt = null } |] } |] }

/// A title for the layout: the document's own (or a short first page's), else the first H1
/// the layout found; as Suggest Structure prefills it.
let suggestedTitle (pdfPath: string) (pages: PageSuggestion list) =
    match DocumentTitles.candidates pdfPath with
    | first :: _ -> Some first.Title
    | [] ->
        pages
        |> List.collect _.Blocks
        |> List.tryFind (fun block -> block.Tag = "H1" && not (String.IsNullOrWhiteSpace block.Text))
        |> Option.map _.Text.Trim()

let private jsonOptions =
    JsonSerializerOptions(WriteIndented = true, Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

let toJson (layout: SavedLayout) = JsonSerializer.Serialize(layout, jsonOptions)

/// What's wrong with layout, if anything: one line per problem.
let problems (layout: SavedLayout) : string list =
    [ if isNull (box layout.Pages) then
          "It has no \"pages\"."
      else
          for page in layout.Pages do
              if isNull (box page.Regions) then
                  $"Page {page.Page} has no \"regions\"."
              else
                  for region in page.Regions do
                      let where = $"Page {page.Page}, region {region.Order}"
                      if not (List.contains region.Tag tags) then
                          let known = String.Join(", ", tags)
                          $"{where}: unknown tag \"{region.Tag}\" (one of {known})."
                      if isNull (box region.Box) || region.Box.Length <> 4 then
                          $"{where}: \"box\" needs four numbers: left, bottom, right, top." ]

/// The layout in path, or why it can't be used.
let read (path: string) : Result<SavedLayout, string> =
    try
        match JsonSerializer.Deserialize<SavedLayout>(File.ReadAllText path, jsonOptions) with
        | null -> Error $"{path} has no layout in it."
        | layout ->
            match problems layout with
            | [] -> Ok layout
            | problems -> Error(String.Join(" ", problems))
    with
    | :? JsonException as ex -> Error $"{path} isn't a layout file: {ex.Message}"
    | :? IOException
    | :? UnauthorizedAccessException as ex -> Error $"Couldn't read {path}: {ex.Message}"

/// The regions to tag from: each page's, in reading order, with their alt text, replacement text
/// (a recognized region's text) and the layout's title.
type FileRegions(layout: SavedLayout) =
    let regions (page: SavedPage) : LayoutRegion list =
        [ for region in page.Regions |> Array.sortBy _.Order ->
              { Tag = region.Tag
                Geometry =
                  { PageNumber = page.Page
                    Left = region.Box[0]
                    Bottom = region.Box[1]
                    Right = region.Box[2]
                    Top = region.Box[3] }
                ListNumbering = region.ListNumbering
                ActualText = (if region.Recognized then (match region.Text with NonNull "" | Null -> null | NonNull text -> text) else null)
                QrText = region.QrText
                Alt = region.Alt } ]

    interface IPageLayoutSource with
        member _.PageRegions(_, pageNumber) =
            match layout.Pages |> Array.tryFind (fun page -> page.Page = pageNumber) with
            | Some page -> regions page :> Collections.Generic.IReadOnlyList<_>
            | None -> [] :> Collections.Generic.IReadOnlyList<_>

        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = layout.Title
