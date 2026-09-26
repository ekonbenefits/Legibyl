/// A document's suggested layout as JSON, to look over and change (a person or an agent: retag
/// a region, drop it, reorder, write alt text, correct OCR'd text) and then tag the document from.
/// The CLI writes it with --suggest-layout and tags from it with --apply-layout; a review file
/// keeps one for an untagged PDF (see ReviewFile). Suggesting one is Legibyl.Layout's.
module Legibyl.Validation.LayoutFile

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

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
      [<JsonPropertyName "alt">] Alt: string | null
      /// Left out of the structure (a person discarded it): its text joins the nearest region
      /// kept, and anything else it holds is decoration. Kept so the choice can be undone.
      [<JsonPropertyName "discarded">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingDefault)>]
      Discarded: bool
      /// For a Figure: a picture of it, a PNG file's path relative to the layout file (written
      /// with --figure-images), to look at when writing its alt text. Not used when tagging.
      [<JsonPropertyName "image">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Image: string | null
      /// For a Figure: the text just before it and just after it in reading order (a caption, the
      /// paragraph about it), since good alt text says why it's there. Not used when tagging.
      [<JsonPropertyName "textBefore">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      TextBefore: string | null
      [<JsonPropertyName "textAfter">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      TextAfter: string | null }

[<CLIMutable>]
type SavedPage =
    { [<JsonPropertyName "page">] Page: int
      /// False for a page with no text to tag (a scan, say): it needs OCR first.
      [<JsonPropertyName "hasText">] HasText: bool
      /// A rotated page isn't detected yet, so it has no regions.
      [<JsonPropertyName "rotated">] Rotated: bool
      /// What the page was when this layout was found (a hash of its size and text): a page
      /// that's changed since (a regenerated PDF, say) is found again rather than tagged from a
      /// layout that no longer fits it. Null to take the layout as it is.
      [<JsonPropertyName "fingerprint">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Fingerprint: string | null
      [<JsonPropertyName "regions">] Regions: SavedRegion array }

[<CLIMutable>]
type SavedLayout =
    { [<JsonPropertyName "version">] Version: int
      /// The PDF it was suggested for (its file name).
      [<JsonPropertyName "file">] File: string
      /// The title to give the document; null to leave its title as it is.
      [<JsonPropertyName "title">] Title: string | null
      [<JsonPropertyName "pages">] Pages: SavedPage array }

/// Each page's fingerprint (see SavedPage.Fingerprint), by page number; none if pdfPath can't
/// be read.
let fingerprints (pdfPath: string) : Map<int, string> =
    if not (File.Exists pdfPath) then
        Map.empty
    else
        try
            use document = UglyToad.PdfPig.PdfDocument.Open pdfPath
            [ for page in document.GetPages() ->
                  let described = $"{page.Width:F1}x{page.Height:F1}\n{page.Text}"
                  let hash = Security.Cryptography.SHA256.HashData(Text.Encoding.UTF8.GetBytes described)
                  page.Number, Convert.ToHexString(hash, 0, 8).ToLowerInvariant() ]
            |> Map.ofList
        with
        | :? IOException
        | :? UnauthorizedAccessException
        | :? UglyToad.PdfPig.Core.PdfDocumentFormatException -> Map.empty

/// layout without the pages that have changed since it was found (their fingerprints don't
/// match pdfPath's pages); pages without a fingerprint are kept.
let current (pdfPath: string) (layout: SavedLayout) : SavedLayout =
    if layout.Pages |> Array.forall (fun page -> isNull page.Fingerprint) then
        layout
    else
        let now = fingerprints pdfPath
        { layout with
            Pages =
                layout.Pages
                |> Array.filter (fun page ->
                    match page.Fingerprint with
                    | NonNull fingerprint -> Map.tryFind page.Page now = Some fingerprint
                    | Null -> true) }

/// The tags a region can have.
let tags = [ "H1"; "H"; "P"; "LI"; "Figure"; "Table"; "Caption"; "Note"; "Formula"; "Lbl"; "Artifact" ]

let private jsonOptions =
    JsonSerializerOptions(WriteIndented = true, Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

let toJson (layout: SavedLayout) = JsonSerializer.Serialize(layout, jsonOptions)

/// The folder a layout file's figure images go in: "scan.layout.json" -> "scan.layout.images",
/// beside it, so deleting the two removes everything.
let imagesFolder (layoutPath: string) =
    let folder = Path.GetDirectoryName(Path.GetFullPath layoutPath) |> string
    Path.Combine(folder, string (Path.GetFileNameWithoutExtension layoutPath) + ".images")

/// Writes a PNG of each figure in layout (rendered by render) into its images folder, emptied of
/// an earlier run's first, and says where each is. Only files named like these are removed.
let withFigureImages (layoutPath: string) (render: StructureGeometry -> byte array) (layout: SavedLayout) : SavedLayout =
    let folder = imagesFolder layoutPath
    if Directory.Exists folder then
        for old in Directory.GetFiles(folder, "p*-r*.png") do
            File.Delete old
    let name = Path.GetFileName folder
    { layout with
        Pages =
            [| for page in layout.Pages ->
                   { page with
                       Regions =
                           [| for region in page.Regions ->
                                  if region.Tag = "Figure" && region.Box.Length = 4 then
                                      Directory.CreateDirectory folder |> ignore
                                      let file = $"p{page.Page}-r{region.Order}.png"
                                      let geometry =
                                          { PageNumber = page.Page
                                            Left = region.Box[0]
                                            Bottom = region.Box[1]
                                            Right = region.Box[2]
                                            Top = region.Box[3] }
                                      File.WriteAllBytes(Path.Combine(folder, file), render geometry)
                                      { region with Image = $"{name}/{file}" }
                                  else
                                      region |] } |] }

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

/// The regions to tag from: each page's that's kept, in reading order, with their alt text,
/// replacement text (a recognized region's text) and the layout's title. A page the layout
/// doesn't have comes from fallback, if given (e.g. the layout model's, for pages a review
/// hasn't reached), else has none.
type FileRegions(layout: SavedLayout, fallback: IPageLayoutSource option) =
    let regions (page: SavedPage) : LayoutRegion list =
        [ for region in page.Regions |> Array.filter (not << _.Discarded) |> Array.sortBy _.Order ->
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

    new(layout: SavedLayout) = FileRegions(layout, None)

    interface IPageLayoutSource with
        member _.PageRegions(pdfPath, pageNumber) =
            match layout.Pages |> Array.tryFind (fun page -> page.Page = pageNumber), fallback with
            | Some page, _ -> regions page :> Collections.Generic.IReadOnlyList<_>
            | None, Some fallback -> fallback.PageRegions(pdfPath, pageNumber)
            | None, None -> [] :> Collections.Generic.IReadOnlyList<_>

        member _.LinkDescription(pdfPath, pageNumber, rectangle) =
            match fallback with
            | Some fallback -> fallback.LinkDescription(pdfPath, pageNumber, rectangle)
            | None -> null

        member _.PageTitles =
            match fallback with
            | Some fallback -> fallback.PageTitles
            | None -> null

        member _.DocumentTitle =
            match layout.Title, fallback with
            | NonNull title, _ -> title
            | Null, Some fallback -> fallback.DocumentTitle
            | Null, None -> null

/// Whether layout has every one of pageCount pages (so tagging from it needs nothing else).
let covers (layout: SavedLayout) (pageCount: int) =
    [ 1..pageCount ] |> List.forall (fun page -> layout.Pages |> Array.exists (fun p -> p.Page = page))
