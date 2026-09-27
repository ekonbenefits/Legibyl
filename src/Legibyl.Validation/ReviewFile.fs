/// A PDF's review, as Legibyl keeps it: the dismissed warnings, the edits made in Suggest
/// Structure and View Structure, and the title, in a "<name>.pdf.legibyl.json" file next to the
/// PDF, and inside the copies Legibyl saves (see EmbeddedReview). The GUI reads and writes it;
/// the CLI applies its edits when fixing.
module Legibyl.Validation.ReviewFile

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

/// "report.pdf" -> "report.pdf.legibyl.json".
let filePath (pdfPath: string) = pdfPath + ".legibyl.json"

// The file's format. CLIMutable gives System.Text.Json the public parameterless constructor it
// needs (a private F# type's constructors aren't public, so these stay public).
[<CLIMutable>]
type SavedWarning =
    { [<JsonPropertyName "rule">] Rule: string
      [<JsonPropertyName "location">] Location: string
      [<JsonPropertyName "message">] Message: string }

/// A change made to a suggested region or an element, found again by where it is. Each field it
/// has says what it changes; the others are left out (null).
[<CLIMutable>]
type SavedEdit =
    { [<JsonPropertyName "page">] Page: int
      /// The suggested tag, e.g. "Figure".
      [<JsonPropertyName "tag">] Tag: string
      /// Left, bottom, right, top, in PDF points.
      [<JsonPropertyName "box">] Box: float array
      /// A figure's alt text.
      [<JsonPropertyName "alt">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Alt: string | null
      /// The text of a region read with OCR, corrected ("" for none to read).
      [<JsonPropertyName "text">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Text: string | null
      /// For paragraphs merged into this one (in View Structure): their boxes, like Box.
      [<JsonPropertyName "mergeWith">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      MergeWith: float array array | null
      /// The tag to give it instead, e.g. "H1".
      [<JsonPropertyName "retag">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Retag: string | null
      /// For a suggested region: leave it out of the structure.
      [<JsonPropertyName "exclude">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingDefault)>]
      Exclude: bool }

/// The review: remembered dismissals and edits, either of which may be missing (null).
[<CLIMutable>]
type SavedFile =
    { [<JsonPropertyName "version">] Version: int
      /// Null if dismissals aren't being remembered (or a hand-edited file leaves them out).
      [<JsonPropertyName "dismissedWarnings">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      DismissedWarnings: SavedWarning array | null
      [<JsonPropertyName "edits">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Edits: SavedEdit array | null
      /// The document's title as set in Suggest Structure, if changed from what was prefilled.
      [<JsonPropertyName "title">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Title: string | null
      /// For an untagged PDF: its layout as reviewed (see LayoutFile), the pages reviewed so far;
      /// tagging takes it, and the layout model for the others.
      [<JsonPropertyName "layout">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Layout: LayoutFile.SavedLayout | null
      /// The auto fixes chosen, by name (see RepairNames); null if none have been.
      [<JsonPropertyName "fixes">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Fixes: string array | null }

// Relaxed escaping keeps the text people write readable ("wolf's", not "wolf's") in a
// file they may open by hand.
let private jsonOptions =
    JsonSerializerOptions(WriteIndented = true, Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

let parse (json: string) : SavedFile option =
    try
        JsonSerializer.Deserialize<SavedFile>(json, jsonOptions) |> Option.ofObj
    with :? JsonException ->
        None

/// The file at path, or None if there's none (or it can't be read).
let readFile (path: string) : SavedFile option =
    if not (File.Exists path) then
        None
    else
        try
            parse (File.ReadAllText path)
        with
        | :? IOException
        | :? UnauthorizedAccessException -> None

/// pdfPath's review: the file next to it, or else the one embedded in it (a copy Legibyl saved;
/// see EmbeddedReview). The file wins, since it holds any changes made since the copy was saved.
let readFor (pdfPath: string) : SavedFile option =
    match readFile (filePath pdfPath) with
    | Some saved -> Some saved
    | None -> EmbeddedReview.read pdfPath |> Option.bind parse

/// The review to read edits from at path: a review file, or a PDF's own review (see readFor).
let readFrom (path: string) : SavedFile option =
    if String.Equals(Path.GetExtension path, ".pdf", StringComparison.OrdinalIgnoreCase) then readFor path
    else readFile path

/// file as it's saved.
let toJson (file: SavedFile) = JsonSerializer.Serialize({ file with Version = 1 }, jsonOptions)

/// Whether file has nothing in it.
let isEmpty (file: SavedFile) =
    let none (items: 'T array | null) =
        match items with
        | NonNull items -> items.Length = 0
        | Null -> true
    // Fixes chosen as none is still a choice, as is none dismissed (over a review carried inside
    // the PDF that has some).
    isNull file.DismissedWarnings && none file.Edits && isNull file.Title && isNull file.Layout && isNull file.Fixes

/// Writes file next to pdfPath, or deletes it when there's nothing in it. Throws
/// IOException/UnauthorizedAccessException if it can't be written.
let writeFile (pdfPath: string) (file: SavedFile) =
    let path = filePath pdfPath
    if isEmpty file then
        if File.Exists path then File.Delete path
    else
        File.WriteAllText(path, JsonSerializer.Serialize(file, jsonOptions))

// ---- Edits ----

/// The edit as the ApplyEdits fix takes it; None for one that changes nothing or has no box.
let toElementEdit (saved: SavedEdit) : ElementEdit option =
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
        if isNull saved.Alt && isNull saved.Text && mergeWith.IsEmpty && isNull saved.Retag && not saved.Exclude then
            None
        else
            Some
                { Page = saved.Page
                  Tag = saved.Tag
                  Box = box
                  Alt = saved.Alt
                  Text = saved.Text
                  MergeWith = mergeWith
                  Retag = saved.Retag
                  Exclude = saved.Exclude }
    | None -> None

/// The edit as it's saved.
let ofElementEdit (e: ElementEdit) : SavedEdit =
    let saved (box: StructureGeometry) = [| box.Left; box.Bottom; box.Right; box.Top |] |> Array.map (fun v -> Math.Round(v, 1))
    { Page = e.Page
      Tag = e.Tag
      Box = saved e.Box
      Alt = e.Alt
      Text = e.Text
      MergeWith = if e.MergeWith.IsEmpty then null else e.MergeWith |> List.map saved |> Array.ofList
      Retag = e.Retag
      Exclude = e.Exclude }

/// The review's edits, as the ApplyEdits fix takes them.
let edits (saved: SavedFile) : ElementEdit list =
    match saved.Edits with
    | NonNull edits -> edits |> Array.choose toElementEdit |> List.ofArray
    | Null -> []

/// edits moved from the regions they were made for to the elements tagging made of them (the
/// tag it wrote, the box of what the element holds), so in the tagged copy they're edits of its
/// elements; a retag is then just the element's tag. Exclusions go (there's no element), and
/// edits for regions that weren't tagged stay as they are.
let rekey (edits: ElementEdit list) (tagged: TaggedRegion seq) : ElementEdit list =
    let tagged = List.ofSeq tagged
    [ for e in edits do
          if not (e.Exclude && not tagged.IsEmpty) then
              // Tagged regions have their edits applied: a retagged one has its new tag.
              let regionTag = match e.Retag with NonNull tag -> tag | Null -> e.Tag
              let moved =
                  tagged
                  |> List.choose (fun t ->
                      match t.Box with
                      | NonNull box when t.Region.Tag = regionTag && t.Region.Geometry.PageNumber = e.Page ->
                          Some(t.Tag, box, ElementEdits.overlap t.Region.Geometry e.Box)
                      | _ -> None)
                  |> List.filter (fun (_, _, o) -> o >= ElementEdits.SameElementOverlap)
                  |> List.sortByDescending (fun (_, _, o) -> o)
                  |> List.tryHead
                  |> Option.map (fun (tag, box, _) -> { e with Tag = tag; Box = box; Retag = null })
              match moved with
              | Some moved when isNull moved.Alt && isNull moved.Text && moved.MergeWith.IsEmpty -> ()
              | Some moved -> yield moved
              | None -> yield e ]

/// Builds the review a copy carries (see VeraPdfValidator.Review): saved, with its edits moved to
/// the elements tagging made (if it tagged), and without the layout it was tagged from (the copy
/// has elements instead). Nothing for no review.
let forCopy (saved: SavedFile option) =
    Func<Collections.Generic.IReadOnlyList<TaggedRegion>, string | null>(fun tagged ->
        match saved with
        | Some saved when tagged.Count = 0 -> toJson saved
        | Some saved ->
            let moved = rekey (edits saved) tagged
            toJson
                { saved with
                    Layout = null
                    Edits = (if moved.IsEmpty then null else moved |> List.map ofElementEdit |> Array.ofList) }
        | None -> null)

/// No regions: a layout source for when there's nothing but a review to tag from.
let noRegions =
    { new IPageLayoutSource with
        member _.PageRegions(_, _) = [] :> Collections.Generic.IReadOnlyList<_>
        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = null }

/// The regions to tag from, as a review has them: the layout it recorded, for the pages it has
/// (see LayoutFile), else inner's, with its edits layered on (see ElementEdits.onRegions) and
/// its title.
type ReviewedRegions(inner: IPageLayoutSource, saved: SavedFile) =
    let inner =
        match saved.Layout with
        | NonNull layout -> LayoutFile.FileRegions(layout, Some inner) :> IPageLayoutSource
        | Null -> inner
    let edits = edits saved

    interface IPageLayoutSource with
        member _.PageRegions(pdfPath, pageNumber) =
            ElementEdits.onRegions edits (inner.PageRegions(pdfPath, pageNumber)) :> Collections.Generic.IReadOnlyList<_>

        member _.LinkDescription(pdfPath, pageNumber, rectangle) = inner.LinkDescription(pdfPath, pageNumber, rectangle)
        member _.PageTitles = inner.PageTitles

        member _.DocumentTitle =
            match saved.Title with
            | NonNull title -> title
            | Null -> inner.DocumentTitle
