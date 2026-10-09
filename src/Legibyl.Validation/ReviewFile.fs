/// A PDF's review, as Legibyl keeps it: the dismissed warnings, the edits made in Suggest
/// Structure… and Structure…, and the title, in a "<name>.pdf.legibyl.json" file next to the
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
      /// For paragraphs merged into this one (in Structure…): their boxes, like Box.
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
      Exclude: bool
      /// For an element moved among its siblings (Structure's Move Up and Move Down): the box of
      /// the sibling it now follows, like Box...
      [<JsonPropertyName "moveAfter">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      MoveAfter: float array | null
      /// ...and that sibling's page, when it isn't Page (0 when it is)...
      [<JsonPropertyName "moveAfterPage">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingDefault)>]
      MoveAfterPage: int
      /// ...or that it's first among them.
      [<JsonPropertyName "moveFirst">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingDefault)>]
      MoveFirst: bool
      /// For a table: the cells to rebuild its rows and cells as (see TableRebuild).
      [<JsonPropertyName "cells">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Cells: SavedCell array | null }

/// A table's cell, as a rebuild keeps it: TH or TD, and its box.
and [<CLIMutable>] SavedCell =
    { [<JsonPropertyName "tag">] Tag: string
      [<JsonPropertyName "box">] Box: float array }

/// A finding as a saved validation keeps it: where it is and what fixes it, without its message
/// (which comes from the rule, when it's read back; see SavedResults).
[<CLIMutable>]
type SavedFinding =
    { [<JsonPropertyName "specification">] Specification: string
      [<JsonPropertyName "clause">] Clause: string
      [<JsonPropertyName "test">] TestNumber: int
      /// "Failure" or "Warning".
      [<JsonPropertyName "severity">] Severity: string
      [<JsonPropertyName "locationContext">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      LocationContext: string | null
      /// Page, left, bottom, right, top.
      [<JsonPropertyName "region">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Region: float array | null
      [<JsonPropertyName "element">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Element: string | null
      /// What fixes it, as PdfFixOptions' value.
      [<JsonPropertyName "fixOptions">] FixOptions: int
      /// The detail a Legibyl warning gives ("12 figures side by side on page 1."); veraPDF's.
      [<JsonPropertyName "errorMessage">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      ErrorMessage: string | null }

/// Something veraPDF logged while it read the file, as a saved validation keeps it (see ValidatorNote).
[<CLIMutable>]
type SavedNote =
    { [<JsonPropertyName "level">] Level: string
      [<JsonPropertyName "logger">] Logger: string
      [<JsonPropertyName "message">] Message: string
      [<JsonPropertyName "count">] Count: int }

/// A validation's result, kept with the review so reopening the PDF shows it at once. It's the
/// result for the exact bytes validated (their SHA-256), by these versions of veraPDF and
/// Legibyl, against this standard; if any of them differs, the PDF is validated again.
[<CLIMutable>]
type SavedValidation =
    { [<JsonPropertyName "hash">] Hash: string
      [<JsonPropertyName "veraPdfVersion">] VeraPdfVersion: string
      [<JsonPropertyName "legibylVersion">] LegibylVersion: string
      /// "ua1" or "ua2".
      [<JsonPropertyName "standard">] Standard: string
      /// "file" (from its PDF version) or "--ua" (asked for).
      [<JsonPropertyName "standardFrom">] StandardFrom: string
      [<JsonPropertyName "pdfVersion">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      PdfVersion: string | null
      [<JsonPropertyName "checkedAt">] CheckedAt: DateTime
      [<JsonPropertyName "isCompliant">] IsCompliant: bool
      [<JsonPropertyName "totalAssertions">] TotalAssertions: int
      [<JsonPropertyName "findings">] Findings: SavedFinding array
      /// What veraPDF logged while it read the file (see ValidatorNote); none in older reviews.
      [<JsonPropertyName "notes">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Notes: SavedNote array | null }

/// The steps of a passing PDF's checklist a person has ticked (see the GUI's Pass finding), for
/// the exact file checked: its hash, as its validation has it. Ticks don't carry to another
/// version of the file.
[<CLIMutable>]
type SavedChecklist =
    { [<JsonPropertyName "hash">] Hash: string
      /// The steps ticked, by name: "title", "language", "alt-text", "headings",
      /// "reading-order", "tables", "links", "colour".
      [<JsonPropertyName "checked">] Checked: string array | null
      /// When a step was last ticked.
      [<JsonPropertyName "checkedAt">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      CheckedAt: Nullable<DateTime>
      /// In a copy's review (see checkedCopy): how long the file checked was, in bytes. A copy
      /// Legibyl writes adds to the file's bytes without changing them, so its first Length bytes
      /// are what was checked (Hash); 0 next to a PDF, where the hash is its own.
      [<JsonPropertyName "length">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingDefault)>]
      Length: int64 }

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
      /// The document's language as a person chose it in Structure… ("es-MX"), which Export…
      /// declares.
      [<JsonPropertyName "language">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Language: string | null
      /// The standard tag a person chose for each of the PDF's own tag names (Structure…'s Role
      /// map), which Export… writes to its role map: "Heading1": "H1".
      [<JsonPropertyName "roleMap">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      RoleMap: Collections.Generic.Dictionary<string, string> | null
      /// What a person gave the files attached to the PDF and its media clips (Structure…'s
      /// Attachments), by each one's key (see Attachments): a file name, a description, a content type.
      [<JsonPropertyName "attachments">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Attachments: Collections.Generic.Dictionary<string, Attachments.AttachmentEdit> | null
      /// For an untagged PDF: its layout as reviewed (see LayoutFile), the pages reviewed so far;
      /// tagging takes it, and the layout model for the others.
      [<JsonPropertyName "layout">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Layout: LayoutFile.SavedLayout | null
      /// The auto fixes chosen, by name (see FixNames); null if none have been.
      [<JsonPropertyName "fixes">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Fixes: string array | null
      /// The last validation's result, as saved (see SavedValidation); never in a copy's review.
      [<JsonPropertyName "validation">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Validation: SavedValidation | null
      /// The passing PDF's checklist, as ticked (see SavedChecklist); never in a copy's review.
      [<JsonPropertyName "checklist">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Checklist: SavedChecklist | null }

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

/// The fields an edit can have.
let editFields = [ "page"; "tag"; "box"; "alt"; "text"; "retag"; "exclude"; "mergeWith"; "moveAfter"; "moveAfterPage"; "moveFirst"; "cells" ]

/// What's wrong with a review's edits as written (json), which reading it would otherwise pass
/// over: fields an edit doesn't have (e.g. "description": a link's description is its "alt"),
/// retags to tags that aren't standard, and role map entries to tags that aren't. Empty when
/// there's nothing wrong, or it isn't JSON.
let problems (json: string) : string list =
    try
        use document = JsonDocument.Parse json
        (match document.RootElement.TryGetProperty "language" with
         | true, language when
             language.ValueKind <> JsonValueKind.Null
             && (language.ValueKind <> JsonValueKind.String
                 || not (Text.RegularExpressions.Regex.IsMatch(language.GetString() |> string, "^[A-Za-z]{2,8}(-[A-Za-z0-9]{1,8})*$")))
             ->
             [ $"language is {language.GetRawText()}, which isn't a language tag (e.g. \"es\", \"en-US\")." ]
         | _ -> [])
        @
        (match document.RootElement.TryGetProperty "roleMap" with
         | true, map when map.ValueKind = JsonValueKind.Object ->
             [ for entry in map.EnumerateObject() do
                   let tag = if entry.Value.ValueKind = JsonValueKind.String then entry.Value.GetString() |> string else ""
                   if not (Tags.standard.Contains tag) then
                       $"roleMap maps \"{entry.Name}\" to \"{tag}\", which isn't a standard tag (e.g. H1 to H6, P, LI, Figure, Caption, Note)." ]
         | _ -> [])
        @
        match document.RootElement.TryGetProperty "edits" with
        | true, edits when edits.ValueKind = JsonValueKind.Array ->
            [ for index, edit in Seq.indexed (edits.EnumerateArray()) do
                  if edit.ValueKind = JsonValueKind.Object then
                      for field in edit.EnumerateObject() do
                          if not (List.contains field.Name editFields) then
                              let hint = if field.Name = "description" || field.Name = "contents" then " (a link's description is its \"alt\")" else ""
                              $"edits[{index}] has \"{field.Name}\", which isn't a field of an edit{hint}. An edit has page, tag and box, and any of alt, text, retag, exclude, mergeWith, moveAfter (with moveAfterPage) or moveFirst, and cells (a table's, rebuilt)."
                      match edit.TryGetProperty "cells" with
                      | true, cells when cells.ValueKind = JsonValueKind.Array ->
                          for c, cell in Seq.indexed (cells.EnumerateArray()) do
                              let tag =
                                  match cell.ValueKind, (if cell.ValueKind = JsonValueKind.Object then cell.TryGetProperty "tag" else (false, Unchecked.defaultof<JsonElement>)) with
                                  | JsonValueKind.Object, (true, tag) when tag.ValueKind = JsonValueKind.String -> tag.GetString() |> string
                                  | _ -> ""
                              if tag <> Tags.TH && tag <> Tags.TD then
                                  $"edits[{index}].cells[{c}] is a \"{tag}\", not a header cell (TH) or a data cell (TD)."
                      | _ -> ()
                      match edit.TryGetProperty "retag" with
                      | true, retag when retag.ValueKind = JsonValueKind.String && not (Tags.retagTargets.Contains(retag.GetString() |> string)) ->
                          $"edits[{index}] retags to \"{retag.GetString()}\", which isn't a standard tag (e.g. H1 to H6, P, LI, Figure, Caption, Note, Artifact)."
                      | _ -> () ]
        | _ -> []
    with :? JsonException -> []

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
    isNull file.DismissedWarnings && none file.Edits && isNull file.Title && isNull file.Language && (match file.RoleMap with null -> true | map -> map.Count = 0)
    && (match file.Attachments with null -> true | edits -> edits.Count = 0)
    && isNull file.Layout && isNull file.Fixes && isNull file.Validation && isNull file.Checklist

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
        let move =
            if saved.MoveFirst then Some First
            else
                match saved.MoveAfter with
                | NonNull [| left; bottom; right; top |] ->
                    let page = if saved.MoveAfterPage > 0 then saved.MoveAfterPage else saved.Page
                    Some(After { PageNumber = page; Left = left; Bottom = bottom; Right = right; Top = top })
                | _ -> None
        let cells =
            match saved.Cells with
            // (A cell is a TH or a TD: one that isn't is left out, as is one without a box.)
            | NonNull cells ->
                [ for cell in cells do
                      match geometry cell.Box with
                      | Some box when cell.Tag = Tags.TH || cell.Tag = Tags.TD -> ({ Tag = cell.Tag; Box = box }: TableCell)
                      | _ -> () ]
            | Null -> []
        if isNull saved.Alt && isNull saved.Text && mergeWith.IsEmpty && isNull saved.Retag && not saved.Exclude && move.IsNone && cells.IsEmpty then
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
                  Exclude = saved.Exclude
                  Move = move
                  Cells = cells }
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
      Exclude = e.Exclude
      MoveAfter = (match e.Move with Some(After box) -> saved box | _ -> null)
      MoveAfterPage = (match e.Move with Some(After box) when box.PageNumber <> e.Page -> box.PageNumber | _ -> 0)
      MoveFirst = (e.Move = Some First)
      Cells = if e.Cells.IsEmpty then null else [| for cell in e.Cells -> ({ Tag = cell.Tag; Box = saved cell.Box }: SavedCell) |] }

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
                  (if e.Tag = "FigureRegion" then [] else tagged)
                  |> List.choose (fun t ->
                      match t.Box with
                      | NonNull box when t.Region.Tag = regionTag && t.Region.Geometry.PageNumber = e.Page ->
                          Some(t.Tag, box, Boxes.overlap t.Region.Geometry e.Box)
                      | _ -> None)
                  |> List.filter (fun (_, _, o) -> o >= ElementEdits.SameElementOverlap)
                  |> List.sortByDescending (fun (_, _, o) -> o)
                  |> List.tryHead
                  |> Option.map (fun (tag, box, _) -> { e with Tag = tag; Box = box; Retag = null })
              match moved with
              | Some moved when isNull moved.Alt && isNull moved.Text && moved.MergeWith.IsEmpty && moved.Move.IsNone -> ()
              | Some moved -> yield moved
              | None -> yield e ]

/// Builds the review a copy carries (see VeraPdfValidator.Review): saved, with its edits moved to
/// the elements tagging made (if it tagged), and without the layout it was tagged from (the copy
/// has elements instead). Nothing for no review.
let forCopy (saved: SavedFile option) =
    Func<Collections.Generic.IReadOnlyList<TaggedRegion>, string | null>(fun tagged ->
        // A copy is validated afresh: a result inside it would be a stale claim about itself.
        match saved |> Option.map (fun saved -> { saved with Validation = null; Checklist = null }) with
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

// ---- A checked copy ----

/// The SHA-256 of pdfPath's first length bytes, in lowercase hex (as SavedResults.hashOf is).
let private hashOfStart (pdfPath: string) (length: int64) =
    use stream = File.OpenRead pdfPath
    use hash = Security.Cryptography.IncrementalHash.CreateHash Security.Cryptography.HashAlgorithmName.SHA256
    let buffer = Array.zeroCreate<byte> 81920
    let mutable left = length
    let mutable reading = true
    while reading && left > 0L do
        let read = stream.Read(buffer, 0, int (min left (int64 buffer.Length)))
        if read = 0 then reading <- false
        else
            hash.AppendData(buffer, 0, read)
            left <- left - int64 read
    Convert.ToHexString(hash.GetHashAndReset()).ToLowerInvariant()

/// The checklist the review inside pdfPath carries (a copy Legibyl wrote for steps a person
/// checked), if its content is still the file that was checked: its first Length bytes hash as
/// the file checked did. None for one that's been changed since (or has none).
let carriedChecklist (pdfPath: string) : SavedChecklist option =
    match EmbeddedReview.read pdfPath |> Option.bind parse with
    | Some { Checklist = NonNull checklist } when checklist.Length > 0L && not (isNull checklist.Checked) ->
        try
            if FileInfo(pdfPath).Length >= checklist.Length && hashOfStart pdfPath checklist.Length = checklist.Hash then Some checklist
            else None
        with
        | :? IOException
        | :? UnauthorizedAccessException -> None
    | _ -> None

/// Writes pdfPath to outputPath unchanged, with review inside it (see EmbeddedReview) carrying
/// steps: the steps a person checked of pdfPath, by its hash (a validation's), and its length.
let checkedCopy (pdfPath: string) (outputPath: string) (review: SavedFile) (hash: string) (steps: Set<string>) (checkedAt: DateTime) =
    let checklist =
        { Hash = hash
          Checked = Array.ofSeq steps
          CheckedAt = Nullable checkedAt
          Length = FileInfo(pdfPath).Length }
    EmbeddedReview.write pdfPath outputPath (toJson { review with Validation = null; Checklist = checklist })

// ---- Dismissed warnings ----

/// A dismissal's location for every warning of its rule, wherever it is.
[<Literal>]
let AnyLocation = "*"

/// The rule a dismissal names a warning by: its specification and rule, e.g. "Legibyl Structure-7".
let dismissalRule (violation: RuleViolation) = $"{violation.Specification} {violation.RuleId}"

/// The location a dismissal names a warning by: its location context ("" for none).
let dismissalLocation (violation: RuleViolation) = violation.LocationContext |> Option.ofObj |> Option.defaultValue ""

/// Whether review dismisses violation: a warning (failures can't be dismissed) whose rule it
/// dismisses there, or everywhere.
let dismisses (review: SavedFile) (violation: RuleViolation) =
    violation.Severity = FindingSeverity.Warning
    && (match review.DismissedWarnings with
        | NonNull warnings ->
            let rule, location = dismissalRule violation, dismissalLocation violation
            warnings |> Array.exists (fun w -> w.Rule = rule && (w.Location = AnyLocation || w.Location = location))
        | Null -> false)

/// Where pdfPath's review is read from (see readFor): the file next to it, or the PDF itself when
/// it carries one; None when it has none.
let sourceFor (pdfPath: string) : string option =
    if File.Exists(filePath pdfPath) then Some(filePath pdfPath)
    elif (EmbeddedReview.read pdfPath).IsSome then Some pdfPath
    else None
