/// Warnings the user has dismissed for a PDF, kept in memory and, if they choose, in a
/// "<name>.pdf.legibyl.json" file next to the PDF so they stay dismissed when it's opened again.
/// The same file holds the edits made in Suggest Structure (see RegionEdits).
module Legibyl.Gui.Dismissals

open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open Legibyl.Validation

/// Identifies a warning across validations of the same file: its rule and where it is, or
/// AnyLocation for every warning of the rule.
type Key = { Rule: string; Location: string }

/// Dismissed warnings, with each one's message (saved only so the file is readable).
type Dismissed = Map<Key, string>

/// Only warnings can be dismissed; failures are what the profile requires.
let canDismiss (violation: RuleViolation) = violation.Severity = FindingSeverity.Warning

let keyOf (violation: RuleViolation) =
    { Rule = $"{violation.Specification} {violation.RuleId}"
      Location = violation.LocationContext |> Option.ofObj |> Option.defaultValue "" }

/// The location of a key that dismisses every warning of its rule, wherever it is.
[<Literal>]
let AnyLocation = "*"

/// The key that ignores every warning of violation's rule (type).
let ruleKeyOf (violation: RuleViolation) =
    { keyOf violation with Location = AnyLocation }

/// Whether the user ignores every warning of violation's rule.
let isRuleIgnored (dismissed: Dismissed) (violation: RuleViolation) =
    canDismiss violation && dismissed.ContainsKey(ruleKeyOf violation)

/// Whether violation is dismissed on its own or ignored with the rest of its rule.
let isDismissed (dismissed: Dismissed) (violation: RuleViolation) =
    canDismiss violation && (dismissed.ContainsKey(keyOf violation) || dismissed.ContainsKey(ruleKeyOf violation))

/// "report.pdf" -> "report.pdf.legibyl.json".
let filePath (pdfPath: string) = pdfPath + ".legibyl.json"

// The file's format. CLIMutable gives System.Text.Json the public parameterless constructor it
// needs (a private F# type's constructors aren't public, so these stay public).
[<CLIMutable>]
type SavedWarning =
    { [<JsonPropertyName "rule">] Rule: string
      [<JsonPropertyName "location">] Location: string
      [<JsonPropertyName "message">] Message: string }

/// Changes made in Suggest Structure to a suggested region, found again by where it is. Each
/// field it has says what it changes; the others are left out (null).
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
      Text: string | null }

/// The file next to a PDF: its remembered dismissals and the edits made in Suggest Structure,
/// either of which may be missing (null).
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
      Title: string | null }

// Relaxed escaping keeps the text people write readable ("wolf's", not "wolf\u0027s") in a
// file they may open by hand.
let private jsonOptions =
    JsonSerializerOptions(WriteIndented = true, Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

let private parse (json: string) : SavedFile option =
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
        with :? IOException ->
            None

/// pdfPath's review: the file next to it, or else the one embedded in it (a copy Legibyl saved;
/// see EmbeddedReview). The file wins, since it holds any changes made since the copy was saved.
let readFor (pdfPath: string) : SavedFile option =
    match readFile (filePath pdfPath) with
    | Some saved -> Some saved
    | None -> EmbeddedReview.read pdfPath |> Option.bind parse

/// Writes file next to pdfPath, or deletes it when there's nothing left in it. Throws
/// IOException/UnauthorizedAccessException if it can't be written.
let writeFile (pdfPath: string) (file: SavedFile) =
    let path = filePath pdfPath
    let empty (items: 'T array | null) =
        match items with
        | NonNull items -> items.Length = 0
        | Null -> true
    if isNull file.DismissedWarnings && empty file.Edits && isNull file.Title then
        // An empty file still stands in for a review embedded in the PDF, so that one doesn't
        // come back.
        if (EmbeddedReview.read pdfPath).IsSome then
            File.WriteAllText(path, JsonSerializer.Serialize(file, jsonOptions))
        elif File.Exists path then
            File.Delete path
    else
        File.WriteAllText(path, JsonSerializer.Serialize(file, jsonOptions))

/// pdfPath's review (see readFor), or an empty one.
let readOrEmpty (pdfPath: string) =
    readFor pdfPath
    |> Option.defaultValue { Version = 1; DismissedWarnings = null; Edits = null; Title = null }

/// The dismissals saved for pdfPath (see readFor), or None if none are (no review, or one
/// holding only edits).
let load (pdfPath: string) : Dismissed option =
    readFor pdfPath
    |> Option.bind (fun saved -> Option.ofObj saved.DismissedWarnings)
    |> Option.map (fun warnings ->
        warnings
        |> Array.map (fun warning -> { Rule = warning.Rule; Location = warning.Location }, warning.Message)
        |> Map.ofArray)

/// Writes dismissed to the file next to pdfPath, keeping any alt text in it. Throws
/// IOException/UnauthorizedAccessException if it can't be written.
let save (pdfPath: string) (dismissed: Dismissed) =
    writeFile
        pdfPath
        { readOrEmpty pdfPath with
            Version = 1
            DismissedWarnings =
                [| for KeyValue(key, message) in dismissed ->
                       { Rule = key.Rule
                         Location = key.Location
                         Message = message } |] }

/// Stops remembering dismissals next to pdfPath: removes them from its file, deleting the file
/// if that leaves it empty.
let forget (pdfPath: string) =
    match readFor pdfPath with
    | Some saved -> writeFile pdfPath { saved with DismissedWarnings = null }
    | None -> ()

/// pdfPath's review as JSON, to embed in a copy of it (see EmbeddedReview), with dismissed in
/// place of its saved dismissals if given; null if there's nothing to carry.
let reviewJson (pdfPath: string) (dismissed: Dismissed option) : string | null =
    let saved = readOrEmpty pdfPath
    let saved =
        match dismissed with
        | Some dismissed when dismissed.IsEmpty -> { saved with DismissedWarnings = null }
        | Some dismissed ->
            { saved with
                DismissedWarnings =
                    [| for KeyValue(key, message) in dismissed ->
                           { Rule = key.Rule
                             Location = key.Location
                             Message = message } |] }
        | None -> saved
    let empty (items: 'T array | null) =
        match items with
        | NonNull items -> items.Length = 0
        | Null -> true
    if empty saved.DismissedWarnings && empty saved.Edits && isNull saved.Title then null
    else JsonSerializer.Serialize({ saved with Version = 1 }, jsonOptions)
