/// Warnings the user has dismissed for a PDF, kept in memory and, if they choose, in a
/// "<name>.pdf.legibyl.json" file next to the PDF so they stay dismissed when it's opened again.
/// The same file holds figures' alt text written in Suggest Structure (see FigureAlt).
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

/// A change made in Suggest Structure to a suggested region, found again by where it is: for
/// now, the alt text written for a figure.
[<CLIMutable>]
type SavedEdit =
    { [<JsonPropertyName "page">] Page: int
      /// The suggested tag, e.g. "Figure".
      [<JsonPropertyName "tag">] Tag: string
      /// Left, bottom, right, top, in PDF points.
      [<JsonPropertyName "box">] Box: float array
      [<JsonPropertyName "alt">] Alt: string }

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
      Edits: SavedEdit array | null }

// Relaxed escaping keeps the text people write readable ("wolf's", not "wolf\u0027s") in a
// file they may open by hand.
let private jsonOptions =
    JsonSerializerOptions(WriteIndented = true, Encoder = System.Text.Encodings.Web.JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

/// The file at path, or None if there's none (or it can't be read).
let readFile (path: string) : SavedFile option =
    if not (File.Exists path) then
        None
    else
        try
            JsonSerializer.Deserialize<SavedFile>(File.ReadAllText path, jsonOptions) |> Option.ofObj
        with
        | :? JsonException
        | :? IOException -> None

/// Writes file next to pdfPath, or deletes it when there's nothing left in it. Throws
/// IOException/UnauthorizedAccessException if it can't be written.
let writeFile (pdfPath: string) (file: SavedFile) =
    let path = filePath pdfPath
    let empty (items: 'T array | null) =
        match items with
        | NonNull items -> items.Length = 0
        | Null -> true
    if isNull file.DismissedWarnings && empty file.Edits then
        if File.Exists path then File.Delete path
    else
        File.WriteAllText(path, JsonSerializer.Serialize(file, jsonOptions))

/// The file next to pdfPath, or an empty one.
let readOrEmpty (pdfPath: string) =
    readFile (filePath pdfPath)
    |> Option.defaultValue { Version = 1; DismissedWarnings = null; Edits = null }

/// The dismissals saved next to pdfPath, or None if none are (no file, or one holding only
/// alt text).
let load (pdfPath: string) : Dismissed option =
    readFile (filePath pdfPath)
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
    match readFile (filePath pdfPath) with
    | Some saved -> writeFile pdfPath { saved with DismissedWarnings = null }
    | None -> ()
