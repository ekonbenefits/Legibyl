/// Warnings the user has dismissed for a PDF, kept in memory and, if they choose, in a
/// "<name>.pdf.legibyl.json" file next to the PDF so they stay dismissed when it's opened again.
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

[<CLIMutable>]
type SavedFile =
    { [<JsonPropertyName "version">] Version: int
      /// Null if a hand-edited file leaves it out.
      [<JsonPropertyName "dismissedWarnings">] DismissedWarnings: SavedWarning array | null }

let private jsonOptions = JsonSerializerOptions(WriteIndented = true)

/// The dismissals saved next to pdfPath, or None if there's no file (or it can't be read).
let load (pdfPath: string) : Dismissed option =
    let path = filePath pdfPath
    if not (File.Exists path) then
        None
    else
        try
            match JsonSerializer.Deserialize<SavedFile>(File.ReadAllText path, jsonOptions) with
            | null -> None
            | saved ->
                saved.DismissedWarnings
                |> Option.ofObj
                |> Option.defaultValue [||]
                |> Array.map (fun warning -> { Rule = warning.Rule; Location = warning.Location }, warning.Message)
                |> Map.ofArray
                |> Some
        with
        | :? JsonException
        | :? IOException -> None

/// Writes dismissed to the file next to pdfPath. Throws IOException/UnauthorizedAccessException
/// if it can't be written.
let save (pdfPath: string) (dismissed: Dismissed) =
    let saved =
        { Version = 1
          DismissedWarnings =
            [| for KeyValue(key, message) in dismissed ->
                   { Rule = key.Rule
                     Location = key.Location
                     Message = message } |] }
    File.WriteAllText(filePath pdfPath, JsonSerializer.Serialize(saved, jsonOptions))

/// Deletes the file next to pdfPath, if there is one.
let forget (pdfPath: string) =
    let path = filePath pdfPath
    if File.Exists path then
        File.Delete path
