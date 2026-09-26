/// Warnings the user has dismissed for a PDF, kept in memory and, if they choose, in a
/// "<name>.pdf.legibyl.json" file next to the PDF so they stay dismissed when it's opened again.
/// The same file holds the edits made in View Structure, on a tagged PDF or a suggested structure (see RegionEdits).
module Legibyl.Gui.Dismissals

open System
open System.IO
open Legibyl.Validation
open Legibyl.Validation.ReviewFile

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

// The file itself (its format, reading and writing it) is ReviewFile's, shared with the CLI.

/// "report.pdf" -> "report.pdf.legibyl.json".
let filePath = ReviewFile.filePath

/// The file at path, or None if there's none (or it can't be read).
let readFile = ReviewFile.readFile

/// pdfPath's review: the file next to it, or else the one embedded in it.
let readFor = ReviewFile.readFor

/// Writes file next to pdfPath, or deletes it when there's nothing left in it.
let writeFile = ReviewFile.writeFile

/// file as it's saved.
let toJson = ReviewFile.toJson

/// pdfPath's review (see readFor), or an empty one.
let readOrEmpty (pdfPath: string) =
    readFor pdfPath
    |> Option.defaultValue { Version = 1; DismissedWarnings = null; Edits = null; Title = null; Layout = null }

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

/// pdfPath's review, to embed in a copy of it (see EmbeddedReview), with dismissed in place of
/// its saved dismissals if given; None if there's nothing to carry.
let reviewFile (pdfPath: string) (dismissed: Dismissed option) : SavedFile option =
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
    if empty saved.DismissedWarnings && empty saved.Edits && isNull saved.Title && isNull saved.Layout then None else Some saved

/// reviewFile as JSON; null if there's nothing to carry.
let reviewJson (pdfPath: string) (dismissed: Dismissed option) : string | null =
    match reviewFile pdfPath dismissed with
    | Some saved -> toJson saved
    | None -> null
