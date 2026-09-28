/// Warnings the user has dismissed for a PDF: part of its review (see DocumentReview), which Save
/// keeps in a "<name>.pdf.legibyl.json" file next to the PDF, with the edits made in View Structure.
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

/// Whether the user dismissed every warning of violation's rule.
let isRuleDismissed (dismissed: Dismissed) (violation: RuleViolation) =
    canDismiss violation && dismissed.ContainsKey(ruleKeyOf violation)

/// Whether violation is dismissed on its own or ignored with the rest of its rule.
let isDismissed (dismissed: Dismissed) (violation: RuleViolation) =
    canDismiss violation && (dismissed.ContainsKey(keyOf violation) || dismissed.ContainsKey(ruleKeyOf violation))

// The file itself (its format, reading and writing it) is ReviewFile's, shared with the CLI;
// a review's dismissals, with the rest of it, are DocumentReview's.

/// "report.pdf" -> "report.pdf.legibyl.json".
let filePath = ReviewFile.filePath

/// The dismissals in a review.
let ofSaved (saved: SavedFile) : Dismissed =
    match saved.DismissedWarnings with
    | NonNull warnings -> warnings |> Array.map (fun w -> { Rule = w.Rule; Location = w.Location }, w.Message) |> Map.ofArray
    | Null -> Map.empty

/// dismissed as a review file has them; null for none.
let toSaved (dismissed: Dismissed) : SavedWarning array | null =
    if dismissed.IsEmpty then null
    else [| for KeyValue(key, message) in dismissed -> { Rule = key.Rule; Location = key.Location; Message = message } |]
