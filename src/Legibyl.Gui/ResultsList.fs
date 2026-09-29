/// The results list's items: a finding as a row, a rule's findings as a group, and the list's
/// items (a rule, a finding, or a PDF's pass).
namespace Legibyl.Gui

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Data
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Input.Platform
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform.Storage
open Avalonia.Styling
open Legibyl.Layout
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Gui.BusyStatus

/// Flattened, display-friendly view of a RuleViolation for the results grid.
type RuleViolationRow =
    { Violation: RuleViolation
      Clause: string
      TestNumber: int
      Severity: string
      /// What an auto fix can do about this finding, e.g. "Fix XMP metadata"; empty if nothing.
      Fixable: string
      Message: string
      LocationContext: string | null
      /// 1-based page number, if the failure is anchored to a page.
      PageNumber: int option
      /// "Page 3" or "—" if no page is known — bound directly to the grid's Page column.
      PageDisplay: string
      /// Plain-English breadcrumb, e.g. "Document → Page 2 → Link annotation". Falls back to the raw context.
      FriendlyLocation: string
      /// A warning the user has dismissed (see Dismissals).
      Dismissed: bool
      /// How many findings break the same rule, it included.
      Occurrences: int }

    static member Of(violation: RuleViolation) =
        let location = violation.Location
        let pageNumber =
            match location, violation.Geometry with
            | NonNull location, _ when location.PageNumber.HasValue -> Some location.PageNumber.Value
            | _, NonNull geometry -> Some geometry.PageNumber
            | _ -> None

        { Violation = violation
          Clause = violation.Clause
          TestNumber = violation.TestNumber
          Severity = violation.Severity.ToString()
          Fixable = violation.FixDescription
          Message = violation.Message
          LocationContext = violation.LocationContext
          PageNumber = pageNumber
          PageDisplay =
            match pageNumber with
            | Some page -> $"Page {page}"
            | None -> "—"
          FriendlyLocation =
            match location, violation.LocationContext with
            | NonNull location, _ -> location.Breadcrumb
            | Null, NonNull context -> context
            | Null, Null -> ""
          Dismissed = false
          Occurrences = 1 }

/// A rule's findings, as the results list shows them: a row for the rule, its findings under it.
/// The same rule, by its key, is the same group (whichever findings of it are shown).
[<CustomEquality; NoComparison>]
type RuleGroup =
    { /// What makes findings the same rule: "7.1-3: Content shall be marked as…".
      Key: string
      /// In the order found (by page).
      Findings: RuleViolationRow list }

    override group.Equals(other: obj) =
        match other with
        | :? RuleGroup as other -> group.Key = other.Key
        | _ -> false

    override group.GetHashCode() = group.Key.GetHashCode()

    member group.First = group.Findings.Head
    /// "7.1-3", or "Structure-9" for Legibyl's own.
    member group.RuleId = group.First.Violation.RuleId
    member group.Message = group.First.Message
    member group.Severity = group.First.Violation.Severity
    /// Those an auto fix (or tagging from the suggested structure) fixes.
    member group.Fixable = group.Findings |> List.filter _.Violation.IsFixable
    /// The pages its findings are on, in order.
    member group.Pages = group.Findings |> List.choose _.PageNumber |> List.distinct |> List.sort

    /// Findings grouped by rule, in the order each rule is first found.
    static member OfRows(rows: RuleViolationRow list) : RuleGroup list =
        let key (row: RuleViolationRow) = $"{row.Violation.RuleId}: {row.Message}"
        rows
        |> List.groupBy key
        |> List.map (fun (key, findings) -> { Key = key; Findings = findings })

    /// Whether its findings start shown: a rule found more than three times starts collapsed.
    member group.ExpandedAtFirst = group.Findings.Length <= 3

/// What to do next with a PDF that fails, as the Next step row (first in the list) says it.
type NextStep =
    { /// What to do: "Suggest the structure".
      Title: string
      /// Why, and how.
      Details: string list
      /// The rules with the most failures no auto fix fixes, the most first (three at most).
      TopRules: RuleGroup list }

module NextStep =
    /// What's been done in the review that the next step depends on.
    type Progress =
        { /// For an untagged PDF: its structure's been suggested (or edited), so Export… tags from it.
          StructureSuggested: bool
          /// Edits not in the PDF yet, which Export… writes.
          EditsPending: int
          /// The auto fixes chosen for Export…; None before any are.
          AutoFixesChosen: PdfFixOptions option }

    let private plural (n: int) (what: string) = if n = 1 then $"1 {what}" else $"{n} {what}s"

    /// The rules with the most of these failures, the most first (three at most).
    let private topRules (failures: RuleViolationRow list) =
        RuleGroup.OfRows failures |> List.sortByDescending _.Findings.Length |> List.truncate 3

    /// The next step for a PDF with these findings, given what's been done: suggest its structure
    /// if it has no tags (then Export… to tag a copy); else choose the auto fixes its failures have
    /// (then Export… to check again); else fix the rest by hand, the rules with the most failures
    /// first. None when nothing fails.
    let decide (rows: RuleViolationRow list) (progress: Progress) : NextStep option =
        let failures = rows |> List.filter (fun r -> r.Violation.Severity = FindingSeverity.Failure)
        let byTagging = failures |> List.filter (fun r -> r.Violation.FixOptions.HasFlag PdfFixOptions.TagFromLayout)
        let untagged = rows |> List.exists (fun r -> r.Violation.FixOptions.HasFlag PdfFixOptions.TagFromLayout)
        let chosen = progress.AutoFixesChosen |> Option.defaultValue PdfFixOptions.None
        let autoFixable = failures |> List.filter (fun r -> r.Violation.IsFixable)
        let unchosen = autoFixable |> List.filter (fun r -> r.Violation.FixOptions &&& chosen = PdfFixOptions.None)
        let byHand = failures |> List.filter (fun r -> not r.Violation.IsFixable)
        let have (n: int) = if n = 1 then "has" else "have"
        let failuresText = plural failures.Length "failure"
        let edits = plural progress.EditsPending "edit"
        let afterThat =
            if byHand.IsEmpty then []
            else
                let byHandText = plural byHand.Length "failure"
                [ $"{byHandText} {have byHand.Length} no auto fix: fix them in Structure…, or where the PDF was made. The rules with the most:" ]
        match failures with
        | [] -> None
        | _ when untagged && not progress.StructureSuggested ->
            Some
                { Title = "Suggest the structure"
                  Details =
                    [ $"This PDF has no tags, which a screen reader needs to read it. Tagging it fixes {byTagging.Length} of its {failuresText}."
                      "Suggest Structure… finds each page's headings, paragraphs, figures and reading order. Check them and edit what's wrong; Export… then tags a copy from them and checks it again." ]
                  TopRules = [] }
        | _ when untagged ->
            Some
                { Title = "Export to tag a copy"
                  Details =
                    [ (if progress.EditsPending > 0 then $"The structure's been suggested, with {edits} on it." else "The structure's been suggested.")
                      "Export… tags a copy of this PDF from it and opens the copy, checked, to see what's left. Suggest Structure… again to check more of it first." ]
                  TopRules = [] }
        | _ when not unchosen.IsEmpty ->
            Some
                { Title = "Choose auto fixes"
                  Details =
                    [ $"{unchosen.Length} of its {failuresText} {have unchosen.Length} an auto fix not chosen yet."
                      "Auto Fixes… chooses them; Export… then writes a fixed copy and opens it, checked. The PDF you opened isn't changed." ]
                    @ afterThat
                  TopRules = topRules byHand }
        | _ when not autoFixable.IsEmpty || progress.EditsPending > 0 ->
            let fixedText = plural autoFixable.Length "failure"
            let isAre = if progress.EditsPending = 1 then "is" else "are"
            let what =
                match autoFixable.Length, progress.EditsPending with
                | 0, _ -> $"{edits} {isAre} not in the PDF yet."
                | _, 0 -> $"The auto fixes chosen fix {fixedText}."
                | _ -> $"The auto fixes chosen fix {fixedText}, and {edits} {isAre} not in the PDF yet."
            Some
                { Title = "Export and check again"
                  Details = [ what; "Export… writes a copy with them and opens it, checked, to see what's left." ] @ afterThat
                  TopRules = topRules byHand }
        | _ ->
            Some
                { Title = "Fix the rules with the most failures"
                  Details =
                    [ $"{failuresText} left, none with an auto fix. Fixing a rule fixes each place it's broken, so start with those broken the most."
                      "Select a rule below to see what it means and what to do: in Structure…, or where the PDF was made." ]
                  TopRules = topRules byHand }

/// An item of the results list: a rule (and whether its findings are shown), or a finding; or,
/// for a PDF that passes, the pass, first (and how far a person has checked it); or, for one that
/// fails, the next step, first.
type ResultItem =
    | RuleItem of RuleGroup * expanded: bool
    | FindingItem of RuleViolationRow
    | PassItem of progress: string
    | NextStepItem of NextStep

    /// The item as a screen reader says it (a list item's name is its item's text): a rule's
    /// severity, rule, message, how many places, how it's fixed and whether it's open; a finding's
    /// severity, page and where it is; the pass, and how far a person has checked it.
    override item.ToString() =
        match item with
        | RuleItem(group, expanded) ->
            let places = if group.Findings.Length = 1 then "1 place" else $"{group.Findings.Length} places"
            let fixable =
                match group.Fixable.Length with
                | 0 -> ""
                | n when n = group.Findings.Length -> ", has an auto fix"
                | n -> $", {n} with an auto fix"
            let shown = if expanded then "expanded" else "collapsed"
            $"{group.Severity} {group.RuleId}: {group.Message}. {places}{fixable}, {shown}"
        | FindingItem row ->
            let dismissed = if row.Dismissed then ", dismissed" else ""
            $"{row.Severity}, {row.PageDisplay}: {row.FriendlyLocation}{dismissed}"
        | PassItem progress -> progress
        | NextStepItem step -> $"Next step: {step.Title}"
