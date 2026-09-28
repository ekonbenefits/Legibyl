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

/// An item of the results list: a rule (and whether its findings are shown), or a finding; or,
/// for a PDF that passes, the pass, first (and how far a person has checked it).
type ResultItem =
    | RuleItem of RuleGroup * expanded: bool
    | FindingItem of RuleViolationRow
    | PassItem of progress: string
