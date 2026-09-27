namespace Legibyl.Cli

open System
open System.Collections.Generic
open Legibyl.Validation

type FailureDetail =
    { Specification: string
      Clause: string
      TestNumber: int
      Message: string
      ErrorMessage: string | null
      LocationContext: string | null
      /// Where it is, readably: "Document → Page 1 → paragraph (P) \"The garden in spring…\"" (see
      /// RuleViolation.Location); null when there's no location.
      Location: string | null
      /// Page region the finding refers to, in PDF points (origin bottom-left), if known.
      Region: StructureGeometry | null
      IsFixable: bool
      /// The repairs that fix it, by the names --fixes takes ("tag-from-layout" for tagging an
      /// untagged PDF, which `layout apply` does too); empty when none do.
      Fixes: string array
      /// What it means for a reader, in plain language, for the common PDF/UA-1 failures (see
      /// RuleGuide); null for others.
      Meaning: string | null }

    static member Of(violation: RuleViolation) =
        { Specification = violation.Specification
          Clause = violation.Clause
          TestNumber = violation.TestNumber
          Message = violation.Message
          ErrorMessage = violation.ErrorMessage
          LocationContext = violation.LocationContext
          Location = violation.Location |> Option.ofObj |> Option.map _.Breadcrumb |> Option.toObj
          Region = violation.Geometry
          IsFixable = violation.IsFixable
          Fixes = RepairNames.ofOptions violation.FixOptions |> Array.ofList
          Meaning = RuleGuide.forFinding violation |> Option.map _.Meaning |> Option.toObj }

/// The PDF/UA standard a file is checked against, and why.
type Target =
    { Profile: PdfProfile
      /// The file's PDF version, e.g. "2.0"; null if it couldn't be read.
      PdfVersion: string | null
      /// Worked out from the PDF version (true), or asked for with --ua.
      FromFile: bool }

    /// Why, as the results say it: "file" or "--ua".
    member target.From = if target.FromFile then "file" else "--ua"

/// `validate`'s result for one file. Field names are camelCased on output.
type ValidationResult =
    { File: string
      /// The standard checked against: "PDF/UA-1" or "PDF/UA-2".
      Standard: string
      /// The file's PDF version, which says which standard applies; null if it couldn't be read.
      PdfVersion: string | null
      /// Why that standard: "file" (from its PDF version) or "--ua" (asked for).
      StandardFrom: string
      /// The flavour id actually validated against (e.g. "ua1"), null if validation errored.
      FlavourId: string | null
      /// True/false if validation completed, null if it errored before a result was produced.
      IsCompliant: Nullable<bool>
      TotalAssertions: Nullable<int>
      FailedCount: Nullable<int>
      /// Only the failed rule checks; empty (not omitted) when compliant, null when errored.
      Failures: IReadOnlyList<FailureDetail> | null
      /// Legibyl warnings; empty (not omitted) when none, null when errored.
      Warnings: IReadOnlyList<FailureDetail> | null
      /// Non-null only if the file could not be validated (missing, corrupt, encrypted, etc).
      Error: string | null
      /// With --structure: the tagged structure tree; null if the file has none (untagged).
      Structure: StructureNode | null
      /// With --structure: why reading the tree failed, if it did.
      StructureError: string | null }

    /// A result with only the file and what it's checked against filled in.
    static member Empty(file, target: Target) =
        { File = file
          Standard = PdfUaTarget.displayName target.Profile
          PdfVersion = target.PdfVersion
          StandardFrom = target.From
          FlavourId = null
          IsCompliant = Nullable()
          TotalAssertions = Nullable()
          FailedCount = Nullable()
          Failures = null
          Warnings = null
          Error = null
          Structure = null
          StructureError = null }

/// A document's standing against its profile: whether it complies, and how many checks failed.
type Compliance = { IsCompliant: bool; FailedCount: int }

/// What `fix` or `layout apply` did to one file.
module FixStatus =
    /// A repaired copy was written (OutputPath).
    [<Literal>]
    let Repaired = "repaired"

    /// The repairs asked for had nothing to change; no copy was written.
    [<Literal>]
    let NothingToDo = "nothing-to-do"

    /// `layout apply` on a file that already has a structure tree: a layout is for an untagged one.
    [<Literal>]
    let AlreadyTagged = "already-tagged"

    /// The file couldn't be repaired (Error says why).
    [<Literal>]
    let Error = "error"

/// `fix`'s or `layout apply`'s result for one file.
type RepairResult =
    { File: string
      /// The standard checked against, as for validate (the metadata repair writes its claim).
      Standard: string
      PdfVersion: string | null
      StandardFrom: string
      /// One of FixStatus.
      Status: string
      /// The repaired copy, if one was written.
      OutputPath: string | null
      /// What each repair changed, in plain words.
      Applied: IReadOnlyList<string>
      /// Repairs asked for that weren't kept or couldn't apply, and why.
      NotApplied: IReadOnlyList<string>
      Error: string | null
      /// The file as it was, and the repaired copy (validated again), when there's one.
      Before: Compliance option
      After: Compliance option }

/// `structure`'s result for one file: its tagged structure tree; null (and no error) if it has
/// none, i.e. it's untagged.
type StructureResult =
    { File: string
      Structure: StructureNode | null
      Error: string | null }

/// `layout suggest --output`'s summary: where the layout (and any figure images) went.
type LayoutSuggestion =
    { File: string
      Layout: string
      Images: string | null
      Pages: int
      Regions: int }
