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
      /// The fixes for it, by the names --fixes takes ("tag-from-layout" for tagging an
      /// untagged PDF, which `layout apply` does too); empty when none do.
      Fixes: string array
      /// What it means for a reader, in plain language, for the common PDF/UA failures (see
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
          Fixes = FixNames.ofOptions violation.FixOptions |> Array.ofList
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

/// The steps of PDF/UA's checklist a person checked, as a copy Legibyl wrote carries them (see
/// ReviewFile.carriedChecklist): only while its content is still what was checked.
type CheckedByPerson =
    { /// By name: "title", "language", "alt-text", "headings", "reading-order", "tables",
      /// "links", "colour".
      Steps: string array
      CheckedAt: Nullable<DateTime> }

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
      /// Only the failed rule checks; empty (never null) when compliant or errored.
      Failures: IReadOnlyList<FailureDetail>
      /// Legibyl warnings; empty (never null) when none or errored.
      Warnings: IReadOnlyList<FailureDetail>
      /// Non-null only if the file could not be validated (missing, corrupt, encrypted, etc).
      Error: string | null
      /// With --structure: the tagged structure tree; null if the file has none (untagged).
      Structure: StructureNode | null
      /// With --structure: why reading the tree failed, if it did.
      StructureError: string | null
      /// What veraPDF logged while it read the file (a font it couldn't read, nested marked
      /// content), counted; not findings. Empty (never null) when none or errored.
      ValidatorNotes: IReadOnlyList<ValidatorNote>
      /// The steps a person checked, when the file is a copy Legibyl wrote carrying them and its
      /// content is still what was checked; null otherwise.
      CheckedByPerson: CheckedByPerson | null
      /// How many warnings the review dismisses (as the GUI does): left out of Warnings, unless
      /// --all-warnings asked for them all, and counted either way.
      DismissedWarnings: int
      /// Where the review came from: the .legibyl.json file next to the PDF, the PDF (a copy
      /// carrying one), or --review's path; null when there's none.
      Review: string | null }

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
          Failures = [||]
          Warnings = [||]
          Error = null
          Structure = null
          StructureError = null
          ValidatorNotes = [||]
          CheckedByPerson = null
          DismissedWarnings = 0
          Review = null }

/// A document's standing against its profile: whether it complies, and how many checks failed.
type Compliance = { IsCompliant: bool; FailedCount: int }

/// What `fix` or `layout apply` did to one file.
module FixStatus =
    /// A fixed copy was written (OutputPath).
    [<Literal>]
    let Fixed = "fixed"

    /// The fixes asked for had nothing to change; no copy was written.
    [<Literal>]
    let NothingToDo = "nothing-to-do"

    /// `layout apply` on a file that already has a structure tree: a layout is for an untagged one.
    [<Literal>]
    let AlreadyTagged = "already-tagged"

    /// The file couldn't be fixed (Error says why).
    [<Literal>]
    let Error = "error"

/// `fix`'s or `layout apply`'s result for one file.
type FixOutcome =
    { File: string
      /// The standard checked against, as for validate (the metadata fix writes its claim).
      Standard: string
      PdfVersion: string | null
      StandardFrom: string
      /// One of FixStatus.
      Status: string
      /// The fixed copy, if one was written.
      OutputPath: string | null
      /// What each fix changed, in plain words.
      Applied: IReadOnlyList<string>
      /// Fixes asked for that weren't kept or couldn't apply, and why.
      NotApplied: IReadOnlyList<string>
      Error: string | null
      /// The file as it was, and the fixed copy (validated again): null when there's none.
      Before: Compliance | null
      After: Compliance | null
      /// What veraPDF logged while it read the file as it was (see ValidationResult); empty for none.
      ValidatorNotes: IReadOnlyList<ValidatorNote> }

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

/// What `append` did.
module AppendStatus =
    /// The joined file was written, and passes: PDF/UA, and each part's structure as its file's.
    [<Literal>]
    let Joined = "joined"

    /// The joined file was written, but fails (After, StructureDifferences say why); it's kept.
    [<Literal>]
    let Fails = "fails"

    /// Nothing was written: the files can't be joined, or one fails PDF/UA (Refusals say why).
    [<Literal>]
    let Refused = "refused"

/// One file `append` was given, as it was validated before joining.
type AppendInput =
    { File: string
      Standard: string
      IsCompliant: bool
      FailedCount: int }

/// Why `append` refused: one file (File), or the set of them (File null).
type AppendRefusal = { File: string | null; Reason: string }

/// `append`'s result.
type AppendOutcome =
    { /// One of AppendStatus.
      Status: string
      /// The joined file, if one was written.
      OutputPath: string | null
      /// The standard it's joined for and checked against: "PDF/UA-1" or "PDF/UA-2".
      Standard: string | null
      Title: string | null
      Language: string | null
      Inputs: IReadOnlyList<AppendInput>
      Refusals: IReadOnlyList<AppendRefusal>
      /// What was renamed (tags, classes, IDs, named destinations) and what wasn't carried over.
      Notes: IReadOnlyList<string>
      /// The joined file, validated; null when none was written.
      After: Compliance | null
      Failures: IReadOnlyList<FailureDetail>
      Warnings: IReadOnlyList<FailureDetail>
      /// Where a part's structure doesn't read as its file's did.
      StructureDifferences: IReadOnlyList<string> }
