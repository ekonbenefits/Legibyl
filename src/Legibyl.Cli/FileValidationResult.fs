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
      /// Page region the finding refers to, in PDF points (origin bottom-left), if known.
      Region: StructureGeometry | null
      IsFixable: bool
      FixOptions: string | null }

    static member Of(violation: RuleViolation) =
        { Specification = violation.Specification
          Clause = violation.Clause
          TestNumber = violation.TestNumber
          Message = violation.Message
          ErrorMessage = violation.ErrorMessage
          LocationContext = violation.LocationContext
          Region = violation.Geometry
          IsFixable = violation.IsFixable
          FixOptions = if violation.IsFixable then violation.FixOptions.ToString() else null }

/// JSON-serializable result for a single validated file. Field names are camelCased on output.
type FileValidationResult =
    { File: string
      Profile: string
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
      /// Repair status from veraPDF's metadata fixer (e.g. "Success", "NoAction"), only
      /// present when --fix was requested. See FixOutputPath.
      FixStatus: string | null
      /// Path to the repaired copy, present only if a fix was actually written (the
      /// CLI currently requests metadata repair only, and the fixer writes an output
      /// file only when it changes something, e.g. adding a missing XMP stream or
      /// PDF/A|UA identifier).
      FixOutputPath: string | null
      /// Human-readable descriptions of the fixes actually applied, if any.
      FixAppliedFixes: IReadOnlyList<string> | null
      /// Requested repairs that were attempted but not kept, and why (e.g. veraPDF's metadata
      /// fixer answering Won't fix), if any.
      FixNotApplied: IReadOnlyList<string> | null
      /// Non-null only if --fix was requested but the fix attempt itself errored.
      FixError: string | null
      /// The tagged logical structure tree (semantic hierarchy used by screen readers),
      /// present only when --structure was requested. Null if --structure wasn't
      /// requested, if the file errored before it could be read, or if the file has no
      /// structure tree at all (i.e. is completely untagged — see StructureError
      /// vs. simply absent for that distinction).
      Structure: StructureNode | null
      /// Non-null only if --structure was requested but reading the tree errored.
      StructureError: string | null }

    /// A result with only the file and profile filled in.
    static member Empty(file, profile) =
        { File = file
          Profile = profile
          FlavourId = null
          IsCompliant = Nullable()
          TotalAssertions = Nullable()
          FailedCount = Nullable()
          Failures = null
          Warnings = null
          Error = null
          FixStatus = null
          FixOutputPath = null
          FixAppliedFixes = null
          FixNotApplied = null
          FixError = null
          Structure = null
          StructureError = null }
