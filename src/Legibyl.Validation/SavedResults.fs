/// A validation's result as the review keeps it (see ReviewFile.SavedValidation), so reopening a
/// PDF shows its results at once rather than validating it again, for as long as it's the same
/// file (the same bytes), checked by the same versions against the same standard.
module Legibyl.Validation.SavedResults

open System
open System.IO
open System.Reflection
open System.Security.Cryptography

/// The SHA-256 of the whole file, in lowercase hex: findings can change with any byte.
let hashOf (pdfPath: string) =
    use stream = File.OpenRead pdfPath
    Convert.ToHexString(SHA256.HashData stream).ToLowerInvariant()

/// This build of Legibyl (with its commit): a new check makes an older result stale.
let legibylVersion =
    match typeof<ReviewFile.SavedValidation>.Assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>() with
    | null -> "unknown"
    | attribute -> attribute.InformationalVersion

let private standardOf (profile: PdfProfile) = if profile = PdfProfile.PdfUa2 then "ua2" else "ua1"

/// The report as the review keeps it, for the file with hash, checked at checkedAt.
let ofReport (report: ValidationReport) (hash: string) (checkedAt: DateTime) : ReviewFile.SavedValidation =
    { Hash = hash
      VeraPdfVersion = VeraPdfValidator.EngineVersion
      LegibylVersion = legibylVersion
      Standard = standardOf report.Profile
      StandardFrom = (if report.ProfileDetected then "file" else "--ua")
      PdfVersion = report.PdfVersion
      CheckedAt = checkedAt
      IsCompliant = report.IsCompliant
      TotalAssertions = report.TotalAssertions
      Findings =
        [| for finding in report.Findings ->
               { Specification = finding.Specification
                 Clause = finding.Clause
                 TestNumber = finding.TestNumber
                 Severity = string finding.Severity
                 LocationContext = finding.LocationContext
                 Region =
                   match finding.Geometry with
                   | NonNull g -> [| float g.PageNumber; g.Left; g.Bottom; g.Right; g.Top |]
                   | Null -> null
                 Element = finding.Element
                 FixOptions = int finding.FixOptions
                 ErrorMessage = finding.ErrorMessage } |] }

/// Whether saved is the result for the file with hash, as this build would give it against profile.
let isCurrent (saved: ReviewFile.SavedValidation) (hash: string) (profile: PdfProfile) =
    saved.Hash = hash
    && saved.VeraPdfVersion = VeraPdfValidator.EngineVersion
    && saved.LegibylVersion = legibylVersion
    && saved.Standard = standardOf profile

/// The report saved for pdfPath, as a fresh validation would give it: each finding's message is
/// its rule's (veraPDF's profile, or Legibyl's own check).
let toReport (saved: ReviewFile.SavedValidation) (pdfPath: string) : ValidationReport =
    let profile = if saved.Standard = "ua2" then PdfProfile.PdfUa2 else PdfProfile.PdfUa1
    let messageOf (finding: ReviewFile.SavedFinding) =
        if finding.Specification = "Legibyl" then LegibylChecks.messageFor finding.TestNumber
        else VeraPdfValidator.RuleDescription(profile, finding.Specification, finding.Clause, finding.TestNumber)
        |> Option.defaultValue ""
    { FilePath = pdfPath
      Profile = profile
      FlavourId = saved.Standard
      PdfVersion = saved.PdfVersion
      ProfileDetected = (saved.StandardFrom = "file")
      IsCompliant = saved.IsCompliant
      TotalAssertions = saved.TotalAssertions
      Assertions =
        [| for finding in saved.Findings ->
               let severity = if finding.Severity = "Warning" then FindingSeverity.Warning else FindingSeverity.Failure
               { Specification = finding.Specification
                 Clause = finding.Clause
                 TestNumber = finding.TestNumber
                 // Legibyl's warnings pass (they're not PDF/UA's); veraPDF's findings failed.
                 Passed = (severity = FindingSeverity.Warning)
                 Message = messageOf finding
                 ErrorMessage = finding.ErrorMessage
                 LocationContext = finding.LocationContext
                 Severity = severity
                 FixOptions = enum<PdfFixOptions> finding.FixOptions
                 Geometry =
                   match finding.Region with
                   | NonNull [| page; left; bottom; right; top |] ->
                       { PageNumber = int page; Left = left; Bottom = bottom; Right = right; Top = top }
                   | _ -> null
                 Element = finding.Element } |] }
