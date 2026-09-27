module Legibyl.Validation.Tests.SavedResultsTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// What a finding shows: everything but its object identity.
let private shown (finding: RuleViolation) =
    finding.Specification, finding.Clause, finding.TestNumber, finding.Message, finding.Severity, finding.FixOptions,
    finding.LocationContext, finding.Element, finding.ErrorMessage,
    (match finding.Geometry with NonNull g -> Some(g.PageNumber, Math.Round(g.Left, 3), Math.Round(g.Top, 3)) | Null -> None)

[<Theory>]
[<InlineData "pdfua1-fail.pdf">]
[<InlineData "pages-clipped-paragraphs.pdf">]
[<InlineData "libreoffice-empty-paragraph.pdf">]
let ``a saved result reads back as the validation gave it, messages and all`` (name: string) =
    let pdf = corpusFile name
    let report = { VeraPdfValidator().Validate(pdf, PdfProfile.PdfUa1) with ProfileDetected = true }
    let hash = SavedResults.hashOf pdf
    let file: ReviewFile.SavedFile =
        { Version = 1; DismissedWarnings = null; Edits = null; Title = null; Layout = null; Fixes = null
          Validation = SavedResults.ofReport report hash (DateTime(2026, 9, 27, 10, 0, 0))
          Checklist = null }

    // Through the review file's JSON, as Save writes it and opening reads it.
    let saved = (ReviewFile.parse (ReviewFile.toJson file)).Value.Validation |> nonNull
    let readBack = SavedResults.toReport saved pdf

    SavedResults.isCurrent saved hash PdfProfile.PdfUa1 |> should be True
    readBack.IsCompliant |> should equal report.IsCompliant
    readBack.TotalAssertions |> should equal report.TotalAssertions
    readBack.Findings |> Seq.map shown |> List.ofSeq |> should equal (report.Findings |> Seq.map shown |> List.ofSeq)
    // No message is kept: they come from the rules.
    ReviewFile.toJson file |> should not' (haveSubstring "\"message\"")

[<Fact>]
let ``a saved result is stale for other bytes, another veraPDF or Legibyl, or another standard`` () =
    let pdf = corpusFile "pdfua1-pass.pdf"
    let report = VeraPdfValidator().Validate(pdf, PdfProfile.PdfUa1)
    let hash = SavedResults.hashOf pdf
    let saved = SavedResults.ofReport report hash DateTime.Now

    SavedResults.isCurrent saved hash PdfProfile.PdfUa1 |> should be True
    SavedResults.isCurrent saved (hash.Replace('a', 'b')) PdfProfile.PdfUa1 |> should be False
    SavedResults.isCurrent { saved with VeraPdfVersion = "1.26.0" } hash PdfProfile.PdfUa1 |> should be False
    SavedResults.isCurrent { saved with LegibylVersion = "0.1" } hash PdfProfile.PdfUa1 |> should be False
    SavedResults.isCurrent saved hash PdfProfile.PdfUa2 |> should be False
    saved.VeraPdfVersion |> should equal "1.28.1"

[<Fact>]
let ``a copy's review doesn't carry the saved result, or the checklist`` () =
    let pdf = corpusFile "pdfua1-pass.pdf"
    let report = VeraPdfValidator().Validate(pdf, PdfProfile.PdfUa1)
    let file: ReviewFile.SavedFile =
        { Version = 1; DismissedWarnings = [||]; Edits = null; Title = "A title"; Layout = null; Fixes = null
          Validation = SavedResults.ofReport report (SavedResults.hashOf pdf) DateTime.Now
          Checklist = { Hash = "abc"; Checked = [| "title" |] } }

    let json = (ReviewFile.forCopy (Some file)).Invoke([]) |> nonNull

    (ReviewFile.parse json).Value.Validation |> should be Null
    (ReviewFile.parse json).Value.Checklist |> should be Null
    (ReviewFile.parse json).Value.Title |> should equal "A title"
