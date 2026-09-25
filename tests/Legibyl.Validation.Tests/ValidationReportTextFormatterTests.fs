module Legibyl.Validation.Tests.ValidationReportTextFormatterTests

open Xunit
open FsUnit.Xunit
open Legibyl.Validation

[<Literal>]
let private RawLocation =
    "root/document[0]/pages[1](7 0 obj PDPage)/annots[0](9 0 obj PDLinkAnnot)"

let private violation =
    { Specification = "ISO 14289-1:2014"
      Clause = "7.1"
      TestNumber = 2
      Passed = false
      Message = "A link annotation needs alternate text."
      ErrorMessage = "Missing Contents entry."
      LocationContext = RawLocation
      Severity = FindingSeverity.Failure
      FixOptions = PdfFixOptions.None
      Geometry = null }

[<Fact>]
let ``BuildFinding includes friendly and raw locations`` () =
    let text = ValidationReportTextFormatter.BuildFinding violation

    text |> should haveSubstring "Location: Document → Page 2 → Link annotation"
    text |> should haveSubstring $"Raw location: {RawLocation}"
    text |> should haveSubstring "Rule: ISO 14289-1:2014 7.1-2"
    text |> should haveSubstring "Message: A link annotation needs alternate text."
    text |> should haveSubstring "Error: Missing Contents entry."

[<Fact>]
let ``BuildFinding describes available fixes in plain words`` () =
    let text =
        ValidationReportTextFormatter.BuildFinding
            { violation with FixOptions = PdfFixOptions.Metadata ||| PdfFixOptions.RemoveEmptyParagraphs }

    text |> should haveSubstring "Fixable: Fix XMP metadata; Remove empty /P structure elements"

[<Fact>]
let ``BuildFinding omits fixable when no fix applies`` () =
    let text = ValidationReportTextFormatter.BuildFinding violation

    text |> should not' (haveSubstring "Fixable:")

[<Fact>]
let ``BuildReport includes locations for each failed finding`` () =
    let report =
        { FilePath = @"C:\documents\sample.pdf"
          Profile = PdfProfile.PdfUa1
          FlavourId = "ua1"
          IsCompliant = false
          TotalAssertions = 2
          Assertions =
            [| violation
               { violation with
                   Clause = "7.2"
                   TestNumber = 3
                   ErrorMessage = null
                   LocationContext = null } |] }

    let text = ValidationReportTextFormatter.BuildReport report

    text |> should haveSubstring "Finding 1"
    text |> should haveSubstring "Finding 2"
    text |> should haveSubstring $"Raw location: {RawLocation}"
    text |> should haveSubstring "Location: Not available"
    text |> should haveSubstring "Raw location: Not available"
