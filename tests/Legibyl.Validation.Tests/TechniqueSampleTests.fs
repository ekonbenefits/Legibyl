/// Validates the PDF Association's "Techniques for Accessible PDF" samples against
/// their Pass/Fail labels. See SampleCorpus/README.md for where they come from.
module Legibyl.Validation.Tests.TechniqueSampleTests

open System.Collections.Generic
open System.IO
open Xunit
open FsUnit.Xunit
open YamlDotNet.Serialization
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

type private Sample =
    { Id: string
      Title: string
      PdfPath: string
      Passes: bool
      /// Matterhorn Protocol checkpoints the sample demonstrates, e.g. "02-001".
      Checkpoints: string list }

let private corpusDirectory = corpusFile "techniques-for-accessible-pdf"

let private readYaml<'T> (path: string) =
    DeserializerBuilder().Build().Deserialize<'T>(File.ReadAllText path)

/// Matterhorn checkpoint → "M" (machine-checkable) or "H" (needs human judgment).
let private checkTypes =
    let config =
        readYaml<Dictionary<string, Dictionary<string, string>>> (
            Path.Combine(corpusDirectory, "config", "matterhorn.yaml")
        )

    config["matterhorn_check_type"]

let private isMachineCheckable (sample: Sample) =
    sample.Checkpoints
    |> List.exists (fun checkpoint ->
        match checkTypes.TryGetValue checkpoint with
        | true, "M" -> true
        | _ -> false)

let private readSample (infoPath: string) =
    let info = readYaml<Dictionary<string, obj>> infoPath
    let text key = string info[key]

    let checkpoints =
        match info.TryGetValue "Matterhorn Protocol" with
        | true, (:? List<obj> as items) -> items |> Seq.map string |> List.ofSeq
        | _ -> []

    { Id = text "UA Technique Finder"
      Title = (text "Title").Trim()
      PdfPath = Directory.GetFiles(Path.GetDirectoryName infoPath |> nonNull, "*.pdf") |> Array.exactlyOne
      Passes = text "Pass / Fail" = "Pass"
      Checkpoints = checkpoints }

let private samples =
    Directory.GetFiles(corpusDirectory, "technique-info.yaml", SearchOption.AllDirectories)
    |> Array.map readSample
    |> Array.sortBy _.Id
    |> Array.map (fun sample -> sample.Id, sample)
    |> dict

let private theoryData (predicate: Sample -> bool) =
    TheoryData<string, string>(
        samples.Values
        |> Seq.filter predicate
        |> Seq.map (fun sample -> TheoryDataRow<string, string>(sample.Id, sample.Title))
    )

let passingSamples = theoryData _.Passes

let machineCheckableFailingSamples =
    theoryData (fun sample -> not sample.Passes && isMachineCheckable sample)

let humanJudgedFailingSamples =
    theoryData (fun sample -> not sample.Passes && not (isMachineCheckable sample))

/// Machine-checkable failing samples whose defect veraPDF doesn't flag (as of veraPDF 1.28.1).
/// Each is skipped with the checkpoints it misses; if veraPDF starts catching one, its test
/// fails so it can be taken off this list.
let private knownVeraPdfGaps =
    set
        [ "UA1_Tpdf-G2_F02"
          "UA1_Tpdf-G2_F03"
          "UA1_Tpdf-G5_F05"
          "UA1_Tpdf-H_F02"
          "UA1_Tpdf-H_F05"
          "UA1_Tpdf-H_F07"
          "UA1_Tpdf-L_F02"
          "UA1_Tpdf-L_F08"
          "UA1_Tpdf-L_F09" ]

/// Every failing sample leaves out the PDF/UA identifier in its XMP metadata, so veraPDF always
/// reports rule 5-1 for it. Only the other failures say whether the sample's actual defect was found.
[<Literal>]
let private MissingPdfUaIdentifierRule = "5-1"

let private validate (sample: Sample) =
    VeraPdfValidator().Validate(sample.PdfPath, PdfProfile.PdfUa1)

let private failuresBesidesMissingIdentifier (report: ValidationReport) =
    report.FailedAssertions
    |> Seq.map _.RuleId
    |> Seq.filter ((<>) MissingPdfUaIdentifierRule)
    |> Seq.distinct
    |> List.ofSeq

[<Theory>]
[<MemberData(nameof passingSamples)>]
let ``passing sample is compliant`` (id: string, _title: string) =
    let report = validate samples[id]

    report.FailedAssertions |> Seq.map _.RuleId |> should be Empty
    report.IsCompliant |> should be True

[<Theory>]
[<MemberData(nameof machineCheckableFailingSamples)>]
let ``machine-checkable failing sample is flagged for its defect`` (id: string, _title: string) =
    let sample = samples[id]
    let report = validate sample
    let failures = failuresBesidesMissingIdentifier report

    report.IsCompliant |> should be False

    if knownVeraPdfGaps.Contains id then
        failures |> should be Empty // veraPDF now catches it: remove it from knownVeraPdfGaps.
        let missed = sample.Checkpoints |> List.filter (fun c -> checkTypes.GetValueOrDefault c = "M")
        Assert.Skip($"""Known veraPDF gap: doesn't flag Matterhorn {String.concat ", " missed}.""")
    else
        failures |> should not' (be Empty)

/// Not run yet: kept so each of these samples is listed in the suite until we decide how to
/// cover checks that need a person (#13).
[<Theory>]
[<MemberData(nameof humanJudgedFailingSamples)>]
let ``human-judged failing sample is flagged for its defect`` (id: string, _title: string) =
    let checkpoints = String.concat ", " samples[id].Checkpoints
    Assert.Skip($"Needs a person: Matterhorn {checkpoints} can only be checked by a human, not veraPDF.")

let failingSamples = theoryData (fun sample -> not sample.Passes)

let private failedRuleIds (report: ValidationReport) =
    report.FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq

[<Theory>]
[<MemberData(nameof failingSamples)>]
let ``metadata fix clears the missing identifier without adding failures`` (id: string, _title: string) =
    use output = new TempPdf ".fixed"

    let result = VeraPdfValidator().Fix(samples[id].PdfPath, PdfProfile.PdfUa1, output.Path, PdfFixOptions.Metadata)

    if not result.WasFixed then
        // veraPDF declines to touch metadata while other rules still fail.
        result.Status |> should equal RepairStatus.WontFix
    else
        let before = failedRuleIds result.OriginalReport
        let after = failedRuleIds (VeraPdfValidator().Validate(output.Path, PdfProfile.PdfUa1))
        after |> should not' (contain MissingPdfUaIdentifierRule)
        Set.difference after before |> should be Empty

[<Theory>]
[<InlineData("UA1_Tpdf-G4_F06")>]
[<InlineData("UA1_Tpdf-G4_F07")>]
let ``all fixes make a tab order sample compliant`` (id: string) =
    use output = new TempPdf ".fixed"

    let result = VeraPdfValidator().Fix(samples[id].PdfPath, PdfProfile.PdfUa1, output.Path, PdfFixOptions.All)

    result.WasFixed |> should be True
    let report = VeraPdfValidator().Validate(output.Path, PdfProfile.PdfUa1)
    failedRuleIds report |> should be Empty
    report.IsCompliant |> should be True

/// /Tabs of every page that has annotations.
let private annotatedPageTabs (path: string) =
    use document = new iText.Kernel.Pdf.PdfDocument(new iText.Kernel.Pdf.PdfReader(path))
    [ for pageNumber in 1 .. document.GetNumberOfPages() do
          let page = document.GetPage(pageNumber).GetPdfObject()
          match page.GetAsArray iText.Kernel.Pdf.PdfName.Annots with
          | null -> ()
          | annotations when annotations.Size() = 0 -> ()
          | _ -> page.GetAsName iText.Kernel.Pdf.PdfName.Tabs ]

[<Theory>]
[<InlineData("UA1_Tpdf-G4_F06")>]
[<InlineData("UA1_Tpdf-G4_F07")>]
let ``tab order fix sets structure tab order on annotated pages`` (id: string) =
    let input = samples[id].PdfPath
    use output = new TempPdf ".fixed"
    annotatedPageTabs input |> should not' (be Empty)
    annotatedPageTabs input |> should not' (contain iText.Kernel.Pdf.PdfName.S)

    let result = VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output.Path, PdfFixOptions.AnnotationTabs)

    result.WasFixed |> should be True
    result.AppliedFixes |> Seq.exists (fun fix -> fix.Contains "/Tabs /S") |> should be True
    annotatedPageTabs output.Path |> List.forall ((=) iText.Kernel.Pdf.PdfName.S) |> should be True
    failedRuleIds result.OriginalReport |> should contain "7.18.3-1"
    // Only the missing PDF/UA identifier is left, which the metadata fix handles.
    failedRuleIds (VeraPdfValidator().Validate(output.Path, PdfProfile.PdfUa1))
    |> should equal (set [ MissingPdfUaIdentifierRule ])
