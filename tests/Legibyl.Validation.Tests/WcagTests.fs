module Legibyl.Validation.Tests.WcagTests

open System.IO
open iText.Kernel.Colors
open iText.Kernel.Pdf
open iText.Layout.Element
open iText.Pdfua
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A PDF/UA page (iText's PDF/UA support makes it so, PDF 2.0 and PDF/UA-2 for ua2) of
/// paragraphs, each in its colour on white, at its size.
let private createPdf (path: string) (ua2: bool) (paragraphs: (string * DeviceRgb * float32) list) =
    let writer = if ua2 then new PdfWriter(path, WriterProperties().SetPdfVersion PdfVersion.PDF_2_0) else new PdfWriter(path)
    use document =
        new PdfUADocument(writer, PdfUAConfig((if ua2 then PdfUAConformance.PDF_UA_2 else PdfUAConformance.PDF_UA_1), "Garden Colours", "en-US"))
    let font =
        iText.Kernel.Font.PdfFontFactory.CreateFont(
            corpusFile (Path.Combine("fonts", "Lato-Regular.ttf")),
            iText.IO.Font.PdfEncodings.WINANSI,
            iText.Kernel.Font.PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED
        )
    use layout = new iText.Layout.Document(document)
    layout.SetFont font |> ignore
    for text, colour, size in paragraphs do
        layout.Add(Paragraph(text).SetFontColor(colour).SetFontSize size) |> ignore

/// The page the tests read: one line that fails WCAG's contrast (about 1.6 to 1), and three that
/// pass: dark text, mid grey just over 4.5 to 1, and 18 pt text, large, at about 3 to 1.
let private paragraphs =
    [ "The garden opens at nine on Saturdays.", DeviceRgb(0x20, 0x20, 0x20), 12f
      "Light grey on white is hard to read.", DeviceRgb(0xCC, 0xCC, 0xCC), 12f
      "Mid grey on white just passes.", DeviceRgb(0x76, 0x76, 0x76), 12f
      "Large text needs less contrast.", DeviceRgb(0x94, 0x94, 0x94), 18f ]

let private validator = lazy (VeraPdfValidator())

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``text without enough contrast fails WCAG 2.2, the rest passes, and PDF/UA isn't changed by it`` (ua2: bool) =
    use pdf = new TempPdf()
    createPdf pdf.Path ua2 paragraphs

    let report = validator.Value.Validate(pdf.Path, (if ua2 then PdfProfile.PdfUa2 else PdfProfile.PdfUa1))

    report.IsCompliant |> should be True
    report.MeetsWcag |> should be False
    report.Passes |> should be False
    let contrast = report.WcagFailures |> Seq.filter (fun f -> f.RuleId = "1.4.3-1") |> List.ofSeq
    // Named with its criterion's level.
    contrast |> List.map (fun f -> f.Specification) |> should equal [ "WCAG 2.2 AA" ]
    // It's the light grey line's, and where it is on the page.
    (contrast.Head.LocationContext |> notNull) |> should haveSubstring "Light grey on white is hard to read."
    contrast.Head.Geometry |> should not' (be Null)
    // What it means, from the rule guide.
    (RuleGuide.forFinding contrast.Head |> Option.map _.Meaning |> Option.defaultValue "") |> should haveSubstring "low vision"

[<Fact>]
let ``a page with enough contrast everywhere passes PDF/UA and WCAG 2.2`` () =
    use pdf = new TempPdf()
    createPdf pdf.Path false [ "The garden opens at nine on Saturdays.", DeviceRgb(0x20, 0x20, 0x20), 12f ]

    let report = validator.Value.Validate(pdf.Path, PdfProfile.PdfUa1)

    report.WcagFailures |> should be Empty
    report.Passes |> should be True

[<Fact>]
let ``WCAG 2.2's findings are saved and read back as WCAG's, with their rule's description`` () =
    use pdf = new TempPdf()
    createPdf pdf.Path false paragraphs
    let report = validator.Value.Validate(pdf.Path, PdfProfile.PdfUa1)

    let saved = SavedResults.toReport (SavedResults.ofReport report (SavedResults.hashOf pdf.Path) System.DateTime.Now) pdf.Path

    saved.MeetsWcag |> should be False
    [ for f in saved.WcagFailures -> f.RuleId, f.Message ] |> should equal [ for f in report.WcagFailures -> f.RuleId, f.Message ]

[<Fact>]
let ``a WCAG criterion above level AA is a warning a review can dismiss, not a failure`` () =
    use pdf = new TempPdf()
    // A link shown, and described, only as its address: WCAG 2.2's 2.4.9 (Link Purpose, Link
    // Only) is level AAA.
    do
        use document = new PdfUADocument(new PdfWriter(pdf.Path), PdfUAConfig(PdfUAConformance.PDF_UA_1, "Garden Links", "en-US"))
        let font =
            iText.Kernel.Font.PdfFontFactory.CreateFont(
                corpusFile (Path.Combine("fonts", "Lato-Regular.ttf")),
                iText.IO.Font.PdfEncodings.WINANSI,
                iText.Kernel.Font.PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED
            )
        use layout = new iText.Layout.Document(document)
        layout.SetFont font |> ignore
        let link = Link("https://example.org/plan", iText.Kernel.Pdf.Action.PdfAction.CreateURI "https://example.org/plan")
        link.GetLinkAnnotation().SetContents "https://example.org/plan" |> ignore
        link.GetAccessibilityProperties().SetAlternateDescription "https://example.org/plan" |> ignore
        layout.Add(Paragraph().Add link) |> ignore

    let report = validator.Value.Validate(pdf.Path, PdfProfile.PdfUa1)

    let linkPurpose = report.Findings |> Seq.filter (fun f -> f.RuleId = "2.4.9-1") |> List.ofSeq
    linkPurpose |> List.map (fun f -> f.Specification, f.Severity) |> should equal [ "WCAG 2.2 AAA", FindingSeverity.Warning ]
    report.WcagFailures |> should be Empty
    report.Passes |> should be True
    // Dismissed in a review, as any warning is.
    let review: ReviewFile.SavedFile =
        { Version = 1
          DismissedWarnings =
            [| { Rule = ReviewFile.dismissalRule linkPurpose.Head; Location = ReviewFile.AnyLocation; Message = linkPurpose.Head.Message } |]
          Edits = null; Title = null; RoleMap = null; Attachments = null; Layout = null; Fixes = null; Validation = null; Checklist = null; Language = null }
    ReviewFile.dismisses review linkPurpose.Head |> should be True
