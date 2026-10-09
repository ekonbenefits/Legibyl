module Legibyl.Validation.Tests.VeraPdfValidatorTests

open System
open iText.IO.Image
open iText.Kernel.Pdf
open iText.Layout
open iText.Layout.Element
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

[<Fact>]
let ``Validate known good PDF/UA-1 document reports compliant`` () =
    let validator = VeraPdfValidator()

    let report = validator.Validate(corpusFile "pdfua1-pass.pdf", PdfProfile.PdfUa1)

    report.IsCompliant |> should be True
    report.FlavourId |> should equal "ua1"
    report.TotalAssertions |> should be (greaterThan 0)
    report.FailedAssertions |> should be Empty

[<Fact>]
let ``Validate known bad PDF/UA-1 document reports non-compliant with failed rules`` () =
    let validator = VeraPdfValidator()

    let report = validator.Validate(corpusFile "pdfua1-fail.pdf", PdfProfile.PdfUa1)

    report.IsCompliant |> should be False
    report.FailedAssertions |> should not' (be Empty)
    report.FailedAssertions |> Seq.map (fun a -> a.Clause) |> should contain "7.1"

[<Fact>]
let ``Validate missing file throws VeraPdfValidationException`` () =
    let validator = VeraPdfValidator()

    (fun () -> validator.Validate(corpusFile "does-not-exist.pdf", PdfProfile.PdfUa1) |> ignore)
    |> should throw typeof<VeraPdfValidationException>

[<Fact>]
let ``Validate clipping path inside marked content does not inflate region`` () =
    let validator = VeraPdfValidator()

    let report = validator.Validate(corpusFile "pages-clipped-paragraphs.pdf", PdfProfile.PdfUa1)

    // The first paragraph (MCID 1) is a single line near the top of the page, drawn
    // inside a page-sized clipping rectangle.
    let firstParagraph =
        report.FailedAssertions
        |> Seq.find (fun a ->
            match a.LocationContext with
            | NonNull context -> context.Contains("{mcid:1}", StringComparison.Ordinal)
            | Null -> false)
    let geometry = firstParagraph.Geometry |> notNull
    geometry.Top - geometry.Bottom |> should be (inRange 5.0 20.0)
    geometry.Bottom |> should be (inRange 700.0 720.0)

/// A tagged PDF with `figures` sections, each a page or more of text followed by a figure
/// that has no alt text.
let private createPdfWithUntitledFigures (path: string) (figures: int) =
    use pdf = new PdfDocument(new PdfWriter(path))
    pdf.SetTagged() |> ignore
    use document = new Document(pdf)
    let pixel = ImageDataFactory.Create(2, 2, 3, 8, Array.create 12 128uy, null)
    for section in 1 .. figures do
        for line in 1 .. 12 do
            document.Add(Paragraph(String.replicate 6 $"Section {section}, paragraph {line}: text for veraPDF to check. ")) |> ignore
        document.Add(Image(pixel).SetWidth(20f)) |> ignore
        if section < figures then
            document.Add(AreaBreak()) |> ignore

[<Fact>]
let ``Validate reports every occurrence of a repeated failure`` () =
    // veraPDF keeps only the first failure of each rule once it has recorded 10,000 results,
    // which logging passed checks reached within a few pages.
    use input = new TempPdf()
    createPdfWithUntitledFigures input.Path 4

    let report = VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1)

    report.TotalAssertions |> should be (greaterThan 10000)
    // One missing-alt-text failure per figure, each placed on its own page.
    report.FailedAssertions
    |> Seq.filter (fun a -> a.RuleId = "7.3-1")
    |> Seq.map (fun a -> (a.Geometry |> notNull).PageNumber)
    |> Seq.distinct
    |> Seq.length
    |> should equal 4

[<Fact>]
let ``Validate reports progress, ending with the final counts`` () =
    use input = new TempPdf()
    createPdfWithUntitledFigures input.Path 10
    let reports = Collections.Concurrent.ConcurrentQueue<ValidationProgress>()
    let progress = { new IProgress<ValidationProgress> with member _.Report value = reports.Enqueue value }

    let report = VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1, progress)

    reports |> should not' (be Empty)
    let last = Seq.last reports
    last.Checks |> should equal report.TotalAssertions
    // veraPDF's count: not the dc:title failure Legibyl reports for its untitled, metadata-less file.
    last.FailedRules |> should equal (report.FailedAssertions |> Seq.filter (fun f -> f.RuleId <> "7.1-9") |> Seq.distinctBy _.RuleId |> Seq.length)
    last.ProcessedObjects |> should be (greaterThan 0)
    last.QueuedObjects |> should equal 0
    // WCAG 2.2's machine checks come last, with every validation; or, where Legibyl's own checks
    // outlast them, running those.
    [ ValidationStage.CheckingWcag; ValidationStage.CheckingLegibyl ] |> should contain last.Stage
    last.PageCount |> should be (greaterThanOrEqualTo 10)
    last.Page |> should equal last.PageCount
    // Seeing a page mid-run shows Legibyl can still read where veraPDF is (a private field).
    reports |> Seq.exists (fun r -> r.Stage = ValidationStage.CheckingPages && r.Page > 0) |> should be True
    // Pages are checked in order, so neither the page nor the stage ever goes backwards.
    let inOrder (values: int list) = values |> List.pairwise |> List.forall (fun (a, b) -> a <= b)
    reports |> Seq.map _.Page |> List.ofSeq |> inOrder |> should be True
    reports |> Seq.map (_.Stage >> int) |> List.ofSeq |> inOrder |> should be True

[<Fact>]
let ``ReadStructureTree reports its stages in order, page by page`` () =
    use input = new TempPdf()
    createPdfWithUntitledFigures input.Path 3
    let reports = Collections.Concurrent.ConcurrentQueue<StructureReadProgress>()
    let progress = { new IProgress<StructureReadProgress> with member _.Report value = reports.Enqueue value }

    VeraPdfValidator().ReadStructureTree(input.Path, progress) |> notNull |> ignore

    let stages = reports |> Seq.map (_.Stage >> int) |> List.ofSeq
    stages |> List.pairwise |> List.forall (fun (a, b) -> a <= b) |> should be True
    stages |> List.distinct |> should equal [ 0; 1; 2 ]
    let textReports = reports |> Seq.filter (fun r -> r.Stage = StructureReadStage.ReadingText) |> List.ofSeq
    textReports.Head.Page |> should equal 1
    textReports |> List.forall (fun r -> r.PageCount >= 3 && r.Page <= r.PageCount) |> should be True

[<Fact>]
let ``warming up gets veraPDF ready, and leaves nothing behind`` () =
    let warmFiles () = IO.Directory.GetFiles(IO.Path.GetTempPath(), "legibyl-warm-*.pdf") |> Set.ofArray
    let before = warmFiles ()

    VeraPdfValidator.Warm()

    // Other warm-ups (tests run in parallel, the GUI tests' in another process) come and go in the
    // same folder: wait them out. One this warm-up left behind would stay.
    let deadline = DateTime.UtcNow.AddSeconds 20.0
    while not (Set.difference (warmFiles ()) before).IsEmpty && DateTime.UtcNow < deadline do
        Threading.Thread.Sleep 100
    Set.difference (warmFiles ()) before |> should be Empty
    // And validations still work, alongside it or after.
    let report = VeraPdfValidator().Validate(IO.Path.Combine(AppContext.BaseDirectory, "SampleCorpus", "pdfua1-pass.pdf"), PdfProfile.PdfUa1)
    report.IsCompliant |> should be True

[<Fact>]
let ``common PDF/UA failures are explained in plain language, with what to do`` () =
    let untagged = RuleGuide.forRule "ISO 14289-1:2014" "7.1" 3 |> Option.get
    untagged.Meaning |> should haveSubstring "neither tagged nor marked as decoration"
    // Shown only where no auto fix applies: a tagged PDF (an untagged one gets Suggest Structure…).
    untagged.WhatToDo |> should haveSubstring "In Structure…, it's listed under Not in the tags"
    (RuleGuide.forRule "ISO 14289-1:2014" "7.3" 1).Value.WhatToDo |> should haveSubstring "Edit Alt Text…"
    // Every test of a clause the guide covers as a whole.
    RuleGuide.forRule "ISO 14289-1:2014" "7.21.6" 4 |> Option.isSome |> should be True
    // A rule it doesn't cover (none, now: see RuleGuideTests) would have only veraPDF's own text.
    RuleGuide.forRule "ISO 14289-1:2014" "9.9" 1 |> should equal None
    // PDF/UA-2's rules have their own entries, by its own clauses.
    (RuleGuide.forRule "ISO 14289-2:2024" "8.2.5.28.2" 1).Value.WhatToDo |> should haveSubstring "Edit Alt Text…"
    (RuleGuide.forRule "ISO 14289-2:2024" "8.2.1" 1).Value.WhatToDo |> should haveSubstring "Suggest Structure…"
    RuleGuide.forRule "ISO 14289-2:2024" "7.3" 1 |> should equal None
    // ISO 32005's tag nesting has one entry for all its rules.
    RuleGuide.forRule "ISO 32005:2023" "Table 5. StructTreeRoot-P" 1 |> Option.isSome |> should be True
    // Only PDF/UA's rules: a PDF/A rule with the same numbers isn't one of them.
    let finding (specification: string) : RuleViolation =
        { Specification = specification; Clause = "7.1"; TestNumber = 3; Passed = false; Message = ""; ErrorMessage = null
          LocationContext = null; Severity = FindingSeverity.Failure; FixOptions = PdfFixOptions.None; Geometry = null; Element = null }
    RuleGuide.forFinding (finding "ISO 14289-1:2014") |> Option.isSome |> should be True
    RuleGuide.forFinding (finding "ISO 19005-2:2011") |> should equal None

[<Fact>]
let ``every entry in RuleGuide.yaml names its clauses and has both texts`` () =
    let entries = RuleGuide.entries.Value |> Map.ofList
    for standard in [ "ISO 14289-1"; "ISO 14289-2"; "ISO 32005" ] do
        entries[standard] |> should not' (be Empty)
    // Only ISO 32005's entry is for all its rules.
    for standard, rules in Map.toList entries do
        for entry in rules do
            (standard = "ISO 32005" || not (isNull entry.Clause) || not (isNull entry.Clauses)) |> should be True
            String.IsNullOrWhiteSpace entry.Meaning |> should be False
            String.IsNullOrWhiteSpace entry.WhatToDo |> should be False

/// A PDF 2.0 file with a line of text.
let private createPdf20 (path: string) =
    use document = new PdfDocument(new PdfWriter(path, WriterProperties().SetPdfVersion(PdfVersion.PDF_2_0)))
    let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas(document.AddNewPage())
    canvas.BeginText().SetFontAndSize(iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA, 12f)
        .MoveText(72.0, 720.0).ShowText("A PDF 2.0 page.").EndText()
    |> ignore

[<Fact>]
let ``the standard to check against follows the file's PDF version`` () =
    PdfUaTarget.detect (IO.Path.Combine(AppContext.BaseDirectory, "SampleCorpus", "pdfua1-pass.pdf")) |> fst |> should equal PdfProfile.PdfUa1
    use pdf20 = new TempPdf()
    createPdf20 pdf20.Path
    PdfUaTarget.detect pdf20.Path |> should equal (PdfProfile.PdfUa2, Some "2.0")
    // Unreadable: PDF/UA-1, as for any version before 2.0.
    PdfUaTarget.detect "no-such-file.pdf" |> should equal (PdfProfile.PdfUa1, (None: string option))
    PdfUaTarget.displayName PdfProfile.PdfUa2 |> should equal "PDF/UA-2"
    PdfUaTarget.displayNameOfFlavour "ua1" |> should equal "PDF/UA-1"

[<Fact>]
let ``a PDF 2.0 file checked as PDF/UA-2 is checked by veraPDF's PDF/UA-2 profile`` () =
    use pdf20 = new TempPdf()
    createPdf20 pdf20.Path

    let report = VeraPdfValidator().Validate(pdf20.Path, PdfProfile.PdfUa2)

    report.FlavourId |> should equal "ua2"
    report.PdfVersion |> should equal "2.0"
    report.IsCompliant |> should be False
    // Its failures are ISO 14289-2's.
    report.FailedAssertions |> Seq.forall (fun f -> f.Specification.StartsWith "ISO 14289-2") |> should be True

[<Fact>]
let ``Validate known good PDF/UA-2 document reports compliant, detected from its PDF version`` () =
    let path = corpusFile "pdfua2-pass.pdf"
    let profile, version = PdfUaTarget.detect path
    profile |> should equal PdfProfile.PdfUa2
    version |> should equal (Some "2.0")

    let report = VeraPdfValidator().Validate(path, profile)

    report.IsCompliant |> should be True
    report.FlavourId |> should equal "ua2"
    report.TotalAssertions |> should be (greaterThan 0)
    report.FailedAssertions |> should be Empty

[<Fact>]
let ``Validate known bad PDF/UA-2 document reports non-compliant with ISO 14289-2 failures`` () =
    let path = corpusFile "pdfua2-fail.pdf"

    let report = VeraPdfValidator().Validate(path, fst (PdfUaTarget.detect path))

    report.IsCompliant |> should be False
    report.FlavourId |> should equal "ua2"
    report.FailedAssertions |> Seq.map (fun a -> a.Specification, a.Clause) |> should contain ("ISO 14289-2:2024", "5")

[<Fact>]
let ``a PDF/UA-2 file checked as PDF/UA-1 fails: its identifier claims the other part`` () =
    let report = VeraPdfValidator().Validate(corpusFile "pdfua2-pass.pdf", PdfProfile.PdfUa1)

    report.FlavourId |> should equal "ua1"
    report.IsCompliant |> should be False

[<Fact>]
let ``PDF/UA-2 failures map to the fixes that fix them, by rule`` () =
    use pdf20 = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(pdf20.Path, WriterProperties().SetPdfVersion(PdfVersion.PDF_2_0)))
        let page = document.AddNewPage()
        let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas(page)
        canvas.BeginText().SetFontAndSize(iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA, 12f)
            .MoveText(72.0, 720.0).ShowText("A PDF 2.0 page with a link.").EndText()
        |> ignore
        let link = iText.Kernel.Pdf.Annot.PdfLinkAnnotation(iText.Kernel.Geom.Rectangle(72f, 700f, 100f, 14f))
        page.AddAnnotation(link.SetAction(iText.Kernel.Pdf.Action.PdfAction.CreateURI "https://example.com")) |> ignore

    let report = VeraPdfValidator().Validate(pdf20.Path, PdfProfile.PdfUa2)
    let fixesFor rule =
        report.FailedAssertions |> Seq.filter (fun f -> f.Specification.StartsWith "ISO 14289-2" && f.RuleId = rule) |> Seq.map _.FixOptions |> Seq.head

    fixesFor "5-1" |> should equal PdfFixOptions.Metadata
    // Untitled, with no title page: no fix has a title to give it.
    fixesFor "8.11.1-1" |> should equal PdfFixOptions.None
    fixesFor "8.11.2-1" |> should equal PdfFixOptions.Title
    fixesFor "8.9.3.3-1" |> should equal (PdfFixOptions.Tabs ||| PdfFixOptions.TagFromLayout)
    // Untagged: tagging it from its layout (Suggest Structure…) fixes these.
    fixesFor "6.2-1" |> should equal PdfFixOptions.TagFromLayout
    fixesFor "8.2.1-1" |> should equal PdfFixOptions.TagFromLayout
    // Every "natural language" rule falls back on the document's /Lang.
    (fixesFor "8.4.4-1").HasFlag PdfFixOptions.Language |> should be True
    // And each has its guide.
    for finding in report.FailedAssertions do
        RuleGuide.forFinding finding |> Option.isSome |> should be True

[<Fact>]
let ``Fix given a report of the file as it is uses it, rather than validating it again; one of another file isn't used`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    use again = new TempPdf()
    createPdfWithUntitledFigures input.Path 2
    let report = VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1)
    let stages = Collections.Concurrent.ConcurrentQueue<FixStage>()
    let progress = { new IProgress<FixProgress> with member _.Report value = stages.Enqueue value.Stage }

    let fix = VeraPdfValidator(Validated = Some report).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.Metadata, progress)

    obj.ReferenceEquals(fix.OriginalReport, report) |> should be True
    stages |> should not' (contain FixStage.CheckingOriginal)
    // Another file's report: validated again.
    let other = { report with FilePath = "elsewhere.pdf" }
    let fixAgain = VeraPdfValidator(Validated = Some other).Fix(input.Path, PdfProfile.PdfUa1, again.Path, PdfFixOptions.Metadata)
    obj.ReferenceEquals(fixAgain.OriginalReport, other) |> should be False

// ---- Cancelling ----

[<Fact>]
let ``Validate cancelled stops veraPDF partway, and WCAG 2.2's pass isn't started`` () =
    use input = new TempPdf()
    createPdfWithUntitledFigures input.Path 30
    let full = VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1, checkWcag = false)
    use cancel = new Threading.CancellationTokenSource()
    let reports = Collections.Concurrent.ConcurrentQueue<ValidationProgress>()
    let progress =
        { new IProgress<ValidationProgress> with
            member _.Report value =
                reports.Enqueue value
                // At its first report (sampled four times a second: a stage it passes quickly may
                // not be).
                cancel.Cancel() }

    (fun () -> VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1, progress, cancel = cancel.Token) |> ignore)
    |> should throw typeof<OperationCanceledException>

    // Fewer checks than the whole file has: it stopped.
    (Seq.last reports).Checks |> should be (lessThan full.TotalAssertions)
    reports |> Seq.exists (fun r -> r.Stage = ValidationStage.CheckingWcag) |> should be False

[<Fact>]
let ``Validate, Fix and ReadStructureTree already cancelled don't start, and Fix writes nothing`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdfWithUntitledFigures input.Path 2
    let cancelled = Threading.CancellationToken(true)
    let validator = VeraPdfValidator()

    (fun () -> validator.Validate(input.Path, PdfProfile.PdfUa1, cancel = cancelled) |> ignore) |> should throw typeof<OperationCanceledException>
    (fun () -> validator.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.All, cancel = cancelled) |> ignore)
    |> should throw typeof<OperationCanceledException>
    (fun () -> validator.ReadStructureTree(input.Path, cancel = cancelled) |> ignore) |> should throw typeof<OperationCanceledException>
    IO.File.Exists output.Path |> should be False

[<Theory>]
[<InlineData(FixStage.Fixing)>]
// After Legibyl's own fixes have written their copy.
[<InlineData(FixStage.FixingMetadata)>]
let ``Fix cancelled partway writes nothing, and leaves no temporary file`` (stage: FixStage) =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdfWithUntitledFigures input.Path 2
    let report = VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1)
    use cancel = new Threading.CancellationTokenSource()
    // Cancelled as the stage starts, past the original's check.
    let progress = { new IProgress<FixProgress> with member _.Report value = if value.Stage = stage then cancel.Cancel() }
    let folder = IO.Path.GetDirectoryName output.Path |> nonNull
    let before = IO.Directory.GetFiles folder |> set

    (fun () ->
        VeraPdfValidator(Validated = Some report).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.All &&& ~~~PdfFixOptions.LinkTitlesFromWeb, progress, cancel.Token)
        |> ignore)
    |> should throw typeof<OperationCanceledException>

    IO.File.Exists output.Path |> should be False
    // (Tests run side by side: only files named for this output count.)
    IO.Directory.GetFiles folder
    |> Array.filter (fun file -> not (before.Contains file) && (string (IO.Path.GetFileName file)).Contains(string (IO.Path.GetFileNameWithoutExtension output.Path)))
    |> should be Empty
