module Legibyl.Validation.Tests.EmbeddedReviewTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open iText.Kernel.Pdf
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A page with an annotation but no tab order, for the AnnotationTabs fix to repair.
let private createNeedingTabs (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let annotations = PdfArray()
    annotations.Add(PdfDictionary())
    document.AddNewPage().GetPdfObject().Put(PdfName.Annots, annotations) |> ignore

let private review = """{"version":1,"dismissedWarnings":[{"rule":"Legibyl Structure-2","location":"*","message":"Likely split paragraph"}],"title":"Café menu"}"""

let private failedRules (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

[<Fact>]
let ``the fixed copy carries the review, compressed, out of readers' sight`` () =
    use input = new TempPdf()
    use withReview = new TempPdf()
    use without = new TempPdf()
    createNeedingTabs input.Path

    VeraPdfValidator(Review = review).Fix(input.Path, PdfProfile.PdfUa1, withReview.Path, PdfFixOptions.AnnotationTabs) |> ignore
    VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, without.Path, PdfFixOptions.AnnotationTabs) |> ignore

    EmbeddedReview.read withReview.Path |> should equal (Some review)
    EmbeddedReview.read without.Path |> should equal None
    use document = new PdfDocument(new PdfReader(withReview.Path))
    let stream = document.GetCatalog().GetPdfObject().GetAsStream(PdfName "LBYL_Review") |> notNull
    stream.GetAsName PdfName.Filter |> should equal PdfName.FlateDecode
    // Nothing a validator minds.
    failedRules withReview.Path |> should equal (failedRules without.Path)

[<Fact>]
let ``a review is only embedded when the fix saves a copy`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createNeedingTabs input.Path
    VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.AnnotationTabs) |> ignore

    // Already has its tab order: nothing to fix, so no copy.
    let result = VeraPdfValidator(Review = review).Fix(output.Path, PdfProfile.PdfUa1, input.Path + ".again.pdf", PdfFixOptions.AnnotationTabs)

    result.WasFixed |> should be False
    File.Exists(input.Path + ".again.pdf") |> should be False

[<Fact>]
let ``embedding adds to the file without changing what was there`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createNeedingTabs input.Path

    EmbeddedReview.write input.Path output.Path review

    let before = File.ReadAllBytes input.Path
    let after = File.ReadAllBytes output.Path
    after.Length |> should be (greaterThan before.Length)
    after[.. before.Length - 1] |> should equal before

[<Fact>]
let ``a PDF without a review, or a file that isn't a PDF, reads as none`` () =
    use input = new TempPdf()
    createNeedingTabs input.Path
    EmbeddedReview.read input.Path |> should equal None

    use notPdf = new TempPdf()
    File.WriteAllText(notPdf.Path, "not a PDF")
    EmbeddedReview.read notPdf.Path |> should equal None
    EmbeddedReview.read (input.Path + ".missing") |> should equal None

[<Fact>]
let ``a PDF that can't be opened reads as none`` () =
    use input = new TempPdf()
    createNeedingTabs input.Path
    if not (OperatingSystem.IsWindows()) then
        File.SetUnixFileMode(input.Path, UnixFileMode.None)
        try
            EmbeddedReview.read input.Path |> should equal None
        finally
            File.SetUnixFileMode(input.Path, UnixFileMode.UserRead ||| UnixFileMode.UserWrite)
