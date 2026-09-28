module Legibyl.Validation.Tests.ParentTreeTests

open System.IO
open System.Text
open System.Text.RegularExpressions
open iText.Kernel.Pdf
open iText.Layout
open iText.Layout.Element
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A tagged page from iText: a paragraph, a list (L > LI > Lbl and LBody, nested marked
/// content), and a paragraph with a link.
let private createTagged (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    use layout = new Document(document)
    layout.Add(Paragraph("The garden in spring.")) |> ignore
    layout.Add(List().Add("Tulips").Add("Daffodils")) |> ignore
    layout.Add(Paragraph().Add(Link("Visit the garden", iText.Kernel.Pdf.Action.PdfAction.CreateURI "https://example.com"))) |> ignore

/// The same file with its parent tree null and no /StructParents on its pages, as Pages writes
/// them: edited in place, byte for byte, so iText doesn't rebuild it on saving. The link keeps its
/// /StructParent, which the parent tree no longer has.
let private breakParentTree (tagged: string) (broken: string) =
    let text = Encoding.Latin1.GetString(File.ReadAllBytes tagged)
    let text = Regex.Replace(text, @"/StructParents \d+", fun m -> string (System.String(' ', m.Length)))
    let text = Regex.Replace(text, @"/ParentTree (\d+ \d+ R)", fun m -> "/ParentTree " + "null".PadRight(m.Groups[1].Length))
    File.WriteAllBytes(broken, Encoding.Latin1.GetBytes text)

let private failedRules (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Seq.sort |> List.ofSeq

[<Fact>]
let ``a missing parent tree is warned about, and rebuilding it finds every tag again`` () =
    use tagged = new TempPdf()
    use broken = new TempPdf()
    use fixedCopy = new TempPdf()
    createTagged tagged.Path
    breakParentTree tagged.Path broken.Path

    let report = VeraPdfValidator().Validate(broken.Path, PdfProfile.PdfUa1)
    let warning = report.Findings |> Seq.find (fun f -> f.Specification = "Legibyl" && f.TestNumber = 9)
    let untagged = report.FailedAssertions |> Seq.filter (fun f -> f.RuleId = "7.1-3") |> List.ofSeq
    let link = report.FailedAssertions |> Seq.find (fun f -> f.RuleId = "7.18.5-1")

    warning.FixOptions |> should equal PdfFixOptions.ParentTree
    warning.ErrorMessage |> should haveSubstring "The parent tree is missing."
    untagged |> should not' (be Empty)
    // Every piece is in a tag, which the rebuilt tree finds: each is fixable, and names its tag.
    for finding in untagged do
        finding.FixOptions.HasFlag PdfFixOptions.ParentTree |> should be True
        finding.Element |> should not' (startWith "content not in the tags")
    link.FixOptions.HasFlag PdfFixOptions.ParentTree |> should be True

    let result = VeraPdfValidator().Fix(broken.Path, PdfProfile.PdfUa1, fixedCopy.Path, PdfFixOptions.ParentTree)

    result.AppliedFixes |> should contain "Rebuilt the missing parent tree, so the tags are found for 1 page's content and 1 tagged annotation."
    // As the file was before it lost its parent tree.
    failedRules fixedCopy.Path |> should equal (failedRules tagged.Path)
    VeraPdfValidator().Validate(fixedCopy.Path, PdfProfile.PdfUa1).Findings
    |> Seq.exists (fun f -> f.Specification = "Legibyl" && f.TestNumber = 9)
    |> should be False

[<Fact>]
let ``the rebuilt parent tree resolves every MCID, nested ones too, and the link by its StructParent`` () =
    use tagged = new TempPdf()
    use broken = new TempPdf()
    use fixedCopy = new TempPdf()
    createTagged tagged.Path
    breakParentTree tagged.Path broken.Path

    VeraPdfValidator().Fix(broken.Path, PdfProfile.PdfUa1, fixedCopy.Path, PdfFixOptions.ParentTree) |> ignore

    // iText finds each piece of content's element through the parent tree, as in the file before.
    let resolvedIn (path: string) =
        use document = new PdfDocument(new PdfReader(path))
        document.GetStructTreeRoot().GetPageMarkedContentReferences(document.GetPage 1) |> Seq.map _.GetMcid() |> Seq.sort |> List.ofSeq
    let resolved = resolvedIn fixedCopy.Path
    resolved |> should equal (resolvedIn tagged.Path)
    resolved.Length |> should be (greaterThan 5)
    use document = new PdfDocument(new PdfReader(fixedCopy.Path))
    let root = document.GetStructTreeRoot()
    let page = document.GetPage 1
    let annotation = page.GetAnnotations() |> Seq.exactlyOne
    let key = annotation.GetPdfObject().GetAsNumber(PdfName.StructParent).IntValue()
    let reference = root.FindObjRefByStructParentIndex(page.GetPdfObject(), key)
    (reference.GetParent() :?> iText.Kernel.Pdf.Tagging.PdfStructElem).GetRole() |> should equal PdfName.Link

[<Fact>]
let ``a file with its parent tree has no warning, and the fix has nothing to do`` () =
    use output = new TempPdf()
    let report = VeraPdfValidator().Validate(corpusFile "pdfua1-pass.pdf", PdfProfile.PdfUa1)
    report.Findings |> Seq.exists (fun f -> f.Specification = "Legibyl" && f.TestNumber = 9) |> should be False

    let result = VeraPdfValidator().Fix(corpusFile "pdfua1-pass.pdf", PdfProfile.PdfUa1, output.Path, PdfFixOptions.ParentTree)

    result.AppliedFixes |> should be Empty

[<Fact>]
let ``the parent tree fix is one of the safe ones, named parent-tree`` () =
    FixNames.safe.HasFlag PdfFixOptions.ParentTree |> should be True
    FixNames.toOptions [ "parent-tree" ] |> should equal PdfFixOptions.ParentTree

[<Fact>]
let ``a Pages export with no parent tree: rebuilding it leaves only the drawing outside every tag`` () =
    use output = new TempPdf()
    let before = failedRules (corpusFile "pages-clipped-paragraphs.pdf") |> List.filter ((=) "7.1-3")

    VeraPdfValidator().Fix(corpusFile "pages-clipped-paragraphs.pdf", PdfProfile.PdfUa1, output.Path, PdfFixOptions.ParentTree) |> ignore

    before.Length |> should equal 6
    failedRules output.Path |> List.filter ((=) "7.1-3") |> should equal [ "7.1-3" ]
