module Legibyl.Validation.Tests.DocumentRootTests

open System
open System.IO
open iText.Kernel.Pdf
open iText.Layout
open iText.Layout.Element
open Xunit
open FsUnit.Xunit
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A tagged PDF of paragraphs (iText tags them under a Document), with its structure tree root
/// then changed by change.
let private create (path: string) (version: PdfVersion) (paragraphs: string list) (change: PdfDocument -> PdfDictionary -> unit) =
    let tagged = path + ".tagged.pdf"
    try
        do
            let pdf = new PdfDocument(new PdfWriter(tagged, WriterProperties().SetPdfVersion version))
            pdf.SetTagged() |> ignore
            pdf.GetCatalog().SetLang(PdfString "en-US") |> ignore
            use document = new Document(pdf)
            for text in paragraphs do
                document.Add(Paragraph text) |> ignore
        use pdf = new PdfDocument(new PdfReader(tagged), new PdfWriter(path))
        change pdf (pdf.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot |> notNull)
    finally
        File.Delete tagged

/// The structure tree root's only kid, iText's Document.
let private documentOf (root: PdfDictionary) =
    match root.Get PdfName.K with
    | :? PdfArray as kids -> kids.GetAsDictionary 0 |> notNull
    | :? PdfDictionary as kid -> kid
    | _ -> failwith "no Document"

/// Its Document retagged as a Part.
let private partAtTop (_: PdfDocument) (root: PdfDictionary) = (documentOf root).Put(PdfName.S, PdfName.Part) |> ignore

/// Its Document taken out: its kids straight under the root.
let private noDocument (_: PdfDocument) (root: PdfDictionary) =
    let document = documentOf root
    let kids = document.GetAsArray PdfName.K |> notNull
    for i in 0 .. kids.Size() - 1 do
        kids.GetAsDictionary(i) |> notNull |> fun kid -> kid.Put(PdfName.P, root) |> ignore
    root.Put(PdfName.K, kids) |> ignore

let private failed (path: string) (profile: PdfProfile) =
    VeraPdfValidator().Validate(path, profile).FailedAssertions |> List.ofSeq

let private ruleIds (failures: RuleViolation list) = failures |> List.map (fun f -> $"{f.Clause}-{f.TestNumber}") |> Set.ofList

/// The tags from the root down, as "Tag(child, child)".
let rec private shape (node: StructureNode) : string =
    match List.ofSeq node.Children with
    | [] -> node.Tag
    | children -> $"""{node.Tag}({String.Join(", ", children |> List.map shape)})"""

let private render (path: string) =
    use renderer = new PdfRegionRenderer()
    let offPage = { PageNumber = 1; Left = -20.0; Bottom = -20.0; Right = -10.0; Top = -10.0 }
    use bitmap = SkiaSharp.SKBitmap.Decode(renderer.RenderPageWithHighlight(path, offPage).PngBytes)
    bitmap.Pixels

let private fix (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.DocumentRoot)

[<Fact>]
let ``a PDF 2.0 file whose tags start with a Part is put under a Document, passing PDF/UA-2's root rules`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    create input.Path PdfVersion.PDF_2_0 [ "The garden opens at eight." ] partAtTop
    let rootRules = set [ "8.2.5.2-1"; "Table 5. StructTreeRoot-Document-1"; "Table 5. StructTreeRoot-Part-1" ]
    let before = failed input.Path PdfProfile.PdfUa2
    Set.intersect (ruleIds before) rootRules |> should equal rootRules
    // Each offers the fix.
    before
    |> List.filter (fun f -> rootRules.Contains $"{f.Clause}-{f.TestNumber}")
    |> List.forall (fun f -> f.FixOptions.HasFlag PdfFixOptions.DocumentRoot)
    |> should be True

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa2, output.Path, PdfFixOptions.DocumentRoot)

    result.WasFixed |> should be True
    result.AppliedFixes |> should contain "Put the Part tag at the top of the structure under a Document tag."
    let after = failed output.Path PdfProfile.PdfUa2
    after |> List.filter (fun f -> f.Clause.StartsWith "Table 5. StructTreeRoot" || f.Clause = "8.2.5.2") |> should be Empty
    // Nothing else is worse, and the page is drawn the same.
    Set.difference (ruleIds after) (ruleIds before) |> should be Empty
    render output.Path |> should equal (render input.Path)
    shape (VeraPdfValidator().ReadStructureTree output.Path |> notNull) |> should startWith "StructTreeRoot(Document(Part(P"

[<Fact>]
let ``a PDF 1.7 file whose tags start with a Part is warned about, and the fix puts it under a Document`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    create input.Path PdfVersion.PDF_1_7 [ "The garden opens at eight." ] partAtTop
    let structure14 (path: string) =
        VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
        |> Seq.filter (fun a -> a.Specification = "Legibyl" && a.TestNumber = 16)
        |> List.ofSeq
    let warning = structure14 input.Path |> List.exactlyOne
    warning.ErrorMessage |> should equal "At the top: Part."
    warning.FixOptions |> should equal PdfFixOptions.DocumentRoot

    let result = fix input.Path output.Path

    result.WasFixed |> should be True
    structure14 output.Path |> should be Empty
    Set.difference (ruleIds (failed output.Path PdfProfile.PdfUa1)) (ruleIds (failed input.Path PdfProfile.PdfUa1)) |> should be Empty
    render output.Path |> should equal (render input.Path)

[<Fact>]
let ``several tags at the top go under one Document, in their order`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    create input.Path PdfVersion.PDF_1_7 [ "First."; "Second."; "Third." ] noDocument
    shape (VeraPdfValidator().ReadStructureTree input.Path |> notNull) |> should startWith "StructTreeRoot(P"

    let result = fix input.Path output.Path

    result.AppliedFixes |> should contain "Put the 3 tags at the top of the structure (P, P, P) under a single Document tag."
    let tree = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    // (iText tags a paragraph's text as a Span in it.)
    shape tree |> should equal "StructTreeRoot(Document(P(Span), P(Span), P(Span)))"
    let document = tree.Children |> Seq.exactlyOne
    [ for p in document.Children -> (Seq.exactlyOne p.Children).Content ] |> should equal [ "First."; "Second."; "Third." ]

[<Fact>]
let ``tags that already start with a single Document are left alone`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    create input.Path PdfVersion.PDF_1_7 [ "The garden opens at eight." ] (fun _ _ -> ())

    let result = fix input.Path output.Path

    result.WasFixed |> should be False
    File.Exists output.Path |> should be False

[<Fact>]
let ``a PDF 2.0 file's Document outside PDF 2.0's namespace is given it`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    create input.Path PdfVersion.PDF_2_0 [ "The garden opens at eight." ] (fun _ root -> (documentOf root).Remove PdfName.NS |> ignore)
    ruleIds (failed input.Path PdfProfile.PdfUa2) |> should contain "8.2.5.2-2"

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa2, output.Path, PdfFixOptions.DocumentRoot)

    result.AppliedFixes |> should contain "Gave the Document tag at the top of the structure PDF 2.0's namespace."
    ruleIds (failed output.Path PdfProfile.PdfUa2) |> should not' (contain "8.2.5.2-2")

[<Fact>]
let ``content straight under the root isn't wrapped: the warning says why, and the fix says so`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // The root holds the Document, and a marked-content reference of its own.
    create input.Path PdfVersion.PDF_1_7 [ "The garden opens at eight." ] (fun _ root ->
        let kids = PdfArray()
        kids.Add(documentOf root)
        kids.Add(PdfNumber 0)
        root.Put(PdfName.K, kids) |> ignore)
    let warning =
        VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1).Assertions
        |> Seq.filter (fun a -> a.Specification = "Legibyl" && a.TestNumber = 16)
        |> Seq.exactlyOne
    warning.FixOptions |> should equal PdfFixOptions.None
    warning.ErrorMessage |> notNull |> should startWith "Not fixable here:"

    let result = fix input.Path output.Path

    result.WasFixed |> should be False
    result.NotApplied |> Seq.exists (fun n -> n.StartsWith("The tags weren't put under a single Document tag", StringComparison.Ordinal)) |> should be True
