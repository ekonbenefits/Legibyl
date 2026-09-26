module Legibyl.Validation.Tests.DocumentLanguageTests

open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private english =
    [ "Adventures, badges and belonging, where kindergarten through fifth graders"
      "try new things alongside their families, led by volunteer parents just like you."
      "Campouts, caving, climbing, hiking, the pinewood derby and Scouting for Food." ]

let private french =
    [ "Aventures, badges et appartenance : les enfants de la maternelle jusqu'au"
      "primaire essaient de nouvelles choses avec leur famille, avec des parents"
      "bénévoles comme vous. Camping, spéléologie, escalade et randonnée." ]

/// A one-page PDF of these lines, with /Lang if given.
let private createPdf (path: string) (lines: string list) (language: string option) =
    use document = new PdfDocument(new PdfWriter(path))
    let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA, "WinAnsi")
    for i, line in List.indexed lines do
        canvas.BeginText().SetFontAndSize(font, 11f).MoveText(72.0, 700.0 - 16.0 * float i).ShowText(line).EndText() |> ignore
    language |> Option.iter (fun language -> document.GetCatalog().Put(PdfName.Lang, PdfString language) |> ignore)

let private fixLanguage (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.LanguageFromText)

let private declaredLanguage (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    document.GetCatalog().GetPdfObject().GetAsString(PdfName.Lang) |> Option.ofObj |> Option.map _.ToUnicodeString()

[<Fact>]
let ``a document without a language gets the one its text is in`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf input.Path english None

    let result = fixLanguage input.Path output.Path

    result.AppliedFixes |> should contain "Declared the document's language as en (English), detected from its text."
    declaredLanguage output.Path |> should equal (Some "en")
    VeraPdfValidator().Validate(output.Path, PdfProfile.PdfUa1).Findings
    |> Seq.exists (fun f -> not f.Passed && f.Message.Contains "Natural language")
    |> should be False

[<Fact>]
let ``French text is declared French`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf input.Path french None
    fixLanguage input.Path output.Path |> ignore
    declaredLanguage output.Path |> should equal (Some "fr")

[<Fact>]
let ``too little text, or a language already declared, is left alone`` () =
    use short = new TempPdf()
    use shortOutput = new TempPdf()
    createPdf short.Path [ "Pack 721" ] None
    (fixLanguage short.Path shortOutput.Path).OutputPath |> should be Null

    use declared = new TempPdf()
    use declaredOutput = new TempPdf()
    createPdf declared.Path english (Some "en-US")
    (fixLanguage declared.Path declaredOutput.Path).OutputPath |> should be Null

[<Fact>]
let ``undetermined language findings are fixable by declaring the language`` () =
    use input = new TempPdf()
    createPdf input.Path english None
    let report = VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1)
    let language = report.Findings |> Seq.filter (fun f -> not f.Passed && f.Message.Contains "Natural language") |> List.ofSeq
    language |> should not' (be Empty)
    language |> List.forall (fun f -> f.FixOptions.HasFlag PdfFixOptions.LanguageFromText) |> should be True
