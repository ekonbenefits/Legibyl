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

let private spanish =
    [ "Aventuras, insignias y compañerismo: niños de preescolar a quinto grado"
      "prueban cosas nuevas junto a sus familias, guiados por padres voluntarios como usted."
      "¿Campamentos, cuevas, escalada y caminatas? ¡Sí, y también la carrera de autos de pino!" ]

/// A one-page PDF of these lines, with /Lang if given.
let private createPdf (path: string) (lines: string list) (language: string option) =
    use document = new PdfDocument(new PdfWriter(path))
    let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA, "WinAnsi")
    for i, line in List.indexed lines do
        canvas.BeginText().SetFontAndSize(font, 11f).MoveText(72.0, 700.0 - 16.0 * float i).ShowText(line).EndText() |> ignore
    language |> Option.iter (fun language -> document.GetCatalog().Put(PdfName.Lang, PdfString language) |> ignore)

let private fixLanguage (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.Language)

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
    language |> List.forall (fun f -> f.FixOptions.HasFlag PdfFixOptions.Language) |> should be True

[<Fact>]
let ``Spanish text is declared Spanish`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf input.Path spanish None
    fixLanguage input.Path output.Path |> ignore
    declaredLanguage output.Path |> should equal (Some "es")

/// Structure-19's warnings: each one's detail, and whether the fix is offered.
let private mismatches (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun finding -> finding.Specification = "Legibyl" && finding.TestNumber = 19)
    |> Seq.map (fun finding -> string finding.ErrorMessage, finding.FixOptions = PdfFixOptions.Language)
    |> List.ofSeq

[<Fact>]
let ``Spanish text in a document that says it's English is warned about, and the fix declares Spanish in its place`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf input.Path spanish (Some "en-US")
    mismatches input.Path |> should equal [ "It says en-US (English), but its text reads as es (Spanish).", true ]

    let result = fixLanguage input.Path output.Path

    result.AppliedFixes |> should contain "Declared the document's language as es (Spanish), which its text reads as, in place of en-US (English)."
    declaredLanguage output.Path |> should equal (Some "es")
    mismatches output.Path |> should be Empty

[<Fact>]
let ``a document that says the language its text is in, or a region of it, isn't warned about or changed`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf input.Path spanish (Some "es-MX")
    mismatches input.Path |> should be Empty
    (fixLanguage input.Path output.Path).WasFixed |> should be False

[<Fact>]
let ``the language a person chose is declared, in place of the one it says or its text reads as, whatever fixes are asked for`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    use again = new TempPdf()
    createPdf input.Path spanish (Some "en-US")

    let result = VeraPdfValidator(Language = "es-MX").Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.Metadata)

    result.AppliedFixes |> should contain "Declared the document's language as es-MX (Spanish), as you chose."
    declaredLanguage output.Path |> should equal (Some "es-MX")
    // Chosen again, it has it already: nothing to say.
    let again' = VeraPdfValidator(Language = "es-MX").Fix(output.Path, PdfProfile.PdfUa1, again.Path, PdfFixOptions.Language)
    again'.AppliedFixes |> Seq.filter (fun (line: string) -> line.Contains "language") |> should be Empty

[<Fact>]
let ``a chosen language is declared with no fixes asked for`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf input.Path spanish None
    VeraPdfValidator(Language = "es").Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.None) |> ignore
    declaredLanguage output.Path |> should equal (Some "es")
