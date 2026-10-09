module Legibyl.Validation.Tests.ParagraphLanguageTests

open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Layout.Element
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A tagged page of these paragraphs (each a P), in the language declared.
let private createPdf (path: string) (language: string) (paragraphs: string list) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString language) |> ignore
    use layout = new iText.Layout.Document(document)
    layout.SetFont(PdfFontFactory.CreateFont(StandardFonts.HELVETICA, "WinAnsi")) |> ignore
    for text in paragraphs do
        layout.Add(Paragraph text) |> ignore

/// How many likely split paragraphs (Structure-2) are warned about.
let private splits (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun finding -> finding.Specification = "Legibyl" && finding.TestNumber = 2)
    |> Seq.length

/// A sentence split before a capitalised "Y" (and): Spanish's word, not English's.
let private splitBeforeY =
    [ "El jardín comunitario abre los sábados por la mañana"
      "Y los domingos por la tarde, con entrada libre para todos." ]

/// A section heading in capitals (its accent lost), then a sentence going on in lower case.
let private afterSection =
    [ "SECCION 3 Horario de apertura del jardín"
      "los sábados por la mañana y los domingos por la tarde." ]

[<Fact>]
let ``a Spanish paragraph starting with Spanish's "and" is warned about as split, and not in an English document`` () =
    use spanish = new TempPdf()
    use english = new TempPdf()
    createPdf spanish.Path "es-ES" splitBeforeY
    createPdf english.Path "en-US" splitBeforeY
    splits spanish.Path |> should equal 1
    splits english.Path |> should equal 0

[<Fact>]
let ``a Spanish section heading stands on its own, accent or not, where English's words don't know it`` () =
    use spanish = new TempPdf()
    use english = new TempPdf()
    createPdf spanish.Path "es" afterSection
    createPdf english.Path "en" afterSection
    splits spanish.Path |> should equal 0
    splits english.Path |> should equal 1

[<Fact>]
let ``a document in a language with no pack is warned about only for a paragraph going on in lower case`` () =
    use pdf = new TempPdf()
    createPdf pdf.Path "ja" splitBeforeY
    splits pdf.Path |> should equal 0
