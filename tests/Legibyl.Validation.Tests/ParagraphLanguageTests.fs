module Legibyl.Validation.Tests.ParagraphLanguageTests

open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Layout.Element
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A tagged page of these paragraphs (each a P), in the language declared ("" for none).
let private createPdf (path: string) (language: string) (paragraphs: string list) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    if language <> "" then document.GetCatalog().SetLang(PdfString language) |> ignore
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
let ``a document in a language with no pack is warned about for a paragraph going on in lower case, not for one starting with English's words`` () =
    use pdf = new TempPdf()
    use lower = new TempPdf()
    // English's "And" doesn't continue a Japanese sentence; lower case still reads as going on.
    createPdf pdf.Path "ja" [ "The garden is open on Saturday mornings for"; "And on Sundays in the afternoon, free for all." ]
    createPdf lower.Path "ja" [ "The garden is open on Saturday mornings for"; "on Sundays in the afternoon, free for all." ]
    splits pdf.Path |> should equal 0
    splits lower.Path |> should equal 1

// ---- Passages in another language (Structure-20) ----

/// A flyer in English and Spanish side by side: each line in both.
let private bilingual =
    [ "The community garden is open on Saturday mornings for every family."
      "El jardín comunitario abre los sábados por la mañana para todas las familias."
      "Bring gloves, water and a hat; we have the tools and the seeds."
      "Traiga guantes, agua y un sombrero; nosotros tenemos las herramientas y las semillas." ]

/// Structure-20's warnings: each one's detail, and whether the fix is offered.
let private passages (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun finding -> finding.Specification = "Legibyl" && finding.TestNumber = 20)
    |> Seq.map (fun finding -> (string finding.ErrorMessage).Split(':').[0..1] |> String.concat ":", finding.FixOptions = PdfFixOptions.PassageLanguages)
    |> List.ofSeq

/// Each paragraph's own language (None where it says none).
let private languages (path: string) =
    let root = VeraPdfValidator().ReadStructureTree path |> notNull
    let rec paragraphs (node: StructureNode) : StructureNode list =
        (if node.Tag = "P" then [ node ] else []) @ [ for child in node.Children do yield! paragraphs child ]
    paragraphs root |> List.map (fun paragraph -> Option.ofObj paragraph.Language)

[<Fact>]
let ``in an English document, the Spanish passages are warned about, and the fix marks them Spanish`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPdf input.Path "en-US" bilingual
    passages input.Path |> should equal [ "Page 1: reads as es (Spanish), in a part read as en-US (English)", true; "Page 1: reads as es (Spanish), in a part read as en-US (English)", true ]

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.PassageLanguages)

    result.AppliedFixes |> should contain "Marked 2 passage(s) in another language than the one around them with their own: 2 in es (Spanish)."
    languages output.Path |> should equal [ None; Some "es"; None; Some "es" ]
    passages output.Path |> should be Empty

[<Fact>]
let ``a bilingual document that says no language is declared in its first passage's, and the others marked with theirs`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    use declared = new TempPdf()
    createPdf input.Path "" bilingual

    VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.Language ||| PdfFixOptions.PassageLanguages) |> ignore

    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetCatalog().GetPdfObject().GetAsString(PdfName.Lang).ToUnicodeString() |> should equal "en"
    languages output.Path |> should equal [ None; Some "es"; None; Some "es" ]

[<Fact>]
let ``a passage too short to tell is left as it is`` () =
    use pdf = new TempPdf()
    createPdf pdf.Path "en-US" [ "The garden is open on Saturdays for all the families."; "Hola amigos"; "Gracias" ]
    passages pdf.Path |> should be Empty
