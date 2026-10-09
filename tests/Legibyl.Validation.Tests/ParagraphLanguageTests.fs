module Legibyl.Validation.Tests.ParagraphLanguageTests

open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Layout.Element
open iText.Kernel.Pdf.Tagging
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

let private render (path: string) =
    use document = new Legibyl.Rendering.PdfiumDocument(path)
    use page = document.Render(1, 1f)
    page.Pixels |> Array.map (fun c -> c.Red, c.Green, c.Blue, c.Alpha)

let private failures (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq

[<Fact>]
let ``Spanish in a heading, a list item and a table cell is marked on the block that holds it, the page the same and nothing worse`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    use again = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        document.SetTagged() |> ignore
        document.GetCatalog().SetLang(PdfString "en-US") |> ignore
        use layout = new iText.Layout.Document(document)
        layout.SetFont(PdfFontFactory.CreateFont(StandardFonts.HELVETICA, "WinAnsi")) |> ignore
        let heading = Paragraph "Horario de apertura del jardín comunitario"
        heading.GetAccessibilityProperties().SetRole StandardRoles.H1 |> ignore
        layout.Add heading |> ignore
        layout.Add(Paragraph "The community garden is open on Saturday mornings for every family.") |> ignore
        let list = List()
        list.Add(ListItem "Traiga guantes, agua y un sombrero para el sol.") |> ignore
        list.Add(ListItem "Bring gloves, water and a hat for the sun.") |> ignore
        layout.Add list |> ignore
        let table = Table(2)
        table.AddCell(Cell().Add(Paragraph "Opening hours on Saturdays")) |> ignore
        table.AddCell(Cell().Add(Paragraph "Los sábados por la mañana, de ocho a doce")) |> ignore
        layout.Add table |> ignore
    // (The list item's text is in a paragraph inside its body: that's the block marked.)
    let marked =
        [ "H1", "Horario de apertura"; "P", "Traiga guantes"; "P", "Los sábados" ]

    VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.PassageLanguages) |> ignore

    let root = VeraPdfValidator().ReadStructureTree output.Path |> notNull
    let rec all (node: StructureNode) : StructureNode list = node :: [ for child in node.Children do yield! all child ]
    let rec text (node: StructureNode) = string node.Content + String.concat "" [ for child in node.Children -> text child ]
    [ for node in all root do
          if not (isNull node.Language) then node.Tag, (text node).Substring(0, min 20 (text node).Length) ]
    |> List.map (fun (tag, start) -> tag, marked |> List.exists (fun (t, s) -> t = tag && start.StartsWith s))
    |> should equal [ "H1", true; "P", true; "P", true ]
    Set.difference (failures output.Path) (failures input.Path) |> should be Empty
    render output.Path |> should equal (render input.Path)
    // Marked, it has nothing left to do: no copy written.
    (VeraPdfValidator().Fix(output.Path, PdfProfile.PdfUa1, again.Path, PdfFixOptions.PassageLanguages)).WasFixed |> should be False

[<Fact>]
let ``a short document that says no language is declared in the one most of its passages are in, though its first is in another`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // A short flyer: too little text for the whole to tell, its passages two thirds English.
    createPdf input.Path "" [ "Bienvenidos a todos amigos"; "The garden opens on Saturdays"; "Bring your gloves and water" ]

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.Language)

    result.AppliedFixes |> Seq.exists (fun (line: string) -> line.Contains "the language most of its passages are in") |> should be True

    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetCatalog().GetPdfObject().GetAsString(PdfName.Lang).ToUnicodeString() |> should equal "en"

[<Fact>]
let ``Catalan in a Catalan document, and Portuguese or Italian in an English one, aren't taken for Spanish`` () =
    use catalan = new TempPdf()
    use english = new TempPdf()
    createPdf
        catalan.Path
        "ca"
        [ "El jardí comunitari obre els dissabtes al matí per a totes les famílies del barri."
          "Porteu guants, aigua i un barret; nosaltres tenim les eines i les llavors." ]
    createPdf
        english.Path
        "en-US"
        [ "The community garden is open on Saturday mornings for every family."
          "O jardim comunitário abre aos sábados de manhã para todas as famílias do bairro."
          "Il giardino comunitario è aperto il sabato mattina per tutte le famiglie del quartiere." ]
    passages catalan.Path |> should be Empty
    passages english.Path |> should be Empty
