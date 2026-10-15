module Legibyl.Validation.Tests.ParagraphJoinsTests

open System
open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// Two paragraphs of two lines each, a line introducing a bulleted item, then two sentences in
/// one block that don't read as one broken off (the first ends with a full stop).
let private createUntagged (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let text (y: float) (value: string) =
        canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, y).ShowText(value).EndText() |> ignore
    text 700.0 "This handbook explains your benefits under the"
    text 686.0 "plan, and how you become a member of it."
    text 650.0 "The second paragraph starts on this line and"
    text 636.0 "carries on to the next one."
    text 600.0 "You will need:"
    text 586.0 "• A completed form"
    text 550.0 "The garden opens at eight every morning."
    text 536.0 "Members lock the gate at dusk."

let private region (bottom, top) : LayoutRegion =
    { Tag = "P"
      Geometry = { PageNumber = 1; Left = 66.0; Bottom = bottom; Right = 400.0; Top = top }
      ListNumbering = null
      ActualText = null
      QrText = null
      Alt = null }

type private FixedLayout(regions: LayoutRegion list) =
    member val Asked = 0 with get, set

    interface IPageLayoutSource with
        member this.PageRegions(_, _) =
            this.Asked <- this.Asked + 1
            regions
        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = null

/// Each line its own region, as a paragraph tagged line by line.
let private lines = [ for y in [ 700.0; 686.0; 650.0; 636.0; 600.0; 586.0; 550.0; 536.0 ] -> region (y - 4.0, y + 11.0) ]

/// How the page is laid out: a block per paragraph, the third holding the line and its item.
let private blocks = [ region (682.0, 712.0); region (632.0, 662.0); region (582.0, 612.0); region (532.0, 562.0) ]

/// A PDF tagged line by line: a P per line.
let private taggedLineByLine (path: string) =
    use untagged = new TempPdf()
    createUntagged untagged.Path
    VeraPdfValidator(LayoutSource = FixedLayout lines).Fix(untagged.Path, PdfProfile.PdfUa1, path, PdfFixOptions.TagFromLayout)
    |> ignore

let private paragraphs (path: string) =
    let root = VeraPdfValidator().ReadStructureTree path |> notNull
    [ for element in (Seq.exactlyOne root.Children).Children -> element.Tag, element.Content ]

let private failedRules (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

/// The ParentTree's entries, flattened: key -> value.
let private parentTree (document: PdfDocument) =
    let rec entries (node: PdfDictionary) =
        [ match node.GetAsArray PdfName.Nums with
          | null -> ()
          | nums -> for i in 0..2 .. nums.Size() - 2 -> nums.GetAsNumber(i).IntValue(), nums.Get(i + 1, false)
          match node.GetAsArray PdfName.Kids with
          | null -> ()
          | kids -> for i in 0 .. kids.Size() - 1 do yield! entries (kids.GetAsDictionary i) ]
    let root = document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) |> notNull
    entries (root.GetAsDictionary PdfName.ParentTree |> notNull) |> Map.ofList

[<Fact>]
let ``paragraphs the split warning flags are joined where the layout shows one block, and no others`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    taggedLineByLine input.Path
    paragraphs input.Path |> List.length |> should equal 8

    let result =
        VeraPdfValidator(LayoutSource = FixedLayout blocks).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.JoinSplitParagraphs)

    result.AppliedFixes
    |> should contain "Joined 4 paragraphs tagged line by line or in pieces into 2, where the page layout shows one block of text."
    paragraphs output.Path
    |> List.map (fun (tag, content) -> tag, (string content).Replace("  ", " ").Trim())
    |> should
        equal
        [ "P", "This handbook explains your benefits under the plan, and how you become a member of it."
          "P", "The second paragraph starts on this line and carries on to the next one."
          "P", "You will need:"
          "P", "• A completed form"
          // One block, but the warning didn't flag them, so they're left as they are.
          "P", "The garden opens at eight every morning."
          "P", "Members lock the gate at dusk." ]
    failedRules output.Path |> should equal (failedRules input.Path)

[<Fact>]
let ``joined paragraphs own their content in the parent tree`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    taggedLineByLine input.Path
    VeraPdfValidator(LayoutSource = FixedLayout blocks).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.JoinSplitParagraphs)
    |> ignore

    use document = new PdfDocument(new PdfReader(output.Path))
    let page = document.GetPage 1
    let owners = (parentTree document)[page.GetPdfObject().GetAsNumber(PdfName.StructParents).IntValue()]
    let owners =
        match owners with
        | :? PdfIndirectReference as reference -> reference.GetRefersTo() :?> PdfArray
        | owners -> owners :?> PdfArray
    let root = document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) |> notNull
    let documentElement = root.GetAsDictionary PdfName.K |> Option.ofObj |> Option.defaultWith (fun () -> root.GetAsArray(PdfName.K).GetAsDictionary 0)
    let mcids (paragraph: PdfDictionary) =
        let kid (obj: PdfObject) =
            match obj with
            | :? PdfNumber as mcid -> [ mcid.IntValue() ]
            | :? PdfDictionary as reference when reference.ContainsKey PdfName.MCID -> [ reference.GetAsNumber(PdfName.MCID).IntValue() ]
            | _ -> []
        match paragraph.Get PdfName.K with
        | :? PdfArray as kids -> [ for i in 0 .. kids.Size() - 1 do yield! kid (kids.Get i) ]
        | kids -> kid kids
    let paragraphs = documentElement.GetAsArray PdfName.K
    let pairs =
        [ for i in 0 .. paragraphs.Size() - 1 do
              let paragraph = paragraphs.GetAsDictionary i
              for mcid in mcids paragraph -> mcid, paragraph ]

    pairs |> List.map fst |> List.sort |> should equal [ 0..7 ]
    for mcid, paragraph in pairs do
        owners.GetAsDictionary mcid |> should be (sameAs paragraph)

[<Fact>]
let ``without a layout, joining paragraphs does nothing`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    taggedLineByLine input.Path

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.JoinSplitParagraphs)

    result.WasFixed |> should be False

[<Fact>]
let ``the layout is only asked about pages with paragraphs next to each other`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    taggedLineByLine input.Path
    let layout = FixedLayout blocks

    VeraPdfValidator(LayoutSource = layout).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.JoinSplitParagraphs)
    |> ignore

    layout.Asked |> should equal 1

[<Fact>]
let ``a Spanish sentence tagged line by line is joined at Spanish's "Y", once the language fix has said it's Spanish`` () =
    use untagged = new TempPdf()
    use input = new TempPdf()
    use output = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(untagged.Path))
        let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
        let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA, "WinAnsi")
        let text (y: float) (value: string) =
            canvas.BeginText().SetFontAndSize(font, 10f).MoveText(72.0, y).ShowText(value).EndText() |> ignore
        text 700.0 "El jardín comunitario del barrio abre todos los sábados por la mañana para"
        text 686.0 "Y también los domingos por la tarde, con entrada libre para todas las familias."
    let wide (bottom, top) = { region (bottom, top) with Geometry = { PageNumber = 1; Left = 66.0; Bottom = bottom; Right = 560.0; Top = top } }
    VeraPdfValidator(LayoutSource = FixedLayout [ wide (696.0, 711.0); wide (682.0, 697.0) ]).Fix(untagged.Path, PdfProfile.PdfUa1, input.Path, PdfFixOptions.TagFromLayout)
    |> ignore
    paragraphs input.Path |> List.length |> should equal 2

    let result =
        VeraPdfValidator(LayoutSource = FixedLayout [ wide (682.0, 711.0) ])
            .Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.Language ||| PdfFixOptions.JoinSplitParagraphs)

    result.AppliedFixes |> should contain "Joined 2 paragraphs tagged line by line or in pieces into 1, where the page layout shows one block of text."
    paragraphs output.Path |> List.length |> should equal 1

// ---- Sentences tagged in pieces, as the page draws them ----

/// A piece of text: where it starts (x, baseline), its size, and its text, each tagged its own
/// paragraph (a region around it); pieces drawn one after another along a line follow on from
/// each other's ends.
type private Drawn = { X: float; Y: float; Size: float32; Text: string }

let private helvetica () = PdfFontFactory.CreateFont(StandardFonts.HELVETICA, "WinAnsi")

/// Pieces drawn one after another along the line at y, from x.
let private along (x: float) (y: float) (size: float32) (texts: string list) =
    let font = helvetica ()
    texts
    |> List.mapFold (fun x text -> { X = x; Y = y; Size = size; Text = text }, x + float (font.GetWidth(text, size))) x
    |> fst

/// A PDF whose pieces are each tagged as a paragraph, in the order given.
let private taggedInPieces (path: string) (pieces: Drawn list) =
    use untagged = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(untagged.Path))
        let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
        let font = helvetica ()
        for piece in pieces do
            canvas.BeginText().SetFontAndSize(font, piece.Size).MoveText(piece.X, piece.Y).ShowText(piece.Text).EndText() |> ignore
    let font = helvetica ()
    let regions =
        [ for piece in pieces ->
              let size = float piece.Size
              let left = piece.X + float (font.GetWidth(piece.Text.TrimEnd(), piece.Size)) - float (font.GetWidth(piece.Text.Trim(), piece.Size))
              { region (piece.Y - 0.3 * size, piece.Y + 0.9 * size) with
                  Geometry =
                    { PageNumber = 1
                      Left = left + 0.5
                      Bottom = piece.Y - 0.3 * size
                      Right = piece.X + float (font.GetWidth(piece.Text.TrimEnd(), piece.Size)) - 0.5
                      Top = piece.Y + 0.9 * size } } ]
    VeraPdfValidator(LayoutSource = FixedLayout regions).Fix(untagged.Path, PdfProfile.PdfUa1, path, PdfFixOptions.TagFromLayout)
    |> ignore
    paragraphs path |> List.length |> should equal pieces.Length

/// The split-paragraph warnings (Structure-2): each one's place.
let private splitWarnings (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun finding -> finding.Specification = "Legibyl" && finding.TestNumber = 2)
    |> List.ofSeq

/// A sentence with a date and an age dropped into it, each piece its own paragraph: along the
/// first line, wrapped onto the second, and along that. None reads as split by its words alone.
let private dropIns =
    along 72.0 700.0 12f [ "Payments assume they begin on "; "September 30, 2026"; " and that you are" ]
    @ along 72.0 686.0 12f [ "age "; "67"; " on that date." ]

[<Fact>]
let ``a sentence tagged in pieces along its lines is warned of as split, and joined where the layout shows one block`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    taggedInPieces input.Path dropIns

    let warnings = splitWarnings input.Path

    warnings |> List.length |> should equal 5
    warnings |> List.forall (fun warning -> warning.FixOptions = PdfFixOptions.JoinSplitParagraphs) |> should be True
    let result =
        VeraPdfValidator(LayoutSource = FixedLayout [ { region (680.0, 712.0) with Geometry = { PageNumber = 1; Left = 66.0; Bottom = 680.0; Right = 560.0; Top = 712.0 } } ])
            .Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.JoinSplitParagraphs)
    result.AppliedFixes
    |> should contain "Joined 6 paragraphs tagged line by line or in pieces into 1, where the page layout shows one block of text."
    paragraphs output.Path
    |> List.map (fun (_, content) -> System.Text.RegularExpressions.Regex.Replace(string content, @"\s+", " ").Trim())
    |> should equal [ "Payments assume they begin on September 30, 2026 and that you are age 67 on that date." ]
    splitWarnings output.Path |> should be Empty

[<Fact>]
let ``a heading over its text isn't a wrapped line: its text's first word would have fitted after it`` () =
    use input = new TempPdf()
    taggedInPieces
        input.Path
        [ { X = 72.0; Y = 700.0; Size = 12f; Text = "Payments and your age" }
          { X = 72.0; Y = 686.0; Size = 12f; Text = "Your payments begin when you reach the age the plan sets." } ]

    splitWarnings input.Path |> should be Empty

[<Fact>]
let ``a larger heading as wide as its text isn't a wrapped line`` () =
    use input = new TempPdf()
    taggedInPieces
        input.Path
        [ { X = 72.0; Y = 700.0; Size = 18f; Text = "Payments and your age at retirement" }
          { X = 72.0; Y = 682.0; Size = 10f; Text = "Your payments begin when you reach the age the plan sets." } ]

    splitWarnings input.Path |> should be Empty

[<Fact>]
let ``paragraphs spaced further apart than lines aren't a wrapped line`` () =
    use input = new TempPdf()
    taggedInPieces
        input.Path
        [ { X = 72.0; Y = 700.0; Size = 12f; Text = "Payments and your age at retirement under the plan" }
          { X = 72.0; Y = 676.0; Size = 12f; Text = "Your payments begin when you reach the age." } ]

    splitWarnings input.Path |> should be Empty

[<Fact>]
let ``a paragraph ending in a quotation isn't wrapped into the next`` () =
    use input = new TempPdf()
    taggedInPieces
        input.Path
        [ { X = 72.0; Y = 700.0; Size = 12f; Text = "Economists gave the years after it a name: the “jobless recovery.”" }
          { X = 72.0; Y = 686.0; Size = 12f; Text = "Hiring picked up again the next year." } ]

    splitWarnings input.Path |> should be Empty

[<Fact>]
let ``a list item's number beside its text isn't a piece of its sentence`` () =
    use input = new TempPdf()
    taggedInPieces
        input.Path
        [ { X = 72.0; Y = 700.0; Size = 12f; Text = "1." }
          { X = 86.0; Y = 700.0; Size = 12f; Text = "Your payments begin when you reach the age the plan sets." } ]

    splitWarnings input.Path |> should be Empty

[<Fact>]
let ``a piece starting with a list marker on the same line isn't the rest of the sentence before it`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    taggedInPieces input.Path (along 72.0 700.0 12f [ "Your options include "; "1. a lump sum" ])

    splitWarnings input.Path |> should be Empty
    VeraPdfValidator(LayoutSource = FixedLayout [ { region (690.0, 712.0) with Geometry = { PageNumber = 1; Left = 66.0; Bottom = 690.0; Right = 560.0; Top = 712.0 } } ])
        .Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.JoinSplitParagraphs)
    |> _.WasFixed
    // Nothing joined: no copy written.
    |> should be False

[<Fact>]
let ``paragraphs spaced a little further apart than lines aren't a wrapped line, whatever letters they have`` () =
    use input = new TempPdf()
    // Lines 14 points apart at 12 points, paragraphs 3 more: the first line's descenders and the
    // next's capitals bring their glyph boxes nearer than its lines' are.
    taggedInPieces
        input.Path
        [ { X = 72.0; Y = 700.0; Size = 12f; Text = "Payments and your age at retirement under the plan, saying" }
          { X = 72.0; Y = 683.0; Size = 12f; Text = "Your payments begin when you reach the age." } ]

    splitWarnings input.Path |> should be Empty

[<Fact>]
let ``pieces along a line with a gap but no space between them aren't joined, which would run their words together`` () =
    use input = new TempPdf()
    let font = helvetica ()
    let first = "Your rate this year is"
    taggedInPieces
        input.Path
        [ { X = 72.0; Y = 700.0; Size = 12f; Text = first }
          { X = 72.0 + float (font.GetWidth(first, 12f)) + 4.0; Y = 700.0; Size = 12f; Text = "4.79% of what you earn" } ]

    splitWarnings input.Path |> should be Empty

[<Fact>]
let ``lines of text drawn sideways aren't pieces of one line`` () =
    use untagged = new TempPdf()
    use input = new TempPdf()
    let font = helvetica ()
    // Turned a quarter turn, reading upwards: each line to the right of the one before.
    let lines = [ 100.0, "Payments and your age at retirement under the plan"; 114.0, "Your payments begin when you reach the age." ]
    do
        use document = new PdfDocument(new PdfWriter(untagged.Path))
        let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
        for x, text in lines do
            canvas.BeginText().SetFontAndSize(font, 12f).SetTextMatrix(0f, 1f, -1f, 0f, float32 x, 300f).ShowText(text).EndText() |> ignore
    let regions =
        [ for x, text in lines ->
              { region (300.0, 300.0 + float (font.GetWidth(text, 12f))) with
                  Geometry = { PageNumber = 1; Left = x - 10.8; Bottom = 300.0; Right = x + 3.6; Top = 300.0 + float (font.GetWidth(text, 12f)) } } ]
    VeraPdfValidator(LayoutSource = FixedLayout regions).Fix(untagged.Path, PdfProfile.PdfUa1, input.Path, PdfFixOptions.TagFromLayout)
    |> ignore
    paragraphs input.Path |> List.length |> should equal 2

    splitWarnings input.Path |> should be Empty
