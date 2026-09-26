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

/// Two paragraphs of two lines each, then a line introducing a bulleted item.
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
let private lines = [ for y in [ 700.0; 686.0; 650.0; 636.0; 600.0; 586.0 ] -> region (y - 4.0, y + 11.0) ]

/// How the page is laid out: a block per paragraph, the last holding the line and its item.
let private blocks = [ region (682.0, 712.0); region (632.0, 662.0); region (582.0, 612.0) ]

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
let ``paragraphs tagged line by line are joined where the layout shows one block, but not onto a list item`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    taggedLineByLine input.Path
    paragraphs input.Path |> List.length |> should equal 6

    let result =
        VeraPdfValidator(LayoutSource = FixedLayout blocks).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.JoinSplitParagraphs)

    result.AppliedFixes
    |> should contain "Joined 4 paragraphs tagged line by line into 2, where the page layout shows one block of text."
    paragraphs output.Path
    |> List.map (fun (tag, content) -> tag, (string content).Replace("  ", " ").Trim())
    |> should
        equal
        [ "P", "This handbook explains your benefits under the plan, and how you become a member of it."
          "P", "The second paragraph starts on this line and carries on to the next one."
          "P", "You will need:"
          "P", "• A completed form" ]
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

    pairs |> List.map fst |> List.sort |> should equal [ 0..5 ]
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
