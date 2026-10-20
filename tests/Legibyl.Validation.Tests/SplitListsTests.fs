module Legibyl.Validation.Tests.SplitListsTests

open System.IO
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// What's on the page, top to bottom: a paragraph tagged P straight under Document, or a list
/// tagged L > LI > LBody, each item's text in its LBody. Each line is drawn at its left edge and
/// its line (0 at the top, 20 points apart).
type private Entry =
    | Paragraph of left: float * line: int * text: string
    | List of (float * int * string) list

let private element (tag: string) (page: PdfPage) (kids: PdfObject list) =
    let element = PdfDictionary()
    element.Put(PdfName.Type, PdfName.StructElem) |> ignore
    element.Put(PdfName.S, PdfName tag) |> ignore
    element.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
    let array = PdfArray()
    kids |> List.iter array.Add
    element.Put(PdfName.K, array) |> ignore
    element

/// As createPdf, with adjust given each element it makes (by its tag) and the document's catalog.
let private createPdfWith (path: string) (entries: Entry list) (adjust: string -> PdfDictionary -> unit) (catalog: PdfDictionary -> unit) =
    let element tag page kids =
        let made = element tag page kids
        adjust tag made
        made
    use document = new PdfDocument(new PdfWriter(path))
    catalog (document.GetCatalog().GetPdfObject())
    let page = document.AddNewPage()
    let canvas = PdfCanvas(page)
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA)
    let mutable mcid = 0
    let draw (tag: string) (left: float, line: int, text: string) =
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
        canvas
            .BeginMarkedContent(PdfName tag, properties)
            .BeginText()
            .SetFontAndSize(font, 12f)
            .MoveText(left, 700.0 - float line * 20.0)
            .ShowText(text)
            .EndText()
            .EndMarkedContent()
        |> ignore
        mcid <- mcid + 1
        PdfNumber(mcid - 1) :> PdfObject
    let body =
        [ for entry in entries ->
              match entry with
              | Paragraph(left, line, text) -> element "P" page [ draw "P" (left, line, text) ] :> PdfObject
              | List items -> element "L" page [ for item in items -> element "LI" page [ element "LBody" page [ draw "LBody" item ] ] ] ]
    let root = PdfDictionary()
    root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    root.Put(PdfName.K, element "Document" page body) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, root) |> ignore

let private createPdf (path: string) (entries: Entry list) = createPdfWith path entries (fun _ _ -> ()) ignore

let private splitWarnings (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun a -> a.Specification = "Legibyl" && a.TestNumber = 22)
    |> List.ofSeq

/// The document's structure below Document, as nested tags: "L(LI(LBody),...)".
let private shape (path: string) =
    let rec describe (node: StructureNode) =
        if node.Children.Count = 0 then node.Tag
        else $"""{node.Tag}({node.Children |> Seq.map describe |> String.concat ","})"""
    let root = VeraPdfValidator().ReadStructureTree path |> notNull
    (Seq.exactlyOne root.Children).Children |> Seq.map describe |> List.ofSeq

let private fixLists (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.Lists)

let private failedRules (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq

/// Steps 1-2, then a-b indented under step 2, then steps 3-4: each tagged as a list of its own.
let private example =
    [ List [ 76.0, 0, "1. Find the accrual rate."; 76.0, 1, "2. Apply the formula:" ]
      List [ 108.0, 2, "a. Take all of the first $11."; 108.0, 3, "b. Take 75 percent of the next $33." ]
      List [ 76.0, 4, "3. Add the two amounts together."; 76.0, 5, "4. Multiply by years of service." ] ]

[<Fact>]
let ``steps numbered on across lists, with a list indented between them, are reported and joined into one list`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path example

    let warning = splitWarnings input.Path |> List.exactlyOne
    warning.ErrorMessage
    |> should equal "Page 1: 3 lists side by side, starting \"1.\": 2 that carry on as one, and 1 indented under an item of it."
    warning.LocationContext |> should equal "StructTreeRoot/K/K[0]"
    warning.FixOptions |> should equal PdfFixOptions.Lists

    let result = fixLists input.Path output.Path

    result.AppliedFixes |> should contain "Joined 1 list(s) tagged in pieces into one each, indented lists inside their items."
    shape output.Path |> should equal [ "L(LI(LBody),LI(LBody(L(LI(LBody),LI(LBody)))),LI(LBody),LI(LBody))" ]
    splitWarnings output.Path |> should be Empty
    Set.difference (failedRules output.Path) (failedRules input.Path) |> should be Empty

[<Fact>]
let ``joining lists doesn't change how the page looks`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path example
    fixLists input.Path output.Path |> ignore
    use renderer = new PdfRegionRenderer()
    let page = { PageNumber = 1; Left = 0.0; Bottom = 0.0; Right = 1.0; Top = 1.0 }

    renderer.RenderPageWithHighlight(output.Path, page).PngBytes
    |> should equal (renderer.RenderPageWithHighlight(input.Path, page).PngBytes)

[<Fact>]
let ``numbered lists side by side that count on are joined`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf
        input.Path
        [ List [ 72.0, 0, "1. Find the accrual rate."; 72.0, 1, "2. Apply the formula." ]
          List [ 72.0, 2, "3. Add the two amounts together." ] ]

    (splitWarnings input.Path |> List.exactlyOne).ErrorMessage
    |> should equal "Page 1: 2 lists side by side, starting \"1.\": 2 that carry on as one."

    fixLists input.Path output.Path |> ignore

    shape output.Path |> should equal [ "L(LI(LBody),LI(LBody),LI(LBody))" ]

[<Fact>]
let ``a list indented after an item that ends with a colon goes inside that item`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf
        input.Path
        [ List [ 76.0, 0, "1. Find the accrual rate."; 76.0, 1, "2. Apply the formula:" ]
          List [ 108.0, 2, "a. Take all of the first $11."; 108.0, 3, "b. Take 75 percent of the next $33." ]
          Paragraph(36.0, 4, "That's the guaranteed amount.") ]

    (splitWarnings input.Path |> List.exactlyOne).ErrorMessage
    |> should equal "Page 1: 2 lists side by side, starting \"1.\": 1 indented under an item of it."

    fixLists input.Path output.Path |> ignore

    shape output.Path |> should equal [ "L(LI(LBody),LI(LBody(L(LI(LBody),LI(LBody)))))"; "P" ]

[<Fact>]
let ``bulleted lists with a list indented between them are joined`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf
        input.Path
        [ List [ 72.0, 0, "• Pension benefits."; 72.0, 1, "• Survivor benefits, including:" ]
          List [ 100.0, 2, "a. Annuities."; 100.0, 3, "b. Lump sums." ]
          List [ 72.0, 4, "• Disability benefits." ] ]

    splitWarnings input.Path |> should haveLength 1

    fixLists input.Path output.Path |> ignore

    shape output.Path |> should equal [ "L(LI(LBody),LI(LBody(L(LI(LBody),LI(LBody)))),LI(LBody))" ]

let private leftAlone (entries: Entry list) =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path entries

    splitWarnings input.Path |> should be Empty
    (fixLists input.Path output.Path).WasFixed |> should be False
    File.Exists output.Path |> should be False

[<Fact>]
let ``bulleted lists side by side are left as they are: they may be two lists`` () =
    leftAlone [ List [ 72.0, 0, "• Pension benefits."; 72.0, 1, "• Survivor benefits." ]; List [ 72.0, 2, "• Disability benefits." ] ]

[<Fact>]
let ``numbered lists side by side that start again are left as they are`` () =
    leftAlone
        [ List [ 72.0, 0, "1. Find the accrual rate."; 72.0, 1, "2. Apply the formula." ]
          List [ 72.0, 2, "1. Add the two amounts together."; 72.0, 3, "2. Multiply them." ] ]

[<Fact>]
let ``a list indented after an item that doesn't end with a colon, with nothing after it, is left beside it`` () =
    leftAlone
        [ List [ 76.0, 0, "1. Find the accrual rate."; 76.0, 1, "2. Apply the formula." ]
          List [ 108.0, 2, "a. Take all of the first $11."; 108.0, 3, "b. Take 75 percent of the next $33." ] ]

[<Fact>]
let ``lists with a paragraph between them are left as they are`` () =
    leftAlone
        [ List [ 72.0, 0, "1. Find the accrual rate."; 72.0, 1, "2. Apply the formula." ]
          Paragraph(36.0, 2, "Then:")
          List [ 72.0, 3, "3. Add the two amounts together." ] ]

let private warnings testNumber (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun a -> a.Specification = "Legibyl" && a.TestNumber = testNumber)
    |> List.ofSeq

[<Fact>]
let ``a list isn't put in an item that says what it reads as: it wouldn't be read`` () =
    use input = new TempPdf()
    // Only the item ending with a colon (the last LBody made) says what it reads as.
    let mutable bodies = 0
    createPdfWith
        input.Path
        [ List [ 76.0, 0, "1. Find the accrual rate."; 76.0, 1, "2. Apply the formula:" ]
          List [ 108.0, 2, "a. Take all of the first $11."; 108.0, 3, "b. Take 75 percent of the next $33." ] ]
        (fun tag element ->
            if tag = "LBody" then
                bodies <- bodies + 1
                if bodies = 2 then element.Put(PdfName.ActualText, PdfString "2. Apply the formula below:") |> ignore)
        ignore

    splitWarnings input.Path |> should be Empty

[<Fact>]
let ``a list at the top of the next column isn't indented under an item ending with a colon`` () =
    use input = new TempPdf()
    createPdf
        input.Path
        [ List [ 76.0, 3, "1. Find the accrual rate."; 76.0, 4, "2. Apply the formula:" ]
          List [ 320.0, 0, "a. Take all of the first $11."; 320.0, 1, "b. Take 75 percent." ] ]

    splitWarnings input.Path |> should be Empty

[<Fact>]
let ``lists indented two levels go each under the item above them`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf
        input.Path
        [ List [ 76.0, 0, "1. Find the accrual rate."; 76.0, 1, "2. Apply the formula:" ]
          List [ 108.0, 2, "a. Take the first amount."; 108.0, 3, "b. Then the rest:" ]
          List [ 140.0, 4, "i. At 75 percent."; 140.0, 5, "ii. Up to the limit." ]
          List [ 76.0, 6, "3. Add the two amounts together."; 76.0, 7, "4. Multiply by years of service." ] ]

    fixLists input.Path output.Path |> ignore

    shape output.Path
    |> should equal [ "L(LI(LBody),LI(LBody(L(LI(LBody),LI(LBody(L(LI(LBody),LI(LBody))))))),LI(LBody),LI(LBody))" ]
    splitWarnings output.Path |> should be Empty

[<Fact>]
let ``items joined into a list in another language keep their own`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    let mutable lists = 0
    createPdfWith
        input.Path
        [ List [ 76.0, 0, "1. Find the accrual rate."; 76.0, 1, "2. Apply the formula." ]
          List [ 76.0, 2, "3. Add the two amounts together."; 76.0, 3, "4. Multiply by years of service." ] ]
        (fun tag element ->
            if tag = "L" then
                lists <- lists + 1
                if lists = 1 then element.Put(PdfName.Lang, PdfString "es") |> ignore)
        (fun catalog -> catalog.Put(PdfName.Lang, PdfString "en-US") |> ignore)

    fixLists input.Path output.Path |> ignore

    let list = (VeraPdfValidator().ReadStructureTree output.Path |> notNull).Children[0].Children[0]
    [ for item in list.Children -> item.Language ] |> should equal [ null; null; "en-US"; "en-US" ]

[<Fact>]
let ``an item tagged as a paragraph between two lists of one is joined in one go`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf
        input.Path
        [ List [ 76.0, 0, "1. Find the accrual rate."; 76.0, 1, "2. Apply the formula." ]
          Paragraph(76.0, 2, "3. Add the two amounts together.")
          List [ 76.0, 3, "4. Multiply by years of service."; 76.0, 4, "5. Divide by twelve." ] ]

    fixLists input.Path output.Path |> ignore

    shape output.Path |> should equal [ "L(LI(LBody),LI(LBody),LI(LBody(P)),LI(LBody),LI(LBody))" ]
    splitWarnings output.Path |> should be Empty
    warnings 21 output.Path |> should be Empty

[<Fact>]
let ``a list further below than its items are apart isn't more of it`` () =
    use input = new TempPdf()
    createPdf
        input.Path
        [ List [ 76.0, 0, "1. Find the accrual rate."; 76.0, 1, "2. Apply the formula." ]
          List [ 76.0, 6, "3. A note at the foot of the page."; 76.0, 7, "4. And another." ] ]

    splitWarnings input.Path |> should be Empty
