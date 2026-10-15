module Legibyl.Validation.Tests.ParagraphListsTests

open System
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private element (tag: string) (page: PdfPage) (kids: PdfObject list) =
    let element = PdfDictionary()
    element.Put(PdfName.Type, PdfName.StructElem) |> ignore
    element.Put(PdfName.S, PdfName tag) |> ignore
    element.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
    let array = PdfArray()
    kids |> List.iter array.Add
    element.Put(PdfName.K, array) |> ignore
    element

/// Where createPdf puts the paragraphs.
type private Container =
    /// Straight under Document.
    | Body
    /// Each in its own item of an existing list: L > LI > LBody > P.
    | EachInListItem
    /// All in one existing list item's body, as a flattened nested list: L > LI > LBody > P, P.
    | OneListItem
    /// All inside a table of contents.
    | TableOfContents

/// One page of paragraphs, each drawn at its indent and tagged P, placed as `container` says.
/// An empty text makes an empty paragraph (a P with no content), as a blank line exports to.
let private createPdf (path: string) (container: Container) (paragraphs: (float32 * string) list) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    let canvas = PdfCanvas(page)
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA)
    let tagged =
        paragraphs
        |> List.mapi (fun mcid (indent, text) ->
          if text = "" then
            element "P" page [] :> PdfObject
          else
            let properties = PdfDictionary()
            properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
            canvas
                .BeginMarkedContent(PdfName.P, properties)
                .BeginText()
                .SetFontAndSize(font, 12f)
                .MoveText(float indent, 700.0 - float mcid * 20.0)
                .ShowText(text)
                .EndText()
                .EndMarkedContent()
            |> ignore
            element "P" page [ PdfNumber mcid ] :> PdfObject)
    let body: PdfObject list =
        match container with
        | Body -> tagged
        | EachInListItem -> [ element "L" page [ for p in tagged -> element "LI" page [ element "LBody" page [ p ] ] ] ]
        | OneListItem -> [ element "L" page [ element "LI" page [ element "LBody" page tagged ] ] ]
        | TableOfContents -> [ element "TOC" page tagged ]
    let root = PdfDictionary()
    root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    root.Put(PdfName.K, element "Document" page body) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, root) |> ignore

let private at72 texts = texts |> List.map (fun text -> 72f, text)

/// One page of paragraphs, each tagged P straight under Document and drawn where it says: its
/// left edge, and its line (0 at the top, 20 points apart), so a bullet and its text can share one.
let private createPdfPlaced (path: string) (paragraphs: (float32 * int * string) list) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    let canvas = PdfCanvas(page)
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA)
    let tagged =
        paragraphs
        |> List.mapi (fun mcid (left, line, text) ->
            let properties = PdfDictionary()
            properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
            canvas
                .BeginMarkedContent(PdfName.P, properties)
                .BeginText()
                .SetFontAndSize(font, 12f)
                .MoveText(float left, 700.0 - float line * 20.0)
                .ShowText(text)
                .EndText()
                .EndMarkedContent()
            |> ignore
            element "P" page [ PdfNumber mcid ] :> PdfObject)
    let root = PdfDictionary()
    root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    root.Put(PdfName.K, element "Document" page tagged) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, root) |> ignore

let private listWarnings (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun a -> a.Specification = "Legibyl" && a.TestNumber = 6)
    |> List.ofSeq

let private warningsFor container paragraphs =
    use input = new TempPdf()
    createPdf input.Path container paragraphs
    listWarnings input.Path

/// The document's structure below Document, as nested tags: "L(LI(LBody(P)),...)".
let private shape (path: string) =
    let rec describe (node: StructureNode) =
        if node.Children.Count = 0 then node.Tag
        else $"""{node.Tag}({node.Children |> Seq.map describe |> String.concat ","})"""
    let root = VeraPdfValidator().ReadStructureTree path |> notNull
    (Seq.exactlyOne root.Children).Children |> Seq.map describe |> List.ofSeq

let private listNumbering (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    let rec find (obj: PdfObject | null) : PdfName list =
        match obj with
        | :? PdfDictionary as dictionary ->
            let own =
                if PdfName.L.Equals(dictionary.GetAsName(PdfName.S)) then
                    [ dictionary.GetAsDictionary(PdfName.A) |> notNull |> _.GetAsName(PdfName.ListNumbering) |> notNull ]
                else []
            own @ find (dictionary.Get(PdfName.K))
        | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do yield! find (array.Get i) ]
        | _ -> []
    find (document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot))

let private fixLists (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.Lists)

let private failedRules (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq

let private numbered = at72 [ "(1) the first condition applies; or"; "(2) the second condition applies; or"; "(3) the third condition applies." ]

[<Fact>]
let ``numbered paragraphs are reported and tagged as a decimal list`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path Body numbered

    let warning = listWarnings input.Path |> List.exactlyOne
    warning.ErrorMessage |> should equal "3 items on page 1, marked \"(1)\" to \"(3)\"."
    warning.FixOptions |> should equal PdfFixOptions.Lists

    let result = fixLists input.Path output.Path

    result.AppliedFixes |> should contain "Tagged 1 run(s) of marked paragraphs as lists."
    shape output.Path |> should equal [ "L(LI(LBody(P)),LI(LBody(P)),LI(LBody(P)))" ]
    listNumbering output.Path |> should equal [ PdfName "Decimal" ]
    listWarnings output.Path |> should be Empty
    Set.difference (failedRules output.Path) (failedRules input.Path) |> should be Empty

[<Fact>]
let ``tagging a list doesn't change how the page looks`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path Body numbered
    fixLists input.Path output.Path |> ignore
    use renderer = new PdfRegionRenderer()
    let page = { PageNumber = 1; Left = 0.0; Bottom = 0.0; Right = 1.0; Top = 1.0 }

    let before = renderer.RenderPageWithHighlight(input.Path, page).PngBytes
    let after = renderer.RenderPageWithHighlight(output.Path, page).PngBytes

    after |> should equal before

[<Fact>]
let ``bulleted paragraphs become a disc list`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path Body (at72 [ "• First item."; "• Second item." ])

    fixLists input.Path output.Path |> ignore

    listNumbering output.Path |> should equal [ PdfName "Disc" ]

[<Theory>]
[<InlineData("a. apples|b. pears|c. plums", "LowerAlpha")>]
[<InlineData("(i) first|(ii) second|(iii) third", "LowerRoman")>]
[<InlineData("A) North|B) South", "UpperAlpha")>]
[<InlineData("- Apples|- Pears", "None")>]
let ``other marker styles get the matching numbering`` (texts: string, numbering: string) =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path Body (at72 (List.ofArray (texts.Split '|')))

    fixLists input.Path output.Path |> ignore

    listNumbering output.Path |> should equal [ PdfName numbering ]

[<Theory>]
[<InlineData("(1) first|(5) second|(2) third")>] // not counting up
[<InlineData("(1) a condition on its own")>] // one paragraph isn't a list
[<InlineData("2024 was a strong year.|2025 was better still.")>] // numbers without a marker's punctuation
[<InlineData("-5% change in the first year|-3% change in the second")>] // a minus sign, not a dash bullet
[<InlineData("(1) the first condition|and a paragraph continuing it|(2) the second condition")>] // interrupted
[<InlineData("• a bullet|(2) then a number")>] // mixed markers
[<InlineData("•|•")>] // bullets with no text
let ``paragraphs that aren't clearly a list are left alone`` (texts: string) =
    warningsFor Body (at72 (List.ofArray (texts.Split '|'))) |> should be Empty

[<Fact>]
let ``paragraphs at different indents aren't one list`` () =
    warningsFor Body [ 72f, "(1) the first"; 120f, "(2) the second" ] |> should be Empty

[<Fact>]
let ``paragraphs already in a list are left alone`` () =
    warningsFor EachInListItem numbered |> should be Empty

[<Fact>]
let ``numbered entries in a table of contents are left alone`` () =
    warningsFor TableOfContents (at72 [ "1. Scope"; "2. Terms"; "3. Eligibility" ]) |> should be Empty

[<Fact>]
let ``a flattened nested list inside a list item is tagged as a nested list`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path OneListItem (at72 [ "(a) the first detail"; "(b) the second detail" ])

    fixLists input.Path output.Path |> ignore

    shape output.Path |> should equal [ "L(LI(LBody(L(LI(LBody(P)),LI(LBody(P))))))" ]
    Set.difference (failedRules output.Path) (failedRules input.Path) |> should be Empty

[<Fact>]
let ``two lists under one parent are both tagged, around the text between them`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path Body (at72 [ "Intro."; "(1) one"; "(2) two"; "Between the lists."; "• first"; "• second"; "Outro." ])

    let result = fixLists input.Path output.Path

    result.AppliedFixes |> should contain "Tagged 2 run(s) of marked paragraphs as lists."
    shape output.Path
    |> should equal [ "P"; "L(LI(LBody(P)),LI(LBody(P)))"; "P"; "L(LI(LBody(P)),LI(LBody(P)))"; "P" ]
    listNumbering output.Path |> should equal [ PdfName "Decimal"; PdfName "Disc" ]

[<Fact>]
let ``blank paragraphs between items don't split the list`` () =
    use input = new TempPdf()
    use listsOnly = new TempPdf ".lists"
    use withEmptyRemoval = new TempPdf ".both"
    createPdf input.Path Body (at72 [ "(1) the first"; ""; "(2) the second"; ""; "(3) the third" ])

    (listWarnings input.Path |> List.exactlyOne).ErrorMessage
    |> should equal "3 items on page 1, marked \"(1)\" to \"(3)\"."

    // On its own, each blank paragraph stays in the item before it; nothing is lost or moved on the page.
    fixLists input.Path listsOnly.Path |> ignore
    shape listsOnly.Path |> should equal [ "L(LI(LBody(P,P)),LI(LBody(P,P)),LI(LBody(P)))" ]

    // With empty paragraph removal too, the blanks go first and the list comes out clean.
    let result =
        VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, withEmptyRemoval.Path, PdfFixOptions.Lists ||| PdfFixOptions.EmptyParagraphs)
    List.ofSeq result.AppliedFixes |> should equal [ "Removed 2 empty /P structure element(s)."; "Tagged 1 run(s) of marked paragraphs as lists." ]
    shape withEmptyRemoval.Path |> should equal [ "L(LI(LBody(P)),LI(LBody(P)),LI(LBody(P)))" ]

/// A bulleted list whose bullets are tagged as paragraphs of their own, each item's text in
/// the paragraphs beside it (the first item's on two lines), between two ordinary paragraphs.
let private bulletsApart =
    [ 72f, 0, "The terms are as follows."
      72f, 1, "•"
      84f, 1, "The first item, which runs"
      84f, 2, "on to a second line."
      72f, 3, "•"
      84f, 3, "The second item."
      72f, 4, "That's all of them." ]

[<Fact>]
let ``bullets tagged apart from their text are a list, not a table`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfPlaced input.Path bulletsApart
    let structureWarnings path =
        VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
        |> Seq.filter (fun a -> a.Specification = "Legibyl" && (a.TestNumber = 6 || a.TestNumber = 15))
        |> Seq.map (fun a -> a.TestNumber, a.ErrorMessage)
        |> List.ofSeq

    structureWarnings input.Path |> should equal [ 6, "2 items on page 1, marked \"•\" to \"•\"." ]

    fixLists input.Path output.Path |> ignore

    // Each bullet is its item's label, and the item's text its body.
    shape output.Path |> should equal [ "P"; "L(LI(Lbl,LBody(P,P)),LI(Lbl,LBody(P)))"; "P" ]
    listNumbering output.Path |> should equal [ PdfName "Disc" ]
    structureWarnings output.Path |> should be Empty
    Set.difference (failedRules output.Path) (failedRules input.Path) |> should be Empty
    use renderer = new PdfRegionRenderer()
    let page = { PageNumber = 1; Left = 0.0; Bottom = 0.0; Right = 1.0; Top = 1.0 }
    renderer.RenderPageWithHighlight(output.Path, page).PngBytes
    |> should equal (renderer.RenderPageWithHighlight(input.Path, page).PngBytes)

[<Fact>]
let ``a bullet on its own isn't an item when what follows isn't beside it`` () =
    use input = new TempPdf()
    // The text under each bullet, at the margin, rather than beside it: maybe not its item's.
    createPdfPlaced input.Path [ 72f, 0, "•"; 72f, 1, "Under the first bullet."; 72f, 2, "•"; 72f, 3, "Under the second." ]

    listWarnings input.Path |> should be Empty

[<Fact>]
let ``a last bullet's item doesn't take the next column, beside it but above`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfPlaced
        input.Path
        [ 72f, 0, "•"; 84f, 0, "The first item."; 72f, 1, "•"; 84f, 1, "The second item."
          // The next column, from the top of the page.
          320f, 0, "A column beside the list."; 320f, 1, "Its second paragraph." ]

    fixLists input.Path output.Path |> ignore

    shape output.Path |> should equal [ "L(LI(Lbl,LBody(P)),LI(Lbl,LBody(P)))"; "P"; "P" ]

[<Fact>]
let ``a table whose first column is a mark isn't a list: a row's cells are beside each other, not below`` () =
    use input = new TempPdf()
    createPdfPlaced
        input.Path
        [ 72f, 0, "•"; 100f, 0, "Apples"; 200f, 0, "3"
          72f, 1, "•"; 100f, 1, "Pears"; 200f, 1, "5" ]

    listWarnings input.Path |> should be Empty

[<Fact>]
let ``a stray bullet after a list is left out of it, not taking the list with it`` () =
    use input = new TempPdf()
    createPdfPlaced input.Path [ 72f, 0, "•"; 84f, 0, "The first item."; 72f, 1, "•"; 84f, 1, "The second item."; 72f, 2, "•" ]

    (listWarnings input.Path |> List.exactlyOne).ErrorMessage |> should equal "2 items on page 1, marked \"•\" to \"•\"."

