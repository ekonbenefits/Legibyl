module Legibyl.Validation.Tests.BookmarksTests

open System
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Navigation
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// Each (page, tag, text) is drawn on its page and tagged Document > tag. `withOutline` gives
/// the document an existing bookmark; it declares its language unless `withoutLanguage`.
let private createPdfWith (path: string) (withOutline: bool) (withoutLanguage: bool) (elements: (int * string * string) list) =
    use document = new PdfDocument(new PdfWriter(path))
    let pageCount = elements |> List.map (fun (page, _, _) -> page) |> List.fold max 1
    let pages = [ for _ in 1 .. pageCount -> document.AddNewPage() ]
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA)
    let nextMcid = Array.zeroCreate<int> (pageCount + 1)
    let kids = PdfArray()
    for i, (pageNumber, tag, text) in List.indexed elements do
        let page = pages[pageNumber - 1]
        let mcid = nextMcid[pageNumber]
        nextMcid[pageNumber] <- mcid + 1
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
        PdfCanvas(page)
            .BeginMarkedContent(PdfName tag, properties)
            .BeginText()
            .SetFontAndSize(font, 12f)
            .MoveText(72.0, 700.0 - float i * 24.0)
            .ShowText(text)
            .EndText()
            .EndMarkedContent()
        |> ignore
        let element = PdfDictionary()
        element.Put(PdfName.Type, PdfName.StructElem) |> ignore
        element.Put(PdfName.S, PdfName tag) |> ignore
        element.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
        element.Put(PdfName.K, PdfNumber mcid) |> ignore
        kids.Add element
    let documentElement = PdfDictionary()
    documentElement.Put(PdfName.Type, PdfName.StructElem) |> ignore
    documentElement.Put(PdfName.S, PdfName.Document) |> ignore
    documentElement.Put(PdfName.K, kids) |> ignore
    let root = PdfDictionary()
    root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    root.Put(PdfName.K, documentElement) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, root) |> ignore
    if not withoutLanguage then
        document.GetCatalog().SetLang(PdfString "en-US")
    if withOutline then
        document.GetOutlines(false).AddOutline("Existing").AddDestination(PdfExplicitDestination.CreateFit(pages[0]))

let private createPdf path withOutline elements = createPdfWith path withOutline false elements

/// The outline as "Title (page N)", children indented under their parent.
let private outlineOf (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    let pageOf (outline: PdfOutline) =
        match outline.GetDestination() with
        | null -> 0
        | destination ->
            match destination.GetPdfObject() with
            | :? PdfArray as array -> document.GetPageNumber(array.GetAsDictionary 0)
            | _ -> 0
    let rec lines indent (outline: PdfOutline) =
        [ for child in outline.GetAllChildren() do
              yield $"{indent}{child.GetTitle()} (page {pageOf child})"
              yield! lines (indent + "  ") child ]
    lines "" (document.GetOutlines(true))

let private bookmarkWarnings (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun a -> a.Specification = "Legibyl" && a.TestNumber = 7)
    |> List.ofSeq

let private addBookmarks (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.Bookmarks)

let private failedRules (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq

[<Fact>]
let ``H1 and H2 headings get bookmarks, H2s under the H1 before them`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf
        input.Path
        false
        [ 1, "H1", "Introduction"
          1, "P", "Some text."
          1, "H2", "Scope"
          2, "H3", "Details"
          2, "H2", "Terms"
          3, "H1", "Summary" ]

    let warning = bookmarkWarnings input.Path |> List.exactlyOne
    warning.ErrorMessage |> should equal "5 heading(s), 4 of them H1 or H2 with text to bookmark."
    warning.FixOptions |> should equal PdfFixOptions.Bookmarks

    let result = addBookmarks input.Path output.Path

    result.AppliedFixes |> should contain "Added 4 bookmark(s) for the H1 and H2 headings."
    outlineOf output.Path
    |> should equal [ "Introduction (page 1)"; "  Scope (page 1)"; "  Terms (page 2)"; "Summary (page 3)" ]
    bookmarkWarnings output.Path |> should be Empty
    Set.difference (failedRules output.Path) (failedRules input.Path) |> should be Empty

[<Fact>]
let ``an H2 before any H1 is a top-level bookmark`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path false [ 1, "H2", "Preface"; 1, "H1", "Chapter one"; 2, "H2", "Section" ]

    addBookmarks input.Path output.Path |> ignore

    outlineOf output.Path |> should equal [ "Preface (page 1)"; "Chapter one (page 1)"; "  Section (page 2)" ]

[<Fact>]
let ``a document that already has bookmarks is left alone`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path true [ 1, "H1", "Introduction"; 1, "H2", "Scope" ]

    bookmarkWarnings input.Path |> should be Empty
    (addBookmarks input.Path output.Path).WasFixed |> should be False

[<Fact>]
let ``a document without headings gets no warning`` () =
    use input = new TempPdf()
    createPdf input.Path false [ 1, "P", "Just a paragraph." ]

    bookmarkWarnings input.Path |> should be Empty

[<Fact>]
let ``headings below H2 are reported but not bookmarked`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path false [ 1, "H3", "A minor heading"; 1, "P", "Text." ]

    let warning = bookmarkWarnings input.Path |> List.exactlyOne
    warning.FixOptions |> should equal PdfFixOptions.None
    (addBookmarks input.Path output.Path).WasFixed |> should be False

[<Fact>]
let ``bookmarks aren't added to a document that doesn't declare its language`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // Bookmark text needs a known language (PDF/UA 7.2-2); adding them would add failures.
    createPdfWith input.Path false true [ 1, "H1", "Introduction"; 1, "H2", "Scope" ]

    let warning = bookmarkWarnings input.Path |> List.exactlyOne
    warning.FixOptions |> should equal PdfFixOptions.None
    warning.ErrorMessage |> should haveSubstring "Declare the document's language (/Lang) first"
    (addBookmarks input.Path output.Path).WasFixed |> should be False
