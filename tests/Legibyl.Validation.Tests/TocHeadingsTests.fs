module Legibyl.Validation.Tests.TocHeadingsTests

open System
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

type private TocEntry = { Label: string; Indent: float32; Page: int }

let private entry label indent page = { Label = label; Indent = indent; Page = page }

let private structElement (tag: string) (page: PdfPage) (kids: PdfObject list) =
    let element = PdfDictionary()
    element.Put(PdfName.Type, PdfName.StructElem) |> ignore
    element.Put(PdfName.S, PdfName tag) |> ignore
    element.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
    let array = PdfArray()
    kids |> List.iter array.Add
    element.Put(PdfName.K, array) |> ignore
    element

/// Page 1 is a table of contents: each entry is drawn at its indent as "Label ..... N" and
/// tagged TOC > TOCI > Link, with a link annotation to page N whose rectangle spans the whole
/// line (as generated ones often do). The body's paragraphs, tagged `bodyTag` (P unless a
/// test wants a heading), follow on the later pages.
let private createPdfWithToc (path: string) (namedDestinations: bool) (entries: TocEntry list) (body: (int * string * string) list) =
    use document = new PdfDocument(new PdfWriter(path))
    let pageCount = List.max ((entries |> List.map _.Page) @ (body |> List.map (fun (page, _, _) -> page)))
    let pages = [ for _ in 1 .. pageCount -> document.AddNewPage() ]
    let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA)
    let nextMcid = Array.zeroCreate<int> (pageCount + 1)
    let annotations = PdfArray()

    /// Draws text at (x, y) on a page in its own marked content; returns its MCID.
    let draw (pageNumber: int) (tag: string) (x: float32) (y: float32) (text: string) =
        let mcid = nextMcid[pageNumber]
        nextMcid[pageNumber] <- mcid + 1
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
        PdfCanvas(pages[pageNumber - 1])
            .BeginMarkedContent(PdfName tag, properties)
            .BeginText()
            .SetFontAndSize(font, 12f)
            .MoveText(float x, float y)
            .ShowText(text)
            .EndText()
            .EndMarkedContent()
        |> ignore
        mcid

    let tocPage = pages[0]
    let tocEntries =
        entries
        |> List.mapi (fun i tocEntry ->
            let y = 700f - float32 i * 20f
            let mcid = draw 1 "Link" tocEntry.Indent y $"{tocEntry.Label} ......... {tocEntry.Page}"
            let explicitDestination = PdfArray()
            explicitDestination.Add(pages[tocEntry.Page - 1].GetPdfObject())
            explicitDestination.Add(PdfName.XYZ)
            let annotation = PdfDictionary()
            annotation.Put(PdfName.Type, PdfName.Annot) |> ignore
            annotation.Put(PdfName.Subtype, PdfName.Link) |> ignore
            annotation.Put(PdfName.Rect, PdfArray([| 50f; y; 550f; y + 12f |])) |> ignore
            if namedDestinations then
                let name = $"section-{i}"
                document.AddNamedDestination(name, explicitDestination)
                annotation.Put(PdfName.Dest, PdfString name) |> ignore
            else
                annotation.Put(PdfName.Dest, explicitDestination) |> ignore
            annotation.MakeIndirect document |> ignore
            annotations.Add annotation
            let objectReference = PdfDictionary()
            objectReference.Put(PdfName.Type, PdfName.OBJR) |> ignore
            objectReference.Put(PdfName.Obj, annotation) |> ignore
            let link = structElement "Link" tocPage [ PdfNumber mcid; objectReference ]
            structElement "TOCI" tocPage [ link ] :> PdfObject)
    tocPage.GetPdfObject().Put(PdfName.Annots, annotations) |> ignore

    let bodyElements =
        body
        |> List.mapi (fun i (pageNumber, tag, text) ->
            let mcid = draw pageNumber tag 72f (700f - float32 i * 30f) text
            structElement tag pages[pageNumber - 1] [ PdfNumber mcid ] :> PdfObject)

    let root = PdfDictionary()
    root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    root.Put(PdfName.K, structElement "Document" tocPage (structElement "TOC" tocPage tocEntries :: bodyElements)) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, root) |> ignore

let private paragraphs page texts = [ for text in texts -> page, "P", text ]

let private tocWarnings (report: ValidationReport) =
    report.Assertions
    |> Seq.filter (fun a -> a.Specification = "Legibyl" && a.TestNumber >= 3 && a.TestNumber <= 5)
    |> List.ofSeq

/// The body's tags in reading order, after the table of contents.
let private bodyTags (path: string) =
    let root = VeraPdfValidator().ReadStructureTree path |> notNull
    let document = root.Children |> Seq.exactlyOne
    document.Children |> Seq.filter (fun n -> n.Tag <> "TOC") |> Seq.map _.Tag |> List.ofSeq

let private fixHeadings (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.HeadingsFromToc)

let private threeLevels =
    [ entry "Introduction" 72f 2; entry "Scope" 90f 2; entry "Details" 108f 3; entry "Summary" 72f 3 ]

let private threeLevelsBody =
    paragraphs 2 [ "Introduction"; "Some body text."; "Scope" ] @ paragraphs 3 [ "Details"; "More body text."; "Summary" ]

[<Fact>]
let ``entries in three indent tiers become H1, H2, and H3`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithToc input.Path false threeLevels threeLevelsBody

    let warnings = tocWarnings (VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1))
    warnings |> List.map _.TestNumber |> should equal [ 3; 3; 3; 3 ]
    warnings |> List.forall (fun w -> w.FixOptions = PdfFixOptions.HeadingsFromToc) |> should be True
    (warnings |> List.find (fun w -> (string w.ErrorMessage).StartsWith "\"Details\"")).ErrorMessage
    |> should haveSubstring "H3"

    let result = fixHeadings input.Path output.Path

    result.AppliedFixes |> should contain "Tagged 4 heading(s) listed in the table of contents."
    bodyTags output.Path |> should equal [ "H1"; "P"; "H2"; "H3"; "P"; "H1" ]
    let fixedReport = VeraPdfValidator().Validate(output.Path, PdfProfile.PdfUa1)
    fixedReport.FailedAssertions |> Seq.map _.RuleId |> should not' (contain "7.4.2-1")
    tocWarnings fixedReport |> should be Empty

[<Fact>]
let ``named destinations are followed too`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithToc input.Path true threeLevels threeLevelsBody

    fixHeadings input.Path output.Path |> ignore

    bodyTags output.Path |> should equal [ "H1"; "P"; "H2"; "H3"; "P"; "H1" ]

[<Fact>]
let ``entries that all share one indent all become H1`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithToc input.Path false [ entry "Introduction" 72f 2; entry "Details" 72f 3 ] threeLevelsBody

    fixHeadings input.Path output.Path |> ignore

    bodyTags output.Path |> should equal [ "H1"; "P"; "P"; "H1"; "P"; "P" ]

[<Fact>]
let ``an entry matching two paragraphs on its page is reported and left alone`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithToc input.Path false [ entry "Introduction" 72f 2; entry "Notes" 72f 3 ] (paragraphs 2 [ "Introduction" ] @ paragraphs 3 [ "Notes"; "Notes" ])

    let warnings = tocWarnings (VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1))
    let unmatched = warnings |> List.filter (fun w -> w.TestNumber = 4) |> List.exactlyOne
    unmatched.ErrorMessage |> should equal "\"Notes\" goes to page 3, which has 2 matching paragraphs."
    unmatched.FixOptions |> should equal PdfFixOptions.None

    fixHeadings input.Path output.Path |> ignore
    bodyTags output.Path |> should equal [ "H1"; "P"; "P" ]

[<Fact>]
let ``an entry matching nothing on its page is reported and left alone`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdfWithToc input.Path false [ entry "Introduction" 72f 2; entry "Glossary" 72f 3 ] (paragraphs 2 [ "Introduction" ] @ paragraphs 3 [ "Terms" ])

    let warnings = tocWarnings (VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1))
    (warnings |> List.filter (fun w -> w.TestNumber = 4) |> List.exactlyOne).ErrorMessage
    |> should equal "\"Glossary\" goes to page 3, which has no matching paragraph."

    fixHeadings input.Path output.Path |> ignore
    bodyTags output.Path |> should equal [ "H1"; "P" ]

[<Fact>]
let ``indents that vary gradually are not turned into levels`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    let gradual = [ entry "Introduction" 72f 2; entry "Scope" 74f 2; entry "Details" 76f 3; entry "Summary" 78f 3 ]
    createPdfWithToc input.Path false gradual threeLevelsBody

    let warnings = tocWarnings (VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1))
    warnings |> List.map _.TestNumber |> should equal [ 5 ]
    warnings.Head.ErrorMessage |> should haveSubstring "indents vary gradually"

    let result = fixHeadings input.Path output.Path
    result.WasFixed |> should be False

[<Fact>]
let ``headings are not recovered when unmatched entries would leave a level skipped`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // "Scope" (the only H2) matches two paragraphs, so the rest would go H1 then H3.
    let entries = [ entry "Introduction" 72f 2; entry "Scope" 90f 2; entry "Details" 108f 3 ]
    createPdfWithToc input.Path false entries (paragraphs 2 [ "Introduction"; "Scope"; "Scope" ] @ paragraphs 3 [ "Details" ])

    let warnings = tocWarnings (VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1))
    warnings |> List.map _.TestNumber |> should equal [ 5; 4 ]
    warnings.Head.ErrorMessage |> should haveSubstring "skip from H1 to H3"

    (fixHeadings input.Path output.Path).WasFixed |> should be False

[<Fact>]
let ``a document that already tags headings is left to its author`` () =
    use input = new TempPdf()
    createPdfWithToc input.Path false threeLevels ([ 2, "H1", "Introduction" ] @ paragraphs 2 [ "Scope" ] @ paragraphs 3 [ "Details"; "Summary" ])

    tocWarnings (VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1)) |> should be Empty

[<Fact>]
let ``only a paragraph on the entry's destination page counts`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // "Details" also appears as ordinary text on page 2; only page 3's is the heading.
    createPdfWithToc input.Path false [ entry "Introduction" 72f 2; entry "Details" 72f 3 ] (paragraphs 2 [ "Introduction"; "Details" ] @ paragraphs 3 [ "Details" ])

    fixHeadings input.Path output.Path |> ignore

    bodyTags output.Path |> should equal [ "H1"; "P"; "H1" ]

[<Fact>]
let ``numbered headings become headings, not a list, when both repairs run`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // "1. Scope" and "2. Terms" are consecutive numbered paragraphs, so they'd pass for a
    // list too; the TOC says they're headings, and headings run first.
    createPdfWithToc input.Path false [ entry "1. Scope" 72f 2; entry "2. Terms" 72f 2 ] (paragraphs 2 [ "1. Scope"; "2. Terms" ])

    VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.HeadingsFromToc ||| PdfFixOptions.ListsFromParagraphs)
    |> ignore

    bodyTags output.Path |> should equal [ "H1"; "H1" ]
