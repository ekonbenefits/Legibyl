module Legibyl.Validation.Tests.SplitWordSpansTests

open iText.IO.Font.Constants
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Font
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private createPdf (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage()
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont StandardFonts.COURIER
    let mutable mcid = 0

    let element (tag: string) (kids: PdfObject list) =
        let element = PdfDictionary()
        element.Put(PdfName.Type, PdfName.StructElem) |> ignore
        element.Put(PdfName.S, PdfName tag) |> ignore
        element.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
        let children = PdfArray()
        for kid in kids do
            match kid with
            | :? PdfDictionary as child -> child.Put(PdfName.P, element) |> ignore
            | _ -> ()
            children.Add kid
        element.Put(PdfName.K, children) |> ignore
        element.MakeIndirect document |> ignore
        element

    let paragraph (y: float) (segments: (string * bool) list) =
        let spans =
            [ for segment, hasActualText in segments do
                  let properties = PdfDictionary()
                  properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
                  canvas.BeginMarkedContent(PdfName.Span, properties) |> ignore
                  canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0 + float (mcid * 120), y).ShowText(segment).EndText() |> ignore
                  canvas.EndMarkedContent() |> ignore
                  let span = element "Span" [ PdfNumber mcid ]
                  if hasActualText then span.Put(PdfName.ActualText, PdfString(segment)) |> ignore
                  mcid <- mcid + 1
                  yield span ]
        element "P" (spans |> List.map (fun span -> span :> PdfObject))

    let paragraphs =
        [ paragraph 700.0 [ "The ", true; "Em", false; "ployer will contribute.", true ]
          paragraph 670.0 [ "Two ", true; "words stay separate.", true ]
          paragraph 640.0 [ "re", true; "-entry remains hyphenated.", true ]
          paragraph 610.0 [ "inter", true; "", true; "national text.", true ] ]

    let root = PdfDictionary()
    root.Put(PdfName.Type, PdfName.StructTreeRoot) |> ignore
    root.MakeIndirect document |> ignore
    let doc = element "Document" (paragraphs |> List.map (fun paragraph -> paragraph :> PdfObject))
    doc.Put(PdfName.P, root) |> ignore
    root.Put(PdfName.K, doc) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.StructTreeRoot, root) |> ignore
    let markInfo = PdfDictionary()
    markInfo.Put(PdfName.Marked, PdfBoolean.TRUE) |> ignore
    document.GetCatalog().GetPdfObject().Put(PdfName.MarkInfo, markInfo) |> ignore

let private splitWordWarnings (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun finding -> finding.Specification = "Legibyl" && finding.TestNumber = 14)
    |> List.ofSeq

let private paragraphTexts (path: string) =
    let root = VeraPdfValidator().ReadStructureTree(path) |> notNull
    let rec descendants (node: StructureNode) =
        seq {
            yield node
            for child in node.Children do
                yield! descendants child
        }
    root.Children
    |> Seq.collect descendants
    |> Seq.filter (fun node -> node.Tag = "P")
    |> Seq.map (_.ActualText >> Option.ofObj)
    |> List.ofSeq

[<Fact>]
let ``synthetic tagged PDF flags only span boundaries inside a word`` () =
    use input = new TempPdf()
    createPdf input.Path

    let warnings = splitWordWarnings input.Path

    warnings |> should haveLength 2
    warnings |> Seq.forall (fun warning -> warning.IsFixable) |> should be True
    warnings |> Seq.forall (fun warning -> warning.FixOptions = PdfFixOptions.JoinSplitWords) |> should be True

[<Fact>]
let ``fix writes the exact reconstructed text as block replacement text`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.JoinSplitWords)

    result.WasFixed |> should be True
    result.AppliedFixes |> should contain "Added replacement text to 2 text blocks whose Span tags split words."
    paragraphTexts output.Path
    |> should equal [ Some "The Employer will contribute."; None; None; Some "international text." ]
    splitWordWarnings output.Path |> should be Empty
