/// link-contents (LinkContents): a table-of-contents link without a description is described by
/// its own text; nothing else is.
module Legibyl.Validation.Tests.LinkContentsTests

open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
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

/// A tagged page: a table of contents with an entry per text in toc, and a paragraph with a link
/// per text in body; each link's annotation has the description given (None for none).
let private create (path: string) (toc: (string * string option) list) (body: (string * string option) list) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    let page = document.AddNewPage()
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let annotations = PdfArray()
    let mutable mcid = 0
    let link (y: float32) (text: string, contents: string option) =
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
        PdfCanvas(page).BeginMarkedContent(PdfName.Link, properties).BeginText().SetFontAndSize(font, 12f)
            .MoveText(72.0, float y).ShowText(text).EndText().EndMarkedContent()
        |> ignore
        let annotation = PdfDictionary()
        annotation.Put(PdfName.Type, PdfName.Annot) |> ignore
        annotation.Put(PdfName.Subtype, PdfName.Link) |> ignore
        annotation.Put(PdfName.Rect, PdfArray([| 70f; y - 2f; 400f; y + 12f |])) |> ignore
        annotation.Put(PdfName.StructParent, PdfNumber(1000 + mcid)) |> ignore
        contents |> Option.iter (fun c -> annotation.Put(PdfName.Contents, PdfString c) |> ignore)
        annotation.MakeIndirect document |> ignore
        annotations.Add annotation
        let objectReference = PdfDictionary()
        objectReference.Put(PdfName.Type, PdfName.OBJR) |> ignore
        objectReference.Put(PdfName.Obj, annotation) |> ignore
        let tagged = element "Link" page [ PdfNumber mcid; objectReference ]
        mcid <- mcid + 1
        tagged
    let tocEntries = toc |> List.mapi (fun i entry -> element "TOCI" page [ link (700f - 20f * float32 i) entry ] :> PdfObject)
    let bodyLinks = body |> List.mapi (fun i entry -> link (400f - 20f * float32 i) entry :> PdfObject)
    page.GetPdfObject().Put(PdfName.Annots, annotations) |> ignore
    let kids =
        [ if not toc.IsEmpty then element "TOC" page tocEntries :> PdfObject
          if not body.IsEmpty then element "P" page bodyLinks :> PdfObject ]
    let root = document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot |> notNull
    root.Put(PdfName.K, element "Document" page kids) |> ignore

/// Each link annotation's /Contents on the first page, in order.
let private descriptions (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    let annotations = document.GetPage(1).GetPdfObject().GetAsArray PdfName.Annots |> notNull
    [ for i in 0 .. annotations.Size() - 1 -> (annotations.GetAsDictionary i |> notNull).GetAsString PdfName.Contents |> Option.ofObj |> Option.map _.ToUnicodeString() ]

let private fails (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq

let private fix (input: string) (output: string) (options: PdfFixOptions) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, options)

[<Fact>]
let ``a contents link is described, and its missing-description failure goes; a body link is left failing`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    create input.Path [ "Eligibility ..... 4", None ] [ "click here", None ]

    let result = fix input.Path output.Path PdfFixOptions.LinkContents

    result.AppliedFixes |> should contain "Described 1 link annotation(s) from their own text."
    descriptions output.Path |> should equal [ Some "Eligibility"; None ]
    fails input.Path |> should contain "7.18.5-2"
    // Still failing, for the body link a person has to describe.
    fails output.Path |> should contain "7.18.5-2"
    VeraPdfValidator().Validate(output.Path, PdfProfile.PdfUa1).FailedAssertions
    |> Seq.filter (fun a -> a.RuleId = "7.18.5-2")
    |> Seq.length
    |> should equal 1

[<Fact>]
let ``with nothing to describe, no copy is written, and it says why`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    create input.Path [ "Eligibility ..... 4", Some "Eligibility" ] [ "click here", None ]

    let result = fix input.Path output.Path PdfFixOptions.LinkContents

    result.WasFixed |> should be False
    System.IO.File.Exists output.Path |> should be False
    List.ofSeq result.NotApplied |> should equal [ "Describe table-of-contents links with their own text: nothing in this PDF needed it." ]

[<Fact>]
let ``an untagged PDF, or a contents with no links, has nothing to describe`` () =
    use untagged = new TempPdf()
    use noLinks = new TempPdf()
    use output = new TempPdf ".fixed"
    do
        use document = new PdfDocument(new PdfWriter(untagged.Path))
        document.AddNewPage() |> ignore
    create noLinks.Path [] []

    (fix untagged.Path output.Path PdfFixOptions.LinkContents).WasFixed |> should be False
    (fix noLinks.Path output.Path PdfFixOptions.LinkContents).WasFixed |> should be False

[<Fact>]
let ``with the safe fixes, contents links are described and the others' changes are there too`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    create input.Path [ "Eligibility ..... 4", None; "Benefits ..... 5", None ] []

    let result = fix input.Path output.Path FixNames.safe

    descriptions output.Path |> should equal [ Some "Eligibility"; Some "Benefits" ]
    // The tab order (another safe fix) is set on the page with the links.
    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetPage(1).GetPdfObject().GetAsName PdfName.Tabs |> should equal PdfName.S
    result.AppliedFixes |> Seq.exists (fun line -> line.StartsWith "Described 2 link") |> should be True
    fails output.Path |> should not' (contain "7.18.5-2")
