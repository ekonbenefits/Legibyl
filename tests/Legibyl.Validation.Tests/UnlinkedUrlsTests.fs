module Legibyl.Validation.Tests.UnlinkedUrlsTests

open System.IO
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Action
open iText.Kernel.Pdf.Annot
open iText.Layout.Element
open iText.Pdfua
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A paragraph: its pieces of text, each a Span of its own (its own marked content).
type private Piece =
    | Plain of string
    /// Linked, to this address.
    | Linked of string * string

/// A PDF/UA-1 page (iText's PDF/UA support makes it so) of paragraphs, one a line.
let private createPdf (path: string) (paragraphs: Piece list list) =
    use document = new PdfUADocument(new PdfWriter(path), PdfUAConfig(PdfUAConformance.PDF_UA_1, "Garden Links", "en-US"))
    let font =
        iText.Kernel.Font.PdfFontFactory.CreateFont(
            corpusFile (Path.Combine("fonts", "Lato-Regular.ttf")),
            iText.IO.Font.PdfEncodings.WINANSI,
            iText.Kernel.Font.PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED
        )
    use layout = new iText.Layout.Document(document)
    layout.SetFont font |> ignore
    for pieces in paragraphs do
        let paragraph = Paragraph()
        for piece in pieces do
            match piece with
            | Plain text -> paragraph.Add(Text text) |> ignore
            | Linked(text, address) ->
                let link = Link(text, PdfAction.CreateURI address)
                link.GetLinkAnnotation().SetContents text |> ignore
                link.GetAccessibilityProperties().SetAlternateDescription text |> ignore
                paragraph.Add link |> ignore
        layout.Add paragraph |> ignore

let private validator = lazy (VeraPdfValidator())

/// Structure-18's warnings: each one's detail, and whether the fix is offered.
let private unlinked (path: string) =
    validator.Value.Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun finding -> finding.Specification = "Legibyl" && finding.TestNumber = 18)
    |> Seq.map (fun finding -> string finding.ErrorMessage, finding.FixOptions = PdfFixOptions.LinkUrls)
    |> List.ofSeq

let private render (path: string) =
    use renderer = new Legibyl.Rendering.PdfRegionRenderer()
    let offPage = { PageNumber = 1; Left = -20.0; Bottom = -20.0; Right = -10.0; Top = -10.0 }
    use bitmap = SkiaSharp.SKBitmap.Decode(renderer.RenderPageWithHighlight(path, offPage).PngBytes)
    bitmap.Pixels

[<Fact>]
let ``a web address shown as text is warned about, and the fix offered only when it's all of its tagged text`` () =
    use pdf = new TempPdf()
    createPdf
        pdf.Path
        [ [ Plain "Our plan: "; Plain "https://example.org/plan" ]
          [ Plain "Visit https://example.org/garden today." ]
          [ Plain "Or "; Plain "www.example.org" ] ]

    unlinked pdf.Path
    |> should
        equal
        [ "Page 1: \"https://example.org/plan\".", true
          "Page 1: \"https://example.org/garden\". Not fixable here: it's part of a longer piece of text, which would have to be split to tag the address alone.", false
          "Page 1: \"www.example.org\".", true ]

[<Fact>]
let ``trailing punctuation is left off, a balanced bracket kept, and look-alikes aren't addresses`` () =
    use pdf = new TempPdf()
    createPdf
        pdf.Path
        [ [ Plain "See https://example.org/wiki/Garden_(plants)." ]
          [ Plain "Read (https://example.org/notes), then version 1.2/beta and 3.14/2." ] ]

    unlinked pdf.Path |> List.map fst
    |> should
        equal
        [ "Page 1: \"https://example.org/wiki/Garden_(plants)\". Not fixable here: it's part of a longer piece of text, which would have to be split to tag the address alone."
          "Page 1: \"https://example.org/notes\". Not fixable here: it's part of a longer piece of text, which would have to be split to tag the address alone." ]

[<Fact>]
let ``an address that's already a link isn't warned about`` () =
    use pdf = new TempPdf()
    createPdf pdf.Path [ [ Plain "Our plan: "; Linked("https://example.org/plan", "https://example.org/plan") ] ]

    unlinked pdf.Path |> should be Empty

[<Fact>]
let ``an address split across spans is found whole, and left for the fix`` () =
    use pdf = new TempPdf()
    createPdf pdf.Path [ [ Plain "Plan: "; Plain "https://exam"; Plain "ple.org/plan" ] ]

    unlinked pdf.Path
    |> should
        equal
        [ "Page 1: \"https://example.org/plan\". Not fixable here: it's drawn in more than one piece, which would have to be joined to tag it as a link.", false ]

[<Fact>]
let ``the fix links each address that's all of its tagged text, the page looks the same, and the rest are reported`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf
        input.Path
        [ [ Plain "Our plan: "; Plain "https://example.org/plan" ]
          [ Plain "Visit https://example.org/garden today." ]
          [ Plain "Or "; Plain "www.example.org" ] ]

    let result = validator.Value.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkUrls)

    result.WasFixed |> should be True
    result.AppliedFixes |> should contain "Linked 2 web addresses shown as text, each to where it goes."
    result.NotApplied
    |> should
        contain
        "Page 1: \"https://example.org/garden\" isn't linked: it's part of a longer piece of text, which would have to be split to tag the address alone."
    // Still PDF/UA, with only the address left that the fix can't tag.
    validator.Value.Validate(output.Path, PdfProfile.PdfUa1).IsCompliant |> should be True
    unlinked output.Path |> List.map snd |> should equal [ false ]
    render output.Path |> should equal (render input.Path)
    // Each a link annotation going to it, described by it, held by a Link tag with its text.
    use document = new PdfDocument(new PdfReader(output.Path))
    let links =
        [ for annotation in document.GetPage(1).GetAnnotations() do
              match annotation with
              | :? PdfLinkAnnotation as link ->
                  link.GetAction().GetAsString(PdfName.URI).ToUnicodeString(), link.GetContents().ToUnicodeString()
              | _ -> () ]
    links |> should equal [ "https://example.org/plan", "https://example.org/plan"; "https://www.example.org", "www.example.org" ]
    // Every Link in the tags, the new ones only: in their spans' places, none of iText's own.
    let rec find (node: StructureNode) : StructureNode list =
        (if node.Tag = "Link" then [ node ] else []) @ (node.Children |> Seq.collect find |> List.ofSeq)
    let tree = validator.Value.ReadStructureTree output.Path |> notNull
    tree
    |> find
    |> List.map (fun link -> string link.Content, string link.LinkUri)
    |> should equal [ "https://example.org/plan", "https://example.org/plan"; "www.example.org", "https://www.example.org" ]

[<Fact>]
let ``a file with no address shown as text isn't changed, and no copy is written`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf input.Path [ [ Plain "The community garden is open on Saturdays." ] ]

    let result = validator.Value.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkUrls)

    result.WasFixed |> should be False
    File.Exists output.Path |> should be False
