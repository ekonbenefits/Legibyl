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
let ``a web address shown as text is warned about, and the fix offered, all of its tagged text or not`` () =
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
          "Page 1: \"https://example.org/garden\".", true
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
        [ "Page 1: \"https://example.org/wiki/Garden_(plants)\"."; "Page 1: \"https://example.org/notes\"." ]

[<Fact>]
let ``an address that's already a link isn't warned about`` () =
    use pdf = new TempPdf()
    createPdf pdf.Path [ [ Plain "Our plan: "; Linked("https://example.org/plan", "https://example.org/plan") ] ]

    unlinked pdf.Path |> should be Empty

[<Fact>]
let ``an address split across spans is found whole, and left unfixed`` () =
    use pdf = new TempPdf()
    createPdf pdf.Path [ [ Plain "Plan: "; Plain "https://exam"; Plain "ple.org/plan" ] ]

    unlinked pdf.Path
    |> should
        equal
        [ "Page 1: \"https://example.org/plan\". Not fixable here: it's drawn in more than one piece, in different tags, which would have to be joined to tag it as a link.", false ]

[<Fact>]
let ``the fix links each address that's all of its tagged text, the page looks the same, and the rest are reported`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    createPdf
        input.Path
        [ [ Plain "Our plan: "; Plain "https://example.org/plan" ]
          [ Plain "Visit "; Plain "https://exam"; Plain "ple.org/garden" ]
          [ Plain "Or "; Plain "www.example.org" ] ]

    let result = validator.Value.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkUrls)

    result.WasFixed |> should be True
    result.AppliedFixes |> should contain "Linked 2 web addresses shown as text, each to where it goes."
    result.NotApplied
    |> should
        contain
        "Page 1: \"https://example.org/garden\" isn't linked: it's drawn in more than one piece, in different tags, which would have to be joined to tag it as a link."
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

// ---- From a cold review: text drawn in other ways, and tags of other shapes ----

/// A tagged file (iText's tagging), titled and in English, each page drawn by draw, in Helvetica.
let private tagged (path: string) (draw: PdfDocument -> iText.Kernel.Pdf.Tagutils.TagTreePointer -> iText.Kernel.Font.PdfFont -> unit) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString "en-US") |> ignore
    draw document (iText.Kernel.Pdf.Tagutils.TagTreePointer document) (iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA)

let private show (canvas: iText.Kernel.Pdf.Canvas.PdfCanvas) font (x: float) (y: float) (text: string) =
    canvas.BeginText().SetFontAndSize(font, 12f).MoveText(x, y).ShowText(text).EndText() |> ignore

/// text drawn at x, y in tags (a P around a Span, say) of its own.
let private inTags (pointer: iText.Kernel.Pdf.Tagutils.TagTreePointer) (page: PdfPage) font (tags: string list) (x: float) (y: float) (text: string) =
    let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas page
    pointer.SetPageForTagging page |> ignore
    for tag in tags do pointer.AddTag tag |> ignore
    canvas.OpenTag(pointer.GetTagReference()) |> ignore
    show canvas font x y text
    canvas.CloseTag() |> ignore
    for _ in tags do pointer.MoveToParent() |> ignore

/// The web addresses Structure-18 names, each with whether its fix is offered.
let private named (path: string) =
    unlinked path |> List.map (fun (detail, fixable) -> detail.Split('"').[1], fixable)

[<Fact>]
let ``an address is read whole however narrow its letters, as many times as it's drawn over itself`` () =
    use narrow = new TempPdf()
    tagged narrow.Path (fun document pointer font ->
        let page = document.AddNewPage()
        for i, text in List.indexed [ "https://example.org/a.html"; "https://example.org/T.Hanks"; "HTTPS://EXAMPLE.ORG/UP" ] do
            inTags pointer page font [ "P"; "Span" ] 72.0 (700.0 - 20.0 * float i) text)
    named narrow.Path |> should equal [ "https://example.org/a.html", true; "https://example.org/T.Hanks", true; "HTTPS://EXAMPLE.ORG/UP", true ]
    // Bold faked by drawing it twice, a little apart: one address, linked to itself once.
    use bold = new TempPdf()
    use fixedCopy = new TempPdf ".fixed"
    tagged bold.Path (fun document pointer font ->
        let page = document.AddNewPage()
        let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas page
        pointer.SetPageForTagging(page).AddTag("P").AddTag("Span") |> ignore
        canvas.OpenTag(pointer.GetTagReference()) |> ignore
        show canvas font 72.0 700.0 "https://example.org/plan"
        show canvas font 72.3 700.0 "https://example.org/plan"
        canvas.CloseTag() |> ignore)
    named bold.Path |> should equal [ "https://example.org/plan", true ]
    validator.Value.Fix(bold.Path, PdfProfile.PdfUa1, fixedCopy.Path, PdfFixOptions.LinkUrls) |> ignore
    use document = new PdfDocument(new PdfReader(fixedCopy.Path))
    [ for annotation in document.GetPage(1).GetAnnotations() -> (annotation :?> PdfLinkAnnotation).GetAction().GetAsString(PdfName.URI).ToUnicodeString() ]
    |> should equal [ "https://example.org/plan" ]

[<Fact>]
let ``an address goes on past a gap to a hyphen touching the rest of it, and isn't linked in part`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    tagged input.Path (fun document pointer font ->
        let page = document.AddNewPage()
        // As a justified line sets it: the hyphen a space's width on, touching the rest.
        let width (text: string) = float (font.GetWidth(text, 12f))
        let gap = width " " * 1.5
        inTags pointer page font [ "P"; "Span" ] 72.0 700.0 "https://example.org/go/dol"
        inTags pointer page font [ "P"; "Span" ] (72.0 + width "https://example.org/go/dol" + gap) 700.0 "-diversification."
        // A dash set off by gaps on both sides is prose's, not the address's.
        inTags pointer page font [ "P"; "Span" ] 72.0 680.0 "https://example.org/plan - then the beds.")
    named input.Path |> should equal [ "https://example.org/go/dol-diversification", false; "https://example.org/plan", true ]

    validator.Value.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkUrls) |> ignore
    named output.Path |> should equal [ "https://example.org/go/dol-diversification", false ]

/// One P on a page, of marked content drawn at x, y, one after another, each piece's text.
let private paragraphOfPieces (path: string) (pieces: (float * float * string) list) =
    tagged path (fun document pointer font ->
        let page = document.AddNewPage()
        let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas page
        pointer.SetPageForTagging(page).AddTag "P" |> ignore
        for x, y, text in pieces do
            canvas.OpenTag(pointer.GetTagReference()) |> ignore
            show canvas font x y text
            canvas.CloseTag() |> ignore)

[<Fact>]
let ``an address drawn in pieces side by side in one tag is linked whole, the page looking the same`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    let width (text: string) = float ((iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA).GetWidth(text, 12f))
    let after (text: string) = 72.0 + width text
    paragraphOfPieces
        input.Path
        [ 72.0, 700.0, "Visit "
          after "Visit ", 700.0, "https://example.org/"
          after "Visit https://example.org/", 700.0, "garden"
          after "Visit https://example.org/garden", 700.0, "/beds" ]
    named input.Path |> should equal [ "https://example.org/garden/beds", true ]

    let result = validator.Value.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkUrls)

    result.AppliedFixes |> should contain "Linked 1 web address shown as text, to where it goes."
    named output.Path |> should be Empty
    let failed (path: string) = validator.Value.Validate(path, PdfProfile.PdfUa1).PdfUaFailures |> Seq.map _.RuleId |> Set.ofSeq
    Set.difference (failed output.Path) (failed input.Path) |> should be Empty
    render output.Path |> should equal (render input.Path)
    // The P reads "Visit ", then a Link holding the address's three pieces and the annotation.
    let paragraph = (validator.Value.ReadStructureTree output.Path |> notNull).Children[0].Children[0]
    // (The tree's text puts a space between pieces of marked content.)
    (string paragraph.Content, [ for child in paragraph.Children -> child.Tag, (string child.Content).Replace(" ", ""), string child.LinkUri, child.ContentLeafCount ])
    |> should equal ("Visit ", [ "Link", "https://example.org/garden/beds", "https://example.org/garden/beds", 4 ])

[<Fact>]
let ``an address whose pieces aren't side by side in their tag isn't linked, and says why`` () =
    use pdf = new TempPdf()
    let width (text: string) = float ((iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA).GetWidth(text, 12f))
    // Its second piece is drawn after the next line's text, so the tag holds another piece between.
    paragraphOfPieces
        pdf.Path
        [ 72.0, 700.0, "https://example.org/"
          72.0, 680.0, "Open daily."
          72.0 + width "https://example.org/", 700.0, "garden" ]
    unlinked pdf.Path
    |> should
        equal
        [ "Page 1: \"https://example.org/garden\". Not fixable here: its pieces aren't side by side in its tag, in the order they're read.", false ]

/// The kids of the first element tagged role (the first page's): a piece of marked content by the
/// text it draws, an element by its tag.
let private kidsOf (role: string) (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    let drawn mcid =
        let text = System.Text.StringBuilder()
        let listener =
            { new iText.Kernel.Pdf.Canvas.Parser.Listener.IEventListener with
                member _.EventOccurred(data, _) =
                    match data with
                    | :? iText.Kernel.Pdf.Canvas.Parser.Data.TextRenderInfo as info when info.GetMcid() = mcid -> text.Append(info.GetText()) |> ignore
                    | _ -> ()
                member _.GetSupportedEvents() = null }
        iText.Kernel.Pdf.Canvas.Parser.PdfCanvasProcessor(listener).ProcessPageContent(document.GetPage 1)
        string text
    let rec paragraphIn (element: Tagging.IStructureNode) : Tagging.PdfStructElem option =
        match element with
        | :? Tagging.PdfStructElem as e when e.GetRole().GetValue() = role -> Some e
        | _ ->
            match element.GetKids() with
            | null -> None
            | kids -> kids |> Seq.tryPick (fun kid -> match kid with null -> None | kid -> paragraphIn kid)
    let paragraph = paragraphIn (document.GetStructTreeRoot()) |> Option.get
    [ for kid in paragraph.GetKids() ->
          match kid with
          | :? Tagging.PdfMcr as mcr -> drawn (mcr.GetMcid())
          | :? Tagging.PdfStructElem as element -> element.GetRole().GetValue()
          | _ -> "?" ]

/// The P's kids (see kidsOf).
let private paragraphKids = kidsOf "P"

/// Linking input to output: the fix applied, no warning left, no new failures, the page the same.
let private linksAlike (input: string) (output: string) =
    let result = validator.Value.Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.LinkUrls)
    result.AppliedFixes |> should contain "Linked 1 web address shown as text, to where it goes."
    named output |> should be Empty
    let failed (path: string) = validator.Value.Validate(path, PdfProfile.PdfUa1).PdfUaFailures |> Seq.map _.RuleId |> Set.ofSeq
    Set.difference (failed output) (failed input) |> should be Empty
    render output |> should equal (render input)

[<Fact>]
let ``an address whose last piece goes on with a full stop is linked whole, the full stop split off after it`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    let width (text: string) = float ((iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA).GetWidth(text, 12f))
    let after (text: string) = 72.0 + width text
    // As a justified line sets it: the hyphen a gap on, each piece its own text object.
    paragraphOfPieces
        input.Path
        [ 72.0, 700.0, "Visit "
          after "Visit ", 700.0, "https://example.org/go/dol"
          after "Visit https://example.org/go/dol " + 1.0, 700.0, "-"
          after "Visit https://example.org/go/dol -" + 1.0, 700.0, "diversification."
          after "Visit https://example.org/go/dol -diversification. " + 1.0, 700.0, "Open daily." ]
    named input.Path |> should equal [ "https://example.org/go/dol-diversification", true ]

    linksAlike input.Path output.Path

    paragraphKids output.Path |> should equal [ "Visit "; "Link"; "."; "Open daily." ]
    let link = (validator.Value.ReadStructureTree output.Path |> notNull).Children[0].Children[0].Children[0]
    ((string link.Content).Replace(" ", ""), string link.LinkUri)
    |> should equal ("https://example.org/go/dol-diversification", "https://example.org/go/dol-diversification")

[<Fact>]
let ``an address's full stop is split off inside the text object its pieces share, its spacing kept`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    tagged input.Path (fun document pointer font ->
        let page = document.AddNewPage()
        let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas page
        pointer.SetPageForTagging(page).AddTag "P" |> ignore
        canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 700.0) |> ignore
        let piece (draw: unit -> unit) =
            canvas.OpenTag(pointer.GetTagReference()) |> ignore
            draw ()
            canvas.CloseTag() |> ignore
        piece (fun () -> canvas.ShowText "See " |> ignore)
        // Kerned: the full stop drawn a little nearer.
        piece (fun () ->
            canvas.ShowText(PdfArray(ResizeArray<PdfObject> [ PdfString "https://example.org/beds" :> PdfObject; PdfNumber 80; PdfString "." ])) |> ignore)
        piece (fun () -> canvas.ShowText " Open daily." |> ignore)
        canvas.EndText() |> ignore)
    named input.Path |> should equal [ "https://example.org/beds", true ]

    linksAlike input.Path output.Path

    paragraphKids output.Path |> should equal [ "See "; "Link"; "."; " Open daily." ]

/// A tagged page with a P holding two pieces of marked content (MCIDs 0 and 1), its content as
/// content writes it given the font's name in the page's resources and the page.
let private rawParagraph (path: string) (content: PdfName -> PdfPage -> string) =
    tagged path (fun document _ font ->
        let page = document.AddNewPage()
        let documentElement = document.GetStructTreeRoot().AddKid(Tagging.PdfStructElem(document, PdfName.Document))
        let paragraph = documentElement.AddKid(Tagging.PdfStructElem(document, PdfName.P, page))
        for _ in 0..1 do
            paragraph.AddKid(Tagging.PdfMcrNumber(page, paragraph)) |> ignore
        let name = page.GetResources().AddFont(document, font)
        let stream = match page.GetFirstContentStream() with null -> page.NewContentStreamAfter() | stream -> stream
        stream.SetData(System.Text.Encoding.ASCII.GetBytes(content name page)) |> ignore)

[<Fact>]
let ``an address's full stop drawn inside a layer in its piece is split off, the layer begun again around it`` () =
    // As Affinity's PDFs draw a paragraph: its text object inside a layer, inside its marked
    // content; then the text object begun before the layer, and the piece begun inside it.
    let layered (font: PdfName) (page: PdfPage) (draw: string -> string -> string) =
        let layer = iText.Kernel.Pdf.Layer.PdfLayer("Publisher layout", page.GetDocument())
        let oc = page.GetResources().AddProperties(layer.GetPdfObject()).GetValue()
        let f = font.GetValue()
        draw $"/{f} 12 Tf" $"/OC /{oc} BDC"
    let cases: (string * (PdfName -> PdfPage -> string)) list =
        [ "text object inside the layer",
          (fun font page ->
              layered font page (fun tf oc ->
                  $"/P <</MCID 0>> BDC {oc} 0 0 1 rg BT {tf} 72 700 Td (https://example.org/beds.) Tj ET EMC EMC\n"
                  + $"/P <</MCID 1>> BDC BT {tf} 250 700 Td ( Open daily.) Tj ET EMC"))
          "layer inside the text object",
          (fun font page ->
              layered font page (fun tf oc ->
                  $"/P <</MCID 0>> BDC BT {tf} 72 700 Td {oc} (https://example.org/beds.) Tj EMC ET EMC\n"
                  + $"/P <</MCID 1>> BDC BT {tf} 250 700 Td ( Open daily.) Tj ET EMC"))
          "piece and layer inside the text object",
          (fun font page ->
              layered font page (fun tf oc ->
                  $"BT {tf} 72 700 Td /P <</MCID 0>> BDC /Span BMC EMC {oc} (https://example.org/beds.) Tj EMC EMC\n"
                  + "/P <</MCID 1>> BDC ( Open daily.) Tj EMC ET")) ]
    for _, content in cases do
        use input = new TempPdf()
        use output = new TempPdf ".fixed"
        rawParagraph input.Path content

        named input.Path |> should equal [ "https://example.org/beds", true ]
        linksAlike input.Path output.Path
        paragraphKids output.Path |> should equal [ "Link"; "."; " Open daily." ]

[<Fact>]
let ``an address's full stop isn't split off where its piece can't be cut safely, and each says why`` () =
    let lines (font: PdfName) (first: string) (draw: string) (after: string) =
        $"BT /{font.GetValue()} 12 Tf 72 700 Td\n{first} BDC {draw} EMC\n/P <</MCID 1>> BDC ( Open daily.) Tj EMC\nET\n{after}"
    let cases: (string * (PdfName -> PdfPage -> string)) list =
        [ // As the fix can split: the case the others change one thing of.
          "none", (fun font _ -> lines font "/P <</MCID 0>>" "(https://example.org/beds.) Tj" "")
          "its marked content holds tagged marked content of its own",
          (fun font _ -> lines font "/P <</MCID 0>>" "/Span <</MCID 2>> BDC EMC (https://example.org/beds.) Tj" "")
          // Marked content open at the cut, saying what it reads as: begun again around the end,
          // it would be said twice. Inline, and named in the page's resources.
          "marked content around it inside its piece says what it reads as, for all of it",
          (fun font _ ->
              lines font "/P <</MCID 0>>" "/Span <</ActualText (https://example.org/beds.)>> BDC (https://example.org/beds.) Tj EMC" "")
          "marked content around it inside its piece says what it reads as, for all of it",
          (fun font page ->
              let properties = PdfDictionary()
              properties.Put(PdfName.ActualText, PdfString "https://example.org/beds.") |> ignore
              let name = page.GetResources().AddProperties properties
              lines font "/P <</MCID 0>>" $"/Span /{name.GetValue()} BDC (https://example.org/beds.) Tj EMC" "")
          "its marked content says what it reads as, for all of it",
          (fun font _ -> lines font "/P <</MCID 0 /ActualText (https://example.org/beds.)>>" "(https://example.org/beds.) Tj" "")
          "the text drawing it moves to a new line first, which the fix doesn't cut",
          (fun font _ -> lines font "/P <</MCID 0>>" "(https://example.org/beds.) '" "")
          "its marked content is drawn in more than one place",
          (fun font _ -> lines font "/P <</MCID 0>>" "(https://example.org/beds.) Tj" "/P <</MCID 0>> BDC EMC\n")
          // Properties named in the page's resources, holding an indirect reference a level down:
          // written inline, the page's content would be broken.
          "its marked content's properties can't be copied",
          (fun font page ->
              let properties = PdfDictionary()
              properties.Put(PdfName.MCID, PdfNumber 0) |> ignore
              let inner = PdfDictionary()
              inner.Put(PdfName "Note", (PdfString "kept elsewhere").MakeIndirect(page.GetDocument())) |> ignore
              properties.Put(PdfName "Extra", inner) |> ignore
              let name = page.GetResources().AddProperties properties
              lines font $"/P /{name.GetValue()}" "(https://example.org/beds.) Tj" "") ]
    for why, content in cases do
        use input = new TempPdf()
        use output = new TempPdf ".fixed"
        rawParagraph input.Path content
        let fixedOnes = validator.Value.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkUrls).WasFixed
        (why, unlinked input.Path, fixedOnes)
        |> should
            equal
            (if why = "none" then
                 why, [ "Page 1: \"https://example.org/beds\".", true ], true
             else
                 why,
                 [ $"Page 1: \"https://example.org/beds\". Not fixable here: the \".\" after it is drawn in the same piece, and can't be split off: {why}.", false ],
                 false)

[<Fact>]
let ``an address's full stop with something drawn after it in the same piece isn't split off, and says why`` () =
    use pdf = new TempPdf()
    tagged pdf.Path (fun document pointer font ->
        let page = document.AddNewPage()
        let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas page
        pointer.SetPageForTagging(page).AddTag "P" |> ignore
        canvas.OpenTag(pointer.GetTagReference()) |> ignore
        show canvas font 72.0 700.0 "https://example.org/beds."
        // An underline, in the same piece.
        canvas.Rectangle(72.0, 698.0, 150.0, 0.5).Fill() |> ignore
        canvas.CloseTag() |> ignore)
    unlinked pdf.Path
    |> should
        equal
        [ "Page 1: \"https://example.org/beds\". Not fixable here: the \".\" after it is drawn in the same piece, and can't be split off: something is drawn after it, in the same piece.", false ]

// ---- An address in the middle of a sentence: carved out of its piece of text ----

[<Fact>]
let ``addresses in the middle of a sentence are carved out of its text and linked, the text around them read as it was`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // Two in one piece of text: the second is in what's left after the first is carved out.
    createPdf input.Path [ [ Plain "Visit https://example.org/garden today, or www.example.org/beds." ] ]
    named input.Path |> should equal [ "https://example.org/garden", true; "www.example.org/beds", true ]

    let result = validator.Value.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkUrls)

    result.AppliedFixes |> should contain "Linked 2 web addresses shown as text, each to where it goes."
    named output.Path |> should be Empty
    let failed (path: string) = validator.Value.Validate(path, PdfProfile.PdfUa1).PdfUaFailures |> Seq.map _.RuleId |> Set.ofSeq
    Set.difference (failed output.Path) (failed input.Path) |> should be Empty
    render output.Path |> should equal (render input.Path)
    let rec holderOf (node: StructureNode) : StructureNode option =
        if node.Children |> Seq.exists (fun child -> child.Tag = "Link") then Some node else node.Children |> Seq.tryPick holderOf
    let holder = (validator.Value.ReadStructureTree output.Path |> notNull) |> holderOf |> Option.get
    // (The tree's text for the Span leaves out its Links'.)
    (string holder.Content, [ for child in holder.Children -> child.Tag, string child.Content, string child.LinkUri ])
    |> should
        equal
        ("Visit   today, or  .",
         [ "Link", "https://example.org/garden", "https://example.org/garden"; "Link", "www.example.org/beds", "https://www.example.org/beds" ])
    kidsOf "Span" output.Path |> should equal [ "Visit "; "Link"; " today, or "; "Link"; "." ]

[<Fact>]
let ``an address in the middle of its piece is carved out however its text is drawn, its lines kept where they were`` () =
    // As a paragraph is drawn inside one text object, the piece begun inside it, each line moved
    // to from the last: by Td, by ', and the address's letters spaced by TJ.
    let cases: (string * (PdfName -> PdfPage -> string)) list =
        [ "Tj, then a line moved to",
          (fun font _ ->
              $"BT /{font.GetValue()} 12 Tf 72 700 Td\n/P <</MCID 0>> BDC (Visit https://example.org/beds for more.) Tj 0 -14 Td (Then the paths.) Tj EMC\n"
              + "/P <</MCID 1>> BDC ( Open daily.) Tj EMC\nET")
          "TJ, spaced inside the address and around it",
          (fun font _ ->
              $"BT /{font.GetValue()} 12 Tf 72 700 Td\n/P <</MCID 0>> BDC [(Visit https://exa) -20 (mple.org/beds) 30 ( for more.)] TJ 0 -14 Td (Then the paths.) Tj EMC\n"
              + "/P <</MCID 1>> BDC ( Open daily.) Tj EMC\nET")
          "', the next line's text",
          (fun font _ ->
              $"BT /{font.GetValue()} 12 Tf 14 TL 72 714 Td\n/P <</MCID 0>> BDC (Visit https://example.org/beds for more.) ' (Then the paths.) ' EMC\n"
              + "/P <</MCID 1>> BDC ( Open daily.) Tj EMC\nET")
          "the address its own Tj, the text around it others",
          (fun font _ ->
              $"BT /{font.GetValue()} 12 Tf 72 700 Td\n/P <</MCID 0>> BDC (Visit ) Tj (https://example.org/beds) Tj ( for more.) Tj 0 -14 Td (Then the paths.) Tj EMC\n"
              + "/P <</MCID 1>> BDC ( Open daily.) Tj EMC\nET") ]
    for case, content in cases do
        use input = new TempPdf()
        use output = new TempPdf ".fixed"
        rawParagraph input.Path content
        (case, named input.Path) |> should equal (case, [ "https://example.org/beds", true ])

        linksAlike input.Path output.Path

        (case, paragraphKids output.Path) |> should equal (case, [ "Visit "; "Link"; " for more.Then the paths."; " Open daily." ])

[<Fact>]
let ``an address in the middle of its piece isn't carved out where it can't be cut safely, and each says why`` () =
    let paragraph (font: PdfName) (draw: string) =
        $"BT /{font.GetValue()} 12 Tf 72 700 Td\n/P <</MCID 0>> BDC {draw} EMC\n/P <</MCID 1>> BDC ( Open daily.) Tj EMC\nET"
    let cases: (string * (PdfName -> PdfPage -> string)) list =
        [ // As the fix can carve: the case the others change one thing of.
          "none", (fun font _ -> paragraph font "(Visit https://example.org/beds today.) Tj")
          "it's shown more than once in its piece",
          (fun font _ -> paragraph font "(Visit https://example.org/beds today.) Tj 0 -14 Td (Again: https://example.org/beds today.) Tj")
          // Its letters drawn in two parts, with other text drawn between them (put back in place).
          "its letters aren't drawn one after another in its piece",
          (fun font _ ->
              let width = (iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA).GetWidth("Visit https://exam", 12f)
              paragraph font $"""(Visit https://exam) Tj 0 -14 Td (Later.) Tj {width.ToString(System.Globalization.CultureInfo.InvariantCulture)} 14 Td (ple.org/beds today.) Tj""")
          "marked content around it inside its piece says what it reads as, for all of it",
          (fun font _ -> paragraph font "/Span <</ActualText (Visit the beds.)>> BDC (Visit https://example.org/beds today.) Tj EMC")
          // Its piece begun outside the text object, a line after it moved to from the last: the
          // text object begun again at the cut would move it.
          "its text is begun inside its piece, and lines after it are placed from where theirs begin, which the fix can't keep once it's cut",
          (fun font _ ->
              $"/P <</MCID 0>> BDC BT /{font.GetValue()} 12 Tf 72 700 Td (Visit https://example.org/beds today.) Tj 0 -14 Td (Then the paths.) Tj ET EMC\n"
              + $"/P <</MCID 1>> BDC BT /{font.GetValue()} 12 Tf 72 650 Td ( Open daily.) Tj ET EMC") ]
    for why, content in cases do
        use input = new TempPdf()
        use output = new TempPdf ".fixed"
        rawParagraph input.Path content
        let found = unlinked input.Path |> List.distinct
        let fixedOnes = validator.Value.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkUrls).WasFixed
        (why, found, fixedOnes)
        |> should
            equal
            (if why = "none" then
                 why, [ "Page 1: \"https://example.org/beds\".", true ], true
             else
                 why,
                 [ $"Page 1: \"https://example.org/beds\". Not fixable here: it's part of a longer piece of text, which can't be cut around it: {why}.", false ],
                 false)

[<Fact>]
let ``an address in the middle of a piece begun outside its text object is carved out when no line after it moves from the last`` () =
    // As Word draws a line: its piece around its own text object, the line placed by Tm.
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    rawParagraph input.Path (fun font _ ->
        let f = font.GetValue()
        $"/P <</MCID 0>> BDC BT /{f} 12 Tf 1 0 0 1 72 700 Tm [(Visit https://exa) -20 (mple.org/beds today.)] TJ 1 0 0 1 72 686 Tm (Then the paths.) Tj ET EMC\n"
        + $"/P <</MCID 1>> BDC BT /{f} 12 Tf 72 650 Td ( Open daily.) Tj ET EMC")
    named input.Path |> should equal [ "https://example.org/beds", true ]

    linksAlike input.Path output.Path

    paragraphKids output.Path |> should equal [ "Visit "; "Link"; " today.Then the paths."; " Open daily." ]

[<Fact>]
let ``an address one of whose pieces is read on both sides of another isn't linked, and says why`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    tagged input.Path (fun document pointer font ->
        let page = document.AddNewPage()
        let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas page
        let width (text: string) = float (font.GetWidth(text, 12f))
        pointer.SetPageForTagging(page).AddTag "P" |> ignore
        // One marked content draws the address's start and its end, the next one its middle: a
        // Link holding them in their tag's order would read "https://example.org//beds garden".
        canvas.OpenTag(pointer.GetTagReference()) |> ignore
        show canvas font 72.0 700.0 "https://example.org/"
        show canvas font (72.0 + width "https://example.org/garden") 700.0 "/beds"
        canvas.CloseTag() |> ignore
        canvas.OpenTag(pointer.GetTagReference()) |> ignore
        show canvas font (72.0 + width "https://example.org/") 700.0 "garden"
        canvas.CloseTag() |> ignore)
    unlinked input.Path
    |> should
        equal
        [ "Page 1: \"https://example.org/garden/beds\". Not fixable here: a piece of it is read on both sides of another, so its pieces can't be tagged in the address's order.", false ]

    validator.Value.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkUrls).WasFixed |> should be False

[<Fact>]
let ``an address drawn right to left is read left to right, whole`` () =
    use pdf = new TempPdf()
    tagged pdf.Path (fun document pointer font ->
        let page = document.AddNewPage()
        let host = float (font.GetWidth("https://example.org", 12f))
        inTags pointer page font [ "P"; "Span" ] (72.0 + host) 700.0 "/plan"
        inTags pointer page font [ "P"; "Span" ] 72.0 700.0 "https://example.org")
    named pdf.Path |> should equal [ "https://example.org/plan", false ]

[<Fact>]
let ``www. inside a word or an e-mail address isn't an address`` () =
    use pdf = new TempPdf()
    tagged pdf.Path (fun document pointer font ->
        inTags pointer (document.AddNewPage()) font [ "P" ] 72.0 700.0 "Write to jay@www.example.org about awww.cool things.")
    named pdf.Path |> should be Empty

[<Fact>]
let ``an address in a running footer (an artifact, not read) isn't warned about`` () =
    use pdf = new TempPdf()
    tagged pdf.Path (fun document pointer font ->
        for i in 1..2 do
            let page = document.AddNewPage()
            inTags pointer page font [ "P" ] 72.0 700.0 $"Page {i} of the garden plan."
            let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas page
            canvas.OpenTag(iText.Kernel.Pdf.Canvas.CanvasArtifact()) |> ignore
            show canvas font 72.0 40.0 "www.example.org/footer"
            canvas.CloseTag() |> ignore)
    named pdf.Path |> should be Empty

[<Fact>]
let ``an untagged file's addresses are warned about, the fix left for tagging`` () =
    use pdf = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(pdf.Path))
        show (iText.Kernel.Pdf.Canvas.PdfCanvas(document.AddNewPage())) (iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA) 72.0 700.0 "See https://example.org/plan today."
    unlinked pdf.Path |> should equal [ "Page 1: \"https://example.org/plan\". Not fixable here: the PDF isn't tagged: tagging it comes first.", false ]

[<Fact>]
let ``a span already in a Link with nothing to follow is reported, not wrapped in another`` () =
    use pdf = new TempPdf()
    tagged pdf.Path (fun document pointer font -> inTags pointer (document.AddNewPage()) font [ "P"; "Link"; "Span" ] 72.0 700.0 "https://example.org/plan")
    unlinked pdf.Path
    |> should equal [ "Page 1: \"https://example.org/plan\". Not fixable here: it's tagged as a link already, with no link annotation to follow.", false ]

[<Fact>]
let ``the fix moves the address's own page's content, not another page's with the same number`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    // One P: MCID 0 on page 1 (other text) and MCID 0 on page 2 (the address alone).
    tagged input.Path (fun document pointer font ->
        let first, second = document.AddNewPage(), document.AddNewPage()
        pointer.SetPageForTagging(first).AddTag("P") |> ignore
        for page, text in [ first, "Intro text on page one."; second, "https://example.org/plan" ] do
            pointer.SetPageForTagging page |> ignore
            let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas page
            canvas.OpenTag(pointer.GetTagReference()) |> ignore
            show canvas font 72.0 700.0 text
            canvas.CloseTag() |> ignore)
    validator.Value.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkUrls).WasFixed |> should be True
    let failed = validator.Value.Validate(output.Path, PdfProfile.PdfUa1).PdfUaFailures |> Seq.map _.RuleId |> List.ofSeq
    failed |> should not' (contain "7.1-3")
    let paragraph = (validator.Value.ReadStructureTree output.Path |> notNull).Children[0].Children[0]
    (string paragraph.Content, [ for child in paragraph.Children -> child.Tag, string child.Content, child.PageNumber.GetValueOrDefault() ])
    |> should equal ("Intro text on page one.", [ "Link", "https://example.org/plan", 2 ])

[<Fact>]
let ``a span taking its page from its paragraph is linked`` () =
    use input = new TempPdf()
    use output = new TempPdf ".fixed"
    tagged input.Path (fun document pointer font ->
        inTags pointer (document.AddNewPage()) font [ "P"; "Span" ] 72.0 700.0 "https://example.org/plan"
        let span = ((document.GetStructTreeRoot().GetKids()[0] :?> iText.Kernel.Pdf.Tagging.PdfStructElem).GetKids()[0] :?> iText.Kernel.Pdf.Tagging.PdfStructElem)
        span.GetPdfObject().Remove PdfName.Pg |> ignore)
    let result = validator.Value.Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.LinkUrls)
    result.AppliedFixes |> should contain "Linked 1 web address shown as text, to where it goes."
    named output.Path |> should be Empty

[<Fact>]
let ``each address is dismissed on its own`` () =
    use pdf = new TempPdf()
    tagged pdf.Path (fun document pointer font ->
        let page = document.AddNewPage()
        inTags pointer page font [ "P"; "Span" ] 72.0 700.0 "https://example.org/one"
        inTags pointer page font [ "P"; "Span" ] 72.0 680.0 "https://example.org/two"
        inTags pointer page font [ "P" ] 72.0 660.0 "See www.example.org and www.example.net.")
    let warnings =
        validator.Value.Validate(pdf.Path, PdfProfile.PdfUa1).Assertions
        |> Seq.filter (fun finding -> finding.Specification = "Legibyl" && finding.TestNumber = 18)
        |> List.ofSeq
    warnings |> List.map ReviewFile.dismissalLocation |> List.distinct |> should haveLength 4
    let first = warnings.Head
    let review: ReviewFile.SavedFile =
        { Version = 1
          DismissedWarnings = [| { Rule = ReviewFile.dismissalRule first; Location = ReviewFile.dismissalLocation first; Message = first.Message } |]
          Edits = null; Title = null; Language = null; RoleMap = null; Attachments = null; Layout = null; Fixes = null; Validation = null; Checklist = null }
    warnings |> List.filter (ReviewFile.dismisses review) |> should equal [ first ]
