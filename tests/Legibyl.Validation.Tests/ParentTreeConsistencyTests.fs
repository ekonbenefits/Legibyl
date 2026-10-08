module Legibyl.Validation.Tests.ParentTreeConsistencyTests

open System.IO
open System.Text
open System.Text.RegularExpressions
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Tagging
open Xunit
open FsUnit.Xunit
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private create (path: string) role orphan draw description =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    let page = document.AddNewPage()
    let parent = PdfStructElem(document, PdfName.Document)
    document.GetStructTreeRoot().AddKid parent |> ignore
    let add (tag: string) (mcid: int) =
        let element = PdfStructElem(document, PdfName tag)
        element.GetPdfObject().Put(PdfName.Pg, page.GetPdfObject()) |> ignore
        parent.AddKid element |> ignore
        element.AddKid(PdfMcrNumber(PdfNumber mcid, element)) |> ignore
        element
    add "P" 0 |> ignore
    let removed = add role 1
    if description then removed.SetAlt(PdfString "A meaningful description") |> ignore
    let canvas = PdfCanvas page
    let marked (tag: string) (mcid: int) action =
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
        if mcid = 1 && draw = "actual-text" then properties.Put(PdfName.ActualText, PdfString "Important replacement text.") |> ignore
        canvas.BeginMarkedContent(PdfName tag, properties) |> ignore
        action canvas
        canvas.EndMarkedContent() |> ignore
    let text (value: string) (c: PdfCanvas) =
        c.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f).MoveText(72.0, 700.0).ShowText(value).EndText() |> ignore
    marked "P" 0 (text "The full disclosure must remain accessible.")
    marked (if role = "Artifact" then "Span" else role) 1 (fun c ->
        match draw with
        | "text" -> text "Important disclosure text." c
        | "space" -> text " " c
        | "shape" -> c.Rectangle(72.0, 600.0, 30.0, 30.0).Fill() |> ignore
        // Glyphs drawn in a font that says nothing of the characters they are: read as no text.
        | "unreadable" -> c.GetContentStream().GetOutputStream().WriteString("BT /Unreadable 12 Tf 72 650 Td <0102> Tj ET\n") |> ignore
        | _ -> c.SaveState().Rectangle(72.0, 600.0, 30.0, 30.0).Clip().EndPath().RestoreState() |> ignore)
    if draw = "unreadable" then
        let font = PdfDictionary()
        font.Put(PdfName.Type, PdfName.Font) |> ignore
        font.Put(PdfName.Subtype, PdfName.Type1) |> ignore
        font.Put(PdfName.BaseFont, PdfName "Helvetica") |> ignore
        let encoding = PdfDictionary()
        let differences = PdfArray()
        differences.Add(PdfNumber 1)
        differences.Add(PdfName "g01")
        differences.Add(PdfName "g02")
        encoding.Put(PdfName.Differences, differences) |> ignore
        font.Put(PdfName.Encoding, encoding) |> ignore
        let resources = page.GetResources().GetPdfObject()
        let fonts = resources.GetAsDictionary PdfName.Font |> Option.ofObj |> Option.defaultWith (fun () -> let d = PdfDictionary() in resources.Put(PdfName.Font, d) |> ignore; d)
        fonts.Put(PdfName "Unreadable", font) |> ignore
    // The old pruning code removed /K directly, leaving iText's reverse registry untouched.
    if orphan then parent.GetPdfObject().GetAsArray(PdfName.K).Remove 1
    if draw = "missing-k" then
        removed.GetPdfObject().Remove PdfName.K |> ignore
        removed.GetPdfObject().Remove PdfName.P |> ignore
    if draw = "missing-parent" then removed.GetPdfObject().Remove PdfName.P |> ignore

let private warnings path =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> f.Specification = "Legibyl" && f.TestNumber = 9)
    |> List.ofSeq

let private render path =
    use renderer = new PdfRegionRenderer()
    let offPage = { PageNumber = 1; Left = -20.0; Bottom = -20.0; Right = -10.0; Top = -10.0 }
    use bitmap = SkiaSharp.SKBitmap.Decode(renderer.RenderPageWithHighlight(path, offPage).PngBytes)
    bitmap.Pixels

let private fix input output options = VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, options)

[<Theory>]
[<InlineData("P", "space")>]
[<InlineData("Figure", "clip")>]
[<InlineData("Figure", "missing-k")>]
[<InlineData("Artifact", "shape")>]
let ``obsolete reverse tags become real artifacts without changing visible or accessible text`` role draw =
    use input = new TempPdf()
    use output = new TempPdf()
    use again = new TempPdf()
    create input.Path role true draw false
    warnings input.Path |> should haveLength 1
    let before = VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq
    let result = fix input.Path output.Path PdfFixOptions.ParentTree
    result.WasFixed |> should be True
    warnings output.Path |> should be Empty
    let after = VeraPdfValidator().Validate(output.Path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq
    Set.difference after before |> should be Empty
    render output.Path |> should equal (render input.Path)
    use document = new PdfDocument(new PdfReader(output.Path))
    let refs = document.GetStructTreeRoot().GetPageMarkedContentReferences(document.GetPage 1)
    refs |> Seq.map _.GetMcid() |> List.ofSeq |> should equal [0]
    let content = System.Text.Encoding.ASCII.GetString(document.GetPage(1).GetContentBytes())
    content |> should haveSubstring "/Artifact BMC"
    content |> should haveSubstring "The full disclosure must remain accessible."
    let repeat = fix output.Path again.Path PdfFixOptions.ParentTree
    repeat.WasFixed |> should be False
    File.Exists again.Path |> should be False

[<Theory>]
[<InlineData("P", "text", false)>]
[<InlineData("Figure", "shape", false)>]
[<InlineData("Figure", "clip", true)>]
[<InlineData("P", "actual-text", false)>]
[<InlineData("P", "unreadable", false)>]
let ``meaningful orphan content is reported and never silently removed`` role draw description =
    use input = new TempPdf()
    use output = new TempPdf()
    create input.Path role true draw description
    let result = fix input.Path output.Path PdfFixOptions.ParentTree
    result.WasFixed |> should be False
    File.Exists output.Path |> should be False
    result.NotApplied |> Seq.exists (fun line -> line.StartsWith "Page 1: 1 piece(s) of content are tied to a tag that isn't in the tags any more") |> should be True
    warnings input.Path |> should haveLength 1

[<Theory>]
[<InlineData("P", "space")>]
[<InlineData("Figure", "clip")>]
[<InlineData("Figure", "missing-parent")>]
let ``pruning empty tags removes reverse slots and writes artifacts even without the parent tree fix`` role draw =
    use input = new TempPdf()
    use output = new TempPdf()
    create input.Path role false draw false
    let option = if role = "P" then PdfFixOptions.EmptyParagraphs else PdfFixOptions.EmptyFigures
    let result = fix input.Path output.Path option
    result.WasFixed |> should be True
    warnings output.Path |> should be Empty
    render output.Path |> should equal (render input.Path)
    use document = new PdfDocument(new PdfReader(output.Path))
    document.GetStructTreeRoot().GetPageMarkedContentReferences(document.GetPage 1)
    |> Seq.map _.GetMcid() |> List.ofSeq |> should equal [0]

[<Fact>]
let ``a paragraph with an image-like drawing is not empty just because it has no text`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    create input.Path "P" false "shape" false
    let result = fix input.Path output.Path PdfFixOptions.EmptyParagraphs
    result.WasFixed |> should be False
    File.Exists output.Path |> should be False

[<Theory>]
[<InlineData(false)>]
[<InlineData(true)>]
let ``an existing page entry with a null or wrong slot is reconciled to the forward owner`` wrongOwner =
    use input = new TempPdf()
    use broken = new TempPdf()
    use output = new TempPdf()
    create input.Path "P" false "text" false
    let oldReference, replacement, slotsNumber =
        use document = new PdfDocument(new PdfReader(input.Path))
        let nums = document.GetStructTreeRoot().GetPdfObject().GetAsDictionary(PdfName.ParentTree).GetAsArray PdfName.Nums
        let slots = nums.GetAsArray 1
        let reference index = slots.GetAsDictionary(index).GetIndirectReference().ToString()
        reference 1, (if wrongOwner then reference 0 else "null"), slots.GetIndirectReference().GetObjNumber()
    let bytes = Encoding.Latin1.GetString(File.ReadAllBytes input.Path)
    let pattern = $@"(?s)(?<!\d){slotsNumber} 0 obj\s*\[.*?\]"
    let mutable changed = false
    let bytes =
        Regex.Replace(bytes, pattern, fun matched ->
            changed <- true
            matched.Value.Replace(oldReference, replacement.PadRight oldReference.Length))
    changed |> should be True
    File.WriteAllBytes(broken.Path, Encoding.Latin1.GetBytes bytes)
    warnings broken.Path |> should haveLength 1
    let result = fix broken.Path output.Path PdfFixOptions.ParentTree
    result.WasFixed |> should be True
    warnings output.Path |> should be Empty
    render output.Path |> should equal (render input.Path)
    use document = new PdfDocument(new PdfReader(output.Path))
    let parent = document.GetStructTreeRoot().GetKids()[0] :?> PdfStructElem
    let expected = parent.GetKids()[1] :?> PdfStructElem
    let actual = document.GetStructTreeRoot().FindMcrByMcid(document.GetPage(1).GetPdfObject(), 1).GetParent() :?> PdfStructElem
    actual.GetPdfObject().GetIndirectReference().GetObjNumber()
    |> should equal (expected.GetPdfObject().GetIndirectReference().GetObjNumber())

[<Fact>]
let ``an empty paragraph that draws something is warned of without the fix, which leaves it`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    create input.Path "P" false "shape" false
    let empty =
        VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1).Findings
        |> Seq.filter (fun f -> f.Specification = "Legibyl" && f.TestNumber = 1)
        |> Seq.exactlyOne
    empty.FixOptions |> should equal PdfFixOptions.None
    empty.ErrorMessage |> should startWith "The fix leaves this one"

    (fix input.Path output.Path PdfFixOptions.EmptyParagraphs).WasFixed |> should be False

[<Fact>]
let ``untying content from a tag that's gone leaves no tag of its own behind`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    create input.Path "P" true "space" false

    (fix input.Path output.Path PdfFixOptions.ParentTree).WasFixed |> should be True

    use document = new PdfDocument(new PdfReader(output.Path))
    [ for i in 1 .. document.GetNumberOfPdfObjects() - 1 do
          match document.GetPdfObject i with
          | :? PdfDictionary as d when PdfName.StructElem.Equals(d.GetAsName PdfName.Type) && PdfName.Span.Equals(d.GetAsName PdfName.S) -> d
          | _ -> () ]
    |> should be Empty

/// A Document with paragraphs: "Kept text." (MCID 0), and then pieces (MCIDs 1 on), each text or
/// " ", in their own P or, if together, in one P; changed by change (the Document, its P's).
let private createPieces (path: string) (pieces: string list) (together: bool) (change: PdfStructElem -> PdfStructElem list -> unit) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    let page = document.AddNewPage()
    let top = PdfStructElem(document, PdfName.Document)
    document.GetStructTreeRoot().AddKid top |> ignore
    let paragraph () =
        let p = PdfStructElem(document, PdfName.P)
        p.GetPdfObject().Put(PdfName.Pg, page.GetPdfObject()) |> ignore
        top.AddKid p |> ignore
        p
    let first = paragraph ()
    first.AddKid(PdfMcrNumber(PdfNumber 0, first)) |> ignore
    let shared = if together then Some(paragraph ()) else None
    let others =
        [ for i in 1 .. pieces.Length do
              let p = shared |> Option.defaultWith paragraph
              p.AddKid(PdfMcrNumber(PdfNumber i, p)) |> ignore
              if shared.IsNone || i = 1 then yield p ]
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    for i, text in List.indexed ("Kept text." :: pieces) do
        let properties = PdfDictionary()
        properties.Put(PdfName.MCID, PdfNumber i) |> ignore
        canvas.BeginMarkedContent(PdfName.P, properties) |> ignore
        canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 700.0 - 20.0 * float i).ShowText(text).EndText() |> ignore
        canvas.EndMarkedContent() |> ignore
    change top (first :: others)

[<Fact>]
let ``an empty paragraph that takes its page from its parent is removed, not a failure`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPieces input.Path [ " " ] false (fun top paragraphs ->
        top.GetPdfObject().Put(PdfName.Pg, (paragraphs.Head.GetPdfObject().GetAsDictionary PdfName.Pg)) |> ignore
        for p in paragraphs do p.GetPdfObject().Remove PdfName.Pg |> ignore)

    let result = fix input.Path output.Path PdfFixOptions.EmptyParagraphs

    result.WasFixed |> should be True
    result.AppliedFixes |> Seq.exists (fun line -> line.Contains "empty") |> should be True

[<Fact>]
let ``content of a tag still in the tags, but placed on no page, isn't taken for a tag that's gone`` () =
    use input = new TempPdf()
    createPieces input.Path [ "More text." ] false (fun _ paragraphs ->
        for p in paragraphs do p.GetPdfObject().Remove PdfName.Pg |> ignore)

    VeraPdfValidator().Validate(input.Path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> f.Specification = "Legibyl" && f.TestNumber = 17)
    |> should be Empty

[<Fact>]
let ``a tag that's gone, holding text and a space, is kept whole`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createPieces input.Path [ "Kept too."; " " ] true (fun top _ ->
        // The shared paragraph taken out of the tags, its parent tree entries left.
        top.GetPdfObject().GetAsArray(PdfName.K).Remove 1)

    let result = fix input.Path output.Path PdfFixOptions.ParentTree

    result.WasFixed |> should be False
    result.NotApplied |> Seq.exists (fun line -> line.StartsWith "Page 1: 2 piece(s) of content are tied to a tag that isn't in the tags any more, and were kept") |> should be True
