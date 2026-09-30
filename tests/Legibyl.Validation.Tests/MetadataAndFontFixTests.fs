module Legibyl.Validation.Tests.MetadataAndFontFixTests

open Xunit
open FsUnit.Xunit
open iText.Kernel.Pdf
open iText.Kernel.XMP
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private failedRules (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

[<Fact>]
let ``a document with no XMP metadata gets a stream from its Info dictionary, which veraPDF then fixes`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        document.AddNewPage() |> ignore
        document.GetDocumentInfo().SetTitle("Cub Scout Pack 721").SetAuthor "Pack 721" |> ignore
    failedRules input.Path |> should contain "7.1-8"

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.Metadata)

    result.AppliedFixes |> should contain "Added an XMP metadata stream, from the document information dictionary."
    result.Status |> should equal RepairStatus.Success
    do
        use document = new PdfDocument(new PdfReader(output.Path))
        let metadata = document.GetCatalog().GetPdfObject().GetAsStream PdfName.Metadata |> notNull
        (metadata.GetAsName PdfName.Type, metadata.GetAsName PdfName.Subtype) |> should equal (PdfName.Metadata, PdfName.XML)
        let xmp = document.GetXmpMetadata() |> notNull
        xmp.GetLocalizedText(XMPConst.NS_DC, "title", null, XMPConst.X_DEFAULT).GetValue() |> should equal "Cub Scout Pack 721"
    // (veraPDF doesn't add a PDF/UA identification while the document fails other rules:
    // it won't claim a conformance the document doesn't have.)
    failedRules output.Path |> should not' (contain "7.1-8")

let private failures (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> List.ofSeq

[<Fact>]
let ``an untitled document with no XMP metadata fails dc:title now, not once the metadata's written`` () =
    use input = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        document.AddNewPage() |> ignore

    let found = failures input.Path
    let title = found |> List.find (fun f -> f.RuleId = "7.1-9")
    title.ErrorMessage |> should haveSubstring "The document has no title"
    title.Message |> should haveSubstring "dc:title"
    // Neither the metadata fix nor the title fix has a title to give it.
    title.FixOptions |> should equal PdfFixOptions.None
    found |> List.filter (fun f -> f.RuleId = "7.1-9") |> should haveLength 1
    (found |> List.find (fun f -> f.RuleId = "7.1-8")).FixOptions |> should equal PdfFixOptions.Metadata
    // So the metadata fix waits for the title.
    FixNames.metadataReady false found |> should be False
    // Unless a title's been typed, which Export… writes before the metadata fix.
    let metadataAndTitle = found |> List.filter (fun f -> f.RuleId = "7.1-8" || f.RuleId = "7.1-9")
    FixNames.metadataReady false metadataAndTitle |> should be False
    FixNames.metadataReady true metadataAndTitle |> should be True

[<Fact>]
let ``an untitled document with a title page can be titled from it`` () =
    use input = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        use layout = new iText.Layout.Document(document)
        layout.Add(iText.Layout.Element.Paragraph "The Garden in Spring") |> ignore
        layout.Add(iText.Layout.Element.AreaBreak()) |> ignore
        layout.Add(iText.Layout.Element.Paragraph "What grows first.") |> ignore

    (failures input.Path |> List.find (fun f -> f.RuleId = "7.1-9")).FixOptions |> should equal PdfFixOptions.TitleFromFirstPage

[<Fact>]
let ``a titled document with no XMP metadata isn't reported for dc:title: the metadata fix copies it`` () =
    use input = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        document.AddNewPage() |> ignore
        document.GetDocumentInfo().SetTitle "Cub Scout Pack 721" |> ignore

    failures input.Path |> List.map _.RuleId |> should not' (contain "7.1-9")

[<Fact>]
let ``a document's own XMP metadata is left for veraPDF to fix`` () =
    use output = new TempPdf()
    let result = VeraPdfValidator().Fix(corpusFile "pdfua1-fail.pdf", PdfProfile.PdfUa1, output.Path, PdfFixOptions.Metadata)
    result.AppliedFixes |> should not' (contain "Added an XMP metadata stream, from the document information dictionary.")

/// A PDF holding CID fonts, as indirect objects: each descendant is (embedded, has a map).
let private createFonts (path: string) (fonts: (bool * bool) list) =
    use document = new PdfDocument(new PdfWriter(path))
    let fontResources = PdfDictionary()
    let resources = PdfDictionary()
    resources.Put(PdfName.Font, fontResources) |> ignore
    document.AddNewPage().GetPdfObject().Put(PdfName.Resources, resources) |> ignore
    for i, (embedded, mapped) in List.indexed fonts do
        let descriptor = PdfDictionary()
        descriptor.Put(PdfName.Type, PdfName.FontDescriptor) |> ignore
        descriptor.Put(PdfName.FontName, PdfName "AAAAAB+Caprasimo-Regular") |> ignore
        if embedded then descriptor.Put(PdfName.FontFile2, PdfStream([| 0uy |]).MakeIndirect document) |> ignore
        let descendant = PdfDictionary()
        descendant.Put(PdfName.Type, PdfName.Font) |> ignore
        descendant.Put(PdfName.Subtype, PdfName.CIDFontType2) |> ignore
        descendant.Put(PdfName.BaseFont, PdfName "AAAAAB+Caprasimo-Regular") |> ignore
        let systemInfo = PdfDictionary()
        systemInfo.Put(PdfName.Registry, PdfString "Adobe") |> ignore
        systemInfo.Put(PdfName.Ordering, PdfString "Identity") |> ignore
        systemInfo.Put(PdfName.Supplement, PdfNumber 0) |> ignore
        descendant.Put(PdfName.CIDSystemInfo, systemInfo) |> ignore
        descendant.Put(PdfName.FontDescriptor, descriptor.MakeIndirect document) |> ignore
        if mapped then descendant.Put(PdfName.CIDToGIDMap, PdfName.Identity) |> ignore
        let font = PdfDictionary()
        font.Put(PdfName.Type, PdfName.Font) |> ignore
        font.Put(PdfName.Subtype, PdfName.Type0) |> ignore
        font.Put(PdfName.BaseFont, PdfName "AAAAAB+Caprasimo-Regular") |> ignore
        font.Put(PdfName.Encoding, PdfName.IdentityH) |> ignore
        // Direct, as Quartz writes it for its own fonts too.
        font.Put(PdfName.DescendantFonts, PdfArray(descendant :> PdfObject)) |> ignore
        fontResources.Put(PdfName $"F{i}", font.MakeIndirect document) |> ignore

let private glyphMaps (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    [ for number in 1 .. document.GetNumberOfPdfObjects() - 1 do
          match document.GetPdfObject number with
          | :? PdfDictionary as font when PdfName.Type0.Equals(font.GetAsName PdfName.Subtype) ->
              let descendant = font.GetAsArray(PdfName.DescendantFonts).GetAsDictionary 0
              yield descendant.GetAsName PdfName.CIDToGIDMap |> Option.ofObj |> Option.map _.GetValue()
          | _ -> () ]

[<Fact>]
let ``embedded TrueType CID fonts without a glyph mapping get Identity, and only those`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // Embedded without a map, embedded with one, not embedded.
    createFonts input.Path [ true, false; true, true; false, false ]

    let result = VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.FontGlyphs)

    result.AppliedFixes
    |> should contain "Set /CIDToGIDMap /Identity on 1 embedded TrueType CID font(s), the mapping readers already use when it's missing."
    glyphMaps output.Path |> should equal [ Some "Identity"; Some "Identity"; None ]
