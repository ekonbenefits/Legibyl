module Legibyl.Validation.Tests.PdfLocationParserTests

open System
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

[<Fact>]
let ``Parse null returns null`` () =
    PdfLocationParser.Parse null |> should be Null

[<Fact>]
let ``Parse empty returns null`` () =
    PdfLocationParser.Parse "" |> should be Null

[<Fact>]
let ``Parse document only has no page number`` () =
    let location = PdfLocationParser.Parse "root/document[0]" |> notNull

    location.PageNumber.HasValue |> should be False
    location.Breadcrumb |> should equal "Document"

[<Fact>]
let ``Parse page with content item extracts one-based page number`` () =
    // veraPDF's page index is 0-based ("pages[0]" is page 1).
    let location =
        PdfLocationParser.Parse
            "root/document[0]/pages[0](2 0 obj PDPage)/contentStream[0](6 0 obj PDSemanticContentStream)/content[0]/contentItem[0]"
        |> notNull

    location.PageNumber |> should equal (Nullable 1)
    location.Breadcrumb |> should haveSubstring "Page 1"
    location.Breadcrumb |> should haveSubstring "Content stream"

[<Fact>]
let ``Parse link annotation on second page extracts page number and object info`` () =
    let location =
        PdfLocationParser.Parse "root/document[0]/pages[1](7 0 obj PDPage)/annots[0](9 0 obj PDLinkAnnot)"
        |> notNull

    location.PageNumber |> should equal (Nullable 2)
    location.ObjectNumber |> should equal (Nullable 9)
    location.ObjectType |> should equal "PDLinkAnnot"
    location.Breadcrumb |> should haveSubstring "Page 2"
    location.Breadcrumb |> should haveSubstring "Link annotation"

[<Fact>]
let ``Parse table cell in structure tree describes tags readably`` () =
    let location =
        PdfLocationParser.Parse(
            "root/document[0]/StructTreeRoot[0](123 0 obj PDStructTreeRoot)/K[0](124 0 obj SEDocument Document)/"
            + "K[231](545 0 obj SETable Table)/K[1](551 0 obj SETR TR)/K[0](552 0 obj SETD TD)"
        )
        |> notNull

    location.PageNumber.HasValue |> should be False
    location.ObjectNumber |> should equal (Nullable 552)
    location.ObjectType |> should equal "SETD TD"
    let breadcrumb = location.Breadcrumb.ToLowerInvariant()
    breadcrumb |> should haveSubstring "table row (tr)"
    breadcrumb |> should haveSubstring "table cell (td)"
    breadcrumb |> should haveSubstring "structure tree"

[<Fact>]
let ``Parse unrecognisable string falls back to raw value`` () =
    let location = PdfLocationParser.Parse "!!! not a real location context ???" |> notNull

    location.PageNumber.HasValue |> should be False
    location.Breadcrumb |> should equal "!!! not a real location context ???"

[<Fact>]
let ``marked content segments read as content, and root alone as the document`` () =
    let location =
        PdfLocationParser.Parse "root/document[0]/pages[0](11 0 obj PDPage)/contentStream[0](21 0 obj PDSemanticContentStream)/content[10]{mcid:22}/contentItem[0]{mcid:22}"
        |> nonNull
    location.Breadcrumb |> should equal "Document → Page 1 → Content stream → Content → Content item"
    (PdfLocationParser.Parse "root" |> nonNull).Breadcrumb |> should equal "Document"

[<Fact>]
let ``a finding's location names what's there: the element, or content no element has`` () =
    let breadcrumbs name =
        VeraPdfValidator().Validate(corpusFile name, PdfProfile.PdfUa1).Findings
        |> Seq.choose (fun f -> f.Location |> Option.ofObj |> Option.map _.Breadcrumb)
        |> List.ofSeq
    // Marked content an element has: that element, by tag and text.
    breadcrumbs "pdfua1-fail.pdf"
    |> should contain "Document → Page 1 → text span (Span) \"Artifacts may also be used to describe a…\""
    // Marked content no element has: what's drawn there.
    breadcrumbs "pages-clipped-paragraphs.pdf"
    |> should contain "Document → Page 1 → content not in the tags \"First paragraph of the document.\""
    // Legibyl's own warnings: the element they're about.
    breadcrumbs "libreoffice-empty-paragraph.pdf" |> should contain "Structure tree → paragraph (P)"
