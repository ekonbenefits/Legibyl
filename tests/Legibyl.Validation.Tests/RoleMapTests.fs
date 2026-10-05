module Legibyl.Validation.Tests.RoleMapTests

open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A tagged page whose elements use tags (own names, as writers give them) and draw their text,
/// with roles as its role map, in a PDF of version. (iText won't write a name that isn't mapped:
/// they're written as P, then named.)
let private createTagged (path: string) (version: PdfVersion) (elements: (string * string) list) (roles: (string * string) list) =
    let plain = path + ".plain.pdf"
    do
        use document = new PdfDocument(new PdfWriter(plain, WriterProperties().SetPdfVersion version))
        document.SetTagged() |> ignore
        document.GetCatalog().SetLang(PdfString "en-US") |> ignore
        let page = document.AddNewPage PageSize.LETTER
        let canvas = PdfCanvas page
        let font = PdfFontFactory.CreateFont(StandardFonts.HELVETICA)
        let pointer = document.GetTagStructureContext().GetAutoTaggingPointer().SetPageForTagging page
        for i, (_, text) in List.indexed elements do
            pointer.AddTag "P" |> ignore
            canvas.OpenTag(pointer.GetTagReference()).BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 700.0 - 30.0 * float i).ShowText(text).EndText().CloseTag()
            |> ignore
            pointer.MoveToParent() |> ignore
    do
        use document = new PdfDocument(new PdfReader(plain), new PdfWriter(path))
        let root = document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot
        // The elements in order (each P, in turn), named; no namespaces (the PDF 1.7 one, then).
        let rec walk (element: PdfDictionary) : PdfDictionary list =
            element.Remove PdfName.NS |> ignore
            let kids =
                match element.Get(PdfName.K, true) with
                | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do match array.Get(i, true) with :? PdfDictionary as d when not (isNull (d.Get PdfName.S)) -> d | _ -> () ]
                | :? PdfDictionary as d when not (isNull (d.Get PdfName.S)) -> [ d ]
                | _ -> []
            (if PdfName.P.Equals(element.GetAsName PdfName.S) then [ element ] else []) @ List.collect walk kids
        let paragraphs = walk (root.GetAsArray(PdfName.K) |> fun kids -> if isNull kids then root.GetAsDictionary PdfName.K else kids.GetAsDictionary 0)
        for element, (tag, _) in List.zip paragraphs elements do
            element.Put(PdfName.S, PdfName tag) |> ignore
        root.Remove PdfName.Namespaces |> ignore
        if roles.IsEmpty then
            root.Remove PdfName.RoleMap |> ignore
        else
            let map = PdfDictionary()
            for name, role in roles do
                map.Put(PdfName name, PdfName role) |> ignore
            root.Put(PdfName.RoleMap, map) |> ignore
    System.IO.File.Delete plain

let private failed (profile: PdfProfile) (path: string) =
    VeraPdfValidator().Validate(path, profile).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

let private fix (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.RoleMap)

/// The role map of the PDF at path, as name → role.
let private roleMap (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot).GetAsDictionary PdfName.RoleMap with
    | null -> Map.empty
    | map -> Map.ofList [ for key in map.KeySet() -> key.GetValue(), map.GetAsName(key).GetValue() ]

[<Fact>]
let ``a tag name says which standard tag it is, as writers name them`` () =
    [ "Heading 1"; "Heading1"; "H1Title"; "heading_3"; "Überschrift 2"; "Heading10"; "Title"; "Body Text"; "Normal"; "Para"
      "Bullet"; "List Paragraph"; "Caption"; "Footnote"; "Image"; "InlineShape"; "TableCell"; "TOC 2"; "figure"; "Zorp"; "Header" ]
    |> List.map RoleMaps.guess
    |> should
        equal
        [ Some "H1"; Some "H1"; Some "H1"; Some "H3"; Some "H2"; None; Some "H1"; Some "P"; Some "P"; Some "P"
          Some "LI"; Some "LI"; Some "Caption"; Some "Note"; Some "Figure"; Some "Figure"; Some "TD"; Some "TOCI"; Some "Figure"; None; None ]

[<Fact>]
let ``names that say what they are are mapped, and the PDF passes`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTagged input.Path PdfVersion.PDF_1_7 [ "Heading 1", "Garden notes"; "Body Text", "The garden opens at 8."; "Body Text", "Bring gloves." ] []

    let result = fix input.Path output.Path

    failed PdfProfile.PdfUa1 input.Path |> should contain "7.1-5"
    failed PdfProfile.PdfUa1 output.Path |> should not' (contain "7.1-5")
    roleMap output.Path |> should equal (Map.ofList [ "Body Text", "P"; "Heading 1", "H1" ])
    result.AppliedFixes |> should contain "Mapped the tag name \"Body Text\" (2 elements) to P, which it says it is: to check."
    result.AppliedFixes |> should contain "Mapped the tag name \"Heading 1\" (1 element) to H1, which it says it is: to check."

[<Fact>]
let ``a name that doesn't say is left alone, and reported for a person to map`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTagged input.Path PdfVersion.PDF_1_7 [ "Heading 1", "Garden notes"; "Zorp", "The garden opens at 8." ] []

    let result = fix input.Path output.Path

    roleMap output.Path |> should equal (Map.ofList [ "Heading 1", "H1" ])
    failed PdfProfile.PdfUa1 output.Path |> should contain "7.1-5"
    result.NotApplied
    |> should contain "The tag name \"Zorp\" (1 element) isn't mapped to a standard tag, and doesn't say which it is: map it in a PDF editor's role map."

[<Fact>]
let ``a name mapped to one that isn't mapped has that one mapped, by what it says`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // Zorp → Image: Image says it's a Figure, so Zorp ends at Figure too.
    createTagged input.Path PdfVersion.PDF_1_7 [ "Zorp", "A photo of the garden." ] [ "Zorp", "Image" ]

    fix input.Path output.Path |> ignore

    roleMap output.Path |> should equal (Map.ofList [ "Image", "Figure"; "Zorp", "Image" ])
    failed PdfProfile.PdfUa1 output.Path |> should not' (contain "7.1-5")

[<Fact>]
let ``a circle and a standard tag mapped are mended`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // Heading 2 → Body Text → Heading 2, and P → H1.
    createTagged
        input.Path
        PdfVersion.PDF_1_7
        [ "Heading 2", "Opening hours"; "Body Text", "The garden opens at 8."; "P", "Bring gloves." ]
        [ "Heading 2", "Body Text"; "Body Text", "Heading 2"; "P", "H1" ]

    let result = fix input.Path output.Path

    let before = failed PdfProfile.PdfUa1 input.Path
    before |> should contain "7.1-6"
    before |> should contain "7.1-7"
    let after = failed PdfProfile.PdfUa1 output.Path
    after |> Set.filter (fun rule -> rule = "7.1-5" || rule = "7.1-6" || rule = "7.1-7") |> should be Empty
    roleMap output.Path |> should equal (Map.ofList [ "Body Text", "P"; "Heading 2", "H2" ])
    result.AppliedFixes |> should contain "Removed the role map's mapping of the standard tag P, which means itself."
    result.AppliedFixes |> should contain "Broke the role map's circle through \"Body Text\", \"Heading 2\"."

[<Fact>]
let ``a mapping says every element it serves, whichever name reached it first`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // Zorp (2) and Blip (3) both map to Image, which isn't mapped.
    createTagged
        input.Path
        PdfVersion.PDF_1_7
        [ "Zorp", "One."; "Zorp", "Two."; "Blip", "Three."; "Blip", "Four."; "Blip", "Five." ]
        [ "Zorp", "Image"; "Blip", "Image" ]

    let result = fix input.Path output.Path

    result.AppliedFixes |> should contain "Mapped the tag name \"Image\" (5 elements) to Figure, which it says it is: to check."

[<Fact>]
let ``two circles are each said`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTagged
        input.Path
        PdfVersion.PDF_1_7
        [ "Heading 2", "Hours."; "Body Text", "Open at 8."; "Figure Caption", "A photo."; "Footnote Text", "1. Weather allowing." ]
        [ "Heading 2", "Body Text"; "Body Text", "Heading 2"; "Figure Caption", "Footnote Text"; "Footnote Text", "Figure Caption" ]

    let result = fix input.Path output.Path

    result.AppliedFixes |> should contain "Broke the role map's circle through \"Body Text\", \"Heading 2\"."
    result.AppliedFixes |> should contain "Broke the role map's circle through \"Figure Caption\", \"Footnote Text\"."
    failed PdfProfile.PdfUa1 output.Path |> Set.filter (fun rule -> rule.StartsWith "7.1-") |> Set.intersect (set [ "7.1-5"; "7.1-6"; "7.1-7" ]) |> should be Empty

[<Fact>]
let ``a PDF 2.0 file's names are mapped too, for PDF/UA-2`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTagged input.Path PdfVersion.PDF_2_0 [ "Heading 1", "Garden notes"; "Body Text", "The garden opens at 8." ] []

    VeraPdfValidator().Fix(input.Path, PdfProfile.PdfUa2, output.Path, PdfFixOptions.RoleMap) |> ignore

    failed PdfProfile.PdfUa2 input.Path |> should contain "8.2.4-1"
    failed PdfProfile.PdfUa2 output.Path |> Set.filter (fun rule -> rule.StartsWith "8.2.4") |> should be Empty

[<Fact>]
let ``a role map that's right is left as it is`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTagged input.Path PdfVersion.PDF_1_7 [ "Heading 1", "Garden notes" ] [ "Heading 1", "H1" ]

    let result = fix input.Path output.Path

    result.OutputPath |> should equal null
    FixNames.nameOf PdfFixOptions.RoleMap |> should equal "role-map"
    FixNames.safe.HasFlag PdfFixOptions.RoleMap |> should be False
