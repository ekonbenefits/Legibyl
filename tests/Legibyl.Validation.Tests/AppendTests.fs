module Legibyl.Validation.Tests.AppendTests

open System
open System.IO
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Action
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Navigation
open iText.Kernel.Pdf.Tagging
open iText.Kernel.Pdf.Tagutils
open iText.Kernel.Pdf.Xobject
open iText.Layout.Element
open iText.Pdfua
open Xunit
open FsUnit.Xunit
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private array (items: PdfObject list) = PdfArray(ResizeArray items)

let private font () =
    iText.Kernel.Font.PdfFontFactory.CreateFont(
        corpusFile (Path.Combine("fonts", "Lato-Regular.ttf")),
        iText.IO.Font.PdfEncodings.WINANSI,
        iText.Kernel.Font.PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED
    )

/// A two-page PDF/UA-1 file, as a pipeline or Legibyl would finish one: a heading, a custom
/// <Callout> mapped to callout, a link, a figure drawn by a Form XObject on the page, a figure
/// tagged inside a Form XObject of its own (an MCR to its stream), a second page, and an outline.
/// (Beside it, path + ".made.pdf": the file before that figure.)
let private createLongInput (path: string) (title: string) (language: string) (callout: string) (pages: int) =
    let made = path + ".made.pdf"
    do
        use document = new PdfUADocument(new PdfWriter(made), PdfUAConfig(PdfUAConformance.PDF_UA_1, title, language))
        document.GetStructTreeRoot().AddRoleMapping("Callout", callout)
        use layout = new iText.Layout.Document(document)
        layout.SetFont(font ()) |> ignore
        let heading = Paragraph title
        heading.GetAccessibilityProperties().SetRole StandardRoles.H1 |> ignore
        layout.Add heading |> ignore
        let note = Paragraph $"A note about {title}."
        note.GetAccessibilityProperties().SetRole "Callout" |> ignore
        layout.Add note |> ignore
        let link = Link("Visit the site", PdfAction.CreateURI $"https://example.org/{title}")
        link.GetLinkAnnotation().SetContents "The example site" |> ignore
        link.GetAccessibilityProperties().SetAlternateDescription "The example site" |> ignore
        layout.Add(Paragraph().Add link) |> ignore
        let square = PdfFormXObject(Rectangle(0f, 0f, 50f, 50f))
        PdfCanvas(square, document).Rectangle(5.0, 5.0, 40.0, 40.0).Fill() |> ignore
        let image = Image square
        image.GetAccessibilityProperties().SetAlternateDescription $"A black square for {title}" |> ignore
        layout.Add image |> ignore
        layout.Add(AreaBreak()) |> ignore
        layout.Add(Paragraph $"The second page about {title}.") |> ignore
        for page in 3..pages do
            layout.Add(AreaBreak()) |> ignore
            layout.Add(Paragraph $"Page {page} about {title}.") |> ignore
        document.GetOutlines(false).AddOutline(title).AddDestination(PdfExplicitDestination.CreateFit(document.GetPage 1))
    // Then a figure tagged inside a Form XObject: its own marked content, which /StructParents
    // ties to its tag. (iText's PDF/UA checker asks for the Do on the page to be tagged too,
    // which would tag it twice.)
    use document = new PdfDocument(new PdfReader(made), new PdfWriter(path))
    let circle = PdfFormXObject(Rectangle(0f, 0f, 60f, 60f))
    let pointer = TagTreePointer(document)
    pointer.SetPageForTagging(document.GetPage 1).SetContentStreamForTagging(circle.GetPdfObject()) |> ignore
    pointer.AddTag(StandardRoles.FIGURE).GetProperties().SetAlternateDescription $"A blue circle for {title}" |> ignore
    PdfCanvas(circle, document).OpenTag(pointer.GetTagReference()).SetFillColorRgb(0f, 0f, 1f).Circle(30.0, 30.0, 25.0).Fill().CloseTag() |> ignore
    pointer.MoveToParent().SetContentStreamForTagging(null) |> ignore
    PdfCanvas(document.GetPage 1).AddXObjectAt(circle, 400f, 400f) |> ignore

/// The same file with what two files can both have: a class (/ClassMap) defined as placement,
/// an ID on its heading, and a named destination "top" (to toPage) the outline goes to.
let private decorate (path: string) (output: string) (placement: string) (toPage: int) =
    use document = new PdfDocument(new PdfReader(path), new PdfWriter(output))
    let root = document.GetStructTreeRoot().GetPdfObject()
    let classMap = PdfDictionary()
    let layout = PdfDictionary()
    layout.Put(PdfName.O, PdfName "Layout") |> ignore
    layout.Put(PdfName "Placement", PdfName placement) |> ignore
    classMap.Put(PdfName "Big", layout) |> ignore
    root.Put(PdfName "ClassMap", classMap) |> ignore
    let documentElement = root.GetAsDictionary PdfName.K |> Option.ofObj |> Option.defaultWith (fun () -> root.GetAsArray(PdfName.K).GetAsDictionary 0)
    let heading = documentElement.GetAsArray(PdfName.K).GetAsDictionary 0
    heading.Put(PdfName.ID, PdfString "intro") |> ignore
    heading.Put(PdfName.C, PdfName "Big") |> ignore
    let ids = PdfDictionary()
    ids.Put(PdfName.Names, array [ PdfString "intro"; heading ]) |> ignore
    root.Put(PdfName "IDTree", ids) |> ignore
    document.AddNamedDestination("top", array [ document.GetPage(toPage).GetPdfObject(); PdfName.Fit ])
    document.GetOutlines(false).AddOutline("Top").GetContent().Put(PdfName.Dest, PdfString "top") |> ignore

let private createInput path title language callout = createLongInput path title language callout 2

/// A folder of inputs, deleted after.
type private Folder() =
    let path = Path.Combine(Path.GetTempPath(), $"legibyl-append-{Guid.NewGuid():N}")
    do Directory.CreateDirectory path |> ignore
    member _.File(name: string) = Path.Combine(path, name)

    interface IDisposable with
        member _.Dispose() = Directory.Delete(path, true)

let private validator = lazy (VeraPdfValidator())

let private run (files: string list) (output: string) (options: Append.Options) =
    Append.run validator.Value files output options ignore

let private failures (outcome: Append.Outcome) =
    outcome.After |> Option.map (fun after -> [ for f in after.FailedAssertions -> $"{f.Clause}-{f.TestNumber} {f.Message}" ]) |> Option.defaultValue []

/// Each page of output renders as its page in the input it came from, pixel for pixel.
let private rendersAsInputs (outcome: Append.Outcome) =
    use renderer = new Legibyl.Rendering.PdfRegionRenderer()
    let joined = outcome.Joined.Value
    let render (file: string) (page: int) =
        use document = new PdfDocument(new PdfReader(file))
        let box = document.GetPage(page).GetPageSize()
        renderer.RenderRegion(
            file,
            { PageNumber = page; Left = float (box.GetLeft()); Bottom = float (box.GetBottom()); Right = float (box.GetRight()); Top = float (box.GetTop()) },
            0.0,
            4096
        )
    for part in joined.Parts do
        for page in 1 .. part.Pages do
            render joined.Output (part.FirstPage + page - 1) |> should equal (render part.File page)

/// The joined file's structure: its Document's parts.
let private parts (file: string) =
    let root = validator.Value.ReadStructureTree file |> notNull
    let document = root.Children |> Seq.filter (fun child -> child.Tag <> "NotInTags") |> Seq.exactlyOne
    document.Tag |> should equal "Document"
    document.Children |> Seq.filter (fun child -> child.Tag <> "NotInTags") |> List.ofSeq

let private rawRoot (document: PdfDocument) = document.GetStructTreeRoot().GetPdfObject()

[<Fact>]
let ``files with links, figures drawn by Form XObjects and tags inside them join into one that passes, each a part read as it was`` () =
    use folder = new Folder()
    let garden, jardin, joined = folder.File "garden.pdf", folder.File "jardin.pdf", folder.File "packet.pdf"
    createInput garden "Garden" "en-US" "P"
    createInput jardin "Jardin" "es-ES" "H2"
    let outcome = run [ garden; jardin ] joined Append.Options.Default
    outcome.Refusals |> should be Empty
    outcome.Inputs |> List.iter (fun (_, report) -> report.IsCompliant |> should equal true)
    failures outcome |> should be Empty
    outcome.Differences |> should be Empty
    outcome.Passes |> should equal true
    let joinedParts = parts joined
    joinedParts |> List.map _.Tag |> should equal [ "Sect"; "Sect" ]
    // The second part says it's Spanish; the document is in English, as the first file was.
    joinedParts |> List.map (fun part -> Option.ofObj part.Language) |> should equal [ None; Some "es-ES" ]
    outcome.Joined.Value.Language |> should equal (Some "en-US")
    outcome.Joined.Value.Parts |> List.map (fun part -> part.FirstPage, part.Pages) |> should equal [ 1, 2; 3, 2 ]
    rendersAsInputs outcome

[<Fact>]
let ``a custom tag two files map differently is renamed in the later one, so each reads as it did`` () =
    use folder = new Folder()
    let garden, jardin, joined = folder.File "garden.pdf", folder.File "jardin.pdf", folder.File "packet.pdf"
    createInput garden "Garden" "en-US" "P"
    createInput jardin "Jardin" "es-ES" "H2"
    let outcome = run [ garden; jardin ] joined Append.Options.Default
    outcome.Joined.Value.Notes |> should contain "Renamed the tag <Callout> in jardin.pdf to <Callout-2>: an earlier file maps <Callout> to something else."
    use document = new PdfDocument(new PdfReader(joined))
    let roleMap = rawRoot(document).GetAsDictionary PdfName.RoleMap
    roleMap.GetAsName(PdfName "Callout") |> should equal PdfName.P
    roleMap.GetAsName(PdfName "Callout-2") |> should equal PdfName.H2

[<Fact>]
let ``a tag both files map the same way is left as it is`` () =
    use folder = new Folder()
    let garden, jardin, joined = folder.File "garden.pdf", folder.File "jardin.pdf", folder.File "packet.pdf"
    createInput garden "Garden" "en-US" "P"
    createInput jardin "Jardin" "es-ES" "P"
    let outcome = run [ garden; jardin ] joined Append.Options.Default
    outcome.Joined.Value.Notes |> should be Empty
    outcome.Passes |> should equal true

[<Fact>]
let ``classes, IDs and named destinations both files have are renamed in the later one, and the outline goes where each did`` () =
    use folder = new Folder()
    let garden, jardin, joined = folder.File "garden.pdf", folder.File "jardin.pdf", folder.File "packet.pdf"
    createInput (folder.File "g.pdf") "Garden" "en-US" "P"
    createInput (folder.File "j.pdf") "Jardin" "es-ES" "P"
    // "top" goes to the garden's second page, and to the jardin's first.
    decorate (folder.File "g.pdf") garden "Block" 2
    decorate (folder.File "j.pdf") jardin "Inline" 1
    let outcome = run [ garden; jardin ] joined Append.Options.Default
    failures outcome |> should be Empty
    outcome.Differences |> should be Empty
    outcome.Joined.Value.Notes
    |> should
        equal
        [ "Renamed the class Big in jardin.pdf to Big-2: an earlier file defines Big differently."
          "Renamed 1 element ID(s) in jardin.pdf: an earlier file has the same ones."
          "Renamed 1 named destination(s) in jardin.pdf: an earlier file has the same names." ]
    use document = new PdfDocument(new PdfReader(joined))
    let root = rawRoot document
    let classMap = root.GetAsDictionary(PdfName "ClassMap")
    classMap.GetAsDictionary(PdfName "Big").GetAsName(PdfName "Placement") |> should equal (PdfName "Block")
    classMap.GetAsDictionary(PdfName "Big-2").GetAsName(PdfName "Placement") |> should equal (PdfName "Inline")
    let ids = root.GetAsDictionary(PdfName "IDTree").GetAsArray PdfName.Names
    [ for i in 0..2 .. ids.Size() - 1 -> ids.GetAsString(i).ToUnicodeString() ] |> should equal [ "intro"; "p2-intro" ]
    // Each ID names its own file's heading, which uses its own class.
    let heading (i: int) = ids.GetAsDictionary(i + 1)
    heading(0).GetAsName PdfName.C |> should equal (PdfName "Big")
    heading(2).GetAsName PdfName.C |> should equal (PdfName "Big-2")
    // The outline: an entry for each part, going to its first page, with its own entries below.
    let pageOf (destination: PdfObject) =
        let named =
            match destination with
            | :? PdfString as name -> document.GetCatalog().GetNameTree(PdfName.Dests).GetNames()[name] :?> PdfArray
            | :? PdfArray as explicitDestination -> explicitDestination
            | _ -> failwith "no destination"
        document.GetPageNumber(named.GetAsDictionary 0)
    let entries =
        [ for part in document.GetOutlines(false).GetAllChildren() ->
              part.GetTitle(),
              pageOf (part.GetContent().Get PdfName.Dest),
              [ for own in part.GetAllChildren() -> own.GetTitle(), pageOf (own.GetContent().Get PdfName.Dest) ] ]
    entries |> should equal [ "Garden", 1, [ "Garden", 1; "Top", 2 ]; "Jardin", 3, [ "Jardin", 3; "Top", 3 ] ]
    rendersAsInputs outcome

[<Fact>]
let ``a long file, whose parent tree is nested (a number tree with kids), joins as one that passes`` () =
    use folder = new Folder()
    let garden, long, joined = folder.File "garden.pdf", folder.File "long.pdf", folder.File "packet.pdf"
    createInput garden "Garden" "en-US" "P"
    // iText writes a number tree of more than 40 entries as kids.
    createLongInput long "Jardin" "es-ES" "P" 45
    do
        use document = new PdfDocument(new PdfReader(long))
        rawRoot(document).GetAsDictionary(PdfName.ParentTree).GetAsArray PdfName.Kids |> should not' (be Null)
    let outcome = run [ garden; long ] joined Append.Options.Default
    outcome.Inputs |> List.iter (fun (_, report) -> report.IsCompliant |> should equal true)
    failures outcome |> should be Empty
    outcome.Differences |> should be Empty
    outcome.Joined.Value.Parts |> List.map (fun part -> part.FirstPage, part.Pages) |> should equal [ 1, 2; 3, 45 ]
    rendersAsInputs outcome

[<Fact>]
let ``PDF 2.0 files join as PDF/UA-2, each a DocumentFragment its outline entry goes to by structure`` () =
    use folder = new Folder()
    let joined = folder.File "packet.pdf"
    let outcome = run [ corpusFile "pdfua2-pass.pdf"; corpusFile "pdfua2-pass.pdf" ] joined Append.Options.Default
    failures outcome |> should be Empty
    outcome.Differences |> should be Empty
    outcome.Joined.Value.Profile |> should equal PdfProfile.PdfUa2
    parts joined |> List.map _.Tag |> should equal [ "DocumentFragment"; "DocumentFragment" ]
    use document = new PdfDocument(new PdfReader(joined))
    document.GetPdfVersion() |> should equal PdfVersion.PDF_2_0
    for entry in document.GetOutlines(false).GetAllChildren() do
        let target = entry.GetContent().GetAsDictionary(PdfName.A).GetAsArray(PdfName.SD).GetAsDictionary 0
        target.GetAsName PdfName.S |> should equal (PdfName "DocumentFragment")
    rendersAsInputs outcome

[<Fact>]
let ``the title, language and part tag can be given`` () =
    use folder = new Folder()
    let garden, jardin, joined = folder.File "garden.pdf", folder.File "jardin.pdf", folder.File "packet.pdf"
    createInput garden "Garden" "en-US" "P"
    createInput jardin "Jardin" "es-ES" "P"
    let outcome = run [ garden; jardin ] joined { Title = Some "The Packet"; Language = Some "es-ES"; PartTag = Some "Part" }
    outcome.Passes |> should equal true
    let joinedParts = parts joined
    joinedParts |> List.map _.Tag |> should equal [ "Part"; "Part" ]
    joinedParts |> List.map (fun part -> Option.ofObj part.Language) |> should equal [ Some "en-US"; None ]
    use document = new PdfDocument(new PdfReader(joined))
    document.GetDocumentInfo().GetTitle() |> should equal "The Packet"
    document.GetCatalog().GetPdfObject().GetAsString(PdfName.Lang).ToUnicodeString() |> should equal "es-ES"

[<Fact>]
let ``a review saved in a file isn't carried over, and the note says so`` () =
    use folder = new Folder()
    let garden, jardin, joined = folder.File "garden.pdf", folder.File "jardin.pdf", folder.File "packet.pdf"
    createInput (folder.File "g.pdf") "Garden" "en-US" "P"
    createInput jardin "Jardin" "es-ES" "P"
    EmbeddedReview.write (folder.File "g.pdf") garden """{"dismissed":[]}"""
    let outcome = run [ garden; jardin ] joined Append.Options.Default
    outcome.Joined.Value.Notes |> should equal [ "The review saved in garden.pdf wasn't carried over: its edits name pages by number." ]
    EmbeddedReview.read joined |> should equal None

// ---- Refusals: nothing is written ----

let private refused (files: string list) =
    use folder = new Folder()
    let joined = folder.File "packet.pdf"
    let outcome = run files joined Append.Options.Default
    outcome.Joined |> should equal None
    File.Exists joined |> should equal false
    outcome.Refusals |> List.map (fun refusal -> Option.ofObj refusal.File |> Option.map Path.GetFileName, refusal.Reason)

[<Fact>]
let ``a file that fails PDF/UA is refused, and nothing is written`` () =
    use folder = new Folder()
    createInput (folder.File "garden.pdf") "Garden" "en-US" "P"
    refused [ folder.File "garden.pdf"; corpusFile "pdfua1-fail.pdf" ]
    |> should equal [ Some "pdfua1-fail.pdf", "It doesn't pass PDF/UA-1: 2 checks fail." ]

[<Fact>]
let ``PDF 2.0 and earlier files together are refused`` () =
    use folder = new Folder()
    createInput (folder.File "garden.pdf") "Garden" "en-US" "P"
    refused [ folder.File "garden.pdf"; corpusFile "pdfua2-pass.pdf" ]
    |> List.map snd |> List.exactlyOne
    |> should
        equal
        ("PDF 2.0 files (PDF/UA-2) and earlier ones (PDF/UA-1) can't be joined into one: garden.pdf (PDF 1.7), pdfua2-pass.pdf (PDF 2.0).")

/// garden.pdf, changed by change, as the second of two files to join.
let private refusedWith (change: PdfDocument -> unit) =
    use folder = new Folder()
    createInput (folder.File "garden.pdf") "Garden" "en-US" "P"
    createInput (folder.File "j.pdf") "Jardin" "es-ES" "P"
    do
        use document = new PdfDocument(new PdfReader(folder.File "j.pdf"), new PdfWriter(folder.File "jardin.pdf"))
        change document
    refused [ folder.File "garden.pdf"; folder.File "jardin.pdf" ] |> List.map snd

[<Fact>]
let ``a form, optional content or an attached file is refused, saying why`` () =
    refusedWith (fun document ->
        let field = iText.Forms.Fields.PdfFormCreator.CreateTextFormField(document)
        ignore field
        let form = PdfDictionary()
        form.Put(PdfName.Fields, array [ PdfDictionary().MakeIndirect document ]) |> ignore
        document.GetCatalog().GetPdfObject().Put(PdfName.AcroForm, form) |> ignore)
    |> should equal [ "It has a form: field names can collide across the files, so forms aren't joined yet." ]
    refusedWith (fun document -> document.GetCatalog().GetPdfObject().Put(PdfName.OCProperties, PdfDictionary()) |> ignore)
    |> should equal [ "It has optional content (layers): their names and visibility would need merging, so it isn't joined yet." ]
    refusedWith (fun document ->
        document.AddFileAttachment("data.csv", iText.Kernel.Pdf.Filespec.PdfFileSpec.CreateEmbeddedFileSpec(document, "a,b"B, "data", "data.csv", null, null)))
    |> should equal [ "It has attached files, which aren't joined yet." ]

[<Fact>]
let ``an untagged file is refused`` () =
    refusedWith (fun document -> document.GetCatalog().GetPdfObject().Remove PdfName.StructTreeRoot |> ignore)
    |> should equal [ "It isn't tagged: it has no structure tree to keep." ]


[<Fact>]
let ``the joined file can't be written over one of the files`` () =
    use folder = new Folder()
    let garden, jardin = folder.File "garden.pdf", folder.File "jardin.pdf"
    createInput garden "Garden" "en-US" "P"
    createInput jardin "Jardin" "es-ES" "P"
    let before = File.ReadAllBytes jardin
    let outcome = run [ garden; jardin ] jardin Append.Options.Default
    outcome.Joined |> should equal None
    outcome.Refusals |> List.map _.Reason |> should equal [ "The joined file would be written over it: save the joined file somewhere else." ]
    File.ReadAllBytes jardin |> should equal before
