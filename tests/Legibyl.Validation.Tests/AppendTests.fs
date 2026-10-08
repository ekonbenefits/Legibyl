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

[<Theory>]
[<InlineData(1)>]
[<InlineData(2)>]
let ``passing inputs with orphan parent entries are refused without guessing sibling order`` (count: int) =
    use folder = new Folder()
    let garden, made, orphan, joined = folder.File "garden.pdf", folder.File "made.pdf", folder.File "orphan.pdf", folder.File "packet.pdf"
    createInput garden "Garden" "en-US" "P"
    createInput made "Jardin" "es-ES" "P"
    do
        use document = new PdfDocument(new PdfReader(made), new PdfWriter(orphan))
        let root = rawRoot document
        let top =
            root.GetAsDictionary PdfName.K
            |> Option.ofObj
            |> Option.defaultWith (fun () -> root.GetAsArray(PdfName.K).GetAsDictionary 0)
        let kids = top.GetAsArray PdfName.K
        // Leave the parent pointers and reverse associations intact, as an ordinary stamp
        // does. Neither pointer tells us which sibling slot each removed tag occupied.
        for _ in 1 .. count do kids.Remove 1
        top.SetModified() |> ignore
    validator.Value.Validate(orphan, PdfProfile.PdfUa1).IsCompliant |> should equal true
    let original = File.ReadAllBytes orphan
    let outcome = run [ garden; orphan ] joined Append.Options.Default
    outcome.Joined |> should equal None
    outcome.After |> should equal None
    outcome.Inputs |> should be Empty
    let refusal = outcome.Refusals |> List.exactlyOne
    refusal.File |> should equal orphan
    Assert.Contains("parent tree entry", refusal.Reason)
    Assert.Contains("isn't in the tags", refusal.Reason)
    Assert.Contains("doesn't fix tags or guess where they belong", refusal.Reason)
    File.Exists joined |> should equal false
    File.ReadAllBytes orphan |> should equal original
    // A refusal must also leave an existing destination intact.
    File.WriteAllText(joined, "keep this existing output")
    (run [ garden; orphan ] joined Append.Options.Default).Joined |> should equal None
    File.ReadAllText joined |> should equal "keep this existing output"
    Assert.Throws<ArgumentException>(fun () -> Append.join [ garden; orphan ] joined Append.Options.Default |> ignore) |> ignore
    File.ReadAllText joined |> should equal "keep this existing output"

[<Fact>]
let ``an existing parent key with a missing MCID slot is refused before merging`` () =
    use folder = new Folder()
    let garden, made, broken = folder.File "garden.pdf", folder.File "made.pdf", folder.File "broken.pdf"
    createInput garden "Garden" "en-US" "P"
    createInput made "Jardin" "es-ES" "P"
    let arrayNumber, owner =
        use document = new PdfDocument(new PdfReader(made))
        let page = document.GetPage(1).GetPdfObject()
        let key = page.GetAsNumber(PdfName.StructParents).IntValue()
        let nums = rawRoot(document).GetAsDictionary(PdfName.ParentTree).GetAsArray PdfName.Nums
        let i = [ 0 .. 2 .. nums.Size() - 2 ] |> List.find (fun i -> nums.GetAsNumber(i).IntValue() = key)
        let parents = nums.GetAsArray(i + 1)
        parents.GetIndirectReference().GetObjNumber(), parents.GetAsDictionary(0).GetIndirectReference().ToString()
    // Change only the reverse entry, byte for byte, so iText cannot repair it while stamping.
    let text = System.Text.Encoding.Latin1.GetString(File.ReadAllBytes made)
    let pattern = $@"(?ms)^{arrayNumber} 0 obj\b.*?endobj"
    let text =
        System.Text.RegularExpressions.Regex.Replace(
            text, pattern,
            fun m -> m.Value.Replace(owner, "null".PadRight(owner.Length))
        )
    File.WriteAllBytes(broken, System.Text.Encoding.Latin1.GetBytes text)
    refused [ garden; broken ]
    |> List.map snd
    |> List.exactlyOne
    |> fun reason -> Assert.Contains("isn't tied to the tag holding it", reason)

[<Fact>]
let ``multiple forward references to one MCID are refused as ambiguous`` () =
    refusedWith (fun document ->
        let rec duplicate (element: PdfDictionary) =
            match element.Get PdfName.K with
            | :? PdfNumber as mcid ->
                element.Put(PdfName.K, array [ mcid; mcid ]) |> ignore
                element.SetModified() |> ignore
                true
            | :? PdfArray as kids ->
                [ 0 .. kids.Size() - 1 ]
                |> List.exists (fun i ->
                    match kids.GetAsDictionary i with
                    | null -> false
                    | child -> duplicate child)
            | :? PdfDictionary as child -> duplicate child
            | _ -> false
        duplicate (rawRoot document) |> should equal true)
    |> List.exactlyOne
    |> fun reason -> Assert.Contains("is listed twice in one tag", reason)

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

[<Fact>]
let ``text in a page's content that reads like the catalog's structure tree entry is left as it is`` () =
    use folder = new Folder()
    let garden, jardin, joined = folder.File "garden.pdf", folder.File "jardin.pdf", folder.File "packet.pdf"
    createInput (folder.File "g.pdf") "Garden" "en-US" "P"
    createInput jardin "Jardin" "es-ES" "P"
    // A comment in the page's content, written uncompressed, as the catalog's entry would read.
    let comment = "% /StructTreeRoot 1 0 R\n"
    do
        use document = new PdfDocument(new PdfReader(folder.File "g.pdf"), new PdfWriter(garden))
        let stream = document.GetPage(1).NewContentStreamAfter()
        stream.SetData(Text.Encoding.ASCII.GetBytes comment)
        stream.SetCompressionLevel CompressionConstants.NO_COMPRESSION
    let outcome = run [ garden; jardin ] joined Append.Options.Default
    outcome.Passes |> should equal true
    use document = new PdfDocument(new PdfReader(joined))
    let page = document.GetPage 1
    [ for i in 0 .. page.GetContentStreamCount() - 1 -> Text.Encoding.ASCII.GetString(page.GetContentStream(i).GetBytes()) ]
    |> should contain comment

[<Fact>]
let ``a named destination a page's own action goes to is renamed with the rest`` () =
    use folder = new Folder()
    let garden, jardin, joined = folder.File "garden.pdf", folder.File "jardin.pdf", folder.File "packet.pdf"
    createInput (folder.File "g.pdf") "Garden" "en-US" "P"
    createInput (folder.File "j.pdf") "Jardin" "es-ES" "P"
    decorate (folder.File "g.pdf") garden "Block" 2
    // The jardin's first page goes to its "top" as it opens (/AA /O).
    do
        use document = new PdfDocument(new PdfReader(folder.File "j.pdf"), new PdfWriter(folder.File "j2.pdf"))
        document.AddNamedDestination("top", array [ document.GetPage(2).GetPdfObject(); PdfName.Fit ])
        let action = PdfDictionary()
        action.Put(PdfName.S, PdfName.GoTo) |> ignore
        action.Put(PdfName.D, PdfString "top") |> ignore
        let events = PdfDictionary()
        events.Put(PdfName.O, action) |> ignore
        document.GetPage(1).GetPdfObject().Put(PdfName.AA, events) |> ignore
    File.Move(folder.File "j2.pdf", jardin)
    let outcome = run [ garden; jardin ] joined Append.Options.Default
    outcome.Passes |> should equal true
    use document = new PdfDocument(new PdfReader(joined))
    let destination = document.GetPage(3).GetPdfObject().GetAsDictionary(PdfName.AA).GetAsDictionary(PdfName.O).GetAsString(PdfName.D).ToUnicodeString()
    destination |> should equal "top-2"
    let target = document.GetCatalog().GetNameTree(PdfName.Dests).GetNames()[PdfString destination] :?> PdfArray
    document.GetPageNumber(target.GetAsDictionary 0) |> should equal 4

[<Fact>]
let ``a file already where the joined one goes is replaced only once it's written and checked, with nothing left beside it`` () =
    use folder = new Folder()
    let garden, jardin, joined = folder.File "garden.pdf", folder.File "jardin.pdf", folder.File "packet.pdf"
    createInput garden "Garden" "en-US" "P"
    createInput jardin "Jardin" "es-ES" "P"
    File.WriteAllText(joined, "an older packet")
    let outcome = run [ garden; jardin ] joined Append.Options.Default
    outcome.Passes |> should equal true
    outcome.Joined.Value.Output |> should equal joined
    outcome.After.Value.FilePath |> should equal joined
    File.ReadAllText(joined).StartsWith "%PDF" |> should equal true
    Directory.GetFiles(Path.GetDirectoryName joined |> nonNull)
    |> Array.map (Path.GetFileName >> string)
    |> Array.filter (fun name -> name.StartsWith ".")
    |> should be Empty

// ---- From the cold review ----

/// The same file changed by change, written to output.
let private changed (input: string) (output: string) (change: PdfDocument -> unit) =
    use document = new PdfDocument(new PdfReader(input), new PdfWriter(output))
    change document

[<Fact>]
let ``the first file's Document language doesn't stay on the joined Document, where a later part would take it`` () =
    use folder = new Folder()
    let garden, jardin, joined = folder.File "garden.pdf", folder.File "jardin.pdf", folder.File "packet.pdf"
    createInput (folder.File "g.pdf") "Garden" "en-US" "P"
    createInput jardin "Jardin" "es-ES" "P"
    // English said on the garden's Document element too, as Acrobat can.
    changed (folder.File "g.pdf") garden (fun document ->
        let root = rawRoot document
        let documentElement = root.GetAsDictionary PdfName.K |> Option.ofObj |> Option.defaultWith (fun () -> root.GetAsArray(PdfName.K).GetAsDictionary 0)
        documentElement.Put(PdfName.Lang, PdfString "en-US") |> ignore)
    let outcome = run [ garden; jardin ] joined { Append.Options.Default with Language = Some "es-ES" }
    outcome.Passes |> should equal true
    let root = validator.Value.ReadStructureTree joined |> notNull
    let documentNode = root.Children |> Seq.find (fun child -> child.Tag = "Document")
    Option.ofObj documentNode.Language |> should equal None
    // The Spanish part takes the document's Spanish; the English one says it's English.
    parts joined |> List.map (fun part -> Option.ofObj part.Language) |> should equal [ Some "en-US"; None ]

[<Fact>]
let ``a named destination nothing in its file goes to is carried over, to its page`` () =
    use folder = new Folder()
    let garden, jardin, joined = folder.File "garden.pdf", folder.File "jardin.pdf", folder.File "packet.pdf"
    createInput (folder.File "g.pdf") "Garden" "en-US" "P"
    createInput (folder.File "j.pdf") "Jardin" "es-ES" "P"
    // "chapter", for links from elsewhere (packet.pdf#nameddest=chapter): page 2, and page 1.
    let chapter (page: int) (document: PdfDocument) =
        document.AddNamedDestination("chapter", array [ document.GetPage(page).GetPdfObject(); PdfName.XYZ; PdfNumber 0; PdfNumber 700; PdfNull.PDF_NULL ])
    changed (folder.File "g.pdf") garden (chapter 2)
    changed (folder.File "j.pdf") jardin (chapter 1)
    let outcome = run [ garden; jardin ] joined Append.Options.Default
    outcome.Passes |> should equal true
    use document = new PdfDocument(new PdfReader(joined))
    let names = document.GetCatalog().GetNameTree(PdfName.Dests).GetNames()
    [ for KeyValue(name, destination) in names ->
          let destination = destination :?> PdfArray
          name.ToUnicodeString(), document.GetPageNumber(destination.GetAsDictionary 0), destination.GetAsName(1).GetValue() ]
    |> List.sort
    |> should equal [ "chapter", 2, "XYZ"; "chapter-2", 3, "XYZ" ]

[<Fact>]
let ``a named destination with letters beyond ASCII both files have is renamed`` () =
    use folder = new Folder()
    let garden, jardin, joined = folder.File "garden.pdf", folder.File "jardin.pdf", folder.File "packet.pdf"
    createInput (folder.File "g.pdf") "Garden" "en-US" "P"
    createInput (folder.File "j.pdf") "Jardin" "es-ES" "P"
    let café (document: PdfDocument) =
        document.GetCatalog().GetNameTree(PdfName.Dests).AddEntry(PdfString("Café", iText.IO.Font.PdfEncodings.UNICODE_BIG), array [ document.GetPage(2).GetPdfObject(); PdfName.Fit ])
        document.GetOutlines(false).AddOutline("Café").GetContent().Put(PdfName.Dest, PdfString("Café", iText.IO.Font.PdfEncodings.UNICODE_BIG)) |> ignore
    changed (folder.File "g.pdf") garden café
    changed (folder.File "j.pdf") jardin café
    let outcome = run [ garden; jardin ] joined Append.Options.Default
    outcome.Passes |> should equal true
    use document = new PdfDocument(new PdfReader(joined))
    [ for KeyValue(name, destination) in document.GetCatalog().GetNameTree(PdfName.Dests).GetNames() ->
          name.ToUnicodeString(), document.GetPageNumber((destination :?> PdfArray).GetAsDictionary 0) ]
    |> List.sort
    |> should equal [ "Café", 2; "Café-2", 4 ]

[<Fact>]
let ``PDF 2.0 files whose namespace maps a tag differently are refused`` () =
    use folder = new Folder()
    // The same namespace in each, mapping <Box> to a paragraph in one and a heading in the other.
    let withNamespace (output: string) (role: string) =
        changed (corpusFile "pdfua2-pass.pdf") output (fun document ->
            let root = rawRoot document
            let roles = PdfDictionary()
            roles.Put(PdfName "Box", PdfName role) |> ignore
            let ns = PdfDictionary()
            ns.Put(PdfName.Type, PdfName.Namespace) |> ignore
            ns.Put(PdfName.NS, PdfString "https://example.org/ns") |> ignore
            ns.Put(PdfName.RoleMapNS, roles) |> ignore
            let list = match root.GetAsArray PdfName.Namespaces with null -> PdfArray() | list -> list
            list.Add(ns.MakeIndirect document)
            root.Put(PdfName.Namespaces, list) |> ignore)
    withNamespace (folder.File "one.pdf") "P"
    withNamespace (folder.File "two.pdf") "H1"
    withNamespace (folder.File "same.pdf") "P"
    refused [ folder.File "one.pdf"; folder.File "two.pdf" ]
    |> should
        equal
        [ Some "two.pdf",
          "Its namespace https://example.org/ns maps its tags differently from one.pdf's, so its tags would read as that file's do; files like that aren't joined yet." ]
    Append.refusals [ folder.File "one.pdf"; folder.File "same.pdf" ] |> should be Empty

[<Fact>]
let ``an encrypted file is refused, saying so, before anything is validated`` () =
    use folder = new Folder()
    createInput (folder.File "garden.pdf") "Garden" "en-US" "P"
    createInput (folder.File "j.pdf") "Jardin" "es-ES" "P"
    // Encrypted, by its trailer naming the standard security handler (iText needs its
    // cryptography to make or open one, which Legibyl doesn't ship).
    do
        let text = Text.Encoding.Latin1.GetString(File.ReadAllBytes(folder.File "j.pdf"))
        let key = String('0', 64)
        let encrypt = $"trailer\n<</Encrypt <</Filter /Standard /V 1 /R 2 /O <{key}> /U <{key}> /P -4>>"
        let at = text.LastIndexOf "trailer\n<<"
        at |> should be (greaterThan 0)
        File.WriteAllBytes(folder.File "jardin.pdf", Text.Encoding.Latin1.GetBytes(text.Substring(0, at) + encrypt + text.Substring(at + "trailer\n<<".Length)))
    Append.refusals [ folder.File "garden.pdf"; folder.File "jardin.pdf" ]
    |> List.map (fun refusal -> Option.ofObj refusal.File |> Option.map Path.GetFileName |> Option.defaultValue "", refusal.Reason)
    |> should equal [ "jardin.pdf", "It's encrypted: save a copy without a password, then join that." ]

[<Fact>]
let ``tags that loop back on themselves are refused, not walked forever`` () =
    refusedWith (fun document ->
        let root = rawRoot document
        let top =
            root.GetAsDictionary PdfName.K
            |> Option.ofObj
            |> Option.defaultWith (fun () -> root.GetAsArray(PdfName.K).GetAsDictionary 0)
        // The first tag below the top one holds the top one again.
        let first = top.GetAsArray(PdfName.K).GetAsDictionary 0
        let kids = PdfArray()
        kids.Add(first.Get(PdfName.K, false))
        kids.Add top
        first.Put(PdfName.K, kids) |> ignore
        first.SetModified() |> ignore)
    |> List.exactlyOne
    |> fun reason -> Assert.Contains("the tags loop back on themselves", reason)

[<Fact>]
let ``a parent tree entry with no value is refused as damaged`` () =
    use folder = new Folder()
    let garden, made, broken = folder.File "garden.pdf", folder.File "made.pdf", folder.File "broken.pdf"
    createInput garden "Garden" "en-US" "P"
    createInput made "Jardin" "es-ES" "P"
    let treeNumber =
        use document = new PdfDocument(new PdfReader(made))
        rawRoot(document).GetAsDictionary(PdfName.ParentTree).GetIndirectReference().GetObjNumber()
    // The tree's pairs made odd, byte for byte (so iText can't mend it): its last value blanked
    // out, "… 1 9 0 R]" becoming "… 1      ]".
    let text = System.Text.Encoding.Latin1.GetString(File.ReadAllBytes made)
    let tree = System.Text.RegularExpressions.Regex.Match(text, $@"(?ms)^{treeNumber} 0 obj\b.*?endobj").Value
    let nums = System.Text.RegularExpressions.Regex.Match(tree, @"/Nums\s*\[[^\]]*?(\d+\s+\d+\s+R)\s*\]")
    nums.Success |> should be True
    let last = nums.Groups[1]
    let odd = nums.Value.Substring(0, last.Index - nums.Index) + String(' ', last.Length) + nums.Value.Substring(last.Index - nums.Index + last.Length)
    File.WriteAllBytes(broken, System.Text.Encoding.Latin1.GetBytes(text.Replace(tree, tree.Replace(nums.Value, odd))))
    refused [ garden; broken ]
    |> List.map snd
    |> List.exactlyOne
    |> fun reason -> Assert.Contains("part of the parent tree is damaged", reason)
