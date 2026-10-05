module Legibyl.Validation.Tests.TableRebuildTests

open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Tagutils
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private box left bottom right top : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

/// A tagged page whose table is tagged wrongly: its cells (TD, each drawing its text where rows
/// put it) sit right in the Table, with no rows. texts is its rows, top first.
let private createBrokenAs (cellTag: string) (path: string) (texts: string list list) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString "en-US") |> ignore
    let page = document.AddNewPage PageSize.LETTER
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let pointer = document.GetTagStructureContext().GetAutoTaggingPointer().SetPageForTagging page
    pointer.AddTag "Table" |> ignore
    for r, row in List.indexed texts do
        for c, text in List.indexed row do
            pointer.AddTag cellTag |> ignore
            canvas.OpenTag(pointer.GetTagReference()).BeginText().SetFontAndSize(font, 11f).MoveText(80.0 + 150.0 * float c, 680.0 - 24.0 * float r).ShowText(text).EndText().CloseTag()
            |> ignore
            pointer.MoveToParent() |> ignore
    pointer.MoveToParent() |> ignore

let private createBroken = createBrokenAs "TD"

let rec private nodes (node: StructureNode) : StructureNode list =
    node :: (node.Children |> Seq.collect nodes |> List.ofSeq)

let private failed (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

/// The cells of the grid: columns at 72 and 222, rows of 24 pt from 696 down.
let private cell tag row column = { Tag = tag; Box = box (72.0 + 150.0 * float column) (672.0 - 24.0 * float row) (222.0 + 150.0 * float column) (696.0 - 24.0 * float row) }

let private rebuildEdit (table: StructureNode) (cells: TableCell list) =
    { Page = 1; Tag = "Table"; Box = table.Geometry |> notNull; Alt = null; Text = null; MergeWith = []; Retag = null; Exclude = false; Move = None; Cells = cells }

/// The rows of the table at path: each row's cells, as tag and text.
let private rows (path: string) =
    let table = VeraPdfValidator().ReadStructureTree path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")
    [ for row in table.Children -> row.Tag, [ for c in row.Children -> c.Tag, (c.Children |> Seq.map _.Content |> Seq.append [ c.Content ] |> Seq.filter (not << isNull) |> String.concat " ") ] ]

[<Fact>]
let ``a table with cells outside rows is rebuilt as rows of the cells checked, its text in them, and passes`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createBroken input.Path [ [ "Plant"; "Sow" ]; [ "Tomato"; "March" ]; [ "Lettuce"; "April" ] ]
    let table = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")
    let before = failed input.Path
    before |> Set.filter (fun r -> r.StartsWith "7.2-") |> should not' (be Empty)
    let edit = rebuildEdit table [ cell "TH" 0 0; cell "TH" 0 1; cell "TD" 1 0; cell "TD" 1 1; cell "TD" 2 0; cell "TD" 2 1 ]
    ElementEdits.pending input.Path [ edit ] |> should equal 1

    let result = VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)

    result.AppliedFixes |> should contain "Rebuilt 1 table(s)' rows and cells from the page, as you checked them."
    rows output.Path
    |> should
        equal
        [ "TR", [ "TH", "Plant"; "TH", "Sow" ]
          "TR", [ "TD", "Tomato"; "TD", "March" ]
          "TR", [ "TD", "Lettuce"; "TD", "April" ] ]
    // The table rules that failed pass, and nothing new fails.
    let after = failed output.Path
    after |> Set.filter (fun r -> r.StartsWith "7.2-") |> should be Empty
    Set.difference after before |> should be Empty
    ElementEdits.pending output.Path [ edit ] |> should equal 0
    ElementEdits.unapplied output.Path [ edit ] |> List.exists (fun line -> line.EndsWith "changed nothing: its rows and cells are those already.") |> should be True

[<Fact>]
let ``a header over two columns spans them`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createBroken input.Path [ [ "Planting" ]; [ "Tomato"; "March" ] ]
    let table = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")
    let header = { Tag = "TH"; Box = box 72.0 672.0 372.0 696.0 }

    VeraPdfValidator(Edits = [ rebuildEdit table [ header; cell "TD" 1 0; cell "TD" 1 1 ] ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)
    |> ignore

    rows output.Path |> should equal [ "TR", [ "TH", "Planting" ]; "TR", [ "TD", "Tomato"; "TD", "March" ] ]
    use document = new PdfDocument(new PdfReader(output.Path))
    let rec find (element: PdfDictionary) : PdfDictionary list =
        let kids =
            match element.Get(PdfName.K, true) with
            | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do match array.Get(i, true) with :? PdfDictionary as d when not (isNull (d.Get PdfName.S)) -> d | _ -> () ]
            | :? PdfDictionary as d when not (isNull (d.Get PdfName.S)) -> [ d ]
            | _ -> []
        (if PdfName.TH.Equals(element.GetAsName PdfName.S) then [ element ] else []) @ List.collect find kids
    let th = find (document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot) |> List.exactlyOne
    let attributes = th.GetAsDictionary PdfName.A
    attributes.GetAsNumber(PdfName.ColSpan).IntValue() |> should equal 2
    attributes.GetAsName PdfName.Scope |> should equal PdfName.Column

[<Fact>]
let ``a rebuild is kept in a review, its cells and all`` () =
    let edit = { Page = 1; Tag = "Table"; Box = box 70.0 620.0 380.0 700.0; Alt = null; Text = null; MergeWith = []; Retag = null; Exclude = false; Move = None; Cells = [ cell "TH" 0 0; cell "TD" 1 0 ] }
    let saved = ReviewFile.ofElementEdit edit
    let json = System.Text.Json.JsonSerializer.Serialize saved
    json |> should haveSubstring "\"cells\""
    ReviewFile.toElementEdit (System.Text.Json.JsonSerializer.Deserialize<ReviewFile.SavedEdit> json |> notNull) |> Option.map _.Cells |> should equal (Some edit.Cells)

[<Fact>]
let ``a table tagged as paragraphs has them put in its cells, whole`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createBrokenAs "P" input.Path [ [ "Plant"; "Sow" ]; [ "Tomato"; "March" ] ]
    let table = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")
    let before = failed input.Path

    VeraPdfValidator(Edits = [ rebuildEdit table [ cell "TH" 0 0; cell "TH" 0 1; cell "TD" 1 0; cell "TD" 1 1 ] ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)
    |> ignore

    let table = VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")
    [ for row in table.Children -> [ for c in row.Children -> c.Tag, [ for p in c.Children -> p.Tag, p.Content ] ] ]
    |> should
        equal
        [ [ "TH", [ "P", "Plant" ]; "TH", [ "P", "Sow" ] ]
          [ "TD", [ "P", "Tomato" ]; "TD", [ "P", "March" ] ] ]
    let after = failed output.Path
    after |> Set.filter (fun r -> r.StartsWith "7.2-") |> should be Empty
    Set.difference after before |> should be Empty

/// The elements the structure tree reaches from its root, by object number.
let private reachable (document: PdfDocument) =
    let found = System.Collections.Generic.HashSet<int>()
    let rec visit (obj: PdfObject | null) =
        match obj with
        | :? PdfIndirectReference as reference ->
            if found.Add(reference.GetObjNumber()) then visit (reference.GetRefersTo())
        | :? PdfDictionary as dictionary when not (isNull (dictionary.Get PdfName.S)) || PdfName.StructTreeRoot.Equals(dictionary.GetAsName PdfName.Type) ->
            match dictionary.Get(PdfName.K, false) with
            | :? PdfArray as kids -> for i in 0 .. kids.Size() - 1 do visit (kids.Get(i, false))
            | kid -> visit kid
        | _ -> ()
    visit (document.GetCatalog().GetPdfObject().Get(PdfName.StructTreeRoot, false))
    found

/// The elements the parent tree says page 1's content belongs to: their object numbers and tags.
let private parentTreeEntries (document: PdfDocument) =
    let tree = document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot).GetAsDictionary PdfName.ParentTree
    let key = document.GetPage(1).GetPdfObject().GetAsNumber(PdfName.StructParents).IntValue()
    let rec find (node: PdfDictionary) : PdfArray option =
        match node.GetAsArray PdfName.Nums with
        | NonNull nums -> [ for i in 0..2 .. nums.Size() - 2 -> nums.GetAsNumber(i).IntValue(), nums.Get(i + 1) ] |> List.tryPick (fun (k, v) -> if k = key then (match v with :? PdfArray as a -> Some a | _ -> None) else None)
        | Null ->
            match node.GetAsArray PdfName.Kids with
            | NonNull kids -> [ for i in 0 .. kids.Size() - 1 -> kids.GetAsDictionary i ] |> List.tryPick find
            | Null -> None
    let entries = find tree |> Option.get
    [ for i in 0 .. entries.Size() - 1 do
          match entries.Get(i, false) with
          | :? PdfIndirectReference as reference -> reference.GetObjNumber(), (reference.GetRefersTo() :?> PdfDictionary).GetAsName(PdfName.S).GetValue()
          | _ -> () ]

[<Fact>]
let ``after a rebuild the parent tree points at the new cells, all in the tree`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createBroken input.Path [ [ "Plant"; "Sow" ]; [ "Tomato"; "March" ] ]
    let table = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")

    VeraPdfValidator(Edits = [ rebuildEdit table [ cell "TH" 0 0; cell "TH" 0 1; cell "TD" 1 0; cell "TD" 1 1 ] ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)
    |> ignore

    use document = new PdfDocument(new PdfReader(output.Path))
    let inTree = reachable document
    let entries = parentTreeEntries document
    entries |> List.map snd |> should equal [ "TH"; "TH"; "TD"; "TD" ]
    entries |> List.forall (fun (number, _) -> inTree.Contains number) |> should be True

[<Fact>]
let ``a table's caption stays the table's, before its rows`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(input.Path))
        document.SetTagged() |> ignore
        document.GetCatalog().SetLang(PdfString "en-US") |> ignore
        let page = document.AddNewPage PageSize.LETTER
        let canvas = PdfCanvas page
        let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
        let pointer = document.GetTagStructureContext().GetAutoTaggingPointer().SetPageForTagging page
        let tagged (tag: string) (x: float) (y: float) (text: string) =
            pointer.AddTag tag |> ignore
            canvas.OpenTag(pointer.GetTagReference()).BeginText().SetFontAndSize(font, 11f).MoveText(x, y).ShowText(text).EndText().CloseTag() |> ignore
            pointer.MoveToParent() |> ignore
        pointer.AddTag "Table" |> ignore
        tagged "Caption" 80.0 704.0 "Planting times"
        tagged "TD" 80.0 680.0 "Plant"
        tagged "TD" 230.0 680.0 "Sow"
        pointer.MoveToParent() |> ignore
    let table = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")

    VeraPdfValidator(Edits = [ rebuildEdit table [ cell "TH" 0 0; cell "TH" 0 1 ] ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits) |> ignore

    let rebuilt = VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")
    [ for kid in rebuilt.Children -> kid.Tag ] |> should equal [ "Caption"; "TR" ]
    (rebuilt.Children[0]).Content |> should equal "Planting times"
    rows output.Path |> List.tail |> should equal [ "TR", [ "TH", "Plant"; "TH", "Sow" ] ]
    // And it counts as rebuilt, caption and all.
    ElementEdits.pending output.Path [ rebuildEdit table [ cell "TH" 0 0; cell "TH" 0 1 ] ] |> should equal 0

[<Fact>]
let ``a rebuild's other changes are made too`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createBroken input.Path [ [ "Plant"; "Sow" ] ]
    let table = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")
    let edit = { rebuildEdit table [ cell "TH" 0 0; cell "TH" 0 1 ] with Alt = "When to sow" }

    VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits) |> ignore

    let rebuilt = VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")
    rebuilt.AlternateDescription |> should equal "When to sow"
    rows output.Path |> should equal [ "TR", [ "TH", "Plant"; "TH", "Sow" ] ]
    ElementEdits.pending output.Path [ edit ] |> should equal 0

[<Fact>]
let ``an edit for an element with a page is its, not a wrapper's without one around it`` () =
    use plain = new TempPdf()
    use input = new TempPdf()
    use output = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(plain.Path))
        document.SetTagged() |> ignore
        let page = document.AddNewPage PageSize.LETTER
        let pointer = document.GetTagStructureContext().GetAutoTaggingPointer().SetPageForTagging page
        pointer.AddTag("Sect").AddTag("Sect").AddTag "P" |> ignore
        PdfCanvas(page).OpenTag(pointer.GetTagReference()).BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 11f).MoveText(80.0, 680.0).ShowText("Open at 8.").EndText().CloseTag()
        |> ignore
    // The inner Sect names its page; the outer one doesn't.
    do
        use document = new PdfDocument(new PdfReader(plain.Path), new PdfWriter(input.Path))
        let rec sects (element: PdfDictionary) : PdfDictionary list =
            let kids =
                match element.Get(PdfName.K, true) with
                | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do match array.Get(i, true) with :? PdfDictionary as d when not (isNull (d.Get PdfName.S)) -> d | _ -> () ]
                | :? PdfDictionary as d when not (isNull (d.Get PdfName.S)) -> [ d ]
                | _ -> []
            (if PdfName.Sect.Equals(element.GetAsName PdfName.S) then [ element ] else []) @ List.collect sects kids
        match sects (document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot) with
        | [ outer; inner ] ->
            outer.Remove PdfName.Pg |> ignore
            inner.Put(PdfName.Pg, document.GetPage(1).GetPdfObject()) |> ignore
        | found -> failwith $"{found.Length} Sects"
    let inner = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "Sect") |> List.last
    let edit = { Page = 1; Tag = "Sect"; Box = inner.Geometry |> notNull; Alt = null; Text = null; MergeWith = []; Retag = "Div"; Exclude = false; Move = None; Cells = [] }

    VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits) |> ignore

    VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes |> List.map _.Tag |> List.filter (fun t -> t = "Sect" || t = "Div")
    |> should equal [ "Sect"; "Div" ]

/// A tagged page whose table is built by build (the tag pointer at the Table, and a function that
/// tags a text at a place with a tag).
let private createTable (path: string) (build: TagTreePointer -> (string -> float -> float -> string -> unit) -> unit) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString "en-US") |> ignore
    let page = document.AddNewPage PageSize.LETTER
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let pointer = document.GetTagStructureContext().GetAutoTaggingPointer().SetPageForTagging page
    let tagged (tag: string) (x: float) (y: float) (text: string) =
        pointer.AddTag tag |> ignore
        canvas.OpenTag(pointer.GetTagReference()).BeginText().SetFontAndSize(font, 11f).MoveText(x, y).ShowText(text).EndText().CloseTag() |> ignore
        pointer.MoveToParent() |> ignore
    pointer.AddTag "Table" |> ignore
    build pointer tagged

[<Fact>]
let ``a table that's one row of all its cells isn't taken for the grid rebuilt`` () =
    use input = new TempPdf()
    // A 2 × 2 table tagged as one row of four cells.
    createTable input.Path (fun pointer tagged ->
        pointer.AddTag "TR" |> ignore
        tagged "TD" 80.0 680.0 "Plant"
        tagged "TD" 230.0 680.0 "Sow"
        tagged "TD" 80.0 656.0 "Tomato"
        tagged "TD" 230.0 656.0 "March")
    let table = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")

    ElementEdits.pending input.Path [ rebuildEdit table [ cell "TD" 0 0; cell "TD" 0 1; cell "TD" 1 0; cell "TD" 1 1 ] ] |> should equal 1

[<Fact>]
let ``a table nested too deeply to follow is left, and said`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createTable input.Path (fun pointer tagged ->
        for _ in 1..70 do pointer.AddTag "TBody" |> ignore
        tagged "TD" 80.0 680.0 "Plant")
    let table = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")
    let edit = rebuildEdit table [ cell "TH" 0 0 ]

    let result = VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)

    result.OutputPath |> should equal null
    ElementEdits.unapplied input.Path [ edit ] |> List.exists (fun line -> line.EndsWith "its rows and cells are nested too deeply to follow.") |> should be True

[<Fact>]
let ``a table holding an annotation on another page is left`` () =
    use plain = new TempPdf()
    use input = new TempPdf()
    createTable plain.Path (fun _ tagged ->
        tagged "TD" 80.0 680.0 "Plant"
        tagged "TD" 230.0 680.0 "Sow")
    // A link on a second page, its reference put in the table's first cell.
    do
        use document = new PdfDocument(new PdfReader(plain.Path), new PdfWriter(input.Path))
        let second = document.AddNewPage PageSize.LETTER
        let link = iText.Kernel.Pdf.Annot.PdfLinkAnnotation(Rectangle(72f, 700f, 80f, 15f))
        second.AddAnnotation link |> ignore
        let rec cells (element: PdfDictionary) : PdfDictionary list =
            let kids =
                match element.Get(PdfName.K, true) with
                | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do match array.Get(i, true) with :? PdfDictionary as d when not (isNull (d.Get PdfName.S)) -> d | _ -> () ]
                | :? PdfDictionary as d when not (isNull (d.Get PdfName.S)) -> [ d ]
                | _ -> []
            (if PdfName.TD.Equals(element.GetAsName PdfName.S) then [ element ] else []) @ List.collect cells kids
        let first = cells (document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot) |> List.head
        let reference = PdfDictionary()
        reference.Put(PdfName.Type, PdfName.OBJR) |> ignore
        reference.Put(PdfName.Obj, link.GetPdfObject()) |> ignore
        let kids = match first.Get(PdfName.K, true) with :? PdfArray as a -> a | kid -> PdfArray([| kid |])
        kids.Add reference |> ignore
        first.Put(PdfName.K, kids) |> ignore
    let table = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")
    let edit = rebuildEdit table [ cell "TH" 0 0; cell "TH" 0 1 ]

    // Not rebuilt (holding something on another page, it isn't the table on page 1 alone), and said.
    ElementEdits.unapplied input.Path [ edit ] |> List.exists (fun line -> line.Contains "wasn't applied") |> should be True
    ElementEdits.pending input.Path [ edit ] |> should equal 0

[<Fact>]
let ``a review's rebuild is no problem, and a cell that isn't a TH or a TD is`` () =
    let edit = { Page = 1; Tag = "Table"; Box = box 70.0 620.0 380.0 700.0; Alt = null; Text = null; MergeWith = []; Retag = null; Exclude = false; Move = None; Cells = [ cell "TH" 0 0; cell "TD" 1 0 ] }
    let review = $"""{{ "version": 1, "edits": [ {System.Text.Json.JsonSerializer.Serialize(ReviewFile.ofElementEdit edit)} ] }}"""
    ReviewFile.problems review |> should be Empty
    ReviewFile.problems (review.Replace("\"TD\"", "\"P\"")) |> should equal [ "edits[0].cells[1] is a \"P\", not a header cell (TH) or a data cell (TD)." ]
