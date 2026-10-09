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

/// A page where each row is one paragraph and its cells are marked-content pieces in that paragraph.
let private createParagraphRows (path: string) (texts: string list list) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString "en-US") |> ignore
    let page = document.AddNewPage PageSize.LETTER
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let pointer = document.GetTagStructureContext().GetAutoTaggingPointer().SetPageForTagging page
    for row, values in List.indexed texts do
        pointer.AddTag "P" |> ignore
        for column, text in List.indexed values do
            pointer.AddTag "Span" |> ignore
            canvas.OpenTag(pointer.GetTagReference()).BeginText().SetFontAndSize(font, 11f).MoveText(80.0 + 150.0 * float column, 680.0 - 24.0 * float row).ShowText(text).EndText().CloseTag()
            |> ignore
            pointer.MoveToParent() |> ignore
        pointer.MoveToParent() |> ignore

/// The first row has one Span across both columns, so moving it whole would lose its cell split.
let private createWideSpanRow (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString "en-US") |> ignore
    let page = document.AddNewPage PageSize.LETTER
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let pointer = document.GetTagStructureContext().GetAutoTaggingPointer().SetPageForTagging page
    pointer.AddTag "P" |> ignore
    pointer.AddTag "Span" |> ignore
    for column, text in [ 0, "Plant"; 1, "Sow" ] do
        canvas.OpenTag(pointer.GetTagReference()).BeginText().SetFontAndSize(font, 11f).MoveText(80.0 + 150.0 * float column, 680.0).ShowText(text).EndText().CloseTag()
        |> ignore
    pointer.MoveToParent() |> ignore
    pointer.MoveToParent() |> ignore
    pointer.AddTag "P" |> ignore
    pointer.AddTag "Span" |> ignore
    canvas.OpenTag(pointer.GetTagReference()).BeginText().SetFontAndSize(font, 11f).MoveText(80.0, 656.0).ShowText("Tomato").EndText().CloseTag()
    |> ignore
    pointer.MoveToParent() |> ignore
    pointer.MoveToParent() |> ignore

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
    { Page = 1; Tag = "Table"; Box = table.Geometry |> notNull; Alt = null; Text = null; MergeWith = []; Retag = null; Exclude = false; Move = None; Cells = cells; Lang = null }

/// The rows of the table at path: each row's cells, as tag and text.
let private rows (path: string) =
    let table = VeraPdfValidator().ReadStructureTree path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")
    let rec content (node: StructureNode) =
        [ yield! Option.ofObj node.Content |> Option.toList
          for child in node.Children do yield! content child ]
    [ for row in table.Children -> row.Tag, [ for c in row.Children -> c.Tag, (content c |> String.concat " ") ] ]

[<Fact>]
let ``consecutive row paragraphs convert to a checked table and keep each cell's marked content`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    use unchanged = new TempPdf()
    createParagraphRows input.Path [ [ "Plant"; "Sow" ]; [ "Tomato"; "March" ] ]
    let paragraphs = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "P")
    let edit =
        { Page = 1
          Tag = "P"
          Box = paragraphs.Head.Geometry |> notNull
          Alt = null
          Text = null
          MergeWith = paragraphs.Tail |> List.map (fun paragraph -> paragraph.Geometry |> notNull)
          Retag = "Table"
          Exclude = false
          Move = None
          Cells = [ cell "TH" 0 0; cell "TH" 0 1; cell "TD" 1 0; cell "TD" 1 1 ]; Lang = null }
    ElementEdits.pending input.Path [ edit ] |> should equal 1

    let result = VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)

    result.AppliedFixes |> should contain "Converted 2 paragraph(s) on page 1 to a table with the checked rows and cells."
    rows output.Path
    |> should
        equal
        [ "TR", [ "TH", "Plant"; "TH", "Sow" ]
          "TR", [ "TD", "Tomato"; "TD", "March" ] ]
    ElementEdits.pending output.Path [ edit ] |> should equal 0
    let after = failed output.Path
    after |> Set.filter (fun rule -> rule.StartsWith "7.2-") |> should be Empty
    let again = VeraPdfValidator(Edits = [ edit ]).Fix(output.Path, PdfProfile.PdfUa1, unchanged.Path, PdfFixOptions.ApplyEdits)
    again.WasFixed |> should be False
    again.NotApplied
    |> Seq.exists (fun note -> note.Contains "they already make this table")
    |> should be True

[<Fact>]
let ``paragraph table conversion refuses paragraphs that are not consecutive siblings`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createParagraphRows input.Path [ [ "Plant"; "Sow" ]; [ "Tomato"; "March" ]; [ "Lettuce"; "April" ] ]
    let paragraphs = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "P")
    let edit =
        { Page = 1
          Tag = "P"
          Box = paragraphs.Head.Geometry |> notNull
          Alt = null
          Text = null
          MergeWith = [ paragraphs[2].Geometry |> notNull ]
          Retag = "Table"
          Exclude = false
          Move = None
          Cells = [ cell "TH" 0 0; cell "TH" 0 1; cell "TD" 1 0; cell "TD" 1 1 ]; Lang = null }

    ElementEdits.pending input.Path [ edit ] |> should equal 0
    let result = VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)

    result.NotApplied |> Seq.length |> should equal 1
    VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "P") |> List.length |> should equal 3

[<Fact>]
let ``paragraph table conversion refuses one span covering more than one cell`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createWideSpanRow input.Path
    let paragraphs = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "P")
    let edit =
        { Page = 1
          Tag = "P"
          Box = paragraphs.Head.Geometry |> notNull
          Alt = null
          Text = null
          MergeWith = paragraphs.Tail |> List.map (fun paragraph -> paragraph.Geometry |> notNull)
          Retag = "Table"
          Exclude = false
          Move = None
          Cells = [ cell "TH" 0 0; cell "TH" 0 1; cell "TD" 1 0; cell "TD" 1 1 ]; Lang = null }

    ElementEdits.pending input.Path [ edit ] |> should equal 0
    let result = VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)

    result.NotApplied |> Seq.exists (fun note -> note.Contains "isn't drawn within one cell of its row") |> should be True
    VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "P") |> List.length |> should equal 2

/// Row paragraphs, as some writers tag them: each cell's text in a Span, with an empty Span
/// (holding nothing) after it; actualText, if given, on the second row's paragraph.
/// Two paragraphs as rows, each cell's text in a span followed by an empty span. With perGlyph, the
/// second row's text is a span per glyph, each with the glyph as its /ActualText, as
/// summary_merge.v7.pdf's vesting table has it.
let private createParagraphRowsTagged (path: string) (actualText: string option) (perGlyph: bool) =
    use document = new PdfDocument(new PdfWriter(path))
    document.SetTagged() |> ignore
    document.GetCatalog().SetLang(PdfString "en-US") |> ignore
    let page = document.AddNewPage PageSize.LETTER
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let pointer = document.GetTagStructureContext().GetAutoTaggingPointer().SetPageForTagging page
    let draw (text: string) x y =
        canvas.OpenTag(pointer.GetTagReference()).BeginText().SetFontAndSize(font, 11f).MoveText(x, y).ShowText(text).EndText().CloseTag()
        |> ignore
    for row, values in List.indexed [ [ "Plant"; "Sow" ]; [ "Tomato"; "March" ] ] do
        pointer.AddTag "P" |> ignore
        if row = 1 then actualText |> Option.iter (fun text -> pointer.GetProperties().SetActualText text |> ignore)
        for column, text in List.indexed values do
            let x = 80.0 + 150.0 * float column
            let y = 680.0 - 24.0 * float row
            if perGlyph && row = 1 then
                let mutable offset = 0.0
                for glyph in text |> Seq.map string do
                    pointer.AddTag "Span" |> ignore
                    pointer.GetProperties().SetActualText glyph |> ignore
                    draw glyph (x + offset) y
                    offset <- offset + float (font.GetWidth(glyph, 11f))
                    pointer.MoveToParent() |> ignore
            else
                pointer.AddTag "Span" |> ignore
                draw text x y
                pointer.MoveToParent() |> ignore
            pointer.AddTag "Span" |> ignore
            pointer.MoveToParent() |> ignore
        pointer.MoveToParent() |> ignore

let private createParagraphRowsWithEmptySpans (path: string) (actualText: string option) =
    createParagraphRowsTagged path actualText false

let private conversionOf (path: string) =
    let paragraphs = VeraPdfValidator().ReadStructureTree path |> notNull |> nodes |> List.filter (fun n -> n.Tag = "P")
    { Page = 1
      Tag = "P"
      Box = paragraphs.Head.Geometry |> notNull
      Alt = null
      Text = null
      MergeWith = paragraphs.Tail |> List.map (fun paragraph -> paragraph.Geometry |> notNull)
      Retag = "Table"
      Exclude = false
      Move = None
      Cells = [ cell "TH" 0 0; cell "TH" 0 1; cell "TD" 1 0; cell "TD" 1 1 ]; Lang = null }

[<Fact>]
let ``empty spans in row paragraphs go with the content beside them, and don't stop the conversion`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createParagraphRowsWithEmptySpans input.Path None
    let edit = conversionOf input.Path
    ElementEdits.unapplied input.Path [ edit ] |> should be Empty

    let result = VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)

    result.AppliedFixes |> should contain "Converted 2 paragraph(s) on page 1 to a table with the checked rows and cells."
    rows output.Path
    |> should
        equal
        [ "TR", [ "TH", "Plant"; "TH", "Sow" ]
          "TR", [ "TD", "Tomato"; "TD", "March" ] ]

[<Fact>]
let ``a row paragraph whose replacement text says other than it draws isn't converted, and says why`` () =
    use input = new TempPdf()
    createParagraphRowsWithEmptySpans input.Path (Some "Tomatoes, in March")
    let notes = ElementEdits.unapplied input.Path [ conversionOf input.Path ]
    notes |> should haveLength 1
    notes.Head |> should haveSubstring "/ActualText"

[<Fact>]
let ``a row paragraph whose replacement text says only what it draws is converted, the text in its cells`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    // As summary_merge.v5.pdf's vesting table has it: the row's text, spaced differently.
    createParagraphRowsWithEmptySpans input.Path (Some " Tomato March    ")
    let edit = conversionOf input.Path
    ElementEdits.unapplied input.Path [ edit ] |> should be Empty

    let result = VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)

    result.AppliedFixes |> should contain "Converted 2 paragraph(s) on page 1 to a table with the checked rows and cells."
    rows output.Path
    |> should
        equal
        [ "TR", [ "TH", "Plant"; "TH", "Sow" ]
          "TR", [ "TD", "Tomato"; "TD", "March" ] ]

[<Fact>]
let ``a row paragraph tagged a glyph to a span, its replacement text what they draw, is converted`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createParagraphRowsTagged input.Path (Some " Tomato March    ") true
    let edit = conversionOf input.Path
    ElementEdits.unapplied input.Path [ edit ] |> should be Empty

    let result = VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)

    result.AppliedFixes |> should contain "Converted 2 paragraph(s) on page 1 to a table with the checked rows and cells."
    rows output.Path |> List.map (snd >> List.map fst) |> should equal [ [ "TH"; "TH" ]; [ "TD"; "TD" ] ]

/// path, rewritten with change made to each structure element's dictionary (and the page).
let private rewriteElements (path: string) (change: PdfDocument -> PdfDictionary -> unit) =
    let bytes = System.IO.File.ReadAllBytes path
    use document = new PdfDocument(new PdfReader(new System.IO.MemoryStream(bytes)), new PdfWriter(path))
    for i in 1 .. document.GetNumberOfPdfObjects() - 1 do
        match document.GetPdfObject i with
        | :? PdfDictionary as d when PdfName.StructElem.Equals(d.GetAsName PdfName.Type) -> change document d
        | _ -> ()

/// The second row's paragraph (the one actualText is given to), changed by change.
let private changeSecondRow (path: string) (change: PdfDictionary -> unit) =
    rewriteElements path (fun _ d ->
        if PdfName.P.Equals(d.GetAsName PdfName.S) && not (isNull (d.Get PdfName.ActualText)) then change d)

[<Fact>]
let ``replacement text with its words broken in other places than the drawn text's isn't taken as the same`` () =
    use input = new TempPdf()
    // "Tomato" | "March" drawn; "Tom atoMarch" has the same letters, but other words.
    createParagraphRowsWithEmptySpans input.Path (Some "Tom atoMarch")

    let notes = ElementEdits.unapplied input.Path [ conversionOf input.Path ]

    notes |> should haveLength 1
    notes.Head |> should haveSubstring "replacement text (/ActualText) other than the text it draws"

[<Fact>]
let ``a row whose content is referred to by marked-content references reads its drawn text, and converts`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createParagraphRowsWithEmptySpans input.Path (Some "Tomato March")
    // Each span's MCID as an /MCR dictionary, naming its page, rather than a number.
    rewriteElements input.Path (fun document d ->
        match d.Get PdfName.K with
        | :? PdfNumber as mcid ->
            let reference = PdfDictionary()
            reference.Put(PdfName.Type, PdfName.MCR) |> ignore
            reference.Put(PdfName.MCID, PdfNumber(mcid.IntValue())) |> ignore
            reference.Put(PdfName.Pg, document.GetPage(1).GetPdfObject()) |> ignore
            d.Put(PdfName.K, reference) |> ignore
        | _ -> ())
    let edit = conversionOf input.Path

    ElementEdits.unapplied input.Path [ edit ] |> should be Empty
    let result = VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)
    result.AppliedFixes |> should contain "Converted 2 paragraph(s) on page 1 to a table with the checked rows and cells."

[<Fact>]
let ``the refusal names what the paragraph says of its own, blank alt text says nothing, and it's known before the model`` () =
    use input = new TempPdf()
    use blank = new TempPdf()
    createParagraphRowsWithEmptySpans input.Path (Some "Tomato March")
    System.IO.File.Copy(input.Path, blank.Path, true)
    changeSecondRow input.Path (fun p -> p.Put(PdfName.Alt, PdfString "A row of the planting table") |> ignore)
    changeSecondRow blank.Path (fun p -> p.Put(PdfName.Alt, PdfString "") |> ignore)
    let boxes (path: string) = [ for p in (conversionOf path |> fun e -> e.Box :: e.MergeWith) -> p ]

    let notes = ElementEdits.unapplied input.Path [ conversionOf input.Path ]
    notes |> should haveLength 1
    notes.Head |> should haveSubstring "alt text (/Alt)"
    // The same, before any cells are found.
    ElementEdits.paragraphTableRefusal input.Path (boxes input.Path) |> should equal (Some "a paragraph has alt text (/Alt), which can't be split among cells")
    ElementEdits.unapplied blank.Path [ conversionOf blank.Path ] |> should be Empty
    ElementEdits.paragraphTableRefusal blank.Path (boxes blank.Path) |> should equal (None: string option)

[<Fact>]
let ``converted, no replacement text is left on the table`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createParagraphRowsWithEmptySpans input.Path (Some " Tomato March    ")
    let edit = conversionOf input.Path

    (VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)).WasFixed |> should be True

    use document = new PdfDocument(new PdfReader(output.Path))
    [ for i in 1 .. document.GetNumberOfPdfObjects() - 1 do
          match document.GetPdfObject i with
          | :? PdfDictionary as d when PdfName.StructElem.Equals(d.GetAsName PdfName.Type) && not (isNull (d.Get PdfName.ActualText)) ->
              d.GetAsName(PdfName.S).GetValue()
          | _ -> () ]
    |> should be Empty

[<Fact>]
let ``a grid without a row for each paragraph isn't converted, and says why`` () =
    use input = new TempPdf()
    createParagraphRowsWithEmptySpans input.Path None
    let notes = ElementEdits.unapplied input.Path [ { conversionOf input.Path with Cells = [ cell "TD" 0 0; cell "TD" 0 1 ] } ]
    notes |> should haveLength 1
    notes.Head |> should haveSubstring "a row for each paragraph"

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
    let edit = { Page = 1; Tag = "Table"; Box = box 70.0 620.0 380.0 700.0; Alt = null; Text = null; MergeWith = []; Retag = null; Exclude = false; Move = None; Cells = [ cell "TH" 0 0; cell "TD" 1 0 ]; Lang = null }
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
    let edit = { Page = 1; Tag = "Sect"; Box = inner.Geometry |> notNull; Alt = null; Text = null; MergeWith = []; Retag = "Div"; Exclude = false; Move = None; Cells = []; Lang = null }

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
    let edit = { Page = 1; Tag = "Table"; Box = box 70.0 620.0 380.0 700.0; Alt = null; Text = null; MergeWith = []; Retag = null; Exclude = false; Move = None; Cells = [ cell "TH" 0 0; cell "TD" 1 0 ]; Lang = null }
    let review = $"""{{ "version": 1, "edits": [ {System.Text.Json.JsonSerializer.Serialize(ReviewFile.ofElementEdit edit)} ] }}"""
    ReviewFile.problems review |> should be Empty
    ReviewFile.problems (review.Replace("\"TD\"", "\"P\"")) |> should equal [ "edits[0].cells[1] is a \"P\", not a header cell (TH) or a data cell (TD)." ]

[<Fact>]
let ``a table rebuilt with a language given it gets the language too`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    createBroken input.Path [ [ "Planta"; "Siembra" ]; [ "Tomate"; "Marzo" ] ]
    let table = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")
    let edit = { rebuildEdit table [ cell "TH" 0 0; cell "TH" 0 1; cell "TD" 1 0; cell "TD" 1 1 ] with Lang = "es" }

    VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits) |> ignore

    (VeraPdfValidator().ReadStructureTree output.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "Table")).Language |> should equal "es"
    ElementEdits.pending output.Path [ edit ] |> should equal 0
