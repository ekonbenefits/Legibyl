module Legibyl.Validation.Tests.TableRebuildTests

open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
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
