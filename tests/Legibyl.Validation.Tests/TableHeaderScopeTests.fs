module Legibyl.Validation.Tests.TableHeaderScopeTests

open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// An untagged page with a ruled table: a header row (Plant, Sow, Harvest) and two rows whose
/// first cells name the plant.
let private createTable (path: string) =
    use document = new PdfDocument(new PdfWriter(path))
    let page = document.AddNewPage PageSize.LETTER
    let canvas = PdfCanvas page
    let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
    let rows = [ [ "Plant"; "Sow"; "Harvest" ]; [ "Tomato"; "March"; "July" ]; [ "Lettuce"; "April"; "June" ] ]
    for r, row in List.indexed rows do
        for x, value in List.zip [ 80.0; 230.0; 380.0 ] row do
            canvas.BeginText().SetFontAndSize(font, 11f).MoveText(x, 680.0 - float r * 24.0).ShowText(value).EndText() |> ignore

let private region tag (left, bottom, right, top) : LayoutRegion =
    { Tag = tag
      Geometry = { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }
      ListNumbering = null
      ActualText = null
      QrText = null
      Alt = null }

/// The table's regions: the first row header cells, and with rowHeaders the first column too.
let private regions (rowHeaders: bool) =
    [ region "Table" (70.0, 624.0, 522.0, 698.0)
      for r in 0..2 do
          for c, (left, right) in List.indexed [ 72.0, 220.0; 220.0, 370.0; 370.0, 520.0 ] do
              let top = 696.0 - float r * 24.0
              region (if r = 0 || (rowHeaders && c = 0) then "TH" else "TD") (left, top - 24.0, right, top) ]

type private FixedLayout(regions: LayoutRegion list) =
    interface IPageLayoutSource with
        member _.PageRegions(_, _) = regions
        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = null

/// The table tagged from its layout, then each header cell's Scope taken away (and, with headers,
/// each data cell naming its headers instead).
let private taggedWithoutScope (path: string) (rowHeaders: bool) (headers: bool) =
    use untagged = new TempPdf()
    use tagged = new TempPdf()
    createTable untagged.Path
    VeraPdfValidator(LayoutSource = FixedLayout(regions rowHeaders)).Fix(untagged.Path, PdfProfile.PdfUa1, tagged.Path, PdfFixOptions.TagFromLayout) |> ignore
    use document = new PdfDocument(new PdfReader(tagged.Path), new PdfWriter(path))
    for n in 1 .. document.GetNumberOfPdfObjects() - 1 do
        match document.GetPdfObject n with
        | :? PdfDictionary as element when PdfName.StructElem.Equals(element.GetAsName PdfName.Type) ->
            match element.GetAsDictionary PdfName.A with
            | null -> ()
            | attributes ->
                attributes.Remove PdfName.Scope |> ignore
                if headers && PdfName.TD.Equals(element.GetAsName PdfName.S) then
                    attributes.Put(PdfName.Headers, PdfArray([ PdfString "h1" :> PdfObject ] |> System.Collections.Generic.List)) |> ignore
        | _ -> ()

/// Each row's cells, as their tag and scope ("" for none).
let private scopes (path: string) =
    use document = new PdfDocument(new PdfReader(path))
    let rec find (element: PdfDictionary) : PdfDictionary list =
        let kids =
            match element.Get(PdfName.K, true) with
            | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do match array.Get(i, true) with :? PdfDictionary as d -> d | _ -> () ]
            | :? PdfDictionary as d -> [ d ]
            | _ -> []
        (if PdfName.TR.Equals(element.GetAsName PdfName.S) then [ element ] else []) @ List.collect find kids
    let root = document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot
    [ for row in find root ->
          [ for i in 0 .. row.GetAsArray(PdfName.K).Size() - 1 ->
                let cell = row.GetAsArray(PdfName.K).GetAsDictionary i
                let scope = match cell.GetAsDictionary PdfName.A with null -> "" | a -> (match a.GetAsName PdfName.Scope with null -> "" | s -> s.GetValue())
                cell.GetAsName(PdfName.S).GetValue(), scope ] ]

let private failed (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

let private fix (input: string) (output: string) =
    VeraPdfValidator().Fix(input, PdfProfile.PdfUa1, output, PdfFixOptions.TableHeaderScope).AppliedFixes |> List.ofSeq

[<Fact>]
let ``a header row's cells head their columns`` () =
    use broken = new TempPdf()
    use fixedPdf = new TempPdf()
    taggedWithoutScope broken.Path false false

    let report = fix broken.Path fixedPdf.Path

    failed broken.Path |> should contain "7.5-1"
    failed fixedPdf.Path |> should not' (contain "7.5-1")
    scopes fixedPdf.Path |> List.head |> should equal [ "TH", "Column"; "TH", "Column"; "TH", "Column" ]
    report |> should contain "Gave 3 table header cell(s) in 1 table(s) a Scope (Column in a header row, Row for one heading its row), so a screen reader can tell which cells each heads: to check."

[<Fact>]
let ``a header cell that starts a row of data heads its row`` () =
    use broken = new TempPdf()
    use fixedPdf = new TempPdf()
    taggedWithoutScope broken.Path true false

    fix broken.Path fixedPdf.Path |> ignore

    failed fixedPdf.Path |> should not' (contain "7.5-1")
    scopes fixedPdf.Path
    |> should
        equal
        [ [ "TH", "Column"; "TH", "Column"; "TH", "Column" ]
          [ "TH", "Row"; "TD", ""; "TD", "" ]
          [ "TH", "Row"; "TD", ""; "TD", "" ] ]

[<Fact>]
let ``a table whose cells name their headers is left as it is, and the fix isn't a safe one`` () =
    use broken = new TempPdf()
    use fixedPdf = new TempPdf()
    taggedWithoutScope broken.Path false true

    let report = fix broken.Path fixedPdf.Path

    report |> List.exists (fun line -> line.Contains "Scope") |> should be False
    FixNames.nameOf PdfFixOptions.TableHeaderScope |> should equal "table-header-scope"
    FixNames.safe.HasFlag PdfFixOptions.TableHeaderScope |> should be False
