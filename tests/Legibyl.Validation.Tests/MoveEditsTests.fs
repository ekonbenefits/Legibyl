module Legibyl.Validation.Tests.MoveEditsTests

open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private box (left, bottom, right, top) : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

// ---- The order moves give ----

/// Siblings A, B, C, D, one above another; each with its move.
let private reorder (moves: (string * ElementMove) list) =
    let sibling name i = name, Some(box (72.0, 700.0 - 50.0 * float i, 300.0, 720.0 - 50.0 * float i))
    let siblings = [ "A"; "B"; "C"; "D" ] |> List.mapi (fun i name -> sibling name i)
    let boxOf name = siblings |> List.find (fst >> (=) name) |> snd |> Option.get
    SiblingOrder.reorder [ for name, place in siblings -> name, place, moves |> List.tryFind (fst >> (=) name) |> Option.map snd ]
    |> String.concat ""
    |> fun order -> order, boxOf

[<Fact>]
let ``with no moves, siblings keep the order they were read in`` () =
    fst (reorder []) |> should equal "ABCD"

[<Fact>]
let ``a sibling moved first, or after another, goes there; the rest keep their order`` () =
    let _, boxOf = reorder []
    fst (reorder [ "C", First ]) |> should equal "CABD"
    fst (reorder [ "A", After(boxOf "C") ]) |> should equal "BCAD"
    // What Move Up on B then C gives: B first, A after it.
    fst (reorder [ "B", First; "A", After(boxOf "B") ]) |> should equal "BACD"

[<Fact>]
let ``a move whose sibling isn't there leaves it where it was`` () =
    fst (reorder [ "B", After(box (400.0, 100.0, 500.0, 120.0)) ]) |> should equal "ABCD"

// ---- Writing them ----

let private region tag bounds : LayoutRegion =
    { Tag = tag; Geometry = box bounds; ListNumbering = null; ActualText = null; QrText = null; Alt = null }

type private FixedLayout(regions: LayoutRegion list) =
    interface IPageLayoutSource with
        member _.PageRegions(_, _) = regions
        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = null

let private lines = [ "The masthead."; "The left column."; "The right column." ]

/// A tagged page of three paragraphs, one above another, tagged in the order drawn.
let private createTagged (path: string) =
    use untagged = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(untagged.Path))
        let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
        for i, text in List.indexed lines do
            canvas.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f).MoveText(72.0, 700.0 - 50.0 * float i).ShowText(text).EndText()
            |> ignore
    let layout = FixedLayout [ for i in 0..2 -> region "P" (70.0, 695.0 - 50.0 * float i, 300.0, 715.0 - 50.0 * float i) ]
    VeraPdfValidator(LayoutSource = layout).Fix(untagged.Path, PdfProfile.PdfUa1, path, PdfFixOptions.TagFromLayout) |> ignore

/// The paragraphs, in the order the structure reads them.
let private paragraphs (path: string) =
    let root = VeraPdfValidator().ReadStructureTree path |> notNull
    let rec all (node: StructureNode) : StructureNode list = node :: (node.Children |> Seq.collect all |> List.ofSeq)
    all root |> List.filter (fun n -> n.Tag = "P")

let private moveEdit (paragraph: StructureNode) (move: ElementMove) : ElementEdit =
    { Page = 1; Tag = "P"; Box = nonNull paragraph.Geometry; Alt = null; Text = null; MergeWith = []; Retag = null; Exclude = false; Move = Some move }

let private failedRules (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).FailedAssertions |> Seq.map _.RuleId |> Set.ofSeq

[<Fact>]
let ``a move reorders its parent's elements and nothing else: the page looks the same, and checks the same`` () =
    use input = new TempPdf()
    use output = new TempPdf ".moved"
    createTagged input.Path
    let masthead, left, right = match paragraphs input.Path with [ a; b; c ] -> a, b, c | other -> failwith $"{other.Length} paragraphs"

    // The right column first, then the masthead after it: right, masthead, left.
    let result =
        VeraPdfValidator(Edits = [ moveEdit right First; moveEdit masthead (After(nonNull right.Geometry)) ])
            .Fix(input.Path, PdfProfile.PdfUa1, output.Path, PdfFixOptions.ApplyEdits)

    result.AppliedFixes |> should contain "Put 3 element(s) in the reading order you gave them."
    paragraphs output.Path |> List.map _.Content |> should equal [ lines[2]; lines[0]; lines[1] ]
    failedRules output.Path |> should equal (failedRules input.Path)
    use renderer = new PdfRegionRenderer()
    let page = { PageNumber = 1; Left = 0.0; Bottom = 0.0; Right = 1.0; Top = 1.0 }
    renderer.RenderPageWithHighlight(output.Path, page).PngBytes
    |> should equal (renderer.RenderPageWithHighlight(input.Path, page).PngBytes)
    // Once written, it's in the PDF: nothing's left to apply.
    ElementEdits.pending output.Path [ moveEdit right First ] |> should equal 0

[<Fact>]
let ``a move to where it is already changes nothing, and says so`` () =
    use input = new TempPdf()
    createTagged input.Path
    let masthead = paragraphs input.Path |> List.head

    let edit = moveEdit masthead First

    ElementEdits.pending input.Path [ edit ] |> should equal 0
    ElementEdits.unapplied input.Path [ edit ] |> List.exactlyOne |> should haveSubstring "changed nothing: it's in that place in the reading order already"

[<Fact>]
let ``a review keeps a move, first or after a sibling (on its own page or another)`` () =
    let after = { box (72.0, 600.0, 300.0, 620.0) with PageNumber = 2 }
    let edits =
        [ { Page = 1; Tag = "P"; Box = box (72.0, 700.0, 300.0, 720.0); Alt = null; Text = null; MergeWith = []; Retag = null; Exclude = false; Move = Some First }
          { Page = 3; Tag = "H1"; Box = { box (72.0, 700.0, 300.0, 720.0) with PageNumber = 3 }; Alt = null; Text = null; MergeWith = []; Retag = null; Exclude = false; Move = Some(After after) } ]

    let back = edits |> List.map ReviewFile.ofElementEdit |> List.choose ReviewFile.toElementEdit

    back |> List.map _.Move |> should equal [ Some First; Some(After after) ]
