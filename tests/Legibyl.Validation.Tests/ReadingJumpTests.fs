module Legibyl.Validation.Tests.ReadingJumpTests

open Xunit
open FsUnit.Xunit
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

let private box (left, bottom, right, top) : StructureGeometry =
    { PageNumber = 1; Left = left; Bottom = bottom; Right = right; Top = top }

type private FixedLayout(regions: LayoutRegion list) =
    interface IPageLayoutSource with
        member _.PageRegions(_, _) = regions
        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = null

/// A page with text at each place, tagged as a paragraph each in the order given.
let private tagged (path: string) (blocks: (string * (float * float * float * float)) list) =
    use untagged = new TempPdf()
    do
        use document = new PdfDocument(new PdfWriter(untagged.Path))
        let canvas = PdfCanvas(document.AddNewPage PageSize.LETTER)
        let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
        for text, (left, bottom, right, top) in blocks do
            // A word in each corner of its place, so its text fills it.
            let width = float (font.GetWidth(text, 10f))
            for x, y in [ left + 2.0, bottom + 2.0; right - width - 2.0, top - 12.0 ] do
                canvas.BeginText().SetFontAndSize(font, 10f).MoveText(x, y).ShowText(text).EndText() |> ignore
    let regions =
        [ for _, bounds in blocks -> { Tag = "P"; Geometry = box bounds; ListNumbering = null; ActualText = null; QrText = null; Alt = null } ]
    VeraPdfValidator(LayoutSource = FixedLayout regions).Fix(untagged.Path, PdfProfile.PdfUa1, path, PdfFixOptions.TagFromLayout) |> ignore

let private jumps (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa1).Assertions
    |> Seq.filter (fun a -> a.Specification = "Legibyl" && a.TestNumber = 12)
    |> List.ofSeq

let private header = "Author or Publication Name", (60.0, 745.0, 190.0, 758.0)
let private footer = "Page 1", (290.0, 30.0, 320.0, 42.0)
let private masthead = "Today's news", (60.0, 620.0, 550.0, 720.0)
let private leftColumn = "The left column", (60.0, 100.0, 290.0, 580.0)
let private rightColumn = "The right column", (310.0, 100.0, 550.0, 580.0)

[<Fact>]
let ``a masthead read after the column under it is a jump, said once for its page`` () =
    use pdf = new TempPdf()
    tagged pdf.Path [ header; footer; leftColumn; masthead; rightColumn ]

    let found = jumps pdf.Path

    let jump = found |> List.exactlyOne
    jump.ErrorMessage |> should equal "Page 1: \"Today's news Today's news\" spans the page but comes after \"The left column The left column\", below it."
    jump.FixOptions |> should equal PdfFixOptions.None
    jump.Severity |> should equal FindingSeverity.Warning

[<Fact>]
let ``two columns in order aren't a jump, nor a running header or footer tagged anywhere`` () =
    use inOrder = new TempPdf()
    tagged inOrder.Path [ footer; masthead; leftColumn; rightColumn; header ]
    use singleColumn = new TempPdf()
    tagged singleColumn.Path [ "One column", (60.0, 100.0, 550.0, 700.0); header; footer ]

    jumps inOrder.Path |> should be Empty
    jumps singleColumn.Path |> should be Empty
