module Legibyl.Validation.Tests.IconTextTests

open Xunit
open FsUnit.Xunit
open iText.Kernel.Font
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Legibyl.Validation
open Legibyl.Validation.Tests.TestSupport

/// A tagged PDF 2.0 page whose paragraph reads "Call ☎ 555-0100", its ☎ an icon font's: the
/// letter X, which its font's ToUnicode map says is U+E001, a private-use character.
let private createIconText (path: string) =
    let plain = path + ".plain.pdf"
    do
        use document = new PdfDocument(new PdfWriter(plain, WriterProperties().SetPdfVersion PdfVersion.PDF_2_0))
        document.SetTagged() |> ignore
        document.GetCatalog().SetLang(PdfString "en-US") |> ignore
        let page = document.AddNewPage PageSize.LETTER
        let font = PdfFontFactory.CreateFont(corpusFile "fonts/Lato-Regular.ttf", iText.IO.Font.PdfEncodings.IDENTITY_H, PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED)
        let pointer = document.GetTagStructureContext().GetAutoTaggingPointer().SetPageForTagging page
        pointer.AddTag "P" |> ignore
        PdfCanvas(page).OpenTag(pointer.GetTagReference()).BeginText().SetFontAndSize(font, 14f).MoveText(72.0, 700.0).ShowText("Call X 555-0100").EndText().CloseTag()
        |> ignore
    do
        use document = new PdfDocument(new PdfReader(plain), new PdfWriter(path))
        let font =
            seq { for n in 1 .. document.GetNumberOfPdfObjects() - 1 -> document.GetPdfObject n }
            |> Seq.pick (function :? PdfDictionary as d when PdfName.Type0.Equals(d.GetAsName PdfName.Subtype) -> Some d | _ -> None)
        let map = ToUnicodeMaps.parse (System.Text.Encoding.Latin1.GetString(font.GetAsStream(PdfName.ToUnicode).GetBytes()))
        let icon = map |> Map.map (fun _ text -> if text = "X" then "" else text)
        font.Put(PdfName.ToUnicode, PdfStream(System.Text.Encoding.ASCII.GetBytes(ToUnicodeMaps.write 2 icon)).MakeIndirect document) |> ignore
    System.IO.File.Delete plain

let rec private nodes (node: StructureNode) : StructureNode list =
    node :: (node.Children |> Seq.collect nodes |> List.ofSeq)

let private failed (path: string) =
    VeraPdfValidator().Validate(path, PdfProfile.PdfUa2).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

[<Theory>]
[<InlineData("Call: phone 555-0100")>]
[<InlineData("")>]
let ``replacement text for text with icon characters satisfies PDF/UA-2, said or empty`` (text: string) =
    use input = new TempPdf()
    use output = new TempPdf()
    createIconText input.Path
    let paragraph = VeraPdfValidator().ReadStructureTree input.Path |> notNull |> nodes |> List.find (fun n -> n.Tag = "P")
    paragraph.Content |> should haveSubstring ""
    failed input.Path |> should contain "8.4.3-1"
    let edit = { Page = 1; Tag = "P"; Box = paragraph.Geometry |> notNull; Alt = null; Text = text; MergeWith = []; Retag = null; Exclude = false; Move = None; Cells = [] }

    VeraPdfValidator(Edits = [ edit ]).Fix(input.Path, PdfProfile.PdfUa2, output.Path, PdfFixOptions.ApplyEdits) |> ignore

    failed output.Path |> should not' (contain "8.4.3-1")
