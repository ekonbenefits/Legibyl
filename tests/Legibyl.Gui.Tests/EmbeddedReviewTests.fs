module Legibyl.Gui.Tests.EmbeddedReviewTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open iText.Kernel.Pdf
open Legibyl.Gui
open Legibyl.Validation

let private logo: StructureGeometry = { PageNumber = 1; Left = 100.0; Bottom = 500.0; Right = 200.0; Top = 600.0 }

let private splitParagraphs = { Dismissals.Rule = "Legibyl Structure-2"; Dismissals.Location = Dismissals.AnyLocation }

/// A PDF (with an annotation but no tab order, for the AnnotationTabs fix to repair) in a fresh
/// temporary folder, and where its fixed copy goes.
let private withPdf (test: string -> string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try
        let pdf = Path.Combine(folder, "flyer.pdf")
        do
            use document = new PdfDocument(new PdfWriter(pdf))
            let annotations = PdfArray()
            annotations.Add(PdfDictionary())
            document.AddNewPage().GetPdfObject().Put(PdfName.Annots, annotations) |> ignore
        test pdf (Path.Combine(folder, "flyer.fixed.pdf"))
    finally
        Directory.Delete(folder, true)

/// Fixes pdf to output, carrying its review with dismissed in place of its saved dismissals.
let private fixCarrying (pdf: string) (output: string) (dismissed: Dismissals.Dismissed) =
    VeraPdfValidator(Review = RegionEdits.reviewFor pdf (Some dismissed)).Fix(pdf, PdfProfile.PdfUa1, output, PdfFixOptions.AnnotationTabs)
    |> ignore

[<Fact>]
let ``the fixed copy opens with the review it was fixed with, without a file next to it`` () =
    withPdf (fun pdf output ->
        RegionEdits.save pdf (RegionEdits.setAlt [] 1 "Figure" logo "The garden's logo")
        RegionEdits.saveTitle pdf (Some "Joining the garden")

        fixCarrying pdf output (Map [ splitParagraphs, "Likely split paragraph" ])

        File.Exists(Dismissals.filePath output) |> should be False
        Dismissals.load output |> should equal (Some(Map [ splitParagraphs, "Likely split paragraph" ]))
        RegionEdits.load output |> List.map _.Alt |> should equal [ Some "The garden's logo" ]
        RegionEdits.loadTitle output |> should equal (Some "Joining the garden"))

[<Fact>]
let ``a file next to the fixed copy wins over the review in it`` () =
    withPdf (fun pdf output ->
        fixCarrying pdf output (Map [ splitParagraphs, "Likely split paragraph" ])

        Dismissals.save output Map.empty

        Dismissals.load output |> should equal (Some(Map.empty: Dismissals.Dismissed)))

[<Fact>]
let ``forgetting the dismissals of a fixed copy keeps them from coming back from the review in it`` () =
    withPdf (fun pdf output ->
        fixCarrying pdf output (Map [ splitParagraphs, "Likely split paragraph" ])

        Dismissals.forget output

        Dismissals.load output |> should equal None)

[<Fact>]
let ``with no review at all, nothing is carried`` () =
    withPdf (fun pdf output ->
        Dismissals.reviewJson pdf (Some Map.empty) |> should equal (null: string | null)
        fixCarrying pdf output Map.empty
        EmbeddedReview.read output |> should equal None)

[<Fact>]
let ``edits can be imported from another PDF: from inside a saved copy, or the file next to one`` () =
    withPdf (fun pdf output ->
        RegionEdits.save pdf (RegionEdits.setAlt [] 1 "Figure" logo "The garden's logo")
        fixCarrying pdf output Map.empty

        // The saved copy carries them inside it...
        RegionEdits.fromFileOrPdf output |> List.map _.Alt |> should equal [ Some "The garden's logo" ]
        // ...and the original has them in the file next to it.
        RegionEdits.fromFileOrPdf pdf |> List.map _.Alt |> should equal [ Some "The garden's logo" ]
        // A .legibyl.json file still imports as before.
        RegionEdits.fromFileOrPdf (Dismissals.filePath pdf) |> List.map _.Alt |> should equal [ Some "The garden's logo" ])

/// Regions as Suggest Structure would give them, with edits made there applied.
type private FixedLayout(regions: LayoutRegion list) =
    interface IPageLayoutSource with
        member _.PageRegions(_, _) = regions
        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = null

[<Fact>]
let ``a copy tagged from the layout carries its edits as edits of the elements tagging made`` () =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try
        let pdf = Path.Combine(folder, "garden.pdf")
        let output = Path.Combine(folder, "garden.v2.pdf")
        do
            use document = new PdfDocument(new PdfWriter(pdf))
            let canvas = iText.Kernel.Pdf.Canvas.PdfCanvas(document.AddNewPage())
            let font = iText.Kernel.Font.PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA
            for y, text in [ 720.0, "Joining the garden"; 690.0, "Plots are assigned each spring." ] do
                canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, y).ShowText(text).EndText() |> ignore
            let pixels = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 255uy; 255uy; 0uy |]
            canvas.AddImageWithTransformationMatrix(iText.IO.Image.ImageDataFactory.Create(2, 2, 3, 8, pixels, null), 100f, 0f, 0f, 100f, 72f, 450f, true)
            |> ignore
        let rect (l, b, r, t) : StructureGeometry = { PageNumber = 1; Left = l; Bottom = b; Right = r; Top = t }
        // A heading suggested as H (tagged as H1, with no title) whose text was corrected, and a
        // figure given alt text: each keyed to its suggested, looser region.
        let heading = rect (60.0, 710.0, 260.0, 736.0)
        let figure = rect (66.0, 444.0, 180.0, 556.0)
        let edits =
            RegionEdits.setAlt (RegionEdits.setText [] 1 "H" heading "Joinng the garden" "Joining the garden") 1 "Figure" figure "Seedlings"
        RegionEdits.save pdf edits
        let region tag geometry (alt: string | null) (text: string | null) : LayoutRegion =
            { Tag = tag; Geometry = geometry; ListNumbering = null; ActualText = text; QrText = null; Alt = alt }
        let layout =
            FixedLayout [ region "H" heading null "Joining the garden"; region "P" (rect (60.0, 684.0, 300.0, 704.0)) null null; region "Figure" figure "Seedlings" null ]

        VeraPdfValidator(LayoutSource = layout, Review = RegionEdits.reviewFor pdf None).Fix(pdf, PdfProfile.PdfUa1, output, PdfFixOptions.TagFromLayout)
        |> ignore

        let carried = RegionEdits.load output
        carried |> List.map (fun e -> e.Tag, e.Alt, e.Text) |> should equal [ "H1", None, Some "Joining the garden"; "Figure", Some "Seedlings", None ]
        // Each is an edit of an element in the copy, and already in it.
        let root = VeraPdfValidator().ReadStructureTree output |> nonNull
        let rec nodes (node: StructureNode) : StructureNode list = node :: (node.Children |> Seq.collect nodes |> List.ofSeq)
        for edit in carried do
            nodes root
            |> List.exists (fun node ->
                node.Tag = edit.Tag
                && not (isNull node.Geometry)
                && RegionEdits.overlap (nonNull node.Geometry) edit.Box >= RegionEdits.SameRegionOverlap)
            |> should be True
        ElementEdits.pending output (RegionEdits.toElementEdits carried) |> should equal 0
        // The original's file keeps them as Suggest Structure made them.
        RegionEdits.load pdf |> List.map _.Tag |> should equal [ "H"; "Figure" ]
    finally
        Directory.Delete(folder, true)
