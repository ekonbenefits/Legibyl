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

/// Fixes pdf to output, carrying its review (as it's saved) with dismissed as its dismissals.
let private fixCarrying (pdf: string) (output: string) (dismissed: Dismissals.Dismissed) =
    let review = Reviews.read pdf
    review.Dismissed <- dismissed
    VeraPdfValidator(Review = review.ForCopy pdf).Fix(pdf, PdfProfile.PdfUa1, output, PdfFixOptions.AnnotationTabs)
    |> ignore

[<Fact>]
let ``the fixed copy carries the review it was fixed with, read-only`` () =
    withPdf (fun pdf output ->
        Reviews.save pdf (fun review ->
            review.Edits <- RegionEdits.setAlt [] 1 "Figure" logo "The garden's logo"
            review.Title <- Some "Joining the garden")

        fixCarrying pdf output (Map [ splitParagraphs, "Likely split paragraph" ])

        File.Exists(Dismissals.filePath output) |> should be False
        let copy = Reviews.read output
        // Carried, not the copy's own review: that starts empty.
        copy.Edits |> should be Empty
        copy.Dismissed |> should be Empty
        copy.CarriedEdits |> List.map _.Alt |> should equal [ Some "The garden's logo" ]
        copy.Carried |> Option.map _.Title |> should equal (Some "Joining the garden")
        // Its dismissals still hold.
        copy.AllDismissed |> should equal (Map [ splitParagraphs, "Likely split paragraph" ]))

[<Fact>]
let ``saving a fixed copy's review keeps the one in it as it was`` () =
    withPdf (fun pdf output ->
        fixCarrying pdf output (Map [ splitParagraphs, "Likely split paragraph" ])
        let other = { Dismissals.Rule = "Legibyl Structure-1"; Dismissals.Location = "StructTreeRoot/K[0]" }

        Reviews.save output (fun review -> review.Dismissed <- Map [ other, "Empty paragraph" ])

        let copy = Reviews.read output
        copy.Dismissed |> should equal (Map [ other, "Empty paragraph" ])
        copy.CarriedDismissed |> should equal (Map [ splitParagraphs, "Likely split paragraph" ])
        copy.AllDismissed.Count |> should equal 2)

[<Fact>]
let ``a copy of a copy carries both reviews`` () =
    withPdf (fun pdf output ->
        fixCarrying pdf output (Map [ splitParagraphs, "Likely split paragraph" ])
        let other = { Dismissals.Rule = "Legibyl Structure-1"; Dismissals.Location = "StructTreeRoot/K[0]" }
        let review = Reviews.read output
        review.Dismissed <- Map [ other, "Empty paragraph" ]
        let third = Path.ChangeExtension(output, ".v3.pdf") |> nonNull

        // Another repair: the copy's tab order is fixed already.
        VeraPdfValidator(Review = review.ForCopy output).Fix(output, PdfProfile.PdfUa1, third, PdfFixOptions.Metadata) |> ignore

        File.Exists third |> should be True
        (Reviews.read third).CarriedDismissed.Keys |> Set.ofSeq |> should equal (Set [ splitParagraphs; other ]))

[<Fact>]
let ``with no review at all, nothing is carried`` () =
    withPdf (fun pdf output ->
        fixCarrying pdf output Map.empty
        EmbeddedReview.read output |> should equal None)

[<Fact>]
let ``edits can be imported from another PDF: from inside a saved copy, or the file next to one`` () =
    withPdf (fun pdf output ->
        Reviews.saveEdits pdf (RegionEdits.setAlt [] 1 "Figure" logo "The garden's logo")
        fixCarrying pdf output Map.empty

        // The saved copy carries them inside it...
        RegionEdits.fromFileOrPdf output |> List.map _.Alt |> should equal [ Some "The garden's logo" ]
        // ...and the original has them in the file next to it.
        RegionEdits.fromFileOrPdf pdf |> List.map _.Alt |> should equal [ Some "The garden's logo" ]
        // A .legibyl.json file still imports as before.
        RegionEdits.fromFileOrPdf (Dismissals.filePath pdf) |> List.map _.Alt |> should equal [ Some "The garden's logo" ])

/// Regions as a suggested structure would give them, with edits made there applied.
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
        Reviews.saveEdits pdf edits
        let region tag geometry (alt: string | null) (text: string | null) : LayoutRegion =
            { Tag = tag; Geometry = geometry; ListNumbering = null; ActualText = text; QrText = null; Alt = alt }
        let layout =
            FixedLayout [ region "H" heading null "Joining the garden"; region "P" (rect (60.0, 684.0, 300.0, 704.0)) null null; region "Figure" figure "Seedlings" null ]

        VeraPdfValidator(LayoutSource = layout, Review = (Reviews.read pdf).ForCopy pdf).Fix(pdf, PdfProfile.PdfUa1, output, PdfFixOptions.TagFromLayout)
        |> ignore

        let carried = (Reviews.read output).CarriedEdits
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
        // The original's file keeps them as they were made on its suggested structure.
        (Reviews.read pdf).Edits |> List.map _.Tag |> should equal [ "H"; "Figure" ]
    finally
        Directory.Delete(folder, true)
