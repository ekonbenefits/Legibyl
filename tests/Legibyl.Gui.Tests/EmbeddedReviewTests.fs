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
    VeraPdfValidator(Review = Dismissals.reviewJson pdf (Some dismissed)).Fix(pdf, PdfProfile.PdfUa1, output, PdfFixOptions.AnnotationTabs)
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
