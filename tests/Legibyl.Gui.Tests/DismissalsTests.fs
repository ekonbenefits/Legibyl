module Legibyl.Gui.Tests.DismissalsTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation

let private finding severity clause testNumber location =
    { Specification = (if severity = FindingSeverity.Warning then "Legibyl" else "ISO 14289-1:2014")
      Clause = clause
      TestNumber = testNumber
      Passed = (severity = FindingSeverity.Warning)
      Message = $"Message for {clause}-{testNumber}"
      ErrorMessage = null
      LocationContext = location
      Severity = severity
      FixOptions = PdfFixOptions.None
      Geometry = null
      Element = null }

let private splitParagraph = finding FindingSeverity.Warning "Structure" 2 "StructTreeRoot/K[0]/K[3] + StructTreeRoot/K[0]/K[4]"
let private emptyParagraph = finding FindingSeverity.Warning "Structure" 1 "StructTreeRoot/K[0]/K[7]"
let private failure = finding FindingSeverity.Failure "7.1" 3 "root/document[0]/pages[0]"

/// A path for a PDF in a fresh temporary folder (the PDF itself needn't exist).
let private withPdfPath (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try test (Path.Combine(folder, "report.pdf")) finally Directory.Delete(folder, true)

[<Fact>]
let ``only warnings can be dismissed`` () =
    let dismissed = Map [ Dismissals.keyOf failure, failure.Message ]
    Dismissals.canDismiss splitParagraph |> should be True
    Dismissals.canDismiss failure |> should be False
    Dismissals.isDismissed dismissed failure |> should be False

[<Fact>]
let ``a warning is recognised by its rule and location`` () =
    let dismissed = Map [ Dismissals.keyOf splitParagraph, splitParagraph.Message ]
    Dismissals.isDismissed dismissed splitParagraph |> should be True
    Dismissals.isDismissed dismissed emptyParagraph |> should be False
    Dismissals.isDismissed dismissed { splitParagraph with LocationContext = "StructTreeRoot/K[1]" } |> should be False

[<Fact>]
let ``ignoring a warning's type dismisses every warning of its rule`` () =
    let dismissed = Map [ Dismissals.ruleKeyOf splitParagraph, splitParagraph.Message ]
    Dismissals.isDismissed dismissed splitParagraph |> should be True
    Dismissals.isDismissed dismissed { splitParagraph with LocationContext = "StructTreeRoot/K[1]" } |> should be True
    Dismissals.isRuleDismissed dismissed splitParagraph |> should be True
    Dismissals.isDismissed dismissed emptyParagraph |> should be False

[<Fact>]
let ``saved dismissals load back`` () =
    withPdfPath (fun pdf ->
        let dismissed =
            Map [ Dismissals.keyOf splitParagraph, splitParagraph.Message
                  Dismissals.ruleKeyOf emptyParagraph, emptyParagraph.Message ]

        Reviews.save pdf (fun review -> review.Dismissed <- dismissed)

        File.Exists(pdf + ".legibyl.json") |> should be True
        (Reviews.read pdf).Dismissed |> should equal dismissed)

[<Fact>]
let ``no file or an unreadable one loads as nothing`` () =
    withPdfPath (fun pdf ->
        (Reviews.read pdf).Dismissed |> should be Empty
        File.WriteAllText(Dismissals.filePath pdf, "not json")
        (Reviews.read pdf).Dismissed |> should be Empty)

[<Fact>]
let ``saving a review with nothing in it deletes the file`` () =
    withPdfPath (fun pdf ->
        Reviews.save pdf (fun review -> review.Dismissed <- Map [ Dismissals.keyOf splitParagraph, splitParagraph.Message ])

        Reviews.save pdf (fun review -> review.Dismissed <- Map.empty)

        File.Exists(Dismissals.filePath pdf) |> should be False)
