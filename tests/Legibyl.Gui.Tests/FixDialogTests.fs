module Legibyl.Gui.Tests.FixDialogTests

open System
open Avalonia
open Avalonia.Controls
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private finding (fixOptions: PdfFixOptions) =
    { Specification = "ISO 14289-1:2014"
      Clause = "7.1"
      TestNumber = 1
      Passed = false
      Message = "A finding"
      ErrorMessage = null
      LocationContext = null
      Severity = FindingSeverity.Failure
      FixOptions = fixOptions
      Geometry = null }

let private report (findings: RuleViolation list) =
    { FilePath = "sample.pdf"
      Profile = PdfProfile.PdfUa1
      FlavourId = "ua1"
      IsCompliant = false
      TotalAssertions = 100
      Assertions = List.toArray findings }

/// Each checkbox's first line of text, whether it's ticked, and its count line.
let private options (dialog: Window) =
    dialog.GetVisualDescendants()
    |> Seq.choose (fun v -> match box v with :? CheckBox as c -> Some c | _ -> None)
    |> Seq.map (fun checkBox ->
        let lines =
            checkBox.GetVisualDescendants()
            |> Seq.choose (fun v -> match box v with :? TextBlock as t -> Some(string t.Text) | _ -> None)
            |> List.ofSeq
        let key = lines.Head.Split(' ') |> Array.truncate 3 |> String.concat " "
        key, (checkBox.IsChecked.GetValueOrDefault(), List.last lines))
    |> Map.ofSeq

[<Fact>]
let ``the fix dialog counts what each repair addresses and ticks only what applies`` () =
    onUiThread (fun () ->
        let owner = HeadlessWindow.show 800.0 600.0
        let findings =
            [ finding PdfFixOptions.AnnotationTabs
              finding PdfFixOptions.AnnotationTabs
              finding (PdfFixOptions.Metadata ||| PdfFixOptions.DocumentTitle)
              finding PdfFixOptions.ListsFromParagraphs
              finding PdfFixOptions.ListsFromParagraphs
              finding PdfFixOptions.ListsFromParagraphs
              finding PdfFixOptions.TagFromLayout
              finding PdfFixOptions.FontCidToGidMap ]

        let pending = Dialogs.chooseFixOptions owner (report findings)
        HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
        let dialog = owner.OwnedWindows |> Seq.exactlyOne
        let shown = options dialog
        dialog.Close()
        HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 2.0) (fun () -> pending.IsCompleted) |> ignore
        owner.Close()

        shown["Set /Tabs /S"] |> should equal (true, "2 found")
        shown["Give the document"] |> should equal (true, "1 found")
        shown["Fix XMP metadata"] |> should equal (true, "1 found")
        shown["Describe table-of-contents links"] |> should equal (false, "None found in this PDF")
        // Opt-in repairs show their count but stay unticked even when found.
        shown["Tag runs of"] |> should equal (false, "3 found")
        shown["Tag this untagged"] |> should equal (false, "1 found")
        shown["Declare the document's"] |> should equal (false, "None found in this PDF")
        shown["With tagging: link"] |> should equal (false, "None found in this PDF")
        // Safe repairs are ticked when found.
        shown["Write the glyph"] |> should equal (true, "1 found")
        shown["Tag headings the"] |> should equal (false, "None found in this PDF")
        shown["Remove empty semantic"] |> should equal (false, "None found in this PDF")
        shown["Add bookmarks for"] |> should equal (false, "None found in this PDF"))
