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

let private lines (checkBox: CheckBox) =
    checkBox.GetVisualDescendants()
    |> Seq.choose (fun v -> match box v with :? TextBlock as t -> Some(string t.Text) | _ -> None)
    |> List.ofSeq

/// Each repair's title, whether it's ticked, and its count, in the order listed.
let private rows (dialog: Window) =
    dialog.GetVisualDescendants()
    |> Seq.choose (fun v -> match box v with :? CheckBox as c -> Some c | _ -> None)
    |> Seq.map (fun checkBox -> let text = lines checkBox in text.Head, (checkBox.IsChecked.GetValueOrDefault(), List.last text))
    |> List.ofSeq

let private options (dialog: Window) = rows dialog |> Map.ofList

/// Everything a repair's row says.
let private rowText (dialog: Window) (title: string) =
    dialog.GetVisualDescendants()
    |> Seq.choose (fun v -> match box v with :? CheckBox as c -> Some c | _ -> None)
    |> Seq.map lines
    |> Seq.find (fun text -> text.Head = title)
    |> String.concat " "

/// Opens the dialog for these findings, runs test on it, then closes it with Cancel unless
/// the test already closed it; returns the dialog's result.
let private withDialog (findings: RuleViolation list) (test: Window -> unit) =
    let owner = HeadlessWindow.show 800.0 600.0
    let pending = Dialogs.chooseFixOptions owner (report findings)
    HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
    let dialog = owner.OwnedWindows |> Seq.exactlyOne
    test dialog
    if not pending.IsCompleted then dialog.Close()
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 2.0) (fun () -> pending.IsCompleted) |> ignore
    owner.Close()
    pending.Result

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private findings =
    [ finding PdfFixOptions.AnnotationTabs
      finding PdfFixOptions.AnnotationTabs
      finding (PdfFixOptions.Metadata ||| PdfFixOptions.DocumentTitle)
      finding PdfFixOptions.ListsFromParagraphs
      finding PdfFixOptions.ListsFromParagraphs
      finding PdfFixOptions.ListsFromParagraphs
      finding PdfFixOptions.TagFromLayout
      finding PdfFixOptions.FontCidToGidMap ]

[<Fact>]
let ``the fix dialog lists what this PDF needs, in the order repairs run, ticking only safe ones`` () =
    onUiThread (fun () ->
        let mutable shown = []
        withDialog findings (fun dialog -> shown <- rows dialog) |> ignore

        shown
        |> should
            equal
            [ "Tag from the page layout", (false, "1 found")
              "Link QR codes", (false, "with tagging")
              "Tag lists", (false, "3 found")
              "Tab order for annotations", (true, "2 found")
              "Font glyph mapping", (true, "1 found")
              // Opt-in: it visits the web.
              "Describe web links from their pages", (false, "with tagging")
              "Document title", (true, "1 found")
              // Every failure here is fixable, so veraPDF can repair the metadata.
              "Repair XMP metadata", (true, "1 found") ])

[<Fact>]
let ``showing all repairs lists the ones this PDF doesn't need too, still in order`` () =
    onUiThread (fun () ->
        let mutable shown = []
        withDialog findings (fun dialog ->
            let toggle = descendants<ToggleSwitch> dialog |> List.exactlyOne
            toggle.IsChecked <- true
            pump ()
            shown <- rows dialog)
        |> ignore

        shown |> List.map fst
        |> should
            equal
            [ "Tag from the page layout"
              "Link QR codes"
              "Declare the language"
              "Headings from the table of contents"
              "Remove empty paragraphs"
              "Remove empty contents entries"
              "Tag lists"
              "Add bookmarks"
              "Tab order for annotations"
              "Font glyph mapping"
              "Describe contents links"
              "Describe web links from their pages"
              "Document title"
              "Title from the first page"
              "Repair XMP metadata" ]
        (Map.ofList shown)["Describe contents links"] |> should equal (false, "none found"))

[<Fact>]
let ``metadata starts unticked when some failure can't be fixed, and says why`` () =
    onUiThread (fun () ->
        let mutable shown = Map.empty
        let mutable text = ""
        withDialog (finding PdfFixOptions.Metadata :: finding PdfFixOptions.None :: findings) (fun dialog ->
            shown <- options dialog
            text <- rowText dialog "Repair XMP metadata")
        |> ignore

        shown["Repair XMP metadata"] |> should equal (false, "2 found")
        text |> should haveSubstring "veraPDF only repairs metadata once every other failure is fixed")

[<Fact>]
let ``continuing applies the ticked repairs`` () =
    onUiThread (fun () ->
        let result =
            withDialog findings (fun dialog ->
                let continueButton = descendants<Button> dialog |> List.find (fun b -> string b.Content = "Continue…")
                continueButton.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs(Button.ClickEvent))
                pump ())

        result
        |> should
            equal
            (Some(
                PdfFixOptions.AnnotationTabs
                ||| PdfFixOptions.FontCidToGidMap
                ||| PdfFixOptions.DocumentTitle
                ||| PdfFixOptions.Metadata
            )))
