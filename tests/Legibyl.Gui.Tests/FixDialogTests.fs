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
      Geometry = null
      Element = null }

let private report (findings: RuleViolation list) =
    { FilePath = "sample.pdf"
      Profile = PdfProfile.PdfUa1
      FlavourId = "ua1"
      PdfVersion = "1.7"
      ProfileDetected = true
      IsCompliant = false
      TotalAssertions = 100
      Assertions = List.toArray findings
      Notes = [||] }

let private lines (checkBox: CheckBox) =
    checkBox.GetVisualDescendants()
    |> Seq.choose (fun v -> match box v with :? TextBlock as t -> Some(string t.Text) | _ -> None)
    |> List.ofSeq

/// Each fix's title, whether it's ticked, and its count, in the order listed.
let private rows (dialog: Window) =
    dialog.GetVisualDescendants()
    |> Seq.choose (fun v -> match box v with :? CheckBox as c -> Some c | _ -> None)
    |> Seq.map (fun checkBox -> let text = lines checkBox in text.Head, (checkBox.IsChecked.GetValueOrDefault(), List.last text))
    |> List.ofSeq

let private options (dialog: Window) = rows dialog |> Map.ofList

/// Everything a fix's row says.
let private rowText (dialog: Window) (title: string) =
    dialog.GetVisualDescendants()
    |> Seq.choose (fun v -> match box v with :? CheckBox as c -> Some c | _ -> None)
    |> Seq.map lines
    |> Seq.find (fun text -> text.Head = title)
    |> String.concat " "

/// Opens the dialog for these findings (with what was chosen before, if anything), runs test
/// on it, then closes it with Cancel unless the test already closed it; returns the dialog's result.
let private withDialogTitled (titleGiven: bool) (chosen: PdfFixOptions option) (tagNote: string option) (findings: RuleViolation list) (test: Window -> unit) =
    let owner = HeadlessWindow.show 800.0 600.0
    let pending = Dialogs.chooseFixOptions owner (report findings) tagNote titleGiven chosen
    HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
    let dialog = owner.OwnedWindows |> Seq.exactlyOne
    test dialog
    if not pending.IsCompleted then dialog.Close()
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 2.0) (fun () -> pending.IsCompleted) |> ignore
    owner.Close()
    pending.Result

let private withDialogChosen chosen tagNote findings test = withDialogTitled false chosen tagNote findings test

let private withDialogNoted tagNote findings test = withDialogChosen None tagNote findings test

let private withDialog findings test = withDialogNoted None findings test

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private findings =
    [ finding PdfFixOptions.Tabs
      finding PdfFixOptions.Tabs
      finding (PdfFixOptions.Metadata ||| PdfFixOptions.Title)
      finding PdfFixOptions.Lists
      finding PdfFixOptions.Lists
      finding PdfFixOptions.Lists
      finding PdfFixOptions.TagFromLayout
      finding PdfFixOptions.FontGlyphs ]

[<Fact>]
let ``the fix dialog lists what this PDF needs, in the order fixes run, ticking only safe ones`` () =
    onUiThread (fun () ->
        let mutable shown = []
        withDialog findings (fun dialog -> shown <- rows dialog) |> ignore

        shown
        |> should
            equal
            // Not tagging itself: an untagged PDF is tagged from its suggested structure.
            [ "Link QR codes", (false, "with tagging")
              "Tag lists", (false, "3 found")
              "Tab order for annotations", (true, "2 found")
              "Font glyph mapping", (true, "1 found")
              // Opt-in: it visits the web.
              "Describe web links from their pages", (false, "with tagging")
              "Document title", (true, "1 found")
              // Lists, and tagging, aren't ticked, so veraPDF can't fix the metadata yet.
              "Fix XMP metadata", (false, "1 found") ])

[<Fact>]
let ``showing all fixes lists the ones this PDF doesn't need too, still in order`` () =
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
            [ "Link QR codes"
              "Rebuild the parent tree"
              "Declare the language"
              "Headings from the table of contents"
              "Remove empty paragraphs"
              "Remove empty contents entries"
              "Remove empty figures"
              "Join split paragraphs"
              "Tag lists"
              "Add bookmarks"
              "Point links at their tags"
              "Tab order for annotations"
              "Font glyph mapping"
              "Font dictionaries"
              "Describe contents links"
              "Describe web links from their pages"
              "Document title"
              "Title from the first page"
              "Fix XMP metadata" ]
        (Map.ofList shown)["Describe contents links"] |> should equal (false, "none found"))

[<Fact>]
let ``metadata starts unticked when some failure can't be fixed, and says why`` () =
    onUiThread (fun () ->
        let mutable shown = Map.empty
        let mutable text = ""
        withDialog (finding PdfFixOptions.Metadata :: finding PdfFixOptions.None :: findings) (fun dialog ->
            shown <- options dialog
            text <- rowText dialog "Fix XMP metadata")
        |> ignore

        shown["Fix XMP metadata"] |> should equal (false, "2 found")
        text |> should haveSubstring "veraPDF only writes it once every other failure is fixed")

[<Fact>]
let ``metadata starts ticked when the fixes ticked at first fix every failure`` () =
    onUiThread (fun () ->
        let mutable shown = Map.empty
        withDialog
            [ finding PdfFixOptions.Tabs
              finding (PdfFixOptions.Metadata ||| PdfFixOptions.Title)
              finding PdfFixOptions.Metadata ]
            (fun dialog -> shown <- options dialog)
        |> ignore

        shown["Fix XMP metadata"] |> should equal (true, "2 found"))

[<Fact>]
let ``with no title, metadata waits for one; a title typed in Structure ticks it, for the same pass`` () =
    onUiThread (fun () ->
        let findings = [ finding PdfFixOptions.Tabs; finding PdfFixOptions.Metadata; { finding PdfFixOptions.None with TestNumber = 9 } ]
        let mutable untitled = Map.empty
        let mutable text = ""
        withDialog findings (fun dialog ->
            untitled <- options dialog
            text <- rowText dialog "Fix XMP metadata")
        |> ignore
        let mutable titled = Map.empty
        withDialogTitled true None None findings (fun dialog -> titled <- options dialog) |> ignore

        fst untitled["Fix XMP metadata"] |> should be False
        text |> should haveSubstring "the document has no title for the metadata. Type one in Structure…'s Document title field"
        fst titled["Fix XMP metadata"] |> should be True)

[<Fact>]
let ``continuing applies the ticked fixes`` () =
    onUiThread (fun () ->
        let result =
            withDialog findings (fun dialog ->
                let continueButton = descendants<Button> dialog |> List.find (fun b -> string b.Content = "Done")
                continueButton.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs(Button.ClickEvent))
                pump ())

        result
        |> should
            equal
            (Some(
                PdfFixOptions.Tabs
                ||| PdfFixOptions.FontGlyphs
                ||| PdfFixOptions.Title
            )))

[<Fact>]
let ``for an untagged PDF, the dialog says it's tagged from its suggested structure`` () =
    onUiThread (fun () ->
        let mutable texts = []
        withDialogNoted (Some "This PDF has no tags: Export… tags a copy from its suggested structure.") findings (fun dialog ->
            texts <- descendants<TextBlock> dialog |> List.map (fun t -> string t.Text))
        |> ignore

        texts |> List.exists (fun t -> t.StartsWith "This PDF has no tags: Export… tags a copy from its suggested structure.") |> should be True
        texts |> should not' (contain "Tag from the page layout"))

[<Fact>]
let ``only the CLI's safe fixes start ticked: bookmarks don't`` () =
    onUiThread (fun () ->
        let mutable shown = Map.empty
        withDialog (finding PdfFixOptions.Bookmarks :: findings) (fun dialog -> shown <- options dialog) |> ignore

        fst shown["Add bookmarks"] |> should be False)

[<Fact>]
let ``with every fix shown on a short screen, the list scrolls and the buttons stay on screen`` () =
    onUiThread (fun () ->
        let mutable height = 0.0
        let mutable continueBottom = Double.MaxValue
        let mutable scrolls = false
        let mutable limited = false
        withDialog findings (fun dialog ->
            // It limits itself to the screen...
            limited <- not (Double.IsInfinity dialog.MaxHeight)
            // ...here a short one.
            dialog.MaxHeight <- 500.0
            (descendants<ToggleSwitch> dialog |> List.exactlyOne).IsChecked <- true
            pump ()
            height <- dialog.Bounds.Height
            let continueButton = descendants<Button> dialog |> List.find (fun b -> string b.Content = "Done")
            continueBottom <- (continueButton.TranslatePoint(Point(0.0, continueButton.Bounds.Height), dialog)).Value.Y
            let list = descendants<ScrollViewer> dialog |> List.head
            scrolls <- list.Extent.Height > list.Viewport.Height)
        |> ignore

        limited |> should be True
        height |> should be (lessThanOrEqualTo 500.0)
        continueBottom |> should be (lessThanOrEqualTo height)
        scrolls |> should be True)

[<Fact>]
let ``reopened, it shows what was chosen before rather than the safe defaults`` () =
    onUiThread (fun () ->
        let mutable shown = []
        withDialogChosen (Some PdfFixOptions.Lists) None findings (fun dialog -> shown <- rows dialog) |> ignore

        shown |> List.filter (snd >> fst) |> List.map fst |> should equal [ "Tag lists" ])

[<Fact>]
let ``done with nothing ticked chooses nothing, and cancelling keeps what was chosen`` () =
    onUiThread (fun () ->
        let untickAllAndFinish (dialog: Window) =
            for checkBox in descendants<CheckBox> dialog do
                checkBox.IsChecked <- false
            pump ()
            (descendants<Button> dialog |> List.find (fun b -> string b.Content = "Done")).RaiseEvent(Avalonia.Interactivity.RoutedEventArgs(Button.ClickEvent))
            pump ()

        withDialog findings untickAllAndFinish |> should equal (Some PdfFixOptions.None)
        withDialogChosen (Some PdfFixOptions.Tabs) None findings ignore |> should equal None)

[<Fact>]
let ``showing all fixes keeps the dialog's size: the list scrolls instead`` () =
    onUiThread (fun () ->
        let mutable before = 0.0
        let mutable after = 0.0
        let mutable scrolls = false
        withDialog findings (fun dialog ->
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 1.0) (fun () -> false) |> ignore
            before <- dialog.Bounds.Height
            (descendants<ToggleSwitch> dialog |> List.exactlyOne).IsChecked <- true
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 1.0) (fun () -> false) |> ignore
            after <- dialog.Bounds.Height
            let list = descendants<ScrollViewer> dialog |> List.head
            scrolls <- list.Extent.Height > list.Viewport.Height)
        |> ignore

        after |> should equal before
        scrolls |> should be True)

[<Fact>]
let ``the Export result with many changes fits the screen: they scroll, OK stays on it`` () =
    onUiThread (fun () ->
        let owner = HeadlessWindow.show 800.0 600.0
        let fix: FixResult =
            { FilePath = "sample.pdf"
              Profile = PdfProfile.PdfUa1
              RequestedOptions = PdfFixOptions.ApplyEdits
              OutputPath = "sample.v2.pdf"
              Status = RepairStatus.Success
              AppliedFixes = [ for i in 1 .. 60 -> $"Change number {i}, described at some length so it wraps onto a second line in the dialog." ]
              NotApplied = [ for i in 1 .. 20 -> $"Edit {i} wasn't applied." ]
              OriginalReport = report [] }
        let pending = Dialogs.showFixResult owner (Ok fix) None
        pump ()
        let dialog = owner.OwnedWindows |> Seq.exactlyOne
        let limited = not (Double.IsInfinity dialog.MaxHeight)
        // Here a short screen.
        dialog.MaxHeight <- 500.0
        pump ()
        let height = dialog.Bounds.Height
        let ok = descendants<Button> dialog |> List.find (fun b -> string b.Content = "OK")
        let okBottom = (ok.TranslatePoint(Point(0.0, ok.Bounds.Height), dialog)).Value.Y
        let list = descendants<ScrollViewer> dialog |> List.head
        let scrolls = list.Extent.Height > list.Viewport.Height
        dialog.Close()
        HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 2.0) (fun () -> pending.IsCompleted) |> ignore
        owner.Close()

        limited |> should be True
        height |> should be (lessThanOrEqualTo 500.0)
        okBottom |> should be (lessThanOrEqualTo height)
        scrolls |> should be True)
