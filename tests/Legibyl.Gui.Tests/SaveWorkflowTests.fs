module Legibyl.Gui.Tests.SaveWorkflowTests

open System
open System.IO
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.VisualTree
open iText.IO.Font.Constants
open iText.IO.Image
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

/// The button whose screen-reader name starts with label, and that name.
let private button (window: Window) (label: string) =
    descendants<Button> window
    |> List.find (fun b -> (string (AutomationProperties.GetName b)).StartsWith label || string b.Content = label)

let private name (b: Button) = match AutomationProperties.GetName b with null -> string b.Content | name -> name

let private rect (l, b, r, t) : StructureGeometry = { PageNumber = 1; Left = l; Bottom = b; Right = r; Top = t }

type private FixedLayout(regions: LayoutRegion list) =
    interface IPageLayoutSource with
        member _.PageRegions(_, _) = regions
        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = null

/// A page with a line of text and an image, in a fresh folder; tagged (a P and a Figure) if asked.
let private withPdf (tagged: bool) (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let untagged = Path.Combine(folder, "garden-untagged.pdf")
    let pdf = Path.Combine(folder, "garden.pdf")
    try
        do
            use document = new PdfDocument(new PdfWriter(untagged))
            let canvas = PdfCanvas(document.AddNewPage())
            canvas.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f).MoveText(72.0, 700.0).ShowText("The garden in spring.").EndText()
            |> ignore
            let pixels = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 255uy; 255uy; 0uy |]
            canvas.AddImageWithTransformationMatrix(ImageDataFactory.Create(2, 2, 3, 8, pixels, null), 100f, 0f, 0f, 100f, 72f, 550f, true) |> ignore
        if tagged then
            let region tag bounds : LayoutRegion =
                { Tag = tag; Geometry = rect bounds; ListNumbering = null; ActualText = null; QrText = null; Alt = null }
            let layout = FixedLayout [ region "P" (70.0, 695.0, 300.0, 715.0); region "Figure" (70.0, 548.0, 175.0, 652.0) ]
            VeraPdfValidator(LayoutSource = layout).Fix(untagged, PdfProfile.PdfUa1, pdf, PdfFixOptions.TagFromLayout) |> ignore
        else
            File.Copy(untagged, pdf)
        test pdf
    finally
        Directory.Delete(folder, true)

let private openPdf (pdf: string) =
    let window = HeadlessWindow.show 1200.0 720.0
    HeadlessWindow.openPdf window pdf
    window

let private waitFor (condition: unit -> bool) =
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) condition |> ignore

[<Fact>]
let ``nothing to export until there's an edit or a chosen fix`` () =
    onUiThread (fun () ->
        withPdf true (fun pdf ->
            let window = openPdf pdf
            let structure = name (button window "Structure…")
            let autoFixes = name (button window "Auto Fixes…")
            let canExport = (button window "Export…").IsEnabled
            window.Close()

            structure |> should equal "Structure…"
            autoFixes |> should equal "Auto Fixes…"
            canExport |> should be False))

[<Fact>]
let ``an edit not in the PDF yet counts on Structure, and can be exported`` () =
    onUiThread (fun () ->
        withPdf true (fun pdf ->
            Reviews.saveEdits pdf (RegionEdits.setAlt [] 1 "Figure" (rect (73.0, 551.0, 171.0, 649.0)) "Tomato seedlings")
            let window = openPdf pdf
            waitFor (fun () -> name (button window "Structure…") <> "Structure…")
            let structure = name (button window "Structure…")
            let canExport = (button window "Export…").IsEnabled
            window.Close()

            structure |> should equal "Structure…, 1 edit not in the PDF yet"
            canExport |> should be True))

[<Fact>]
let ``choosing auto fixes counts them, starting from the safe ones, and makes Export possible`` () =
    onUiThread (fun () ->
        withPdf true (fun pdf ->
            let window = openPdf pdf
            (button window "Auto Fixes…").RaiseEvent(RoutedEventArgs Button.ClickEvent)
            waitFor (fun () -> window.OwnedWindows.Count = 1)
            let dialog = window.OwnedWindows |> Seq.exactlyOne
            let ticked = descendants<CheckBox> dialog |> List.filter (fun c -> c.IsChecked.GetValueOrDefault()) |> List.length
            (descendants<Button> dialog |> List.find (fun b -> string b.Content = "Done")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            waitFor (fun () -> window.OwnedWindows.Count = 0)
            let autoFixes = name (button window "Auto Fixes…")
            let canExport = (button window "Export…").IsEnabled
            window.Close()

            ticked |> should be (greaterThan 0)
            autoFixes |> should equal $"Auto Fixes…, {ticked} chosen"
            canExport |> should be True))

[<Fact>]
let ``the auto fixes chosen are part of the review: Save keeps them for next time`` () =
    onUiThread (fun () ->
        withPdf true (fun pdf ->
            let window = openPdf pdf
            (button window "Auto Fixes…").RaiseEvent(RoutedEventArgs Button.ClickEvent)
            waitFor (fun () -> window.OwnedWindows.Count = 1)
            let dialog = window.OwnedWindows |> Seq.exactlyOne
            (descendants<Button> dialog |> List.find (fun b -> string b.Content = "Done")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            waitFor (fun () -> window.OwnedWindows.Count = 0)
            let chosen = name (button window "Auto Fixes…")
            // File › Save (or ⌘S): there's no Save button.
            let canSave = Reviews.chooseFromFileMenu window "Save"
            waitFor (fun () -> not (window.Title.Contains "Edited"))
            window.Close()

            let reopened = openPdf pdf
            let again = name (button reopened "Auto Fixes…")
            reopened.Close()

            chosen |> should not' (equal "Auto Fixes…")
            canSave |> should be True
            File.ReadAllText(pdf + ".legibyl.json") |> should haveSubstring "\"fixes\""
            again |> should equal chosen))

[<Fact>]
let ``an untagged PDF's edits on its suggested structure count on Structure, and Export tags from them`` () =
    onUiThread (fun () ->
        withPdf false (fun pdf ->
            Reviews.saveEdits pdf (RegionEdits.setAlt [] 1 "Figure" (rect (70.0, 548.0, 175.0, 652.0)) "Tomato seedlings")
            let window = openPdf pdf
            waitFor (fun () -> name (button window "Suggest Structure…") <> "Suggest Structure…")
            let structure = name (button window "Suggest Structure…")
            let canExport = (button window "Export…").IsEnabled
            window.Close()

            structure |> should equal "Suggest Structure…, 1 edit not in the PDF yet"
            canExport |> should be True))

[<Fact>]
let ``an untagged PDF waits for Suggest Structure… before it can be exported, and is tagged from it`` () =
    onUiThread (fun () ->
        withPdf false (fun pdf ->
            let window = openPdf pdf
            let suggest = button window "Suggest Structure…"
            let prominent = suggest.Classes.Contains "Warning"
            let before = (button window "Export…").IsEnabled
            // Its first page's layout found, as opening Suggest Structure… does.
            let review = DocumentShell.Shared.Reviews.For pdf
            review.Pages[1] <-
                { PageNumber = 1
                  Blocks =
                    [ { Order = 1; Label = "text"; Tag = "P"; Score = 0.9; Geometry = rect (70.0, 695.0, 300.0, 715.0); Text = "The garden in spring."
                        ListNumbering = None; Recognized = false; QrText = None } ]
                  HasText = true
                  Rotated = false }
            // Counted again, as it is when the window's back in front (here, validating it again).
            HeadlessWindow.openPdf window pdf
            let after = (button window "Export…").IsEnabled
            window.Close()

            prominent |> should be True
            before |> should be False
            after |> should be True))

[<Fact>]
let ``an untagged PDF's findings point to Suggest Structure…`` () =
    onUiThread (fun () ->
        withPdf false (fun pdf ->
            let window = openPdf pdf
            let rows = HeadlessWindow.findings window
            HeadlessWindow.selectRow window (rows |> List.findIndex (fun r -> r.Violation.FixOptions.HasFlag PdfFixOptions.TagFromLayout))
            let texts = descendants<TextBox> window |> List.map (fun t -> string t.Text)
            let headings = descendants<TextBlock> window |> List.map (fun t -> string t.Text)
            window.Close()

            headings |> should contain "Suggest Structure fixes it"
            headings |> should not' (contain "Auto fix")
            texts
            |> List.exists (fun t -> t.EndsWith "Open Suggest Structure… to see the headings, figures and reading order suggested for each page; Export… then tags a copy from it.")
            |> should be True))

[<Fact>]
let ``there's no Save button; Auto Fixes has the wrench its fixes have, and Suggest Structure the wand`` () =
    onUiThread (fun () ->
        withPdf false (fun pdf ->
            let window = openPdf pdf
            let labels = descendants<Button> window |> List.map (fun b -> name b)
            let icons (label: string) =
                descendants<Shapes.Path> (button window label) |> List.length
            let autoFixes, suggest = icons "Auto Fixes…", icons "Suggest Structure…"
            // Findings fixed by suggesting the structure are marked with the wand too.
            let fixedBy =
                descendants<Shapes.Path> (HeadlessWindow.resultsList window)
                |> List.map (fun p -> string (AutomationProperties.GetName p))
                |> List.filter (fun n -> n.StartsWith "Suggest Structure: ")
            window.Close()

            fixedBy |> should not' (be Empty)

            labels |> should not' (contain "Save")
            autoFixes |> should equal 1
            suggest |> should equal 1))
