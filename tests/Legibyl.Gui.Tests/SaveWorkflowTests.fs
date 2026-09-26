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
let ``nothing to save until there's an edit or a chosen fix`` () =
    onUiThread (fun () ->
        withPdf true (fun pdf ->
            let window = openPdf pdf
            let structure = name (button window "Structure…")
            let autoFixes = name (button window "Auto Fixes…")
            let canSave = (button window "Save…").IsEnabled
            window.Close()

            structure |> should equal "Structure…"
            autoFixes |> should equal "Auto Fixes…"
            canSave |> should be False))

[<Fact>]
let ``an edit not in the PDF yet counts on Structure, and can be saved`` () =
    onUiThread (fun () ->
        withPdf true (fun pdf ->
            RegionEdits.save pdf (RegionEdits.setAlt [] 1 "Figure" (rect (73.0, 551.0, 171.0, 649.0)) "Tomato seedlings")
            let window = openPdf pdf
            waitFor (fun () -> name (button window "Structure…") <> "Structure…")
            let structure = name (button window "Structure…")
            let canSave = (button window "Save…").IsEnabled
            window.Close()

            structure |> should equal "Structure…, 1 edit to save"
            canSave |> should be True))

[<Fact>]
let ``choosing auto fixes counts them, starting from the safe ones, and makes Save possible`` () =
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
            let canSave = (button window "Save…").IsEnabled
            window.Close()

            ticked |> should be (greaterThan 0)
            autoFixes |> should equal $"Auto Fixes…, {ticked} chosen"
            canSave |> should be True))

[<Fact>]
let ``an untagged PDF's review in Suggest Structure counts on Structure, and Save tags from it`` () =
    onUiThread (fun () ->
        withPdf false (fun pdf ->
            RegionEdits.save pdf (RegionEdits.setAlt [] 1 "Figure" (rect (70.0, 548.0, 175.0, 652.0)) "Tomato seedlings")
            let window = openPdf pdf
            waitFor (fun () -> name (button window "Structure…") <> "Structure…")
            let structure = name (button window "Structure…")
            let canSave = (button window "Save…").IsEnabled
            window.Close()

            structure |> should equal "Structure…, 1 edit to save"
            canSave |> should be True))
