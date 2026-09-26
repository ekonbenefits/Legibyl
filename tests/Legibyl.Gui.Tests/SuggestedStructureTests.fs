module Legibyl.Gui.Tests.SuggestedStructureTests

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Font
open iText.IO.Font.Constants
open Legibyl.Gui
open Legibyl.Layout
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private button (root: Visual) (text: string) =
    descendants<Button> root |> List.find (fun b -> string b.Content = text)

let private click (b: Button) = b.RaiseEvent(RoutedEventArgs(Button.ClickEvent))

/// An untagged one-page PDF with a line of text, in its own temporary folder.
let private withUntaggedPdf (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let path = Path.Combine(folder, "untagged.pdf")
    do
        use document = new PdfDocument(new PdfWriter(path))
        let canvas = PdfCanvas(document.AddNewPage())
        canvas.BeginText().SetFontAndSize(PdfFontFactory.CreateFont StandardFonts.HELVETICA, 12f).MoveText(72.0, 720.0).ShowText("Hello").EndText() |> ignore
    try test path finally Directory.Delete(folder, true)

[<Fact>]
let ``an untagged PDF's structure offers to suggest one`` () =
    withUntaggedPdf (fun pdf ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window pdf
            click (button window "View Structure…")
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 20.0) (fun () -> window.OwnedWindows.Count = 1) |> ignore
            let dialog = window.OwnedWindows |> Seq.exactlyOne
            click (button dialog "Suggest Structure…")
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 5.0) (fun () ->
                window.OwnedWindows |> Seq.exists (fun w -> w :? SuggestedStructureWindow))
            |> ignore
            let opened = window.OwnedWindows |> Seq.filter (fun w -> w :? SuggestedStructureWindow) |> List.ofSeq
            for owned in List.ofSeq window.OwnedWindows do owned.Close()
            window.Close()
            opened |> should haveLength 1))

[<Fact>]
let ``the suggested structure lists each region the detector finds`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    onUiThread (fun () ->
        let window = SuggestedStructureWindow(Path.Combine(AppContext.BaseDirectory, "SampleCorpus", "pdfua1-pass.pdf"))
        window.Show()
        let blocks () =
            descendants<Button> window |> List.filter (fun b -> match b.Content with :? DockPanel -> true | _ -> false)
        let images () = descendants<Image> window |> List.filter (fun image -> not (isNull image.Source))
        HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 60.0) (fun () -> not (blocks().IsEmpty) && not (images().IsEmpty)) |> ignore
        let found = blocks().Length
        let texts = descendants<TextBlock> window |> List.map (fun t -> string t.Text)
        window.Close()
        found |> should be (greaterThan 0)
        texts |> List.exists (fun t -> t.StartsWith "Page 1 of 1") |> should be True
        texts |> List.exists (fun t -> t.StartsWith " 1 ") |> should be True)

[<Fact>]
let ``every page's layout is found in the background, so the next page shows at once`` () =
    if not (LayoutModel.isDownloaded ()) then
        Assert.Skip "The layout model isn't downloaded; open Suggest Structure in the app to fetch it."
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "two-pages.pdf")
    do
        use document = new PdfDocument(new PdfWriter(pdf))
        let font = PdfFontFactory.CreateFont StandardFonts.HELVETICA
        for title in [ "First page"; "Second page" ] do
            let canvas = PdfCanvas(document.AddNewPage())
            canvas.BeginText().SetFontAndSize(font, 24f).MoveText(72.0, 700.0).ShowText(title).EndText() |> ignore
            for line in 0 .. 5 do
                canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 640.0 - 16.0 * float line)
                    .ShowText("A paragraph of ordinary text, long enough to read as a block of body copy.").EndText()
                |> ignore
    try
        onUiThread (fun () ->
            let window = SuggestedStructureWindow(pdf)
            window.Show()
            let texts () = descendants<TextBlock> window |> List.map (fun t -> string t.Text)
            let allFound () = texts () |> List.exists (fun t -> t.EndsWith "every page's layout found")
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 60.0) allFound |> ignore
            let foundBeforeMoving = allFound ()

            click (button window "Next ▶")
            HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore
            let afterMoving = texts ()
            window.Close()

            foundBeforeMoving |> should be True
            afterMoving |> List.exists (fun t -> t.StartsWith "Page 2 of 2") |> should be True
            afterMoving |> List.exists (fun t -> t.StartsWith "Finding the layout") |> should be False)
    finally
        Directory.Delete(folder, true)
