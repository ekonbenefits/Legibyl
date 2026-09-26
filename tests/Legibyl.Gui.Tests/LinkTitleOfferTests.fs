module Legibyl.Gui.Tests.LinkTitleOfferTests

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
open Legibyl.Gui
open Legibyl.Layout
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

/// Titles from a list, with no web involved; remembers what it was asked for.
type private FakeTitles(titles: Map<string, string>) =
    member val Asked: string list = [] with get, set
    interface IPageTitleSource with
        member this.PageTitles(addresses) =
            this.Asked <- List.ofSeq addresses
            titles |> Map.filter (fun address _ -> List.contains address this.Asked) :> Collections.Generic.IReadOnlyDictionary<_, _>

/// A PDF, in its own folder, with a link that has no description and a QR code, if asked for.
let private withPdf (links: bool) (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "flyer.pdf")
    do
        use document = new PdfDocument(new PdfWriter(pdf))
        let page = document.AddNewPage()
        if links then
            let link = Annot.PdfLinkAnnotation(iText.Kernel.Geom.Rectangle(72f, 700f, 120f, 14f))
            link.SetAction(Action.PdfAction.CreateURI "https://example.com/apply") |> ignore
            page.AddAnnotation link |> ignore
            let canvas = PdfCanvas(page)
            let matrix = ZXing.QrCode.QRCodeWriter().encode("https://example.com/scan", ZXing.BarcodeFormat.QR_CODE, 144, 144)
            for row in 0 .. matrix.Height - 1 do
                for column in 0 .. matrix.Width - 1 do
                    if matrix[column, row] then canvas.Rectangle(72.0 + float column, 500.0 - float row, 1.0, 1.0) |> ignore
            canvas.Fill() |> ignore
    try test pdf finally Directory.Delete(folder, true)

/// Opens the offer, waits for its scan, runs act on it, and returns once it's closed.
let private runOffer pdf (review: LayoutReview) titles (act: Window -> unit) =
    let owner = HeadlessWindow.show 800.0 600.0
    let pending = LinkTitleOffer.offer owner pdf review titles
    let dialog () = owner.OwnedWindows |> Seq.tryHead
    let found (window: Window) = descendants<TextBlock> window |> List.exists (fun t -> (string t.Text).StartsWith "Found ")
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 30.0) (fun () -> pending.IsCompleted || (dialog () |> Option.exists found)) |> ignore
    dialog () |> Option.iter act
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) (fun () -> pending.IsCompleted) |> ignore
    owner.Close()
    pending.IsCompleted |> should be True

let private click (window: Window) (text: string) =
    let button = descendants<Button> window |> List.find (fun b -> string b.Content = text)
    button.RaiseEvent(RoutedEventArgs(Button.ClickEvent))
    pump ()

[<Fact>]
let ``it lists links and QR codes, and reads the titles of the ticked ones`` () =
    withPdf true (fun pdf ->
        onUiThread (fun () ->
            let review = LayoutReview(DateTime.UtcNow)
            let titles = FakeTitles(Map [ "https://example.com/apply", "Apply online"; "https://example.com/scan", "Scan me" ])
            let mutable listed = []
            runOffer pdf review titles (fun dialog ->
                listed <- descendants<CheckBox> dialog |> List.map (fun c -> (descendants<TextBlock> c).Head.Text |> string)
                // Untick the QR code's address.
                (descendants<CheckBox> dialog |> List.find (fun c -> (descendants<TextBlock> c).Head.Text = "https://example.com/scan")).IsChecked <- false
                pump ()
                click dialog "Read Titles")

            listed |> should equal [ "https://example.com/apply"; "https://example.com/scan" ]
            titles.Asked |> should equal [ "https://example.com/apply" ]
            review.LinkTitles |> should equal (Read(set [ "https://example.com/scan" ]))
            review.Titles |> should equal (Map [ "https://example.com/apply", "Apply online" ])
            review.WebLinks |> List.map _.Address |> should equal [ "https://example.com/apply" ]))

[<Fact>]
let ``skipping reads nothing`` () =
    withPdf true (fun pdf ->
        onUiThread (fun () ->
            let review = LayoutReview(DateTime.UtcNow)
            let titles = FakeTitles(Map [ "https://example.com/apply", "Apply online" ])
            runOffer pdf review titles (fun dialog -> click dialog "Skip")
            review.LinkTitles |> should equal Skipped
            titles.Asked |> should be Empty
            review.Titles |> should be Empty))

[<Fact>]
let ``a PDF that doesn't link to the web isn't asked about, and an answer isn't asked for again`` () =
    withPdf false (fun pdf ->
        onUiThread (fun () ->
            let review = LayoutReview(DateTime.UtcNow)
            runOffer pdf review (FakeTitles Map.empty) ignore
            review.LinkTitles |> should equal NotAsked))
    withPdf true (fun pdf ->
        onUiThread (fun () ->
            let review = LayoutReview(DateTime.UtcNow, LinkTitles = Skipped)
            let owner = HeadlessWindow.show 800.0 600.0
            let pending = LinkTitleOffer.offer owner pdf review (FakeTitles Map.empty)
            let asked = owner.OwnedWindows.Count
            owner.Close()
            pending.IsCompleted |> should be True
            asked |> should equal 0))

[<Fact>]
let ``a link is described as prefilled from its page's title, or as edited`` () =
    withPdf true (fun pdf ->
        use suggester = new LayoutSuggester(pdf)
        let link = { Page = 1; Box = { PageNumber = 1; Left = 72.0; Bottom = 700.0; Right = 192.0; Top = 714.0 }; Address = "https://example.com/apply" }
        let review = LayoutReview(DateTime.UtcNow, WebLinks = [ link ], Titles = Map [ link.Address, "Apply online" ])
        let source = ReviewedLayout(LayoutPrefetcher(suggester, Collections.Concurrent.ConcurrentDictionary()), review) :> IPageLayoutSource
        source.LinkDescription(pdf, 1, link.Box) |> should equal "Apply online (example.com)"
        // Only a change from what was prefilled is recorded.
        RegionEdits.setAltAgainst [] 1 "Link" link.Box "Apply online (example.com)" "Apply online (example.com)" |> should be Empty
        review.Edits <- RegionEdits.setAltAgainst [] 1 "Link" link.Box "Apply online (example.com)" "Apply to join Pack 721"
        source.LinkDescription(pdf, 1, link.Box) |> should equal "Apply to join Pack 721"
        // Cleared: no description.
        review.Edits <- RegionEdits.setAltAgainst [] 1 "Link" link.Box "Apply online (example.com)" ""
        source.LinkDescription(pdf, 1, link.Box) |> should be Null)
