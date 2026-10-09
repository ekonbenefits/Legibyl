/// Before the suggested structure opens: if the PDF links to web pages without saying where (links
/// with no description, QR codes), offers to read those pages' titles to describe them.
module Legibyl.Gui.LinkTitleOffer

open System
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading
open Legibyl.Layout
open Legibyl.Validation

/// A web address found, and where ("link on page 2, QR code on page 3").
type Found = { Address: string; Where: string list }

let private add (found: Found list) (address: string) (where: string) =
    match found |> List.tryFindIndex (fun f -> f.Address = address) with
    | Some i -> found |> List.mapi (fun j f -> if i = j && not (List.contains where f.Where) then { f with Where = f.Where @ [ where ] } else f)
    | None -> found @ [ { Address = address; Where = [ where ] } ]

/// The web addresses QR codes on the pages found so far go to, each with its pages.
let qrCodesIn (review: DocumentReview) : Found list =
    review.Pages.Values
    |> Seq.sortBy _.PageNumber
    |> Seq.fold
        (fun found page ->
            page.Blocks
            |> List.choose (fun block -> block.QrText |> Option.bind LinkTitles.webAddress)
            |> List.fold (fun all address -> add all address $"QR code on page {page.PageNumber}") found)
        []

/// Offers to read the titles of the web pages links go to (those without a description, found
/// first) and QR codes found already (qrCodes; those found later follow the answer), each of
/// which can be unticked, with Read Titles and Skip, in a window of its own beside owner (not
/// stopping it being used). Remembers the answer in review, and doesn't ask again once it's
/// answered. With nothing that links to the web, it isn't shown. Returns once it's closed, true;
/// or, finding it had nothing to ask, false.
let offer (owner: Window) (pdfPath: string) (review: DocumentReview) (titleSource: IPageTitleSource) (qrCodes: Found list) : Task<bool> =
    if review.LinkTitles <> NotAsked then
        Task.FromResult false
    else
      task {
        // The PDF's links (quick: no page is drawn), in the background.
        let! links =
            Task.Run(fun () ->
                try
                    use scanner = new LinkScanner(pdfPath)
                    scanner.WebLinks()
                with _ ->
                    // Can't read them: offer the QR codes, if any.
                    [])
        // (As found before, if they were.)
        if review.WebLinks.IsEmpty then review.WebLinks <- links
        // Its links first, then the QR codes.
        let found =
            let linked = links |> List.fold (fun all link -> add all link.Address $"link on page {link.Page}") []
            qrCodes |> List.fold (fun all code -> code.Where |> List.fold (fun all where -> add all code.Address where) all) linked
        if not found.IsEmpty then
            let window =
                HostWindow(
                    Title = "Describe Web Links?",
                    Width = 560.0,
                    SizeToContent = SizeToContent.Height,
                    CanResize = false,
                    ShowInTaskbar = false,
                    WindowStartupLocation = WindowStartupLocation.CenterOwner
                )
            window.Content <-
                Component(fun ctx ->
                    let unticked = ctx.useState (Set.empty: Set<string>)
                    let reading = ctx.useState false

                    let skip _ =
                        review.LinkTitles <- Skipped
                        window.Close()

                    let read _ =
                        review.LinkTitles <- Read unticked.Current
                        reading.Set true
                        let wanted = found |> List.map _.Address |> List.filter (unticked.Current.Contains >> not)
                        task {
                            try
                                let! titles = Task.Run(fun () -> titleSource.PageTitles wanted)
                                review.Titles <- titles |> Seq.fold (fun all (KeyValue(address, title)) -> Map.add address title all) review.Titles
                            with _ ->
                                ()
                            window.Close()
                        }
                        |> ignore

                    let secondary = Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                    StackPanel.create [
                        StackPanel.margin 16.0
                        StackPanel.spacing 10.0
                        StackPanel.children [
                            TextBlock.create [
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.text (
                                    "This PDF links to web pages without saying where they go. Legibyl can read those pages' titles "
                                    + "to describe the links and QR codes, prefilled on the suggested structure for you to check. "
                                    + "Only public web pages are visited, and only their titles read; the sites see the visit."
                                )
                            ]
                            TextBlock.create [
                                secondary
                                TextBlock.text (
                                    if reading.Current then "Reading titles…" else $"Found {found.Length} web addresses."
                                )
                            ]
                            ScrollViewer.create [
                                ScrollViewer.maxHeight 280.0
                                ScrollViewer.content (
                                    StackPanel.create [
                                        StackPanel.children [
                                            for item in found ->
                                                CheckBox.create [
                                                    CheckBox.isChecked (not (unticked.Current.Contains item.Address))
                                                    CheckBox.isEnabled (not reading.Current)
                                                    CheckBox.onIsCheckedChanged (fun e ->
                                                        match e.Source with
                                                        | :? CheckBox as box ->
                                                            let ticked = box.IsChecked.GetValueOrDefault()
                                                            if ticked = unticked.Current.Contains item.Address then
                                                                unticked.Set(if ticked then unticked.Current.Remove item.Address else unticked.Current.Add item.Address)
                                                        | _ -> ())
                                                    CheckBox.content (
                                                        StackPanel.create [
                                                            StackPanel.children [
                                                                TextBlock.create [ TextBlock.text item.Address; TextBlock.textWrapping TextWrapping.Wrap ]
                                                                TextBlock.create [ TextBlock.text (String.Join(", ", item.Where)); TextBlock.fontSize 12.0; secondary ]
                                                            ]
                                                        ]
                                                    )
                                                ]
                                        ]
                                    ]
                                )
                            ]
                            TextBlock.create [
                                secondary
                                TextBlock.fontSize 12.0
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.text "QR codes found later, as each page's layout is found, follow this answer."
                            ]
                            StackPanel.create [
                                StackPanel.orientation Orientation.Horizontal
                                StackPanel.horizontalAlignment HorizontalAlignment.Right
                                StackPanel.spacing 8.0
                                StackPanel.children [
                                    Button.create [
                                        Button.content "Skip"
                                        Button.isCancel true
                                        Button.isEnabled (not reading.Current)
                                        ToolTip.tip "Don't read any titles: links and QR codes are described by their addresses."
                                        Button.onClick skip
                                    ]
                                    Button.create [
                                        Button.content "Read Titles"
                                        Button.isDefault true
                                        Button.isEnabled (
                                            not reading.Current
                                            && found |> List.exists (fun f -> not (unticked.Current.Contains f.Address))
                                        )
                                        Button.onClick read
                                    ]
                                ]
                            ]
                        ]
                    ])
            let closed = TaskCompletionSource<bool>()
            window.Closed.Add(fun _ -> closed.TrySetResult true |> ignore)
            window.Show owner
            return! closed.Task
        else
            return false
      }
