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

/// Lists the addresses as they're found (links at once, QR codes as a quick scan reaches each
/// page), each of which can be unticked, with Read titles and Skip. Remembers the answer in
/// review, and doesn't ask again once it's answered. If nothing links to the web, closes by
/// itself. Returns once it's closed.
let offer (owner: Window) (pdfPath: string) (review: DocumentReview) (titleSource: IPageTitleSource) : Task =
    if review.LinkTitles <> NotAsked then
        Task.CompletedTask
    else
        let window =
            HostWindow(
                Title = "Describe Web Links?",
                Width = 560.0,
                SizeToContent = SizeToContent.Height,
                CanResize = false,
                ShowInTaskbar = false,
                WindowStartupLocation = WindowStartupLocation.CenterOwner
            )
        let cancel = new CancellationTokenSource()
        window.Content <-
            Component(fun ctx ->
                let found = ctx.useState ([]: Found list)
                let unticked = ctx.useState (Set.empty: Set<string>)
                // The page being scanned and the page count; None once the scan is done.
                let scanning = ctx.useState (Some(0, 0))
                let reading = ctx.useState false

                ctx.useEffect (
                    (fun () ->
                        let post f = Dispatcher.UIThread.Post(fun () -> f ())
                        Task.Run(fun () ->
                            try
                                use scanner = new LinkScanner(pdfPath)
                                let links = scanner.WebLinks()
                                review.WebLinks <- links
                                post (fun () ->
                                    found.Set(links |> List.fold (fun all link -> add all link.Address $"link on page {link.Page}") found.Current))
                                let mutable page = 1
                                while page <= scanner.PageCount && not cancel.IsCancellationRequested do
                                    let current = page
                                    post (fun () -> scanning.Set(Some(current, scanner.PageCount)))
                                    let codes = scanner.QrCodes current |> List.choose (fst >> LinkTitles.webAddress)
                                    post (fun () ->
                                        found.Set(codes |> List.fold (fun all address -> add all address $"QR code on page {current}") found.Current))
                                    page <- page + 1
                            with _ ->
                                // Can't scan: offer what was found, if anything.
                                ()
                            post (fun () ->
                                scanning.Set None
                                if found.Current.IsEmpty && not reading.Current then window.Close()))
                        |> ignore),
                    [ EffectTrigger.AfterInit ]
                )

                let skip _ =
                    review.LinkTitles <- Skipped
                    window.Close()

                let read _ =
                    review.LinkTitles <- Read unticked.Current
                    reading.Set true
                    let wanted = found.Current |> List.map _.Address |> List.filter (unticked.Current.Contains >> not)
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
                                match scanning.Current, reading.Current with
                                | _, true -> "Reading titles…"
                                | Some(page, count), _ when count > 0 -> $"Looking for QR codes: page {page} of {count}"
                                | Some _, _ -> "Looking for links…"
                                | None, _ -> $"Found {found.Current.Length} web addresses."
                            )
                        ]
                        ScrollViewer.create [
                            ScrollViewer.maxHeight 280.0
                            ScrollViewer.content (
                                StackPanel.create [
                                    StackPanel.children [
                                        for item in found.Current ->
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
                            TextBlock.text "QR codes found later, as each page is looked at, follow this answer."
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
                                        && (scanning.Current.IsSome || found.Current |> List.exists (fun f -> not (unticked.Current.Contains f.Address)))
                                    )
                                    Button.onClick read
                                ]
                            ]
                        ]
                    ]
                ])
        task {
            do! window.ShowDialog owner
            cancel.Cancel()
        }
