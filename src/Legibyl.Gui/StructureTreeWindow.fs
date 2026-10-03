namespace Legibyl.Gui

open System
open System.IO
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform.Storage
open Avalonia.VisualTree
open Legibyl.Layout
open Legibyl.Rendering
open Legibyl.Validation

/// What Structure… shows: a tagged PDF's structure, or for an untagged one, the layout
/// suggested for it (found page by page as the window is open), with the review's edits on it.
type StructureSource =
    | Tagged of StructureNode
    | Suggested

/// Formats a StructureNode into a single-line summary for the structure TreeView.
module StructureSummary =
    let format (node: StructureNode) =
        let parts =
            [ yield $"<{node.Tag}>"
              match node.Role with NonNull role -> yield $"role={role}" | Null -> ()
              if node.PageNumber.HasValue then yield $"p.{node.PageNumber.Value}"
              match node.Language with NonNull lang -> yield $"[{lang}]" | Null -> ()
              match node.AlternateDescription with NonNull alt -> yield $"alt: \"{alt}\"" | Null -> ()
              match node.ActualText with NonNull text -> yield $"actualText: \"{text}\"" | Null -> ()
              match node.FormFieldDescription with NonNull tooltip -> yield $"tooltip: \"{tooltip}\"" | Null -> ()
              match node.FormFieldName with NonNull name -> yield $"field: {name}" | Null -> ()
              match node.FormFieldType with NonNull fieldType -> yield $"type: {fieldType}" | Null -> ()
              match node.LinkUri with NonNull href -> yield $"href: {href}" | Null -> ()
              match node.LinkDescription with NonNull description -> yield $"description: \"{description}\"" | Null -> ()
              match node.LinkDestinationDescription with NonNull dest -> yield $"dest: {dest}" | Null -> ()
              if node.ContentLeafCount > 0 then
                  yield $"""({node.ContentLeafCount} content item{if node.ContentLeafCount = 1 then "" else "s"})""" ]

        let summary = String.Join("  ", parts)
        match node.Content with
        | NonNull content ->
            let maxPreview = 60
            let preview = if content.Length > maxPreview then content[.. maxPreview - 1] + "…" else content
            summary + $"  — \"{preview}\""
        | Null -> summary

/// Shows a tagged PDF's logical structure tree (the semantic hierarchy that screen
/// readers use) both as an HTML-like nested "boxes with borders" layout (so a sighted
/// user can visually see block-level nesting and the real text each element owns) and
/// as a browsable outline, with an option to export the whole tree — including content
/// text — as JSON for AI agents or other tooling.
module private StructureView =
    open StructureColors
    open StructurePreview
    open StructureActions
    open StructureBlocks

    let jsonOptions = JsonSerializerOptions(WriteIndented = true, PropertyNamingPolicy = JsonNamingPolicy.CamelCase)

    /// Whether the page preview draws only the selection, for this session: a busy page's
    /// outlines can hide it.
    let mutable private selectionOnlyThisSession = false

    /// Over an element on the page preview, to say it can be clicked.
    let private handCursor = lazy (new Cursor(StandardCursorType.Hand))

    /// Edits change review (Save in the main window keeps it); onEditsChanged is called after each
    /// (so the main window can count them). For a suggested layout, prefetcher finds its pages and
    /// tagged is unused.
    let view
        (window: Window)
        (renderer: PdfRegionRenderer)
        (tagged: StructureNode)
        (review: DocumentReview)
        (prefetcher: LayoutPrefetcher option)
        (sourceFilePath: string)
        (onEditsChanged: unit -> unit)
        (findings: FindingMarkers.Findings)
        (layoutRegions: ((int -> StructureGeometry list) option) ref)
        =
        // What's drawn outside every tag, most likely decoration first.
        let tagged = sortUntagged tagged
        Component(fun ctx ->
            let suggested = prefetcher |> Option.map (fun prefetcher -> review, prefetcher)
            let isSuggested = suggested.IsSome
            // The findings on each block: a tagged PDF's only (a suggested structure's findings are
            // about the untagged file, not its blocks).
            let placed =
                ctx.useStateLazy(fun () ->
                    FindingMarkers.place tagged (if isSuggested then [] else findings.Items)).Current
            // A suggested layout's pages as elements, by page: each built once, so a selection
            // stays with its elements as more pages are found.
            let pageNodes = ctx.useState (Collections.Generic.Dictionary<int, StructureNode list>(), false)
            let buildRoot () =
                match suggested with
                | Some(review, _) ->
                    for KeyValue(number, page) in review.Pages do
                        if not (pageNodes.Current.ContainsKey number) then
                            pageNodes.Current[number] <- SuggestedTree.pageNodes review page
                    SuggestedTree.build [ for number in Seq.sort pageNodes.Current.Keys -> pageNodes.Current[number] ]
                | None -> tagged
            let root = ctx.useStateLazy buildRoot
            // The highlighted borders and the styles to restore when the selection moves.
            let highlighted = ctx.useState (([]: (Border * (IBrush | null) * Thickness) list), false)
            // The selected blocks' nodes, in document order, and the one a shift-click extends from.
            let selected = ctx.useState ([]: StructureNode list)
            let anchor = ctx.useState ((None: StructureNode option), false)
            // Collapsed blocks opened to bring one selected on the preview into view.
            let expanded = ctx.useState ([]: StructureNode list)
            // Each node's parent, to extend a selection over its siblings (worked out again when a
            // suggested layout's root changes).
            let parentsCache =
                ctx.useState ((None: (StructureNode * Collections.Generic.Dictionary<StructureNode, StructureNode>) option), false)
            let parents () =
                match parentsCache.Current with
                | Some(forRoot, parents) when obj.ReferenceEquals(forRoot, root.Current) -> parents
                | _ ->
                    let parents = Collections.Generic.Dictionary<StructureNode, StructureNode>(HashIdentity.Reference)
                    let rec add (node: StructureNode) =
                        for child in node.Children do
                            parents[child] <- node
                            add child
                    add root.Current
                    parentsCache.Set(Some(root.Current, parents))
                    parents
            // The user's edits for this PDF: its review's.
            let edits = ctx.useState review.Edits
            // The edits as of the last move, which the blocks' order follows.
            let moves = ctx.useState review.Edits
            /// Whether node is moved and isn't in that place in the PDF yet: its predecessor as read
            /// isn't the one its move names.
            let movePending (node: StructureNode) =
                match editFor edits.Current node |> Option.bind _.Move, (parents ()).TryGetValue node with
                | Some move, (true, parent) ->
                    let asRead = List.ofSeq parent.Children
                    let before =
                        match asRead |> List.tryFindIndex (fun n -> obj.ReferenceEquals(n, node)) with
                        | Some i when i > 0 -> Some asRead[i - 1]
                        | _ -> None
                    match move, before with
                    | First, None -> false
                    | After box, Some previous ->
                        match previous.Geometry with
                        | NonNull own -> own.PageNumber <> box.PageNumber || Boxes.overlap own box < RegionEdits.SameRegionOverlap
                        | Null -> true
                    | _ -> true
                | _ -> false
            let modelReady = ctx.useState (not isSuggested || LayoutModel.isDownloaded ())
            // The page the preview shows, if any.
            let shownPage = ctx.useState ((None: int option), false)
            let previewImage = ctx.useState (None: Bitmap option)
            // For a link: the page it goes to, with its destination marked in blue.
            let targetImage = ctx.useState (None: Bitmap option)
            let targetNote = ctx.useState ""
            let previewNote =
                ctx.useState "Select an element to highlight where it is on its page, when that's known."
            // Bumped on every selection so a slower preview for an earlier one can't overwrite it.
            let previewVersion = ctx.useState (0, false)
            // Draw only the selection on the preview, not every element on its page.
            let selectionOnly = ctx.useState selectionOnlyThisSession

            /// A region's marker on the preview for the findings on it and on what's inside it (an
            /// element's own block may be nested in the one outlined): red with failures, amber with
            /// only warnings, and how many.
            let markerFor (node: StructureNode) =
                let rec findingsIn (node: StructureNode) : RuleViolation list =
                    (match placed.TryGetValue node with | true, found -> found | _ -> [])
                    @ [ for child in node.Children do yield! findingsIn child ]
                match findingsIn node with
                | [] -> None
                | found ->
                    let color = if found |> List.exists (fun f -> f.Severity = FindingSeverity.Failure) then 0xE53935u else 0xF9A825u
                    Some(color, string found.Length)

            /// Shows the page the selection is on, every element on it outlined and numbered in
            /// reading order, the selected ones (or those inside a selected container) filled; and
            /// for a link, the page it goes to. With nothing selected, the first page.
            let showPage (nodes: StructureNode list) =
                task {
                    previewVersion.Set(previewVersion.Current + 1)
                    let version = previewVersion.Current
                    let tree = root.Current
                    let currentEdits = edits.Current
                    previewImage.Set None
                    targetImage.Set None
                    targetNote.Set ""
                    let within (node: StructureNode) =
                        let rec up (current: StructureNode) depth =
                            nodes |> List.exists (fun s -> obj.ReferenceEquals(s, current))
                            || (depth < 200 && (match (parents ()).TryGetValue current with | true, parent -> up parent (depth + 1) | _ -> false))
                        up node 0
                    let page =
                        match nodes |> List.tryPick firstPage with
                        | Some page -> Some page
                        | None when nodes.IsEmpty -> tree.Children |> Seq.tryPick firstPage
                        | None -> None
                    shownPage.Set page
                    let destination =
                        match nodes with
                        | [ node ] -> Option.ofObj node.LinkDestination
                        | _ -> None
                    match page with
                    | None ->
                        previewNote.Set(
                            match nodes with
                            | [] when isSuggested && not modelReady.Current -> "Each page's layout is found once the layout model is downloaded (above)."
                            | [] when isSuggested -> "Finding each page's layout… (several seconds a page)"
                            | [ node ] ->
                                $"Selected <{node.Tag}>. It has no place on a page to show (a container without content, or one across pages)."
                            | _ -> "Nothing here has a place on a page to show."
                        )
                    | Some page -> previewNote.Set $"Rendering page {page}…"
                    if page.IsSome || destination.IsSome then
                        try
                            // Render and decode off the UI thread; a full page is a large image.
                            let! source, target =
                                Task.Run(fun () ->
                                    let decode (bytes: byte array) =
                                        use stream = new MemoryStream(bytes)
                                        new Bitmap(stream)
                                    page
                                    |> Option.map (fun page ->
                                        let elements = pageElements (orderedChildren currentEdits) tree page
                                        // As edited: given another tag, or merged into its neighbour.
                                        let outlined =
                                            [ for i, element in List.indexed elements ->
                                                  let edit = editFor currentEdits element
                                                  let tag = edit |> Option.bind _.Retag |> Option.defaultValue element.Tag
                                                  let leftOut = edit |> Option.exists _.Exclude
                                                  { Geometry = nonNull element.Geometry
                                                    Label = if leftOut then $"× {i + 1} {tag}" else $"{i + 1} {tag}"
                                                    // Not a region of its own, not read, or outside every tag: grey.
                                                    Color = if leftOut || tag = "Artifact" || tag = "Untagged" then leftOutColor else outlineColor tag
                                                    // Outside every tag (whatever's chosen for it): dashed.
                                                    Dashed = isUntagged element
                                                    Emphasised = within element
                                                    Marker = markerFor element } ]
                                        // A selection that isn't one of them (a span in a paragraph, say) is filled too.
                                        let extra =
                                            [ for node in nodes do
                                                  match node.Geometry with
                                                  | NonNull geometry when geometry.PageNumber = page && not (elements |> List.exists (fun e -> obj.ReferenceEquals(e, node))) ->
                                                      { Geometry = geometry; Label = node.Tag; Color = outlineColor node.Tag; Emphasised = true; Marker = markerFor node; Dashed = isUntagged node }
                                                  | _ -> () ]
                                        // Only the selection, when asked: the rest are still there to click.
                                        let drawn =
                                            if selectionOnly.Current then (outlined |> List.filter _.Emphasised) @ extra
                                            else outlined @ extra
                                        let note =
                                            let count = elements.Length
                                            let what = if count = 1 then "its element" else $"its {count} elements"
                                            let hidden = count - (outlined |> List.filter _.Emphasised).Length
                                            let hiddenNote = if hidden = 1 then "1 element hidden" else $"{hidden} elements hidden"
                                            if selectionOnly.Current && nodes.IsEmpty then
                                                $"Page {page}: select an element (here, or in the list) to show it; {hiddenNote}."
                                            elif selectionOnly.Current then $"Page {page}: the selection only; {hiddenNote}."
                                            elif nodes.IsEmpty then $"Page {page}: {what}, outlined and numbered in reading order."
                                            else $"Page {page}: {what}, outlined and numbered in reading order; the selection filled."
                                        page, decode (renderer.RenderPageWithRegions(sourceFilePath, page, drawn, previewScale)), note),
                                    destination |> Option.map (fun d -> let result = renderer.RenderPageWithTarget(sourceFilePath, d) in result, decode result.PngBytes))
                            if version = previewVersion.Current then
                                match source with
                                | Some(_, image, note) ->
                                    previewImage.Set(Some image)
                                    previewNote.Set(if destination.IsSome then $"{note} Where the link goes is below." else note)
                                | None -> ()
                                match target with
                                | Some(result, image) ->
                                    targetImage.Set(Some image)
                                    targetNote.Set $"Where the link goes (marked in blue), page {result.PageNumber}: {result.Note}"
                                | None -> ()
                            else
                                source |> Option.iter (fun (_, image, _) -> image.Dispose())
                                target |> Option.iter (snd >> _.Dispose())
                        with :? InvalidOperationException | :? IOException as ex ->
                            if version = previewVersion.Current then
                                previewNote.Set $"Could not render the page: {ex.Message}"
                }
                |> ignore

            // The first page's overview, to start with.
            ctx.useEffect ((fun () -> showPage []), [ EffectTrigger.AfterInit ])

            let select (node: StructureNode) (border: Border) (extend: bool) =
                // Extending: every sibling from the anchor to this one (a shift-click on a block
                // with another parent selects just it).
                let nodes =
                    match anchor.Current with
                    | Some from when extend && not (obj.ReferenceEquals(from, node)) ->
                        match (parents ()).TryGetValue from, (parents ()).TryGetValue node with
                        | (true, parent), (true, other) when obj.ReferenceEquals(parent, other) ->
                            let siblings = List.ofSeq parent.Children
                            let i = siblings |> List.findIndex (fun s -> obj.ReferenceEquals(s, from))
                            let j = siblings |> List.findIndex (fun s -> obj.ReferenceEquals(s, node))
                            siblings[min i j .. max i j]
                        | _ -> [ node ]
                    | _ -> [ node ]
                if nodes.Length = 1 then anchor.Set(Some node)
                selected.Set nodes
                // Restyle just the affected borders; re-rendering the layout would re-diff every
                // block, which is slow for large documents.
                for previous, previousBrush, previousThickness in highlighted.Current do
                    previous.BorderBrush <- previousBrush
                    previous.BorderThickness <- previousThickness
                let borders =
                    if nodes.Length = 1 then
                        [ border ]
                    else
                        window.GetVisualDescendants()
                        |> Seq.choose (fun v ->
                            match v with
                            | :? Border as b when (match b.Tag with :? StructureNode as n -> nodes |> List.exists (fun s -> obj.ReferenceEquals(s, n)) | _ -> false) ->
                                Some b
                            | _ -> None)
                        |> List.ofSeq
                highlighted.Set [ for b in borders -> b, b.BorderBrush, b.BorderThickness ]
                for b in borders do
                    b.BorderBrush <- Brushes.DodgerBlue
                    b.BorderThickness <- Thickness 3.0

                showPage nodes

            /// Makes next the review's edits (Save in the main window keeps them).
            let saveEdits (next: RegionEdits.Edit list) =
                task {
                    review.Edits <- next
                    edits.Set next
                    if movesIn next <> movesIn moves.Current then moves.Set next
                    onEditsChanged ()
                    // The overview labels elements as edited.
                    showPage selected.Current
                }

            let editNode (node: StructureNode) (kind: EditKind) =
                match node.Geometry with
                | null -> ()
                | box ->
                    task {
                        let page = box.PageNumber
                        let existing = editFor edits.Current node
                        let title, prompt, current, placeholder =
                            match kind with
                            | AltText ->
                                // Decoration isn't described but tagged Artifact, not read: a suggested region,
                                // or a tagged PDF's figure.
                                let decoration =
                                    if isSuggested || node.Tag = "Figure" then " If it's only decoration, close this and give it the tag Artifact instead."
                                    else ""
                                // A piece outside the tags is given alt text as the figure it's tagged as.
                                let shownTag = if isUntagged node then "Figure" else node.Tag
                                "Alt Text",
                                $"Alt text for this <{shownTag}> on page {page}: what a screen reader says for it. "
                                + "Say what it's for here, not only what it shows: \"Map of the route to the campsite\", not \"map\". "
                                + $"Keep it to a sentence or so, and leave out \"image of\": the reader says it's an image.{decoration}",
                                existing |> Option.bind _.Alt |> Option.defaultValue (string node.AlternateDescription),
                                "What it's for, in a sentence"
                            | LinkDescription ->
                                let target = match node.LinkUri with NonNull uri -> $" (it goes to {uri})" | Null -> ""
                                "Link Description",
                                $"What a screen reader announces for this link on page {page}{target}: where it goes, e.g. \"Apply for benefits (example.com)\".",
                                existing |> Option.bind _.Alt |> Option.defaultValue (string node.LinkDescription),
                                "Where the link goes"
                            | ReplacementText ->
                                "Replacement Text",
                                $"What a screen reader reads for this <{node.Tag}> on page {page} (its /ActualText), in place of what's drawn.",
                                existing |> Option.bind _.Text |> Option.defaultValue (string node.ActualText),
                                "Nothing to read here (cleared)"
                        match! Dialogs.promptText window title prompt current placeholder with
                        | Some text ->
                            let next =
                                match kind with
                                // Blank drops the edit: an element's alt text can't be made empty.
                                | AltText when String.IsNullOrWhiteSpace text -> RegionEdits.setAlt edits.Current page node.Tag box ""
                                | AltText -> RegionEdits.setAltAgainst edits.Current page node.Tag box (string node.AlternateDescription) text
                                | LinkDescription when String.IsNullOrWhiteSpace text -> RegionEdits.setAlt edits.Current page node.Tag box ""
                                | LinkDescription -> RegionEdits.setAltAgainst edits.Current page node.Tag box (string node.LinkDescription) text
                                | ReplacementText -> RegionEdits.setText edits.Current page node.Tag box (string node.ActualText) text
                            do! saveEdits next
                        | None -> ()
                    }
                    |> ignore

            /// Selects the block for node, as clicking it does, opening collapsed blocks above it if
            /// it's in one, and scrolls it into view; then does after with its block. A node without
            /// a block of its own (a table's row, say) selects the nearest one above it that has.
            let selectNodeThen (node: StructureNode) (extend: bool) (after: Border -> unit) =
                let borderFor (node: StructureNode) =
                    window.GetVisualDescendants()
                    |> Seq.tryPick (fun v ->
                        match v with
                        | :? Border as border when obj.ReferenceEquals(border.Tag, node) -> Some border
                        | _ -> None)
                let rec ancestors (node: StructureNode) acc =
                    match (parents ()).TryGetValue node with
                    | true, parent -> ancestors parent (parent :: acc)
                    | _ -> acc
                let finish () =
                    match node :: List.rev (ancestors node []) |> List.tryPick (fun n -> borderFor n |> Option.map (fun b -> n, b)) with
                    | Some(shown, border) ->
                        select shown border extend
                        border.BringIntoView() |> ignore
                        after border
                    | None -> ()
                match borderFor node with
                | Some _ -> finish ()
                | None ->
                    expanded.Set(ancestors node [] @ expanded.Current)
                    // Once the opened blocks are laid out.
                    Avalonia.Threading.Dispatcher.UIThread.Post(finish, Avalonia.Threading.DispatcherPriority.Background)

            let selectNode (node: StructureNode) (extend: bool) = selectNodeThen node extend ignore

            /// The page preview's element under the pointer, if any.
            let previewNodeAt (e: PointerEventArgs) =
                match e.Source, shownPage.Current, previewImage.Current with
                | (:? Visual as source), Some page, Some bitmap ->
                    source.GetSelfAndVisualAncestors()
                    |> Seq.tryPick (fun v -> match v with :? Image as image -> Some image | _ -> None)
                    |> Option.bind (fun image ->
                        pointOnPage image bitmap (e.GetPosition image) |> Option.map (fun point -> image, page, point))
                    |> Option.bind (fun (image, page, (x, y)) -> nodeAt root.Current page x y |> Option.map (fun node -> image, node))
                | _ -> None

            /// Marks every piece outside the tags like this one (the same kind, about the same size,
            /// on any page) as decoration, once a person has seen how many and where.
            let markAllLike (piece: StructureNode) =
                task {
                    let size (n: StructureNode) = let g = nonNull n.Geometry in g.Right - g.Left, g.Top - g.Bottom
                    let width, height = size piece
                    let near (a: float) (b: float) = abs (a - b) <= max 2.0 (b * 0.1)
                    let rec all (node: StructureNode) : StructureNode list =
                        node :: [ for child in node.Children do yield! all child ]
                    let alike =
                        all root.Current
                        |> List.filter (fun n ->
                            isUntagged n
                            && not (isNull n.Geometry)
                            && untaggedKind n = untaggedKind piece
                            && (let w, h = size n in near w width && near h height)
                            && (editFor edits.Current n |> Option.bind _.Retag) <> Some "Artifact")
                    let pages = alike |> List.map (fun n -> (nonNull n.Geometry).PageNumber) |> List.distinct |> List.sort
                    let where =
                        match pages with
                        | [ page ] -> $"page {page}"
                        | pages -> "pages " + (pages |> List.map string |> String.concat ", ")
                    let n (value: float) = Math.Round(value).ToString(Globalization.CultureInfo.InvariantCulture)
                    let count = if alike.Length = 1 then "1 piece" else $"{alike.Length} pieces"
                    let! confirmed =
                        Dialogs.showMessageWithAction
                            window
                            $"Mark {count} like this ({untaggedKind piece}, about {n width} × {n height} pt), on {where}, as decoration? Each is still drawn, but a screen reader skips it. Check none is a chart or picture that needs describing."
                            "Mark All Like This"
                            $"Mark {count} as Decoration"
                    if confirmed then
                        do!
                            alike
                            |> List.fold (fun edits n -> let box = nonNull n.Geometry in RegionEdits.setRetag edits box.PageNumber n.Tag box "Artifact") edits.Current
                            |> saveEdits
                }
                |> ignore

            let selection =
                { Select = select
                  Edits = edits
                  Edit = editNode
                  Suggested = isSuggested
                  Expanded = expanded
                  Findings = placed
                  ShowFinding = findings.Show
                  Moves = moves
                  MovePending = movePending }

            // For a suggested layout: how many pages are found, and the titles of the pages its QR
            // codes link to, read as they're found (if the offer before the window allowed it).
            let readyCount = ctx.useState (suggested |> Option.map (fun (review, _) -> review.Pages.Count) |> Option.defaultValue 0)
            let failures = ctx.useState (Map.empty: Map<int, string>)
            let readingTitles = ctx.useState 0
            let asked = ctx.useState (Collections.Generic.HashSet<string>(), false)
            let status = ctx.useState ""
            let downloadProgress = ctx.useState (None: float option)

            /// Reads the titles of the pages a found page's QR codes go to, if allowed and not read
            /// yet; then builds the pages again, their alt text prefilled with them.
            let readTitlesFor (review: DocumentReview) (page: PageSuggestion) =
                match
                    page.Blocks
                    |> List.choose (fun block -> block.QrText |> Option.bind LinkTitles.webAddress)
                    |> List.filter (fun address -> review.MayRead address && not (review.Titles.ContainsKey address))
                    |> List.filter asked.Current.Add
                with
                | [] -> ()
                | wanted ->
                    readingTitles.Set(readingTitles.Current + 1)
                    task {
                        try
                            let! found = Task.Run(fun () -> (LinkTitles.WebPageTitles() :> IPageTitleSource).PageTitles wanted)
                            review.Titles <- found |> Seq.fold (fun all (KeyValue(address, title)) -> Map.add address title all) review.Titles
                            pageNodes.Current.Clear()
                            root.Set(buildRoot ())
                            showPage selected.Current
                        finally
                            readingTitles.Set(readingTitles.Current - 1)
                    }
                    |> ignore

            // Pages are found in the background; each is added to the structure as it is.
            ctx.useEffect (
                (fun () ->
                    match suggested with
                    | Some(review, prefetcher) ->
                        let ready =
                            prefetcher.PageReady.Subscribe(fun (_, page) ->
                                Avalonia.Threading.Dispatcher.UIThread.Post(fun () ->
                                    readyCount.Set review.Pages.Count
                                    root.Set(buildRoot ())
                                    if shownPage.Current.IsNone then showPage selected.Current
                                    readTitlesFor review page))
                        let failed =
                            prefetcher.PageFailed.Subscribe(fun (number, reason) ->
                                Avalonia.Threading.Dispatcher.UIThread.Post(fun () -> failures.Set(failures.Current.Add(number, reason))))
                        for page in review.Pages.Values do
                            readTitlesFor review page
                        if modelReady.Current then prefetcher.Start()
                        { new IDisposable with
                            member _.Dispose() =
                                ready.Dispose()
                                failed.Dispose() }
                    | None -> { new IDisposable with member _.Dispose() = () }),
                [ EffectTrigger.AfterInit ]
            )

            let download _ =
                task {
                    downloadProgress.Set(Some 0.0)
                    try
                        do! LayoutModel.download (Progress<float>(fun fraction -> downloadProgress.Set(Some fraction))) CancellationToken.None
                        downloadProgress.Set None
                        modelReady.Set true
                        suggested |> Option.iter (fun (_, prefetcher) -> prefetcher.Start())
                        showPage selected.Current
                    with ex ->
                        downloadProgress.Set None
                        status.Set $"Couldn't download the layout model: {ex.Message}"
                }
                |> ignore

            /// Adds edits from another .legibyl.json file, e.g. one saved for an earlier version of
            /// this PDF under another name, or from another PDF: those saved for it, or inside it (a
            /// copy Legibyl saved).
            let importEdits _ =
                task {
                    let! files =
                        window.StorageProvider.OpenFilePickerAsync(
                            FilePickerOpenOptions(
                                Title = "Import edits",
                                AllowMultiple = false,
                                FileTypeFilter = [ FilePickerFileType("PDFs and Legibyl files", Patterns = [ "*.pdf"; "*.legibyl.json"; "*.json" ]) ]
                            )
                        )
                    match files |> Seq.tryHead |> Option.bind (fun f -> Option.ofObj (f.TryGetLocalPath())) with
                    | Some path ->
                        let! imported = Task.Run(fun () -> RegionEdits.fromFileOrPdf path)
                        let merged, added = RegionEdits.merge edits.Current imported
                        if added > 0 then do! saveEdits merged
                        do!
                            Dialogs.showMessage
                                window
                                (if imported.IsEmpty then $"{Path.GetFileName path} has no edits to import."
                                 else $"Imported {added} of {imported.Length} edits from {Path.GetFileName path}; the rest were already here. They're in the Edits tab.")
                                "Import Edits"
                    | None -> ()
                }
                |> ignore

            let exportJson _ =
                task {
                    let! path =
                        Dialogs.pickSavePath
                            window
                            (FilePickerSaveOptions(
                                Title = "Export structure as JSON",
                                SuggestedFileName = $"{Path.GetFileNameWithoutExtension sourceFilePath}-structure.json",
                                DefaultExtension = "json",
                                FileTypeChoices = [ FilePickerFileTypes.Json; FilePickerFileTypes.All ]
                            ))
                            sourceFilePath
                    match path with
                    | Some path -> File.WriteAllText(path, JsonSerializer.Serialize(root.Current, jsonOptions))
                    | None -> ()
                }
                |> ignore

            // The synthetic StructTreeRoot node itself carries no semantic info, so show
            // its children (the actual document hierarchy) as the top level in both views.
            // Order Page N by Layout: finding the page's layout.
            let ordering = ctx.useState false
            /// The element whose children are the tagged PDF's top level (its Document), if there's one.
            let topLevel () =
                match root.Current.Children |> Seq.filter (fun n -> n.Tag <> "NotInTags") |> List.ofSeq with
                | [ document ] when not (fixedOrder.Contains document.Tag) -> Some document
                | _ -> None
            /// Suggests page's reading order from its layout, to accept or not.
            let orderByLayout (page: int) =
                task {
                    match topLevel () with
                    | None -> status.Set "Ordering a page by its layout orders a document's top-level elements; this PDF has no one element they're in."
                    | Some parent ->
                        ordering.Set true
                        try
                            try
                                let! regions =
                                    match layoutRegions.Value with
                                    | Some regionsOf -> Task.FromResult(regionsOf page)
                                    | None ->
                                        task {
                                            if not (LayoutModel.isDownloaded ()) then
                                                downloadProgress.Set(Some 0.0)
                                                do! LayoutModel.download (Progress<float>(fun fraction -> downloadProgress.Set(Some fraction))) CancellationToken.None
                                                downloadProgress.Set None
                                            return! Task.Run(fun () -> LayoutOrder.detect sourceFilePath page)
                                        }
                                let now = orderedChildren edits.Current parent
                                let suggestion = LayoutOrder.propose now page regions
                                if List.forall2 (fun a b -> obj.ReferenceEquals(a, b)) now suggestion then
                                    status.Set $"Page {page}'s layout reads it in the order it's tagged already."
                                else
                                    // Put in that order at once, as moves; Restore Page N Original Order undoes them.
                                    status.Set ""
                                    recordOrder edits.Current parent suggestion |> saveEdits |> ignore
                            with ex ->
                                downloadProgress.Set None
                                status.Set $"Couldn't find page {page}'s layout: {ex.Message}"
                        finally
                            ordering.Set false
                }
                |> ignore

            // Above a tagged PDF's blocks: the reading order (see StructureActions.orderBar).
            let orderBar () : IView =
                StructureActions.orderBar
                    { Page = shownPage.Current
                      Selected = selected.Current
                      Parents = parents ()
                      Edits = edits.Current
                      Ordering = ordering.Current
                      Download = downloadProgress.Current
                      CanOrderByLayout = (topLevel ()).IsSome
                      OrderByLayout = orderByLayout
                      SaveEdits = fun next -> saveEdits next |> ignore }

            /// Every block in the order shown (moves included), depth first.
            let shownOrder () =
                let rec under (node: StructureNode) : StructureNode list =
                    [ for child in orderedChildren edits.Current node do
                          yield child
                          yield! under child ]
                under root.Current
            // The blocks with failures or warnings, in that order, each with its place in it:
            // marked beside the blocks (see FindingRuler), and gone to with the arrows below them.
            let findingMarks =
                if isSuggested || placed.Count = 0 then []
                else
                    let order = shownOrder ()
                    FindingRuler.marks order placed |> List.map (fun mark -> mark, order |> List.findIndex (fun n -> obj.ReferenceEquals(n, mark.Node)))
            /// Selects node's block and moves the focus to it, so it's said.
            let goToBlock (node: StructureNode) = selectNodeThen node false (fun border -> border.Focus() |> ignore)
            /// The next (or previous) marked block after the selected one, round to the first (or last).
            let nextFinding (forward: bool) =
                let here =
                    match selected.Current with
                    | first :: _ -> shownOrder () |> List.tryFindIndex (fun n -> obj.ReferenceEquals(n, first))
                    | [] -> None
                let candidates = if forward then findingMarks else List.rev findingMarks
                let after (index: int) =
                    match here with
                    | Some here -> if forward then index > here else index < here
                    | None -> true
                candidates
                |> List.tryFind (fun (_, index) -> after index)
                |> Option.orElse (List.tryHead candidates)
                |> Option.iter (fun (mark, _) -> goToBlock mark.Node)
            /// The pages with blocks on, in order.
            let blockPages () =
                shownOrder ()
                |> List.choose (fun n -> n.Geometry |> Option.ofObj |> Option.map _.PageNumber)
                |> List.distinct
                |> List.sort
            /// Goes to page's first block, scrolled to the top so the page's blocks follow it.
            let goToPage (page: int) =
                let toTop (border: Border) =
                    match border.FindAncestorOfType<ScrollViewer>() with
                    | NonNull scroll ->
                        match scroll.Content with
                        | :? Visual as content ->
                            match border.TranslatePoint(Point(0.0, 0.0), content) |> Option.ofNullable with
                            | Some top -> scroll.Offset <- Vector(scroll.Offset.X, max 0.0 (top.Y - 8.0))
                            | None -> ()
                        | _ -> ()
                    | Null -> ()
                    border.Focus() |> ignore
                shownOrder ()
                |> List.tryFind (fun n -> match n.Geometry with NonNull g -> g.PageNumber = page | Null -> false)
                |> Option.iter (fun node -> selectNodeThen node false toTop)
            let ruler = ctx.useStateLazy((fun () -> FindingRuler.Ruler()), false).Current
            ruler.Marks <- List.map fst findingMarks
            ruler.ParentOf <- fun node -> match (parents ()).TryGetValue node with | true, parent -> Some parent | _ -> None
            ruler.Go <- goToBlock
            ruler.Describe <- StructureBlocks.blockName
            ruler.Refresh()

            // Below the blocks (away from Move Up and Down): going to a page, and to the blocks
            // with findings, each with arrows for the one before and after.
            let navigationBar () : IView =
                let arrow (up: bool) (name: string) (key: KeyGesture) (enabled: bool) (go: unit -> unit) =
                    Button.create [
                        Button.content (Icons.iconPath (if up then Icons.chevronUp else Icons.chevronDown) Theme.Tokens.Primary 12.0 [])
                        Button.padding (7.0, 6.0)
                        Button.margin (2.0, 0.0, 0.0, 4.0)
                        Button.isEnabled enabled
                        AttrBuilder<Button>.CreateProperty<KeyGesture | null>(Button.HotKeyProperty, key, ValueNone)
                        AttrBuilder<Button>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, name, ValueNone)
                        ToolTip.tip $"{name} ({key})"
                        Button.onClick ((fun _ -> go ()), SubPatchOptions.Always)
                    ]
                let label (text: string) (left: float) =
                    TextBlock.create [
                        TextBlock.text text
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        TextBlock.margin (left, 0.0, 6.0, 4.0)
                    ]
                let pages = blockPages ()
                let page = shownPage.Current
                let before = pages |> List.filter (fun p -> page |> Option.exists (fun page -> p < page)) |> List.tryLast
                let after = pages |> List.tryFind (fun p -> match page with Some page -> p > page | None -> true)
                let failing = findingMarks |> List.filter (fun (mark, _) -> mark.Failure) |> List.length
                let warning = findingMarks.Length - failing
                let blocks (n: int) = if n = 1 then "1 block" else $"{n} blocks"
                let summary =
                    [ if failing > 0 then $"{blocks failing} with failures"
                      if warning > 0 then $"{blocks warning} with warnings" ]
                    |> String.concat ", "
                WrapPanel.create [
                    DockPanel.dock Dock.Bottom
                    WrapPanel.margin (8.0, 6.0, 8.0, 0.0)
                    WrapPanel.children [
                        if pages.Length > 1 then
                            label "Page" 0.0
                            ComboBox.create [
                                ComboBox.dataItems pages
                                ComboBox.selectedIndex (match page with Some page -> pages |> List.tryFindIndex ((=) page) |> Option.defaultValue -1 | None -> -1)
                                ComboBox.minWidth 64.0
                                ComboBox.margin (0.0, 0.0, 0.0, 4.0)
                                AttrBuilder<ComboBox>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, "Go to page", ValueNone)
                                ToolTip.tip "Go to a page's first block."
                                ComboBox.onSelectedItemChanged (
                                    (fun item ->
                                        match item with
                                        | :? int as picked when Some picked <> shownPage.Current -> goToPage picked
                                        | _ -> ()),
                                    SubPatchOptions.Always
                                )
                            ]
                            label $"of {List.last pages}" 6.0
                            arrow true "Previous page" (KeyGesture(Key.PageUp, KeyModifiers.Alt)) before.IsSome (fun () -> before |> Option.iter goToPage)
                            arrow false "Next page" (KeyGesture(Key.PageDown, KeyModifiers.Alt)) after.IsSome (fun () -> after |> Option.iter goToPage)
                        if not findingMarks.IsEmpty then
                            // The counts as the blocks' markers show them; said in full.
                            StackPanel.create [
                                StackPanel.orientation Orientation.Horizontal
                                StackPanel.verticalAlignment VerticalAlignment.Center
                                StackPanel.margin ((if pages.Length > 1 then 20.0 else 0.0), 0.0, 6.0, 4.0)
                                StackPanel.spacing 4.0
                                ToolTip.tip summary
                                AttrBuilder<StackPanel>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, summary, ValueNone)
                                StackPanel.children [
                                    if failing > 0 then
                                        Icons.iconPath Icons.failure Theme.Tokens.Danger 14.0 []
                                        TextBlock.create [ TextBlock.text (string failing); TextBlock.verticalAlignment VerticalAlignment.Center; TextBlock.margin (0.0, 0.0, 6.0, 0.0) ]
                                    if warning > 0 then
                                        Icons.iconPath Icons.warning Theme.Tokens.Warning 14.0 []
                                        TextBlock.create [ TextBlock.text (string warning); TextBlock.verticalAlignment VerticalAlignment.Center ]
                                ]
                            ]
                            arrow true "Previous block with failures or warnings" (KeyGesture(Key.F8, KeyModifiers.Shift)) true (fun () -> nextFinding false)
                            arrow false "Next block with failures or warnings" (KeyGesture(Key.F8)) true (fun () -> nextFinding true)
                    ]
                ]

            let layoutTab =
                Grid.create [
                    Grid.columnDefinitions [
                        ColumnDefinition(GridLength(3.0, GridUnitType.Star), MinWidth = 320.0)
                        ColumnDefinition(GridLength 6.0)
                        ColumnDefinition(GridLength(2.0, GridUnitType.Star), MinWidth = 260.0)
                    ]
                    Grid.children [
                        DockPanel.create [
                          DockPanel.children [
                        if not isSuggested then orderBar ()
                        navigationBar ()
                        if not findingMarks.IsEmpty then
                            ContentControl.create [
                                DockPanel.dock Dock.Right
                                ContentControl.content ruler
                            ]
                        // Its own component, so preview updates don't re-render (and re-diff) the layout.
                        Component.create("structure-layout", fun ctx ->
                        let root = ctx.usePassedRead root
                        ScrollViewer.create [
                            ScrollViewer.name FindingRuler.blocksName
                            ScrollViewer.verticalScrollBarVisibility Primitives.ScrollBarVisibility.Auto
                            ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Disabled
                            Theme.resources [ ScrollViewer.BackgroundProperty, Theme.Tokens.Background ]
                            ScrollViewer.content (
                                StackPanel.create [
                                    StackPanel.margin 8.0
                                    // The blocks are one tab stop: Up and Down move between them (see StructureBlocks).
                                    AttrBuilder<StackPanel>.CreateProperty<KeyboardNavigationMode>(
                                        KeyboardNavigation.TabNavigationProperty, KeyboardNavigationMode.Once, ValueNone)
                                    StackPanel.children [
                                        let levels = levelsWithinBudget root.Current.Children
                                        for child in root.Current.Children -> element selection (Thickness(0.0, 0.0, 0.0, 6.0)) (levels - 1) child
                                    ]
                                ]
                            )
                        ])
                          ]
                        ]
                        GridSplitter.create [
                            Grid.column 1
                            GridSplitter.width 6.0
                            GridSplitter.horizontalAlignment HorizontalAlignment.Stretch
                            Theme.resources [ GridSplitter.BackgroundProperty, Theme.Tokens.Border ]
                            GridSplitter.resizeDirection GridResizeDirection.Columns
                        ]
                        Grid.create [
                            Grid.column 2
                            Grid.margin (10.0, 0.0, 0.0, 0.0)
                            Grid.rowDefinitions "Auto,Auto,Auto,*"
                            Grid.children [
                                DockPanel.create [
                                    DockPanel.margin (0.0, 0.0, 0.0, 6.0)
                                    DockPanel.children [
                                        TextBlock.create [
                                            TextBlock.text "Selected element"
                                            TextBlock.fontWeight FontWeight.SemiBold
                                            TextBlock.verticalAlignment VerticalAlignment.Center
                                        ]
                                    ]
                                ]
                                StackPanel.create [
                                  Grid.row 1
                                  StackPanel.margin (0.0, 0.0, 0.0, 8.0)
                                  StackPanel.children [
                                    TextBlock.create [
                                        TextBlock.text previewNote.Current
                                        TextBlock.textWrapping TextWrapping.Wrap
                                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                    ]
                                  ]
                                ]
                                StructureActions.selectionActions
                                    { Selected = selected
                                      Edits = edits
                                      Suggested = isSuggested
                                      SaveEdits = saveEdits
                                      MarkAllLike = markAllLike
                                      EditNode = editNode }
                                // The selected block's page, and for a link, the page it goes to below it.
                                // Clicking an element on the page selects it (shift-click extends).
                                let imageBox (row: int) (image: Bitmap option) (clickable: bool) =
                                    Border.create [
                                        Grid.row row
                                        Theme.resources
                                            [ Border.BorderBrushProperty, Theme.Tokens.Border
                                              Border.BackgroundProperty, Theme.Tokens.Fill ]
                                        Border.borderThickness 1.0
                                        Border.padding 6.0
                                        Border.child (
                                            Image.create [
                                                match image with
                                                | Some image -> Image.source image
                                                | None -> ()
                                                Image.stretch Stretch.Uniform
                                                if clickable then
                                                    Image.onPointerPressed (
                                                        (fun e ->
                                                            match previewNodeAt e with
                                                            | Some(image, node) when e.GetCurrentPoint(image).Properties.IsLeftButtonPressed ->
                                                                e.Handled <- true
                                                                selectNode node (e.KeyModifiers.HasFlag KeyModifiers.Shift)
                                                            | _ -> ()),
                                                        SubPatchOptions.Always
                                                    )
                                                    // A hand over an element, to say it can be clicked.
                                                    Image.onPointerMoved (
                                                        (fun e ->
                                                            match e.Source with
                                                            | :? Image as image ->
                                                                let cursor = if (previewNodeAt e).IsSome then handCursor.Value else Cursor.Default
                                                                if not (obj.ReferenceEquals(image.Cursor, cursor)) then image.Cursor <- cursor
                                                            | _ -> ()),
                                                        SubPatchOptions.Always
                                                    )
                                            ]
                                        )
                                    ]
                                Grid.create [
                                    Grid.row 3
                                    Grid.rowDefinitions (if targetImage.Current.IsSome then "*,Auto,Auto,*" else "*,Auto")
                                    Grid.children [
                                        imageBox 0 previewImage.Current true
                                        CheckBox.create [
                                            Grid.row 1
                                            CheckBox.margin (0.0, 6.0, 0.0, 0.0)
                                            CheckBox.content "Show selected block only"
                                            CheckBox.isChecked selectionOnly.Current
                                            ToolTip.tip "Outline only the selected block on the page above, not every block: for a busy page. Clicking the page still selects what's there."
                                            CheckBox.onIsCheckedChanged (
                                                (fun e ->
                                                    match e.Source with
                                                    | :? CheckBox as checkBox ->
                                                        let only = checkBox.IsChecked.GetValueOrDefault()
                                                        if only <> selectionOnly.Current then
                                                            selectionOnly.Set only
                                                            selectionOnlyThisSession <- only
                                                            showPage selected.Current
                                                    | _ -> ()),
                                                SubPatchOptions.Always
                                            )
                                        ]
                                        if targetImage.Current.IsSome then
                                            TextBlock.create [
                                                Grid.row 2
                                                TextBlock.text targetNote.Current
                                                TextBlock.textWrapping TextWrapping.Wrap
                                                Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                                TextBlock.margin (0.0, 8.0, 0.0, 8.0)
                                            ]
                                            imageBox 3 targetImage.Current false
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]

            /// Every edit for this PDF, and whether it's in the PDF yet (see StructureEditsTab).
            let editsTab =
                StructureEditsTab.view
                    { Edits = selection.Edits
                      Root = root
                      Review = review
                      Suggested = suggested
                      SourceFilePath = sourceFilePath
                      MovePending = movePending
                      SaveEdits = fun next -> saveEdits next |> ignore }

            let treeTab =
                TreeView.create [
                    TreeView.dataItems root.Current.Children
                    TreeView.itemTemplate (
                        DataTemplateView<_>.create (
                            Func<StructureNode, StructureNode seq>(fun node -> node.Children),
                            fun node ->
                                TextBlock.create [
                                    TextBlock.text (StructureSummary.format node)
                                    TextBlock.fontFamily Fonts.Monospace
                                    TextBlock.fontSize 12.0
                                ]
                        )
                    )
                ]

            // The document's title. For a suggested layout, prefilled as the title fixes would find
            // it (the document's own, else a short first page), else the first H1 found; for a
            // tagged PDF, its own title (empty if it has none), the others offered. Only a change
            // from what's prefilled is saved, and Export writes it.
            let titleEdit = ctx.useState review.Title
            let documentCandidates = ctx.useStateLazy(fun () -> try DocumentTitles.candidates sourceFilePath with _ -> []).Current
            // PDF/UA's title failures (no title in the metadata; the file name shown in its place),
            // marked on the title's row.
            let titleFindings =
                if isSuggested then [] else findings.Items |> List.filter FixNames.isTitleRule

            let titleRow (review: DocumentReview) =
                let tidy (text: string) = Text.RegularExpressions.Regex.Replace(text, @"\s+", " ").Trim()
                let headingCandidate =
                    if isSuggested then
                        match review.Pages.TryGetValue 1 with
                        | true, first ->
                            first.Blocks
                            |> List.tryFind (fun block -> RegionEdits.tagOf edits.Current 1 block = "H1")
                            |> Option.bind (fun block ->
                                RegionEdits.textFor edits.Current 1 block
                                |> Option.orElse (Some block.Text)
                                |> Option.map tidy
                                |> Option.filter (String.IsNullOrWhiteSpace >> not))
                        | _ -> None
                    else
                        let rec firstHeading (node: StructureNode) : string option =
                            if node.Tag = "H1" then Option.ofObj node.Content |> Option.map tidy |> Option.filter (String.IsNullOrWhiteSpace >> not)
                            else node.Children |> Seq.tryPick firstHeading
                        firstHeading root.Current
                    |> Option.map (fun title -> { DocumentTitles.Title = title; DocumentTitles.Source = "first heading" })
                let candidates = documentCandidates @ Option.toList headingCandidate |> List.distinctBy _.Title
                let prefilled =
                    if isSuggested then candidates |> List.tryHead |> Option.map _.Title |> Option.defaultValue ""
                    else
                        // Its own title, only: one taken from its first page or heading is offered.
                        candidates
                        |> List.tryFind (fun c -> c.Source <> "first page" && c.Source <> "first heading")
                        |> Option.map _.Title
                        |> Option.defaultValue ""
                if isSuggested then review.ShownTitle <- (if prefilled = "" then None else Some prefilled)
                let shownTitle = titleEdit.Current |> Option.defaultValue prefilled
                let setTitle (text: string) =
                    let next = if text = prefilled then None else Some text
                    if next <> titleEdit.Current then
                        titleEdit.Set next
                        review.Title <- next
                        onEditsChanged ()
                Grid.create [
                    Grid.columnDefinitions "Auto,*"
                    Grid.rowDefinitions "Auto,Auto"
                    Grid.children [
                        StackPanel.create [
                            StackPanel.orientation Orientation.Horizontal
                            StackPanel.verticalAlignment VerticalAlignment.Center
                            StackPanel.margin (0.0, 0.0, 10.0, 0.0)
                            StackPanel.children [
                                TextBlock.create [
                                    TextBlock.text "Document title"
                                    TextBlock.fontWeight FontWeight.SemiBold
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                    TextBlock.margin (0.0, 0.0, 6.0, 0.0)
                                ]
                                match findingsMarker titleFindings findings.Show with
                                | Some marker -> marker
                                | None -> ()
                            ]
                        ]
                        TextBox.create [
                            Grid.column 1
                            TextBox.text shownTitle
                            AttrBuilder<TextBox>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, "Document title", ValueNone)
                            TextBox.placeHolderText "What a screen reader and the title bar call this document"
                            ToolTip.tip (
                                if isSuggested then "Given to the tagged copy as its title, shown in place of the file name. Save keeps a change next to the PDF."
                                else "Export… gives the copy this title, shown in place of the file name. Save keeps a change next to the PDF."
                            )
                            // What's prefilled can change as the first page is found.
                            TextBox.onTextChanged (setTitle, SubPatchOptions.Always)
                        ]
                        WrapPanel.create [
                            Grid.row 1
                            Grid.column 1
                            WrapPanel.margin (0.0, 4.0, 0.0, 0.0)
                            WrapPanel.isVisible (candidates |> List.exists (fun c -> c.Title <> shownTitle))
                            WrapPanel.children [
                                yield
                                    TextBlock.create [
                                        TextBlock.text "Use:"
                                        TextBlock.verticalAlignment VerticalAlignment.Center
                                        TextBlock.margin (0.0, 0.0, 6.0, 0.0)
                                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                    ]
                                    :> IView
                                for candidate in candidates do
                                    if candidate.Title <> shownTitle then
                                        yield
                                            Button.create [
                                                Button.margin (0.0, 0.0, 6.0, 0.0)
                                                Button.padding (8.0, 2.0)
                                                Button.content $"{candidate.Title} · {candidate.Source}"
                                                Button.onClick ((fun _ -> setTitle candidate.Title), SubPatchOptions.Always)
                                            ]
                                            :> IView
                            ]
                        ]
                    ]
                ]

            /// For a suggested layout: that it isn't tagged yet, how far finding it has got, and
            /// the layout model to download first if it isn't yet.
            let banner (review: DocumentReview) (prefetcher: LayoutPrefetcher) =
                let secondary (text: string) =
                    TextBlock.create [
                        TextBlock.text text
                        TextBlock.textWrapping TextWrapping.Wrap
                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                    ]
                    :> IView
                let progress =
                    [ if readyCount.Current >= prefetcher.PageCount then "Every page's layout found."
                      elif modelReady.Current then $"{readyCount.Current} of {prefetcher.PageCount} pages' layouts found; the rest are being found (several seconds a page)."
                      else $"{readyCount.Current} of {prefetcher.PageCount} pages' layouts found."
                      if readingTitles.Current > 0 then "Reading the titles of pages QR codes link to…"
                      for page in review.Pages.Values |> Seq.sortBy _.PageNumber do
                          if page.Rotated then $"Page {page.PageNumber} is rotated, and rotated pages aren't detected yet."
                          elif page.Blocks.IsEmpty then $"Nothing was found on page {page.PageNumber}."
                          elif not page.HasText then
                              $"Page {page.PageNumber} has no text layer (it may be a scan), so tagging it would need OCR first."
                      for KeyValue(page, reason) in failures.Current do
                          $"Couldn't find page {page}'s layout: {reason}" ]
                Border.create [
                    Border.padding 10.0
                    Border.cornerRadius 4.0
                    Border.borderThickness 1.0
                    Theme.resources
                        [ Border.BorderBrushProperty, Theme.Tokens.Border
                          Border.BackgroundProperty, Theme.Tokens.Fill ]
                    Border.child (
                        StackPanel.create [
                            StackPanel.spacing 6.0
                            StackPanel.children [
                                TextBlock.create [
                                    TextBlock.text "Not tagged yet: this is the structure suggested for it"
                                    TextBlock.fontWeight FontWeight.SemiBold
                                ]
                                secondary (
                                    "This PDF has no tags, so this is the structure a layout detector suggests from what each page looks like: "
                                    + "its regions, the tag each suggests, and their reading order (the numbers on the page). "
                                    + "What you change is kept as edits on top: Save in the main window keeps them, and Export… tags a copy with them."
                                )
                                secondary (String.Join(" ", progress))
                                if not modelReady.Current then
                                    secondary (
                                        $"Finding the layout needs a layout detection model (PP-DocLayoutV3, about {LayoutModel.DownloadSizeMegabytes} MB). "
                                        + $"It's downloaded once from {LayoutModel.source ()} into {LayoutModel.directory ()}, and runs on this computer: "
                                        + "the PDF isn't sent anywhere."
                                    )
                                    match downloadProgress.Current with
                                    | Some fraction ->
                                        ProgressBar.create [ ProgressBar.minimum 0.0; ProgressBar.maximum 1.0; ProgressBar.value fraction ]
                                    | None ->
                                        Button.create [
                                            Button.content "Download model"
                                            Button.horizontalAlignment HorizontalAlignment.Left
                                            Button.onClick download
                                        ]
                                if status.Current <> "" then
                                    TextBlock.create [
                                        TextBlock.text status.Current
                                        TextBlock.textWrapping TextWrapping.Wrap
                                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.Danger ]
                                    ]
                            ]
                        ]
                    )
                ]

            Grid.create [
                Grid.margin 12.0
                Grid.rowDefinitions "Auto,*,Auto"
                Grid.children [
                    StackPanel.create [
                        StackPanel.margin (0.0, 0.0, 0.0, 8.0)
                        StackPanel.spacing 10.0
                        StackPanel.children [
                            match suggested with
                            | Some(review, prefetcher) ->
                                banner review prefetcher
                                titleRow review
                            | None ->
                                TextBlock.create [
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                    TextBlock.text
                                        "This is the tagged logical structure tree — the semantic heading/paragraph/list/table/link hierarchy that determines what content and reading order a screen reader presents to a user. Only elements that directly own page content show a page number."
                                ]
                                titleRow review
                        ]
                    ]
                    TabControl.create [
                        Grid.row 1
                        TabControl.viewItems [
                            // "Layout" is a suggested structure's: this is the elements drawn as blocks.
                            TabItem.create [ TabItem.header "Blocks"; TabItem.content layoutTab ]
                            TabItem.create [ TabItem.header "Tree"; TabItem.content treeTab ]
                            TabItem.create [
                                TabItem.header (if edits.Current.IsEmpty then "Edits" else $"Edits ({edits.Current.Length})")
                                TabItem.content editsTab
                            ]
                        ]
                    ]
                    StackPanel.create [
                        Grid.row 2
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.horizontalAlignment HorizontalAlignment.Right
                        StackPanel.margin (0.0, 8.0, 0.0, 0.0)
                        StackPanel.children [
                            Button.create [
                                Button.content "Import Edits…"
                                Button.padding (12.0, 4.0)
                                Button.margin (0.0, 0.0, 8.0, 0.0)
                                ToolTip.tip
                                    "Use the edits made for another PDF, such as an earlier version of this one: pick that PDF (its edits saved next to it, or inside a copy Legibyl saved), or the .legibyl.json file saved for it."
                                Button.onClick importEdits
                            ]
                            Button.create [
                                Button.content "Export JSON…"
                                Button.padding (12.0, 4.0)
                                Button.margin (0.0, 0.0, 8.0, 0.0)
                                ToolTip.tip "Save this hierarchy as JSON — useful for feeding to an AI agent or other tooling."
                                Button.onClick exportJson
                            ]
                            Button.create [
                                Button.content "Close"
                                Button.padding (12.0, 4.0)
                                Button.isCancel true
                                Button.onClick (fun _ -> window.Close())
                            ]
                        ]
                    ]
                ]
            ])

open StructureView

/// Edits change review; onEditsChanged is called after each (or a suggested layout's title changed).
/// findings are marked on the blocks they're about (a tagged PDF's; see FindingMarkers).
type StructureTreeWindow
    (source: StructureSource, sourceFilePath: string, review: DocumentReview, onEditsChanged: unit -> unit, findings: FindingMarkers.Findings) as this =
    inherit
        HostWindow(
            Title = (match source with Tagged _ -> "Structure" | Suggested -> "Suggested Structure"),
            Width = 1000.0,
            Height = 760.0,
            MinWidth = 500.0,
            MinHeight = 400.0
        )
    // Keeps the PDF open and recent pages rendered while the window is open.
    let renderer = new PdfRegionRenderer()
    // For a suggested layout: what finds each page's, in the background.
    let finding =
        match source with
        | Suggested ->
            let suggester = new LayoutSuggester(sourceFilePath)
            Some(suggester, LayoutPrefetcher(suggester, review.Pages))
        | Tagged _ -> None
    // Where Order Page N by Layout gets a page's regions: the layout detector, unless replaced.
    let layoutRegions = ref None
    do
        let tagged = match source with Tagged root -> root | Suggested -> SuggestedTree.build []
        this.Content <- view this renderer tagged review (finding |> Option.map snd) sourceFilePath onEditsChanged findings layoutRegions

    /// The review its edits change.
    member _.Review = review

    /// Where Order Page N by Layout gets a page's regions, in reading order: None for the layout
    /// detector (downloading its model first, if need be).
    member _.LayoutRegions
        with get (): (int -> StructureGeometry list) option = layoutRegions.Value
        and set (value: (int -> StructureGeometry list) option) = layoutRegions.Value <- value

    /// Without findings to mark.
    new(source: StructureSource, sourceFilePath: string, review: DocumentReview, onEditsChanged: unit -> unit) =
        new StructureTreeWindow(source, sourceFilePath, review, onEditsChanged, FindingMarkers.none)

    /// A tagged PDF's structure, with a review of its own (read from the file next to it).
    new(root: StructureNode, sourceFilePath: string) =
        new StructureTreeWindow(Tagged root, sourceFilePath, DocumentReviews().For sourceFilePath, ignore)

    override _.OnClosed(e) =
        base.OnClosed e
        match finding with
        | Some(suggester, prefetcher) ->
            // A page may still be being found: stop after it, then let go of the suggester.
            Task.Run(fun () ->
                prefetcher.Stop()
                (suggester :> IDisposable).Dispose())
            |> ignore
        | None -> ()
        renderer.Dispose()
