module Legibyl.Gui.Dialogs

open System.IO
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Platform.Storage
open Legibyl.Validation

/// Shows a save dialog starting in nearPath's folder and returns the chosen local
/// path, or None if the user cancelled.
let pickSavePath (topLevel: TopLevel) (options: FilePickerSaveOptions) (nearPath: string) : Task<string option> =
    task {
        match Path.GetDirectoryName nearPath with
        | NonNull folder when folder <> "" ->
            let! start = topLevel.StorageProvider.TryGetFolderFromPathAsync folder
            options.SuggestedStartLocation <- start
        | _ -> ()

        let! file = topLevel.StorageProvider.SaveFilePickerAsync options
        return file |> Option.ofObj |> Option.bind (fun f -> Option.ofObj (f.TryGetLocalPath()))
    }

let private dialogWindow (title: string) (width: float) =
    HostWindow(
        Title = title,
        Width = width,
        SizeToContent = SizeToContent.Height,
        CanResize = false,
        ShowInTaskbar = false,
        WindowStartupLocation = WindowStartupLocation.CenterOwner
    )

/// Minimal modal OK-only message box. Avalonia has no built-in equivalent of WPF's
/// MessageBox, and this is all the app needs.
let showMessage (owner: Window) (message: string) (title: string) : Task =
    let window = dialogWindow title 460.0
    window.Content <-
        Component(fun _ ->
            StackPanel.create [
                StackPanel.margin 16.0
                StackPanel.spacing 16.0
                StackPanel.children [
                    SelectableTextBlock.create [
                        SelectableTextBlock.text message
                        SelectableTextBlock.textWrapping TextWrapping.Wrap
                    ]
                    Button.create [
                        Button.content "OK"
                        Button.minWidth 82.0
                        Button.isDefault true
                        Button.isCancel true
                        Button.horizontalAlignment HorizontalAlignment.Right
                        Button.horizontalContentAlignment HorizontalAlignment.Center
                        Button.onClick (fun _ -> window.Close())
                    ]
                ]
            ])
    window.ShowDialog owner

/// A message with an action button beside Close; true if the user chose the action.
let showMessageWithAction (owner: Window) (message: string) (title: string) (action: string) : Task<bool> =
    let window = dialogWindow title 460.0
    let mutable chosen = false
    window.Content <-
        Component(fun _ ->
            StackPanel.create [
                StackPanel.margin 16.0
                StackPanel.spacing 16.0
                StackPanel.children [
                    SelectableTextBlock.create [
                        SelectableTextBlock.text message
                        SelectableTextBlock.textWrapping TextWrapping.Wrap
                    ]
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.horizontalAlignment HorizontalAlignment.Right
                        StackPanel.spacing 8.0
                        StackPanel.children [
                            Button.create [
                                Button.content action
                                Button.isDefault true
                                Button.onClick (fun _ ->
                                    chosen <- true
                                    window.Close())
                            ]
                            Button.create [
                                Button.content "Close"
                                Button.minWidth 82.0
                                Button.isCancel true
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.onClick (fun _ -> window.Close())
                            ]
                        ]
                    ]
                ]
            ])
    task {
        do! window.ShowDialog owner
        return chosen
    }

/// A repair the Fix dialog offers.
type private FixChoice =
    { Option: PdfFixOptions
      Title: string
      Detail: string
      /// Stays unticked even when found: it changes what the document says it is, or guesses.
      OptIn: bool }

/// The repairs, in the order they run: tagging first, then StructureFixes.orderedFixes, then
/// veraPDF's metadata fix.
let private fixChoices =
    [ { Option = PdfFixOptions.TagFromLayout
        Title = "Tag from the page layout"
        Detail =
          "Headings, paragraphs, lists, figures and reading order, found by a layout detector; the page looks the same. "
          + $"Several seconds a page; downloads the layout model (about {Legibyl.Layout.LayoutModel.DownloadSizeMegabytes} MB) the first time."
        OptIn = true }
      { Option = PdfFixOptions.QrCodeLinks
        Title = "Link QR codes"
        Detail = "With tagging: a borderless link over each QR code that encodes a web address."
        OptIn = true }
      { Option = PdfFixOptions.LanguageFromText
        Title = "Declare the language"
        Detail = "Set /Lang from the language the text is in, when there's enough text to be sure."
        OptIn = true }
      { Option = PdfFixOptions.HeadingsFromToc
        Title = "Headings from the table of contents"
        Detail = "Tag paragraphs a contents entry names as headings (H1–H6, from its indent), and point its links at them."
        OptIn = true }
      { Option = PdfFixOptions.RemoveEmptyParagraphs
        Title = "Remove empty paragraphs"
        Detail = "Empty /P structure elements (optional cleanup)."
        OptIn = true }
      { Option = PdfFixOptions.RemoveEmptyTocEntries
        Title = "Remove empty contents entries"
        Detail = "Empty table-of-contents entries (TOCI) (optional cleanup)."
        OptIn = true }
      { Option = PdfFixOptions.ListsFromParagraphs
        Title = "Tag lists"
        Detail = "Runs of paragraphs that start with list markers (•, (1), a.) become L/LI; the page looks the same."
        OptIn = true }
      { Option = PdfFixOptions.BookmarksFromHeadings
        Title = "Add bookmarks"
        Detail = "Bookmarks for the H1 and H2 headings, when the document has none."
        OptIn = false }
      { Option = PdfFixOptions.AnnotationTabs
        Title = "Tab order for annotations"
        Detail = "Set /Tabs /S on pages that contain annotations, so keyboard order follows the structure."
        OptIn = false }
      { Option = PdfFixOptions.FontCidToGidMap
        Title = "Font glyph mapping"
        Detail = "Write the /CIDToGIDMap /Identity embedded TrueType CID fonts leave out; readers already use it, so nothing changes on the page."
        OptIn = false }
      { Option = PdfFixOptions.LinkContents
        Title = "Describe contents links"
        Detail = "Give table-of-contents links without a description the entry's own text (/Contents)."
        OptIn = false }
      { Option = PdfFixOptions.LinkTitlesFromWeb
        Title = "Describe web links from their pages"
        Detail =
          "Visits each web address a link without a description (or, with tagging, a QR code) goes to, and uses the page's title, "
          + "with the link's own site: \"Apply for benefits (example.com)\". Only public web pages are visited, and only their titles read; "
          + "the sites see the visit. Check the descriptions."
        OptIn = true }
      { Option = PdfFixOptions.DocumentTitle
        Title = "Document title"
        Detail = "Use the title the document carries, and show it in place of the file name (/DisplayDocTitle)."
        OptIn = false }
      { Option = PdfFixOptions.TitleFromFirstPage
        Title = "Title from the first page"
        Detail = "If the document has no title anywhere, use the text of a short first page (a title page)."
        OptIn = true }
      { Option = PdfFixOptions.Metadata
        Title = "Repair XMP metadata"
        Detail = "veraPDF's repair: metadata stream, PDF/UA identifier, Info sync."
        OptIn = false } ]

/// Lets the user pick which repairs to run, showing how many of the report's findings each
/// one addresses. Lists only the repairs this PDF needs, unless asked for all of them.
/// Returns None if the user cancelled.
/// tagNote, if any, says what tagging from the layout will use (a review in Suggest Structure).
let chooseFixOptions (owner: Window) (report: ValidationReport) (tagNote: string option) : Task<PdfFixOptions option> =
    let window = dialogWindow "Fix PDF" 560.0
    // Never taller than the screen: the list scrolls in whatever room is left. (All the repairs
    // at once are taller than a laptop screen.)
    window.MaxHeight <-
        match owner.Screens.ScreenFromWindow owner with
        | NonNull screen -> float screen.WorkingArea.Height / screen.Scaling * 0.9
        | Null -> 700.0
    let found (option: PdfFixOptions) = report.FixableCount option
    /// Whether this PDF has something for the repair to do. Linking QR codes comes with tagging,
    /// and so can describing them from the pages they go to.
    let withTagging (choice: FixChoice) =
        (choice.Option = PdfFixOptions.QrCodeLinks || choice.Option = PdfFixOptions.LinkTitlesFromWeb)
        && found PdfFixOptions.TagFromLayout > 0
    let applies (choice: FixChoice) = found choice.Option > 0 || withTagging choice
    // veraPDF only repairs metadata (adding the PDF/UA identifier) when the document would then
    // conform, so it's only worth ticking when every failure can be fixed here.
    let everyFailureFixable =
        report.Findings |> Seq.forall (fun f -> f.Passed || f.Severity <> FindingSeverity.Failure || f.IsFixable)
    let tickedAtFirst (choice: FixChoice) =
        applies choice && not choice.OptIn && (choice.Option <> PdfFixOptions.Metadata || everyFailureFixable)
    window.Content <-
        Component(fun ctx ->
            let selected =
                ctx.useState (fixChoices |> List.filter tickedAtFirst |> List.fold (fun all c -> all ||| c.Option) PdfFixOptions.None)
            let showAll = ctx.useState false
            let needed = fixChoices |> List.filter applies

            /// The repair's title and detail, with how many findings it addresses on the right.
            let row (choice: FixChoice) =
                let count =
                    match found choice.Option with
                    | 0 when withTagging choice -> "with tagging"
                    | 0 -> "none found"
                    | n -> $"{n} found"
                let detail =
                    if choice.Option = PdfFixOptions.TagFromLayout && tagNote.IsSome then
                        $"{choice.Detail} {tagNote.Value}"
                    elif choice.Option = PdfFixOptions.Metadata && applies choice && not everyFailureFixable then
                        $"{choice.Detail} Unticked: veraPDF only repairs metadata once every other failure is fixed, and some here can't be fixed automatically."
                    else
                        choice.Detail
                CheckBox.create [
                    CheckBox.margin (0.0, 0.0, 0.0, 8.0)
                    CheckBox.horizontalAlignment HorizontalAlignment.Stretch
                    CheckBox.horizontalContentAlignment HorizontalAlignment.Stretch
                    CheckBox.isChecked (selected.Current.HasFlag choice.Option)
                    CheckBox.onIsCheckedChanged (fun e ->
                        match e.Source with
                        | :? CheckBox as checkBox ->
                            let isChecked = checkBox.IsChecked.GetValueOrDefault()
                            if isChecked <> selected.Current.HasFlag choice.Option then
                                selected.Set(if isChecked then selected.Current ||| choice.Option else selected.Current &&& ~~~choice.Option)
                        | _ -> ())
                    CheckBox.content (
                        Grid.create [
                            Grid.columnDefinitions "*,Auto"
                            Grid.children [
                                StackPanel.create [
                                    StackPanel.children [
                                        WrapPanel.create [
                                            WrapPanel.children [
                                                TextBlock.create [ TextBlock.text choice.Title; TextBlock.fontWeight FontWeight.SemiBold ]
                                                if choice.OptIn then
                                                    TextBlock.create [
                                                        TextBlock.text "opt-in"
                                                        TextBlock.fontSize 11.0
                                                        TextBlock.margin (8.0, 2.0, 0.0, 0.0)
                                                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                                    ]
                                            ]
                                        ]
                                        TextBlock.create [
                                            TextBlock.text detail
                                            TextBlock.fontSize 12.0
                                            TextBlock.textWrapping TextWrapping.Wrap
                                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                        ]
                                    ]
                                ]
                                Border.create [
                                    Grid.column 1
                                    Border.margin (12.0, 1.0, 0.0, 0.0)
                                    Border.padding (8.0, 1.0)
                                    Border.cornerRadius 8.0
                                    Border.verticalAlignment VerticalAlignment.Top
                                    Theme.resources [ Border.BackgroundProperty, Theme.Tokens.Fill ]
                                    Border.child (
                                        TextBlock.create [
                                            TextBlock.text count
                                            TextBlock.fontSize 12.0
                                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                        ]
                                    )
                                ]
                            ]
                        ]
                    )
                ]

            let onContinue _ =
                // Only what's shown: a repair ticked under "all" and then hidden again isn't run.
                let shown = if showAll.Current then fixChoices else needed
                let options = shown |> List.fold (fun all c -> if selected.Current.HasFlag c.Option then all ||| c.Option else all) PdfFixOptions.None
                if options = PdfFixOptions.None then
                    showMessage window "Select at least one fix to apply." "Fix PDF" |> ignore
                else
                    window.Close(Some options)

            Grid.create [
                Grid.margin 14.0
                Grid.rowDefinitions "Auto,Auto,*,Auto,Auto"
                Grid.children [
                    TextBlock.create [
                        TextBlock.text "Choose the repairs to apply to a copy of this PDF:"
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.margin (0.0, 0.0, 0.0, 8.0)
                    ]
                    ToggleSwitch.create [
                        Grid.row 1
                        ToggleSwitch.margin (0.0, 0.0, 0.0, 10.0)
                        ToggleSwitch.isChecked showAll.Current
                        ToggleSwitch.onContent $"Showing all {fixChoices.Length} repairs"
                        ToggleSwitch.offContent $"Showing the {needed.Length} this PDF needs"
                        ToggleSwitch.onIsCheckedChanged (fun e ->
                            match e.Source with
                            | :? ToggleSwitch as toggle -> showAll.Set(toggle.IsChecked.GetValueOrDefault())
                            | _ -> ())
                    ]
                    ScrollViewer.create [
                        Grid.row 2
                        ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Disabled
                        ScrollViewer.content (
                            StackPanel.create [
                                StackPanel.children [
                                    let shown = if showAll.Current then fixChoices else needed
                                    if shown.IsEmpty then
                                        yield
                                            TextBlock.create [
                                                TextBlock.textWrapping TextWrapping.Wrap
                                                Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                                TextBlock.text "Nothing in these results can be repaired automatically. Show all repairs to choose one anyway."
                                            ]
                                            :> IView
                                    for choice in shown do
                                        yield row choice :> IView
                                ]
                            ]
                        )
                    ]
                    TextBlock.create [
                        Grid.row 3
                        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.margin (0.0, 8.0, 0.0, 16.0)
                        TextBlock.text
                            "Counts are findings in the current results, dismissed warnings included. These repairs are best-effort: they don't add missing alt text or infer tables' rows and cells, and tags from the layout are a detector's best guess, to check."
                    ]
                    StackPanel.create [
                        Grid.row 4
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.horizontalAlignment HorizontalAlignment.Right
                        StackPanel.children [
                            Button.create [
                                Button.content "Cancel"
                                Button.minWidth 82.0
                                Button.margin (0.0, 0.0, 8.0, 0.0)
                                Button.isCancel true
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.onClick (fun _ -> window.Close(None: PdfFixOptions option))
                            ]
                            Button.create [
                                Button.content "Continue…"
                                Button.minWidth 96.0
                                Button.isDefault true
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.onClick onContinue
                            ]
                        ]
                    ]
                ]
            ])

    // Closing with the title-bar button returns null, which is None.
    window.ShowDialog<PdfFixOptions option> owner

type private StatusStyle =
    { Title: string
      Detail: string
      Glyph: string
      ColorToken: string }

let private statusStyle (outcome: Result<FixResult, string>) =
    match outcome with
    | Error message ->
        { Title = "Repair failed"; Detail = message; Glyph = "✕"; ColorToken = Theme.Tokens.Danger }
    | Ok fix ->
        match fix.Status with
        | RepairStatus.Success ->
            { Title = "Repaired"
              Detail = "The selected fixes changed the file, and a repaired copy was saved."
              Glyph = "✓"
              ColorToken = Theme.Tokens.Success }
        | RepairStatus.IdRemoved ->
            { Title = "Conformance claim removed"
              Detail =
                "The file claimed PDF/A or PDF/UA conformance it doesn't meet, so veraPDF removed that claim. "
                + "Without it, the check that requires a conformance claim now fails too, "
                + "so the repaired copy can show one more failure."
              Glyph = "!"
              ColorToken = Theme.Tokens.Warning }
        | RepairStatus.NoAction ->
            { Title = "Nothing to repair"
              Detail = "The selected fixes had nothing to change in this file, so no copy was saved."
              Glyph = "i"
              ColorToken = Theme.Tokens.Primary }
        | RepairStatus.WontFix ->
            { Title = "Couldn't repair"
              Detail = "veraPDF found no way to fix these problems with the selected repairs, so no copy was saved."
              Glyph = "!"
              ColorToken = Theme.Tokens.Warning }
        | _ ->
            { Title = "Repair failed"
              Detail = "veraPDF tried to repair the metadata but failed, so no copy was saved."
              Glyph = "✕"
              ColorToken = Theme.Tokens.Danger }

let private sectionHeading (text: string) : IView =
    TextBlock.create [
        TextBlock.text text
        TextBlock.fontWeight FontWeight.SemiBold
        TextBlock.margin (0.0, 16.0, 0.0, 6.0)
        Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
    ]

/// One before → after row. `better` says whether the change is an improvement
/// (Some true), a regression (Some false) or neither (None).
let private comparisonCells (row: int) (label: string) (before: string) (after: string) (better: bool option) : IView list =
    let cell column (text: string) (attrs: IAttr<TextBlock> list) : IView =
        TextBlock.create [
            Grid.row row
            Grid.column column
            TextBlock.text text
            TextBlock.margin (0.0, 2.0, 16.0, 2.0)
            yield! attrs
        ]

    [ cell 0 label []
      cell 1 before [ Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ] ]
      cell 2 "→" [ Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ] ]
      cell 3 after [
          TextBlock.fontWeight FontWeight.SemiBold
          match better with
          | Some true -> Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.Success ]
          | Some false -> Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.Danger ]
          | None -> ()
      ] ]

let private comparison (before: ValidationReport) (after: ValidationReport) : IView =
    let count label (b: int) (a: int) = label, string b, string a, (if a < b then Some true elif a > b then Some false else None)
    let yesNo value = if value then "Yes" else "No"
    let rows =
        [ count "Failed checks" before.FailedCount after.FailedCount
          count "Warnings" before.WarningCount after.WarningCount
          $"Compliant with {after.FlavourId.ToUpperInvariant()}",
          yesNo before.IsCompliant,
          yesNo after.IsCompliant,
          (if after.IsCompliant = before.IsCompliant then None else Some after.IsCompliant) ]

    Grid.create [
        Grid.columnDefinitions "Auto,Auto,Auto,Auto"
        Grid.rowDefinitions (String.replicate rows.Length "Auto," |> fun s -> s.TrimEnd ',')
        Grid.children [
            for index, (label, b, a, better) in List.indexed rows do
                yield! comparisonCells index label b a better
        ]
    ]

/// Shows the outcome of a Fix: a plain-language status, what changed, how the
/// repaired copy validates compared with the original, and where it was saved.
let showFixResult (owner: Window) (outcome: Result<FixResult, string>) (after: ValidationReport option) : Task =
    let window = dialogWindow "Fix PDF" 540.0
    let style = statusStyle outcome

    let savedPath =
        match outcome with
        | Ok fix when fix.WasFixed -> Option.ofObj fix.OutputPath
        | _ -> None

    let openFolder (folder: string) =
        window.Launcher.LaunchDirectoryInfoAsync(DirectoryInfo folder) |> ignore

    window.Content <-
        Component(fun _ ->
            StackPanel.create [
                StackPanel.margin 20.0
                StackPanel.children [
                    // Status headline
                    Grid.create [
                        Grid.columnDefinitions "Auto,*"
                        Grid.children [
                            Border.create [
                                Border.width 36.0
                                Border.height 36.0
                                Border.cornerRadius 18.0
                                Border.margin (0.0, 0.0, 14.0, 0.0)
                                Border.verticalAlignment VerticalAlignment.Top
                                Theme.resources [ Border.BackgroundProperty, style.ColorToken ]
                                Border.child (
                                    TextBlock.create [
                                        TextBlock.text style.Glyph
                                        TextBlock.fontSize 18.0
                                        TextBlock.fontWeight FontWeight.Bold
                                        TextBlock.foreground Brushes.White
                                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                                        TextBlock.verticalAlignment VerticalAlignment.Center
                                    ]
                                )
                            ]
                            StackPanel.create [
                                Grid.column 1
                                StackPanel.children [
                                    TextBlock.create [
                                        TextBlock.text style.Title
                                        TextBlock.fontSize 18.0
                                        TextBlock.fontWeight FontWeight.SemiBold
                                    ]
                                    SelectableTextBlock.create [
                                        SelectableTextBlock.text style.Detail
                                        SelectableTextBlock.textWrapping TextWrapping.Wrap
                                        SelectableTextBlock.margin (0.0, 4.0, 0.0, 0.0)
                                        Theme.resources [ SelectableTextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                    ]
                                ]
                            ]
                        ]
                    ]

                    // Before → after
                    match outcome, after with
                    | Ok fix, Some after ->
                        sectionHeading "Result (original → repaired copy)"
                        comparison fix.OriginalReport after
                    | _ -> ()

                    // Changes made
                    match outcome with
                    | Ok fix when fix.AppliedFixes.Count > 0 ->
                        sectionHeading "Changes made"
                        StackPanel.create [
                            StackPanel.spacing 4.0
                            StackPanel.children [
                                for change in fix.AppliedFixes ->
                                    Grid.create [
                                        Grid.columnDefinitions "Auto,*"
                                        Grid.children [
                                            TextBlock.create [
                                                TextBlock.text "✓"
                                                TextBlock.margin (0.0, 0.0, 8.0, 0.0)
                                                Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.Success ]
                                            ]
                                            TextBlock.create [
                                                Grid.column 1
                                                TextBlock.text change
                                                TextBlock.textWrapping TextWrapping.Wrap
                                            ]
                                        ]
                                    ]
                            ]
                        ]
                    | _ -> ()

                    // Attempted but not kept
                    match outcome with
                    | Ok fix when fix.NotApplied.Count > 0 ->
                        sectionHeading "Not repaired"
                        StackPanel.create [
                            StackPanel.spacing 4.0
                            StackPanel.children [
                                for reason in fix.NotApplied ->
                                    Grid.create [
                                        Grid.columnDefinitions "Auto,*"
                                        Grid.children [
                                            TextBlock.create [
                                                TextBlock.text "!"
                                                TextBlock.margin (0.0, 0.0, 8.0, 0.0)
                                                Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.Warning ]
                                            ]
                                            TextBlock.create [
                                                Grid.column 1
                                                TextBlock.text reason
                                                TextBlock.textWrapping TextWrapping.Wrap
                                            ]
                                        ]
                                    ]
                            ]
                        ]
                    | _ -> ()

                    // Saved as
                    match savedPath with
                    | Some path ->
                        let folder = Path.GetDirectoryName path |> Option.ofObj |> Option.defaultValue ""
                        sectionHeading "Saved as"
                        Grid.create [
                            Grid.columnDefinitions "*,Auto"
                            Grid.children [
                                StackPanel.create [
                                    StackPanel.children [
                                        SelectableTextBlock.create [
                                            SelectableTextBlock.text (match Path.GetFileName path with NonNull name -> name | Null -> path)
                                            SelectableTextBlock.fontWeight FontWeight.SemiBold
                                        ]
                                        SelectableTextBlock.create [
                                            SelectableTextBlock.text folder
                                            SelectableTextBlock.textWrapping TextWrapping.Wrap
                                            Theme.resources [ SelectableTextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                        ]
                                        TextBlock.create [
                                            TextBlock.text "Legibyl is now showing this copy."
                                            TextBlock.margin (0.0, 4.0, 0.0, 0.0)
                                            Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                        ]
                                    ]
                                ]
                                Button.create [
                                    Grid.column 1
                                    Button.content "Open folder"
                                    Button.margin (12.0, 0.0, 0.0, 0.0)
                                    Button.verticalAlignment VerticalAlignment.Top
                                    Button.isEnabled (folder <> "")
                                    Button.onClick (fun _ -> openFolder folder)
                                ]
                            ]
                        ]
                    | None -> ()

                    // Footnote and OK
                    Grid.create [
                        Grid.margin (0.0, 20.0, 0.0, 0.0)
                        Grid.columnDefinitions "*,Auto"
                        Grid.children [
                            match outcome with
                            | Ok _ ->
                                TextBlock.create [
                                    TextBlock.text
                                        "Fixes are best-effort and limited to the repairs you selected. They don't add missing alt text or infer table headers, and tags from the layout are a detector's best guess, to check."
                                    TextBlock.fontSize 12.0
                                    TextBlock.textWrapping TextWrapping.Wrap
                                    TextBlock.margin (0.0, 0.0, 16.0, 0.0)
                                    TextBlock.verticalAlignment VerticalAlignment.Center
                                    Theme.resources [ TextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
                                ]
                            | Error _ -> ()
                            Button.create [
                                Grid.column 1
                                Button.content "OK"
                                Button.minWidth 82.0
                                Button.isDefault true
                                Button.isCancel true
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.verticalAlignment VerticalAlignment.Bottom
                                Button.onClick (fun _ -> window.Close())
                            ]
                        ]
                    ]
                ]
            ])

    window.ShowDialog owner
