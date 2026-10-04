module Legibyl.Gui.Dialogs

open System.IO
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
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

/// The most a dialog can be tall on owner's screen, so its buttons stay on it (its content
/// scrolls in whatever room is left).
let private screenLimit (owner: Window) =
    match owner.Screens.ScreenFromWindow owner with
    | NonNull screen -> float screen.WorkingArea.Height / screen.Scaling * 0.9
    | Null -> 700.0

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

/// Asks whether to keep a review's changes before they'd be lost (closing, or opening another
/// PDF): Some true to save them, Some false not to, None to cancel.
let askToSave (owner: Window) (fileName: string) : Task<bool option> =
    let window = dialogWindow "Save Your Review?" 460.0
    let mutable chosen = None
    let choice (label: string) (result: bool option) (attrs: IAttr<Button> list) =
        Button.create [
            Button.content label
            Button.minWidth 82.0
            Button.horizontalContentAlignment HorizontalAlignment.Center
            Button.onClick (fun _ ->
                chosen <- result
                window.Close())
            yield! attrs
        ]
    window.Content <-
        Component(fun _ ->
            StackPanel.create [
                StackPanel.margin 16.0
                StackPanel.spacing 16.0
                StackPanel.children [
                    TextBlock.create [
                        TextBlock.text $"Save your review of {fileName}? Your edits, dismissed warnings and chosen auto fixes are lost if you don't. The PDF isn't changed either way."
                        TextBlock.textWrapping TextWrapping.Wrap
                    ]
                    DockPanel.create [
                        DockPanel.children [
                            StackPanel.create [
                                DockPanel.dock Dock.Right
                                StackPanel.orientation Orientation.Horizontal
                                StackPanel.spacing 8.0
                                StackPanel.children [
                                    choice "Cancel" None [ Button.isCancel true ]
                                    choice "Save" (Some true) [ Button.isDefault true ]
                                ]
                            ]
                            StackPanel.create [
                                StackPanel.children [ choice "Don't Save" (Some false) [] ]
                            ]
                        ]
                    ]
                ]
            ])
    task {
        do! window.ShowDialog owner
        return chosen
    }

/// Asks for a piece of text (alt text, say): prompt above a box starting with initial. Some text
/// on OK, None on Cancel.
let promptText (owner: Window) (title: string) (prompt: string) (initial: string) (placeholder: string) : Task<string option> =
    let window = dialogWindow title 520.0
    let mutable chosen = None
    window.Content <-
        Component(fun ctx ->
            let text = ctx.useState initial
            StackPanel.create [
                StackPanel.margin 16.0
                StackPanel.spacing 12.0
                StackPanel.children [
                    TextBlock.create [ TextBlock.text prompt; TextBlock.textWrapping TextWrapping.Wrap ]
                    TextBox.create [
                        AttrBuilder<TextBox>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, prompt, ValueNone)
                        TextBox.text text.Current
                        TextBox.placeHolderText placeholder
                        TextBox.acceptsReturn true
                        TextBox.textWrapping TextWrapping.Wrap
                        TextBox.minHeight 60.0
                        TextBox.onTextChanged text.Set
                    ]
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.horizontalAlignment HorizontalAlignment.Right
                        StackPanel.spacing 8.0
                        StackPanel.children [
                            Button.create [
                                Button.content "OK"
                                Button.minWidth 82.0
                                Button.isDefault true
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.onClick (fun _ ->
                                    chosen <- Some text.Current
                                    window.Close())
                            ]
                            Button.create [
                                Button.content "Cancel"
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

/// A fix the Auto Fixes dialog offers.
type private FixChoice =
    { Option: PdfFixOptions
      Title: string
      Detail: string }

    /// Stays unticked even when found: it isn't one of the safe fixes (FixNames.safe, as the
    /// CLI's fix applies by default), since it changes what the document says it is, or guesses.
    member choice.OptIn = not (FixNames.safe.HasFlag choice.Option)

/// The fixes, in the order they run: tagging first, then StructureFixes.orderedFixes, then
/// veraPDF's metadata fix.
let private fixChoices =
    // Tagging an untagged PDF isn't among them: it's tagged from its suggested structure.
    [ { Option = PdfFixOptions.QrLinks
        Title = "Link QR codes"
        Detail = "With tagging: a borderless link over each QR code that encodes a web address." }
      { Option = PdfFixOptions.ParentTree
        Title = "Rebuild the parent tree"
        Detail = "The tags are there, but the PDF doesn't say which page content each covers, so they read as untagged. Rebuilt from the tags; nothing on the page changes." }
      { Option = PdfFixOptions.RoleMap
        Title = "Map the PDF's own tag names"
        Detail = "A PDF that uses tag names of its own (Heading 1, Body Text, Image) has to say which standard tag each means, or a screen reader can't tell a heading from a paragraph. Each name that says what it is is mapped to that tag; a broken map (one going round in a circle, or redefining a standard tag) is mended. Names that don't say are left, and reported. Check the headings and lists read right." }
      { Option = PdfFixOptions.Language
        Title = "Declare the language"
        Detail = "Say what language the document is in, detected from its text when there's enough to be sure, so a screen reader reads it with the right voice (/Lang)." }
      { Option = PdfFixOptions.HeadingsFromToc
        Title = "Headings from the table of contents"
        Detail = "Tag paragraphs a contents entry names as headings (H1–H6, from its indent), and point its links at them." }
      { Option = PdfFixOptions.EmptyParagraphs
        Title = "Remove empty paragraphs"
        Detail = "Paragraph tags with nothing in them, which a screen reader stops on." }
      { Option = PdfFixOptions.EmptyTocEntries
        Title = "Remove empty contents entries"
        Detail = "Entries in the table of contents' tags with nothing in them, which a screen reader stops on." }
      { Option = PdfFixOptions.EmptyFigures
        Title = "Remove empty figures"
        Detail = "Figure tags around nothing on the page, which fail for having no alt text when there's nothing to describe." }
      { Option = PdfFixOptions.JoinSplitParagraphs
        Title = "Join split paragraphs"
        Detail =
          "Paragraphs the split-paragraph warning flags become one where a layout detector confirms they're one block of text; the page looks the same. "
          + "Several seconds a page"
          + (match Legibyl.Layout.LayoutModel.downloadMegabytes () with
             | 0 -> "."
             | size -> $"; downloads the layout models (about {size} MB) the first time.") }
      { Option = PdfFixOptions.Lists
        Title = "Tag lists"
        Detail = "Runs of paragraphs that start with list markers (•, (1), a.) become a list, so a screen reader announces it and its number of items; the page looks the same." }
      { Option = PdfFixOptions.TableHeaderScope
        Title = "Table header scope"
        Detail = "A table's header cells (TH) need to say which cells they head, or a screen reader can't announce the right header with each cell. This gives each one that doesn't a Scope: Column in a header row, Row for one heading its row. Check the tables read right." }
      { Option = PdfFixOptions.Bookmarks
        Title = "Add bookmarks"
        Detail = "Bookmarks for the H1 and H2 headings, when the document has none." }
      { Option = PdfFixOptions.LinkTargets
        Title = "Point links at their tags"
        Detail = "PDF 2.0 only: links, bookmarks and where the document opens name the tag they go to as well as the page (a structure destination), as PDF/UA-2 asks. They go to the same place." }
      { Option = PdfFixOptions.Tabs
        Title = "Tab order for annotations"
        Detail = "On pages with links or other annotations, the Tab key moves through them in reading order (/Tabs /S)." }
      { Option = PdfFixOptions.EmbedFonts
        Title = "Embed fonts"
        Detail = "Fonts the PDF uses without embedding them may look different, or not read the same, elsewhere. Helvetica, Times, Courier, Symbol and ZapfDingbats, and Arial, Times New Roman and Courier New, are embedded from open fonts made to the same measure (URW's and Liberation), only where one has every letter the pages use, each as wide: the text stays where it is, and the letters look much the same. Another font is embedded from this computer's own copy, if it has it and its licence lets it be embedded, with the same check. Others are left, and reported." }
      { Option = PdfFixOptions.FontGlyphs
        Title = "Font glyph mapping"
        Detail = "Some embedded fonts leave out how their character codes map to their glyphs; readers assume the usual mapping, and this writes it down (/CIDToGIDMap). Nothing changes on the page." }
      { Option = PdfFixOptions.FontDictionaries
        Title = "Font dictionaries"
        Detail = "Some embedded fonts' dictionaries don't agree with the font itself: a subset font's incomplete list of its glyphs (CharSet, CIDSet) is removed, and a composite font's character collection (CIDSystemInfo) and writing direction (/WMode) are set to its CMap's. Nothing changes on the page." }
      { Option = PdfFixOptions.FontUnicode
        Title = "Readable font text"
        Detail = "Some fonts don't say which characters they draw, so a screen reader reads their text as nothing, or the wrong letters. This writes it down (a ToUnicode map) from what each font says itself: its glyph names, or its own Unicode table. Characters a font says nothing about are left out, and reported. Nothing changes on the page; check the text reads right." }
      { Option = PdfFixOptions.LinkContents
        Title = "Describe contents links"
        Detail = "Give table-of-contents links without a description the entry's own text, which a screen reader announces for them." }
      { Option = PdfFixOptions.LinkTitlesFromWeb
        Title = "Describe web links from their pages"
        Detail =
          "Visits each web address a link without a description (or, with tagging, a QR code) goes to, and uses the page's title, "
          + "with the link's own site: \"Apply for benefits (example.com)\". Only public web pages are visited, and only their titles read; "
          + "the sites see the visit. Check the descriptions." }
      { Option = PdfFixOptions.Title
        Title = "Document title"
        Detail = "Use the title the document carries, and have readers show it in place of the file name." }
      { Option = PdfFixOptions.TitleFromFirstPage
        Title = "Title from the first page"
        Detail = "If the document has no title anywhere, use the text of a short first page (a title page)." }
      { Option = PdfFixOptions.Metadata
        Title = "Fix XMP metadata"
        Detail = "Write the metadata PDF/UA asks for, and the identifier saying the file follows PDF/UA (veraPDF's fix)." } ]

/// Lets the user pick which fixes to run, showing how many of the report's findings each
/// one addresses. Lists only the fixes this PDF needs, unless asked for all of them.
/// Returns None if the user cancelled.
/// tagNote, if any, says how an untagged PDF is tagged (from its suggested structure), shown
/// above the fixes. titleGiven says a title's been typed in Structure…, which Export writes first.
/// chosen is what was chosen before, if anything; the first time, the safe fixes this PDF
/// needs start ticked. Export writes the chosen fixes (and the user's edits) to a copy.
let chooseFixOptions
    (owner: Window)
    (report: ValidationReport)
    (tagNote: string option)
    (titleGiven: bool)
    (chosen: PdfFixOptions option)
    : Task<PdfFixOptions option> =
    let window = dialogWindow "Auto Fixes" 560.0
    // Never taller than the screen: the list scrolls in whatever room is left. (All the fixes
    // at once are taller than a laptop screen.)
    window.MaxHeight <- screenLimit owner
    // Its size is set once it opens, room enough for most of the fixes: showing all of them
    // then scrolls the list rather than resizing the window, which grew down from where it
    // opened, past the bottom of the screen.
    window.Opened.Add(fun _ ->
        // Once it's laid out at the height its content wants.
        Avalonia.Threading.Dispatcher.UIThread.Post(
            (fun () ->
                let height = min window.MaxHeight (max window.Bounds.Height 620.0)
                window.SizeToContent <- SizeToContent.Manual
                window.Height <- height
                // Centred on the window it's for again, at its new height.
                let top = owner.Position.Y + int ((owner.Bounds.Height - height) * owner.RenderScaling / 2.0)
                window.Position <- PixelPoint(window.Position.X, max owner.Position.Y top)),
            Avalonia.Threading.DispatcherPriority.Background
        ))
    let found (option: PdfFixOptions) = report.FixableCount option
    /// Whether this PDF has something for the fix to do. Linking QR codes comes with tagging,
    /// and so can describing them from the pages they go to.
    let withTagging (choice: FixChoice) =
        (choice.Option = PdfFixOptions.QrLinks || choice.Option = PdfFixOptions.LinkTitlesFromWeb)
        && found PdfFixOptions.TagFromLayout > 0
    let applies (choice: FixChoice) = found choice.Option > 0 || withTagging choice
    let failures = report.Findings |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure) |> List.ofSeq
    // veraPDF only fixes metadata (adding the PDF/UA identifier) when the document would then
    // conform, so it's only worth ticking when the safe fixes, or the title typed in Structure…
    // (which Export… writes first), fix every failure.
    let everyFailureFixable = FixNames.metadataReady titleGiven failures
    // Only the document's title stands in its way.
    let onlyTitleMissing = not everyFailureFixable && FixNames.metadataReady true failures
    let tickedAtFirst (choice: FixChoice) =
        applies choice && not choice.OptIn && (choice.Option <> PdfFixOptions.Metadata || everyFailureFixable)
    window.Content <-
        Component(fun ctx ->
            let selected =
                ctx.useState (
                    match chosen with
                    | Some chosen -> chosen
                    | None -> fixChoices |> List.filter tickedAtFirst |> List.fold (fun all c -> all ||| c.Option) PdfFixOptions.None
                )
            let showAll = ctx.useState false
            let needed = fixChoices |> List.filter applies

            /// The fix's title and detail, with how many findings it addresses on the right.
            let row (choice: FixChoice) =
                let count =
                    match found choice.Option with
                    | 0 when withTagging choice -> "with tagging"
                    | 0 -> "none found"
                    | n -> $"{n} found"
                let detail =
                    if choice.Option = PdfFixOptions.Metadata && applies choice && onlyTitleMissing then
                        $"{choice.Detail} Unticked: the document has no title for the metadata. Type one in Structure…'s Document title field: Export… writes it first, then this fix, in the same pass."
                    elif choice.Option = PdfFixOptions.Metadata && applies choice && not everyFailureFixable then
                        $"{choice.Detail} Unticked: veraPDF only writes it once every other failure is fixed, and some here need more than the fixes ticked at first (a fix that guesses, tagging, or fixing by hand)."
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
                                                        TextBlock.text "off at first"
                                                        ToolTip.tip "It guesses, or changes what the document says it is, so it's only used when you tick it."
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

            let onDone _ =
                // Only what's shown: a fix ticked under "all" and then hidden again isn't run.
                let shown = if showAll.Current then fixChoices else needed
                window.Close(Some(shown |> List.fold (fun all c -> if selected.Current.HasFlag c.Option then all ||| c.Option else all) PdfFixOptions.None))

            Grid.create [
                Grid.margin 14.0
                Grid.rowDefinitions "Auto,Auto,*,Auto,Auto"
                Grid.children [
                    TextBlock.create [
                        TextBlock.text (
                            match tagNote with
                            | Some note -> $"{note}\n\nChoose the fixes Export applies to the copy:"
                            | None -> "Choose the fixes Export applies to a copy of this PDF:"
                        )
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.margin (0.0, 0.0, 0.0, 8.0)
                    ]
                    ToggleSwitch.create [
                        Grid.row 1
                        ToggleSwitch.margin (0.0, 0.0, 0.0, 10.0)
                        ToggleSwitch.isChecked showAll.Current
                        AttrBuilder<ToggleSwitch>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, "Show every fix, not only those this PDF needs", ValueNone)
                        ToggleSwitch.onContent $"Showing all {fixChoices.Length} fixes"
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
                                                TextBlock.text "Nothing in these results has an auto fix. Show all fixes to choose one anyway."
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
                            "Counts are findings in the current results, dismissed warnings included. These fixes are best-effort: they don't write alt text themselves (Export writes the alt text you wrote, with your other edits), and tags from the layout, tables' rows, cells and header rows among them, are a detector's best guess, to check."
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
                                Button.content "Done"
                                Button.minWidth 96.0
                                Button.isDefault true
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                                Button.onClick onDone
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
        { Title = "Fix failed"; Detail = message; Glyph = "✕"; ColorToken = Theme.Tokens.Danger }
    | Ok fix ->
        match fix.Status with
        | RepairStatus.Success ->
            { Title = "Exported"
              Detail =
                if fix.RequestedOptions.HasFlag PdfFixOptions.TagFromLayout then
                    "A copy was tagged from the suggested structure, with your edits and the chosen fixes. Check its headings, figures and reading order in Structure… in the copy's window. The original is unchanged."
                else
                    "A copy was written with your edits and the chosen fixes. The original is unchanged."
              Glyph = "✓"
              ColorToken = Theme.Tokens.Success }
        | RepairStatus.IdRemoved ->
            { Title = "Conformance claim removed"
              Detail =
                "The file claimed PDF/UA conformance it doesn't meet, so veraPDF removed that claim. "
                + "Without it, the check that requires a conformance claim now fails too, "
                + "so the fixed copy can show one more failure."
              Glyph = "!"
              ColorToken = Theme.Tokens.Warning }
        | RepairStatus.NoAction ->
            { Title = "Nothing to change"
              Detail = "Your edits and the chosen fixes had nothing to change in this file, so no copy was written."
              Glyph = "i"
              ColorToken = Theme.Tokens.Primary }
        | RepairStatus.WontFix ->
            { Title = "Couldn't fix"
              Detail = "veraPDF found no way to fix these problems with the selected fixes, so no copy was written."
              Glyph = "!"
              ColorToken = Theme.Tokens.Warning }
        | _ ->
            { Title = "Fix failed"
              Detail = "veraPDF tried to fix the metadata but failed, so no copy was written."
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
          $"Compliant with {PdfUaTarget.displayNameOfFlavour after.FlavourId}",
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
/// fixed copy validates compared with the original, and where it was written.
let showFixResult (owner: Window) (outcome: Result<FixResult, string>) (after: ValidationReport option) : Task =
    let window = dialogWindow "Export" 540.0
    let style = statusStyle outcome

    let savedPath =
        match outcome with
        | Ok fix when fix.WasFixed -> Option.ofObj fix.OutputPath
        | _ -> None

    let openFolder (folder: string) =
        window.Launcher.LaunchDirectoryInfoAsync(DirectoryInfo folder) |> ignore

    // Never taller than the screen: the results scroll, OK stays below them.
    window.MaxHeight <- screenLimit owner

    window.Content <-
        Component(fun _ ->
            Grid.create [
                Grid.margin 20.0
                Grid.rowDefinitions "*,Auto"
                Grid.children [
                    ScrollViewer.create [
                        ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Disabled
                        ScrollViewer.content (
                            StackPanel.create [
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
                                                    // Black: at least 3:1 (large text) on each status colour, light and dark.
                                                    TextBlock.foreground Brushes.Black
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
                                    sectionHeading "Result (original → fixed copy)"
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
                                    sectionHeading "Not fixed"
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

                                // Written as
                                match savedPath with
                                | Some path ->
                                    let folder = Path.GetDirectoryName path |> Option.ofObj |> Option.defaultValue ""
                                    sectionHeading "Exported as"
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
                                ]
                            ]
                        )
                    ]

                    // Footnote and OK
                    Grid.create [
                        Grid.row 1
                        Grid.margin (0.0, 20.0, 0.0, 0.0)
                        Grid.columnDefinitions "*,Auto"
                        Grid.children [
                            match outcome with
                            | Ok _ ->
                                TextBlock.create [
                                    TextBlock.text
                                        "Fixes are best-effort and limited to the ones you selected. They don't write alt text themselves (only the alt text you wrote), and tags from the layout, tables' rows, cells and header rows among them, are a detector's best guess, to check."
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
