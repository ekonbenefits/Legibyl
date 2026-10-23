/// The About window: which Legibyl this is, what it's built on, its licenses, where to find the
/// project, and where its own files are; Copy gives a bug report the same.
module Legibyl.Gui.About

open System
open System.IO
open System.Reflection
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Input.Platform
open Avalonia.Media.Imaging
open Avalonia.Platform.Storage
open Legibyl.Layout
open Legibyl.Validation

type private Marker = class end

let private assembly = typeof<Marker>.Assembly

/// A build property stamped into this assembly (see Legibyl.Gui.fsproj).
let private stamped (key: string) =
    assembly.GetCustomAttributes<AssemblyMetadataAttribute>()
    |> Seq.tryFind (fun a -> a.Key = key)
    |> Option.bind (fun a -> Option.ofObj a.Value)
    |> Option.defaultValue "unknown"

/// The project's page, and where to report a problem.
let projectPage = "https://github.com/ekonbenefits/Legibyl"
let issuesPage = projectPage + "/issues"

/// Where Legibyl keeps its own files (the recent PDFs).
let dataFolder = Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "Legibyl")

/// What it's built on: each with its version and what it does here.
let components () =
    BuildInfo.validationComponents ()
    @ [ "PDFium", stamped "PdfiumVersion", "drawing pages"
        "PaddleOCR (PP-DocLayoutV3)",
        BuildInfo.libraryVersion typeof<PaddleOcrSharp.Download.ModelDownloader>.Assembly,
        (if LayoutModel.layoutDownloaded () then $"suggesting structure; its model is in {LayoutModel.directory ()}"
         else "suggesting structure; its model isn't downloaded yet")
        "Table Transformer", "1.0 (8-bit)",
        (if TableModel.isDownloaded () then $"finding tables' rows and cells; its model is in {IO.Path.GetDirectoryName(TableModel.path ())}"
         else "finding tables' rows and cells; its model isn't downloaded yet")
        "ONNX Runtime", BuildInfo.libraryVersion typeof<Microsoft.ML.OnnxRuntime.InferenceSession>.Assembly, "running the table model"
        "Tesseract tessdata", stamped "TessdataVersion", "reading text drawn as outlines or pixels (English and Spanish)"
        "Avalonia", BuildInfo.libraryVersion typeof<Application>.Assembly, "the windows" ]

/// What Copy puts on the clipboard: the version line `legibyl --version` prints, then what it's
/// built on.
let copyText () =
    String.Join(
        Environment.NewLine,
        BuildInfo.versionLine :: [ for name, version, what in components () -> $"  {name} {version}: {what}" ]
    )

/// The license texts it carries (licenses/ in the repository): NOTICE.md first.
let licenses () : (string * string) list =
    [ for name in assembly.GetManifestResourceNames() do
          if name.StartsWith("licenses/", StringComparison.Ordinal) then
              use stream = assembly.GetManifestResourceStream name |> nonNull
              use reader = new StreamReader(stream)
              yield name.Substring "licenses/".Length, reader.ReadToEnd() ]
    |> List.sortBy (fun (name, _) -> (if name = "NOTICE.md" then 0 else 1), name)

let private window (title: string) (width: float) =
    HostWindow(
        Title = title,
        Width = width,
        SizeToContent = SizeToContent.Height,
        CanResize = false,
        ShowInTaskbar = false,
        WindowStartupLocation = WindowStartupLocation.CenterOwner
    )

/// The license texts, one at a time, scrolling: the notice drawn from its Markdown.
let showLicenses (owner: Window) : Task =
    let texts = licenses ()
    let shown = HostWindow(Title = "Licenses", Width = 720.0, Height = 560.0, ShowInTaskbar = false, WindowStartupLocation = WindowStartupLocation.CenterOwner)
    shown.Content <-
        Component(fun ctx ->
            let chosen = ctx.useState (texts |> List.tryHead |> Option.map fst |> Option.defaultValue "")
            DockPanel.create [
                DockPanel.margin 16.0
                DockPanel.children [
                    ComboBox.create [
                        DockPanel.dock Dock.Top
                        ComboBox.margin (0.0, 0.0, 0.0, 8.0)
                        ComboBox.dataItems (texts |> List.map fst)
                        AttrBuilder<ComboBox>.CreateProperty<string | null>(Automation.AutomationProperties.NameProperty, "License", ValueNone)
                        ComboBox.selectedItem chosen.Current
                        ComboBox.onSelectedItemChanged (fun item -> match item with :? string as name -> chosen.Set name | _ -> ())
                    ]
                    Button.create [
                        DockPanel.dock Dock.Bottom
                        Button.content "Close"
                        Button.isCancel true
                        Button.isDefault true
                        Button.margin (0.0, 8.0, 0.0, 0.0)
                        Button.horizontalAlignment HorizontalAlignment.Right
                        Button.onClick (fun _ -> shown.Close())
                    ]
                    let text = texts |> List.tryFind (fst >> (=) chosen.Current) |> Option.map snd |> Option.defaultValue ""
                    ScrollViewer.create [
                        // The notice is Markdown, drawn; the licenses are plain text, as written.
                        if chosen.Current.EndsWith(".md", StringComparison.OrdinalIgnoreCase) then
                            ScrollViewer.content (Markdown.view text)
                        else
                            ScrollViewer.content (
                                SelectableTextBlock.create [
                                    SelectableTextBlock.text text
                                    SelectableTextBlock.textWrapping TextWrapping.Wrap
                                    SelectableTextBlock.fontFamily Fonts.Monospace
                                    SelectableTextBlock.fontSize 12.0
                                ]
                            )
                    ]
                ]
            ])
    shown.ShowDialog owner

/// The app's icon (assets/Legibyl.png, made from icon_vector.svg by assets/make-icons.fsx).
let private icon =
    lazy
        (match assembly.GetManifestResourceStream "logo.png" with
         | null -> None
         | stream ->
             use stream = stream
             Some(Bitmap.DecodeToWidth(stream, 128)))

/// The About window, modal to owner; Esc or Close closes it.
let show (owner: Window) : Task =
    let about = window "About Legibyl" 560.0
    let link (label: string) (address: string) =
        Button.create [
            Button.content label
            Button.padding (0.0, 2.0)
            Button.background Brushes.Transparent
            Button.borderThickness 0.0
            Button.cursor (new Avalonia.Input.Cursor(Avalonia.Input.StandardCursorType.Hand))
            Theme.resources [ Button.ForegroundProperty, Theme.Tokens.Primary ]
            ToolTip.tip address
            Button.onClick (fun _ -> about.Launcher.LaunchUriAsync(Uri address) |> ignore)
        ]
    let line (text: string) (secondary: bool) =
        SelectableTextBlock.create [
            SelectableTextBlock.text text
            SelectableTextBlock.textWrapping TextWrapping.Wrap
            if secondary then Theme.resources [ SelectableTextBlock.ForegroundProperty, Theme.Tokens.SecondaryText ]
        ]
    let heading (text: string) =
        TextBlock.create [ TextBlock.text text; TextBlock.fontWeight FontWeight.SemiBold; TextBlock.margin (0.0, 8.0, 0.0, 0.0) ]
    about.Content <-
        Component(fun _ ->
            StackPanel.create [
                StackPanel.margin 16.0
                StackPanel.spacing 4.0
                StackPanel.children [
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.spacing 12.0
                        StackPanel.children [
                            match icon.Value with
                            | Some image -> Image.create [ Image.source image; Image.width 56.0; Image.height 56.0 ]
                            | None -> ()
                            StackPanel.create [
                                StackPanel.verticalAlignment VerticalAlignment.Center
                                StackPanel.children [
                                    TextBlock.create [ TextBlock.text "Legibyl"; TextBlock.fontSize 22.0; TextBlock.fontWeight FontWeight.SemiBold ]
                                    line $"Version {BuildInfo.version} (vendored veraPDF engine {VeraPdfValidator.EngineVersion})" false
                                    line $"{BuildInfo.copyright} <{BuildInfo.contact}>" true
                                ]
                            ]
                        ]
                    ]
                    heading "Built on"
                    StackPanel.create [
                        StackPanel.spacing 2.0
                        StackPanel.children [ for name, version, what in components () -> line $"{name} {version}: {what}" true ]
                    ]
                    heading "License"
                    line
                        "Legibyl is free software under the GNU Affero General Public License v3.0 or later. It bundles veraPDF (GPLv3+ / MPLv2+), IKVM.NET (zlib, its Java library GPLv2 with the Classpath exception) and others."
                        false
                    heading "Links"
                    link "Project page" projectPage
                    link "Report a problem" issuesPage
                    // A released build's: the source of everything it's built with but Legibyl itself.
                    match BuildInfo.dependencySource, BuildInfo.dependencySourceAddress with
                    | Some release, Some address -> link $"Its dependencies' source ({release})" address
                    | _ -> ()
                    heading "Its files"
                    line $"Recent PDFs, in {dataFolder}" true
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.horizontalAlignment HorizontalAlignment.Right
                        StackPanel.spacing 8.0
                        StackPanel.margin (0.0, 12.0, 0.0, 0.0)
                        StackPanel.children [
                            Button.create [
                                Button.content "Licenses…"
                                Button.onClick (fun _ -> showLicenses about |> ignore)
                            ]
                            Button.create [
                                Button.content "Open Its Folder"
                                ToolTip.tip dataFolder
                                Button.onClick (fun _ ->
                                    Directory.CreateDirectory dataFolder |> ignore
                                    about.Launcher.LaunchDirectoryInfoAsync(DirectoryInfo dataFolder) |> ignore)
                            ]
                            Button.create [
                                Button.content "Copy"
                                ToolTip.tip "Copy the version and what it's built on, for a bug report."
                                Button.onClick (fun _ ->
                                    match about.Clipboard with
                                    | NonNull clipboard -> clipboard.SetTextAsync(copyText ()) |> ignore
                                    | Null -> ())
                            ]
                            Button.create [
                                Button.content "Close"
                                Button.isCancel true
                                Button.isDefault true
                                Button.onClick (fun _ -> about.Close())
                            ]
                        ]
                    ]
                ]
            ])
    about.ShowDialog owner
