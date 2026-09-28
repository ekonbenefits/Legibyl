/// Markdown, drawn (the Licenses window's NOTICE.md): parsed with Markdig, each block a control
/// and its text selectable, with links as link buttons among the words.
module Legibyl.Gui.Markdown

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Documents
open Avalonia.Layout
open Avalonia.Media
open Markdig
open Markdig.Syntax
open Markdig.Syntax.Inlines

/// CommonMark, and bare addresses (https://…, www.…) taken as links.
let private pipeline = MarkdownPipelineBuilder().UseAutoLinks().Build()

/// How a run of text is drawn: bold, italic.
type private Style = { Bold: bool; Italic: bool }

let private run (style: Style) (text: string) =
    Run(
        text,
        FontWeight = (if style.Bold then FontWeight.SemiBold else FontWeight.Normal),
        FontStyle = (if style.Italic then FontStyle.Italic else FontStyle.Normal)
    )

/// A link among the words, weighted as they are: a link button, which opens address and says it
/// as its tip. (Its words are a text block's, not the button's own, which would take an _ for
/// an access key.)
let private link (style: Style) (text: string) (address: string) : Documents.Inline =
    let words = TextBlock(Text = text, FontWeight = (if style.Bold then FontWeight.SemiBold else FontWeight.Normal))
    let button = HyperlinkButton(Content = words, Padding = Thickness 0.0, VerticalAlignment = VerticalAlignment.Bottom)
    Automation.AutomationProperties.SetName(button, text)
    match Uri.TryCreate(address, UriKind.Absolute) with
    | true, uri ->
        button.NavigateUri <- uri
        ToolTip.SetTip(button, address)
    | _ -> ()
    InlineUIContainer(button, BaselineAlignment = BaselineAlignment.TextBottom)

/// The plain text of inlines (a link's words).
let rec private plainText (inlines: ContainerInline) =
    String.concat "" [
        for i in inlines do
            match i with
            | :? LiteralInline as literal -> literal.Content.ToString()
            | :? CodeInline as code -> code.Content
            | :? ContainerInline as inner -> plainText inner
            | :? LineBreakInline -> " "
            | _ -> ""
    ]

let rec private inlines (style: Style) (container: ContainerInline) : Documents.Inline list =
    [ for i in container do
          match i with
          | :? LiteralInline as literal -> run style (literal.Content.ToString())
          | :? LineBreakInline as lineBreak -> if lineBreak.IsHard then LineBreak() else run style " "
          | :? CodeInline as code ->
              let r = run style code.Content
              r.FontFamily <- Fonts.Monospace
              r
          | :? AutolinkInline as auto -> link style auto.Url (if auto.IsEmail then "mailto:" + auto.Url else auto.Url)
          | :? LinkInline as l when not l.IsImage ->
              let words = plainText l
              link style (if words = "" then string l.Url else words) (string l.Url)
          | :? EmphasisInline as emphasis ->
              let style = if emphasis.DelimiterCount >= 2 then { style with Bold = true } else { style with Italic = true }
              yield! inlines style emphasis
          | :? ContainerInline as inner -> yield! inlines style inner
          | :? HtmlInline as html -> run style html.Tag
          | _ -> () ]

let private textBlock (container: ContainerInline | null) =
    let block = SelectableTextBlock(TextWrapping = TextWrapping.Wrap, Inlines = InlineCollection())
    match container with
    | NonNull container ->
        for i in inlines { Bold = false; Italic = false } container do
            (nonNull block.Inlines).Add i
    | Null -> ()
    block

let rec private blocks (container: ContainerBlock) : Control list =
    [ for block in container do
          match block with
          | :? HeadingBlock as heading ->
              let text = textBlock heading.Inline
              text.FontSize <- (match heading.Level with 1 -> 20.0 | 2 -> 16.0 | _ -> 14.0)
              text.FontWeight <- FontWeight.SemiBold
              text.Margin <- Thickness(0.0, (if heading.Level = 1 then 0.0 else 8.0), 0.0, 0.0)
              Automation.AutomationProperties.SetHeadingLevel(text, heading.Level)
              text
          | :? ParagraphBlock as paragraph -> textBlock paragraph.Inline
          | :? CodeBlock as code ->
              let text = SelectableTextBlock(Text = code.Lines.ToString(), FontFamily = Fonts.Monospace, FontSize = 12.0)
              let box = Border(Child = text, Padding = Thickness 8.0, CornerRadius = CornerRadius 4.0, BorderThickness = Thickness 1.0)
              box.Bind(Border.BorderBrushProperty, box.GetResourceObservable Theme.Tokens.Border) |> ignore
              box
          | :? ListBlock as list ->
              let items = StackPanel(Spacing = 4.0)
              let mutable number = match Int32.TryParse list.OrderedStart with | true, n -> n | _ -> 1
              for item in list do
                  let marker = TextBlock(Text = (if list.IsOrdered then $"{number}." else "•"), Margin = Thickness(4.0, 0.0, 8.0, 0.0))
                  number <- number + 1
                  let row = DockPanel()
                  DockPanel.SetDock(marker, Dock.Left)
                  row.Children.Add marker
                  let body = StackPanel(Spacing = 4.0)
                  match item with
                  | :? ContainerBlock as inner -> body.Children.AddRange(blocks inner)
                  | _ -> ()
                  row.Children.Add body
                  items.Children.Add row
              items
          | :? QuoteBlock as quote ->
              let body = StackPanel(Spacing = 6.0)
              body.Children.AddRange(blocks quote)
              let bar = Border(Child = body, Padding = Thickness(10.0, 0.0, 0.0, 0.0), BorderThickness = Thickness(3.0, 0.0, 0.0, 0.0))
              bar.Bind(Border.BorderBrushProperty, bar.GetResourceObservable Theme.Tokens.Border) |> ignore
              bar
          | :? ThematicBreakBlock ->
              let rule = Border(Height = 1.0, Margin = Thickness(0.0, 4.0))
              rule.Bind(Border.BackgroundProperty, rule.GetResourceObservable Theme.Tokens.Border) |> ignore
              rule
          | :? ContainerBlock as inner -> yield! blocks inner
          | _ -> () ]

/// The Markdown, drawn as a column of its blocks.
let view (markdown: string) : Control =
    let column = StackPanel(Spacing = 10.0)
    column.Children.AddRange(blocks (Markdig.Markdown.Parse(markdown, pipeline)))
    column
