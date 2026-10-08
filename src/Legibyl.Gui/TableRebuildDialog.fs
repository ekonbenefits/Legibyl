/// The grid a table's rows and cells are rebuilt as, for a person to check before it's kept: the
/// cells the table-structure model found on the page, laid out in their rows and columns (with
/// their spans) as the table is drawn, each with the text drawn in it and a picker to make it a
/// header cell (TH) or a data cell (TD).
module internal Legibyl.Gui.TableRebuildDialog

open System
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.Layout
open Avalonia.Media
open Legibyl.Validation

/// Shows cells (texts: the text drawn in each) over owner; the cells as checked, or None if
/// cancelled.
let showAs (owner: Window) (title: string) (instructions: string) (cells: TableCell list) (texts: string list) : Task<TableCell list option> =
    let window =
        HostWindow(
            Title = title,
            Width = 720.0,
            SizeToContent = SizeToContent.Height,
            CanResize = true,
            ShowInTaskbar = false,
            WindowStartupLocation = WindowStartupLocation.CenterOwner
        )
    let places = TableGrid.place [ for cell in cells -> cell.Box ]
    let rowCount = places |> List.map (fun p -> p.Row + p.RowSpan) |> List.fold max 1
    let columnCount = places |> List.map (fun p -> p.Column + p.ColSpan) |> List.fold max 1
    let mutable chosen = None
    window.Content <-
        Component(fun ctx ->
            let tags = ctx.useState [ for cell in cells -> cell.Tag ]
            StackPanel.create [
                StackPanel.margin 16.0
                StackPanel.spacing 12.0
                StackPanel.children [
                    TextBlock.create [
                        TextBlock.textWrapping TextWrapping.Wrap
                        TextBlock.text $"{instructions} The grid has {rowCount} row(s) and {columnCount} column(s). Check each cell is a header cell (TH) or a data cell (TD)."
                    ]
                    ScrollViewer.create [
                        ScrollViewer.maxHeight 480.0
                        ScrollViewer.horizontalScrollBarVisibility Primitives.ScrollBarVisibility.Auto
                        ScrollViewer.content (
                            Grid.create [
                                Grid.columnDefinitions (String.Join(",", List.replicate columnCount "*"))
                                Grid.rowDefinitions (String.Join(",", List.replicate rowCount "Auto"))
                                Grid.children [
                                    for index, ((place, text), tag) in List.indexed (List.zip (List.zip places texts) tags.Current) ->
                                        Border.create [
                                            Grid.row place.Row
                                            Grid.column place.Column
                                            Grid.rowSpan place.RowSpan
                                            Grid.columnSpan place.ColSpan
                                            Border.borderThickness 1.0
                                            Border.padding 6.0
                                            Theme.resources [
                                                Border.BorderBrushProperty, Theme.Tokens.Border
                                                Border.BackgroundProperty, (if tag = Tags.TH then Theme.Tokens.Fill else Theme.Tokens.Background)
                                            ]
                                            Border.child (
                                                StackPanel.create [
                                                    StackPanel.spacing 4.0
                                                    StackPanel.children [
                                                        TextBlock.create [
                                                            TextBlock.text (if text = "" then "(empty)" else text)
                                                            TextBlock.textWrapping TextWrapping.Wrap
                                                            TextBlock.fontWeight (if tag = Tags.TH then FontWeight.SemiBold else FontWeight.Normal)
                                                        ]
                                                        ComboBox.create [
                                                            ComboBox.dataItems [ Tags.TH; Tags.TD ]
                                                            ComboBox.selectedItem tag
                                                            AttrBuilder<ComboBox>.CreateProperty<string | null>(
                                                                Automation.AutomationProperties.NameProperty,
                                                                $"Cell in row {place.Row + 1}, column {place.Column + 1}",
                                                                ValueNone
                                                            )
                                                            ToolTip.tip "TH, a header cell (it heads its column or row); TD, a data cell."
                                                            ComboBox.onSelectedItemChanged (
                                                                (fun item ->
                                                                    match item with
                                                                    | :? string as picked when picked <> tags.Current[index] ->
                                                                        tags.Set(tags.Current |> List.mapi (fun i t -> if i = index then picked else t))
                                                                    | _ -> ()),
                                                                SubPatchOptions.Always
                                                            )
                                                        ]
                                                    ]
                                                ]
                                            )
                                        ]
                                ]
                            ]
                        )
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
                                    chosen <- Some [ for cell, tag in List.zip cells tags.Current -> { cell with Tag = tag } ]
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

let show (owner: Window) (cells: TableCell list) (texts: string list) : Task<TableCell list option> =
    showAs
        owner
        "Rebuild Rows and Cells"
        "The table's rows and cells, as found on the page. Export then tags the table as these, each piece of it in the cell it's drawn in."
        cells
        texts
