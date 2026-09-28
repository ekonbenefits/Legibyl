/// The details pane's parts for a finding or a rule: its title by its severity, how it's fixed
/// (Auto fix or Suggest Structure), and its text, selectable.
module Legibyl.Gui.FindingDetails

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Data
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Input.Platform
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform.Storage
open Avalonia.Styling
open Legibyl.Layout
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Gui.BusyStatus
open Legibyl.Gui.Icons

/// The wrench and the wand, as the rest of the main window calls them.
let fixIcon = Icons.wrench
let wandIcon = Icons.wand

let severityIconAndColor (violation: RuleViolation) =
    match violation.Severity with
    | FindingSeverity.Warning -> Icons.warning, Theme.Tokens.Warning
    | _ -> Icons.failure, Theme.Tokens.Danger

let severityIcon (violation: RuleViolation) =
    let icon, color = severityIconAndColor violation
    gridIcon icon color (string violation.Severity)

/// The details pane's title, anchored by the severity icon.
let detailTitle (violation: RuleViolation) (title: IView) =
    let icon, color = severityIconAndColor violation
    withIcon (decorativeIcon icon color 18.0) title

/// The icon for how a finding is fixed: a wand for tagging from the suggested structure, a
/// wrench for an auto fix.
let fixIconFor (violation: RuleViolation) =
    if violation.FixOptions.HasFlag PdfFixOptions.TagFromLayout then wandIcon else fixIcon

/// How a finding is fixed, named after the button that does it: "Auto fix", or, for tagging
/// from the suggested structure, "Suggest Structure" (the heading adds "fixes it").
let fixedBy (violation: RuleViolation) =
    if violation.FixOptions.HasFlag PdfFixOptions.TagFromLayout then "Suggest Structure" else "Auto fix"

/// The details pane's heading for how a finding is fixed, anchored by its icon.
let fixableHeading (violation: RuleViolation) =
    let text = if violation.FixOptions.HasFlag PdfFixOptions.TagFromLayout then "Suggest Structure fixes it" else "Auto fix"
    withIcon
        (decorativeIcon (fixIconFor violation) Theme.Tokens.Primary 14.0)
        (TextBlock.create [ TextBlock.text text; TextBlock.fontWeight FontWeight.SemiBold ])

/// A wand for findings suggesting the structure fixes, a wrench for those an auto fix
/// fixes; nothing otherwise.
let fixableIcon (row: RuleViolationRow) : IView =
    if row.Violation.IsFixable then
        gridIcon (fixIconFor row.Violation) Theme.Tokens.Primary $"{fixedBy row.Violation}: {row.Fixable}"
    else
        TextBlock.create []

/// The Auto Fixes button's icon: the wrench its fixes have in the results.
let autoFixesIcon = Some(fixIcon, Theme.Tokens.Primary)

/// The Suggest Structure button's icon: the wand its fixes have, white on its filled colour.
let suggestIcon = Some(wandIcon, "SemiColorWhite")

let detailText (text: string) (secondary: bool) (attrs: IAttr<TextBox> list) =
    TextBox.create [
        if secondary then
            Theme.resources [ TextBox.ForegroundProperty, Theme.Tokens.SecondaryText ]
        TextBox.text text
        TextBox.isReadOnly true
        TextBox.textWrapping TextWrapping.Wrap
        TextBox.borderThickness 0.0
        TextBox.background "Transparent"
        TextBox.padding (0.0, 2.0)
        TextBox.minHeight 0.0
        yield! attrs
    ]
