/// The checklist a person works through for a PDF that passes (the Pass row): its steps, for this
/// file, how far it's got, and how a report lists it.
namespace Legibyl.Gui

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

/// A step of the checklist a person works through for a PDF that passes (see the Pass row).
type ChecklistStep =
    { /// Its name, as the review keeps it ticked: "title", "alt-text"…
      Id: string
      Title: string
      /// What the file has, where it shows ("Title: Annual Report").
      Shown: string option
      /// What to check, and where to do it.
      What: string
      /// Whether it's done in Structure… (the step has a button to open it).
      InStructure: bool }

module Checklist =
    /// The steps for a passing PDF, those that don't apply (no figures, tables or links) left out.
    let steps (facts: DocumentFacts.Facts) : ChecklistStep list =
        let plural (n: int) (what: string) = if n = 1 then $"1 {what}" else $"{n} {what}s"
        [ { Id = "title"
            Title = "Title"
            Shown = Some(match facts.Title with Some title -> $"Title: {title}" | None -> "No title of its own")
            What = "Is it the document's real title (not \"Untitled\", or a file name)? If not, give it one in Structure…'s Document title, or where the PDF was made."
            InStructure = true }
          { Id = "language"
            Title = "Language"
            Shown =
              Some(
                  match facts.Language with
                  | Some(tag, Some name) -> $"Language: {tag} ({name})"
                  | Some(tag, None) -> $"Language: {tag}"
                  | None -> "No language declared"
              )
            What = "Is it the language the text is in? If not, choose it in Structure… › Document language, and Export…."
            InStructure = true }
          if facts.Figures > 0 then
              { Id = "alt-text"
                Title = "Alt text"
                Shown = Some(plural facts.Figures "figure" + $", {facts.FiguresWithAlt} with alt text")
                What =
                  "In Structure…, select a figure › Edit Alt Text… (or F2): say what it's for here, in a sentence. A figure that's only decoration: give it the tag Artifact (not read) in the Tag picker."
                InStructure = true }
          { Id = "headings"
            Title = "Headings"
            Shown =
              Some(
                  match facts.Headings with
                  | [] -> "No headings"
                  | levels ->
                      let counts = levels |> List.map (fun (level, n) -> $"{level} ×{n}") |> String.concat ", "
                      "Headings: " + counts
              )
            What = "In Structure…, check each is a heading at the right level and every section has one; the Tag picker changes a level."
            InStructure = true }
          { Id = "reading-order"
            Title = "Reading order"
            Shown = None
            What = "Structure… numbers each page's elements in the order they're read, on the page preview. To change it, select an element and Move Up or Move Down (Alt+Up, Alt+Down), or Order Page n by Layout for the whole page."
            InStructure = true }
          if facts.Tables > 0 then
              { Id = "tables"
                Title = "Tables"
                Shown = Some(plural facts.Tables "table")
                What = "Header cells should be marked as headers (TH): in the tool that made the PDF, or a PDF editor's table tools."
                InStructure = false }
          if facts.Links > 0 then
              { Id = "links"
                Title = "Links"
                Shown = Some(plural facts.Links "link")
                What = "Each link's text or description should say where it goes: Structure…, right-click a link › Edit Link Description…."
                InStructure = true }
          { Id = "colour"
            Title = "Colour"
            Shown = None
            What = "Nothing should be conveyed by colour, contrast or position alone: look at the page."
            InStructure = false } ]

    /// How far a person has checked it, as the Pass row says: "3 of 7 checked", "Checked by a
    /// person"; before any, that it passes.
    let progress (profile: string) (steps: ChecklistStep list) (ticked: Set<string>) =
        let done' = steps |> List.filter (fun s -> ticked.Contains s.Id) |> List.length
        if steps.IsEmpty || done' = 0 then $"Passes {profile}'s automated checks"
        elif done' = steps.Length then "Checked by a person"
        else $"{done'} of {steps.Length} checked"

    /// The checklist as a report has it: what a person verified, not only what a program did.
    let text (profile: string) (steps: ChecklistStep list) (ticked: Set<string>) =
        let lines =
            [ $"PASS — compliant with {profile}. {progress profile steps ticked}."
              "PDF/UA also needs a person to check:"
              for i, step in List.indexed steps do
                  let mark = if ticked.Contains step.Id then "[x]" else "[ ]"
                  let shown = match step.Shown with Some shown -> $" ({shown})" | None -> ""
                  $"{mark} {i + 1}. {step.Title}{shown}: {step.What}" ]
        String.concat Environment.NewLine lines + Environment.NewLine + Environment.NewLine
