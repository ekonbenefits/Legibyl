/// The Next step row a PDF that fails opens on: what to do next, from its findings and what's
/// been done in its review.
module Legibyl.Gui.Tests.NextStepTests

open System
open Avalonia.Controls
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

/// A finding: rule clause-test, a failure unless said, fixed by fixes.
let private finding (clause: string) (test: int) (severity: FindingSeverity) (fixes: PdfFixOptions) =
    RuleViolationRow.Of
        { Specification = "ISO 14289-1:2014"
          Clause = clause
          TestNumber = test
          Passed = false
          Message = $"Rule {clause}-{test}"
          ErrorMessage = null
          LocationContext = null
          Severity = severity
          FixOptions = fixes
          Geometry = null
          Element = null }

let private failure clause test fixes = finding clause test FindingSeverity.Failure fixes
let private times n row = List.replicate n row

let private nothingDone: NextStep.Progress = { StructureSuggested = false; EditsPending = 0; AutoFixesChosen = None }

let private decide rows progress = NextStep.decide rows progress |> Option.get

[<Fact>]
let ``nothing failing, no next step`` () =
    NextStep.decide [ finding "7.1" 1 FindingSeverity.Warning PdfFixOptions.None ] nothingDone |> should equal None

[<Fact>]
let ``an untagged PDF suggests its structure, then exporting a tagged copy`` () =
    let rows = [ yield! times 4 (failure "7.1" 3 PdfFixOptions.TagFromLayout); failure "7.2" 1 PdfFixOptions.Title ]
    let first = decide rows nothingDone
    let suggested = decide rows { nothingDone with StructureSuggested = true; EditsPending = 2 }

    first.Title |> should equal "Suggest the structure"
    first.Details.Head |> should haveSubstring "Tagging it fixes 4 of its 5 failures."
    suggested.Title |> should equal "Export to tag a copy"
    suggested.Details.Head |> should equal "The structure's been suggested, with 2 edits on it."

[<Fact>]
let ``a tagged PDF with auto fixes not chosen suggests choosing them, and says what's left by hand`` () =
    let rows =
        [ yield! times 3 (failure "7.1" 8 PdfFixOptions.Bookmarks)
          failure "7.2" 1 PdfFixOptions.Title
          yield! times 2 (failure "7.1" 1 PdfFixOptions.None) ]
    let step = decide rows { nothingDone with AutoFixesChosen = Some PdfFixOptions.Title }

    step.Title |> should equal "Choose auto fixes"
    step.Details.Head |> should equal "3 of its 6 failures have an auto fix not chosen yet."
    step.Details |> should contain "2 failures have no auto fix: fix them in Structure…, or where the PDF was made. The rules with the most:"
    step.TopRules |> List.map _.RuleId |> should equal [ "7.1-1" ]

[<Fact>]
let ``with every auto fix chosen, or edits not in the PDF yet, it's Export and check again`` () =
    let rows = [ failure "7.1" 8 PdfFixOptions.Bookmarks; failure "7.1" 1 PdfFixOptions.None ]
    let chosen = decide rows { nothingDone with AutoFixesChosen = Some PdfFixOptions.Bookmarks }
    let edited = decide [ failure "7.1" 1 PdfFixOptions.None ] { nothingDone with EditsPending = 1 }

    chosen.Title |> should equal "Export and check again"
    chosen.Details.Head |> should equal "The auto fixes chosen fix 1 failure."
    chosen.Details |> should contain "1 failure has no auto fix: fix them in Structure…, or where the PDF was made. The rules with the most:"
    edited.Title |> should equal "Export and check again"
    edited.Details.Head |> should equal "1 edit is not in the PDF yet."

[<Fact>]
let ``only failures by hand: the three rules broken the most, the most first`` () =
    let rows =
        [ failure "7.1" 1 PdfFixOptions.None
          yield! times 5 (failure "7.18" 5 PdfFixOptions.None)
          yield! times 2 (failure "7.3" 1 PdfFixOptions.None)
          yield! times 3 (failure "7.2" 34 PdfFixOptions.None)
          // Warnings aren't what it counts.
          yield! times 9 (finding "7.5" 1 FindingSeverity.Warning PdfFixOptions.None) ]
    let step = decide rows nothingDone

    step.Title |> should equal "Fix the rules with the most failures"
    step.Details.Head |> should haveSubstring "11 failures left, none with an auto fix."
    step.TopRules |> List.map (fun g -> g.RuleId, g.Findings.Length) |> should equal [ "7.18-5", 5; "7.2-34", 3; "7.3-1", 2 ]

let private texts (window: Window) =
    window.GetVisualDescendants() |> Seq.choose (function :? TextBlock as t -> Some(string t.Text) | _ -> None) |> List.ofSeq

let private corpusFile name = IO.Path.Combine(AppContext.BaseDirectory, "SampleCorpus", name)

[<Fact>]
let ``a PDF that fails opens on its Next step row, first, and says what to do`` () =
    onUiThread (fun () ->
        let window = HeadlessWindow.show 1200.0 720.0
        HeadlessWindow.openPdf window (corpusFile "pdfua1-fail.pdf")
        let items = HeadlessWindow.resultItems window
        let selected = (HeadlessWindow.resultsList window).SelectedItem
        let shown = texts window
        let menu = (HeadlessWindow.resultsList window).ContextMenu |> nonNull |> _.Items |> Seq.cast<MenuItem> |> List.ofSeq
        window.Close()

        let step = match items with NextStepItem step :: RuleItem _ :: _ -> Some step | _ -> None
        step |> should not' (equal None)
        (match selected with :? ResultItem as NextStepItem _ -> true | _ -> false) |> should be True
        shown |> should contain "NEXT STEP"
        shown |> should contain $"Next step: {step.Value.Title}"
        for detail in step.Value.Details do
            shown |> should contain detail
        menu |> List.map (fun m -> string m.Header, m.IsEnabled) |> should equal [ "Nothing to dismiss: this is the next step", false ])
