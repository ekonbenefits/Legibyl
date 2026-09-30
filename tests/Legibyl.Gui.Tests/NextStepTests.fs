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
let ``nothing failing and no warnings, or only dismissed ones: no next step`` () =
    let dismissed = { finding "7.5" 1 FindingSeverity.Warning PdfFixOptions.None with Dismissed = true }
    NextStep.decide [] nothingDone |> should equal None
    NextStep.decide [ dismissed ] nothingDone |> should equal None

[<Fact>]
let ``passing with warnings: review them, and dismiss those that are inaccurate`` () =
    let rows =
        [ yield! times 2 (finding "Structure" 7 FindingSeverity.Warning PdfFixOptions.Bookmarks)
          finding "Structure" 3 FindingSeverity.Warning PdfFixOptions.None
          // Dismissed already: not counted.
          { finding "Structure" 5 FindingSeverity.Warning PdfFixOptions.None with Dismissed = true } ]
    let step = decide rows nothingDone

    step.Title |> should equal "Review the warnings"
    step.Details.Head |> should haveSubstring "with 3 warnings"
    step.Details |> List.exists (fun d -> d.Contains "right-click it › Dismiss") |> should be True
    step.Details |> should contain "2 warnings have an auto fix: choose it in Auto Fixes… if what it would do is right."
    step.TopRules |> List.map (fun g -> g.RuleId, g.Findings.Length) |> should equal [ "Structure-7", 2; "Structure-3", 1 ]

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
let ``safe auto fixes not chosen come first; guessing ones are left to check in Structure`` () =
    let rows =
        [ yield! times 2 (failure "7.1" 10 PdfFixOptions.Tabs)
          yield! times 3 (failure "7.1" 8 PdfFixOptions.Bookmarks)
          failure "7.2" 1 PdfFixOptions.Metadata
          yield! times 2 (failure "7.1" 1 PdfFixOptions.None) ]
    let step = decide rows { nothingDone with AutoFixesChosen = Some PdfFixOptions.Metadata }

    step.Title |> should equal "Choose auto fixes"
    step.Details.Head |> should equal "2 of its 8 failures have a safe auto fix (no guesswork) not chosen yet."
    step.Details |> List.exists (fun d -> d.StartsWith "3 failures have an auto fix that guesses" && d.Contains "in Structure… rather than running another auto fix") |> should be True
    step.Details |> should contain "2 failures have no auto fix: fix them in Structure…, or where the PDF was made."
    step.TopRules |> List.map _.RuleId |> should equal [ "7.1-8"; "7.1-1" ]

[<Fact>]
let ``the metadata fix isn't suggested until the safe fixes fix every other failure`` () =
    let waiting = [ failure "7.1" 10 PdfFixOptions.Tabs; failure "5" 1 PdfFixOptions.Metadata; failure "7.1" 1 PdfFixOptions.None ]
    let ready = [ failure "7.1" 10 PdfFixOptions.Tabs; failure "5" 1 PdfFixOptions.Metadata ]
    let tabsChosen = { nothingDone with AutoFixesChosen = Some PdfFixOptions.Tabs }

    let step = decide waiting tabsChosen
    step.Title |> should equal "Export and check again"
    step.Details |> should contain "1 failure has only the XMP metadata fix, which veraPDF only writes once every other failure is fixed: leave it until then."
    step.TopRules |> List.map _.RuleId |> should equal [ "7.1-1" ]

    let step = decide ready tabsChosen
    step.Title |> should equal "Choose auto fixes"
    step.Details.Head |> should equal "1 of its 2 failures has a safe auto fix (no guesswork) not chosen yet."

[<Fact>]
let ``only guessing auto fixes left: check the structure, not another auto fix pass`` () =
    let rows = [ yield! times 3 (failure "7.1" 8 PdfFixOptions.Lists); failure "7.1" 1 PdfFixOptions.None ]
    let step = decide rows nothingDone

    step.Title |> should equal "Check the structure"
    step.Details.Head |> should haveSubstring "3 of its 4 failures have only an auto fix that guesses"
    step.Details |> List.exists (fun d -> d.StartsWith "Rather than another auto fix pass, open Structure…") |> should be True
    step.TopRules |> List.map _.RuleId |> should equal [ "7.1-8"; "7.1-1" ]

[<Fact>]
let ``with auto fixes chosen, or edits not in the PDF yet, it's Export and check again`` () =
    let rows = [ failure "7.1" 8 PdfFixOptions.Bookmarks; failure "7.1" 1 PdfFixOptions.None ]
    let chosen = decide rows { nothingDone with AutoFixesChosen = Some PdfFixOptions.Bookmarks }
    let edited = decide [ failure "7.1" 1 PdfFixOptions.None ] { nothingDone with EditsPending = 1 }

    chosen.Title |> should equal "Export and check again"
    chosen.Details.Head |> should equal "The auto fixes chosen fix 1 failure."
    chosen.Details |> should contain "1 failure has no auto fix: fix them in Structure…, or where the PDF was made."
    edited.Title |> should equal "Export and check again"
    edited.Details.Head |> should equal "1 edit is not in the PDF yet."
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
