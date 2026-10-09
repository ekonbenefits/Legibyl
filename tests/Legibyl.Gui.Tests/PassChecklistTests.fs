/// The Pass row a PDF that passes opens on, and its checklist of what a person checks.
module Legibyl.Gui.Tests.PassChecklistTests

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.VisualTree
open iText.Kernel.Pdf
open iText.Layout.Element
open iText.Pdfua
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private texts (window: Window) = descendants<TextBlock> window |> List.map (fun t -> string t.Text)

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

/// A PDF/UA-1 file (iText's PDF/UA support makes it so): a heading and a paragraph, and no
/// bookmarks, which Legibyl warns about (Structure-7). In a fresh folder.
let private withPassingPdf (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "garden.pdf")
    try
        do
            use document = new PdfUADocument(new PdfWriter(pdf), PdfUAConfig(PdfUAConformance.PDF_UA_1, "The Community Garden", "en-US"))
            let font =
                iText.Kernel.Font.PdfFontFactory.CreateFont(
                    Path.Combine(AppContext.BaseDirectory, "SampleCorpus", "fonts", "Lato-Regular.ttf"),
                    iText.IO.Font.PdfEncodings.WINANSI,
                    iText.Kernel.Font.PdfFontFactory.EmbeddingStrategy.FORCE_EMBEDDED
                )
            use layout = new iText.Layout.Document(document)
            layout.SetFont font |> ignore
            let heading = Paragraph "The Community Garden"
            heading.GetAccessibilityProperties().SetRole(iText.Kernel.Pdf.Tagging.StandardRoles.H1) |> ignore
            layout.Add heading |> ignore
            layout.Add(Paragraph "Open to every resident of the neighbourhood, whatever they grow.") |> ignore
        test pdf
    finally
        Directory.Delete(folder, true)

let private reopen (pdf: string) =
    DocumentShell.Shared.Reviews.Forget pdf
    let window = HeadlessWindow.show 1200.0 720.0
    HeadlessWindow.openPdf window pdf
    HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) (fun () -> texts window |> List.exists (fun t -> t.StartsWith "1. Title")) |> ignore
    window

let private passProgress (window: Window) =
    HeadlessWindow.resultItems window |> List.pick (function PassItem progress -> Some progress | _ -> None)

let private banner (window: Window) =
    texts window |> List.tryFind (fun t -> t.StartsWith "PASS") |> Option.defaultValue ""

/// Ticks a step's Checked box.
let private tick (window: Window) (step: string) =
    let border = descendants<Border> window |> List.find (fun b -> (b.Tag :? string) && string b.Tag = $"step-{step}")
    (descendants<CheckBox> border |> List.exactlyOne).IsChecked <- true
    pump ()

let private closeWithoutSaving (window: Window) =
    window.Close()
    pump ()
    for asked in List.ofSeq window.OwnedWindows do
        if asked.Title = "Save Your Review?" then
            (descendants<Button> asked |> List.find (fun b -> string b.Content = "Don't Save")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()

[<Fact>]
let ``a PDF that passes with a warning opens on its Pass row, the warning below it`` () =
    withPassingPdf (fun pdf ->
        onUiThread (fun () ->
            let window = reopen pdf
            let items = HeadlessWindow.resultItems window
            let selected = (HeadlessWindow.resultsList window).SelectedItem
            let shown = texts window
            let menu = (HeadlessWindow.resultsList window).ContextMenu |> nonNull |> _.Items |> Seq.cast<MenuItem> |> List.ofSeq
            closeWithoutSaving window

            (match items with PassItem _ :: NextStepItem step :: RuleItem(group, _) :: _ -> step.Title = "Review the warnings" && group.RuleId = "Structure-7" | _ -> false) |> should be True
            (match selected with :? ResultItem as PassItem _ -> true | _ -> false) |> should be True
            shown |> should contain "1 warning below is worth a look: what Legibyl noticed that PDF/UA doesn't require, and WCAG 2.2 above level AA."
            shown |> should contain "Title: The Community Garden"
            shown |> should contain "Language: en-US (English)"
            shown |> should contain "Headings: H1 ×1"
            // The pass can't be dismissed.
            menu |> List.map (fun m -> string m.Header, m.IsEnabled) |> should equal [ "Nothing to dismiss: the PDF passes", false ]))

[<Fact>]
let ``ticking steps shows progress; they're saved for this file, and all ticked it's checked by a person`` () =
    withPassingPdf (fun pdf ->
        onUiThread (fun () ->
            let window = reopen pdf
            tick window "title"
            let afterOne = passProgress window
            Reviews.chooseFromFileMenu window "Save" |> ignore
            pump ()
            let saved = (ReviewFile.readFile (ReviewFile.filePath pdf)).Value.Checklist |> nonNull
            window.Close()

            let reopened = reopen pdf
            let stillTicked = passProgress reopened
            let steps =
                descendants<Border> reopened
                |> List.choose (fun b -> match b.Tag with :? string as tag when tag.StartsWith "step-" -> Some(tag.Substring 5) | _ -> None)
            for step in steps do
                tick reopened step
            let allTicked = passProgress reopened
            let said = banner reopened
            closeWithoutSaving reopened

            afterOne |> should equal $"1 of {steps.Length} checked"
            saved.Hash |> should equal (SavedResults.hashOf pdf)
            saved.Checked |> should equal [| "title" |]
            stillTicked |> should equal afterOne
            allTicked |> should equal "Checked by a person"
            said |> should haveSubstring ", checked by a person"))

[<Fact>]
let ``ticks don't carry to another version of the file`` () =
    withPassingPdf (fun pdf ->
        onUiThread (fun () ->
            let window = reopen pdf
            tick window "title"
            Reviews.chooseFromFileMenu window "Save" |> ignore
            pump ()
            window.Close()
            File.AppendAllText(pdf, "\n")
            let changed = reopen pdf
            let progress = passProgress changed
            closeWithoutSaving changed

            progress |> should equal "Passes PDF/UA-1 and WCAG 2.2's automated checks"))

[<Fact>]
let ``a report leads with the checklist and what's been checked`` () =
    let facts: DocumentFacts.Facts =
        { Title = Some "The Community Garden"; Language = Some("en", Some "English"); Figures = 0; FiguresWithAlt = 0
          Headings = [ "H1", 1 ]; Tables = 0; Links = 0 }
    let steps = Checklist.steps facts
    let text = Checklist.text "PDF/UA-1" steps (set [ "title"; "language" ])

    text |> should startWith $"PASS — compliant with PDF/UA-1. 2 of {steps.Length} checked."
    text |> should haveSubstring "[x] 1. Title (Title: The Community Garden)"
    text |> should haveSubstring "[ ] 3. Headings (Headings: H1 ×1)"
    steps |> List.map _.Id |> should equal [ "title"; "language"; "headings"; "reading-order"; "colour" ]

/// Export…: its badge (the steps to go into the copy, 0 for none), and whether it's enabled.
let private exportButton (window: Window) =
    let button = descendants<Button> window |> List.find (fun b -> (string (Automation.AutomationProperties.GetName b)).StartsWith "Export…")
    let badge =
        descendants<TextBlock> button |> List.map (fun t -> string t.Text) |> List.tryPick (fun t -> match Int32.TryParse t with true, n -> Some n | _ -> None)
    badge |> Option.defaultValue 0, button.IsEnabled

[<Fact>]
let ``any tick enables Export, badged with the steps to go into the copy`` () =
    withPassingPdf (fun pdf ->
        onUiThread (fun () ->
            let window = reopen pdf
            let before = exportButton window
            tick window "title"
            let afterOne = exportButton window
            tick window "language"
            let afterTwo = exportButton window
            let said = texts window
            closeWithoutSaving window

            before |> should equal (0, false)
            afterOne |> should equal (1, true)
            afterTwo |> should equal (2, true)
            said |> List.exists (fun t -> t.StartsWith "2 of " && t.EndsWith " steps checked. Export… writes a copy that carries them.") |> should be True))

/// The original's review with title and language ticked, and a copy written carrying them.
let private withCheckedCopy (test: string -> string -> unit) =
    withPassingPdf (fun pdf ->
        let copy = Path.Combine(Path.GetDirectoryName pdf |> nonNull, "garden.v2.pdf")
        let review = DocumentReviews().For pdf
        let hash = SavedResults.hashOf pdf
        review.Checklist <- Some(hash, set [ "title"; "language" ])
        review.ChecklistAt <- Some(DateTime(2026, 9, 27, 10, 0, 0))
        review.WriteCheckedCopy(pdf, copy, hash)
        test pdf copy)

[<Fact>]
let ``the copy carries the steps checked, the original's hash and the date, and checks the same`` () =
    withCheckedCopy (fun pdf copy ->
        let carried = ReviewFile.carriedChecklist copy |> Option.get
        let original = VeraPdfValidator().Validate(pdf, PdfProfile.PdfUa1)
        let copied = VeraPdfValidator().Validate(copy, PdfProfile.PdfUa1)

        nonNull carried.Checked |> Array.sort |> should equal [| "language"; "title" |]
        carried.Hash |> should equal (SavedResults.hashOf pdf)
        carried.CheckedAt |> should equal (Nullable(DateTime(2026, 9, 27, 10, 0, 0)))
        // The original, unchanged, with the review added after it.
        File.ReadAllBytes(copy)[.. int (FileInfo(pdf).Length) - 1] |> should equal (File.ReadAllBytes pdf)
        copied.IsCompliant |> should be True
        [ for a in copied.Assertions -> a.RuleId, a.LocationContext ] |> should equal [ for a in original.Assertions -> a.RuleId, a.LocationContext ]
        // The original carries none.
        ReviewFile.carriedChecklist pdf |> should equal None)

[<Fact>]
let ``a copy changed since doesn't carry its checks, nor one exported with changes`` () =
    withCheckedCopy (fun pdf copy ->
        // Its first bytes changed: not what was checked.
        let bytes = File.ReadAllBytes copy
        bytes[20] <- bytes[20] ^^^ 1uy
        let changed = Path.ChangeExtension(copy, ".changed.pdf") |> nonNull
        File.WriteAllBytes(changed, bytes)
        // Exported with an auto fix: its review has no checklist.
        let fixedCopy = Path.ChangeExtension(copy, ".fixed.pdf") |> nonNull
        let review = DocumentReviews().For pdf
        review.Checklist <- Some(SavedResults.hashOf pdf, set [ "title" ])
        VeraPdfValidator(Review = review.ForCopy pdf).Fix(pdf, PdfProfile.PdfUa1, fixedCopy, PdfFixOptions.Bookmarks) |> ignore

        ReviewFile.carriedChecklist changed |> should equal None
        File.Exists fixedCopy |> should be True
        (EmbeddedReview.read fixedCopy |> Option.bind ReviewFile.parse |> Option.get).Checklist |> should equal null)

[<Fact>]
let ``opening the copy shows its steps checked, read-only, and ticks more to go into another copy`` () =
    withCheckedCopy (fun _ copy ->
        onUiThread (fun () ->
            let window = reopen copy
            let said = texts window
            let titleBox =
                let border = descendants<Border> window |> List.find (fun b -> (b.Tag :? string) && string b.Tag = "step-title")
                descendants<CheckBox> border |> List.exactlyOne
            let carried = titleBox.IsChecked, titleBox.IsEnabled
            let atFirst = exportButton window
            let steps =
                descendants<Border> window
                |> List.choose (fun b -> match b.Tag with :? string as tag when tag.StartsWith "step-" -> Some(tag.Substring 5) | _ -> None)
            let third = steps |> List.find (fun s -> s <> "title" && s <> "language")
            tick window third
            let afterThird = exportButton window
            Reviews.chooseFromFileMenu window "Save" |> ignore
            pump ()
            let saved = (ReviewFile.readFile (ReviewFile.filePath copy)).Value.Checklist |> nonNull
            window.Close()

            said |> should contain "2 steps checked by a person on 27 Sep 2026, in this copy."
            carried |> should equal (Nullable true, false)
            atFirst |> should equal (0, false)
            afterThird |> should equal (1, true)
            saved.Hash |> should equal (SavedResults.hashOf copy)
            saved.Checked |> should equal [| third |]))
