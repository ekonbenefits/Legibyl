/// A validation's result kept with the review (see SavedResults), shown at once on reopening.
module Legibyl.Gui.Tests.SavedValidationTests

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

/// A copy of pdfua1-fail.pdf in a fresh folder.
let private withCopy (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "report.pdf")
    File.Copy(Path.Combine(AppContext.BaseDirectory, "SampleCorpus", "pdfua1-fail.pdf"), pdf)
    try test pdf finally Directory.Delete(folder, true)

/// The result's banner ("" while it's checking).
let private banner (window: Window) =
    descendants<TextBlock> window
    |> List.map (fun t -> string t.Text)
    |> List.tryFind (fun t -> t.StartsWith "FAIL" || t.StartsWith "PASS")
    |> Option.defaultValue ""

/// Whether File › Save has something to save.
let private canSave (window: Window) =
    let file = NativeMenu.GetMenu window |> nonNull |> _.Items |> Seq.cast<NativeMenuItem> |> Seq.head
    (nonNull file.Menu).Items |> Seq.pick (function :? NativeMenuItem as item when string item.Header = "Save" -> Some item.IsEnabled | _ -> None)

/// Opens pdf in a new window, as reopening it would: its review read from the file again.
let private reopen (pdf: string) =
    DocumentShell.Shared.Reviews.Forget pdf
    let window = HeadlessWindow.show 1200.0 720.0
    HeadlessWindow.openPdf window pdf
    window

let private closeWithoutSaving (window: Window) =
    window.Close()
    pump ()
    for asked in List.ofSeq window.OwnedWindows do
        if asked.Title = "Save Your Review?" then
            (descendants<Button> asked |> List.find (fun b -> string b.Content = "Don't Save")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            pump ()

/// The review file's saved result, changed by change.
let private changeSaved (pdf: string) (change: ReviewFile.SavedValidation -> ReviewFile.SavedValidation) =
    let file = (ReviewFile.readFile (ReviewFile.filePath pdf)).Value
    File.WriteAllText(ReviewFile.filePath pdf, ReviewFile.toJson { file with Validation = change (nonNull file.Validation) })

[<Fact>]
let ``a validation is a change Save keeps, and reopening shows it at once`` () =
    withCopy (fun pdf ->
        onUiThread (fun () ->
            let window = reopen pdf
            let editedAfterValidating = canSave window
            let findings = HeadlessWindow.findings window |> List.length
            Reviews.chooseFromFileMenu window "Save" |> ignore
            pump ()
            let saved = (ReviewFile.readFile (ReviewFile.filePath pdf)).Value.Validation |> nonNull
            window.Close()
            // Take one finding out of what's saved: if reopening shows one fewer, it's the saved
            // result shown, not a fresh validation.
            changeSaved pdf (fun v -> { v with Findings = v.Findings[1..] })

            let reopened = reopen pdf
            let shown = HeadlessWindow.findings reopened |> List.length
            let said = banner reopened
            let editedOnReopening = canSave reopened
            reopened.Close()

            editedAfterValidating |> should be True
            saved.Hash |> should equal (SavedResults.hashOf pdf)
            saved.VeraPdfVersion |> should equal VeraPdfValidator.EngineVersion
            shown |> should equal (findings - 1)
            said |> should haveSubstring ", as checked "
            editedOnReopening |> should be False))

[<Fact>]
let ``a changed file, or another veraPDF, is validated afresh`` () =
    withCopy (fun pdf ->
        onUiThread (fun () ->
            let window = reopen pdf
            Reviews.chooseFromFileMenu window "Save" |> ignore
            pump ()
            window.Close()
            changeSaved pdf (fun v -> { v with VeraPdfVersion = "1.26.0" })
            let otherEngine = reopen pdf
            let saidForOtherEngine = banner otherEngine
            closeWithoutSaving otherEngine
            // A byte added after the end: the same PDF to a reader, not to the hash.
            changeSaved pdf (fun v -> { v with VeraPdfVersion = VeraPdfValidator.EngineVersion })
            File.AppendAllText(pdf, "\n")
            let changed = reopen pdf
            let saidForChanged = banner changed
            closeWithoutSaving changed

            saidForOtherEngine |> should not' (haveSubstring "as checked")
            saidForChanged |> should not' (haveSubstring "as checked")))

[<Fact>]
let ``Check Again validates afresh, whatever's saved, and it's a change to keep`` () =
    withCopy (fun pdf ->
        onUiThread (fun () ->
            let window = reopen pdf
            Reviews.chooseFromFileMenu window "Save" |> ignore
            pump ()
            window.Close()
            let reopened = reopen pdf
            let before = banner reopened
            (descendants<Button> reopened |> List.find (fun b -> string b.Content = "Check Again")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 30.0) (fun () -> let b = banner reopened in b <> "" && not (b.Contains "as checked")) |> ignore
            let after = banner reopened
            let edited = canSave reopened
            closeWithoutSaving reopened

            before |> should haveSubstring "as checked"
            after |> should not' (haveSubstring "as checked")
            edited |> should be True))
