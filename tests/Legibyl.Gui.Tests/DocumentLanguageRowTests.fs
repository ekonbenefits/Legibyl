/// Structure…'s Document language: the language a person chooses for the document.
module Legibyl.Gui.Tests.DocumentLanguageRowTests

open System
open System.IO
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.VisualTree
open iText.IO.Font.Constants
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Layout.Element
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Avalonia.Visual> (root: Avalonia.Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private texts (root: Avalonia.Visual) = descendants<TextBlock> root |> List.map (fun t -> string t.Text)

/// A tagged PDF of Spanish paragraphs that says it's in English, in a fresh folder.
let private withSpanishSaidEnglish (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "jardin.pdf")
    try
        do
            use document = new PdfDocument(new PdfWriter(pdf))
            document.SetTagged() |> ignore
            document.GetCatalog().SetLang(PdfString "en-US") |> ignore
            use layout = new iText.Layout.Document(document)
            layout.SetFont(PdfFontFactory.CreateFont(StandardFonts.HELVETICA, "WinAnsi")) |> ignore
            for text in
                [ "Aventuras, insignias y compañerismo: niños de preescolar a quinto grado prueban cosas nuevas."
                  "Junto a sus familias, guiados por padres voluntarios como usted, cada sábado en el jardín." ] do
                layout.Add(Paragraph text) |> ignore
        test pdf
    finally
        Directory.Delete(folder, true)

[<Fact>]
let ``a PDF that says it's in English with Spanish text says so, and a person can choose Spanish`` () =
    withSpanishSaidEnglish (fun pdf ->
        onUiThread (fun () ->
            let review = DocumentReviews().For pdf
            let source = StructureSource.Tagged(VeraPdfValidator().ReadStructureTree pdf |> nonNull)
            let window = StructureTreeWindow(source, pdf, review, ignore, FindingMarkers.none)
            window.Show()
            pump ()
            try
                let picker () = descendants<ComboBox> window |> List.find (fun c -> string (AutomationProperties.GetName c) = "Document language")
                string (picker ()).SelectedItem |> should equal "English (en-US)"
                texts window |> List.exists (fun text -> text.Contains "It says English, but its text reads as Spanish") |> should be True

                (descendants<Button> window |> List.find (fun b -> string b.Content = "Use Spanish")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
                pump ()

                review.Language |> should equal (Some "es")
                string (picker ()).SelectedItem |> should equal "Spanish (es)"
                texts window |> List.exists (fun text -> text.Contains "but its text reads as") |> should be False
                // Saved next to the PDF as the review's language.
                (review.ToFile pdf).Language |> should equal "es"
            finally
                window.Close()))
