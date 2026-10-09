module Legibyl.Gui.Tests.AppendDialogTests

open System
open System.IO
open Avalonia.Controls
open Avalonia.VisualTree
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Screenshots
open Legibyl.Validation
open Legibyl.Gui.Tests.HeadlessSession

let private descendants<'T when 'T :> Avalonia.Visual> (root: Avalonia.Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

[<Fact>]
let ``File › Append PDFs… lists the PDF open first, and joins only two or more`` () =
    let pdf = Path.Combine(AppContext.BaseDirectory, "SampleCorpus", "pdfua1-pass.pdf")
    onUiThread (fun () ->
        let window = HeadlessWindow.show 1200.0 720.0
        HeadlessWindow.openPdf window pdf
        let file = (NativeMenu.GetMenu window |> nonNull).Items |> Seq.cast<NativeMenuItem> |> Seq.exactlyOne
        let append = (nonNull file.Menu).Items |> Seq.pick (function :? NativeMenuItem as item when string item.Header = "Append PDFs…" -> Some item | _ -> None)
        (nonNull append.Command).Execute null
        HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) (fun () -> window.OwnedWindows.Count > 0) |> ignore
        let dialog = window.OwnedWindows |> Seq.last
        let listed = descendants<ListBox> dialog |> List.exactlyOne |> _.Items |> Seq.map string |> List.ofSeq
        let join = descendants<Button> dialog |> List.find (fun b -> string b.Content = "Join…")
        let joinEnabled = join.IsEnabled
        let title = dialog.Title
        dialog.Close()
        window.Close()

        title |> should equal "Append PDFs"
        listed |> should equal [ "1. pdfua1-pass.pdf" ]
        joinEnabled |> should be False)

[<Fact>]
let ``a file moves up and down the list, and not past its ends`` () =
    let files = [ "a.pdf"; "b.pdf"; "c.pdf" ]
    AppendDialog.move files 1 -1 |> should equal [ "b.pdf"; "a.pdf"; "c.pdf" ]
    AppendDialog.move files 1 1 |> should equal [ "a.pdf"; "c.pdf"; "b.pdf" ]
    AppendDialog.move files 0 -1 |> should equal files
    AppendDialog.move files 2 1 |> should equal files

let private joined (notes: string list) : Append.Joined =
    { Output = "/tmp/packet.pdf"
      Profile = PdfProfile.PdfUa1
      Title = "Packet"
      Language = Some "en-US"
      Parts =
        [ { File = "/tmp/garden.pdf"; FirstPage = 1; Pages = 2; RenamedTags = Map.empty }
          { File = "/tmp/jardin.pdf"; FirstPage = 3; Pages = 2; RenamedTags = Map.empty } ]
      Notes = notes
      NotCarriedOver = []
      AllNotes = notes }

let private report (compliant: bool) : ValidationReport =
    { FilePath = "/tmp/packet.pdf"
      Profile = PdfProfile.PdfUa1
      FlavourId = "ua1"
      PdfVersion = "1.7"
      ProfileDetected = true
      IsCompliant = compliant
      TotalAssertions = 100
      Assertions = []
      Notes = []
      WcagError = null }

[<Fact>]
let ``the summary says why nothing was written, or how the joined file checks out and what was renamed`` () =
    let refused: Append.Outcome =
        { Refusals = [ { File = "/tmp/jardin.pdf"; Reason = "It has a form." }; { File = null; Reason = "PDF 1.7 and 2.0 can't be joined." } ]
          Inputs = []
          Joined = None
          After = None
          Differences = [] }
    AppendDialog.summary refused
    |> should equal (String.Join(Environment.NewLine, [ "Nothing was written."; ""; "Why:"; "• jardin.pdf: It has a form."; "• PDF 1.7 and 2.0 can't be joined." ]))
    let passing = { refused with Refusals = []; Joined = Some(joined [ "Renamed the tag <Callout> in jardin.pdf to <Callout-2>." ]); After = Some(report true) }
    AppendDialog.summary passing
    |> should
        equal
        (String.Join(
            Environment.NewLine,
            [ "Joined 2 files into packet.pdf. It passes PDF/UA-1, and each part reads as its file did."
              ""
              "Changed to join them:"
              "• Renamed the tag <Callout> in jardin.pdf to <Callout-2>." ]
        ))
    AppendDialog.summary { passing with After = Some(report false); Joined = Some(joined []) }
    |> should equal "Joined 2 files into packet.pdf, but it doesn't pass PDF/UA-1: its report says why."

[<Fact>]
let ``a review not carried over is said under its own heading, not as a change made to join them`` () =
    let review = "The review saved in garden.pdf wasn't carried over: its edits name pages by number."
    let outcome: Append.Outcome =
        { Refusals = []
          Inputs = []
          Joined = Some { joined [] with NotCarriedOver = [ review ] }
          After = Some(report true)
          Differences = [] }
    AppendDialog.summary outcome
    |> should
        equal
        (String.Join(
            Environment.NewLine,
            [ "Joined 2 files into packet.pdf. It passes PDF/UA-1, and each part reads as its file did."
              ""
              "Not carried over:"
              "• " + review ]
        ))
