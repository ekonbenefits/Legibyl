/// What a tagged page draws outside every tag, in the Structure window (see UntaggedContent).
module Legibyl.Gui.Tests.UntaggedStructureTests

open System
open System.IO
open Avalonia
open Avalonia.Automation
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.VisualTree
open iText.Kernel.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas
open iText.Kernel.Pdf.Tagging
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation
open Legibyl.Screenshots
open Legibyl.Gui.Tests.HeadlessSession


/// What a button says: its screen-reader name (an edit button's label, after its pencil), else its text.
let private buttonText (b: Button) =
    match Automation.AutomationProperties.GetName b with
    | null | "" -> string b.Content
    | name -> name

let private descendants<'T when 'T :> Visual> (root: Visual) : 'T list =
    root.GetVisualDescendants() |> Seq.choose (fun v -> match box v with :? 'T as c -> Some c | _ -> None) |> List.ofSeq

let private pump () = HeadlessWindow.pumpUntil TimeSpan.Zero (fun () -> true) |> ignore

let private texts (root: Visual) = descendants<TextBlock> root |> List.map (fun t -> string t.Text)

/// A tagged PDF, each page with a paragraph, and outside every tag: a background fill, a rule,
/// an image and a line of text. In a fresh folder.
let private withPdf (pages: int) (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let pdf = Path.Combine(folder, "newsletter.pdf")
    try
        do
            use document = new PdfDocument(new PdfWriter(pdf))
            document.SetTagged() |> ignore
            let font = PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA
            let documentElement = document.GetStructTreeRoot().AddKid(PdfStructElem(document, PdfName.Document))
            for _ in 1 .. pages do
                let page = document.AddNewPage()
                let canvas = PdfCanvas(page)
                canvas.SetFillColorRgb(0.95f, 0.95f, 0.9f).Rectangle(0.0, 0.0, 612.0, 792.0).Fill() |> ignore
                let element = documentElement.AddKid(PdfStructElem(document, PdfName.P, page))
                let reference = PdfMcrNumber(page, element)
                element.AddKid reference |> ignore
                let properties = PdfDictionary()
                properties.Put(PdfName.MCID, PdfNumber(reference.GetMcid())) |> ignore
                canvas.BeginMarkedContent(PdfName.P, properties).BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 700.0).ShowText("The garden.").EndText().EndMarkedContent()
                |> ignore
                canvas.SaveState().SetLineWidth(2f).MoveTo(72.0, 690.0).LineTo(540.0, 690.0).Stroke().RestoreState() |> ignore
                let pixels = [| 255uy; 0uy; 0uy; 0uy; 255uy; 0uy; 0uy; 0uy; 255uy; 255uy; 255uy; 0uy |]
                canvas.AddImageWithTransformationMatrix(iText.IO.Image.ImageDataFactory.Create(2, 2, 3, 8, pixels, null), 60f, 0f, 0f, 60f, 72f, 560f) |> ignore
                canvas.BeginText().SetFontAndSize(font, 12f).MoveText(72.0, 500.0).ShowText("Stray text.").EndText() |> ignore
        test pdf
    finally
        Directory.Delete(folder, true)

let private show (pdf: string) =
    let window = StructureTreeWindow(VeraPdfValidator().ReadStructureTree pdf |> nonNull, pdf)
    window.Width <- 1000.0
    window.Height <- 760.0
    window.Show()
    pump ()
    window

/// The blocks of pieces outside the tags, in order.
let private pieceBlocks (window: Window) =
    descendants<Border> window |> List.filter (fun b -> match b.Tag with :? StructureNode as n -> n.Tag = "Untagged" | _ -> false)

let private nodeOf (block: Border) = nonNull block.Tag :?> StructureNode

let private click (block: Border) (extend: bool) =
    let modifiers = if extend then Input.KeyModifiers.Shift else Input.KeyModifiers.None
    let properties = Input.PointerPointProperties(Input.RawInputModifiers.LeftMouseButton, Input.PointerUpdateKind.LeftButtonPressed)
    block.RaiseEvent(Input.PointerPressedEventArgs(block, new Input.Pointer(0, Input.PointerType.Mouse, true), block, Point(4.0, 4.0), 0UL, properties, modifiers))
    pump ()

let private buttons (window: Window) = descendants<Button> window |> List.map buttonText

let private press (window: Window) (label: string) =
    (descendants<Button> window |> List.find (fun b -> buttonText b = label)).RaiseEvent(RoutedEventArgs Button.ClickEvent)
    pump ()

let private blockStarting (window: Window) (start: string) =
    pieceBlocks window |> List.find (fun b -> (string (nodeOf b).Content).StartsWith start)

[<Fact>]
let ``disconnected text is visible separately and cannot be moved as a reading-order sibling`` () =
    withPdf 1 (fun pdf ->
        let disconnected = Path.Combine(Path.GetDirectoryName(pdf) |> nonNull, "disconnected.pdf")
        do
            use document = new PdfDocument(new PdfReader(pdf), new PdfWriter(disconnected))
            let root = document.GetStructTreeRoot()
            let parent = root.GetKids()[0] :?> PdfStructElem
            let page = document.GetPage 1
            let orphan = parent.AddKid(PdfStructElem(document, PdfName.P, page))
            let reference = PdfMcrNumber(page, orphan)
            orphan.AddKid reference |> ignore
            let properties = PdfDictionary()
            properties.Put(PdfName.MCID, PdfNumber(reference.GetMcid())) |> ignore
            PdfCanvas(page).BeginMarkedContent(PdfName.P, properties).BeginText()
                .SetFontAndSize(PdfFontFactory.CreateFont iText.IO.Font.Constants.StandardFonts.HELVETICA, 12f)
                .MoveText(72.0, 400.0).ShowText("Disconnected disclosure.").EndText().EndMarkedContent() |> ignore
            let kids = parent.GetPdfObject().GetAsArray PdfName.K
            kids.Remove(kids.Size() - 1)
        onUiThread (fun () ->
            let window = show disconnected
            try
                texts window |> List.exists (fun text -> text.Contains "Disconnected associations") |> should be True
                texts window |> List.exists (fun text -> text.Contains "Disconnected disclosure.") |> should be True
                let block =
                    descendants<Border> window
                    |> List.find (fun border ->
                        match border.Tag with
                        | :? StructureNode as node -> node.Tag = "DisconnectedTag"
                        | _ -> false)
                let node = nodeOf block
                node.Geometry |> should not' (be Null)
                StructureActions.canRetag false node |> should be False
                StructureActions.editKinds node |> should be Empty
                StructureActions.fixedOrder.Contains "Disconnected" |> should be True
                click block false
                buttons window |> List.exists (fun label -> label = "Move Up" || label = "Move Down") |> should be False
            finally window.Close()))

[<Fact>]
let ``what's drawn outside the tags is listed per page, likely decoration first, with hints`` () =
    withPdf 1 (fun pdf ->
        onUiThread (fun () ->
            let window = show pdf
            let order = pieceBlocks window |> List.map (fun b -> (string (nodeOf b).Content).Split(',').[0])
            let shown = texts window
            window.Close()

            shown |> should contain "Not in the tags"
            order |> should equal [ "a fill"; "a line"; "an image"; "text \"Stray text.\"" ]
            shown |> should contain "likely decoration: the page's background"
            shown |> should contain "likely decoration: a rule or border"))

[<Fact>]
let ``a piece of drawing can be made not read, or tagged as a figure with alt text, and kept untagged again`` () =
    withPdf 1 (fun pdf ->
        onUiThread (fun () ->
            let window = show pdf
            click (blockStarting window "a fill") false
            let forDrawing = buttons window
            press window "Not Read (Artifact)"
            let madeDecoration = window.Review.Edits |> List.map (fun e -> e.Tag, e.Retag)
            press window "Keep Untagged"
            let kept = window.Review.Edits
            click (blockStarting window "an image") false
            press window "Tag as Figure"
            let figure = window.Review.Edits |> List.map (fun e -> e.Tag, e.Retag)
            let canDescribe = buttons window |> List.contains "Edit Alt Text…"
            click (blockStarting window "text") false
            let forText = buttons window
            window.Close()

            forDrawing |> should contain "Not Read (Artifact)"
            forDrawing |> should contain "Tag as Figure"
            forDrawing |> should not' (contain "Tag as Paragraph")
            madeDecoration |> should equal [ "Untagged", Some "Artifact" ]
            kept |> should be Empty
            figure |> should equal [ "Untagged", Some "Figure" ]
            canDescribe |> should be True
            // Text is read: tagged as a paragraph, never made decoration.
            forText |> should contain "Tag as Paragraph"
            forText |> should not' (contain "Not Read (Artifact)")))

[<Fact>]
let ``Mark All Like This marks every piece like it, on every page, once confirmed`` () =
    withPdf 3 (fun pdf ->
        onUiThread (fun () ->
            let window = show pdf
            click (blockStarting window "a fill") false
            press window "Mark All Like This…"
            let asked = window.OwnedWindows |> Seq.exactlyOne
            let question = texts asked |> List.find (fun t -> t.StartsWith "Mark ")
            press asked "Mark 3 pieces as Decoration"
            let marked = window.Review.Edits |> List.map (fun e -> e.Page, e.Retag)
            window.Close()

            question |> should haveSubstring "Mark 3 pieces like this (a fill, about 612 × 792 pt), on pages 1, 2, 3, as decoration?"
            marked |> List.sort |> should equal [ 1, Some "Artifact"; 2, Some "Artifact"; 3, Some "Artifact" ]))

[<Fact>]
let ``a piece's failures are marked on it`` () =
    withPdf 1 (fun pdf ->
        onUiThread (fun () ->
            let window = HeadlessWindow.show 1200.0 720.0
            HeadlessWindow.openPdf window pdf
            (descendants<Button> window |> List.find (fun b -> (string (AutomationProperties.GetName b)).StartsWith "Structure…")).RaiseEvent(RoutedEventArgs Button.ClickEvent)
            let structure () = window.OwnedWindows |> Seq.tryPick (fun w -> match w with :? StructureTreeWindow as s -> Some s | _ -> None)
            HeadlessWindow.pumpUntil (TimeSpan.FromSeconds 10.0) (fun () -> (structure ()).IsSome) |> ignore
            let structureWindow = structure () |> Option.get
            pump ()
            let marked =
                descendants<Button> structureWindow
                |> List.filter (fun b -> (b.Tag :? string) && string b.Tag = "finding-marker")
                |> List.choose (fun m ->
                    m.GetVisualAncestors()
                    |> Seq.tryPick (fun v -> match v with :? Border as b -> (match b.Tag with :? StructureNode as n when n.Tag = "Untagged" -> Some n | _ -> None) | _ -> None))
            structureWindow.Close()
            window.Close()

            marked |> List.map (fun n -> (string n.Content).Split(',').[0]) |> should contain "a fill"))
