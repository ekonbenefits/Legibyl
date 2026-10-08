/// Web addresses a page shows as text that nothing links (Structure-18), and linking them
/// (LinkUrls). PDF/UA doesn't ask for it, but someone using a screen reader or a keyboard can't
/// follow an address that isn't a link.
///
/// How text is taken for an address is qr-link-pdf's (github.com/jbtule/qr-link-pdf,
/// TextLinker.fs): the page's text in lines, a space where the gap between glyphs is a word's;
/// "http(s)://" or "www." up to whitespace; trailing punctuation off, a closing bracket only when
/// it isn't balanced; then an absolute web address (LinkTitles.webAddress). Text more than half
/// under a link annotation is already linked. It reads the page's glyphs, not one marked content
/// at a time, so an address split across spans is found.
module internal Legibyl.Validation.UnlinkedUrls

open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open iText.IO.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Annot
open iText.Kernel.Pdf.Tagging
open Legibyl.Validation.MarkedContentTextIndex
open Legibyl.Validation.StructureTreeObjects

/// An address on a page that nothing links.
type Candidate =
    { Page: int
      /// As the page shows it.
      Text: string
      /// Where it goes: Text, with "www." taken to be https.
      Address: string
      Geometry: StructureGeometry
      /// The marked content (MCID) that's all of it, and the element it belongs to, when the fix
      /// can tag it as a link: then it moves under a new Link element.
      Run: (int * PdfDictionary) option
      /// Why the fix leaves it, when it does.
      Left: string option }

let private addressPattern = Regex(@"(?:https?://|www\.)[^\s<>""']+", RegexOptions.IgnoreCase ||| RegexOptions.CultureInvariant)

/// Trailing punctuation off ("see https://example.org." links the address, not the full stop);
/// a closing bracket only when it isn't balanced inside, so ".../wiki/Foo_(bar)" stays whole.
let rec private trimTrailing (candidate: string) =
    if candidate = "" then
        candidate
    else
        let last = candidate[candidate.Length - 1]
        let count (c: char) = candidate |> Seq.filter ((=) c) |> Seq.length
        let trimmable =
            match last with
            | ')' -> count ')' > count '('
            | ']' -> count ']' > count '['
            | '}' -> count '}' > count '{'
            | c -> ".,;:!?\"'".Contains c
        if trimmable then trimTrailing (candidate.Substring(0, candidate.Length - 1)) else candidate

/// A page's text, its lines on lines of their own and a space wherever the gap between two
/// glyphs is a word's, and for each character the glyph it came from (-1 for those added).
let private pageText (letters: PageLetter array) =
    let text = StringBuilder()
    let from = List<int>()
    let add (s: string) (letter: int) =
        for c in s do
            text.Append c |> ignore
            from.Add letter
    let blank (s: string) = String.IsNullOrWhiteSpace s
    letters
    |> Array.iteri (fun i letter ->
        if i > 0 then
            let previous = letters[i - 1]
            let size = max previous.Size letter.Size
            if abs (letter.Baseline - previous.Baseline) > size * 0.5 then add "\n" -1
            elif not (blank letter.Value) && not (blank previous.Value) && letter.Left - previous.Right > size * 0.15 then add " " -1
        add letter.Value i)
    text.ToString(), from

/// The page's link annotations' boxes.
let private linkBoxes (page: PdfPage) =
    [ for annotation in page.GetAnnotations() do
          if PdfName.Link.Equals(annotation.GetSubtype()) then
              match annotation.GetRectangle() with
              | null -> ()
              | rectangle ->
                  let box = rectangle.ToRectangle()
                  yield float (box.GetLeft()), float (box.GetBottom()), float (box.GetRight()), float (box.GetTop()) ]

/// Whether more than half of the smaller of the two boxes is in both.
let private mostlyUnder (geometry: StructureGeometry) (left, bottom, right, top) =
    let width = min geometry.Right right - max geometry.Left left
    let height = min geometry.Top top - max geometry.Bottom bottom
    let area (w: float) (h: float) = max 0.0 w * max 0.0 h
    let smaller = min (area (geometry.Right - geometry.Left) (geometry.Top - geometry.Bottom)) (area (right - left) (top - bottom))
    smaller > 0.0 && width > 0.0 && height > 0.0 && width * height / smaller > 0.5

/// The element's kid that is mcid on page (a bare MCID, or an MCR to the page, not to a Form
/// XObject's stream), by index in its /K.
let private kidIndex (owner: PdfDictionary) (mcid: int) =
    let kids =
        match owner.Get PdfName.K with
        | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 -> array.Get i ]
        | NonNull kid -> [ kid ]
        | Null -> []
    kids
    |> List.tryFindIndex (fun kid ->
        match kid with
        | :? PdfNumber as number -> number.IntValue() = mcid
        | :? PdfDictionary as reference when isNull (reference.GetAsName PdfName.S) && isNull (reference.Get PdfName.Stm) ->
            match reference.GetAsNumber PdfName.MCID with
            | NonNull number -> number.IntValue() = mcid
            | Null -> false
        | _ -> false)

/// The addresses pages show as text that nothing links, in page order.
let find (document: PdfDocument) (context: Context) : Candidate list =
    let tagged = lazy (ParentTree.tagged document context)
    [ for KeyValue(pageNumber, letters) in context.MarkedContent.Letters |> Seq.sortBy _.Key do
          if pageNumber <= document.GetNumberOfPages() then
              let text, from = pageText letters
              let links = lazy (linkBoxes (document.GetPage pageNumber))
              for m in addressPattern.Matches text do
                  let written = trimTrailing m.Value
                  match LinkTitles.webAddress written with
                  | Some address ->
                      let glyphs = [ for i in m.Index .. m.Index + written.Length - 1 do if from[i] >= 0 then from[i] ] |> List.distinct
                      let boxes = [ for i in glyphs -> letters[i] ]
                      let geometry =
                          { PageNumber = pageNumber
                            Left = boxes |> List.map _.Left |> List.min
                            Bottom = boxes |> List.map _.Bottom |> List.min
                            Right = boxes |> List.map _.Right |> List.max
                            Top = boxes |> List.map _.Top |> List.max }
                      if not (links.Value |> List.exists (mostlyUnder geometry)) then
                          // The fix tags it when it's all of one marked content: its glyphs, less
                          // whitespace, are all of that content's.
                          let mcids = boxes |> List.map _.Mcid |> List.distinct
                          let run, left =
                              match mcids with
                              | [ mcid ] when mcid >= 0 ->
                                  let squeeze (values: string seq) = String.Concat(values).Replace(" ", "").Trim()
                                  let whole = letters |> Seq.filter (fun l -> l.Mcid = mcid) |> Seq.map _.Value |> squeeze
                                  let owner =
                                      match tagged.Value.MarkedContent.TryGetValue((pageNumber, mcid)) with
                                      | true, owner -> Some owner
                                      | _ -> None
                                  match owner with
                                  | _ when whole <> squeeze (boxes |> List.map _.Value) ->
                                      None, Some "it's part of a longer piece of text, which would have to be split to tag the address alone"
                                  | None -> None, Some "its text isn't in the tags"
                                  | Some owner when PdfName.Link.Equals(standardTag owner context) ->
                                      None, Some "it's tagged as a link already, with no link annotation to follow"
                                  | Some owner when (kidIndex owner mcid).IsNone -> None, Some "its tag holds it in a way the fix doesn't change"
                                  | Some owner -> Some(mcid, owner), None
                              | [ _ ] -> None, Some "its text isn't in the tags"
                              | _ -> None, Some "it's drawn in more than one piece, which would have to be joined to tag it as a link"
                          yield
                              { Page = pageNumber
                                Text = written
                                Address = address
                                Geometry = geometry
                                Run = run
                                Left = left }
                  | None -> () ]

/// Links each address the fix can: a borderless link annotation over it, going to it, described
/// by it, and a Link element in its tag's place holding its text and the annotation. Those it
/// can't are said, and why.
let apply (document: PdfDocument) (context: Context) =
    let candidates = find document context
    let mutable linked = 0
    for candidate in candidates do
        match candidate.Run, candidate.Left with
        | Some(mcid, owner), _ ->
            let page = document.GetPage candidate.Page
            let element = PdfStructElem owner
            match kidIndex owner mcid with
            | Some index ->
                element.RemoveKid index |> ignore
                let link = PdfStructElem(document, PdfName.Link, page)
                element.AddKid(index, link) |> ignore
                link.AddKid(PdfMcrNumber(PdfNumber mcid, link)) |> ignore
                let g = candidate.Geometry
                let annotation = PdfLinkAnnotation(iText.Kernel.Geom.Rectangle(float32 g.Left, float32 g.Bottom, float32 (g.Right - g.Left), float32 (g.Top - g.Bottom)))
                annotation.SetAction(iText.Kernel.Pdf.Action.PdfAction.CreateURI candidate.Address) |> ignore
                // No border, so the page looks the same.
                annotation.SetBorder(PdfArray([| 0.0; 0.0; 0.0 |])) |> ignore
                annotation.SetFlags PdfAnnotation.PRINT |> ignore
                annotation.SetContents(PdfString(candidate.Text, PdfEncodings.UNICODE_BIG)) |> ignore
                // Not tagged by iText, which would give it a Link of its own at the top: it's this one's.
                page.AddAnnotation(-1, annotation, false) |> ignore
                link.AddKid(PdfObjRef(annotation, link, document.GetNextStructParentIndex())) |> ignore
                // Its annotations in the order of the tags, as PDF/UA asks of a page with them.
                page.GetPdfObject().Put(PdfName.Tabs, PdfName.S) |> ignore
                linked <- linked + 1
            | None -> ()
        | None, Some why -> context.Notes.Add $"Page {candidate.Page}: \"{candidate.Text}\" isn't linked: {why}."
        | None, None -> ()
    match linked with
    | 0 -> []
    | 1 -> [ "Linked 1 web address shown as text, to where it goes." ]
    | n -> [ $"Linked {n} web addresses shown as text, each to where it goes." ]
