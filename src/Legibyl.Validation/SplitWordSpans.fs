/// Finds text blocks whose adjacent Span children split a word, and reconstructs their text
/// from those spans for a replacement-text fix.
module internal Legibyl.Validation.SplitWordSpans

open System
open iText.Kernel.Pdf
open Legibyl.Validation.Interop
open Legibyl.Validation.StructureTreeObjects

type Candidate =
    { Element: PdfDictionary
      Path: string
      Page: int option
      /// The block's text from its spans: all of it when Fixable, else the spans' that are known.
      Text: string
      Geometry: StructureGeometry option
      /// Whether the fix can write Text as the block's replacement text: every child is a Span
      /// whose text is known and that carries nothing replacement text would hide (/Lang, /Alt…).
      Fixable: bool }

let private actualText (dictionary: PdfDictionary) =
    dictionary.GetAsString(PdfName.ActualText) |?> _.ToUnicodeString()

let private contentText (context: Context) (page: int option) (mcid: PdfNumber) =
    match page with
    | Some page ->
        lookup context.MarkedContent (page, mcid.IntValue())
        |> Option.bind (fun content -> if content.Text = "" && content.Geometry.IsSome then None else Some content.Text)
    | None -> None

let rec private exactText (obj: PdfObject | null) (context: Context) (inheritedPage: int option) : string option =
    match resolve obj with
    | null -> None
    | :? PdfArray as array ->
        let parts = [ for i in 0 .. array.Size() - 1 -> exactText (array.Get i) context inheritedPage ]
        if parts |> List.forall Option.isSome then Some(parts |> List.choose id |> String.concat "") else None
    | :? PdfNumber as mcid -> contentText context inheritedPage mcid
    | :? PdfDictionary as dictionary ->
        match actualText dictionary with
        | NonNull text -> Some text
        | Null ->
            let page = structElemPage dictionary context inheritedPage
            match dictionary.GetAsNumber PdfName.MCID, dictionary.Get PdfName.Stm with
            // A marked-content reference (/MCR) on a page; one into a Form XObject's stream
            // isn't in the page's text.
            | NonNull mcid, null -> contentText context page mcid
            | NonNull _, _ -> None
            | Null, _ ->
                match dictionary.Get(PdfName.K) with
                | null -> None
                | kids -> exactText kids context page
    | _ -> None

let private isTextBlock (tag: PdfName) =
    tag.Equals(PdfName.P)
    || tag.Equals(PdfName.Caption)
    || tag.Equals(PdfName.LBody)
    || tag.Equals(PdfName.TH)
    || tag.Equals(PdfName.TD)
    || ([ 1 .. 6 ] |> List.exists (fun level -> tag.Equals(PdfName($"H{level}"))))

let private splitsWord (left: string) (right: string) =
    left.Length > 0
    && right.Length > 0
    && Char.IsLetterOrDigit left[left.Length - 1]
    && Char.IsLetterOrDigit right[0]

/// Whether right's text starts where left's ends on the page: on the same line, with no more
/// room between them than letters have (a word space is about a quarter of the type's height).
/// The page's text has no space between separate spans' content, so text alone can't tell a
/// split word from two words drawn apart.
let private continues (left: StructureGeometry option) (right: StructureGeometry option) =
    match left, right with
    | Some left, Some right when left.PageNumber = right.PageNumber ->
        let height = min (left.Top - left.Bottom) (right.Top - right.Bottom)
        let overlap = min left.Top right.Top - max left.Bottom right.Bottom
        let gap = right.Left - left.Right
        height > 0.0 && overlap >= height * 0.5 && gap <= height * 0.15 && gap >= -height * 0.5
    | _ -> false

let private canFlattenSpan (span: PdfDictionary) =
    [ PdfName.Alt; PdfName.E; PdfName.ID; PdfName.Lang; PdfName.A ]
    |> List.forall (fun key -> isNull (span.Get key))

/// Text blocks with no replacement text or alternate description of their own, where two
/// adjacent Span children's text (known, and drawn one straight after the other) splits a word.
/// Each is fixable only when the whole block can be given its text back (see Candidate.Fixable),
/// so the fix doesn't override richer structure or invent text for content it can't extract.
let find (context: Context) (visits: StructElemVisit list) =
    [ for visit in visits do
          let element = visit.Element
          let kids = element.GetAsArray(PdfName.K)
          match standardTag element context with
          | NonNull tag when
              isTextBlock tag
              && not (isNull kids)
              && kids.Size() > 0
              && isNull (element.Get(PdfName.ActualText))
              && isNull (element.Get(PdfName.Alt))
              && isNull (element.Get(PdfName.E)) ->
              let page = structElemPage element context visit.Page
              // Each child: whether it's a Span, then its text and where it's drawn, if known.
              let children =
                  [ for i in 0 .. kids.Size() - 1 ->
                        match resolve (kids.Get i) with
                        | :? PdfDictionary as child when PdfName.Span.Equals(standardTag child context) ->
                            Some child, exactText child context page, textGeometry child context page
                        | _ -> None, None, None ]
              let split =
                  children
                  |> List.choose (fun (span, text, geometry) ->
                      match span, text with
                      | Some _, Some text when text <> "" -> Some(text, geometry)
                      | _ -> None)
                  |> List.pairwise
                  |> List.exists (fun ((left, leftGeometry), (right, rightGeometry)) ->
                      splitsWord left right && continues leftGeometry rightGeometry)
              if split then
                  let fixable =
                      children
                      |> List.forall (fun (span, text, _) ->
                          match span, text with
                          | Some span, Some _ -> canFlattenSpan span
                          | _ -> false)
                  yield
                      { Element = element
                        Path = visit.Path
                        Page = visit.Page
                        Text = children |> List.choose (fun (_, text, _) -> text) |> String.concat ""
                        Geometry = subtreeGeometry element context visit.Page
                        Fixable = fixable }
          | _ -> () ]

let apply (document: PdfDocument) (context: Context) =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> []
    | root ->
        let candidates = find context (walk root context |> List.ofSeq) |> List.filter _.Fixable
        for candidate in candidates do
            candidate.Element.Put(PdfName.ActualText, PdfString(candidate.Text, iText.IO.Font.PdfEncodings.UNICODE_BIG)) |> ignore
        match candidates.Length with
        | 0 -> []
        | 1 -> [ "Added replacement text to 1 text block whose Span tags split a word." ]
        | count -> [ $"Added replacement text to {count} text blocks whose Span tags split words." ]
