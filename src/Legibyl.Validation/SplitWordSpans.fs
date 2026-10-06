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
      Text: string
      Geometry: StructureGeometry option }

let private actualText (dictionary: PdfDictionary) =
    dictionary.GetAsString(PdfName.ActualText) |?> _.ToUnicodeString()

let rec private exactText (obj: PdfObject | null) (context: Context) (inheritedPage: int option) : string option =
    match resolve obj with
    | null -> None
    | :? PdfArray as array ->
        let parts = [ for i in 0 .. array.Size() - 1 -> exactText (array.Get i) context inheritedPage ]
        if parts |> List.forall Option.isSome then Some(parts |> List.choose id |> String.concat "") else None
    | :? PdfNumber as mcid when inheritedPage.IsSome ->
        lookup context.MarkedContent (inheritedPage.Value, mcid.IntValue())
        |> Option.bind (fun content ->
            if content.Text = "" && content.Geometry.IsSome then None else Some content.Text)
    | :? PdfDictionary as dictionary ->
        match actualText dictionary with
        | NonNull text -> Some text
        | Null ->
            let page = structElemPage dictionary context inheritedPage
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

let private canFlattenSpan (span: PdfDictionary) =
    [ PdfName.Alt; PdfName.E; PdfName.ID; PdfName.Lang; PdfName.A ]
    |> List.forall (fun key -> isNull (span.Get key))

/// Only handles blocks made entirely of Span children with known text and no existing
/// replacement text or alternate description. That keeps the fix from overriding richer
/// structure or inventing text for content it cannot extract.
let find (context: Context) (visits: StructElemVisit list) =
    [ for visit in visits do
          let element = visit.Element
          let kids = element.GetAsArray(PdfName.K)
          match standardTag element context with
          | NonNull tag when
              isTextBlock tag
              && not (isNull kids)
              && isNull (element.Get(PdfName.ActualText))
              && isNull (element.Get(PdfName.Alt))
              && isNull (element.Get(PdfName.E)) ->
              let children =
                  [ for i in 0 .. kids.Size() - 1 do
                        match resolve (kids.Get i) with
                        | :? PdfDictionary as child when
                            PdfName.Span.Equals(standardTag child context)
                            && canFlattenSpan child ->
                            yield child, exactText child context visit.Page
                        | _ -> () ]
              let allChildrenAreSpans =
                  kids.Size() > 0
                  && children.Length = kids.Size()
                  && (seq { 0 .. kids.Size() - 1 }
                      |> Seq.forall (fun i ->
                          match resolve (kids.Get i) with
                          | :? PdfDictionary as child -> PdfName.Span.Equals(standardTag child context)
                          | _ -> false))
              let texts = children |> List.choose snd
              let split =
                  texts
                  |> List.filter (String.IsNullOrEmpty >> not)
                  |> List.pairwise
                  |> List.exists (fun (left, right) -> splitsWord left right)
              if allChildrenAreSpans && texts.Length = children.Length && split then
                  yield
                      { Element = element
                        Path = visit.Path
                        Page = visit.Page
                        Text = String.concat "" texts
                        Geometry = subtreeGeometry element context visit.Page }
          | _ -> () ]

let apply (document: PdfDocument) (context: Context) =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> []
    | root ->
        let candidates = find context (walk root context |> List.ofSeq)
        for candidate in candidates do
            candidate.Element.Put(PdfName.ActualText, PdfString(candidate.Text, iText.IO.Font.PdfEncodings.UNICODE_BIG)) |> ignore
        match candidates.Length with
        | 0 -> []
        | 1 -> [ "Added replacement text to 1 text block whose Span tags split a word." ]
        | count -> [ $"Added replacement text to {count} text blocks whose Span tags split words." ]
