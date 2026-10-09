/// Passages in another language than the one they're read in: a Spanish paragraph in an English
/// document, say, which a screen reader reads with an English voice unless it says it's Spanish
/// (its own /Lang, as PDF/UA asks). Each block of text (a paragraph, a heading, a list item, a
/// cell…) is told apart among the document's language and those with a language pack only
/// (English and Spanish to begin with; see LanguagePacks), which short text can be. Bilingual
/// documents, English and Spanish side by side, are the common case.
module internal Legibyl.Validation.PassageLanguages

open System
open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

/// The blocks of text a passage is: one that holds no other is a passage.
let private blocks =
    Tags.headings
    @ [ Tags.P; Tags.H; Tags.Title; Tags.LI; Tags.LBody; Tags.TD; Tags.TH; Tags.Caption; Tags.Note; Tags.BlockQuote; Tags.Quote; Tags.TOCI ]
    |> Set.ofList

let private isBlock (element: PdfDictionary) context =
    match standardTag element context with
    | NonNull tag -> blocks.Contains(tag.GetValue())
    | Null -> false

/// Whether a block holds another (a list item its body, a cell its paragraph): then it's the
/// inner one that's a passage.
let private holdsBlock (element: PdfDictionary) context =
    let rec kids (value: PdfObject | null) depth =
        depth < 50
        && match resolve value with
           | :? PdfArray as array -> seq { 0 .. array.Size() - 1 } |> Seq.exists (fun i -> kids (array.Get i) depth)
           | :? PdfDictionary as kid when not (isNull (kid.GetAsName PdfName.S)) ->
               isBlock kid context || kids (kid.Get PdfName.K) (depth + 1)
           | _ -> false
    kids (element.Get PdfName.K) 0

let private langOf (dictionary: PdfDictionary) =
    match dictionary.GetAsString PdfName.Lang with
    | NonNull language when not (String.IsNullOrWhiteSpace(language.ToUnicodeString())) -> Some(language.ToUnicodeString().Trim())
    | _ -> None

/// The language an element is read in when it says none: its nearest ancestor's, else the
/// document's (as it is now: a fix before may have declared it).
let private inherited (document: PdfDocument) (element: PdfDictionary) =
    let rec climb (dictionary: PdfDictionary | null) depth =
        match dictionary with
        | null -> DocumentLanguage.declared document
        | _ when depth > 200 -> DocumentLanguage.declared document
        | dictionary ->
            match langOf dictionary with
            | Some language -> Some language
            | None -> climb (dictionary.GetAsDictionary PdfName.P) (depth + 1)
    climb (element.GetAsDictionary PdfName.P) 0

/// A passage in another language than the one it's read in.
type Passage =
    { Element: PdfDictionary
      Path: string
      Page: int option
      /// The language its text reads as.
      Language: string
      /// The language it's read in now.
      ReadIn: string
      Text: string
      Geometry: StructureGeometry option }

/// The languages a passage is told apart among: the one it's read in, and those with a pack.
let private candidates (readIn: string) =
    readIn :: (fst LanguagePacks.installed.Value |> List.map _.Tag)

/// Each passage (a block of text holding no other, saying no language of its own) and the language
/// its text reads as, where that's clear; with the language it's read in, if it has one.
let private passages (document: PdfDocument) context =
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> []
    | root ->
        [ for visit in walk root context do
              if isBlock visit.Element context && (langOf visit.Element).IsNone && not (holdsBlock visit.Element context) then
                  let text = (extractText visit.Element context visit.Page).Trim()
                  let readIn = inherited document visit.Element
                  let among = candidates (defaultArg readIn "en")
                  match DocumentLanguage.detectAmong among text with
                  | Some language -> visit, text, language, readIn
                  | None -> () ]

/// The passages in another language than the one they're read in, in reading order.
let find (document: PdfDocument) context : Passage list =
    [ for visit, text, language, readIn in passages document context do
          match readIn with
          | Some readIn when DocumentLanguage.primary language <> DocumentLanguage.primary readIn ->
              let geometry = subtreeGeometry visit.Element context visit.Page
              { Element = visit.Element
                Path = visit.Path
                // Its page, else where its text is drawn.
                Page = visit.Page |> Option.orElse (geometry |> Option.map _.PageNumber)
                Language = language
                ReadIn = readIn
                Text = text
                Geometry = geometry }
          | _ -> () ]

/// The language most of a document's passages are in, by their letters, when one has three in
/// five or more; else the first passage's. For a document whose text as a whole isn't clearly
/// one language (English and Spanish side by side).
let mostly (document: PdfDocument) context : string option =
    let read = [ for _, text, language, _ in passages document context -> DocumentLanguage.primary language, text |> Seq.filter Char.IsLetter |> Seq.length ]
    let total = read |> List.sumBy snd
    match read |> List.groupBy fst |> List.map (fun (language, items) -> language, List.sumBy snd items) |> List.sortByDescending snd with
    | (language, letters) :: _ when total > 0 && float letters >= 0.6 * float total -> Some language
    | _ -> read |> List.tryHead |> Option.map fst

/// Gives each passage in another language its own /Lang.
let apply (document: PdfDocument) context =
    let found = find document context
    for passage in found do
        passage.Element.Put(PdfName.Lang, PdfString passage.Language) |> ignore
    [ match found with
      | [] -> ()
      | found ->
          let byLanguage =
              found
              |> List.countBy (fun passage -> DocumentLanguage.describe passage.Language)
              |> List.map (fun (language, count) -> $"{count} in {language}")
              |> String.concat ", "
          $"Marked {found.Length} passage(s) in another language than the one around them with their own: {byLanguage}." ]
