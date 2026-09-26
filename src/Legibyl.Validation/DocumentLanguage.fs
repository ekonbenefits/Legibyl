/// The document's language (/Lang), detected from its text for documents that don't declare
/// one. PDF/UA needs every piece of text's language to be known; without a /Lang, veraPDF
/// fails each one (7.2-34 and the other "natural language" rules). Detection uses
/// LanguageDetection (a naive Bayes model over 53 languages' Wikipedia profiles), and only
/// answers when there's enough text and it's confident.
module internal Legibyl.Validation.DocumentLanguage

open System
open System.Globalization
open System.Text
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas.Parser
open LanguageDetection
open Legibyl.Validation.StructureTreeObjects

/// Enough letters to tell languages apart reliably.
[<Literal>]
let private MinimumLetters = 100

/// How sure the detector must be.
[<Literal>]
let private MinimumProbability = 0.9

/// How much text to look at; the detector itself reads at most 10,000 characters.
[<Literal>]
let private SampleLength = 10000

let private detector =
    lazy
        (let detector = LanguageDetector()
         detector.AddAllLanguages()
         // The detector samples at random; a fixed seed gives the same answer every time.
         detector.RandomSeed <- Nullable 1
         detector)

/// Whether the document declares its language.
let isDeclared (document: PdfDocument) =
    match document.GetCatalog().GetPdfObject().GetAsString PdfName.Lang with
    | null -> false
    | language -> not (String.IsNullOrWhiteSpace(language.ToUnicodeString()))

/// "eng" -> "en": the detector's ISO 639-3 code as the BCP 47 tag /Lang takes (two letters
/// where there are two, as usual). Codes with a region ("zh-cn") are already tags.
let tagFor (code: string) =
    if code.Contains '-' then
        code
    else
        CultureInfo.GetCultures CultureTypes.NeutralCultures
        |> Array.tryFind (fun culture -> culture.ThreeLetterISOLanguageName = code && culture.TwoLetterISOLanguageName.Length = 2)
        |> Option.map _.TwoLetterISOLanguageName
        |> Option.defaultValue code

/// The language text is in, as a BCP 47 tag, if there's enough of it and the detector is sure.
let detect (text: string) : string option =
    if (text |> Seq.filter Char.IsLetter |> Seq.length) < MinimumLetters then
        None
    else
        let detector = detector.Value
        let candidates = lock detector (fun () -> detector.DetectAll text |> List.ofSeq)
        match candidates |> List.sortByDescending _.Probability with
        | best :: _ when best.Probability >= MinimumProbability -> Some(tagFor best.Language)
        | _ -> None

/// Text to detect the language from: the pages' text, and the structure's replacement text
/// (ActualText, Alt), which is all there is when text is drawn as outlines.
let sample (document: PdfDocument) context =
    let text = StringBuilder()
    let mutable page = 1
    while text.Length < SampleLength && page <= document.GetNumberOfPages() do
        text.Append(PdfTextExtractor.GetTextFromPage(document.GetPage page)).Append(' ') |> ignore
        page <- page + 1
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> ()
    | root ->
        for visit in walk root context do
            if text.Length < SampleLength then
                for key in [ PdfName.ActualText; PdfName.Alt ] do
                    match visit.Element.GetAsString key with
                    | null -> ()
                    | value -> text.Append(value.ToUnicodeString()).Append(' ') |> ignore
    text.ToString()
