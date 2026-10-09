/// The document's language (/Lang), detected from its text for documents that don't declare
/// one. PDF/UA needs every piece of text's language to be known; without a /Lang, veraPDF
/// fails each one (7.2-34 and the other "natural language" rules). Detection uses
/// LanguageDetection (a naive Bayes model over 53 languages' Wikipedia profiles), and only
/// answers when there's enough text and it's confident.
module internal Legibyl.Validation.DocumentLanguage

open System
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

/// The detector's languages (its ISO 639-3 codes), each with the BCP 47 tag /Lang takes (two
/// letters, as usual) and its name in English. A table rather than .NET's cultures, which the
/// CLI doesn't carry (it runs with invariant globalization).
let private languages =
    dict
        [ "afr", ("af", "Afrikaans")
          "ara", ("ar", "Arabic")
          "ben", ("bn", "Bangla")
          "bul", ("bg", "Bulgarian")
          "ces", ("cs", "Czech")
          "dan", ("da", "Danish")
          "deu", ("de", "German")
          "ell", ("el", "Greek")
          "eng", ("en", "English")
          "est", ("et", "Estonian")
          "fas", ("fa", "Persian")
          "fin", ("fi", "Finnish")
          "fra", ("fr", "French")
          "guj", ("gu", "Gujarati")
          "heb", ("he", "Hebrew")
          "hin", ("hi", "Hindi")
          "hrv", ("hr", "Croatian")
          "hun", ("hu", "Hungarian")
          "ind", ("id", "Indonesian")
          "ita", ("it", "Italian")
          "jpn", ("ja", "Japanese")
          "kan", ("kn", "Kannada")
          "kor", ("ko", "Korean")
          "lav", ("lv", "Latvian")
          "lit", ("lt", "Lithuanian")
          "mal", ("ml", "Malayalam")
          "mar", ("mr", "Marathi")
          "mkd", ("mk", "Macedonian")
          "nep", ("ne", "Nepali")
          "nld", ("nl", "Dutch")
          "nor", ("no", "Norwegian")
          "pan", ("pa", "Punjabi")
          "pol", ("pl", "Polish")
          "por", ("pt", "Portuguese")
          "ron", ("ro", "Romanian")
          "rus", ("ru", "Russian")
          "slk", ("sk", "Slovak")
          "slv", ("sl", "Slovenian")
          "som", ("so", "Somali")
          "spa", ("es", "Spanish")
          "sqi", ("sq", "Albanian")
          "swa", ("sw", "Swahili")
          "swe", ("sv", "Swedish")
          "tam", ("ta", "Tamil")
          "tel", ("te", "Telugu")
          "tgl", ("tl", "Tagalog")
          "tha", ("th", "Thai")
          "tur", ("tr", "Turkish")
          "twi", ("tw", "Twi")
          "ukr", ("uk", "Ukrainian")
          "urd", ("ur", "Urdu")
          "vie", ("vi", "Vietnamese")
          "zho", ("zh", "Chinese") ]

/// "eng" -> "en": the detector's code as the tag /Lang takes. Codes it doesn't know are kept.
let tagFor (code: string) =
    match languages.TryGetValue code with
    | true, (tag, _) -> tag
    | _ -> code

/// "en" -> "English": a tag tagFor gives, named in English (the tag itself for others).
let nameOf (tag: string) =
    languages.Values |> Seq.tryFind (fun (t, _) -> t = tag) |> Option.map snd |> Option.defaultValue tag

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

/// The pages' text, enough of it to detect the language from (pages whose text can't be read
/// left out).
let private pagesText (document: PdfDocument) =
    let text = StringBuilder()
    let mutable page = 1
    while text.Length < SampleLength && page <= document.GetNumberOfPages() do
        try
            text.Append(PdfTextExtractor.GetTextFromPage(document.GetPage page)).Append(' ') |> ignore
        with _ ->
            ()
        page <- page + 1
    text

/// The language the document says it's in (its /Lang), if it says.
let declared (document: PdfDocument) =
    match document.GetCatalog().GetPdfObject().GetAsString PdfName.Lang with
    | NonNull language when not (String.IsNullOrWhiteSpace(language.ToUnicodeString())) -> Some(language.ToUnicodeString().Trim())
    | _ -> None

/// A tag's language, without its region or script ("es-MX" -> "es").
let primary (tag: string) = tag.Split('-').[0].Trim().ToLowerInvariant()

/// Detectors that tell only some languages apart, by their codes.
let private among = Collections.Concurrent.ConcurrentDictionary<string, LanguageDetector>()

/// The language a short text (a paragraph, a heading) is in, among these languages (tags) only:
/// told apart far more surely than among all 53. None unless it has three words or more and the
/// detector is all but certain.
let detectAmong (tags: string list) (text: string) : string option =
    let codes =
        tags
        |> List.choose (fun tag -> languages |> Seq.tryFind (fun kv -> fst kv.Value = primary tag) |> Option.map _.Key)
        |> List.distinct
        |> List.sort
    let words = text.Split((null: char array | null), StringSplitOptions.RemoveEmptyEntries) |> Array.filter (Seq.exists Char.IsLetter)
    if codes.Length < 2 || words.Length < 3 || (text |> Seq.filter Char.IsLetter |> Seq.length) < 15 then
        None
    else
        let detector =
            among.GetOrAdd(
                String.Join("+", codes),
                fun _ ->
                    let detector = LanguageDetector()
                    detector.AddLanguages(Array.ofList codes)
                    detector.RandomSeed <- Nullable 1
                    detector
            )
        let candidates = lock detector (fun () -> detector.DetectAll text |> List.ofSeq)
        match candidates |> List.sortByDescending _.Probability with
        | best :: _ when best.Probability >= 0.99 -> Some(tagFor best.Language)
        | _ -> None

/// The languages detection knows: each one's tag and its name in English.
let known () = languages.Values |> List.ofSeq

/// The language the document's pages' text reads as, if that's clear.
let readsAs (document: PdfDocument) = (pagesText document).ToString() |> detect

/// The language the document's text is in: the one it says, else the one its pages' text reads
/// as, if that's clear.
let ofDocument (document: PdfDocument) =
    declared document |> Option.orElseWith (fun () -> (pagesText document).ToString() |> detect)

/// Text to detect the language from: the pages' text, and the structure's replacement text
/// (ActualText, Alt), which is all there is when text is drawn as outlines.
let sample (document: PdfDocument) context =
    let text = pagesText document
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

/// The language the document says it's in and the one its text reads as, when they're different
/// languages (not just regions of one), the text clearly reads as the other, and that's one of
/// those given (the languages with a pack): text whose letters are garbled (a font mapping them
/// wrong) can read as Slovak or Somali, but isn't taken for a language Legibyl knows well.
let mismatch (among: string list) (document: PdfDocument) context =
    match declared document with
    | Some said ->
        match detect (sample document context) with
        | Some reads when primary reads <> primary said && among |> List.exists (fun tag -> primary tag = primary reads) -> Some(said, reads)
        | _ -> None
    | None -> None

/// "es-MX" -> "es-MX (Spanish)": a tag as a message gives it, named where it's known.
let describe (tag: string) =
    match nameOf (primary tag) with
    | name when name = primary tag -> tag
    | name -> $"{tag} ({name})"
