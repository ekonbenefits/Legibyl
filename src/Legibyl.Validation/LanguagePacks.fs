/// Language packs: what Legibyl knows of a language beyond telling that a text is in it (which
/// DocumentLanguage does for 53 languages, with nothing added), kept as data anyone can add. A
/// pack is a folder named for its language's tag ("es", "pt-BR") holding a language.yaml and,
/// if it reads scans, a Tesseract model. English and Spanish ship in the app's own languages
/// folder; others are dropped into the user's (see roots). docs/language-packs.md says how to
/// write one. A language without a pack is still checked and fixed, with Legibyl's English where
/// it writes text, and a broken pack is reported (legibyl list languages) and left out, never the
/// end of a check.
module Legibyl.Validation.LanguagePacks

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open iText.Kernel.Pdf
open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions

/// A pack's language.yaml, as written there.
[<CLIMutable>]
type PackFile =
    { /// Its tag, as /Lang takes it; the folder's name when not given.
      Tag: string | null
      /// The language's name, in English.
      Name: string | null
      /// The file name of its Tesseract model in the folder ("spa.traineddata").
      Ocr: string | null
      /// Legibyl's phrases (see phrases), translated.
      Phrases: Dictionary<string, string> | null
      /// Words that start the rest of a sentence: a paragraph beginning with one ("and", "y")
      /// likely goes on from the one before.
      SentenceContinuations: string array | null
      /// Words that start a paragraph standing on its own ("SECTION", "SECCIÓN").
      StandaloneHeadings: string array | null }

/// A language, from every folder that has a pack for it.
type Pack =
    { /// Its tag, as /Lang takes it ("es", "pt-BR").
      Tag: string
      /// Its name, in English.
      Name: string
      /// The folders it's read from, in the order read (the app's first).
      Folders: string list
      /// Its Tesseract model's path, if it has one.
      Ocr: string option
      /// Its phrases, by key (see phrases).
      Phrases: Map<string, string>
      SentenceContinuations: string list
      StandaloneHeadings: string list }

/// Something wrong with a pack, which was left out (all of it, or the part named).
type Problem = { Folder: string; Message: string }

/// The text Legibyl writes into PDFs itself, by key: in English, which a pack without the phrase
/// (or a language without a pack) gets, marked as English. A translation keeps the {names}.
let phrases =
    Map.ofList
        [ "qrCodeLinkingTo", "QR code linking to {title} ({address})"
          "qrCodeLinkingToAddress", "QR code linking to {address}"
          "qrCode", "QR code: {text}"
          "qrCodeLinkTo", "QR code link to {title} ({address})"
          "qrCodeLinkToAddress", "QR code link to {address}" ]

let private placeholders (text: string) =
    Regex.Matches(text, @"\{(\w+)\}") |> Seq.map (fun m -> m.Groups[1].Value) |> Set.ofSeq

/// The folders packs are read from, in order: the app's own, the user's, and any LEGIBYL_LANGUAGES
/// names (separated as PATH separates folders), such as one an organisation shares.
let roots () =
    [ Path.Combine(AppContext.BaseDirectory, "languages")
      Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "Legibyl", "languages")
      match Environment.GetEnvironmentVariable "LEGIBYL_LANGUAGES" with
      | NonNull folders -> yield! folders.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
      | Null -> () ]

let private deserializer = lazy (DeserializerBuilder().WithNamingConvention(CamelCaseNamingConvention.Instance).Build())

/// One folder's pack, and what's wrong with it.
let private read (folder: string) : PackFile option * Problem list =
    let file = Path.Combine(folder, "language.yaml")
    let problem message = { Folder = folder; Message = message }
    if not (File.Exists file) then
        None, [ problem "it has no language.yaml" ]
    else
        try
            match deserializer.Value.Deserialize<PackFile>(File.ReadAllText file) |> box with
            | null -> None, [ problem "its language.yaml is empty" ]
            | pack -> Some(pack :?> PackFile), []
        with ex ->
            None, [ problem $"its language.yaml can't be read: {ex.Message}" ]

/// Each folder's pack, checked: its phrases against Legibyl's, its model there.
let private checkedPack (folder: string) (file: PackFile) =
    let problems = List<Problem>()
    let problem message = problems.Add { Folder = folder; Message = message }
    let tag = match file.Tag with NonNull tag when not (String.IsNullOrWhiteSpace tag) -> tag.Trim() | _ -> string (Path.GetFileName folder)
    let ocr =
        match file.Ocr with
        | NonNull name when not (String.IsNullOrWhiteSpace name) ->
            let path = Path.Combine(folder, name)
            if Path.GetFileName name <> name || not (name.EndsWith(".traineddata", StringComparison.Ordinal)) then
                problem $"ocr names \"{name}\", not a .traineddata file in its folder"
                None
            elif not (File.Exists path) then
                problem $"ocr names {name}, which isn't in its folder"
                None
            else
                Some path
        | _ -> None
    let phrasesOf =
        [ for KeyValue(key, text) in Option.ofObj file.Phrases |> Option.defaultValue (Dictionary()) do
              match Map.tryFind key phrases with
              | None ->
                  let known = String.Join(", ", phrases.Keys)
                  problem $"its phrase {key} isn't one Legibyl writes ({known})"
              | Some _ when String.IsNullOrWhiteSpace text -> problem $"its phrase {key} is empty"
              | Some english when placeholders text <> placeholders english ->
                  let names = String.Join(", ", placeholders english |> Seq.map (fun name -> "{" + name + "}"))
                  problem $"its phrase {key} has to keep {names}, and only those"
              | Some _ -> key, text ]
    let words (items: string array | null) =
        Option.ofObj items |> Option.defaultValue [||] |> Array.choose (fun word -> if String.IsNullOrWhiteSpace word then None else Some(word.Trim())) |> List.ofArray
    { Tag = tag
      Name = match file.Name with NonNull name when not (String.IsNullOrWhiteSpace name) -> name.Trim() | _ -> tag
      Folders = [ folder ]
      Ocr = ocr
      Phrases = Map.ofList phrasesOf
      SentenceContinuations = words file.SentenceContinuations
      StandaloneHeadings = words file.StandaloneHeadings },
    List.ofSeq problems

/// Two packs for one language as one: the later's name and model where it gives them, its phrases
/// over the earlier's, and both's words.
let private merge (earlier: Pack) (later: Pack) =
    { Tag = earlier.Tag
      Name = (if later.Name = later.Tag then earlier.Name else later.Name)
      Folders = earlier.Folders @ later.Folders
      Ocr = later.Ocr |> Option.orElse earlier.Ocr
      Phrases = Map.fold (fun phrases key text -> Map.add key text phrases) earlier.Phrases later.Phrases
      SentenceContinuations = earlier.SentenceContinuations @ later.SentenceContinuations |> List.distinct
      StandaloneHeadings = earlier.StandaloneHeadings @ later.StandaloneHeadings |> List.distinct }

/// The packs in roots' folders (each a folder of packs), one per language however many folders
/// have it, by tag; and what's wrong with any of them.
let load (roots: string list) : Pack list * Problem list =
    let packs = List<Pack>()
    let problems = List<Problem>()
    for root in roots do
        // A folder of packs that can't be listed is said, and the others read.
        let folders =
            try
                if Directory.Exists root then Directory.GetDirectories root |> Array.sort else [||]
            with ex ->
                problems.Add { Folder = root; Message = $"its packs can't be listed: {ex.Message}" }
                [||]
        for folder in folders do
            let file, found = read folder
            problems.AddRange found
            match file with
            | Some file ->
                let pack, found = checkedPack folder file
                problems.AddRange found
                match packs |> Seq.tryFindIndex (fun p -> String.Equals(p.Tag, pack.Tag, StringComparison.OrdinalIgnoreCase)) with
                | Some i -> packs[i] <- merge packs[i] pack
                | None -> packs.Add pack
            | None -> ()
    List.ofSeq packs, List.ofSeq problems

/// The packs installed (in roots ()), read once.
let installed = lazy (load (roots ()))

/// The pack for a language tag, from those given: its own ("es-MX"), else its language's ("es").
let forTagIn (packs: Pack list) (tag: string) : Pack option =
    let find (wanted: string) = packs |> List.tryFind (fun pack -> String.Equals(pack.Tag, wanted, StringComparison.OrdinalIgnoreCase))
    match tag.Trim() with
    | "" -> None
    | tag -> find tag |> Option.orElse (find (tag.Split('-').[0]))

/// The installed pack for a language tag (see forTagIn).
let forTag (tag: string) = forTagIn (fst installed.Value) tag

/// A phrase Legibyl writes (see phrases), in the language tagged, with values for its {names}:
/// its text, and the tag of the language it's in (English, "en", where the language's pack
/// doesn't have it).
let phraseIn (packs: Pack list) (language: string option) (key: string) (values: (string * string) list) =
    let text, tag =
        match language |> Option.bind (forTagIn packs) |> Option.bind (fun pack -> pack.Phrases |> Map.tryFind key |> Option.map (fun text -> text, pack.Tag)) with
        | Some found -> found
        | None -> phrases[key], "en"
    // In one pass, so a value that reads like a {name} ("{address}" in a title) stays as it is.
    let given = dict values
    Regex.Replace(text, @"\{(\w+)\}", fun m -> match given.TryGetValue m.Groups[1].Value with | true, value -> value | _ -> m.Value), tag

/// A phrase, from the installed packs (see phraseIn).
let phrase (language: string option) (key: string) (values: (string * string) list) = phraseIn (fst installed.Value) language key values

/// The language Legibyl writes a document's own text in (see phrase): the one it says it's in,
/// else the one its text reads as, if that's clear.
let writtenIn (document: PdfDocument) = DocumentLanguage.ofDocument document

/// The language Legibyl writes a PDF's own text in (see writtenIn); None if it can't be read.
let writtenInFile (pdfPath: string) =
    try
        use document = new PdfDocument(new PdfReader(pdfPath))
        writtenIn document
    with _ -> None

/// The languages a person can choose for a document: those Legibyl can detect and those with a
/// pack, each its tag and name, by name.
let choices () =
    let packs = fst installed.Value |> List.map (fun pack -> pack.Tag, pack.Name)
    DocumentLanguage.known () @ packs |> List.distinctBy (fun (tag, _) -> tag.ToLowerInvariant()) |> List.sortBy snd

/// The language a PDF says it's in, and the one its pages' text reads as (if that's clear); both
/// None if it can't be read.
let ofFile (pdfPath: string) =
    try
        use document = new PdfDocument(new PdfReader(pdfPath))
        DocumentLanguage.declared document, DocumentLanguage.readsAs document
    with _ -> None, None
