module Legibyl.Validation.Tests.LanguagePacksTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open Legibyl.Validation

/// A folder of packs, deleted after: each written as its folder name and its language.yaml.
type private Root(packs: (string * string) list) =
    let path = Path.Combine(Path.GetTempPath(), $"legibyl-languages-{Guid.NewGuid():N}")
    do
        for folder, yaml in packs do
            Directory.CreateDirectory(Path.Combine(path, folder)) |> ignore
            File.WriteAllText(Path.Combine(path, folder, "language.yaml"), yaml)
    member _.Path = path
    member _.Folder(name: string) = Path.Combine(path, name)

    interface IDisposable with
        member _.Dispose() = Directory.Delete(path, true)

let private builtIn = Path.Combine(AppContext.BaseDirectory, "languages")

[<Fact>]
let ``English and Spanish are built in, with nothing wrong with them`` () =
    let packs, problems = LanguagePacks.load [ builtIn ]
    problems |> should be Empty
    packs |> List.map (fun pack -> pack.Tag, pack.Name) |> should equal [ "en", "English"; "es", "Spanish" ]
    let spanish = LanguagePacks.forTagIn packs "es" |> Option.get
    spanish.Phrases.Keys |> Set.ofSeq |> should equal (LanguagePacks.phrases.Keys |> Set.ofSeq)
    spanish.SentenceContinuations |> should contain "y"

[<Fact>]
let ``a pack dropped in is found by its tag and its language's, and its phrases are used`` () =
    use root =
        new Root(
            [ "fr",
              "tag: fr\nname: French\nphrases:\n  qrCode: \"Code QR : {text}\"\nsentenceContinuations: [et, ou]\n" ]
        )
    let packs, problems = LanguagePacks.load [ builtIn; root.Path ]
    problems |> should be Empty
    (LanguagePacks.forTagIn packs "fr-CA" |> Option.map _.Name) |> should equal (Some "French")
    LanguagePacks.phraseIn packs (Some "fr-CA") "qrCode" [ "text", "Bonjour" ] |> should equal ("Code QR : Bonjour", "fr")
    // A phrase the pack doesn't have is Legibyl's English, marked as English.
    LanguagePacks.phraseIn packs (Some "fr") "qrCodeLinkingToAddress" [ "address", "https://example.org" ]
    |> should equal ("QR code linking to https://example.org", "en")
    // As is any phrase in a language with no pack.
    LanguagePacks.phraseIn packs (Some "ja") "qrCode" [ "text", "x" ] |> should equal ("QR code: x", "en")
    LanguagePacks.phraseIn packs None "qrCode" [ "text", "x" ] |> should equal ("QR code: x", "en")

[<Fact>]
let ``a pack for a language already there adds to it: its words added, its phrases over the other's`` () =
    use root = new Root([ "es", "phrases:\n  qrCode: \"QR: {text}\"\nsentenceContinuations: [pero, y]\n" ])
    let packs, problems = LanguagePacks.load [ builtIn; root.Path ]
    problems |> should be Empty
    let spanish = LanguagePacks.forTagIn packs "es" |> Option.get
    spanish.Name |> should equal "Spanish"
    spanish.Folders |> should equal [ Path.Combine(builtIn, "es"); root.Folder "es" ]
    spanish.Phrases["qrCode"] |> should equal "QR: {text}"
    spanish.Phrases["qrCodeLinkingTo"] |> should equal "Código QR que enlaza a {title} ({address})"
    spanish.SentenceContinuations |> List.filter (fun word -> word = "pero" || word = "y") |> should equal [ "y"; "pero" ]

[<Fact>]
let ``what's wrong with a pack is said, and only that part of it left out`` () =
    use root =
        new Root(
            [ "broken", "tag: [unclosed\n"
              "de",
              "name: German\nocr: deu.traineddata\nphrases:\n  qrCode: \"QR-Code\"\n  qrCodeTo: \"QR-Code zu {address}\"\n  qrCodeLinkingToAddress: \"QR-Code, der zu {address} führt\"\n" ]
        )
    Directory.CreateDirectory(root.Folder "empty") |> ignore
    let packs, problems = LanguagePacks.load [ root.Path ]
    problems
    |> List.map (fun problem -> Path.GetFileName problem.Folder, problem.Message.Split(':').[0])
    |> List.sort
    |> should
        equal
        [ "broken", "its language.yaml can't be read"
          "de", "its phrase qrCode has to keep {text}, and only those"
          "de", "its phrase qrCodeTo isn't one Legibyl writes (qrCode, qrCodeLinkingTo, qrCodeLinkingToAddress)"
          "de", "ocr names deu.traineddata, which isn't in its folder"
          "empty", "it has no language.yaml" ]
    let german = LanguagePacks.forTagIn packs "de" |> Option.get
    (german.Name, german.Ocr, german.Phrases |> Map.toList)
    |> should equal ("German", (None: string option), [ "qrCodeLinkingToAddress", "QR-Code, der zu {address} führt" ])
