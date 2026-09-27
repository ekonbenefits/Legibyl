/// What PDF/UA's failures mean in plain language, and what to do about those no auto fix
/// repairs, read from RuleGuide.yaml (embedded in this assembly), where the text is kept to read
/// and edit on its own.
module Legibyl.Validation.RuleGuide

open System
open System.IO
open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions

/// A failure explained: what it means for a reader, and, where no auto fix repairs it, what to
/// do instead.
type Guide = { Meaning: string; WhatToDo: string }

/// An entry of RuleGuide.yaml, as written there.
[<CLIMutable>]
type Entry =
    { /// Every rule of its standard when neither Clause nor Clauses is given.
      Clause: string | null
      Clauses: string array | null
      /// Every test of the clause when none are listed.
      Tests: int array | null
      Meaning: string
      WhatToDo: string }

[<CLIMutable>]
type GuideFile =
    { /// PDF/UA-1's (ISO 14289-1).
      Rules: Entry array | null
      /// PDF/UA-2's (ISO 14289-2).
      PdfUa2Rules: Entry array | null
      /// ISO 32005's, on which tags may contain which, which PDF/UA-2 includes.
      TagNestingRules: Entry array | null }

/// Each standard's entries, by the start of its specification ("ISO 14289-1"), in order: the
/// first that matches a failure is its guide.
let entries: Lazy<(string * Entry list) list> =
    lazy
        (let assembly = typeof<Guide>.Assembly
         use stream = assembly.GetManifestResourceStream "Legibyl.Validation.RuleGuide.yaml" |> nonNull
         use reader = new StreamReader(stream)
         let deserializer = DeserializerBuilder().WithNamingConvention(CamelCaseNamingConvention.Instance).Build()
         let file = deserializer.Deserialize<GuideFile>(reader)
         [ "ISO 14289-1", file.Rules; "ISO 14289-2", file.PdfUa2Rules; "ISO 32005", file.TagNestingRules ]
         |> List.map (fun (specification, rules) -> specification, List.ofArray (Option.ofObj rules |> Option.defaultValue [||])))

let private covers (clause: string) (test: int) (entry: Entry) =
    let clauses = [ yield! Option.toList (Option.ofObj entry.Clause); yield! Option.ofObj entry.Clauses |> Option.defaultValue [||] ]
    (clauses.IsEmpty || List.contains clause clauses)
    && (match entry.Tests with
        | NonNull tests when tests.Length > 0 -> Array.contains test tests
        | _ -> true)

/// The guide for a rule, by its specification ("ISO 14289-2:2024"), clause and test number.
let forRule (specification: string) (clause: string) (test: int) : Guide option =
    entries.Value
    |> List.tryFind (fun (standard, _) -> specification.StartsWith(standard, StringComparison.Ordinal))
    |> Option.bind (fun (_, rules) -> rules |> List.tryFind (covers clause test))
    |> Option.map (fun entry -> { Meaning = entry.Meaning; WhatToDo = entry.WhatToDo })

/// The guide for a finding, if it's a PDF/UA failure this covers.
let forFinding (violation: RuleViolation) : Guide option =
    forRule violation.Specification violation.Clause violation.TestNumber
