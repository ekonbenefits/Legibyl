/// What PDF/UA-1's failures mean in plain language, and what to do about those no auto fix
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
    { Clause: string | null
      Clauses: string array | null
      /// Every test of the clause when none are listed.
      Tests: int array | null
      Meaning: string
      WhatToDo: string }

[<CLIMutable>]
type GuideFile = { Rules: Entry array }

/// The entries, in order: the first that matches a failure is its guide.
let entries: Lazy<Entry list> =
    lazy
        (let assembly = typeof<Guide>.Assembly
         use stream = assembly.GetManifestResourceStream "Legibyl.Validation.RuleGuide.yaml" |> nonNull
         use reader = new StreamReader(stream)
         let deserializer = DeserializerBuilder().WithNamingConvention(CamelCaseNamingConvention.Instance).Build()
         deserializer.Deserialize<GuideFile>(reader).Rules |> List.ofArray)

let private covers (clause: string) (test: int) (entry: Entry) =
    let clauses = [ yield! Option.toList (Option.ofObj entry.Clause); yield! Option.ofObj entry.Clauses |> Option.defaultValue [||] ]
    List.contains clause clauses
    && (match entry.Tests with
        | NonNull tests when tests.Length > 0 -> Array.contains test tests
        | _ -> true)

/// The guide for a PDF/UA-1 (ISO 14289-1) rule, by its clause and test number.
let forRule (clause: string) (test: int) : Guide option =
    entries.Value
    |> List.tryFind (covers clause test)
    |> Option.map (fun entry -> { Meaning = entry.Meaning; WhatToDo = entry.WhatToDo })

/// The guide for a finding, if it's a PDF/UA-1 failure this covers.
let forFinding (violation: RuleViolation) : Guide option =
    if violation.Specification.StartsWith("ISO 14289-1", StringComparison.Ordinal) then forRule violation.Clause violation.TestNumber
    else None
