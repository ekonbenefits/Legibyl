module Legibyl.Validation.Tests.RuleGuideTests

open System
open Xunit
open FsUnit.Xunit
open Legibyl.Validation

/// The rules of profile (the bundled veraPDF's) that RuleGuide.yaml has no entry for.
let private withoutGuide (profile: PdfProfile) =
    [ for specification, clause, test, text in VeraPdfValidator.Rules profile do
          if (RuleGuide.forRule specification clause test).IsNone then
              yield $"{specification} {clause}-{test}: {text}" ]

[<Fact>]
let ``every PDF/UA-1 rule has a guide: what it means, and what to do`` () =
    VeraPdfValidator.Rules PdfProfile.PdfUa1 |> should not' (be Empty)
    withoutGuide PdfProfile.PdfUa1 |> should be Empty

[<Fact>]
let ``every PDF/UA-2 rule has a guide, ISO 32005's tag nesting ones included`` () =
    let rules = VeraPdfValidator.Rules PdfProfile.PdfUa2
    rules |> List.exists (fun (specification, _, _, _) -> specification.StartsWith "ISO 32005") |> should be True
    withoutGuide PdfProfile.PdfUa2 |> should be Empty

[<Fact>]
let ``no guide says nothing`` () =
    for _, entries in RuleGuide.entries.Value do
        for entry in entries do
            String.IsNullOrWhiteSpace entry.Meaning |> should be False
            String.IsNullOrWhiteSpace entry.WhatToDo |> should be False
