module Legibyl.Gui.Tests.BusyStatusTests

open System
open Xunit
open FsUnit.Xunit
open Legibyl.Gui.BusyStatus
open Legibyl.Validation

let private validation stage page pageCount =
    { Stage = stage
      Page = page
      PageCount = pageCount
      Checks = 0
      FailedRules = 0
      ProcessedObjects = 0
      QueuedObjects = 0
      Elapsed = TimeSpan.Zero }

let private fixing stage step steps page pageCount =
    Fixing { Stage = stage; Step = step; Steps = steps; Page = page; PageCount = pageCount }

[<Fact>]
let ``every progress line says what's happening first`` () =
    [ Validating("Validating", validation ValidationStage.Preparing 0 0)
      Validating("Validating", validation ValidationStage.CheckingPages 57 675)
      Validating("Validating the fixed copy", validation ValidationStage.Finishing 10 10)
      ReadingStructure
          { Stage = StructureReadStage.ReadingText
            Page = 3
            PageCount = 10
            Elements = 0
            Elapsed = TimeSpan.Zero }
      DownloadingLayoutModel 0.45
      fixing FixStage.CheckingOriginal 1 4 12 40
      fixing FixStage.Tagging 2 4 3 10
      fixing FixStage.Fixing 3 4 0 0
      fixing FixStage.Fixing 3 4 2 10
      fixing FixStage.FixingMetadata 4 4 0 0
      Working "Opening garden.pdf…" ]
    |> List.map statusText
    |> should
        equal
        [ "Validating: reading the PDF…"
          "Validating: page 57 of 675"
          "Validating the fixed copy: finishing up…"
          "Reading the structure: page text 3 of 10"
          "Downloading the layout model: 45%"
          "Fixing, step 1 of 4: checking the original, page 12 of 40"
          "Fixing, step 2 of 4: finding page 3 of 10's layout, to tag it"
          "Fixing, step 3 of 4: applying the fixes…"
          "Fixing, step 3 of 4: finding page 2 of 10's layout, to join split paragraphs"
          "Fixing, step 4 of 4: fixing metadata with veraPDF…"
          "Opening garden.pdf…" ]

[<Fact>]
let ``a screen reader hears the activity and a tenth of the way at a time, not every page`` () =
    [ Validating("Validating", validation ValidationStage.CheckingPages 57 675)
      Validating("Validating", validation ValidationStage.CheckingPages 58 675)
      Validating("Validating", validation ValidationStage.CheckingPages 400 675)
      Validating("Validating", validation ValidationStage.Finishing 675 675)
      DownloadingLayoutModel 0.37
      fixing FixStage.Tagging 2 4 3 10
      fixing FixStage.FixingMetadata 4 4 0 0
      Working "Opening garden.pdf…" ]
    |> List.map announcement
    |> should
        equal
        [ "Validating: 0%"
          "Validating: 0%"
          "Validating: 50%"
          "Validating: finishing up…"
          "Downloading the layout model: 30%"
          "Fixing, step 2 of 4: 20%"
          "Fixing, step 4 of 4"
          "Opening garden.pdf…" ]
