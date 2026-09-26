module Legibyl.Validation.Tests.CopyNamesTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open Legibyl.Validation

let private inFolder (existing: string list) (test: string -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    try
        for name in existing do
            File.WriteAllText(Path.Combine(folder, name), "")
        test folder
    finally
        Directory.Delete(folder, true)

[<Theory>]
[<InlineData("flyer.pdf", "flyer.v2.pdf")>]
[<InlineData("flyer.v2.pdf", "flyer.v3.pdf")>]
[<InlineData("flyer v9.pdf", "flyer v10.pdf")>]
[<InlineData("flyer-V2.pdf", "flyer-v3.pdf")>]
[<InlineData("flyer_v2.pdf", "flyer_v3.pdf")>]
[<InlineData("Report 2024.pdf", "Report 2024.v2.pdf")>]
[<InlineData("move.pdf", "move.v2.pdf")>]
let ``a copy is named as the next version`` (name: string, expected: string) =
    inFolder [] (fun folder -> CopyNames.nextVersion (Path.Combine(folder, name)) |> should equal expected)

[<Fact>]
let ``versions that already exist are skipped`` () =
    inFolder [ "flyer.v2.pdf"; "flyer.v3.pdf" ] (fun folder ->
        CopyNames.nextVersion (Path.Combine(folder, "flyer.pdf")) |> should equal "flyer.v4.pdf")
