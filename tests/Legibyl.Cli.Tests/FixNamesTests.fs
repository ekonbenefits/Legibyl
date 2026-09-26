module Legibyl.Cli.Tests.FixNamesTests

open Xunit
open FsUnit.Xunit
open Legibyl.Cli
open Legibyl.Validation

[<Fact>]
let ``repairs are named in a list, with the safe set and every repair`` () =
    FixNames.parse "safe, Lists,headings-from-toc"
    |> should equal (Ok(FixNames.safe ||| PdfFixOptions.ListsFromParagraphs ||| PdfFixOptions.HeadingsFromToc) : Result<PdfFixOptions, string>)
    FixNames.parse "all" |> should equal (Ok FixNames.all : Result<PdfFixOptions, string>)

[<Fact>]
let ``every repair leaves out only the one that visits web pages`` () =
    FixNames.all.HasFlag PdfFixOptions.LinkTitlesFromWeb |> should be False
    for _, option in FixNames.names do
        if option <> PdfFixOptions.LinkTitlesFromWeb then
            FixNames.all.HasFlag option |> should be True

[<Fact>]
let ``an unknown or missing name says so`` () =
    FixNames.parse "safe,nope" |> should equal (Error "Unknown repair 'nope'. Run --list-fixes to see the names." : Result<PdfFixOptions, string>)
    FixNames.parse " , " |> Result.isError |> should be True

[<Fact>]
let ``each repair is listed with what it does`` () =
    let listing = FixNames.listing ()
    listing |> should haveLength FixNames.names.Length
    listing |> List.find (fun line -> line.StartsWith "lists ") |> should haveSubstring "Tag paragraphs that start with list markers"
    listing |> List.find (fun line -> line.StartsWith "metadata ") |> should haveSubstring "(safe)"
