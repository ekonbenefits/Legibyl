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
    FixNames.parse "safe,nope" |> should equal (Error "Unknown repair 'nope'. Run 'legibyl list fixes' to see the names." : Result<PdfFixOptions, string>)
    FixNames.parse " , " |> Result.isError |> should be True

[<Fact>]
let ``each repair is listed with what it does`` () =
    let listing = FixNames.listing ()
    listing |> should haveLength FixNames.names.Length
    listing |> List.find (fun line -> line.StartsWith "lists ") |> should haveSubstring "Tag paragraphs that start with list markers"
    listing |> List.find (fun line -> line.StartsWith "metadata ") |> should haveSubstring "(safe)"

[<Fact>]
let ``none names no repairs, for edits or a layout alone`` () =
    FixNames.parse "none" |> should equal (Ok PdfFixOptions.None : Result<PdfFixOptions, string>)
    FixNames.parse "none,lists" |> should equal (Ok PdfFixOptions.ListsFromParagraphs : Result<PdfFixOptions, string>)

/// The repository's README.md, found above the tests' output folder.
let private readme () =
    let rec find (folder: System.IO.DirectoryInfo | null) =
        match folder with
        | null -> failwith "README.md not found above the test output"
        | folder ->
            let path = System.IO.Path.Combine(folder.FullName, "README.md")
            if System.IO.File.Exists path && System.IO.Directory.Exists(System.IO.Path.Combine(folder.FullName, "src")) then path
            else find folder.Parent
    System.IO.File.ReadAllText(find (System.IO.DirectoryInfo System.AppContext.BaseDirectory))

[<Fact>]
let ``the README's table of repairs is every repair, as list fixes gives it`` () =
    let text = readme ()
    let table = text.Substring(text.IndexOf "<!-- repairs:start -->", text.IndexOf "<!-- repairs:end -->" - text.IndexOf "<!-- repairs:start -->")
    let rows =
        [ for line in table.Split('\n') do
              let line = line.Trim()
              if line.StartsWith "| `" then
                  match line.Trim('|').Split('|') |> Array.map _.Trim() with
                  | [| name; does; notes |] -> yield name.Trim('`'), does, notes
                  | cells -> failwith $"A row with {cells.Length} cells: {line}" ]

    rows |> should equal (FixNames.repairs ())
