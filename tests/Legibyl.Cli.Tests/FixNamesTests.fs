module Legibyl.Cli.Tests.FixNamesTests

open Xunit
open FsUnit.Xunit
open Legibyl.Cli
open Legibyl.Validation

[<Fact>]
let ``fixes are named in a list, with the safe set and every fix`` () =
    FixNames.parse "safe, Lists,headings-from-toc"
    |> should equal (Ok(FixNames.safe ||| PdfFixOptions.Lists ||| PdfFixOptions.HeadingsFromToc) : Result<PdfFixOptions, string>)
    FixNames.parse "all" |> should equal (Ok FixNames.all : Result<PdfFixOptions, string>)

[<Fact>]
let ``every fix leaves out only the one that visits web pages`` () =
    FixNames.all.HasFlag PdfFixOptions.LinkTitlesFromWeb |> should be False
    for _, option in FixNames.names do
        if option <> PdfFixOptions.LinkTitlesFromWeb then
            FixNames.all.HasFlag option |> should be True

[<Fact>]
let ``an unknown or missing name says so`` () =
    FixNames.parse "safe,nope" |> should equal (Error "Unknown fix 'nope'. Run 'legibyl list fixes' to see the names." : Result<PdfFixOptions, string>)
    FixNames.parse " , " |> Result.isError |> should be True

[<Fact>]
let ``each fix is listed with what it does`` () =
    let listing = FixNames.listing ()
    listing |> should haveLength FixNames.names.Length
    listing |> List.find (fun line -> line.StartsWith "lists ") |> should haveSubstring "Tag paragraphs that start with list markers"
    listing |> List.find (fun line -> line.StartsWith "metadata ") |> should haveSubstring "(safe)"

[<Fact>]
let ``none names no fixes, for edits or a layout alone`` () =
    FixNames.parse "none" |> should equal (Ok PdfFixOptions.None : Result<PdfFixOptions, string>)
    FixNames.parse "none,lists" |> should equal (Ok PdfFixOptions.Lists : Result<PdfFixOptions, string>)

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
let ``the README's table of fixes is every fix, as list fixes gives it`` () =
    let text = readme ()
    let table = text.Substring(text.IndexOf "<!-- fixes:start -->", text.IndexOf "<!-- fixes:end -->" - text.IndexOf "<!-- fixes:start -->")
    let rows =
        [ for line in table.Split('\n') do
              let line = line.Trim()
              if line.StartsWith "| `" then
                  match line.Trim('|').Split('|') |> Array.map _.Trim() with
                  | [| name; does; notes |] -> yield name.Trim('`'), does, notes
                  | cells -> failwith $"A row with {cells.Length} cells: {line}" ]

    rows |> should equal (FixNames.fixes ())

[<Fact>]
let ``every fix's name is its flag's, in kebab-case, and reads back as it`` () =
    let flags =
        System.Enum.GetValues<PdfFixOptions>()
        |> Array.filter (fun f -> f <> PdfFixOptions.None && f <> PdfFixOptions.All && f <> PdfFixOptions.ApplyEdits)
        |> Set.ofArray
    // Each fix is named, once.
    FixNames.order |> Set.ofList |> should equal flags
    FixNames.order |> List.distinct |> List.length |> should equal FixNames.order.Length
    for name, option in FixNames.names do
        name |> should equal (FixNames.nameOf option)
        name |> should equal (System.Text.RegularExpressions.Regex.Replace(string option, "([a-z])([A-Z])", "$1-$2").ToLowerInvariant())
        FixNames.toOptions [ name ] |> should equal option
    FixNames.nameOf PdfFixOptions.FontGlyphs |> should equal "font-glyphs"
    FixNames.nameOf PdfFixOptions.LinkTitlesFromWeb |> should equal "link-titles-from-web"
    // All is every flag.
    PdfFixOptions.All |> should equal (flags |> Set.fold (|||) PdfFixOptions.ApplyEdits)

[<Fact>]
let ``what people read says fix, not repair, except veraPDF's own names`` () =
    let readmePath =
        let rec find (folder: System.IO.DirectoryInfo | null) =
            match folder with
            | null -> failwith "README.md not found above the test output"
            | folder when System.IO.Directory.Exists(System.IO.Path.Combine(folder.FullName, "src")) -> folder.FullName
            | folder -> find folder.Parent
        find (System.IO.DirectoryInfo System.AppContext.BaseDirectory)
    // veraPDF's metadata fixer reports a "repair status" (MetadataFixerResult.RepairStatus).
    let allowed (text: string) = System.Text.RegularExpressions.Regex.Replace(text, "(?i)repair ?status", "")
    let saysRepair (text: string) = (allowed text).Contains("repair", System.StringComparison.OrdinalIgnoreCase)
    let inReadme = [ for line in readme().Split('\n') do if saysRepair line then yield $"README.md: {line.Trim()}" ]
    // Every string in the sources: CLI help and errors, the GUI's text, the library's messages.
    let inStrings =
        [ for project in [ "Legibyl.Cli"; "Legibyl.Gui"; "Legibyl.Validation"; "Legibyl.Layout" ] do
              for file in System.IO.Directory.GetFiles(System.IO.Path.Combine(readmePath, "src", project), "*.fs") do
                  for m in System.Text.RegularExpressions.Regex.Matches(System.IO.File.ReadAllText file, "\"(?:[^\"\\\\\\n]|\\\\.)*\"") do
                      if saysRepair m.Value then yield $"{System.IO.Path.GetFileName file}: {m.Value}" ]

    inReadme @ inStrings |> should be Empty
