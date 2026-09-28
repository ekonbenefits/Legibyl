/// Each fix's name, as the CLI's --fixes and a review's "fixes" have it.
module Legibyl.Validation.FixNames

open System

/// The fixes in the order they run (ApplyEdits isn't one to name: edits apply with --edits).
let order =
    [ PdfFixOptions.TagFromLayout
      PdfFixOptions.QrLinks
      PdfFixOptions.ParentTree
      PdfFixOptions.Language
      PdfFixOptions.HeadingsFromToc
      PdfFixOptions.EmptyParagraphs
      PdfFixOptions.EmptyTocEntries
      PdfFixOptions.EmptyFigures
      PdfFixOptions.JoinSplitParagraphs
      PdfFixOptions.Lists
      PdfFixOptions.Bookmarks
      PdfFixOptions.LinkTargets
      PdfFixOptions.Tabs
      PdfFixOptions.FontGlyphs
      PdfFixOptions.LinkContents
      PdfFixOptions.LinkTitlesFromWeb
      PdfFixOptions.Title
      PdfFixOptions.TitleFromFirstPage
      PdfFixOptions.Metadata ]

/// A fix's name as the CLI and a review have it: its flag's name in kebab-case (FontGlyphs is
/// "font-glyphs").
let nameOf (option: PdfFixOptions) =
    System.Text.RegularExpressions.Regex.Replace(string option, "(?<=[a-z])(?=[A-Z])", "-").ToLowerInvariant()

/// Each fix's name, in the order they run.
let names = [ for option in order -> nameOf option, option ]

/// The fixes that need no guesswork about the document's meaning: what the CLI's `fix` applies
/// by default, and what the GUI's Auto Fixes start with ticked. The title only moves across a
/// title the author did write, in Info or in XMP; a title is never invented from the file name.
let safe =
    PdfFixOptions.ParentTree
    ||| PdfFixOptions.Metadata
    ||| PdfFixOptions.Tabs
    ||| PdfFixOptions.LinkContents
    ||| PdfFixOptions.Title
    ||| PdfFixOptions.FontGlyphs

/// The names of the fixes in options, in the order they run.
let ofOptions (options: PdfFixOptions) =
    [ for name, option in names do if options.HasFlag option then name ]

/// The fixes named (any case); names it doesn't know are left out.
let toOptions (named: string seq) =
    named
    |> Seq.choose (fun name -> names |> List.tryFind (fst >> (=) (name.ToLowerInvariant())) |> Option.map snd)
    |> Seq.fold (|||) PdfFixOptions.None

/// Every fix but the one that visits web pages, which has to be named.
let all =
    names |> List.fold (fun all (_, option) -> all ||| option) PdfFixOptions.None
    &&& ~~~PdfFixOptions.LinkTitlesFromWeb

/// The fixes a comma-separated list names ("safe", "all", "none", or fix names, in any case).
let parse (text: string) : Result<PdfFixOptions, string> =
    let parts = text.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    if parts.Length = 0 then
        Error "--fixes needs at least one fix name. Run 'legibyl list fixes' to see them."
    else
        parts
        |> Array.fold
            (fun result (part: string) ->
                result
                |> Result.bind (fun options ->
                    match part.ToLowerInvariant() with
                    | "safe" -> Ok(options ||| safe)
                    | "all" -> Ok(options ||| all)
                    | "none" -> Ok options
                    | name ->
                        match names |> List.tryFind (fst >> (=) name) with
                        | Some(_, option) -> Ok(options ||| option)
                        | None -> Error $"Unknown fix '{part}'. Run 'legibyl list fixes' to see the names."))
            (Ok PdfFixOptions.None)

/// Whether the fixes need the layout model (tagging, and what reads the page's layout).
let usesLayout (options: PdfFixOptions) =
    options.HasFlag PdfFixOptions.TagFromLayout || options.HasFlag PdfFixOptions.JoinSplitParagraphs

/// Each fix, in the order they run: its name, what it does, and notes on it (whether it's in
/// "safe", needs the layout model, visits web pages). The README's table of fixes is this.
let fixes () =
    [ for name, option in names ->
          let described = PdfFixDescriptions.Describe option |> List.tryHead |> Option.defaultValue ""
          let tags =
              [ if safe.HasFlag option then "safe"
                if not (all.HasFlag option) then "not in all: it visits web pages"
                if usesLayout option then "downloads the layout model"
                if option = PdfFixOptions.QrLinks then "with tag-from-layout" ]
          name, described, String.Join(", ", tags) ]

/// One line per fix: its name, what it does, and notes on it (see fixes).
let listing () =
    [ for name, described, notes in fixes () ->
          let notes = if notes = "" then "" else $" ({notes})"
          $"{name,-22} {described}{notes}" ]
