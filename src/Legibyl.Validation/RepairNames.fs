/// Each repair's name, as the CLI's --fixes and a review's "fixes" have it.
module Legibyl.Validation.RepairNames

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

/// The repairs that need no guesswork about the document's meaning: what the CLI's `fix` applies
/// by default, and what the GUI's Auto Fixes start with ticked. The title only moves across a
/// title the author did write, in Info or in XMP; a title is never invented from the file name.
let safe =
    PdfFixOptions.ParentTree
    ||| PdfFixOptions.Metadata
    ||| PdfFixOptions.Tabs
    ||| PdfFixOptions.LinkContents
    ||| PdfFixOptions.Title
    ||| PdfFixOptions.FontGlyphs

/// The names of the repairs in options, in the order they run.
let ofOptions (options: PdfFixOptions) =
    [ for name, option in names do if options.HasFlag option then name ]

/// The repairs named (any case); names it doesn't know are left out.
let toOptions (named: string seq) =
    named
    |> Seq.choose (fun name -> names |> List.tryFind (fst >> (=) (name.ToLowerInvariant())) |> Option.map snd)
    |> Seq.fold (|||) PdfFixOptions.None
