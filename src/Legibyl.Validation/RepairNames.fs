/// Each repair's name, as the CLI's --fixes and a review's "fixes" have it.
module Legibyl.Validation.RepairNames

/// Each repair's name, in the order they run.
let names =
    [ "tag-from-layout", PdfFixOptions.TagFromLayout
      "qr-links", PdfFixOptions.QrCodeLinks
      "language", PdfFixOptions.LanguageFromText
      "headings-from-toc", PdfFixOptions.HeadingsFromToc
      "empty-paragraphs", PdfFixOptions.RemoveEmptyParagraphs
      "empty-toc-entries", PdfFixOptions.RemoveEmptyTocEntries
      "join-split-paragraphs", PdfFixOptions.JoinSplitParagraphs
      "lists", PdfFixOptions.ListsFromParagraphs
      "bookmarks", PdfFixOptions.BookmarksFromHeadings
      "link-targets", PdfFixOptions.LinkTargets
      "tabs", PdfFixOptions.AnnotationTabs
      "font-glyphs", PdfFixOptions.FontCidToGidMap
      "link-contents", PdfFixOptions.LinkContents
      "link-titles-from-web", PdfFixOptions.LinkTitlesFromWeb
      "title", PdfFixOptions.DocumentTitle
      "title-from-first-page", PdfFixOptions.TitleFromFirstPage
      "metadata", PdfFixOptions.Metadata ]

/// The repairs that need no guesswork about the document's meaning: what the CLI's `fix` applies
/// by default, and what the GUI's Auto Fixes start with ticked. The title only moves across a
/// title the author did write, in Info or in XMP; a title is never invented from the file name.
let safe =
    PdfFixOptions.Metadata
    ||| PdfFixOptions.AnnotationTabs
    ||| PdfFixOptions.LinkContents
    ||| PdfFixOptions.DocumentTitle
    ||| PdfFixOptions.FontCidToGidMap

/// The names of the repairs in options, in the order they run.
let ofOptions (options: PdfFixOptions) =
    [ for name, option in names do if options.HasFlag option then name ]

/// The repairs named (any case); names it doesn't know are left out.
let toOptions (named: string seq) =
    named
    |> Seq.choose (fun name -> names |> List.tryFind (fst >> (=) (name.ToLowerInvariant())) |> Option.map snd)
    |> Seq.fold (|||) PdfFixOptions.None
