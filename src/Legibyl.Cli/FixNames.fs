/// The repairs --fixes names, and the sets "safe", "all" and "none" (only edits or a layout).
module Legibyl.Cli.FixNames

open System
open Legibyl.Validation

/// Each repair's name on the command line, in the order they run.
let names = RepairNames.names

/// The repairs that need no guesswork about the document's meaning: what `fix` applies by default. The
/// title only moves across a title the author did write, in Info or in XMP; a title is never
/// invented from the file name.
let safe =
    PdfFixOptions.Metadata
    ||| PdfFixOptions.AnnotationTabs
    ||| PdfFixOptions.LinkContents
    ||| PdfFixOptions.DocumentTitle
    ||| PdfFixOptions.FontCidToGidMap

/// Every repair but the one that visits web pages, which has to be named.
let all =
    names |> List.fold (fun all (_, option) -> all ||| option) PdfFixOptions.None
    &&& ~~~PdfFixOptions.LinkTitlesFromWeb

/// The repairs a comma-separated list names ("safe", "all", "none", or repair names, in any case).
let parse (text: string) : Result<PdfFixOptions, string> =
    let parts = text.Split(',', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
    if parts.Length = 0 then
        Error "--fixes needs at least one repair name. Run 'legibyl list fixes' to see them."
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
                        | None -> Error $"Unknown repair '{part}'. Run 'legibyl list fixes' to see the names."))
            (Ok PdfFixOptions.None)

/// Whether the repairs need the layout model (tagging, and what reads the page's layout).
let usesLayout (options: PdfFixOptions) =
    options.HasFlag PdfFixOptions.TagFromLayout || options.HasFlag PdfFixOptions.JoinSplitParagraphs

/// One line per repair: its name, what it does, and whether it's in "safe".
let listing () =
    [ for name, option in names ->
          let described = PdfFixDescriptions.Describe option |> List.tryHead |> Option.defaultValue ""
          let tags =
              [ if safe.HasFlag option then "safe"
                if not (all.HasFlag option) then "not in all: it visits web pages"
                if usesLayout option then "downloads the layout model"
                if option = PdfFixOptions.QrCodeLinks then "with tag-from-layout" ]
          let joined = String.Join(", ", tags)
          let tags = if tags.IsEmpty then "" else $" ({joined})"
          $"{name,-22} {described}{tags}" ]
