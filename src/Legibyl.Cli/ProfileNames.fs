/// Maps user-supplied profile strings (case-insensitive, with common aliases) to PdfProfile.
module Legibyl.Cli.ProfileNames

open System
open System.Collections.Generic
open System.Reflection
open Legibyl.Validation

let private aliases =
    let map = Dictionary<string, PdfProfile>(StringComparer.OrdinalIgnoreCase)
    for names, profile in
        [ [ "pdfua1"; "pdf/ua-1"; "ua1"; "ua" ], PdfProfile.PdfUa1
          [ "pdfa1b"; "pdf/a-1b"; "1b" ], PdfProfile.PdfA1B
          [ "pdfa2b"; "pdf/a-2b"; "2b" ], PdfProfile.PdfA2B
          [ "pdfa2u"; "pdf/a-2u"; "2u" ], PdfProfile.PdfA2U
          [ "pdfa3b"; "pdf/a-3b"; "3b" ], PdfProfile.PdfA3B
          [ "pdfa4"; "pdf/a-4"; "4" ], PdfProfile.PdfA4 ] do
        for name in names do
            map[name] <- profile
    map

/// The default profile used when none is specified.
[<Literal>]
let DefaultName = "pdfua1"

/// Version of the veraPDF engine, stamped from VeraPdfVersion in Directory.Build.props.
let VendoredEngineVersion =
    Assembly.GetExecutingAssembly().GetCustomAttributes<AssemblyMetadataAttribute>()
    |> Seq.tryFind (fun a -> a.Key = "VeraPdfVersion")
    |> Option.bind (fun a -> Option.ofObj a.Value)
    |> Option.defaultValue "unknown"

let tryParse (name: string) : PdfProfile option =
    match aliases.TryGetValue(name.Trim()) with
    | true, profile -> Some profile
    | _ -> None

/// All recognised profile names, for --list-profiles / help output.
let CanonicalNames = [ "pdfua1"; "pdfa1b"; "pdfa2b"; "pdfa2u"; "pdfa3b"; "pdfa4" ]
