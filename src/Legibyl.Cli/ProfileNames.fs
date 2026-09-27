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
          [ "pdfua2"; "pdf/ua-2"; "ua2" ], PdfProfile.PdfUa2 ] do
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
let CanonicalNames = [ "pdfua1"; "pdfua2" ]
