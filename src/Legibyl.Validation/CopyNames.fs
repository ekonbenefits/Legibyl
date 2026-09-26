/// The names suggested for copies Legibyl saves: the next version of the PDF's name.
module Legibyl.Validation.CopyNames

open System
open System.IO
open System.Text.RegularExpressions

/// A version at the end of a name: "flyer.v2", "flyer-v2", "flyer_v2", "flyer v2". Only ASCII
/// digits, few enough to be an int; a longer run of digits is part of the name.
let private versioned = Regex(@"^(?<name>.*?)(?<separator>[.\-_ ])[vV](?<version>[0-9]{1,9})$", RegexOptions.Compiled)

/// The file name for the next version of pdfPath: "flyer.pdf" -> "flyer.v2.pdf" (the original
/// counts as version 1), "flyer v2.pdf" -> "flyer v3.pdf", skipping versions whose file already
/// exists next to it.
let nextVersion (pdfPath: string) =
    let folder = Path.GetDirectoryName pdfPath |> Option.ofObj |> Option.defaultValue ""
    let extension = string (Path.GetExtension pdfPath)
    let name = string (Path.GetFileNameWithoutExtension pdfPath)
    let baseName, separator, version =
        match versioned.Match name with
        | found when found.Success -> found.Groups["name"].Value, found.Groups["separator"].Value, int found.Groups["version"].Value
        | _ -> name, ".", 1
    Seq.initInfinite (fun i -> $"{baseName}{separator}v{version + 1 + i}{extension}")
    |> Seq.find (fun candidate -> not (File.Exists(Path.Combine(folder, candidate))))
