/// The PDFs opened lately, newest first, for File › Open Recent: kept in "recent.json" in the
/// app's folder of local data.
module Legibyl.Gui.RecentFiles

open System
open System.IO
open System.Text.Json

/// How many are kept.
[<Literal>]
let Limit = 10

/// Where the list is kept; the headless harness points it elsewhere, so tests and screenshots
/// don't change a person's own list.
let mutable FilePath =
    Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData, "Legibyl", "recent.json")

let private changed = Event<unit>()

/// Raised when the list changes.
let Changed = changed.Publish

let private samePath (a: string) (b: string) =
    let comparison = if OperatingSystem.IsLinux() then StringComparison.Ordinal else StringComparison.OrdinalIgnoreCase
    String.Equals(Path.GetFullPath a, Path.GetFullPath b, comparison)

let private readAll (file: string) : string list =
    try
        if File.Exists file then
            JsonSerializer.Deserialize<string array>(File.ReadAllText file) |> Option.ofObj |> Option.map List.ofArray |> Option.defaultValue []
        else
            []
    with
    | :? IOException
    | :? UnauthorizedAccessException
    | :? JsonException -> []

let private write (file: string) (paths: string list) =
    try
        Directory.CreateDirectory(Path.GetDirectoryName file |> nonNull) |> ignore
        File.WriteAllText(file, JsonSerializer.Serialize(Array.ofList paths))
    with
    | :? IOException
    | :? UnauthorizedAccessException -> ()
    changed.Trigger()

/// The PDFs listed in file that are still there, newest first.
let readFrom (file: string) = readAll file |> List.filter File.Exists

/// Puts pdf at the top of the list in file.
let addTo (file: string) (pdf: string) =
    let full = Path.GetFullPath pdf
    write file (full :: (readAll file |> List.filter (not << samePath full)) |> List.truncate Limit)

/// The PDFs opened lately that are still there, newest first.
let read () = readFrom FilePath

/// Puts pdf at the top of the list.
let add (pdf: string) = addTo FilePath pdf

/// Empties the list.
let clear () = write FilePath []

/// A name for each, to tell them apart in a menu: the file's name, and its folder's where
/// another has the same name.
let labels (paths: string list) =
    let names = paths |> List.map (fun p -> string (Path.GetFileName p))
    [ for path, name in List.zip paths names ->
          if (names |> List.filter ((=) name) |> List.length) > 1 then
              $"{name} — {Path.GetFileName(Path.GetDirectoryName path |> nonNull)}"
          else
              name ]
