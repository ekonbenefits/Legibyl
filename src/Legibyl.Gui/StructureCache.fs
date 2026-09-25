namespace Legibyl.Gui

open System
open System.IO
open Legibyl.Validation

/// The last structure tree read, so reopening View Structure on the same, unchanged file
/// doesn't read it all again (that takes about 18 s on a 675-page document). Remembers an
/// untagged file's "no structure tree" (null) too.
[<Sealed>]
type StructureCache() =
    let mutable last: (string * DateTime * (StructureNode | null)) option = None

    /// Identifies the file as it is now. Take it before reading, so a file that changes
    /// while it's being read gets read again next time.
    member _.Stamp(pdfPath: string) =
        let fullPath = Path.GetFullPath pdfPath
        fullPath, File.GetLastWriteTimeUtc fullPath

    /// The structure read earlier for this file, if the file hasn't changed since.
    member this.TryFind(pdfPath: string) : (StructureNode | null) option =
        let fullPath, modified = this.Stamp pdfPath
        match last with
        | Some(path, stamp, structure) when path = fullPath && stamp = modified -> Some structure
        | _ -> None

    member _.Remember((fullPath: string, modified: DateTime), structure: StructureNode | null) =
        last <- Some(fullPath, modified, structure)
