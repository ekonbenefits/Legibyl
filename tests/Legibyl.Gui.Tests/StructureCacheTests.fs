module Legibyl.Gui.Tests.StructureCacheTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open Legibyl.Gui
open Legibyl.Validation

let private tree () =
    { Tag = "StructTreeRoot"
      Role = null
      AlternateDescription = null
      ActualText = null
      Content = null
      FormFieldName = null
      FormFieldDescription = null
      FormFieldType = null
      FormFieldValue = null
      LinkUri = null
      LinkDestinationPageNumber = Nullable()
      LinkDestinationDescription = null
      Geometry = null
      Language = null
      PageNumber = Nullable()
      ContentLeafCount = 0
      Children = [||] }

/// A temporary file standing in for a PDF (the cache only looks at its path and timestamp).
let private withFile (test: string -> unit) =
    let path = Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid():N}.pdf")
    File.WriteAllText(path, "pdf")
    try test path finally File.Delete path

[<Fact>]
let ``reuses the structure for the same unchanged file`` () =
    withFile (fun path ->
        let cache = StructureCache()
        cache.TryFind path |> should equal None
        let structure = tree ()

        cache.Remember(cache.Stamp path, structure)

        match cache.TryFind path with
        | Some found -> obj.ReferenceEquals(found, structure) |> should be True
        | None -> failwith "The structure wasn't remembered.")

[<Fact>]
let ``reads again once the file changes`` () =
    withFile (fun path ->
        let cache = StructureCache()
        cache.Remember(cache.Stamp path, tree ())

        File.SetLastWriteTimeUtc(path, DateTime.UtcNow.AddMinutes 1.0)

        cache.TryFind path |> should equal None)

[<Fact>]
let ``doesn't reuse another file's structure`` () =
    withFile (fun first ->
        withFile (fun second ->
            let cache = StructureCache()
            cache.Remember(cache.Stamp first, tree ())

            cache.TryFind second |> should equal None))

[<Fact>]
let ``remembers that a file has no structure tree`` () =
    withFile (fun path ->
        let cache = StructureCache()

        cache.Remember(cache.Stamp path, null)

        match cache.TryFind path with
        | Some structure -> structure |> should be Null
        | None -> failwith "The untagged result wasn't remembered.")
