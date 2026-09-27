module Legibyl.Gui.Tests.RecentFilesTests

open System
open System.IO
open Xunit
open FsUnit.Xunit
open Legibyl.Gui

/// Files named names in a fresh folder, and a recent list there (not the app's, which the
/// window tests use).
let private withFiles (names: string list) (test: string -> string list -> unit) =
    let folder = Path.Combine(Path.GetTempPath(), $"legibyl-{Guid.NewGuid():N}")
    Directory.CreateDirectory folder |> ignore
    let recent = Path.Combine(folder, "recent.json")
    try
        let paths =
            [ for name in names ->
                  let path = Path.Combine(folder, name)
                  Directory.CreateDirectory(Path.GetDirectoryName path |> nonNull) |> ignore
                  File.WriteAllText(path, "")
                  path ]
        test recent paths
    finally
        Directory.Delete(folder, true)

[<Fact>]
let ``the newest is first, each once, and only the last ten are kept`` () =
    withFiles [ for i in 1..12 -> $"{i}.pdf" ] (fun recent paths ->
        for path in paths do
            RecentFiles.addTo recent path
        RecentFiles.addTo recent paths[5]

        let listed = RecentFiles.readFrom recent
        listed |> should haveLength RecentFiles.Limit
        listed.Head |> should equal (Path.GetFullPath paths[5])
        listed |> List.distinct |> should haveLength RecentFiles.Limit)

[<Fact>]
let ``files no longer there are left out`` () =
    withFiles [ "a.pdf"; "b.pdf" ] (fun recent paths ->
        for path in paths do
            RecentFiles.addTo recent path
        File.Delete paths[0]

        RecentFiles.readFrom recent |> should equal [ Path.GetFullPath paths[1] ])

[<Fact>]
let ``files with the same name are told apart by their folders`` () =
    withFiles [ "spring/flyer.pdf"; "fall/flyer.pdf"; "menu.pdf" ] (fun _ paths ->
        RecentFiles.labels paths |> should equal [ "flyer.pdf — spring"; "flyer.pdf — fall"; "menu.pdf" ])

[<Fact>]
let ``the list changing tells whoever's listening`` () =
    withFiles [ "a.pdf" ] (fun recent paths ->
        let mutable told = 0
        use _ = RecentFiles.Changed.Subscribe(fun () -> told <- told + 1)
        RecentFiles.addTo recent paths.Head
        told |> should equal 1)
