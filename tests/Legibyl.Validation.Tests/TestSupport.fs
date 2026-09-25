module Legibyl.Validation.Tests.TestSupport

open System
open System.IO
open FsUnit.Xunit

let corpusFile (name: string) =
    Path.Combine(AppContext.BaseDirectory, "SampleCorpus", name)

/// A uniquely named path in the temp directory, deleted on dispose if anything was written there.
type TempPdf(?suffix: string) =
    let path =
        Path.Combine(Path.GetTempPath(), $"""{Guid.NewGuid():N}{defaultArg suffix ""}.pdf""")

    member _.Path = path

    interface IDisposable with
        member _.Dispose() =
            if File.Exists path then
                File.Delete path

/// Asserts the value isn't null and returns it as a non-nullable value.
let notNull (value: 'T | null) : 'T =
    value |> should not' (be Null)
    nonNull value

/// Asserts exactly one item matches the predicate and returns it.
let single (predicate: 'T -> bool) (items: 'T seq) : 'T =
    let matches = items |> Seq.filter predicate |> List.ofSeq
    matches |> should haveLength 1
    matches.Head
