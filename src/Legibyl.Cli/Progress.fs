/// What the CLI says on stderr while it works; stdout stays the report alone. On a terminal, one
/// line rewritten as it goes ("[2/5] garden.pdf: Validating: page 37 of 200"); redirected, a line
/// per stage and a heartbeat every few seconds, so a long run doesn't look hung (or get killed by
/// an agent waiting for output); with --progress json, one JSON object a line, for an agent to
/// read; with --quiet, nothing. Every way but quiet ends each file with a line saying how it went.
module Legibyl.Cli.Progress

open System
open System.Diagnostics
open System.Text.Json

/// How progress is said.
type Mode =
    /// One line on a terminal, rewritten in place.
    | Line
    /// A line per stage, and a heartbeat while one runs long.
    | Plain
    /// A JSON object a line.
    | Json
    | Quiet

/// The mode --progress names ("auto" or none: a line on a terminal, plain lines redirected), or
/// --quiet's; an error for a name it isn't.
let choose (progress: string option) (quiet: bool) : Result<Mode, string> =
    match quiet, progress |> Option.map _.ToLowerInvariant() with
    | true, _ -> Ok Quiet
    | false, (None | Some "auto") -> Ok(if Console.IsErrorRedirected then Plain else Line)
    | false, Some "line" -> Ok Line
    | false, Some "plain" -> Ok Plain
    | false, Some "json" -> Ok Json
    | false, Some "none" -> Ok Quiet
    | false, Some other -> Error $"Unknown --progress '{other}'. Expected auto, line, plain, json or none."

/// A heartbeat this often while a stage runs on (plain lines).
let private heartbeat = TimeSpan.FromSeconds 5.0

/// A rewritten line this often at most.
let private refresh = TimeSpan.FromMilliseconds 100.0

let private jsonOptions =
    JsonSerializerOptions(
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        DefaultIgnoreCondition = Serialization.JsonIgnoreCondition.WhenWritingNull
    )

/// A line of --progress json: where a file's got (stage, at of total, in unit), or how it went (done).
type Event =
    { File: string
      Index: int
      Count: int
      Stage: string | null
      At: int
      Total: int
      Unit: string | null
      Done: string | null
      ElapsedMs: int64 }

/// Says how a run over files is going, on stderr. Reports can come from any thread.
type Reporter(mode: Mode, count: int) =
    let gate = obj ()
    let clock = Stopwatch.StartNew()
    let mutable index = 0
    let mutable file = ""
    let mutable lastStage = ""
    let mutable lastShown = TimeSpan.MinValue
    let mutable shownWidth = 0

    /// "[2/5] garden.pdf: ", or just "garden.pdf: " for one file.
    let prefix () =
        if count > 1 then $"[{index}/{count}] {IO.Path.GetFileName file}: " else $"{IO.Path.GetFileName file}: "

    let clearLine () =
        if shownWidth > 0 then
            eprintf "\r%s\r" (String(' ', shownWidth))
            shownWidth <- 0

    let json (stage: string | null) (at: int) (total: int) (unit: string | null) (outcome: string | null) =
        let event =
            { File = file
              Index = index
              Count = count
              Stage = stage
              At = at
              Total = total
              Unit = unit
              Done = outcome
              ElapsedMs = clock.ElapsedMilliseconds }
        eprintfn "%s" (JsonSerializer.Serialize(event, jsonOptions))

    member _.Mode = mode

    /// Starts on the next file.
    member _.Begin(path: string) =
        lock gate (fun () ->
            index <- index + 1
            file <- path
            lastStage <- ""
            lastShown <- TimeSpan.MinValue)

    /// Where it's got: a stage ("Validating"), and how far through it (page, pages; or MB of a
    /// download) where that's counted.
    member _.Status(stage: string, ?position: int * int, ?unit: string) =
        lock gate (fun () ->
            let now = clock.Elapsed
            let counted =
                match position with
                | Some(at, total) when total > 0 ->
                    match unit with
                    | Some unit -> $"{stage}: {at:N0} of {total:N0} {unit}"
                    | None -> $"{stage}: page {at:N0} of {total:N0}"
                | _ -> stage
            let changed = stage <> lastStage
            match mode with
            | Line when changed || now - lastShown >= refresh ->
                let text = prefix () + counted
                let width = max shownWidth text.Length
                eprintf "\r%s" (text.PadRight width)
                shownWidth <- width
                lastShown <- now
            | Plain when changed || now - lastShown >= heartbeat ->
                eprintfn "%s%s" (prefix ()) counted
                lastShown <- now
            | Json when changed || now - lastShown >= TimeSpan.FromSeconds 1.0 ->
                let at, total = defaultArg position (0, 0)
                let unit: string | null = match unit with Some unit -> unit | None when total > 0 -> "pages" | None -> null
                json stage at total unit null
                lastShown <- now
            | _ -> ()
            lastStage <- stage)

    /// A note while it runs ("Reading the titles of linked pages: up to two minutes").
    member this.Note(text: string) = this.Status text

    /// How the file went ("PASS", "FAIL 3", "fixed → flyer.v2.pdf"), ending its line.
    member _.Done(outcome: string) =
        lock gate (fun () ->
            match mode with
            | Line ->
                clearLine ()
                eprintfn "%s%s" (prefix ()) outcome
            | Plain -> eprintfn "%s%s" (prefix ()) outcome
            | Json -> json null 0 0 null outcome
            | Quiet -> ())

    /// Clears a rewritten line, before something else is said on stderr.
    member _.Clear() = lock gate clearLine
