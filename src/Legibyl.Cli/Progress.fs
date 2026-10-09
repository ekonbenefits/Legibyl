/// What the CLI says on stderr while it works; stdout stays the report alone. On a terminal, one
/// line rewritten as it goes ("[2/5] garden.pdf: Validating: page 37 of 200"); redirected, a line
/// per stage and a heartbeat every few seconds, so a long run doesn't look hung (or get killed by
/// an agent waiting for output); with --progress json, one JSON object a line, for an agent to
/// read; with --quiet, nothing but errors. Every way but quiet ends each file with a line saying how
/// it went.
module Legibyl.Cli.Progress

open System
open System.Diagnostics
open System.Text.Json
open System.Threading

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
    let named =
        match progress |> Option.map _.ToLowerInvariant() with
        | None
        | Some "auto" -> Ok(if Console.IsErrorRedirected then Plain else Line)
        | Some "line" -> Ok Line
        | Some "plain" -> Ok Plain
        | Some "json" -> Ok Json
        | Some "none" -> Ok Quiet
        | Some _ -> Error $"Unknown --progress '{progress.Value}'. Expected auto, line, plain, json or none."
    // (A name it isn't is wrong with --quiet too.)
    named |> Result.map (fun mode -> if quiet then Quiet else mode)

/// A heartbeat this often while a stage runs on (plain lines, and JSON).
let private heartbeat = TimeSpan.FromSeconds 5.0

/// A rewritten line this often at most.
let private refresh = TimeSpan.FromMilliseconds 100.0

let private jsonOptions =
    JsonSerializerOptions(
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        DefaultIgnoreCondition = Serialization.JsonIgnoreCondition.WhenWritingNull
    )

/// A line of --progress json: where a file's got (stage, and at of total in unit where that's
/// counted), or how it went (done).
type Event =
    { File: string
      Index: int
      Count: int
      Stage: string | null
      At: Nullable<int>
      Total: Nullable<int>
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
    // What's running, as last said (for the heartbeat): the stage, counted, and where.
    let mutable running: (string * (int * int) option * string option) option = None
    let mutable stageStarted = TimeSpan.Zero
    // Says the stage again while it runs on with nothing new to say (plain lines and JSON), from
    // Begin to Done.
    let mutable beat: Timer | null = null

    /// "[2/5] garden.pdf: ", or just "garden.pdf: " for one file.
    let prefix () =
        if count > 1 then $"[{index}/{count}] {IO.Path.GetFileName file}: " else $"{IO.Path.GetFileName file}: "

    let clearLine () =
        if shownWidth > 0 then
            eprintf "\r%s\r" (String(' ', shownWidth))
            shownWidth <- 0

    let json (stage: string | null) (position: (int * int) option) (unit: string | null) (outcome: string | null) =
        let event =
            { File = file
              Index = index
              Count = count
              Stage = stage
              At = position |> Option.map fst |> Option.toNullable
              Total = position |> Option.map snd |> Option.toNullable
              Unit = unit
              Done = outcome
              ElapsedMs = clock.ElapsedMilliseconds }
        eprintfn "%s" (JsonSerializer.Serialize(event, jsonOptions))

    /// "Validating: page 37 of 200", or "Downloading: 12 of 140 MB".
    let counted (stage: string) (position: (int * int) option) (unit: string option) =
        match position with
        | Some(at, total) when total > 0 ->
            match unit with
            | Some unit -> $"{stage}: {at:N0} of {total:N0} {unit}"
            | None -> $"{stage}: page {at:N0} of {total:N0}"
        | _ -> stage

    /// Says where it's got: always, or (not forced) only once a stage changes or it's been a while;
    /// with after at the end of the line (not in JSON).
    let say (forced: bool) (after: string) (stage: string) (position: (int * int) option) (unit: string option) =
        let now = clock.Elapsed
        let changed = forced || stage <> lastStage
        match mode with
        | Line when changed || now - lastShown >= refresh ->
            let text = prefix () + counted stage position unit + after
            let width = max shownWidth text.Length
            eprintf "\r%s" (text.PadRight width)
            shownWidth <- width
            lastShown <- now
        | Plain when changed || now - lastShown >= heartbeat ->
            eprintfn "%s%s%s" (prefix ()) (counted stage position unit) after
            lastShown <- now
        | Json when changed || now - lastShown >= TimeSpan.FromSeconds 1.0 ->
            let position = position |> Option.filter (fun (_, total) -> total > 0)
            let unit: string | null =
                match unit, position with
                | Some unit, Some _ -> unit
                | None, Some _ -> "pages"
                | _ -> null
            json stage position unit null
            lastShown <- now
        | _ -> ()

    /// A heartbeat: the stage again, and how long it's been running, if nothing's been said lately.
    let tick () =
        lock gate (fun () ->
            match running with
            | Some(stage, position, unit) when clock.Elapsed - lastShown >= heartbeat ->
                let seconds = int (clock.Elapsed - stageStarted).TotalSeconds
                say true $" (still working, {seconds} s)" stage position unit
            | _ -> ())

    let stopBeating () =
        match beat with
        | NonNull timer ->
            timer.Dispose()
            beat <- null
        | Null -> ()

    member _.Mode = mode

    /// Starts on the next file.
    member _.Begin(path: string) =
        lock gate (fun () ->
            index <- index + 1
            file <- path
            lastStage <- ""
            lastShown <- TimeSpan.MinValue
            running <- None
            if (mode = Plain || mode = Json) && isNull beat then
                // (Looking every second, so it's said close to when it's due.)
                beat <- new Timer((fun _ -> tick ()), null, TimeSpan.FromSeconds 1.0, TimeSpan.FromSeconds 1.0))

    /// Where it's got: a stage ("Validating"), and how far through it (page, pages; or MB of a
    /// download) where that's counted.
    member _.Status(stage: string, ?position: int * int, ?unit: string) =
        lock gate (fun () ->
            if stage <> lastStage then stageStarted <- clock.Elapsed
            say false "" stage position unit
            running <- Some(stage, position, unit)
            lastStage <- stage)

    /// A note while it runs ("Reading the titles of linked pages: up to two minutes").
    member this.Note(text: string) = this.Status text

    /// How the file went ("PASS", "FAIL 3", "fixed → flyer.v2.pdf"), ending its line.
    member _.Done(outcome: string) =
        lock gate (fun () ->
            stopBeating ()
            running <- None
            match mode with
            | Line ->
                clearLine ()
                eprintfn "%s%s" (prefix ()) outcome
            | Plain -> eprintfn "%s%s" (prefix ()) outcome
            | Json -> json null None null outcome
            | Quiet -> ())

    /// The file couldn't be done: why ("error: garden.pdf: …"), said even with --quiet; as JSON,
    /// its done ("ERROR, …").
    member _.Error(message: string) =
        lock gate (fun () ->
            stopBeating ()
            running <- None
            match mode with
            | Json -> json null None null $"ERROR, {message}"
            | _ ->
                clearLine ()
                eprintfn $"error: {file}: {message}")

    /// Says text on a line of its own (what veraPDF logs, with --verbose), clearing a rewritten line
    /// first; it's drawn again as progress goes on.
    member _.Say(text: string) =
        lock gate (fun () ->
            clearLine ()
            lastShown <- TimeSpan.MinValue
            eprintfn "%s" text)

    /// Clears a rewritten line, before something else is said on stderr.
    member _.Clear() = lock gate clearLine
