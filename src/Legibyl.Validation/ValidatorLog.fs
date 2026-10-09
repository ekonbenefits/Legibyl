/// What veraPDF logs (through java.util.logging) while it reads a file. IKVM ships Java's default
/// logging configuration, which prints every record to the console; instead, a handler of our own
/// keeps them for the validation they came from, as notes on its report (ValidationReport.Notes).
module internal Legibyl.Validation.ValidatorLog

open System
open System.Collections.Concurrent
open System.Text.RegularExpressions
open System.Threading

/// A record, as kept.
type private Record = { Level: string; Logger: string; Message: string }

/// The records of the validation running here (the flow of one Validate call), if any.
let private current = AsyncLocal<ConcurrentQueue<Record> | null>()

/// Whether what's logged here (the flow of one quietly call) is left out: neither raised nor kept.
let private quiet = AsyncLocal<bool>()

/// Each record kept, as it comes (Count 1, its message whole): for a caller to show as it goes,
/// as the CLI's --verbose prints them. The library itself never writes to the console.
let recorded = Event<ValidatorNote>()

type private Collector() =
    inherit java.util.logging.Handler()
    let formatter = java.util.logging.SimpleFormatter()

    override _.publish(record: java.util.logging.LogRecord) =
        match record with
        | null -> ()
        | record when record.getLevel().intValue () < java.util.logging.Level.WARNING.intValue () -> ()
        | _ when quiet.Value -> ()
        | record ->
            let name = record.getLoggerName () |> Option.ofObj |> Option.defaultValue ""
            let kept =
                { Level = record.getLevel().getName ()
                  Logger = name.Substring(name.LastIndexOf '.' + 1)
                  Message = formatter.formatMessage record |> Option.ofObj |> Option.defaultValue "" }
            recorded.Trigger { Level = kept.Level; Logger = kept.Logger; Message = kept.Message; Count = 1 }
            match current.Value with
            | null -> ()
            | records -> records.Enqueue kept

    override _.flush() = ()
    override _.close() = ()

let private installLock = obj ()
let mutable private installed = false

/// Takes over java.util.logging's root logger: its console handler goes, and ours keeps records
/// instead. Once, before veraPDF logs anything.
let install () =
    lock installLock (fun () ->
        if not installed then
            let root = java.util.logging.Logger.getLogger ""
            for handler in root.getHandlers () do
                root.removeHandler handler
            root.addHandler (Collector())
            installed <- true)

/// A message's shape: without the number a repeat of it ends with ("Nested MCID - 132").
let private shape (message: string) = Regex.Replace(message.Trim(), @"\s*-\s*\d+$", "")

/// Runs f, returning what it gives and the notes veraPDF logged while it ran: WARNING and above,
/// one per level, logger and message shape, counted, in the order first logged.
let collecting (f: unit -> 'T) : 'T * ValidatorNote list =
    let records = ConcurrentQueue<Record>()
    let outer = current.Value
    current.Value <- records
    let result =
        try
            f ()
        finally
            current.Value <- outer
    let notes =
        records
        |> Seq.map (fun r -> { r with Message = shape r.Message })
        |> Seq.countBy id
        |> Seq.map (fun (r, count) -> { Level = r.Level; Logger = r.Logger; Message = r.Message; Count = count })
        |> List.ofSeq
    result, notes

/// Runs f with what veraPDF logs while it runs left out: neither raised nor kept, as for a second
/// read of a file whose notes the first gave.
let quietly (f: unit -> 'T) : 'T =
    let outer = quiet.Value
    quiet.Value <- true
    try
        f ()
    finally
        quiet.Value <- outer
