/// Small helpers for working with Java (IKVM) and iText objects, which are null-oblivious.
module internal Legibyl.Validation.Interop

open System

/// Applies f unless value is null, like C#'s `?.` operator.
let inline (|?>) (value: 'T | null) ([<InlineIfLambda>] f: 'T -> 'U) : 'U when 'U: null =
    match value with
    | null -> null
    | value -> f value

/// Enumerates a Java collection.
let javaSeq (items: java.util.Collection) : obj seq =
    seq {
        let iterator = items.iterator ()
        while iterator.hasNext () do
            yield iterator.next ()
    }

let nullIfEmpty (value: string | null) : string | null =
    if String.IsNullOrEmpty value then null else value

let lookup (map: Collections.Generic.IReadOnlyDictionary<'K, 'V>) key =
    match map.TryGetValue key with
    | true, value -> Some value
    | _ -> None
