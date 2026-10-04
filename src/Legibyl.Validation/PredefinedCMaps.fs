/// The CMaps ISO 32000 predefines (9.7.5.2), which a PDF can name without embedding: the
/// character collection each belongs to.
module Legibyl.Validation.PredefinedCMaps

open System

/// The character collection (registry, ordering) a predefined CMap's name belongs to (ISO 32000,
/// 9.7.5.2), for the CJK CMaps that name one; Identity CMaps have none.
let ordering (cmap: string) =
    let starts (prefixes: string list) = prefixes |> List.exists (fun prefix -> cmap.StartsWith(prefix, StringComparison.Ordinal))
    if starts [ "UniGB"; "GB" ] then Some "GB1"
    elif starts [ "UniCNS"; "B5"; "ETen"; "HK"; "CNS" ] then Some "CNS1"
    elif cmap = "H" || cmap = "V"
         || starts [ "UniJIS"; "90ms"; "90msp"; "90pv"; "83pv"; "78"; "Add"; "EUC"; "Ext"; "NWP"; "RKSJ"; "Hankaku"; "Hiragana"; "Katakana"; "Roman"; "WP-Symbol" ] then
        Some "Japan1"
    elif starts [ "UniKS"; "KSC" ] then Some "Korea1"
    else None
