/// Which PDF/UA standard applies to a file: PDF/UA-1 (ISO 14289-1) is defined on PDF 1.7 and
/// PDF/UA-2 (ISO 14289-2) on PDF 2.0, so it follows from the PDF version the file declares. (Its
/// own pdfuaid:part claim isn't a better signal: a wrong claim is what validation should catch,
/// and the metadata fix writes the claim for the standard checked.)
module Legibyl.Validation.PdfUaTarget

open System
open iText.Kernel.Pdf

/// The PDF version a file declares, e.g. "1.7" or "2.0": its header's, or its catalog's
/// /Version where that's later. None if the file can't be read (encrypted, damaged, missing).
let pdfVersion (pdfPath: string) : string option =
    try
        use document = new PdfDocument(new PdfReader(pdfPath))
        // "PDF-1.7" -> "1.7"
        Some(document.GetPdfVersion().ToString().Replace("PDF-", ""))
    with
    | :? iText.Commons.Exceptions.ITextException
    | :? IO.IOException
    | :? UnauthorizedAccessException -> None

/// The standard for a PDF version: PDF/UA-2 for 2.0 or later, PDF/UA-1 before (and when the
/// version isn't known).
let forVersion (version: string option) : PdfProfile =
    match version |> Option.bind (fun v -> match Version.TryParse v with | true, NonNull parsed -> Some parsed | _ -> None) with
    | Some parsed when parsed.Major >= 2 -> PdfProfile.PdfUa2
    | _ -> PdfProfile.PdfUa1

/// The standard that applies to a file, and the PDF version it was worked out from.
let detect (pdfPath: string) : PdfProfile * string option =
    let version = pdfVersion pdfPath
    forVersion version, version

/// A standard's name, as people know it: "PDF/UA-1" or "PDF/UA-2".
let displayName (profile: PdfProfile) =
    match profile with
    | PdfProfile.PdfUa2 -> "PDF/UA-2"
    | _ -> "PDF/UA-1"

/// A veraPDF flavour id's name, e.g. "ua1" -> "PDF/UA-1"; other ids uppercased.
let displayNameOfFlavour (flavourId: string) =
    match flavourId.ToLowerInvariant() with
    | "ua1" -> "PDF/UA-1"
    | "ua2" -> "PDF/UA-2"
    | other -> other.ToUpperInvariant()

/// What a report was checked against, and why: "PDF/UA-2 (worked out from the file: PDF 2.0)",
/// or "PDF/UA-2 (asked for; the file is PDF 1.7)".
let describe (profile: PdfProfile) (version: string | null) (detected: bool) =
    let name = displayName profile
    match Option.ofObj version, detected with
    | Some version, true -> $"{name} (worked out from the file: PDF {version})"
    | Some version, false -> $"{name} (asked for; the file is PDF {version})"
    | None, _ -> name

/// The standard a finding's rule belongs to, by its specification ("ISO 14289-2:2024" is
/// PDF/UA-2's); None for others (ISO 32005's tag nesting, which PDF/UA-2 includes, and Legibyl's).
let ofSpecification (specification: string) : PdfProfile option =
    if specification.StartsWith("ISO 14289-1", StringComparison.Ordinal) then Some PdfProfile.PdfUa1
    elif specification.StartsWith("ISO 14289-2", StringComparison.Ordinal) then Some PdfProfile.PdfUa2
    else None
