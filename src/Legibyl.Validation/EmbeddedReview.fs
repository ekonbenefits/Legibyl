/// Legibyl's review of a PDF (what's in the "<name>.pdf.legibyl.json" file next to it: dismissed
/// warnings, edits made in Suggest Structure, the title) carried inside a copy Legibyl saves, so
/// the review travels with the PDF. It's a compressed (FlateDecode) stream of the JSON that the
/// document catalog points to under a private key; readers ignore keys they don't know, so it
/// isn't shown anywhere, and it's added as an incremental update, leaving what's before it as it
/// was (iText's update also sets the modification date and producer).
module Legibyl.Validation.EmbeddedReview

open System
open System.IO
open System.Text
open iText.Kernel.Pdf

/// The catalog key. PDF asks private keys to start with a developer prefix and an underscore
/// (ISO 32000-2, Annex E).
let private key = PdfName "LBYL_Review"

/// The review embedded in pdfPath, as JSON, or None if it has none (or can't be read).
let read (pdfPath: string) : string option =
    try
        use document = PdfFiles.openToRead pdfPath
        match document.GetCatalog().GetPdfObject().GetAsStream key with
        | null -> None
        | stream -> Some(Encoding.UTF8.GetString(stream.GetBytes()))
    with
    // iText's own exceptions (not a PDF, damaged, encrypted), and files that can't be read.
    | :? iText.Commons.Exceptions.ITextException
    | :? IOException
    | :? UnauthorizedAccessException -> None

/// Writes inputPath to outputPath with json embedded as its review (replacing any it had).
/// inputPath must read without errors (iText only appends to such a file): in Fix it's always
/// one iText has just written.
let write (inputPath: string) (outputPath: string) (json: string) =
    // (The writer its own, so it's closed if the document can't be opened.)
    use writer = new PdfWriter(outputPath)
    use document = new PdfDocument(PdfFiles.reader inputPath, writer, StampingProperties().UseAppendMode())
    let stream = PdfStream(Encoding.UTF8.GetBytes json)
    stream.SetCompressionLevel CompressionConstants.DEFAULT_COMPRESSION
    stream.MakeIndirect document |> ignore
    let catalog = document.GetCatalog().GetPdfObject()
    catalog.Put(key, stream) |> ignore
    catalog.SetModified() |> ignore
