/// Opening PDFs to read: from their bytes, read at once, so the file isn't held open while it's
/// read (or after, when what reads it fails). On Windows a file held open can't be saved over,
/// renamed or deleted: a background read (a review's language, Legibyl's own checks) would stop
/// a person doing that, and one that failed on a broken file would until it was collected.
module Legibyl.Validation.PdfFiles

open System.IO
open iText.Kernel.Pdf

/// An iText reader of the PDF at path, from its bytes.
let reader (path: string) = new PdfReader(new MemoryStream(File.ReadAllBytes path))

/// The PDF at path, opened by iText to read.
let openToRead (path: string) = new PdfDocument(reader path)

/// The PDF at path, opened by PdfPig.
let openWithPdfPig (path: string) = UglyToad.PdfPig.PdfDocument.Open(File.ReadAllBytes path)
