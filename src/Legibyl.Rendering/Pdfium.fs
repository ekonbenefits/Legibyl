/// Page rendering with PDFium (Chrome's PDF renderer; see Legibyl.Validation's PdfiumPages), into
/// SkiaSharp bitmaps. PdfPig still reads the text and positions; PDFium only draws, because it
/// draws what readers draw (PdfPig's own renderer misses some content, such as a Canva flyer's
/// QR code).
namespace Legibyl.Rendering

open System
open SkiaSharp
open Legibyl.Validation

/// A PDF opened with PDFium, for drawing its pages. It keeps the file's bytes pinned while open.
[<Sealed>]
type PdfiumDocument(bytes: byte array) =
    let pages = new PdfiumPages(bytes)

    new(path: string) = new PdfiumDocument(IO.File.ReadAllBytes path)

    member _.PageCount = pages.PageCount

    /// Draws a 1-based page on white, at scale pixels per point, as a Bgra8888 bitmap for the
    /// caller to dispose. Covers the page's visible area (crop box), turned as the page says.
    member _.Render(pageNumber: int, scale: float32) : SKBitmap =
        let result = ref (null: SKBitmap | null)
        let size (width: float32) (height: float32) =
            max 1 (int (MathF.Round(width * scale))), max 1 (int (MathF.Round(height * scale)))
        // PDFium draws straight into the SKBitmap's own pixels.
        let pixels width height =
            let bitmap = new SKBitmap(SKImageInfo(width, height, SKColorType.Bgra8888, SKAlphaType.Premul))
            result.Value <- bitmap
            bitmap.GetPixels(), bitmap.RowBytes
        try
            pages.Draw(pageNumber, size, pixels, true)
        with _ ->
            match result.Value with
            | null -> ()
            | bitmap -> bitmap.Dispose()
            reraise ()
        let bitmap: SKBitmap = result.Value |> nonNull
        bitmap.NotifyPixelsChanged()
        bitmap

    interface IDisposable with
        member _.Dispose() = (pages :> IDisposable).Dispose()
