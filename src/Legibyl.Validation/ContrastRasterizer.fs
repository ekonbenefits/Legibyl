/// Draws pages with PDFium for veraPDF's WCAG contrast check, in place of PDFBox: Legibyl's
/// ContrastRatioConsumer asks legibyl.wcag.PageRasterizers for them (see java/README.md).
module internal Legibyl.Validation.ContrastRasterizer

open System
open System.Runtime.InteropServices

/// Whether e means PDFium can't draw anything here (its native library missing or the wrong
/// one, or out of memory), not that this PDF or page is one it can't draw.
let private cantDraw (e: exn) =
    match e with
    | :? DllNotFoundException
    | :? EntryPointNotFoundException
    | :? BadImageFormatException
    | :? TypeInitializationException
    | :? OutOfMemoryException -> true
    | _ -> false

/// What veraPDF is told when drawing fails. A PDF or page PDFium can't draw is an IOException:
/// veraPDF catches it and goes without contrast for that file or page, as it did for one PDFBox
/// couldn't read. PDFium not drawing at all is an IllegalStateException, which veraPDF doesn't
/// catch: the WCAG pass fails, and the report says WCAG 2.2 wasn't checked (WcagError), rather
/// than every PDF passing contrast unchecked.
let private failure (what: string) (e: exn) : exn =
    if cantDraw e then java.lang.IllegalStateException($"PDFium can't draw pages for WCAG's contrast check: {e.Message}")
    else java.io.IOException($"PDFium couldn't {what} for WCAG's contrast check: {e.Message}")

/// One PDF, opened for one contrast pass.
type private ContrastPages(path: string) =
    let pages =
        try
            new PdfiumPages(path)
        with e ->
            raise (failure "open the PDF" e)

    interface legibyl.wcag.RasterDocument with
        /// The 0-based page at dpi, as PDFBox's renderImageWithDPI drew it: its size in points
        /// times dpi / 72, rounded down, and no anti-aliasing. Pixels are Java's ARGB ints, which
        /// is PDFium's blue-first BGRA bytes on a little-endian machine.
        member _.render(pageIndex, dpi, size) =
            let drawn = ref None
            let scale = float32 dpi / 72f
            let sized (width: float32) (height: float32) =
                max 1 (int (MathF.Floor(width * scale))), max 1 (int (MathF.Floor(height * scale)))
            let pixels width height =
                let buffer = Array.zeroCreate<int> (width * height)
                let pin = GCHandle.Alloc(buffer, GCHandleType.Pinned)
                drawn.Value <- Some(buffer, pin, width, height)
                pin.AddrOfPinnedObject(), width * 4
            try
                try
                    pages.Draw(pageIndex + 1, sized, pixels, false)
                with e ->
                    raise (failure $"draw page {pageIndex + 1}" e)
            finally
                match drawn.Value with
                | Some(_, pin, _, _) -> pin.Free()
                | None -> ()
            match drawn.Value with
            | Some(buffer, _, width, height) ->
                size[0] <- width
                size[1] <- height
                buffer
            | None -> raise (java.io.IOException $"PDFium didn't draw page {pageIndex + 1} for WCAG's contrast check.")

        member this.close() = (this :> IDisposable).Dispose()

    // IKVM gives java.io.Closeable .NET's IDisposable too.
    interface IDisposable with
        member _.Dispose() = (pages :> IDisposable).Dispose()

/// What veraPDF's contrast check opens each PDF with.
let rasterizer =
    { new legibyl.wcag.PageRasterizer with
        member _.``open`` path = new ContrastPages(path) :> legibyl.wcag.RasterDocument }
