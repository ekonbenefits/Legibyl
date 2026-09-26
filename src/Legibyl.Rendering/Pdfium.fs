/// Page rendering with PDFium (Chrome's PDF renderer), through its C API: the native builds come
/// from the bblanchon.PDFium packages for Windows, Linux and macOS. PdfPig still reads the text
/// and positions; PDFium only draws, because it draws what readers draw (PdfPig's own renderer
/// misses some content, such as a Canva flyer's QR code). PDFium isn't thread-safe, so every
/// call goes through one lock.
namespace Legibyl.Rendering

open System
open System.Runtime.InteropServices
open SkiaSharp

module private Native =
    [<Literal>]
    let Library = "pdfium"

    [<DllImport(Library)>]
    extern void FPDF_InitLibrary()

    [<DllImport(Library)>]
    extern unativeint FPDF_LoadMemDocument64(nativeint data, unativeint size, string | null password)

    [<DllImport(Library)>]
    extern void FPDF_CloseDocument(unativeint document)

    [<DllImport(Library)>]
    extern int FPDF_GetPageCount(unativeint document)

    [<DllImport(Library)>]
    extern unativeint FPDF_LoadPage(unativeint document, int pageIndex)

    [<DllImport(Library)>]
    extern void FPDF_ClosePage(unativeint page)

    [<DllImport(Library)>]
    extern float32 FPDF_GetPageWidthF(unativeint page)

    [<DllImport(Library)>]
    extern float32 FPDF_GetPageHeightF(unativeint page)

    [<DllImport(Library)>]
    extern uint32 FPDF_GetLastError()

    [<DllImport(Library)>]
    extern unativeint FPDFBitmap_CreateEx(int width, int height, int format, nativeint firstScan, int stride)

    [<DllImport(Library)>]
    extern void FPDFBitmap_FillRect(unativeint bitmap, int left, int top, int width, int height, uint32 color)

    [<DllImport(Library)>]
    extern nativeint FPDFBitmap_GetBuffer(unativeint bitmap)

    [<DllImport(Library)>]
    extern int FPDFBitmap_GetStride(unativeint bitmap)

    [<DllImport(Library)>]
    extern void FPDFBitmap_Destroy(unativeint bitmap)

    [<DllImport(Library)>]
    extern void FPDF_RenderPageBitmap(unativeint bitmap, unativeint page, int startX, int startY, int sizeX, int sizeY, int rotate, int flags)

    /// FPDFBitmap_BGRA: 4 bytes a pixel, blue first, which is SkiaSharp's Bgra8888.
    [<Literal>]
    let BitmapBgra = 4

    /// FPDF_ANNOT: draw annotations' appearances too, as a reader does.
    [<Literal>]
    let RenderAnnotations = 0x01

module Pdfium =
    let internal gate = obj ()
    let private initialized = lazy (lock gate Native.FPDF_InitLibrary)

    /// Runs f holding PDFium's lock, with the library initialised.
    let internal locked f =
        initialized.Force()
        lock gate f

/// A PDF opened with PDFium, for drawing its pages. It keeps the file's bytes pinned while open.
[<Sealed>]
type PdfiumDocument(bytes: byte array) =
    let pinned = GCHandle.Alloc(bytes, GCHandleType.Pinned)
    let handle =
        Pdfium.locked (fun () -> Native.FPDF_LoadMemDocument64(pinned.AddrOfPinnedObject(), unativeint bytes.Length, null))
    do
        if handle = 0un then
            let error = Pdfium.locked Native.FPDF_GetLastError
            pinned.Free()
            raise (InvalidOperationException $"PDFium couldn't open the PDF (error {error}).")

    new(path: string) = new PdfiumDocument(IO.File.ReadAllBytes path)

    member _.PageCount = Pdfium.locked (fun () -> Native.FPDF_GetPageCount handle)

    /// Draws a 1-based page on white, at scale pixels per point, as a Bgra8888 bitmap for the
    /// caller to dispose. Covers the page's visible area (crop box), turned as the page says.
    member _.Render(pageNumber: int, scale: float32) : SKBitmap =
        Pdfium.locked (fun () ->
            let page = Native.FPDF_LoadPage(handle, pageNumber - 1)
            if page = 0un then
                raise (InvalidOperationException $"Page {pageNumber} is out of range (document has {Native.FPDF_GetPageCount handle} pages).")
            try
                let width = max 1 (int (MathF.Round(Native.FPDF_GetPageWidthF page * scale)))
                let height = max 1 (int (MathF.Round(Native.FPDF_GetPageHeightF page * scale)))
                let result = new SKBitmap(SKImageInfo(width, height, SKColorType.Bgra8888, SKAlphaType.Premul))
                // PDFium draws straight into the SKBitmap's own pixels.
                let bitmap = Native.FPDFBitmap_CreateEx(width, height, Native.BitmapBgra, result.GetPixels(), result.RowBytes)
                try
                    Native.FPDFBitmap_FillRect(bitmap, 0, 0, width, height, 0xFFFFFFFFu)
                    Native.FPDF_RenderPageBitmap(bitmap, page, 0, 0, width, height, 0, Native.RenderAnnotations)
                finally
                    Native.FPDFBitmap_Destroy bitmap
                result.NotifyPixelsChanged()
                result
            finally
                Native.FPDF_ClosePage page)

    interface IDisposable with
        member _.Dispose() =
            Pdfium.locked (fun () -> Native.FPDF_CloseDocument handle)
            pinned.Free()
