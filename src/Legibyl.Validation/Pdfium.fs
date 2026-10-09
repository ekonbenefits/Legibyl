/// PDFium (Chrome's PDF renderer), through its C API: the native builds come from the
/// bblanchon.PDFium packages for Windows, Linux and macOS. It draws pages for WCAG's contrast
/// check (ContrastRasterizer) and, through Legibyl.Rendering, for everything Legibyl shows.
/// PDFium isn't thread-safe, so every call, from either, goes through one lock.
namespace Legibyl.Validation

open System
open System.Runtime.InteropServices

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

    /// FPDF_RENDER_NO_SMOOTHTEXT, FPDF_RENDER_NO_SMOOTHIMAGE and FPDF_RENDER_NO_SMOOTHPATH: no
    /// anti-aliasing, as veraPDF has PDFBox draw pages for its contrast check.
    [<Literal>]
    let NoSmoothing = 0x7000

module internal Pdfium =
    let private gate = obj ()
    let private initialized = lazy (lock gate Native.FPDF_InitLibrary)

    /// Runs f holding PDFium's lock, with the library initialised.
    let internal locked f =
        initialized.Force()
        lock gate f

/// A PDF opened with PDFium, for drawing its pages. It keeps the file's bytes pinned while open.
[<Sealed>]
type PdfiumPages(bytes: byte array) =
    let pinned = GCHandle.Alloc(bytes, GCHandleType.Pinned)
    let handle =
        Pdfium.locked (fun () -> Native.FPDF_LoadMemDocument64(pinned.AddrOfPinnedObject(), unativeint bytes.Length, null))
    do
        if handle = 0un then
            let error = Pdfium.locked Native.FPDF_GetLastError
            pinned.Free()
            raise (InvalidOperationException $"PDFium couldn't open the PDF (error {error}).")

    new(path: string) = new PdfiumPages(IO.File.ReadAllBytes path)

    member _.PageCount = Pdfium.locked (fun () -> Native.FPDF_GetPageCount handle)

    /// Draws a 1-based page on white, its visible area (crop box) turned as the page says. size
    /// gives the picture's width and height in pixels from the page's in points; pixels gives
    /// where to draw them, 4 bytes a pixel, blue first, and the bytes in a row. smooth draws as
    /// a reader shows it, anti-aliased; without, as veraPDF's contrast check measures it.
    member _.Draw(pageNumber: int, size: float32 -> float32 -> int * int, pixels: int -> int -> nativeint * int, smooth: bool) =
        Pdfium.locked (fun () ->
            let page = Native.FPDF_LoadPage(handle, pageNumber - 1)
            if page = 0un then
                raise (InvalidOperationException $"Page {pageNumber} is out of range (document has {Native.FPDF_GetPageCount handle} pages).")
            try
                let width, height = size (Native.FPDF_GetPageWidthF page) (Native.FPDF_GetPageHeightF page)
                let buffer, stride = pixels width height
                let bitmap = Native.FPDFBitmap_CreateEx(width, height, Native.BitmapBgra, buffer, stride)
                try
                    Native.FPDFBitmap_FillRect(bitmap, 0, 0, width, height, 0xFFFFFFFFu)
                    let flags = if smooth then Native.RenderAnnotations else Native.RenderAnnotations ||| Native.NoSmoothing
                    Native.FPDF_RenderPageBitmap(bitmap, page, 0, 0, width, height, 0, flags)
                finally
                    Native.FPDFBitmap_Destroy bitmap
            finally
                Native.FPDF_ClosePage page)

    interface IDisposable with
        member _.Dispose() =
            Pdfium.locked (fun () -> Native.FPDF_CloseDocument handle)
            pinned.Free()
