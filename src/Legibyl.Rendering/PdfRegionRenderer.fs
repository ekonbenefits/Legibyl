namespace Legibyl.Rendering

open System
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open SkiaSharp
open UglyToad.PdfPig
open UglyToad.PdfPig.Core
open Legibyl.Validation

/// Result of rendering a page image for a rule violation, with a best-effort highlight box
/// drawn around the failing object when its exact region on the page could be determined.
type HighlightImageResult =
    { /// The rendered page image, PNG-encoded.
      PngBytes: byte array
      /// The 1-based page number that was rendered.
      PageNumber: int
      /// True if a highlight box was drawn around the specific failing object. False means the full
      /// page is shown for context only — most structural/tagging failures (e.g. an untagged
      /// paragraph, a table cell missing scope) aren't attached to page geometry in veraPDF's model,
      /// only annotations (links, form widgets, etc.) currently expose a usable rectangle.
      HasPreciseRegion: bool
      /// Human-readable explanation of what was (or wasn't) highlighted, for display alongside the image.
      Note: string }

/// A labelled box to draw on a page, e.g. a suggested structure element.
type PageRegion =
    { Geometry: StructureGeometry
      /// Drawn at the box's top-left corner, e.g. "3 H".
      Label: string
      /// 0xRRGGBB.
      Color: uint32
      /// Drawn filled and thicker, e.g. the selected one.
      Emphasised: bool }

type IPdfRegionRenderer =
    /// Renders the page containing location as a PNG, highlighting the failing object's
    /// region when it can be determined. Throws InvalidOperationException if location
    /// isn't anchored to a page.
    abstract RenderPageWithHighlight:
        pdfPath: string * location: PdfLocation * [<Optional; DefaultParameterValue(2.0f)>] scale: float32 ->
            HighlightImageResult

    /// Renders the page containing geometry as a PNG, highlighting that semantic
    /// structure node's best-effort bounding rectangle.
    abstract RenderPageWithHighlight:
        pdfPath: string * geometry: StructureGeometry * [<Optional; DefaultParameterValue(2.0f)>] scale: float32 ->
            HighlightImageResult

    /// Renders the page an internal link goes to, with the part of it the destination names
    /// highlighted in a different color from a finding: its rectangle (/FitR), a band where the
    /// view's top edge is (/XYZ, /FitH), a band at its left edge (/FitV), or the whole page.
    abstract RenderPageWithTarget:
        pdfPath: string * destination: LinkDestination * [<Optional; DefaultParameterValue(2.0f)>] scale: float32 ->
            HighlightImageResult

/// An open PDF: PdfPig reads it (annotation rectangles), PDFium draws it.
type private OpenPdf =
    { Reader: PdfDocument
      Pages: PdfiumDocument }

    member this.Dispose() =
        this.Reader.Dispose()
        (this.Pages :> IDisposable).Dispose()

module private Documents =
    let openDocument (pdfPath: string) =
        let bytes = File.ReadAllBytes pdfPath
        { Reader = PdfDocument.Open bytes
          Pages = new PdfiumDocument(bytes) }

    let renderPage (document: OpenPdf) pageNumber (scale: float32) =
        if pageNumber < 1 || pageNumber > document.Reader.NumberOfPages then
            raise (InvalidOperationException $"Page {pageNumber} is out of range (document has {document.Reader.NumberOfPages} pages).")
        document.Pages.Render(pageNumber, scale)

module private Rendering =
    /// A finding or selected element.
    let highlightColor = SKColor(0xE8uy, 0x1Buy, 0x23uy)
    /// Where a link goes, so it reads differently from the link itself.
    let targetColor = SKColor(0x1Auy, 0x73uy, 0xE8uy)

    let encodePng (bitmap: SKBitmap) =
        use image = SKImage.FromBitmap bitmap
        use encoded = image.Encode(SKEncodedImageFormat.Png, 100)
        encoded.ToArray()

    let drawHighlight (color: SKColor) (bitmap: SKBitmap) (rect: PdfRectangle) (scale: float32) =
        use canvas = new SKCanvas(bitmap)

        let left = min rect.Left rect.Right
        let right = max rect.Left rect.Right
        let bottom = min rect.Top rect.Bottom
        let top = max rect.Top rect.Bottom

        // PdfPig's rectangle is bottom-left-origin, y-up; convert to the bitmap's top-left-origin, y-down space.
        let padding = 4f
        let scaled (value: float) = float32 (value * float scale)
        let height = float32 bitmap.Height
        let box =
            SKRect(scaled left - padding, height - scaled top - padding, scaled right + padding, height - scaled bottom + padding)

        use fill = new SKPaint(Color = color.WithAlpha 40uy, Style = SKPaintStyle.Fill, IsAntialias = true)
        use stroke = new SKPaint(Color = color, Style = SKPaintStyle.Stroke, StrokeWidth = 3f, IsAntialias = true)
        canvas.DrawRect(box, fill)
        canvas.DrawRect(box, stroke)

    /// A labelled region: an outline, filled when emphasised, with its label on a tab above
    /// its top-left corner.
    let drawRegion (bitmap: SKBitmap) (region: PageRegion) (scale: float32) =
        use canvas = new SKCanvas(bitmap)
        let geometry = region.Geometry
        let scaled (value: float) = float32 (value * float scale)
        let height = float32 bitmap.Height
        let box = SKRect(scaled geometry.Left, height - scaled geometry.Top, scaled geometry.Right, height - scaled geometry.Bottom)
        let color = SKColor(byte (region.Color >>> 16), byte (region.Color >>> 8), byte region.Color)
        use fill = new SKPaint(Color = color.WithAlpha(if region.Emphasised then 70uy else 18uy), Style = SKPaintStyle.Fill)
        use stroke =
            new SKPaint(Color = color, Style = SKPaintStyle.Stroke, StrokeWidth = (if region.Emphasised then 4f else 2f), IsAntialias = true)
        canvas.DrawRect(box, fill)
        canvas.DrawRect(box, stroke)
        use font = new SKFont(SKTypeface.Default, 11f * scale)
        use text = new SKPaint(Color = SKColors.White, IsAntialias = true)
        let width = font.MeasureText region.Label
        let tabHeight = font.Size * 1.25f
        let tabTop = max 0f (box.Top - tabHeight)
        use tab = new SKPaint(Color = color, Style = SKPaintStyle.Fill)
        canvas.DrawRect(SKRect(box.Left, tabTop, box.Left + width + 6f * scale, tabTop + tabHeight), tab)
        canvas.DrawText(region.Label, box.Left + 3f * scale, tabTop + font.Size, font, text)

    /// "PDLinkAnnot" -> "Link": the PdfPig annotation type veraPDF's object type corresponds to.
    let private expectedAnnotationTypeName (objectType: string | null) =
        match objectType with
        | NonNull objectType when objectType.StartsWith("PD", StringComparison.Ordinal) && objectType.EndsWith("Annot", StringComparison.Ordinal) ->
            Some objectType[2 .. objectType.Length - 6]
        | _ -> None

    let tryFindAnnotationRect (document: PdfDocument) pageNumber (location: PdfLocation) =
        if not location.AnnotationIndex.HasValue then
            None
        else
            let annotationIndex = location.AnnotationIndex.Value
            let annotations = document.GetPage(pageNumber).GetAnnotations() |> Array.ofSeq
            if annotationIndex < 0 || annotationIndex >= annotations.Length then
                None
            else
                let annotation = annotations[annotationIndex]
                let typeName = annotation.Type.ToString()

                // Sanity-check the annotation type against veraPDF's reported type (e.g. "PDLinkAnnot" -> Link)
                // so a page whose annotation array has shifted (e.g. a slightly different PDF revision) doesn't
                // silently highlight the wrong region.
                match expectedAnnotationTypeName location.ObjectType with
                | Some expected when not (String.Equals(expected, typeName, StringComparison.OrdinalIgnoreCase)) -> None
                | _ -> Some(annotation.Rectangle, $"Highlighted the {typeName.ToLowerInvariant()} annotation on this page.")

open Rendering

/// Keeps the last PDF open and its last few rendered pages, since previews are usually
/// requested one after another for the same file (and often the same page). Reopens the
/// file if it changes on disk. PdfPig documents aren't thread-safe, so all use is locked.
type private PageCache() =
    let gate = obj ()
    let capacity = 6
    let mutable current: (string * DateTime * OpenPdf) option = None
    // Most recently used last.
    let pages = List<struct (int * float32 * SKBitmap)>()

    let clearPages () =
        for struct (_, _, bitmap) in pages do
            bitmap.Dispose()
        pages.Clear()

    let close () =
        clearPages ()
        match current with
        | Some(_, _, document) -> document.Dispose()
        | None -> ()
        current <- None

    let documentFor (pdfPath: string) =
        let fullPath = Path.GetFullPath pdfPath
        let modified = File.GetLastWriteTimeUtc fullPath
        match current with
        | Some(path, stamp, document) when path = fullPath && stamp = modified -> document
        | _ ->
            close ()
            let document = Documents.openDocument fullPath
            current <- Some(fullPath, modified, document)
            document

    /// A copy of the page's rendering (from the cache if possible), for the caller to draw on and dispose.
    let pageCopy (document: OpenPdf) pageNumber scale =
        let cached =
            match pages |> Seq.tryFindIndex (fun struct (page, pageScale, _) -> page = pageNumber && pageScale = scale) with
            | Some index ->
                let entry = pages[index]
                pages.RemoveAt index
                pages.Add entry
                let struct (_, _, bitmap) = entry
                bitmap
            | None ->
                let bitmap = Documents.renderPage document pageNumber scale
                pages.Add(struct (pageNumber, scale, bitmap))
                if pages.Count > capacity then
                    let struct (_, _, oldest) = pages[0]
                    oldest.Dispose()
                    pages.RemoveAt 0
                bitmap
        cached.Copy()

    /// Runs f with the open document and a fresh copy of the page to draw on.
    member _.WithPage(pdfPath: string, pageNumber: int, scale: float32, f: PdfDocument -> SKBitmap -> 'T) : 'T =
        lock gate (fun () ->
            let document = documentFor pdfPath
            use bitmap = pageCopy document pageNumber scale
            f document.Reader bitmap)

    interface IDisposable with
        member _.Dispose() = lock gate close

[<Sealed>]
type PdfRegionRenderer() =
    let cache = new PageCache()

    member _.RenderPageWithHighlight
        (
            pdfPath: string,
            geometry: StructureGeometry,
            [<Optional; DefaultParameterValue(2.0f)>] scale: float32
        ) : HighlightImageResult =
        cache.WithPage(
            pdfPath,
            geometry.PageNumber,
            scale,
            fun _ bitmap ->
                drawHighlight highlightColor bitmap (PdfRectangle(geometry.Left, geometry.Bottom, geometry.Right, geometry.Top)) scale

                { PngBytes = encodePng bitmap
                  PageNumber = geometry.PageNumber
                  HasPreciseRegion = true
                  Note = "Highlighted this structure node's best-effort page geometry." }
        )

    member _.RenderPageWithHighlight
        (
            pdfPath: string,
            location: PdfLocation,
            [<Optional; DefaultParameterValue(2.0f)>] scale: float32
        ) : HighlightImageResult =
        if not location.PageNumber.HasValue then
            raise (InvalidOperationException "This failure isn't anchored to a specific page, so no page image can be rendered.")
        let pageNumber = location.PageNumber.Value

        cache.WithPage(
            pdfPath,
            pageNumber,
            scale,
            fun document bitmap ->
                let hasPreciseRegion, note =
                    match tryFindAnnotationRect document pageNumber location with
                    | Some(rect, note) ->
                        drawHighlight highlightColor bitmap rect scale
                        true, note
                    | None ->
                        false,
                        "Exact region unavailable for this finding — showing the full page for context."

                { PngBytes = encodePng bitmap
                  PageNumber = pageNumber
                  HasPreciseRegion = hasPreciseRegion
                  Note = note }
        )

    member _.RenderPageWithTarget
        (
            pdfPath: string,
            destination: LinkDestination,
            [<Optional; DefaultParameterValue(2.0f)>] scale: float32
        ) : HighlightImageResult =
        cache.WithPage(
            pdfPath,
            destination.PageNumber,
            scale,
            fun _ bitmap ->
                let width = float bitmap.Width / float scale
                let height = float bitmap.Height / float scale
                // How much of the page below a destination's top edge to mark.
                let band = 28.0
                let rect, note =
                    if destination.Kind = "FitR" && destination.Left.HasValue && destination.Top.HasValue
                       && destination.Right.HasValue && destination.Bottom.HasValue then
                        PdfRectangle(destination.Left.Value, destination.Bottom.Value, destination.Right.Value, destination.Top.Value),
                        "Highlighted the area the link shows."
                    elif destination.Top.HasValue then
                        let left = if destination.Left.HasValue then destination.Left.Value else 0.0
                        PdfRectangle(left, destination.Top.Value - band, width, destination.Top.Value),
                        "Highlighted where the link goes: the view starts at this line."
                    elif destination.Left.HasValue then
                        PdfRectangle(destination.Left.Value, 0.0, destination.Left.Value + band, height),
                        "Highlighted where the link goes: the view starts at this edge."
                    else
                        PdfRectangle(0.0, 0.0, width, height), "The link goes to this page, without a position on it."
                drawHighlight targetColor bitmap rect scale

                { PngBytes = encodePng bitmap
                  PageNumber = destination.PageNumber
                  HasPreciseRegion = destination.Top.HasValue || destination.Left.HasValue
                  Note = note }
        )

    /// Renders just geometry's region of its page, with margin points around it, as a PNG whose
    /// longest side is at most maxSide pixels: a figure, say, for someone (or something) to look at.
    member _.RenderRegion
        (
            pdfPath: string,
            geometry: StructureGeometry,
            [<Optional; DefaultParameterValue(12.0)>] margin: float,
            [<Optional; DefaultParameterValue(1024)>] maxSide: int
        ) : byte array =
        let scale = 2.0f
        cache.WithPage(
            pdfPath,
            geometry.PageNumber,
            scale,
            fun _ bitmap ->
                let scaled (value: float) = int (Math.Round(value * float scale))
                let clampX value = max 0 (min bitmap.Width value)
                let clampY value = max 0 (min bitmap.Height value)
                // Bottom-left-origin points to the bitmap's top-left-origin pixels, as drawHighlight does.
                let area =
                    SKRectI(
                        clampX (scaled (geometry.Left - margin)),
                        clampY (bitmap.Height - scaled (geometry.Top + margin)),
                        clampX (scaled (geometry.Right + margin)),
                        clampY (bitmap.Height - scaled (geometry.Bottom - margin))
                    )
                if area.Width <= 0 || area.Height <= 0 then
                    raise (InvalidOperationException $"The region is outside page {geometry.PageNumber}.")
                use cropped = new SKBitmap()
                bitmap.ExtractSubset(cropped, area) |> ignore
                let longest = max area.Width area.Height
                if longest <= maxSide then
                    encodePng cropped
                else
                    let ratio = float maxSide / float longest
                    let size = SKImageInfo(max 1 (int (float area.Width * ratio)), max 1 (int (float area.Height * ratio)))
                    use smaller = cropped.Resize(size, SKSamplingOptions(SKFilterMode.Linear, SKMipmapMode.Linear))
                    encodePng smaller
        )

    /// Renders pageNumber with each region outlined and labelled.
    member _.RenderPageWithRegions
        (
            pdfPath: string,
            pageNumber: int,
            regions: PageRegion seq,
            [<Optional; DefaultParameterValue(2.0f)>] scale: float32
        ) : byte array =
        cache.WithPage(
            pdfPath,
            pageNumber,
            scale,
            fun _ bitmap ->
                // The emphasised region last, so it's on top.
                for region in regions |> Seq.sortBy _.Emphasised do
                    drawRegion bitmap region scale
                encodePng bitmap
        )

    /// Closes the cached document and frees the cached pages.
    member _.Dispose() = (cache :> IDisposable).Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

    interface IPdfRegionRenderer with
        member this.RenderPageWithHighlight(pdfPath: string, location: PdfLocation, scale) =
            this.RenderPageWithHighlight(pdfPath, location, scale)

        member this.RenderPageWithHighlight(pdfPath: string, geometry: StructureGeometry, scale) =
            this.RenderPageWithHighlight(pdfPath, geometry, scale)

        member this.RenderPageWithTarget(pdfPath, destination, scale) =
            this.RenderPageWithTarget(pdfPath, destination, scale)
