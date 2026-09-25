namespace Legibyl.Rendering

open System
open System.Runtime.InteropServices
open SkiaSharp
open UglyToad.PdfPig
open UglyToad.PdfPig.Core
open UglyToad.PdfPig.Rendering.Skia
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

module private Rendering =
    let private highlightColor = SKColor(0xE8uy, 0x1Buy, 0x23uy)

    let openDocument (pdfPath: string) =
        let document = PdfDocument.Open(pdfPath, SkiaRenderingParsingOptions.Instance)
        document.AddSkiaPageFactory()
        document

    let renderPage (document: PdfDocument) pageNumber (scale: float32) =
        if pageNumber < 1 || pageNumber > document.NumberOfPages then
            raise (InvalidOperationException $"Page {pageNumber} is out of range (document has {document.NumberOfPages} pages).")
        document.GetPageAsSKBitmap(pageNumber, scale, SKColors.White)

    let encodePng (bitmap: SKBitmap) =
        use image = SKImage.FromBitmap bitmap
        use encoded = image.Encode(SKEncodedImageFormat.Png, 100)
        encoded.ToArray()

    let drawHighlight (bitmap: SKBitmap) (rect: PdfRectangle) (scale: float32) =
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

        use fill = new SKPaint(Color = highlightColor.WithAlpha 40uy, Style = SKPaintStyle.Fill, IsAntialias = true)
        use stroke = new SKPaint(Color = highlightColor, Style = SKPaintStyle.Stroke, StrokeWidth = 3f, IsAntialias = true)
        canvas.DrawRect(box, fill)
        canvas.DrawRect(box, stroke)

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

[<Sealed>]
type PdfRegionRenderer() =
    member _.RenderPageWithHighlight
        (
            pdfPath: string,
            geometry: StructureGeometry,
            [<Optional; DefaultParameterValue(2.0f)>] scale: float32
        ) : HighlightImageResult =
        use document = openDocument pdfPath
        use bitmap = renderPage document geometry.PageNumber scale
        drawHighlight bitmap (PdfRectangle(geometry.Left, geometry.Bottom, geometry.Right, geometry.Top)) scale

        { PngBytes = encodePng bitmap
          PageNumber = geometry.PageNumber
          HasPreciseRegion = true
          Note = "Highlighted this structure node's best-effort page geometry." }

    member _.RenderPageWithHighlight
        (
            pdfPath: string,
            location: PdfLocation,
            [<Optional; DefaultParameterValue(2.0f)>] scale: float32
        ) : HighlightImageResult =
        if not location.PageNumber.HasValue then
            raise (InvalidOperationException "This failure isn't anchored to a specific page, so no page image can be rendered.")
        let pageNumber = location.PageNumber.Value

        use document = openDocument pdfPath
        use bitmap = renderPage document pageNumber scale

        let hasPreciseRegion, note =
            match tryFindAnnotationRect document pageNumber location with
            | Some(rect, note) ->
                drawHighlight bitmap rect scale
                true, note
            | None ->
                false,
                "Exact region unavailable for this finding — showing the full page for context."

        { PngBytes = encodePng bitmap
          PageNumber = pageNumber
          HasPreciseRegion = hasPreciseRegion
          Note = note }

    interface IPdfRegionRenderer with
        member this.RenderPageWithHighlight(pdfPath: string, location: PdfLocation, scale) =
            this.RenderPageWithHighlight(pdfPath, location, scale)

        member this.RenderPageWithHighlight(pdfPath: string, geometry: StructureGeometry, scale) =
            this.RenderPageWithHighlight(pdfPath, geometry, scale)
