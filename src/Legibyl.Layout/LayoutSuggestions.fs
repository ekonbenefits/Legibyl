/// Suggests a structure for untagged pages from how they look, using PaddleOCR's layout detector
/// (PP-DocLayoutV3): the regions of a page, what kind each is (title, text, table, figure...) and
/// their reading order. Suggestions only: nothing here changes a PDF. The detector's model (about
/// 130 MB) is downloaded on first use into PaddleOCR's cache (~/.cache/paddleocr-sharp).
namespace Legibyl.Layout

open System
open System.Threading
open System.Threading.Tasks
open SkiaSharp
open UglyToad.PdfPig
open UglyToad.PdfPig.Rendering.Skia
open PaddleOcrSharp.Download
open PaddleOcrSharp.Imaging
open PaddleOcrSharp.Models.Layout
open Legibyl.Validation

/// One region the detector found, as a suggested structure element.
type SuggestedBlock =
    { /// 1-based position in the page's reading order.
      Order: int
      /// The detector's name for the region, e.g. "paragraph_title".
      Label: string
      /// The tag it suggests, e.g. "H", or "Artifact" for page furniture like running headers.
      Tag: string
      /// The detector's confidence, 0-1.
      Score: float
      Geometry: StructureGeometry
      /// The page's own text inside the region (empty for a figure, or a page without a text layer).
      Text: string }

type PageSuggestion =
    { PageNumber: int
      Blocks: SuggestedBlock list
      /// False when the page has no text to tag (a scan, say): it would need OCR first.
      HasText: bool }

module LayoutSuggestions =
    /// The tag a detector label suggests. Heading levels aren't known from one page's layout,
    /// so titles within the document are plain H.
    let tagFor (label: string) =
        match label with
        | "doc_title" -> "H1"
        | "paragraph_title" -> "H"
        | "table" -> "Table"
        | "image"
        | "chart"
        | "seal" -> "Figure"
        | "figure_title" -> "Caption"
        | "footnote"
        | "vision_footnote" -> "Note"
        | "display_formula"
        | "inline_formula" -> "Formula"
        | "formula_number" -> "Lbl"
        | "header"
        | "footer"
        | "number"
        | "header_image"
        | "footer_image" -> "Artifact"
        | _ -> "P"

    /// A box in the rendered image's pixels (top-left origin) as PDF user space (bottom-left
    /// origin, in points), for a page rendered at scale pixels per point.
    let toGeometry pageNumber (pageHeight: float) (scale: float) (box: LayoutBox) : StructureGeometry =
        { PageNumber = pageNumber
          Left = float box.Left / scale
          Right = float box.Right / scale
          Top = pageHeight - float box.Top / scale
          Bottom = pageHeight - float box.Bottom / scale }

    let private contains (geometry: StructureGeometry) (x: float, y: float) =
        x >= geometry.Left && x <= geometry.Right && y >= geometry.Bottom && y <= geometry.Top

    let private area (geometry: StructureGeometry) =
        (geometry.Right - geometry.Left) * (geometry.Top - geometry.Bottom)

    /// Each word's text goes to the smallest region its centre is in, so nested or overlapping
    /// regions don't both claim it.
    let assignText (regions: StructureGeometry list) (words: (string * (float * float)) list) : string list =
        let byRegion = Array.init regions.Length (fun _ -> ResizeArray<string>())
        let indexed = List.indexed regions
        for text, centre in words do
            indexed
            |> List.filter (fun (_, region) -> contains region centre)
            |> List.sortBy (snd >> area)
            |> List.tryHead
            |> Option.iter (fun (index, _) -> byRegion[index].Add text)
        [ for words in byRegion -> String.Join(" ", words) ]

    /// The bitmap's pixels as tightly packed RGB, which the detector takes.
    let internal rgbImage (bitmap: SKBitmap) =
        use rgba = new SKBitmap(bitmap.Width, bitmap.Height, SKColorType.Rgba8888, SKAlphaType.Unpremul)
        bitmap.CopyTo(rgba, SKColorType.Rgba8888) |> ignore
        let source = rgba.GetPixelSpan()
        let pixels = Array.zeroCreate<byte> (bitmap.Width * bitmap.Height * 3)
        for i in 0 .. bitmap.Width * bitmap.Height - 1 do
            pixels[i * 3] <- source[i * 4]
            pixels[i * 3 + 1] <- source[i * 4 + 1]
            pixels[i * 3 + 2] <- source[i * 4 + 2]
        RgbImage.Wrap(pixels, bitmap.Width, bitmap.Height)

/// The layout detector's model: whether it's downloaded, and downloading it.
module LayoutModel =
    /// Roughly how much there is to download, for telling the user before they do.
    [<Literal>]
    let DownloadSizeMegabytes = 130

    let private downloader () = new ModelDownloader()

    /// Where the model is (or will be) kept.
    let directory () =
        use downloader = downloader ()
        downloader.DirectoryFor ModelCatalog.PpDocLayoutV3

    /// Where it's downloaded from.
    let source () =
        use downloader = downloader ()
        downloader.Endpoint

    let isDownloaded () =
        use downloader = downloader ()
        downloader.IsComplete ModelCatalog.PpDocLayoutV3

    /// Downloads the model if it isn't already, reporting the fraction done (0-1).
    let download (progress: IProgress<float>) (cancellation: CancellationToken) : Task =
        task {
            use downloader = downloader ()
            let totals = Collections.Generic.Dictionary<string, int64 * int64>()
            let report =
                Progress<DownloadProgress>(fun p ->
                    lock totals (fun () ->
                        totals[p.File] <- (p.BytesReceived, (if p.TotalBytes.HasValue then p.TotalBytes.Value else p.BytesReceived))
                        let received = totals.Values |> Seq.sumBy fst
                        // The weights are nearly all of it; before their size is known, guess.
                        let total = max (totals.Values |> Seq.sumBy snd) (int64 DownloadSizeMegabytes * 1_000_000L)
                        progress.Report(float received / float total)))
            let! _ = downloader.EnsureAsync(ModelCatalog.PpDocLayoutV3, report, cancellation)
            ()
        }

/// Suggests structure for a PDF's pages, one page at a time (each takes seconds). Keeps the
/// PDF and the detector open until disposed; calls are serialised, since neither is thread-safe.
[<Sealed>]
type LayoutSuggester(pdfPath: string) =
    let gate = obj ()
    // Pixels per point to render at; the detector scales pages to its own input size anyway.
    let scale = 2.0
    let document =
        let document = PdfDocument.Open(pdfPath, SkiaRenderingParsingOptions.Instance)
        document.AddSkiaPageFactory()
        document
    let detector = lazy (LayoutDetector.Load(LayoutModel.directory ()))

    member _.PageCount = document.NumberOfPages

    /// Detects pageNumber's regions. The model must be downloaded (see LayoutModel).
    member _.Suggest(pageNumber: int) : PageSuggestion =
        lock gate (fun () ->
            let page = document.GetPage pageNumber
            let boxes =
                use bitmap = document.GetPageAsSKBitmap(pageNumber, float32 scale, SKColors.White)
                use image = LayoutSuggestions.rgbImage bitmap
                detector.Value.Detect image
            let regions = [ for box in boxes -> LayoutSuggestions.toGeometry pageNumber page.Height scale box ]
            let words =
                [ for word in page.GetWords() do
                      let centre = word.BoundingBox.Centroid
                      yield word.Text, (centre.X, centre.Y) ]
            let texts = LayoutSuggestions.assignText regions words
            { PageNumber = pageNumber
              HasText = not words.IsEmpty
              Blocks =
                [ for i, (box, region, text) in List.indexed (List.zip3 (List.ofSeq boxes) regions texts) ->
                      { Order = i + 1
                        Label = box.Label
                        Tag = LayoutSuggestions.tagFor box.Label
                        Score = float box.Score
                        Geometry = region
                        Text = text } ] })

    interface IDisposable with
        member _.Dispose() =
            lock gate (fun () ->
                if detector.IsValueCreated then
                    detector.Value.Dispose()
                document.Dispose())
