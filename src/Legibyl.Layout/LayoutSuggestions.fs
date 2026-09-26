/// Suggests a structure for untagged pages from how they look, using PaddleOCR's layout detector
/// (PP-DocLayoutV3): the regions of a page, what kind each is (title, text, table, figure...) and
/// their reading order. Suggestions only: nothing here changes a PDF. The detector's model (about
/// 130 MB) is downloaded on first use into PaddleOCR's cache (~/.cache/paddleocr-sharp).
namespace Legibyl.Layout

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
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
      Text: string
      /// For a list item (LI): its list's /ListNumbering, e.g. "Disc" or "Decimal".
      ListNumbering: string option
      /// True when Text was recognised by OCR, because the region's text is drawn as
      /// outlines or pixels rather than as text.
      Recognized: bool }

type PageSuggestion =
    { PageNumber: int
      Blocks: SuggestedBlock list
      /// False when the page has no text to tag (a scan, say): it would need OCR first.
      HasText: bool
      /// A rotated page isn't detected yet: its rendering's coordinates are turned.
      Rotated: bool }

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
    /// origin, in points), for a page whose visible area (crop box) has its top-left corner at
    /// (left, top), rendered at scale pixels per point.
    let toGeometry pageNumber (left: float, top: float) (scale: float) (box: LayoutBox) : StructureGeometry =
        { PageNumber = pageNumber
          Left = left + float box.Left / scale
          Right = left + float box.Right / scale
          Top = top - float box.Top / scale
          Bottom = top - float box.Bottom / scale }

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

    /// Whether a suggested tag is for text (so a region with none as text needs OCR).
    let holdsText (tag: string) = tag <> "Figure" && tag <> "Table" && tag <> "Artifact"

    // Items of one list start within this many points of each other.
    [<Literal>]
    let private SameIndent = 6.0

    /// The detector has no list regions: a list's items come out as paragraphs. Like the
    /// ListsFromParagraphs fix, runs of two or more consecutive paragraphs that start at the
    /// same indent with markers forming one list ("•", "1.", "(a)") become list items.
    let findLists (blocks: SuggestedBlock list) : SuggestedBlock list =
        let isCandidate (block: SuggestedBlock) = block.Tag = "P" && ParagraphLists.startsWithMarker block.Text
        let runs =
            (blocks, [])
            ||> List.foldBack (fun block runs ->
                match runs with
                | (next :: _ as run) :: rest when
                    isCandidate block && isCandidate next
                    && block.Geometry.PageNumber = next.Geometry.PageNumber
                    && abs (block.Geometry.Left - next.Geometry.Left) <= SameIndent
                    ->
                    (block :: run) :: rest
                | _ -> [ block ] :: runs)
        [ for run in runs do
              match ParagraphLists.numberingOfTexts (run |> List.map _.Text) with
              | Some numbering when isCandidate run.Head ->
                  for block in run -> { block with Tag = "LI"; ListNumbering = Some numbering }
              | _ -> yield! run ]

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

/// Reads text drawn as outlines or pixels, with Tesseract and its English model (shipped in
/// the app's tessdata folder). Unavailable where Tesseract has no native library (Intel Macs).
module TextRecognition =
    open Tesseract

    let private tessdata () = Path.Combine(AppContext.BaseDirectory, "tessdata")

    /// A Tesseract engine, or None if it can't be loaded here.
    let createEngine () : TesseractEngine option =
        try
            Some(new TesseractEngine(tessdata (), "eng", EngineMode.Default))
        with _ ->
            // No native library for this platform, or it failed to load: no OCR, rather
            // than no layout.
            None

    /// The text in a PNG of one region, on one line.
    let recognize (engine: TesseractEngine) (png: byte array) =
        use pix = Pix.LoadFromMemory png
        use page = engine.Process(pix, PageSegMode.SingleBlock)
        Regex.Replace(page.GetText(), @"\s+", " ").Trim()

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
    let ocr = lazy (TextRecognition.createEngine ())
    // OCR wants more pixels than the detector: about 300 dpi.
    let ocrScale = 4.0

    /// The text of regions that have none as text but look like they hold some.
    let recognizeText pageNumber (crop: UglyToad.PdfPig.Core.PdfRectangle) (regions: (string * StructureGeometry) list) =
        match ocr.Value with
        | Some engine when regions |> List.exists (fun (tag, _) -> tag <> "") ->
            use bitmap = document.GetPageAsSKBitmap(pageNumber, float32 ocrScale, SKColors.White)
            [ for tag, geometry in regions ->
                  if tag = "" then
                      None
                  else
                      let pixels (value: float) = int (Math.Round(value * ocrScale))
                      let area =
                          SKRectI(
                              max 0 (pixels (geometry.Left - crop.Left)),
                              max 0 (pixels (crop.Top - geometry.Top)),
                              min bitmap.Width (pixels (geometry.Right - crop.Left)),
                              min bitmap.Height (pixels (crop.Top - geometry.Bottom))
                          )
                      if area.Width < 4 || area.Height < 4 then
                          None
                      else
                          use region = new SKBitmap()
                          if bitmap.ExtractSubset(region, area) then
                              use image = SKImage.FromBitmap region
                              use png = image.Encode(SKEncodedImageFormat.Png, 100)
                              match TextRecognition.recognize engine (png.ToArray()) with
                              | "" -> None
                              | text -> Some text
                          else
                              None ]
        | _ -> regions |> List.map (fun _ -> None)

    member _.PageCount = document.NumberOfPages

    /// Detects pageNumber's regions. The model must be downloaded (see LayoutModel).
    member _.Suggest(pageNumber: int) : PageSuggestion =
        lock gate (fun () ->
            let page = document.GetPage pageNumber
            let rotated = page.Rotation.Value % 360 <> 0
            let boxes =
                if rotated then
                    [||] :> IReadOnlyList<_>
                else
                    use bitmap = document.GetPageAsSKBitmap(pageNumber, float32 scale, SKColors.White)
                    use image = LayoutSuggestions.rgbImage bitmap
                    detector.Value.Detect image
            let crop = page.CropBox.Bounds
            let regions = [ for box in boxes -> LayoutSuggestions.toGeometry pageNumber (crop.Left, crop.Top) scale box ]
            let words =
                [ for word in page.GetWords() do
                      let centre = word.BoundingBox.Centroid
                      yield word.Text, (centre.X, centre.Y) ]
            let texts = LayoutSuggestions.assignText regions words
            // Regions that should hold text but have none as text: their text is drawn as
            // outlines or pixels, so read it with OCR.
            let recognized =
                recognizeText
                    pageNumber
                    crop
                    [ for box, region, text in List.zip3 (List.ofSeq boxes) regions texts ->
                          let tag = LayoutSuggestions.tagFor box.Label
                          (if text = "" && LayoutSuggestions.holdsText tag then tag else ""), region ]
            { PageNumber = pageNumber
              HasText = not words.IsEmpty
              Rotated = rotated
              Blocks =
                [ for i, ((box, region, text), recognized) in List.indexed (List.zip (List.zip3 (List.ofSeq boxes) regions texts) recognized) ->
                      { Order = i + 1
                        Label = box.Label
                        Tag = LayoutSuggestions.tagFor box.Label
                        Score = float box.Score
                        Geometry = region
                        Text = defaultArg recognized text
                        ListNumbering = None
                        Recognized = recognized.IsSome } ]
                |> LayoutSuggestions.findLists })

    interface IDisposable with
        member _.Dispose() =
            lock gate (fun () ->
                if detector.IsValueCreated then
                    detector.Value.Dispose()
                if ocr.IsValueCreated then
                    ocr.Value |> Option.iter _.Dispose()
                document.Dispose())

/// Regions for the TagFromLayout fix, detected page by page as the fix asks for them.
/// progress gets (page, page count) as each page starts. The model must be downloaded.
[<Sealed>]
type LayoutRegionSource(progress: IProgress<int * int> | null) =
    let suggesters = Dictionary<string, LayoutSuggester>()

    interface IPageLayoutSource with
        member _.PageRegions(pdfPath, pageNumber) =
            let suggester =
                match suggesters.TryGetValue pdfPath with
                | true, suggester -> suggester
                | _ ->
                    let suggester = new LayoutSuggester(pdfPath)
                    suggesters[pdfPath] <- suggester
                    suggester
            match progress with
            | NonNull progress -> progress.Report((pageNumber, suggester.PageCount))
            | Null -> ()
            [ for block in (suggester.Suggest pageNumber).Blocks ->
                  { Tag = block.Tag
                    Geometry = block.Geometry
                    ListNumbering = Option.toObj block.ListNumbering
                    ActualText = if block.Recognized then block.Text else null } ]

    interface IDisposable with
        member _.Dispose() =
            for suggester in suggesters.Values do
                (suggester :> IDisposable).Dispose()
