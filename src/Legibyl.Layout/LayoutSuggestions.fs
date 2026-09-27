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
      Recognized: bool
      /// For a figure that's a QR code: what it encodes (often a web address).
      QrText: string option }

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

    let private centreOf (g: StructureGeometry) = (g.Left + g.Right) / 2.0, (g.Bottom + g.Top) / 2.0

    /// Puts QR codes found on the page into the layout: a code inside a figure describes that
    /// figure; one the detector didn't take for a figure becomes a figure of its own, in
    /// reading order after the regions above it. Orders are renumbered.
    let placeQrCodes (codes: (string * StructureGeometry) list) (blocks: SuggestedBlock list) : SuggestedBlock list =
        let placed =
            (blocks, codes)
            ||> List.fold (fun blocks (text, geometry) ->
                let point = centreOf geometry
                let figure =
                    blocks
                    |> List.filter (fun b -> b.Tag = "Figure" && b.QrText.IsNone && contains b.Geometry point)
                    |> List.sortBy (fun b -> area b.Geometry)
                    |> List.tryHead
                match figure with
                | Some figure -> blocks |> List.map (fun b -> if obj.ReferenceEquals(b, figure) then { b with QrText = Some text } else b)
                | None ->
                    let code =
                        { Order = 0
                          Label = "qr_code"
                          Tag = "Figure"
                          Score = 1.0
                          Geometry = geometry
                          Text = ""
                          ListNumbering = None
                          Recognized = false
                          QrText = Some text }
                    let above = blocks |> List.takeWhile (fun b -> b.Geometry.Top >= geometry.Top) |> List.length
                    List.insertAt above code blocks)
        placed |> List.mapi (fun i b -> { b with Order = i + 1 })

    /// Suggested blocks as the regions the tag-from-layout fix writes.
    let toRegions (blocks: SuggestedBlock list) : LayoutRegion list =
        [ for block in blocks ->
              { Tag = block.Tag
                Geometry = block.Geometry
                ListNumbering = Option.toObj block.ListNumbering
                ActualText = if block.Recognized then block.Text else null
                QrText = Option.toObj block.QrText
                Alt = null } ]

    /// Whether a suggested tag is for text (so a region with none as text needs OCR).
    let holdsText (tag: string) = tag <> "Figure" && tag <> "Table" && tag <> "Artifact"

    /// The bands of region, full width (bottom, top in points, highest first), that none of its
    /// words cover and that are tall enough for a line of their size: where a region with some
    /// text as text may have more drawn as outlines or pixels ("5th grade" under a badge's
    /// name, say). words are the page's word boxes; those centred in region count.
    let uncoveredBands (region: StructureGeometry) (words: StructureGeometry list) : (float * float) list =
        let inside =
            words
            |> List.filter (fun w -> contains region ((w.Left + w.Right) / 2.0, (w.Bottom + w.Top) / 2.0))
        match inside with
        | [] -> []
        | _ ->
            let heights = inside |> List.map (fun w -> w.Top - w.Bottom) |> List.sort
            let lineHeight = heights[heights.Length / 2]
            // Down from the region's top: each gap above a word's span, then what's below the last.
            // A word's box is about its letters, not their descenders and ascenders, so each span
            // is widened by a quarter of a line: reading those alone gives OCR nonsense.
            let margin = 0.25 * lineHeight
            let spans =
                inside
                |> List.map (fun w -> max region.Bottom (w.Bottom - margin), min region.Top (w.Top + margin))
                |> List.sortByDescending snd
            let gaps, lowest =
                spans
                |> List.fold
                    (fun (gaps, above) (bottom, top) -> (if top < above then (top, above) :: gaps else gaps), min above bottom)
                    ([], region.Top)
            (region.Bottom, lowest) :: gaps
            |> List.filter (fun (bottom, top) -> top - bottom >= 0.8 * lineHeight)
            |> List.sortByDescending snd

    /// Whether text OCR read looks like words rather than decoration or the edge of a line read as
    /// text ("C—O ———_", "FG en a ae re"): mostly letters and digits, with a word of three or more.
    let looksLikeText (text: string) =
        let visible = text |> Seq.filter (not << Char.IsWhiteSpace) |> Seq.length
        let alphanumeric = text |> Seq.filter Char.IsLetterOrDigit |> Seq.length
        visible > 0
        && float alphanumeric / float visible >= 0.6
        && text.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.exists (fun w -> (w |> Seq.filter Char.IsLetterOrDigit |> Seq.length) >= 3)

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
    // ---- As a layout file (see LayoutFile) ----

    let private boxOf (geometry: StructureGeometry) =
        [| geometry.Left; geometry.Bottom; geometry.Right; geometry.Top |] |> Array.map (fun v -> Math.Round(v, 1))

    /// The text of the nearest region with some before (or after) index in blocks, skipping page
    /// furniture and other figures.
    let private nearbyText (blocks: SuggestedBlock list) (index: int) (step: int) =
        let rec look i =
            if i < 0 || i >= blocks.Length then
                null
            else
                let block = blocks[i]
                if holdsText block.Tag && not (String.IsNullOrWhiteSpace block.Text) then block.Text.Trim()
                else look (i + step)
        look (index + step)

    /// The layout suggested for pdfPath's pages, with title (e.g. its own, or its first H1's text).
    let toLayoutFile (pdfPath: string) (pages: PageSuggestion list) (title: string option) : LayoutFile.SavedLayout =
        let region (blocks: SuggestedBlock list) index (block: SuggestedBlock) : LayoutFile.SavedRegion =
            let figure = block.Tag = "Figure"
            { Order = block.Order
              Tag = block.Tag
              Box = boxOf block.Geometry
              Label = block.Label
              Score = Math.Round(block.Score, 3)
              Text = block.Text
              Recognized = block.Recognized
              ListNumbering = Option.toObj block.ListNumbering
              QrText = Option.toObj block.QrText
              Image = null
              TextBefore = if figure then nearbyText blocks index -1 else null
              TextAfter = if figure then nearbyText blocks index 1 else null }
        let fingerprints = LayoutFile.fingerprints pdfPath
        let page (page: PageSuggestion) : LayoutFile.SavedPage =
            { Page = page.PageNumber
              HasText = page.HasText
              Rotated = page.Rotated
              Fingerprint = Map.tryFind page.PageNumber fingerprints |> Option.toObj
              Regions = page.Blocks |> List.mapi (region page.Blocks) |> Array.ofList }
        { Version = 1
          File = string (Path.GetFileName pdfPath)
          Title = Option.toObj title
          Pages = pages |> List.map page |> Array.ofList }

    /// A saved page's regions as the model's suggestions (what it found, as the layout keeps it).
    let ofLayoutPage (page: LayoutFile.SavedPage) : PageSuggestion =
        { PageNumber = page.Page
          HasText = page.HasText
          Rotated = page.Rotated
          Blocks =
            [ for region in page.Regions |> Array.sortBy _.Order ->
                  { Order = region.Order
                    Label = string region.Label
                    Tag = region.Tag
                    Score = region.Score
                    Geometry =
                      { PageNumber = page.Page
                        Left = region.Box[0]
                        Bottom = region.Box[1]
                        Right = region.Box[2]
                        Top = region.Box[3] }
                    Text = string region.Text
                    ListNumbering = Option.ofObj region.ListNumbering
                    Recognized = region.Recognized
                    QrText = Option.ofObj region.QrText } ] }

    /// A title for the layout: the document's own (or a short first page's), else the first H1
    /// the layout found; as Suggest Structure prefills it.
    let suggestedTitle (pdfPath: string) (pages: PageSuggestion list) =
        match DocumentTitles.candidates pdfPath with
        | first :: _ -> Some first.Title
        | [] ->
            pages
            |> List.collect _.Blocks
            |> List.tryFind (fun block -> block.Tag = "H1" && not (String.IsNullOrWhiteSpace block.Text))
            |> Option.map _.Text.Trim()

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

/// Reads QR codes, so a figure that is one can say what it encodes, and link to it.
module QrCodes =
    open ZXing
    open ZXing.Common

    let private reader =
        BarcodeReaderGeneric(Options = DecodingOptions(PossibleFormats = [| BarcodeFormat.QR_CODE |], TryHarder = true))

    /// The image in grey two ways: ordinary luminance, and each pixel's darkest channel. Codes
    /// with coloured finder patterns (yellow rings, say) nearly vanish in ordinary luminance,
    /// where yellow is almost as light as white; in the darkest channel any strong colour is
    /// dark, as the code's design intends. Each is also given inverted, for light codes on a
    /// dark background.
    let private greyVersions (bitmap: SKBitmap) : LuminanceSource list =
        use rgba = new SKBitmap(bitmap.Width, bitmap.Height, SKColorType.Rgba8888, SKAlphaType.Unpremul)
        if bitmap.CopyTo(rgba, SKColorType.Rgba8888) then
            let pixels = rgba.GetPixelSpan()
            let count = rgba.Width * rgba.Height
            let luminance = Array.zeroCreate<byte> count
            let darkest = Array.zeroCreate<byte> count
            for i in 0 .. count - 1 do
                let r, g, b = pixels[i * 4], pixels[i * 4 + 1], pixels[i * 4 + 2]
                luminance[i] <- byte ((299 * int r + 587 * int g + 114 * int b) / 1000)
                darkest[i] <- min r (min g b)
            [ for grey in [ luminance; darkest ] do
                  let source = RGBLuminanceSource(grey, rgba.Width, rgba.Height, RGBLuminanceSource.BitmapFormat.Gray8) :> LuminanceSource
                  yield source
                  yield source.invert () ]
        else
            []

    /// What the QR code in the bitmap encodes, if it holds one (dark on light, or light on dark).
    let decode (bitmap: SKBitmap) : string option =
            greyVersions bitmap
            |> List.tryPick (fun colours ->
                match lock reader (fun () -> reader.Decode colours) with
                | null -> None
                | result when String.IsNullOrWhiteSpace result.Text -> None
                | result -> Some(result.Text.Trim()))

    /// The QR codes ZXing finds in one image: what each encodes, and its bounds in the
    /// image's pixels (left, top, right, bottom).
    let private decodeIn (source: LuminanceSource) =
        match lock reader (fun () -> reader.DecodeMultiple source) with
        | null -> []
        | results ->
            [ for result in results do
                  if not (String.IsNullOrWhiteSpace result.Text) && not (isNull result.ResultPoints) && result.ResultPoints.Length >= 3 then
                      // The points are the finder patterns' centres, about 3.5 modules in from
                      // the code's edges; widen by a sixth of their spread to cover it.
                      let xs = result.ResultPoints |> Array.map (fun p -> float p.X)
                      let ys = result.ResultPoints |> Array.map (fun p -> float p.Y)
                      let margin = max (Array.max xs - Array.min xs) (Array.max ys - Array.min ys) / 6.0
                      yield
                          result.Text.Trim(),
                          (Array.min xs - margin, Array.min ys - margin, Array.max xs + margin, Array.max ys + margin) ]

    /// The scan pyramid, as in qr-link-pdf (github.com/jbtule/qr-link-pdf): ZXing can miss a
    /// code that's too small a part of a large page, and also one that's too large until the
    /// image is shrunk, so the page is scanned at each of these scales and the results merged.
    let private scales = [ 1.0; 0.6; 0.4; 0.25 ]

    /// Every QR code in a page image, scanned at several resolutions, each in both greys and
    /// inverted (see greyVersions): what each encodes, and its bounds in the image's pixels.
    let decodeAll (bitmap: SKBitmap) : (string * (float * float * float * float)) list =
        let found = ResizeArray<string * (float * float * float * float)>()
        let add (text: string, (left, top, right, bottom)) =
            let x, y = (left + right) / 2.0, (top + bottom) / 2.0
            let seen =
                found
                |> Seq.exists (fun (other, (l, t, r, b)) -> other = text && x >= l && x <= r && y >= t && y <= b)
            if not seen then found.Add((text, (left, top, right, bottom)))
        for scale in scales do
            let width, height = int (float bitmap.Width * scale), int (float bitmap.Height * scale)
            if width >= 64 && height >= 64 then
                use scaled =
                    if scale = 1.0 then bitmap.Copy() else bitmap.Resize(SKImageInfo(width, height), SKSamplingOptions(SKFilterMode.Linear))
                for colours in greyVersions scaled do
                    for text, (l, t, r, b) in decodeIn colours do
                        add (text, (l / scale, t / scale, r / scale, b / scale))
        List.ofSeq found

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
    let bytes = File.ReadAllBytes pdfPath
    // PdfPig reads the words and page boxes; PDFium draws the pages.
    let document = PdfDocument.Open bytes
    let pages = new Legibyl.Rendering.PdfiumDocument(bytes)
    let detector = lazy (LayoutDetector.Load(LayoutModel.directory ()))
    let ocr = lazy (TextRecognition.createEngine ())
    // OCR wants more pixels than the detector: about 300 dpi.
    let ocrScale = 4.0
    // QR codes are scanned at 400 dpi, as qr-link-pdf does: stylised codes (rounded modules,
    // a logo in the middle) need the detail.
    let qrScale = 400.0 / 72.0

    /// Reads each region that's asked for (Some) from a high-resolution rendering of the page,
    /// made at scale pixels per point.
    let readRegions (bitmap: SKBitmap) (scale: float) (crop: UglyToad.PdfPig.Core.PdfRectangle) (regions: ((SKBitmap -> 'T option) option * StructureGeometry) list) =
            [ for read, geometry in regions ->
                  match read with
                  | None -> None
                  | Some read ->
                      let pixels (value: float) = int (Math.Round(value * scale))
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
                          if bitmap.ExtractSubset(region, area) then read region else None ]

    /// OCR for one region's pixels.
    let recognize (region: SKBitmap) =
        match ocr.Value with
        | Some engine ->
            use image = SKImage.FromBitmap region
            use png = image.Encode(SKEncodedImageFormat.Png, 100)
            match TextRecognition.recognize engine (png.ToArray()) with
            | "" -> None
            | text -> Some text
        | None -> None

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
                    use bitmap = pages.Render(pageNumber, float32 scale)
                    use image = LayoutSuggestions.rgbImage bitmap
                    detector.Value.Detect image
            let crop = page.CropBox.Bounds
            let regions = [ for box in boxes -> LayoutSuggestions.toGeometry pageNumber (crop.Left, crop.Top) scale box ]
            let words =
                [ for word in page.GetWords() do
                      let centre = word.BoundingBox.Centroid
                      yield word.Text, (centre.X, centre.Y) ]
            let texts = LayoutSuggestions.assignText regions words
            // A high-resolution rendering, for OCR and QR codes, once there's something to read.
            use highResolution =
                if boxes.Count = 0 then null else pages.Render(pageNumber, float32 ocrScale)
            // Regions that should hold text but have none as text: their text is drawn as
            // outlines or pixels, so read it with OCR.
            let recognized =
                match highResolution with
                | null -> texts |> List.map (fun _ -> None)
                | bitmap ->
                    readRegions
                        bitmap
                        ocrScale
                        crop
                        [ for box, region, text in List.zip3 (List.ofSeq boxes) regions texts ->
                              let tag = LayoutSuggestions.tagFor box.Label
                              (if text = "" && LayoutSuggestions.holdsText tag then Some recognize else None), region ]
            // Regions with some text as text, and bands of them no word covers (e.g. a line drawn as
            // outlines under one that's text): what OCR reads there that looks like words, with
            // the text, in order down the page.
            let completed =
                match highResolution with
                | null -> texts |> List.map (fun _ -> None)
                | bitmap ->
                    let wordBoxes: StructureGeometry list =
                        [ for word in page.GetWords() ->
                              { PageNumber = pageNumber
                                Left = word.BoundingBox.Left
                                Bottom = word.BoundingBox.Bottom
                                Right = word.BoundingBox.Right
                                Top = word.BoundingBox.Top } ]
                    [ for box, region, text in List.zip3 (List.ofSeq boxes) regions texts ->
                          match LayoutSuggestions.uncoveredBands region wordBoxes with
                          | bands when text <> "" && LayoutSuggestions.holdsText (LayoutSuggestions.tagFor box.Label) && not bands.IsEmpty ->
                              let read =
                                  readRegions bitmap ocrScale crop [ for bottom, top in bands -> Some recognize, { region with Bottom = bottom; Top = top } ]
                              let found =
                                  List.zip bands read
                                  |> List.choose (fun ((_, top), read) -> read |> Option.filter LayoutSuggestions.looksLikeText |> Option.map (fun r -> top, r))
                              if found.IsEmpty then
                                  None
                              else
                                  let wordsTop =
                                      wordBoxes
                                      |> List.filter (fun w -> w.Left >= region.Left - 1.0 && w.Right <= region.Right + 1.0 && w.Bottom >= region.Bottom - 1.0 && w.Top <= region.Top + 1.0)
                                      |> List.map _.Top
                                      |> List.fold max region.Bottom
                                  let above, below = found |> List.partition (fun (top, _) -> top > wordsTop)
                                  Some(String.Join(" ", List.map snd above @ [ text ] @ List.map snd below))
                          | _ -> None ]
            // QR codes: the detector's figures first (a region holding one decodes most
            // reliably), then the whole page, scanned at several resolutions, for any it didn't
            // take for a figure. In PDF points.
            let qrCodes =
                if boxes.Count = 0 then
                    []
                else
                    use bitmap = pages.Render(pageNumber, float32 qrScale)
                    let points (left, top, right, bottom) : StructureGeometry =
                        { PageNumber = pageNumber
                          Left = crop.Left + left / qrScale
                          Right = crop.Left + right / qrScale
                          Top = crop.Top - top / qrScale
                          Bottom = crop.Top - bottom / qrScale }
                    let inFigures =
                        readRegions
                            bitmap
                            qrScale
                            crop
                            [ for box, region in List.zip (List.ofSeq boxes) regions ->
                                  (if LayoutSuggestions.tagFor box.Label = "Figure" then Some QrCodes.decode else None), region ]
                        |> List.zip regions
                        |> List.choose (fun (region, text) -> text |> Option.map (fun text -> text, region))
                    let onPage = [ for text, bounds in QrCodes.decodeAll bitmap -> text, points bounds ]
                    // The page scan finds the figures' codes again; keep those only once.
                    let known (text, geometry: StructureGeometry) =
                        let x, y = (geometry.Left + geometry.Right) / 2.0, (geometry.Bottom + geometry.Top) / 2.0
                        inFigures
                        |> List.exists (fun (other, (r: StructureGeometry)) -> other = text && x >= r.Left && x <= r.Right && y >= r.Bottom && y <= r.Top)
                    inFigures @ (onPage |> List.filter (not << known))
            { PageNumber = pageNumber
              HasText = not words.IsEmpty
              Rotated = rotated
              Blocks =
                [ for i, ((box, region, text), (recognized, completed)) in
                      List.indexed (List.zip (List.zip3 (List.ofSeq boxes) regions texts) (List.zip recognized completed)) ->
                      // Text read with OCR, all of it or some, is what a reader hears (ActualText).
                      let read = recognized |> Option.orElse completed
                      { Order = i + 1
                        Label = box.Label
                        Tag = LayoutSuggestions.tagFor box.Label
                        Score = float box.Score
                        Geometry = region
                        Text = defaultArg read text
                        ListNumbering = None
                        Recognized = read.IsSome
                        QrText = None } ]
                |> LayoutSuggestions.placeQrCodes qrCodes
                |> LayoutSuggestions.findLists })

    interface IDisposable with
        member _.Dispose() =
            lock gate (fun () ->
                if detector.IsValueCreated then
                    detector.Value.Dispose()
                if ocr.IsValueCreated then
                    ocr.Value |> Option.iter _.Dispose()
                (pages :> IDisposable).Dispose()
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
            LayoutSuggestions.toRegions (suggester.Suggest pageNumber).Blocks

        member _.LinkDescription(_, _, _) = null
        member _.PageTitles = null
        member _.DocumentTitle = null

    interface IDisposable with
        member _.Dispose() =
            for suggester in suggesters.Values do
                (suggester :> IDisposable).Dispose()

/// A link in the PDF to a web page, with no description of its own (/Contents).
type WebLink =
    { Page: int
      Box: StructureGeometry
      Address: string }

/// Finds a PDF's links to web pages without the layout model, before Suggest Structure opens:
/// its link annotations that have no description, and, page by page, its QR codes (the same
/// scan Suggest Structure uses, about a second a page).
[<Sealed>]
type LinkScanner(pdfPath: string) =
    let bytes = File.ReadAllBytes pdfPath
    let document = PdfDocument.Open bytes
    let pages = new Legibyl.Rendering.PdfiumDocument(bytes)
    let qrScale = 400.0 / 72.0

    member _.PageCount = document.NumberOfPages

    /// The link annotations that go to a web page and have no description.
    member _.WebLinks() : WebLink list =
        use reader = new iText.Kernel.Pdf.PdfReader(new MemoryStream(bytes))
        use pdf = new iText.Kernel.Pdf.PdfDocument(reader)
        [ for page in 1 .. pdf.GetNumberOfPages() do
              for annotation in pdf.GetPage(page).GetAnnotations() do
                  let dictionary = annotation.GetPdfObject()
                  let uri =
                      match dictionary.GetAsDictionary iText.Kernel.Pdf.PdfName.A with
                      | null -> None
                      | action ->
                          match action.GetAsString iText.Kernel.Pdf.PdfName.URI with
                          | null -> None
                          | uri -> LinkTitles.webAddress (uri.ToUnicodeString())
                  match uri, annotation.GetRectangle() with
                  | Some address, NonNull rectangle when
                      iText.Kernel.Pdf.PdfName.Link.Equals(annotation.GetSubtype())
                      && isNull (dictionary.Get iText.Kernel.Pdf.PdfName.Contents)
                      ->
                      let r = rectangle.ToRectangle()
                      yield
                          { Page = page
                            Box =
                              { PageNumber = page
                                Left = float (r.GetLeft())
                                Bottom = float (r.GetBottom())
                                Right = float (r.GetRight())
                                Top = float (r.GetTop()) }
                            Address = address }
                  | _ -> () ]

    /// The QR codes on a page: what each encodes, and where, in PDF points.
    member _.QrCodes(pageNumber: int) : (string * StructureGeometry) list =
        let crop = document.GetPage(pageNumber).CropBox.Bounds
        use bitmap = pages.Render(pageNumber, float32 qrScale)
        [ for text, (left, top, right, bottom) in QrCodes.decodeAll bitmap ->
              text,
              { PageNumber = pageNumber
                Left = crop.Left + left / qrScale
                Right = crop.Left + right / qrScale
                Top = crop.Top - top / qrScale
                Bottom = crop.Top - bottom / qrScale } ]

    interface IDisposable with
        member _.Dispose() =
            (pages :> IDisposable).Dispose()
            document.Dispose()
