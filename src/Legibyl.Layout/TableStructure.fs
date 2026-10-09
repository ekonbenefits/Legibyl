/// A table's rows and cells, found from how it looks by Microsoft's Table Transformer (its
/// structure-recognition model, MIT; Xenova's 8-bit ONNX export of v1.0), run with ONNX Runtime:
/// rows, columns, the column header, and cells spanning several, as boxes. Those become the
/// table's grid: a cell for each row and column, a spanning cell for those it covers, header cells
/// in the header's rows. Its model (about 30 MB) is bundled with the app by the build (models/); a
/// build without it downloads it on first use, with the layout detector's (see LayoutModel).
namespace Legibyl.Layout

open System
open System.IO
open System.Net.Http
open System.Security.Cryptography
open System.Threading
open System.Threading.Tasks
open Microsoft.ML.OnnxRuntime
open Microsoft.ML.OnnxRuntime.Tensors
open SkiaSharp

/// The table-structure model: where it is, whether it's downloaded, and downloading it.
module TableModel =
    /// Roughly how much there is to download.
    [<Literal>]
    let DownloadSizeMegabytes = 30

    /// Xenova's ONNX export, at a fixed revision, and what its file must be.
    let private address =
        "https://huggingface.co/Xenova/table-transformer-structure-recognition/resolve/5387550de655512721e1b88e4e42117001ba4813/onnx/model_quantized.onnx"

    let private sha256 = "c11f4033da75e9c4d41c403ef356e89caa0a37a7d111b55461e7d5ba856bb6b6"
    let private size = 30158413L

    /// Where it's downloaded to, with Legibyl's own files, when it isn't bundled.
    let private downloaded () =
        Path.Combine(
            Environment.GetFolderPath Environment.SpecialFolder.LocalApplicationData,
            "Legibyl",
            "models",
            "table-transformer-structure-recognition",
            "model_quantized.onnx"
        )

    /// Where the build bundles it, with the app (see Legibyl.Layout.fsproj).
    let private bundled () =
        Path.Combine(AppContext.BaseDirectory, "models", "table-transformer-structure-recognition", "model_quantized.onnx")

    let private complete (path: string) =
        let file = FileInfo path
        file.Exists && file.Length = size

    /// Where it is: bundled with the app, else where it's downloaded to.
    let path () = if complete (bundled ()) then bundled () else downloaded ()

    let isDownloaded () = complete (path ())

    /// Saves what's at address to target, reporting the fraction done (0-1).
    let private fetch (target: string) (progress: IProgress<float>) (cancellation: CancellationToken) : Task =
        task {
            use client = new HttpClient()
            use! response = client.GetAsync(address, HttpCompletionOption.ResponseHeadersRead, cancellation)
            response.EnsureSuccessStatusCode() |> ignore
            use! source = response.Content.ReadAsStreamAsync cancellation
            use file = File.Create target
            let buffer = Array.zeroCreate<byte> 81920
            let mutable received = 0L
            let mutable reading = true
            while reading do
                let! n = source.ReadAsync(buffer.AsMemory(), cancellation)
                if n = 0 then
                    reading <- false
                else
                    file.Write(buffer, 0, n)
                    received <- received + int64 n
                    progress.Report(min 1.0 (float received / float size))
        }

    /// Downloads it if it isn't already, reporting the fraction done (0-1). A file that isn't the
    /// one expected (its SHA-256) isn't kept.
    let download (progress: IProgress<float>) (cancellation: CancellationToken) : Task =
        task {
            if not (isDownloaded ()) then
                let target = downloaded ()
                Directory.CreateDirectory(Path.GetDirectoryName target |> string) |> ignore
                let partial = target + ".partial"
                do! fetch partial progress cancellation
                // Off the caller's thread (the UI's, in the app): 30 MB to read and hash.
                let! hash =
                    Task.Run(fun () ->
                        use stream = File.OpenRead partial
                        Convert.ToHexString(SHA256.HashData stream).ToLowerInvariant())
                if hash <> sha256 then
                    File.Delete partial
                    failwith $"The table model downloaded from {address} isn't the one expected."
                File.Move(partial, target, true)
        }

/// A table's grid, from what the model found in a picture of it.
module TableStructure =
    /// What the model found: what it is, how sure it is (0-1), and its box in the picture's pixels.
    type Found =
        { Label: string
          Score: float32
          Box: SKRect }

    /// One cell of the grid: whether it's a header cell, and its box in the picture's pixels.
    type Cell = { Header: bool; Box: SKRect }

    let private labels = [| "table"; "column"; "row"; "column header"; "projected row header"; "spanning cell" |]

    /// How sure the model must be of something to take it.
    [<Literal>]
    let private Threshold = 0.5f

    /// Runs the model on picture (a table, with a little margin), as its own preprocessing does:
    /// the shortest edge to 800 pixels (the longest at most 1000), each channel scaled to 0-1 and
    /// normalised by ImageNet's mean and deviation.
    let find (session: InferenceSession) (picture: SKBitmap) : Found list =
        let scale = min (800.0 / float (min picture.Width picture.Height)) (1000.0 / float (max picture.Width picture.Height))
        let width, height = max 1 (int (round (float picture.Width * scale))), max 1 (int (round (float picture.Height * scale)))
        use resized =
            picture.Resize(SKImageInfo(width, height, SKColorType.Rgba8888, SKAlphaType.Unpremul), SKSamplingOptions(SKFilterMode.Linear, SKMipmapMode.None))
        let mean = [| 0.485f; 0.456f; 0.406f |]
        let deviation = [| 0.229f; 0.224f; 0.225f |]
        let pixels = DenseTensor<float32>([| 1; 3; height; width |])
        let colours = resized.Pixels
        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                let colour = colours[y * width + x]
                pixels[0, 0, y, x] <- (float32 colour.Red / 255f - mean[0]) / deviation[0]
                pixels[0, 1, y, x] <- (float32 colour.Green / 255f - mean[1]) / deviation[1]
                pixels[0, 2, y, x] <- (float32 colour.Blue / 255f - mean[2]) / deviation[2]
        let inputs = ResizeArray<NamedOnnxValue>()
        inputs.Add(NamedOnnxValue.CreateFromTensor("pixel_values", pixels))
        if session.InputMetadata.ContainsKey "pixel_mask" then
            let mask = DenseTensor<int64>([| 1; height; width |])
            mask.Fill 1L
            inputs.Add(NamedOnnxValue.CreateFromTensor("pixel_mask", mask))
        use results = session.Run inputs
        let logits = (results |> Seq.find (fun r -> r.Name = "logits")).AsTensor<float32>()
        let boxes = (results |> Seq.find (fun r -> r.Name = "pred_boxes")).AsTensor<float32>()
        let classes = logits.Dimensions[2]
        [ for query in 0 .. logits.Dimensions[1] - 1 do
              // The softmax over its classes; the last is "no object".
              let raw = [| for k in 0 .. classes - 1 -> logits[0, query, k] |]
              let top = Array.max raw
              let exps = raw |> Array.map (fun v -> exp (v - top))
              let sum = Array.sum exps
              let label, score = [| for k in 0 .. classes - 2 -> k, exps[k] / sum |] |> Array.maxBy snd
              if score > Threshold && label < labels.Length then
                  // Centre, width and height, as fractions of the picture.
                  let cx, cy, w, h = boxes[0, query, 0], boxes[0, query, 1], boxes[0, query, 2], boxes[0, query, 3]
                  let W, H = float32 picture.Width, float32 picture.Height
                  yield
                      { Label = labels[label]
                        Score = score
                        Box = SKRect((cx - w / 2f) * W, (cy - h / 2f) * H, (cx + w / 2f) * W, (cy + h / 2f) * H) } ]

    /// found's boxes labelled label, most sure first, leaving out any mostly over one kept (along
    /// the axis they're stacked on), in order along it.
    let private distinct (label: string) (vertical: bool) (found: Found list) =
        let span (b: SKRect) = if vertical then b.Top, b.Bottom else b.Left, b.Right
        let overlap (a: SKRect) (b: SKRect) =
            let a0, a1 = span a
            let b0, b1 = span b
            max 0f (min a1 b1 - max a0 b0) / max 1f (min (a1 - a0) (b1 - b0))
        found
        |> List.filter (fun f -> f.Label = label)
        |> List.sortByDescending _.Score
        |> List.fold (fun (kept: SKRect list) f -> if kept |> List.exists (fun k -> overlap k f.Box > 0.5f) then kept else f.Box :: kept) []
        |> List.sortBy (fun b -> fst (span b))

    /// The bands between found's boxes, edge to edge: each box's band reaches halfway to the next.
    let private bands (boxes: (float32 * float32) list) =
        let boxes = Array.ofList boxes
        [ for i in 0 .. boxes.Length - 1 ->
              let start = if i = 0 then fst boxes[0] else (snd boxes[i - 1] + fst boxes[i]) / 2f
              let finish = if i = boxes.Length - 1 then snd boxes[i] else (snd boxes[i] + fst boxes[i + 1]) / 2f
              start, finish ]

    /// The grid what the model found makes: a cell for each row and column; one for the rows and
    /// columns a spanning cell covers (more than half of each); header cells in the rows the
    /// column header covers. None for fewer than two cells (it's no table). Projected row headers
    /// (a row that heads the rows below it, across the table) are spanning cells across it.
    let grid (found: Found list) : Cell list =
        let rows = distinct "row" true found |> List.map (fun b -> b.Top, b.Bottom) |> bands
        let columns = distinct "column" false found |> List.map (fun b -> b.Left, b.Right) |> bands
        if rows.Length * columns.Length < 2 then
            []
        else
            let covered (start: float32, finish: float32) (b0: float32, b1: float32) =
                max 0f (min finish b1 - max start b0) > (finish - start) / 2f
            let headers =
                found |> List.filter (fun f -> f.Label = "column header") |> List.map (fun f -> f.Box.Top, f.Box.Bottom)
            let isHeader (row: int) =
                let top, bottom = rows[row]
                headers |> List.exists (fun (h0, h1) -> (top + bottom) / 2f >= h0 && (top + bottom) / 2f <= h1)
            let rectangle (r0: int) (r1: int) (c0: int) (c1: int) =
                SKRect(fst columns[c0], fst rows[r0], snd columns[c1], snd rows[r1])
            // Spanning cells, most sure first, each over rows and columns no other has taken.
            let taken = Collections.Generic.HashSet<int * int>()
            let spanning =
                [ for f in found |> List.filter (fun f -> f.Label = "spanning cell" || f.Label = "projected row header") |> List.sortByDescending _.Score do
                      let across = f.Label = "projected row header"
                      let inRows = [ for i, band in List.indexed rows do if covered band (f.Box.Top, f.Box.Bottom) then i ]
                      let inColumns =
                          if across then [ 0 .. columns.Length - 1 ]
                          else [ for i, band in List.indexed columns do if covered band (f.Box.Left, f.Box.Right) then i ]
                      match inRows, inColumns with
                      | r0 :: _, c0 :: _ ->
                          let r1, c1 = List.last inRows, List.last inColumns
                          let cells = [ for r in r0..r1 do for c in c0..c1 -> r, c ]
                          if cells.Length > 1 && not (cells |> List.exists taken.Contains) then
                              for cell in cells do taken.Add cell |> ignore
                              yield { Header = isHeader r0; Box = rectangle r0 r1 c0 c1 }
                      | _ -> () ]
            [ for r in 0 .. rows.Length - 1 do
                  for c in 0 .. columns.Length - 1 do
                      if not (taken.Contains(r, c)) then
                          yield { Header = isHeader r; Box = rectangle r r c c } ]
            @ spanning
