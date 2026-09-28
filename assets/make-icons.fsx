// Makes Legibyl's icons from icon_vector.svg (exported from icon_vector.af), each size drawn from
// the vector rather than scaled from a bitmap:
//
//   Legibyl.icns   macOS: the artwork on Apple's icon grid (824 of 1024, centred, with a soft
//                  shadow below), 16 to 1024 pixels
//   Legibyl.ico    Windows: the artwork to the edge, 16 to 256 pixels
//   Legibyl.png    the About window's, 256 pixels, as Windows'
//
// After changing the SVG, run from the repository:
//
//   dotnet fsi assets/make-icons.fsx
//
// It needs only .NET (it writes the .icns and .ico itself), so it runs on macOS, Windows or Linux.

#r "nuget: Svg.Skia, 5.2.3"

open System.IO
open SkiaSharp

let folder = __SOURCE_DIRECTORY__
let source = Path.Combine(folder, "icon_vector.svg")

let svg = new Svg.Skia.SKSvg()
svg.Load source |> ignore
let picture = svg.Picture |> Option.ofObj |> Option.defaultWith (fun () -> failwith $"Couldn't read {source}")
let bounds = picture.CullRect

/// The artwork drawn at size pixels, square: filling inset..size-inset (in 1024ths), with Apple's
/// shadow if asked.
let draw (size: int) (inset: float32) (shadow: bool) : SKImage =
    use surface = SKSurface.Create(SKImageInfo(size, size, SKColorType.Rgba8888, SKAlphaType.Premul))
    let canvas = surface.Canvas
    canvas.Clear SKColors.Transparent
    let scale = float32 size / 1024f
    let side = float32 size - 2f * inset * scale
    use paint = new SKPaint(IsAntialias = true)
    if shadow then
        // Apple's template: black at 30%, 10 below, blurred 10 (in 1024ths).
        paint.ImageFilter <- SKImageFilter.CreateDropShadow(0f, 10f * scale, 10f * scale, 10f * scale, SKColor(0uy, 0uy, 0uy, 77uy))
    canvas.SaveLayer paint |> ignore
    canvas.Translate(inset * scale, inset * scale)
    canvas.Scale(side / bounds.Width, side / bounds.Height)
    canvas.Translate(-bounds.Left, -bounds.Top)
    canvas.DrawPicture picture
    canvas.Restore()
    surface.Snapshot()

let png (image: SKImage) =
    use data = image.Encode(SKEncodedImageFormat.Png, 100)
    data.ToArray()

/// Apple's run-length packing of one channel: a run of 3 to 130 of a byte is 0x80 + (n - 3) then
/// the byte; anything else goes as up to 128 bytes, after n - 1.
let packChannel (bytes: byte array) =
    let packed = ResizeArray<byte>()
    let mutable i = 0
    while i < bytes.Length do
        let mutable run = 1
        while i + run < bytes.Length && run < 130 && bytes[i + run] = bytes[i] do
            run <- run + 1
        if run >= 3 then
            packed.Add(byte (0x80 + run - 3))
            packed.Add bytes[i]
            i <- i + run
        else
            // Literal bytes, up to where a run of 3 starts.
            let start = i
            let mutable n = 0
            while i < bytes.Length && n < 128
                  && not (i + 2 < bytes.Length && bytes[i] = bytes[i + 1] && bytes[i] = bytes[i + 2]) do
                i <- i + 1
                n <- n + 1
            packed.Add(byte (n - 1))
            packed.AddRange bytes[start .. start + n - 1]
    packed.ToArray()

/// The small sizes' format (ic04, ic05): "ARGB", then each channel packed, alpha first, the colours
/// premultiplied by it (as macOS reads them). (macOS reads these two sizes' PNGs wrong.)
let argb (image: SKImage) =
    let size = image.Width
    use bitmap = new SKBitmap(SKImageInfo(size, size, SKColorType.Rgba8888, SKAlphaType.Premul))
    image.ReadPixels(bitmap.Info, bitmap.GetPixels(), bitmap.RowBytes, 0, 0) |> ignore
    let pixels = bitmap.Bytes
    let channel offset = Array.init (size * size) (fun p -> pixels[p * 4 + offset])
    Array.concat [ System.Text.Encoding.ASCII.GetBytes "ARGB"; packChannel (channel 3); packChannel (channel 0); packChannel (channel 1); packChannel (channel 2) ]

let macOS size = draw size 100f true
let windows size = png (draw size 0f false)

let bigEndian (value: int) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// An .icns: its PNGs, by the type each size goes in.
let icns (entries: (string * byte array) list) =
    let body =
        [| for kind, png in entries do
               yield! System.Text.Encoding.ASCII.GetBytes kind
               yield! bigEndian (png.Length + 8)
               yield! png |]
    Array.concat [ System.Text.Encoding.ASCII.GetBytes "icns"; bigEndian (body.Length + 8); body ]

/// An .ico: its PNGs, by their size.
let ico (entries: (int * byte array) list) =
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    writer.Write 0us
    writer.Write 1us
    writer.Write(uint16 entries.Length)
    let mutable offset = 6 + 16 * entries.Length
    for size, png in entries do
        writer.Write(byte (if size >= 256 then 0 else size))
        writer.Write(byte (if size >= 256 then 0 else size))
        writer.Write 0uy
        writer.Write 0uy
        writer.Write 1us
        writer.Write 32us
        writer.Write(uint32 png.Length)
        writer.Write(uint32 offset)
        offset <- offset + png.Length
    for _, png in entries do
        writer.Write png
    writer.Flush()
    stream.ToArray()

let write name (bytes: byte array) =
    let path = Path.Combine(folder, name)
    File.WriteAllBytes(path, bytes)
    printfn $"Wrote {path} ({bytes.Length:N0} bytes)"

write
    "Legibyl.icns"
    (icns [
        "ic04", argb (macOS 16)
        "ic11", png (macOS 32) // 16@2x
        "ic05", argb (macOS 32)
        "ic12", png (macOS 64) // 32@2x
        "ic07", png (macOS 128)
        "ic13", png (macOS 256) // 128@2x
        "ic08", png (macOS 256)
        "ic14", png (macOS 512) // 256@2x
        "ic09", png (macOS 512)
        "ic10", png (macOS 1024) // 512@2x
    ])

write "Legibyl.ico" (ico [ for size in [ 16; 20; 24; 32; 40; 48; 64; 256 ] -> size, windows size ])

write "Legibyl.png" (windows 256)
