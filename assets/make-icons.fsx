// Makes Legibyl's icons from icon_glass.icon, made in Apple's Icon Composer (its one layer is
// icon_vector.svg, exported from icon_vector.af), with Apple's own tools:
//
//   Assets.car     macOS 26 and later: the icon as Icon Composer made it (Liquid Glass, with its
//                  dark and tinted looks), compiled by actool; the app's Info.plist names it
//                  (CFBundleIconName icon_glass)
//   Legibyl.icns   macOS before 26: the icon on Apple's icon grid (824 of 1024, centred, with a
//                  soft shadow below), 16 to 1024 pixels
//   Legibyl.ico    Windows: the icon to the edge, 16 to 256 pixels
//   Legibyl.png    the About window's, 256 pixels, as Windows'
//
// Every size is rendered by Icon Composer's ictool at that size, rather than scaled from a bitmap;
// up to 32 pixels from icon_glass_small.icon instead, a simpler drawing, if there is one.
// After changing the icon, run from the repository:
//
//   dotnet fsi assets/make-icons.fsx
//
// It needs a Mac with Xcode 26 or later (for actool and ictool); ictool also comes with the
// Icon Composer app on its own.

#r "nuget: SkiaSharp, 3.119.4"

open System
open System.Diagnostics
open System.IO
open SkiaSharp

let folder = __SOURCE_DIRECTORY__
let source = Path.Combine(folder, "icon_glass.icon")
/// The icon's name in Assets.car: its file's (Info.plist's CFBundleIconName).
let iconName = Path.GetFileNameWithoutExtension source

/// A simpler drawing for the small sizes, if there is one: detail that reads at 256 pixels is
/// noise at 16. Used up to SmallUpTo pixels (Windows' 16 to 32, and the .icns's 16 and 32); the
/// full icon above, and in Assets.car, which macOS scales itself.
let small = Path.Combine(folder, "icon_glass_small.icon")

[<Literal>]
let SmallUpTo = 32

/// Runs a program, failing with what it said if it fails.
let run (program: string) (arguments: string list) =
    let info = ProcessStartInfo(program, RedirectStandardOutput = true, RedirectStandardError = true)
    for argument in arguments do
        info.ArgumentList.Add argument
    use proc = Process.Start info
    let output = proc.StandardOutput.ReadToEnd()
    let error = proc.StandardError.ReadToEnd()
    proc.WaitForExit()
    if proc.ExitCode <> 0 then failwith $"{program} failed ({proc.ExitCode}): {error}{output}"
    output

if not (OperatingSystem.IsMacOS()) then failwith "Making the icons needs a Mac: Apple's actool and ictool make them."

/// The selected Xcode's Developer folder (xcode-select -p).
let developer = (run "xcode-select" [ "-p" ]).Trim()

let actool =
    try (run "xcrun" [ "--find"; "actool" ]).Trim()
    with _ -> failwith "Couldn't find actool: it comes with Xcode 26 or later."

/// Icon Composer's ictool: the one in the selected Xcode's copy of the app, else the app's own. (The
/// ictool xcrun finds, Xcode's command-line one, can't render previews.)
let ictool =
    [ Path.Combine(developer, "..", "Applications", "Icon Composer.app", "Contents", "Executables", "ictool")
      "/Applications/Icon Composer.app/Contents/Executables/ictool" ]
    |> List.map Path.GetFullPath
    |> List.tryFind File.Exists
    |> Option.defaultWith (fun () -> failwith "Couldn't find Icon Composer's ictool: it comes with Xcode 26 or later, and with the Icon Composer app.")

let work = Directory.CreateTempSubdirectory "legibyl-icons"

/// The icon as macOS shows it, size pixels square, to the edge (its rounded square fills it): the
/// simpler drawing at the small sizes, if there is one.
let render (size: int) : SKImage =
    let path = Path.Combine(work.FullName, $"{size}.png")
    let icon = if size <= SmallUpTo && Directory.Exists small then small else source
    // Platform, appearance, width, height, scale, and tilt (none).
    run ictool [ icon; "--export-preview"; "macOS"; "Light"; string size; string size; "1"; "0"; path ] |> ignore
    SKImage.FromEncodedData path |> Option.ofObj |> Option.defaultWith (fun () -> failwith $"ictool wrote no image for {size} pixels")

/// The icon on Apple's icon grid at size pixels: rendered at 824 of 1024 of it, centred, with the
/// template's shadow (black at 30%, 10 below, blurred 10, in 1024ths).
let onGrid (size: int) : SKImage =
    let scale = float32 size / 1024f
    let inner = max 1 (int (MathF.Round(824f * scale)))
    use artwork = render inner
    use surface = SKSurface.Create(SKImageInfo(size, size, SKColorType.Rgba8888, SKAlphaType.Premul))
    let canvas = surface.Canvas
    canvas.Clear SKColors.Transparent
    use paint = new SKPaint(IsAntialias = true)
    paint.ImageFilter <- SKImageFilter.CreateDropShadow(0f, 10f * scale, 10f * scale, 10f * scale, SKColor(0uy, 0uy, 0uy, 77uy))
    let offset = float32 (size - inner) / 2f
    canvas.DrawImage(artwork, offset, offset, paint)
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
    Array.concat [ Text.Encoding.ASCII.GetBytes "ARGB"; packChannel (channel 3); packChannel (channel 0); packChannel (channel 1); packChannel (channel 2) ]

let windows size = png (render size)

let bigEndian (value: int) = [| byte (value >>> 24); byte (value >>> 16); byte (value >>> 8); byte value |]

/// An .icns: its images, by the type each size goes in.
let icns (entries: (string * byte array) list) =
    let body =
        [| for kind, image in entries do
               yield! Text.Encoding.ASCII.GetBytes kind
               yield! bigEndian (image.Length + 8)
               yield! image |]
    Array.concat [ Text.Encoding.ASCII.GetBytes "icns"; bigEndian (body.Length + 8); body ]

/// An .ico: its PNGs, by their size.
let ico (entries: (int * byte array) list) =
    use stream = new MemoryStream()
    use writer = new BinaryWriter(stream)
    writer.Write 0us
    writer.Write 1us
    writer.Write(uint16 entries.Length)
    let mutable offset = 6 + 16 * entries.Length
    for size, image in entries do
        writer.Write(byte (if size >= 256 then 0 else size))
        writer.Write(byte (if size >= 256 then 0 else size))
        writer.Write 0uy
        writer.Write 0uy
        writer.Write 1us
        writer.Write 32us
        writer.Write(uint32 image.Length)
        writer.Write(uint32 offset)
        offset <- offset + image.Length
    for _, image in entries do
        writer.Write image
    writer.Flush()
    stream.ToArray()

let write name (bytes: byte array) =
    let path = Path.Combine(folder, name)
    File.WriteAllBytes(path, bytes)
    printfn $"Wrote {path} ({bytes.Length:N0} bytes)"

try
    // Assets.car, as Xcode builds it for an app whose icon is the .icon (its .icns isn't kept: it
    // stops at 256 pixels, so Legibyl.icns, below, is the one before macOS 26).
    let compiled = Directory.CreateDirectory(Path.Combine(work.FullName, "compiled"))
    run actool [ source; "--compile"; compiled.FullName; "--app-icon"; iconName; "--enable-on-demand-resources"; "NO"
                 "--development-region"; "en"; "--target-device"; "mac"; "--platform"; "macosx"
                 "--minimum-deployment-target"; "12.0"
                 "--output-partial-info-plist"; Path.Combine(work.FullName, "partial.plist") ]
    |> ignore
    write "Assets.car" (File.ReadAllBytes(Path.Combine(compiled.FullName, "Assets.car")))

    write
        "Legibyl.icns"
        (icns [
            "ic04", argb (onGrid 16)
            "ic11", png (onGrid 32) // 16@2x
            "ic05", argb (onGrid 32)
            "ic12", png (onGrid 64) // 32@2x
            "ic07", png (onGrid 128)
            "ic13", png (onGrid 256) // 128@2x
            "ic08", png (onGrid 256)
            "ic14", png (onGrid 512) // 256@2x
            "ic09", png (onGrid 512)
            "ic10", png (onGrid 1024) // 512@2x
        ])

    write "Legibyl.ico" (ico [ for size in [ 16; 20; 24; 32; 40; 48; 64; 256 ] -> size, windows size ])

    write "Legibyl.png" (windows 256)
finally
    work.Delete true
