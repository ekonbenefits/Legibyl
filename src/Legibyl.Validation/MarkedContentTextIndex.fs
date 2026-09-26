/// Builds a (page number, MCID) -> visible text index for a PDF using PdfPig, so that
/// veraPDF's structure tree walk (VeraPdfValidator.ReadStructureTree) can resolve each
/// structure element's directly-owned marked content back to the actual text a screen
/// reader would read there. This is intentionally a separate, independent parse of the
/// file (PdfPig, not veraPDF/IKVM) — text extraction and PDF/UA structural validation
/// are unrelated concerns, and PdfPig already exposes marked-content/MCID data directly
/// (Page.GetMarkedContents) that veraPDF's PD model does not.
module internal Legibyl.Validation.MarkedContentTextIndex

open System
open System.Collections.Generic
open System.Text
open UglyToad.PdfPig
open UglyToad.PdfPig.Content
open UglyToad.PdfPig.Core

// PdfPig 0.1.16 marks IPdfImage.Bounds obsolete in favor of BoundingBox, but the
// public IPdfImage interface in this version only exposes Bounds.
#nowarn "44"

type MarkedContentInfo = { Text: string; Geometry: StructureGeometry option }

type Index = Dictionary<int * int, MarkedContentInfo>

let private geometry pageNumber left bottom right top =
    { PageNumber = pageNumber; Left = left; Bottom = bottom; Right = right; Top = top }

let private unionGeometry (geometries: StructureGeometry list) =
    match geometries with
    | [] -> None
    | first :: _ ->
        Some
            { PageNumber = first.PageNumber
              Left = geometries |> List.map _.Left |> List.min
              Bottom = geometries |> List.map _.Bottom |> List.min
              Right = geometries |> List.map _.Right |> List.max
              Top = geometries |> List.map _.Top |> List.max }

let private hasArea (rect: PdfRectangle) = rect.Width > 0.01 && rect.Height > 0.01

/// A box for a glyph whose tight box is empty (whitespace, e.g. the lone space Word writes
/// for an empty paragraph): the font's line-height box, or failing that a one-character
/// box synthesized from the baseline and point size.
let private whitespaceRectangle (letter: Letter) =
    if hasArea letter.GlyphRectangleLoose then
        letter.GlyphRectangleLoose
    else
        let size = if letter.PointSize > 0.0 then letter.PointSize else 12.0
        let x = letter.StartBaseLine.X
        let y = letter.StartBaseLine.Y
        PdfRectangle(x, y - 0.2 * size, x + max letter.Width (0.5 * size), y + 0.8 * size)

let private geometryFromLetters pageNumber (letters: IReadOnlyList<Letter>) =
    if letters.Count = 0 then
        None
    else
        // Only fall back to whitespace boxes when nothing visible was drawn, so ordinary
        // text keeps its tight box.
        let rects =
            match letters |> Seq.map _.BoundingBox |> Seq.filter hasArea |> List.ofSeq with
            | [] -> letters |> Seq.map whitespaceRectangle |> List.ofSeq
            | visible -> visible
        Some(
            geometry
                pageNumber
                (rects |> List.map (fun r -> min r.Left r.Right) |> List.min)
                (rects |> List.map (fun r -> min r.Bottom r.Top) |> List.min)
                (rects |> List.map (fun r -> max r.Left r.Right) |> List.max)
                (rects |> List.map (fun r -> max r.Bottom r.Top) |> List.max)
        )

let private geometryFromMarkedContent pageNumber (element: MarkedContentElement) =
    let images =
        [ for image in element.Images ->
              geometry pageNumber image.Bounds.Left image.Bounds.Bottom image.Bounds.Right image.Bounds.Top ]

    let paths =
        [ for path in element.Paths do
              // Clipping paths (e.g. "re W n") and unpainted paths aren't visible content.
              let painted = (path.IsFilled || path.IsStroked) && not path.IsClipping
              let bounds = if painted then path.GetBoundingRectangle() else Nullable()
              if bounds.HasValue then
                  let b = bounds.Value
                  geometry pageNumber b.Left b.Bottom b.Right b.Top ]

    unionGeometry (Option.toList (geometryFromLetters pageNumber element.Letters) @ images @ paths)

/// Joins a marked content sequence's glyphs into readable text, in the order they
/// were painted. Inserts a space wherever there's a noticeable horizontal gap
/// between consecutive glyphs that isn't already whitespace — many PDF writers
/// position words apart via spacing operators rather than an explicit space glyph,
/// so naive concatenation alone would run words together — and where the text moves
/// to a new line, which ends a word just the same ("Arrow of" / "Light").
let private concatenateLetters (letters: IReadOnlyList<Letter>) =
    let sb = StringBuilder()
    let mutable previous: Letter option = None
    for letter in letters |> Seq.sortBy _.TextSequence do
        match previous with
        | Some previous when letter.Value.Length > 0 && not (Char.IsWhiteSpace letter.Value[0]) ->
            let previousEndsWithSpace =
                previous.Value.Length > 0 && Char.IsWhiteSpace previous.Value[previous.Value.Length - 1]
            let gap = letter.StartBaseLine.X - previous.EndBaseLine.X
            let lineHeight = max previous.PointSize previous.GlyphRectangle.Height
            let newLine = abs (letter.StartBaseLine.Y - previous.StartBaseLine.Y) > lineHeight * 0.5
            if not previousEndsWithSpace && (gap > previous.Width * 0.15 || newLine) then
                sb.Append ' ' |> ignore
        | _ -> ()
        sb.Append letter.Value |> ignore
        previous <- Some letter
    sb.ToString()

let rec private indexMarkedContent pageNumber (elements: IReadOnlyList<MarkedContentElement>) (index: Index) =
    for element in elements do
        let text = concatenateLetters element.Letters
        let geometry = geometryFromMarkedContent pageNumber element
        if not (String.IsNullOrEmpty text) || geometry.IsSome then
            index[(pageNumber, element.MarkedContentIdentifier)] <- { Text = text; Geometry = geometry }
        if element.Children.Count > 0 then
            indexMarkedContent pageNumber element.Children index

/// Builds the index, calling onPage (page number, page count) as each page starts.
let buildReporting (pdfPath: string) (onPage: int -> int -> unit) : Index =
    let index = Index()
    use document = PdfDocument.Open pdfPath
    for page in document.GetPages() do
        onPage page.Number document.NumberOfPages
        indexMarkedContent page.Number (page.GetMarkedContents()) index
    index

let build (pdfPath: string) : Index = buildReporting pdfPath (fun _ _ -> ())
