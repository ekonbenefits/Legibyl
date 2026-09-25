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

let private geometryFromLetters pageNumber (letters: IReadOnlyList<Letter>) =
    if letters.Count = 0 then
        None
    else
        Some(
            geometry
                pageNumber
                (letters |> Seq.map _.BoundingBox.Left |> Seq.min)
                (letters |> Seq.map _.BoundingBox.Bottom |> Seq.min)
                (letters |> Seq.map _.BoundingBox.Right |> Seq.max)
                (letters |> Seq.map _.BoundingBox.Top |> Seq.max)
        )

let private geometryFromMarkedContent pageNumber (element: MarkedContentElement) =
    let images =
        [ for image in element.Images ->
              geometry pageNumber image.Bounds.Left image.Bounds.Bottom image.Bounds.Right image.Bounds.Top ]

    let paths =
        [ for path in element.Paths do
              let bounds = path.GetBoundingRectangle()
              if bounds.HasValue then
                  let b = bounds.Value
                  geometry pageNumber b.Left b.Bottom b.Right b.Top ]

    unionGeometry (Option.toList (geometryFromLetters pageNumber element.Letters) @ images @ paths)

/// Joins a marked content sequence's glyphs into readable text, in the order they
/// were painted. Inserts a space wherever there's a noticeable horizontal gap
/// between consecutive glyphs that isn't already whitespace — many PDF writers
/// position words apart via spacing operators rather than an explicit space glyph,
/// so naive concatenation alone would run words together.
let private concatenateLetters (letters: IReadOnlyList<Letter>) =
    let sb = StringBuilder()
    let mutable previous: Letter option = None
    for letter in letters |> Seq.sortBy _.TextSequence do
        match previous with
        | Some previous when letter.Value.Length > 0 && not (Char.IsWhiteSpace letter.Value[0]) ->
            let previousEndsWithSpace =
                previous.Value.Length > 0 && Char.IsWhiteSpace previous.Value[previous.Value.Length - 1]
            let gap = letter.StartBaseLine.X - previous.EndBaseLine.X
            if not previousEndsWithSpace && gap > previous.Width * 0.15 then
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

let build (pdfPath: string) : Index =
    let index = Index()
    use document = PdfDocument.Open pdfPath
    for page in document.GetPages() do
        indexMarkedContent page.Number (page.GetMarkedContents()) index
    index
