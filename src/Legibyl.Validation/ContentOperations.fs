/// A page's content stream as its operators, with where each is in the bytes, so a repair can
/// copy the stream with its marked content added or changed (see LayoutTagging, and
/// ElementEdits' Not Read).
module internal Legibyl.Validation.ContentOperations

open System.Collections.Generic
open iText.IO.Source
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Util

type Operation =
    { Name: string
      /// The operator's bytes in the original stream, operands included.
      Start: int64
      End: int64 }

/// Splits a content stream into its operators, the way PdfCanvasProcessor reads it (so the
/// indexes line up), keeping each one's position in the bytes; and the stream's length.
let operationsOf (bytes: byte array) (resources: PdfResources) =
    use tokenizer = new PdfTokenizer(RandomAccessFileOrArray(RandomAccessSourceFactory().CreateSource bytes))
    let parser = PdfCanvasParser(tokenizer, resources)
    let operands = List<PdfObject>()
    let operations = List<Operation>()
    let mutable start = 0L
    parser.Parse operands |> ignore
    while operands.Count > 0 do
        let name = string operands[operands.Count - 1]
        if name = "BI" then
            // The inline image's dictionary and data, through EI.
            InlineImageParsingUtils.Parse(parser, resources.GetResource PdfName.ColorSpace) |> ignore
        operations.Add { Name = name; Start = start; End = tokenizer.GetPosition() }
        start <- tokenizer.GetPosition()
        parser.Parse operands |> ignore
    operations, bytes.LongLength

/// Path construction, and clipping, before a path is painted.
let pathConstruction = set [ "m"; "l"; "c"; "v"; "y"; "h"; "re"; "W"; "W*" ]

/// Path painting (and "n", ending a path without painting it).
let pathPainting = set [ "S"; "s"; "f"; "F"; "f*"; "B"; "B*"; "b"; "b*"; "n" ]

/// Operators that show text.
let textShowing = set [ "Tj"; "TJ"; "'"; "\"" ]

/// Operators a marked-content sequence mustn't straddle.
let boundaries = set [ "BT"; "ET"; "q"; "Q"; "BMC"; "BDC"; "EMC"; "BX"; "EX" ]
