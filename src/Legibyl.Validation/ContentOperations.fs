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
      /// Its operands, in order (the operator itself left out).
      Operands: PdfObject array
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
        operations.Add
            { Name = name
              Operands = [| for i in 0 .. operands.Count - 2 -> operands[i] |]
              Start = start
              End = tokenizer.GetPosition() }
        start <- tokenizer.GetPosition()
        parser.Parse operands |> ignore
    operations, bytes.LongLength

/// Path construction, and clipping, before a path is painted.
let pathConstruction = set [ "m"; "l"; "c"; "v"; "y"; "h"; "re"; "W"; "W*" ]

/// Path painting (and "n", ending a path without painting it).
let pathPainting = set [ "S"; "s"; "f"; "F"; "f*"; "B"; "B*"; "b"; "b*"; "n" ]

/// Operators that show text.
let textShowing = set [ "Tj"; "TJ"; "'"; "\"" ]

/// A marked-content operator's MCID (BDC's properties, inline or in the resources' /Properties).
let mcidOf (resources: PdfResources) (operation: Operation) : int option =
    if operation.Name <> "BDC" || operation.Operands.Length < 2 then
        None
    else
        let properties: PdfDictionary option =
            match operation.Operands[1] with
            | :? PdfDictionary as properties -> Some properties
            | :? PdfName as name ->
                match resources.GetResource PdfName.Properties with
                | null -> None
                | all -> Option.ofObj (all.GetAsDictionary name)
            | _ -> None
        properties |> Option.bind (fun properties -> properties.GetAsNumber PdfName.MCID |> Option.ofObj |> Option.map _.IntValue())

/// Operators a marked-content sequence mustn't straddle.
let boundaries = set [ "BT"; "ET"; "q"; "Q"; "BMC"; "BDC"; "EMC"; "BX"; "EX" ]
