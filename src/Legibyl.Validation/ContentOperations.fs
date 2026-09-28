/// A page's content stream as its operators, with where each is in the bytes, so a fix can
/// copy the stream with its marked content added or changed (see LayoutTagging, and
/// ElementEdits' Not Read).
module internal Legibyl.Validation.ContentOperations

open System.Collections.Generic
open iText.IO.Source
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Pdf.Canvas.Parser.Listener
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
        // (An inline image, BI … ID … EI, comes as one operation: see inlineImage.)
        let name = string operands[operands.Count - 1]
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

/// An inline image (BI, its dictionary, ID, its data, EI): iText's parser reads it as one
/// operation, by its last operator.
[<Literal>]
let inlineImage = "EI"

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

// ---- Where each piece of content is drawn ----

type ContentKind =
    | Text
    | Image
    | Path

/// Something an operator draws, and its bounding box in user space.
type Drawn = { Kind: ContentKind; Box: Rectangle }

let private boxOf (points: (float32 * float32) seq) =
    let points = Array.ofSeq points
    if points.Length = 0 then
        None
    else
        let xs = points |> Array.map fst
        let ys = points |> Array.map snd
        let left, bottom = Array.min xs, Array.min ys
        Some(Rectangle(left, bottom, Array.max xs - left, Array.max ys - bottom))

let private transform (matrix: Matrix) (x: float32, y: float32) =
    let v = Vector(x, y, 1f).Cross matrix
    v.Get Vector.I1, v.Get Vector.I2

/// Records what each top-level operator of a content stream draws. Content inside a form
/// XObject counts as its Do operator's.
type DrawnByOperator() =
    let drawn = Dictionary<int, List<Drawn>>()
    let mutable current = -1

    member val Depth = 0 with get, set
    member _.Current with set value = current <- value
    member _.Drawn = drawn

    member private _.Add(kind, box: Rectangle option) =
        match box with
        | Some box when current >= 0 ->
            match drawn.TryGetValue current with
            | true, list -> list.Add { Kind = kind; Box = box }
            | _ -> drawn[current] <- List [ { Kind = kind; Box = box } ]
        | _ -> ()

    interface IEventListener with
        member this.EventOccurred(data, eventType) =
            match eventType, data with
            | EventType.RENDER_TEXT, (:? TextRenderInfo as text) ->
                let points =
                    [ for line in [ text.GetAscentLine(); text.GetDescentLine() ] do
                          for point in [ line.GetStartPoint(); line.GetEndPoint() ] -> point.Get Vector.I1, point.Get Vector.I2 ]
                this.Add(Text, boxOf points)
            | EventType.RENDER_IMAGE, (:? ImageRenderInfo as image) ->
                let ctm = image.GetImageCtm()
                this.Add(Image, boxOf [ for corner in [ 0f, 0f; 1f, 0f; 0f, 1f; 1f, 1f ] -> transform ctm corner ])
            | EventType.RENDER_PATH, (:? PathRenderInfo as path) when path.GetOperation() <> PathRenderInfo.NO_OP ->
                let ctm = path.GetCtm()
                let points =
                    [ for subpath in path.GetPath().GetSubpaths() do
                          for segment in subpath.GetSegments() do
                              for point in segment.GetBasePoints() -> transform ctm (float32 (point.GetX()), float32 (point.GetY())) ]
                this.Add(Path, boxOf points)
            | _ -> ()

        member _.GetSupportedEvents() = null

/// Wraps an operator so the recorder knows which top-level operator is running.
type private CountingOperator(inner: IContentOperator, recorder: DrawnByOperator, next: unit -> int) =
    interface IContentOperator with
        member _.Invoke(processor, operator, operands) =
            if recorder.Depth = 0 then
                recorder.Current <- next ()
            recorder.Depth <- recorder.Depth + 1
            try
                inner.Invoke(processor, operator, operands)
            finally
                recorder.Depth <- recorder.Depth - 1

/// What each operator of a content stream draws, by its index (as operationsOf numbers them).
let drawnByOperator (bytes: byte array) (resources: PdfResources) =
    let recorder = DrawnByOperator()
    let processor = PdfCanvasProcessor(recorder)
    let mutable index = -1
    let next () =
        index <- index + 1
        index
    for name in List.ofSeq (processor.GetRegisteredOperatorStrings()) do
        match processor.RegisterContentOperator(name, null) with
        | null -> ()
        | inner -> processor.RegisterContentOperator(name, CountingOperator(inner, recorder, next)) |> ignore
    processor.ProcessContent(bytes, resources)
    recorder.Drawn
