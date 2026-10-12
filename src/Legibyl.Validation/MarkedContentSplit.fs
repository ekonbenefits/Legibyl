/// Splitting the end off a piece of marked content into a piece of its own, the page looking the
/// same: "diversification." drawn as one piece becomes "diversification" and "." (see
/// UnlinkedUrls, which links an address without the full stop after it).
///
/// Only the end of the last text the piece draws, with nothing drawn after it, and nothing tagged
/// (with an MCID of its own) inside it. The text-showing operator is cut in two between glyphs.
/// Whatever is open inside the piece at the cut (a layer's marked content, a text object) is
/// ended before the piece ends, and begun again inside the new one, the same: the text state
/// lasts past ET, and the text matrix is set where the end was drawn. A piece begun inside a text
/// object ends and the new one begins between the two halves. Marked as an artifact at the cut
/// isn't split: the end would read as the artifact does.
module internal Legibyl.Validation.MarkedContentSplit

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Canvas.Parser
open iText.Kernel.Pdf.Canvas.Parser.Data
open iText.Kernel.Pdf.Canvas.Parser.Listener
open Legibyl.Validation.ContentOperations

/// A glyph a text-showing operator draws: its bytes, its text, and the text matrix it's drawn at.
type private Glyph = { Bytes: byte array; Text: string; Matrix: Matrix }

/// The glyphs each top-level operator draws, by its index (as operationsOf numbers them): one
/// array per string it shows.
type private Shown() =
    let shown = Dictionary<int, List<Glyph array>>()
    member val Depth = 0 with get, set
    member val Current = -1 with get, set
    member _.Shown = shown

    interface IEventListener with
        member this.EventOccurred(data, eventType) =
            match eventType, data with
            // Only the page's own operators' text (not a form's, which is its Do's).
            | EventType.RENDER_TEXT, (:? TextRenderInfo as text) when this.Depth = 1 && this.Current >= 0 ->
                let glyphs =
                    [| for glyph in text.GetCharacterRenderInfos() ->
                           { Bytes = glyph.GetPdfString().GetValueBytes()
                             Text = glyph.GetText()
                             Matrix = glyph.GetTextMatrix() } |]
                match shown.TryGetValue this.Current with
                | true, list -> list.Add glyphs
                | _ -> shown[this.Current] <- List [ glyphs ]
            | _ -> ()

        member _.GetSupportedEvents() = null

type private Counting(inner: IContentOperator, shown: Shown, next: unit -> int) =
    interface IContentOperator with
        member _.Invoke(processor, operator, operands) =
            if shown.Depth = 0 then shown.Current <- next ()
            shown.Depth <- shown.Depth + 1
            try inner.Invoke(processor, operator, operands)
            finally shown.Depth <- shown.Depth - 1

let private shownBy (bytes: byte array) (resources: PdfResources) =
    let shown = Shown()
    let processor = PdfCanvasProcessor(shown)
    let mutable index = -1
    let next () =
        index <- index + 1
        index
    for name in List.ofSeq (processor.GetRegisteredOperatorStrings()) do
        match processor.RegisterContentOperator(name, null) with
        | null -> ()
        | inner -> processor.RegisterContentOperator(name, Counting(inner, shown, next)) |> ignore
    processor.ProcessContent(bytes, resources)
    shown.Shown

/// What's open inside the piece where it's cut: a marked-content sequence (its BDC or BMC), or a
/// text object.
type Frame =
    private
    | Marked of Operation
    | TextObject

/// What a split takes: the page's content and where to cut it.
type Plan =
    private
        { Bytes: byte array
          /// The operator drawing the end, and the glyphs it draws before the cut and from it.
          Operation: Operation
          Head: PdfObject list
          Tail: PdfObject list
          /// What's open inside the piece at the cut, outermost first: ended before the piece
          /// ends, and begun again inside the new one.
          Inside: Frame list
          /// Where the end is drawn (its first glyph's text matrix).
          At: Matrix
          Tag: PdfName
          Properties: PdfDictionary
          Mcid: int }

let private blank (s: string) = String.IsNullOrWhiteSpace s
let private unspaced (s: string) = String.Concat(s |> Seq.filter (not << Char.IsWhiteSpace))

/// Painting operators: what's drawn after the end would be in the new piece, or out of its place.
let private paints (operation: Operation) =
    textShowing.Contains operation.Name
    || operation.Name = ContentOperators.Do
    || operation.Name = inlineImage
    || operation.Name = ContentOperators.sh
    || (pathPainting.Contains operation.Name && operation.Name <> ContentOperators.n)

/// Whether value is, or holds at any depth, an indirect reference: one a page's content can't
/// write inline (properties named in the page's resources can hold them).
let rec private holdsReference (value: PdfObject | null) =
    match value with
    | null -> false
    | value when value.IsIndirectReference() -> true
    | :? PdfDictionary as dictionary -> dictionary.KeySet() |> Seq.exists (fun key -> holdsReference (dictionary.Get(key, false)))
    | :? PdfArray as array -> seq { 0 .. array.Size() - 1 } |> Seq.exists (fun i -> holdsReference (array.Get(i, false)))
    | _ -> false

/// How the end (tail, whitespace aside) of the piece of marked content mcid on the page would
/// be split off, numbered past usedMcids (those the tags give the page); or why it can't be.
let plan (page: PdfPage) (mcid: int) (tail: string) (usedMcids: int seq) : Result<Plan, string> =
    try
        let bytes = page.GetContentBytes()
        let resources = page.GetResources()
        let operations, _ = operationsOf bytes resources
        let begins = [ for i in 0 .. operations.Count - 1 do if mcidOf resources operations[i] = Some mcid then i ]
        match begins with
        | [ first ] ->
            // Its end: the EMC closing it.
            let mutable depth = 0
            let mutable last = -1
            let mutable tagged = false
            let mutable i = first + 1
            while last < 0 && i < operations.Count do
                let name = operations[i].Name
                if name = ContentOperators.BDC || name = ContentOperators.BMC then
                    if (mcidOf resources operations[i]).IsSome then tagged <- true
                    depth <- depth + 1
                elif name = ContentOperators.EMC then
                    if depth = 0 then last <- i else depth <- depth - 1
                i <- i + 1
            let isBT (op: Operation) = op.Name = ContentOperators.BT
            let inText =
                operations
                |> Seq.take first
                |> Seq.fold (fun open' op -> if isBT op then true elif op.Name = ContentOperators.ET then false else open') false
            // What's open inside the piece before operator at, outermost first.
            let insideAt at =
                let mutable frames = []
                for k in first + 1 .. at - 1 do
                    let op = operations[k]
                    if op.Name = ContentOperators.BDC || op.Name = ContentOperators.BMC then frames <- Marked op :: frames
                    elif isBT op then frames <- TextObject :: frames
                    elif op.Name = ContentOperators.EMC || op.Name = ContentOperators.ET then
                        match frames with
                        | _ :: rest -> frames <- rest
                        | [] -> ()
                List.rev frames
            let isArtifact frame =
                match frame with
                | Marked op -> op.Operands.Length > 0 && op.Operands[0] = (PdfName.Artifact :> PdfObject)
                | TextObject -> false
            let propertiesOf (op: Operation) =
                if op.Operands.Length < 2 then None
                else
                    match op.Operands[1] with
                    | :? PdfDictionary as properties -> Some properties
                    | :? PdfName as name ->
                        match resources.GetResource PdfName.Properties with
                        | null -> None
                        | all -> Option.ofObj (all.GetAsDictionary name)
                    | _ -> None
            let saysWhatItReadsAs (properties: PdfDictionary) =
                [ PdfName.ActualText; PdfName.Alt; PdfName.E ] |> List.exists properties.ContainsKey
            // Marked content open at the cut is begun again around the end: what it says it reads
            // as would be said twice.
            let readsAsSomethingElse frame =
                match frame with
                | Marked op when op.Name = ContentOperators.BDC ->
                    match propertiesOf op with
                    | Some properties -> saysWhatItReadsAs properties
                    | None -> true
                | _ -> false
            let tag = operations[first].Operands[0] :?> PdfName
            let properties = propertiesOf operations[first]
            let showing = [ for k in first + 1 .. last - 1 do if textShowing.Contains operations[k].Name then k ]
            if last < 0 then Error "its marked content isn't ended"
            elif tagged then Error "its marked content holds tagged marked content of its own"
            elif properties.IsNone then Error "its marked content's properties can't be read"
            elif saysWhatItReadsAs properties.Value then
                Error "its marked content says what it reads as, for all of it"
            elif properties.Value.KeySet() |> Seq.exists (fun key -> holdsReference (properties.Value.Get(key, false))) then
                Error "its marked content's properties can't be copied"
            elif showing.IsEmpty then Error "its marked content draws no text"
            else
                let at = List.last showing
                let operation = operations[at]
                let inside = insideAt at
                if [ for k in at + 1 .. last - 1 -> operations[k] ] |> List.exists paints then
                    Error "something is drawn after it, in the same piece"
                elif operation.Name <> ContentOperators.Tj && operation.Name <> ContentOperators.TJ then
                    Error "the text drawing it moves to a new line first, which the fix doesn't cut"
                // (Its text isn't the paragraph's, so not an address's: kept in case that changes.)
                elif inside |> List.exists isArtifact then
                    Error "it's marked as an artifact inside its piece"
                elif inside |> List.exists readsAsSomethingElse then
                    Error "marked content around it inside its piece says what it reads as, for all of it"
                elif inText = (inside |> List.exists (function TextObject -> true | Marked _ -> false)) then
                    Error "its text isn't drawn in the usual way"
                else
                    let shown = shownBy bytes resources
                    let strings =
                        match operation.Name, operation.Operands with
                        | n, [| :? PdfString as s |] when n = ContentOperators.Tj -> Some [ s :> PdfObject ]
                        | _, [| :? PdfArray as array |] -> Some [ for k in 0 .. array.Size() - 1 -> array.Get k ]
                        | _ -> None
                    let events = match shown.TryGetValue at with | true, list -> List.ofSeq list | _ -> []
                    // Each string the operator shows with the glyphs drawing it (each the same bytes).
                    let paired =
                        strings
                        |> Option.bind (fun items ->
                            let texts = items |> List.filter (fun item -> item :? PdfString)
                            let texts =
                                if texts.Length = events.Length then Some texts
                                else
                                    let filled = texts |> List.filter (fun item -> (item :?> PdfString).GetValueBytes().Length > 0)
                                    if filled.Length = events.Length then Some filled else None
                            texts
                            |> Option.bind (fun texts ->
                                let glyphsOf = Dictionary<PdfObject, Glyph array>(HashIdentity.Reference)
                                List.zip texts events |> List.iter (fun (item, glyphs) -> glyphsOf[item] <- glyphs)
                                let same =
                                    texts
                                    |> List.forall (fun item ->
                                        (item :?> PdfString).GetValueBytes() = Array.concat (glyphsOf[item] |> Array.map _.Bytes))
                                if same then Some(items, glyphsOf) else None))
                    match paired with
                    | None -> Error "the text drawing it can't be cut between its letters"
                    | Some(items, glyphsOf) ->
                        // Every glyph the operator draws, by the string it's in.
                        let all =
                            [ for itemIndex, item in List.indexed items do
                                  match glyphsOf.TryGetValue item with
                                  | true, glyphs -> for g in 0 .. glyphs.Length - 1 -> itemIndex, g, glyphs[g]
                                  | _ -> () ]
                        let wanted = unspaced tail
                        // From the last glyph back, the fewest that read as the end.
                        let rec back k (read: string) =
                            if k < 0 || read.Length >= wanted.Length then (if read = wanted then Some(k + 1) else None)
                            else
                                let (_, _, glyph) = all[k]
                                let read = unspaced glyph.Text + read
                                if blank glyph.Text then back (k - 1) read
                                elif wanted.EndsWith read then back (k - 1) read
                                else None
                        // (The first glyph of the end, not whitespace before it.)
                        let rec firstInk k = if k < all.Length && blank (let (_, _, g) = all[k] in g.Text) then firstInk (k + 1) else k
                        match back (all.Length - 1) "" |> Option.map firstInk with
                        | Some cut when wanted <> "" && cut < all.Length ->
                            let cutItem, cutGlyph, firstGlyph = all[cut]
                            let stringOf (glyphs: Glyph seq) =
                                let s = PdfString(Array.concat (glyphs |> Seq.map _.Bytes))
                                s.SetHexWriting true |> ignore
                                s :> PdfObject
                            let head =
                                [ for itemIndex, item in List.indexed items do
                                      if itemIndex < cutItem then item
                                      elif itemIndex = cutItem && cutGlyph > 0 then stringOf (glyphsOf[item][.. cutGlyph - 1]) ]
                            let tailItems =
                                [ for itemIndex, item in List.indexed items do
                                      if itemIndex = cutItem then stringOf (glyphsOf[item][cutGlyph ..])
                                      elif itemIndex > cutItem then item ]
                            // What it draws before the end: in this operator, or one before it.
                            let headDraws = head |> List.exists (fun item -> item :? PdfString && (item :?> PdfString).GetValueBytes().Length > 0)
                            if not headDraws && showing.Length = 1 then
                                Error "it's all the text its piece draws"
                            else
                                let mcids = seq { for op in operations do match mcidOf resources op with Some id -> id | None -> () }
                                let next = Seq.append mcids usedMcids |> Seq.fold max -1 |> (+) 1
                                Ok
                                    { Bytes = bytes
                                      Operation = operation
                                      Head = head
                                      Tail = tailItems
                                      Inside = inside
                                      At = firstGlyph.Matrix
                                      Tag = tag
                                      Properties = properties.Value
                                      Mcid = next }
                        | _ -> Error "it isn't the end of the text its piece draws"
        | [] -> Error "its marked content isn't in the page's own content"
        | _ -> Error "its marked content is drawn in more than one place"
    with ex -> Error $"the page's content couldn't be read safely ({ex.Message})"

let private written (value: PdfObject) =
    use stream = new MemoryStream()
    let output = new PdfOutputStream(stream)
    output.Write value |> ignore
    output.Flush()
    stream.ToArray()

/// Splits the end off as its own marked content, its tag and properties the same but its MCID, and
/// writes the page's content; its MCID, which nothing in the tags holds yet.
let split (document: PdfDocument) (page: PdfPage) (plan: Plan) =
    let number (value: float32) = (float value).ToString("0.######", CultureInfo.InvariantCulture)
    let showing (items: PdfObject list) =
        match items with
        | [ :? PdfString as s ] when plan.Operation.Name = ContentOperators.Tj -> Array.append (written s) (Encoding.ASCII.GetBytes " Tj\n")
        | items -> Array.append (written (PdfArray(ResizeArray items))) (Encoding.ASCII.GetBytes " TJ\n")
    let properties = plan.Properties.Clone() :?> PdfDictionary
    properties.Put(PdfName.MCID, PdfNumber plan.Mcid) |> ignore
    let opening = Array.concat [ written plan.Tag; Encoding.ASCII.GetBytes " "; written properties; Encoding.ASCII.GetBytes " BDC\n" ]
    let m = plan.At
    let at =
        String.Join(" ", [ m.Get Matrix.I11; m.Get Matrix.I12; m.Get Matrix.I21; m.Get Matrix.I22; m.Get Matrix.I31; m.Get Matrix.I32 ] |> List.map number)
    let ending frame =
        match frame with
        | Marked _ -> Encoding.ASCII.GetBytes "\nEMC\n"
        | TextObject -> Encoding.ASCII.GetBytes "\nET\n"
    let beginning frame =
        match frame with
        | Marked op -> Array.append plan.Bytes[int op.Start .. int op.End - 1] (Encoding.ASCII.GetBytes "\n")
        | TextObject -> Encoding.ASCII.GetBytes $"BT\n{at} Tm\n"
    let between =
        Array.concat
            [ yield! plan.Inside |> List.rev |> List.map ending
              Encoding.ASCII.GetBytes "\nEMC\n"
              opening
              yield! plan.Inside |> List.map beginning ]
    use output = new MemoryStream()
    let write (bytes: byte array) = output.Write(bytes, 0, bytes.Length)
    output.Write(plan.Bytes, 0, int plan.Operation.Start)
    write (Encoding.ASCII.GetBytes "\n")
    // (Spacing before the end, with no glyphs, still moves it.)
    if not plan.Head.IsEmpty then write (showing plan.Head)
    write between
    write (showing plan.Tail)
    output.Write(plan.Bytes, int plan.Operation.End, plan.Bytes.Length - int plan.Operation.End)
    page.GetPdfObject().Put(PdfName.Contents, PdfStream(output.ToArray()).MakeIndirect document) |> ignore
    plan.Mcid
