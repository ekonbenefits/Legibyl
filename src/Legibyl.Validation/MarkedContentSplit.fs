/// Splitting the end off a piece of marked content into a piece of its own, the page looking the
/// same: "diversification." drawn as one piece becomes "diversification" and "." (see
/// UnlinkedUrls, which links an address without the full stop after it). And carving text out of
/// the middle of a piece: "Visit www.example.org for more." becomes "Visit ", "www.example.org"
/// and " for more.", each a piece of its own, read in the same order.
///
/// Only the end of the last text the piece draws, with nothing drawn after it, and nothing tagged
/// (with an MCID of its own) inside it. The text-showing operator is cut in two between glyphs.
/// Whatever is open inside the piece at the cut (a layer's marked content, a text object) is
/// ended before the piece ends, and begun again inside the new one, the same: the text state
/// lasts past ET, and the text matrix is set where the end was drawn. A piece begun inside a text
/// object ends and the new one begins between the two halves. Marked as an artifact at the cut
/// isn't split: the end would read as the artifact does.
///
/// A carve cuts the same way, before the text and after it, where its glyphs are drawn one after
/// another, once in the piece. A text object begun again at a cut only keeps the lines after it
/// in their places if none of them is placed from where the last began (Td, TD, T*, ', "), so a
/// carve that would need that isn't made.
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

/// A glyph a text-showing operator draws: its bytes, its text, and the text matrix it's drawn at;
/// whether it's a clip (render modes 4 to 7, which clip once its text object ends), and whether
/// its font writes downwards (a vertical CMap), where the text matrix iText gives isn't where it is.
type private Glyph =
    { Bytes: byte array
      Text: string
      Matrix: Matrix
      Clips: bool
      Vertical: bool }

/// Whether font writes downwards: its encoding a vertical CMap ("-V", or one with /WMode 1).
let private writesVertically (font: iText.Kernel.Font.PdfFont | null) =
    match font with
    | NonNull font ->
        match font.GetPdfObject().Get PdfName.Encoding with
        | :? PdfName as name -> name.GetValue().EndsWith "-V"
        | :? PdfStream as cmap -> (match cmap.GetAsNumber(PdfName "WMode") with NonNull mode -> mode.IntValue() = 1 | Null -> false)
        | _ -> false
    | Null -> false

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
                let clips = text.GetTextRenderMode() >= 4
                let vertical = writesVertically (text.GetFont())
                let glyphs =
                    [| for glyph in text.GetCharacterRenderInfos() ->
                           { Bytes = glyph.GetPdfString().GetValueBytes()
                             Text = glyph.GetText()
                             Matrix = glyph.GetTextMatrix()
                             Clips = clips
                             Vertical = vertical } |]
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

/// A page's piece of marked content, as a split reads it: its BDC (First) and its EMC (Last)
/// among the page's operators, and the text-showing operators between them.
type private Piece =
    { Bytes: byte array
      Resources: PdfResources
      Operations: List<Operation>
      First: int
      Last: int
      /// Whether it's begun inside a text object.
      InText: bool
      Tag: PdfName
      Properties: PdfDictionary
      Showing: int list }

let private propertiesOf (resources: PdfResources) (op: Operation) =
    if op.Operands.Length < 2 then None
    else
        match op.Operands[1] with
        | :? PdfDictionary as properties -> Some properties
        | :? PdfName as name ->
            match resources.GetResource PdfName.Properties with
            | null -> None
            | all -> Option.ofObj (all.GetAsDictionary name)
        | _ -> None

let private saysWhatItReadsAs (properties: PdfDictionary) =
    [ PdfName.ActualText; PdfName.Alt; PdfName.E ] |> List.exists properties.ContainsKey

let private isBT (op: Operation) = op.Name = ContentOperators.BT

/// The piece of marked content mcid on the page, when a split can copy it: drawn once, ended,
/// with no tagged marked content inside it, its properties readable, copyable and not saying what
/// all of it reads as, and some text drawn in it; or why not.
let private pieceOf (page: PdfPage) (mcid: int) : Result<Piece, string> =
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
        let inText =
            operations
            |> Seq.take first
            |> Seq.fold (fun open' op -> if isBT op then true elif op.Name = ContentOperators.ET then false else open') false
        let properties = propertiesOf resources operations[first]
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
            Ok
                { Bytes = bytes
                  Resources = resources
                  Operations = operations
                  First = first
                  Last = last
                  InText = inText
                  Tag = operations[first].Operands[0] :?> PdfName
                  Properties = properties.Value
                  Showing = showing }
    | [] -> Error "its marked content isn't in the page's own content"
    | _ -> Error "its marked content is drawn in more than one place"

/// Why the piece's text can't be cut where a text object would be ended and begun again: its text
/// is a clip (ending the text object would clip to what's drawn so far), or written downwards
/// (where it's begun again isn't where iText says the glyph is). None if it can be.
let private uncuttable (piece: Piece) (shown: Dictionary<int, List<Glyph array>>) =
    let glyphs =
        [ for k in piece.First + 1 .. piece.Last - 1 do
              match shown.TryGetValue k with
              | true, list -> for glyphs in list do yield! glyphs
              | _ -> () ]
    if glyphs |> List.exists _.Clips then Some "its text is drawn as a clip, which cutting it would clip short"
    elif glyphs |> List.exists _.Vertical then Some "its text is written downwards, which the fix doesn't cut"
    else None

/// What's open inside the piece before operator at, outermost first.
let private insideAt (piece: Piece) at =
    let mutable frames = []
    for k in piece.First + 1 .. at - 1 do
        let op = piece.Operations[k]
        if op.Name = ContentOperators.BDC || op.Name = ContentOperators.BMC then frames <- Marked op :: frames
        elif isBT op then frames <- TextObject :: frames
        elif op.Name = ContentOperators.EMC || op.Name = ContentOperators.ET then
            match frames with
            | _ :: rest -> frames <- rest
            | [] -> ()
    List.rev frames

let private isArtifact frame =
    match frame with
    | Marked op -> op.Operands.Length > 0 && op.Operands[0] = (PdfName.Artifact :> PdfObject)
    | TextObject -> false

/// Marked content open at a cut is begun again after it: what it says it reads as would be said
/// twice.
let private readsAsSomethingElse (piece: Piece) frame =
    match frame with
    | Marked op when op.Name = ContentOperators.BDC ->
        match propertiesOf piece.Resources op with
        | Some properties -> saysWhatItReadsAs properties
        | None -> true
    | _ -> false

/// The strings a text-showing operator shows (and, for TJ, its spacing between them), each
/// paired with the glyphs drawing it (each the same bytes); None when they can't be paired.
let private pairedGlyphs (operation: Operation) (events: Glyph array list) =
    let strings =
        match operation.Name, operation.Operands with
        | n, [| :? PdfString as s |] when n = ContentOperators.Tj || n = ContentOperators.NextLineTj -> Some [ s :> PdfObject ]
        | n, [| _; _; :? PdfString as s |] when n = ContentOperators.SpacedNextLineTj -> Some [ s :> PdfObject ]
        | n, [| :? PdfArray as array |] when n = ContentOperators.TJ -> Some [ for k in 0 .. array.Size() - 1 -> array.Get k ]
        | _ -> None
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

let private stringOf (glyphs: Glyph seq) =
    let s = PdfString(Array.concat (glyphs |> Seq.map _.Bytes))
    s.SetHexWriting true |> ignore
    s :> PdfObject

/// The page's MCIDs past those its content and its tags (usedMcids) give it.
let private nextMcid (piece: Piece) (usedMcids: int seq) =
    let mcids = seq { for op in piece.Operations do match mcidOf piece.Resources op with Some id -> id | None -> () }
    Seq.append mcids usedMcids |> Seq.fold max -1 |> (+) 1

/// How the end (tail, whitespace aside) of the piece of marked content mcid on the page would
/// be split off, numbered past usedMcids (those the tags give the page); or why it can't be.
let plan (page: PdfPage) (mcid: int) (tail: string) (usedMcids: int seq) : Result<Plan, string> =
    try
        match pieceOf page mcid with
        | Error why -> Error why
        | Ok piece ->
            let operations = piece.Operations
            let at = List.last piece.Showing
            let operation = operations[at]
            let inside = insideAt piece at
            if [ for k in at + 1 .. piece.Last - 1 -> operations[k] ] |> List.exists paints then
                Error "something is drawn after it, in the same piece"
            elif operation.Name <> ContentOperators.Tj && operation.Name <> ContentOperators.TJ then
                Error "the text drawing it moves to a new line first, which the fix doesn't cut"
            // (Its text isn't the paragraph's, so not an address's: kept in case that changes.)
            elif inside |> List.exists isArtifact then
                Error "it's marked as an artifact inside its piece"
            elif inside |> List.exists (readsAsSomethingElse piece) then
                Error "marked content around it inside its piece says what it reads as, for all of it"
            elif piece.InText = (inside |> List.exists (function TextObject -> true | Marked _ -> false)) then
                Error "its text isn't drawn in the usual way"
            else
                let shown = shownBy piece.Bytes piece.Resources
                let events = match shown.TryGetValue at with | true, list -> List.ofSeq list | _ -> []
                let endsTextObject = inside |> List.exists (function TextObject -> true | Marked _ -> false)
                match pairedGlyphs operation events, (if endsTextObject then uncuttable piece shown else None) with
                | _, Some why -> Error why
                | None, None -> Error "the text drawing it can't be cut between its letters"
                | Some(items, glyphsOf), None ->
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
                        if not headDraws && piece.Showing.Length = 1 then
                            Error "it's all the text its piece draws"
                        else
                            Ok
                                { Bytes = piece.Bytes
                                  Operation = operation
                                  Head = head
                                  Tail = tailItems
                                  Inside = inside
                                  At = firstGlyph.Matrix
                                  Tag = piece.Tag
                                  Properties = piece.Properties
                                  Mcid = nextMcid piece usedMcids }
                    | _ -> Error "it isn't the end of the text its piece draws"
    with ex -> Error $"the page's content couldn't be read safely ({ex.Message})"

/// Which pieces a carve leaves: the text before the address (keeping the piece's MCID), when
/// there is any; the address's; and the text after it, when there is any.
type Carved = { Before: bool; Address: int; After: int option }

/// What carving text out of the middle of a piece of marked content takes: the page's content,
/// and what to write in place of which of its bytes.
type Carving =
    private
        { Bytes: byte array
          /// Each edit: from, to (exclusive; the same for an insertion), and what's written there.
          Edits: (int64 * int64 * byte array) list
          Pieces: Carved }

let private written (value: PdfObject) =
    use stream = new MemoryStream()
    let output = new PdfOutputStream(stream)
    output.Write value |> ignore
    output.Flush()
    stream.ToArray()

let private number (value: float32) = (float value).ToString("0.######", CultureInfo.InvariantCulture)

/// What ends a piece (and what's open inside it) and begins another, its tag and properties the
/// same but its MCID: the text object, if one's open, begun again at at.
let private between (bytes: byte array) (tag: PdfName) (properties: PdfDictionary) (mcid: int) (inside: Frame list) (at: Matrix) =
    let properties = properties.Clone() :?> PdfDictionary
    properties.Put(PdfName.MCID, PdfNumber mcid) |> ignore
    let opening = Array.concat [ written tag; Encoding.ASCII.GetBytes " "; written properties; Encoding.ASCII.GetBytes " BDC\n" ]
    let matrix =
        String.Join(" ", [ at.Get Matrix.I11; at.Get Matrix.I12; at.Get Matrix.I21; at.Get Matrix.I22; at.Get Matrix.I31; at.Get Matrix.I32 ] |> List.map number)
    let ending frame =
        match frame with
        | Marked _ -> Encoding.ASCII.GetBytes "\nEMC\n"
        | TextObject -> Encoding.ASCII.GetBytes "\nET\n"
    let beginning frame =
        match frame with
        | Marked op -> Array.append bytes[int op.Start .. int op.End - 1] (Encoding.ASCII.GetBytes "\n")
        | TextObject -> Encoding.ASCII.GetBytes $"BT\n{matrix} {ContentOperators.Tm}\n"
    Array.concat
        [ yield! inside |> List.rev |> List.map ending
          Encoding.ASCII.GetBytes "\nEMC\n"
          opening
          yield! inside |> List.map beginning ]

/// Moves relative to a line's start, which a text object begun again at a cut wouldn't keep.
let private movesByLine =
    set [ ContentOperators.Td; ContentOperators.TD; ContentOperators.TStar; ContentOperators.NextLineTj; ContentOperators.SpacedNextLineTj ]

/// How text (whitespace aside) drawn in the middle of the piece of marked content mcid on the
/// page would be carved out into a piece of its own, with what's drawn before it left in the piece
/// and what's drawn after it in another, the page looking the same; numbered past usedMcids. Or
/// why it can't be. Its glyphs have to be drawn one after another, once in the piece.
let around (page: PdfPage) (mcid: int) (text: string) (usedMcids: int seq) : Result<Carving, string> =
    try
        match pieceOf page mcid with
        | Error why -> Error why
        // (An artifact's text isn't the paragraph's: one inside the piece could be read into the
        // address searched for.)
        | Ok piece when
            [ piece.First + 1 .. piece.Last - 1 ]
            |> List.exists (fun k ->
                let op = piece.Operations[k]
                (op.Name = ContentOperators.BDC || op.Name = ContentOperators.BMC)
                && op.Operands.Length > 0
                && op.Operands[0] = (PdfName.Artifact :> PdfObject))
            ->
            Error "an artifact is marked inside its piece"
        | Ok piece ->
            let shown = shownBy piece.Bytes piece.Resources
            // Every glyph the piece's own text draws, in the order it's drawn: its operator, and
            // its place among the operator's glyphs.
            let glyphs =
                [| for at in piece.Showing do
                       match shown.TryGetValue at with
                       | true, list ->
                           let all = Array.concat list
                           for g in 0 .. all.Length - 1 -> at, g, all[g]
                       | _ -> () |]
            let texts = glyphs |> Array.map (fun (_, _, glyph) -> unspaced glyph.Text)
            let starts = texts |> Array.scan (fun start text -> start + text.Length) 0
            let joined = String.Concat texts
            let wanted = unspaced text
            let occurrences =
                let rec from i found =
                    match joined.IndexOf(wanted, i, StringComparison.Ordinal) with
                    | -1 -> List.rev found
                    | at -> from (at + 1) (at :: found)
                if wanted = "" then [] else from 0 []
            let glyphStarting at = [ 0 .. texts.Length - 1 ] |> List.tryFind (fun k -> texts[k] <> "" && starts[k] = at)
            let glyphEnding at = [ 0 .. texts.Length - 1 ] |> List.tryFind (fun k -> texts[k] <> "" && starts[k + 1] = at)
            match occurrences with
            | [] -> Error "its letters aren't drawn one after another in its piece"
            | _ :: _ :: _ -> Error "it's shown more than once in its piece"
            | [ at ] ->
                match glyphStarting at, glyphEnding (at + wanted.Length) with
                | Some first, Some last ->
                    let inkBefore = texts[.. first - 1] |> Array.exists ((<>) "")
                    let inkAfter = texts[last + 1 ..] |> Array.exists ((<>) "")
                    let next = nextMcid piece usedMcids
                    let pieces =
                        if inkBefore then { Before = true; Address = next; After = (if inkAfter then Some(next + 1) else None) }
                        else { Before = false; Address = mcid; After = (if inkAfter then Some next else None) }
                    // Where each new piece begins: before a glyph (by its place among them all), and its MCID.
                    let cuts =
                        [ if inkBefore then first, pieces.Address
                          match pieces.After with
                          | Some id -> last + 1, id
                          | None -> () ]
                    let checkCut (k, id) =
                        let at, g, glyph = glyphs[k]
                        let inside = insideAt piece at
                        let inTextObject = inside |> List.exists (function TextObject -> true | Marked _ -> false)
                        // A text object begun again at the cut (the operator is cut, see insertions)
                        // starts at the glyph: later lines placed by where the line began would move.
                        let nextLines =
                            let from = at + 1
                            let ends =
                                [ from .. piece.Last - 1 ]
                                |> List.tryFind (fun k -> piece.Operations[k].Name = ContentOperators.ET)
                                |> Option.defaultValue piece.Last
                            [ from .. ends - 1 ] |> List.exists (fun k -> movesByLine.Contains piece.Operations[k].Name)
                        if inside |> List.exists isArtifact then Error "it's marked as an artifact inside its piece"
                        elif inside |> List.exists (readsAsSomethingElse piece) then
                            Error "marked content around it inside its piece says what it reads as, for all of it"
                        elif piece.InText = inTextObject then Error "its text isn't drawn in the usual way"
                        elif inTextObject && nextLines then
                            Error "its text is begun inside its piece, and lines after it are placed from where theirs begin, which the fix can't keep once it's cut"
                        else
                            match (if inTextObject then uncuttable piece shown else None) with
                            | Some why -> Error why
                            | None -> Ok(at, g, glyph, id, inside, inTextObject)
                    let checkedCuts = cuts |> List.map checkCut
                    match checkedCuts |> List.tryPick (function Error why -> Some why | Ok _ -> None) with
                    | Some why -> Error why
                    | None ->
                        let cuts = checkedCuts |> List.choose (function Ok cut -> Some cut | Error _ -> None)
                        let breakAt (glyph: Glyph) id inside = between piece.Bytes piece.Tag piece.Properties id inside glyph.Matrix
                        // Before an operator's first glyph: between it and the one before it. (In a
                        // text object begun again there, it's cut instead, so the spacing TJ puts
                        // before its first glyph isn't made twice.)
                        let between' (_, g, _, _, _, inTextObject) = g = 0 && not inTextObject
                        let insertions =
                            [ for at, _, glyph, id, inside, _ as cut in cuts do
                                  if between' cut then
                                      let start = piece.Operations[at].Start
                                      Ok(start, start, Array.append (Encoding.ASCII.GetBytes "\n") (breakAt glyph id inside)) ]
                        // Inside an operator: it's cut between its glyphs, its first part shown as
                        // it was, the rest as Tj or TJ.
                        let splits =
                            [ for at, cutsHere in cuts |> List.filter (between' >> not) |> List.groupBy (fun (at, _, _, _, _, _) -> at) do
                                  let operation = piece.Operations[at]
                                  let events = match shown.TryGetValue at with | true, list -> List.ofSeq list | _ -> []
                                  match pairedGlyphs operation events with
                                  | None -> Error "the text drawing it can't be cut between its letters"
                                  | Some(items, glyphsOf) ->
                                      let all =
                                          [| for itemIndex, item in List.indexed items do
                                                 match glyphsOf.TryGetValue item with
                                                 | true, glyphs -> for g in 0 .. glyphs.Length - 1 -> itemIndex, g
                                                 | _ -> () |]
                                      let places = cutsHere |> List.map (fun (_, g, _, _, _, _) -> all[g])
                                      let parts = List<List<PdfObject>>()
                                      parts.Add(List())
                                      for itemIndex, item in List.indexed items do
                                          let here = places |> List.filter (fun (i, _) -> i = itemIndex) |> List.map snd
                                          if here.IsEmpty then parts[parts.Count - 1].Add item
                                          else
                                              let glyphs = glyphsOf[item]
                                              let bounds = 0 :: here @ [ glyphs.Length ]
                                              for i, (s, e) in List.indexed (List.pairwise bounds) do
                                                  if i > 0 then parts.Add(List())
                                                  if e > s then parts[parts.Count - 1].Add(stringOf glyphs[s .. e - 1])
                                      let shows first (items: PdfObject list) =
                                          let op name = Encoding.ASCII.GetBytes $" {name}\n"
                                          let items =
                                              if first && items.IsEmpty && operation.Name <> ContentOperators.TJ then [ PdfString "" :> PdfObject ]
                                              else items
                                          match items with
                                          | [ :? PdfString as s ] when first && operation.Name = ContentOperators.NextLineTj ->
                                              Array.append (written s) (op ContentOperators.NextLineTj)
                                          | [ :? PdfString as s ] when first && operation.Name = ContentOperators.SpacedNextLineTj ->
                                              Array.concat
                                                  [ written operation.Operands[0]; Encoding.ASCII.GetBytes " "
                                                    written operation.Operands[1]; Encoding.ASCII.GetBytes " "
                                                    written s; op ContentOperators.SpacedNextLineTj ]
                                          | [ :? PdfString as s ] -> Array.append (written s) (op ContentOperators.Tj)
                                          | items -> Array.append (written (PdfArray(ResizeArray items))) (op ContentOperators.TJ)
                                      let breaks = cutsHere |> List.map (fun (_, _, glyph, id, inside, _) -> breakAt glyph id inside)
                                      let replacement =
                                          Array.concat
                                              [ yield Encoding.ASCII.GetBytes "\n"
                                                yield shows true (List.ofSeq parts[0])
                                                for i, part in List.indexed (List.ofSeq parts |> List.tail) do
                                                    yield breaks[i]
                                                    yield shows false (List.ofSeq part) ]
                                      Ok(operation.Start, operation.End, replacement) ]
                        match insertions @ splits |> List.tryPick (function Error why -> Some why | Ok _ -> None) with
                        | Some why -> Error why
                        | None ->
                            Ok
                                { Bytes = piece.Bytes
                                  Edits = insertions @ splits |> List.choose (function Ok edit -> Some edit | Error _ -> None) |> List.sortBy (fun (from, until, _) -> from, until)
                                  Pieces = pieces }
                | _ -> Error "its first or last letter is drawn together with the text beside it"
    with ex -> Error $"the page's content couldn't be read safely ({ex.Message})"

/// Carves the text out, writing the page's content; the pieces it leaves, whose new MCIDs
/// nothing in the tags holds yet.
let carve (document: PdfDocument) (page: PdfPage) (carving: Carving) =
    use output = new MemoryStream()
    let mutable position = 0L
    for from, until, bytes in carving.Edits do
        output.Write(carving.Bytes, int position, int (from - position))
        output.Write(bytes, 0, bytes.Length)
        position <- until
    output.Write(carving.Bytes, int position, carving.Bytes.Length - int position)
    page.GetPdfObject().Put(PdfName.Contents, PdfStream(output.ToArray()).MakeIndirect document) |> ignore
    carving.Pieces

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
