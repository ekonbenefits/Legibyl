/// Figures drawn in pieces: some tools tag a picture made of vector shapes as many small figures
/// side by side (Pages tags a border of coloured pencils as 67), each needing its own alt text.
/// This finds runs of them, to warn: a person merges them in Structure… (see ElementEdits'
/// merges), to describe (or mark as decoration) once.
module internal Legibyl.Validation.SplitFigures

open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

/// The most a piece's longer side can be, in points, for it to look like part of a picture.
[<Literal>]
let private MaxPiece = 36.0

/// The most the gap between two pieces can be, in points.
[<Literal>]
let private MaxGap = 6.0

/// How many small figures side by side make a run.
[<Literal>]
let private MinRun = 3

/// A run of small figures, side by side under one parent, to merge into its first.
type Run =
    { Join: ParagraphJoins.JoinRun
      /// Where the first is, for the warning.
      Path: string
      /// The whole run's box.
      Box: StructureGeometry }

let private isSmall (box: StructureGeometry) = max (box.Right - box.Left) (box.Top - box.Bottom) <= MaxPiece

/// The gap between two boxes on a page, 0 where they touch or overlap.
let private gap (a: StructureGeometry) (b: StructureGeometry) =
    let dx = max 0.0 (max (a.Left - b.Right) (b.Left - a.Right))
    let dy = max 0.0 (max (a.Bottom - b.Top) (b.Bottom - a.Top))
    max dx dy

let private union (boxes: StructureGeometry list) =
    { boxes.Head with
        Left = boxes |> List.map _.Left |> List.min
        Bottom = boxes |> List.map _.Bottom |> List.min
        Right = boxes |> List.map _.Right |> List.max
        Top = boxes |> List.map _.Top |> List.max }

/// Runs of three or more small figures without alt text, one after another under one parent,
/// each next to the one before it on one page.
let find (document: PdfDocument) context : Run list =
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> []
    | root ->
        [ for visit in walk root context do
              match resolve (visit.Element.Get PdfName.K) with
              | :? PdfArray as kids ->
                  // Each kid that's a small figure without alt text, with its index and box.
                  let pieces =
                      [ for i in 0 .. kids.Size() - 1 ->
                            match resolve (kids.Get i) with
                            | :? PdfDictionary as kid when PdfName.Figure.Equals(standardTag kid context) && isNull (kid.Get PdfName.Alt) ->
                                match subtreeGeometry kid context (structElemPage kid context visit.Page) with
                                | Some box when isSmall box -> Some(i, box)
                                | _ -> None
                            | _ -> None ]
                  // Split into runs of neighbours.
                  let mutable run: (int * StructureGeometry) list = []
                  let runs = ResizeArray<(int * StructureGeometry) list>()
                  let finish () =
                      if run.Length >= MinRun then runs.Add(List.rev run)
                      run <- []
                  for piece in pieces do
                      match piece, run with
                      | Some(i, box), (j, last) :: _ when j = i - 1 && last.PageNumber = box.PageNumber && gap last box <= MaxGap ->
                          run <- (i, box) :: run
                      | Some piece, _ ->
                          finish ()
                          run <- [ piece ]
                      | None, _ -> finish ()
                  finish ()
                  for pieces in runs do
                      let boxes = pieces |> List.map snd
                      yield
                          { Join = { Parent = visit.Element; Indexes = pieces |> List.map fst; Page = boxes.Head.PageNumber }
                            Path = $"{visit.Path}/K[{fst pieces.Head}]"
                            Box = union boxes }
              | _ -> () ]
