/// Builds a StructureNode tree by walking veraPDF's plain PD model (org.verapdf.pd) — the
/// same model the validator itself uses internally — enriched with text and geometry from
/// PdfPig (MarkedContentTextIndex) and link annotation targets from iText.
module internal Legibyl.Validation.StructureTreeWalk

open System
open System.Collections.Generic
open iText.Kernel.Pdf
open org.verapdf.``as``
open org.verapdf.cos
open org.verapdf.pd
open org.verapdf.pd.structure
open Legibyl.Validation.Interop

// Guards against pathological/malformed structure trees (shouldn't happen for a
// real tree, but a corrupt or maliciously crafted PDF could contain very deep or
// cyclic-looking nesting) so a single bad file can't hang or stack-overflow the walk.
[<Literal>]
let private MaxStructureDepth = 200

type private FormFieldInfo =
    { Name: string | null
      Description: string | null
      Type: string | null
      Value: string | null
      Geometry: StructureGeometry option }

type private LinkTargetInfo =
    { Uri: string | null
      DestinationPageNumber: int option
      DestinationDescription: string | null
      Destination: LinkDestination option }

/// A destination's kind and coordinates (from its entries after the page), read with `number`
/// giving each entry as a number if it is one.
let private linkDestination (pageNumber: int) (kind: string) (number: int -> float option) =
    let at i = number i |> Option.toNullable
    let none = Nullable()
    let left, top, right, bottom =
        match kind with
        | Destinations.XYZ -> at 2, at 3, none, none
        | Destinations.FitH | Destinations.FitBH -> none, at 2, none, none
        | Destinations.FitV | Destinations.FitBV -> at 2, none, none, none
        | Destinations.FitR -> at 2, at 5, at 4, at 3
        | _ -> none, none, none, none
    { PageNumber = pageNumber
      Kind = kind
      Left = left
      Top = top
      Right = right
      Bottom = bottom }

type private LinkAnnotationTarget = { Geometry: StructureGeometry; Target: LinkTargetInfo }

type private WalkContext =
    { PageNumberByKey: IReadOnlyDictionary<string, int>
      TextIndex: MarkedContentTextIndex.Index
      LinkAnnotationTargets: LinkAnnotationTarget list
      /// Called once per structure element walked, for progress.
      OnElement: unit -> unit }

let private atom (value: string) = ASAtom.getASAtom value

let private atomValue (value: ASAtom | null) = value |?> _.getValue()

let private normalizedGeometry pageNumber left bottom right top =
    { PageNumber = pageNumber
      Left = min left right
      Bottom = min bottom top
      Right = max left right
      Top = max bottom top }

let private area (geometry: StructureGeometry) =
    max 1.0 ((geometry.Right - geometry.Left) * (geometry.Top - geometry.Bottom))

let private overlapRatio (content: StructureGeometry) (annotation: StructureGeometry) =
    let left = max content.Left annotation.Left
    let right = min content.Right annotation.Right
    let bottom = max content.Bottom annotation.Bottom
    let top = min content.Top annotation.Top
    if right <= left || top <= bottom then 0.0
    else (right - left) * (top - bottom) / min (area content) (area annotation)

// ---- COS object helpers ----

let private numberAt (array: COSObject) (index: int) : float option =
    match array.at index with
    | null -> None
    | item when item.getType() |> isNull |> not && item.getType().isNumber() ->
        match item.getReal() with
        | null -> item.getInteger() |> Option.ofObj |> Option.map _.doubleValue()
        | real -> Some(real.doubleValue())
    | _ -> None

let private cosSize (value: COSObject) =
    match value.size () with
    | null -> 0
    | size -> size.intValue ()

let private geometryFromCosRect pageNumber (rect: COSObject | null) =
    match rect with
    | null -> None
    | rect when rect.size().intValue() < 4 -> None
    | rect ->
        match numberAt rect 0, numberAt rect 1, numberAt rect 2, numberAt rect 3 with
        | Some left, Some bottom, Some right, Some top -> Some(normalizedGeometry pageNumber left bottom right top)
        | _ -> None

let private pageNumberOfKey context (key: obj | null) =
    match key with
    | null -> None
    | key -> lookup context.PageNumberByKey (string key)

/// An element's /A: one attribute dictionary, or an array of them, each optionally followed
/// by a revision number ([ << ... >> 0 ], as Canva writes it). The layout /BBox from the first
/// dictionary that has one.
let rec private bboxFromAttributes (attributes: COSObject | null) pageNumber =
    match attributes with
    | null -> None
    | attributes when attributes.getType () = COSObjType.COS_DICT ->
        match attributes.knownKey (atom "BBox") with
        | NonNull known when known.booleanValue () -> geometryFromCosRect pageNumber (attributes.getKey (atom "BBox"))
        | _ -> None
    | attributes when attributes.getType () = COSObjType.COS_ARRAY ->
        seq { 0 .. cosSize attributes - 1 } |> Seq.tryPick (fun i -> bboxFromAttributes (attributes.at i) pageNumber)
    // A revision number, or anything else: no attributes here.
    | _ -> None

let private layoutBBoxGeometry (elem: PDStructElem) (pageNumber: int option) =
    pageNumber |> Option.bind (bboxFromAttributes (elem.getKey(atom "A")))

/// An integer attribute (a table cell's /ColSpan or /RowSpan) from the first of an element's
/// /A dictionaries that has it (see bboxFromAttributes for how /A is written).
let rec private integerAttribute (key: string) (attributes: COSObject | null) =
    match attributes with
    | null -> None
    | attributes when attributes.getType () = COSObjType.COS_DICT ->
        match attributes.knownKey (atom key) with
        | NonNull known when known.booleanValue () ->
            match attributes.getKey (atom key) with
            | null -> None
            | value -> value.getInteger () |> Option.ofObj |> Option.map (fun n -> int (n.longValue ()))
        | _ -> None
    | attributes when attributes.getType () = COSObjType.COS_ARRAY ->
        seq { 0 .. cosSize attributes - 1 } |> Seq.tryPick (fun i -> integerAttribute key (attributes.at i))
    | _ -> None

/// How many columns or rows a table cell spans: at least 1 (a missing or nonsense span is 1).
let private cellSpan (key: string) (tag: string) (elem: PDStructElem) =
    if tag = Tags.TH || tag = Tags.TD then
        integerAttribute key (elem.getKey (atom "A")) |> Option.filter (fun n -> n > 1) |> Option.defaultValue 1
    else
        1

// ---- Structure element children ----

/// The widget annotation referenced by an object reference, if it is one.
let private referencedAnnotation (child: obj) (subtype: string) =
    match child with
    | :? PDOBJRDictionary as objr ->
        match objr.getReferencedObject () with
        | null -> None
        | referenced when referenced.empty () -> None
        | referenced when atomValue (referenced.getNameKey(atom "Subtype")) = subtype -> Some referenced
        | _ -> None
    | _ -> None

/// The annotation (not a form field's widget) an object reference is to, if it is to one.
let private referencedOtherAnnotation (child: obj) =
    match child with
    | :? PDOBJRDictionary as objr ->
        match objr.getReferencedObject () with
        | null -> None
        | referenced when referenced.empty () -> None
        | referenced when
            (match atomValue (referenced.getNameKey(atom "Subtype")) with
             | null | "Widget" -> false
             | _ -> true)
            ->
            Some referenced
        | _ -> None
    | _ -> None

/// A link's or another annotation's description: its /Contents, if it has one.
let private readLinkDescription (child: obj) =
    referencedOtherAnnotation child |> Option.bind (fun referenced -> Option.ofObj (nullIfEmpty (referenced.getStringKey(atom "Contents"))))

let private readFormFieldInfo (child: obj) (ownPage: int option) =
    referencedAnnotation child "Widget"
    |> Option.map (fun referenced ->
        // The widget and the fields above it (its /Parent, and theirs): a widget that isn't a
        // field itself has its field's name, tooltip, type and value there, and type and value
        // can be inherited from further up.
        let chain =
            let rec up (node: COSObject) (depth: int) =
                [ yield node
                  match node.getKey (atom "Parent") with
                  | NonNull parent when depth < 32 && not (parent.empty ()) -> yield! up parent (depth + 1)
                  | _ -> () ]
            up referenced 0
        let first (read: COSObject -> string | null) = chain |> List.tryPick (fun node -> Option.ofObj (read node)) |> Option.toObj
        let value =
            first (fun node ->
                match nullIfEmpty (node.getStringKey(atom "V")) with
                | null -> atomValue (node.getNameKey(atom "V"))
                | value -> value)

        { Name = first (fun node -> nullIfEmpty (node.getStringKey(atom "T")))
          Description = first (fun node -> nullIfEmpty (node.getStringKey(atom "TU")))
          Type = first (fun node -> atomValue (node.getNameKey(atom "FT")))
          Value = value
          Geometry =
            match ownPage with
            | Some page -> geometryFromCosRect page (referenced.getKey(atom "Rect"))
            | None -> None })

let private formatCosDestinationCoordinates (destination: COSObject) =
    match cosSize destination with
    | size when size < 2 -> ""
    | size ->
        let parts = [ for i in 1 .. size - 1 -> destination.at i |?> _.toString() |> Option.ofObj |> Option.defaultValue "?" ]
        $""" ({String.Join(' ', parts)})"""

let private readDestination (destination: COSObject | null) context =
    match destination with
    | null -> None
    | destination when destination.empty () -> None
    | destination ->
        let pageObject = if cosSize destination > 0 then destination.at 0 else destination
        let pageNumber = pageNumberOfKey context (pageObject |?> _.getKey())
        let description =
            match pageNumber with
            | None -> $"Internal destination: {destination}"
            | Some page -> $"Internal destination: page {page}{formatCosDestinationCoordinates destination}"
        let kind = if cosSize destination > 1 then atomValue ((destination.at 1).getName ()) else null
        Some
            { Uri = null
              DestinationPageNumber = pageNumber
              DestinationDescription = description
              Destination =
                match pageNumber, kind with
                | Some page, NonNull kind -> Some(linkDestination page kind (numberAt destination))
                | _ -> None }

let private uriTarget (uri: string) =
    { Uri = uri
      DestinationPageNumber = None
      DestinationDescription = $"URI: {uri}"
      Destination = None }

let private readLinkTargetInfo (child: obj) context =
    referencedAnnotation child "Link"
    |> Option.bind (fun referenced ->
        let action = referenced.getKey(atom "A")
        match nullIfEmpty (action |?> _.getStringKey(atom "URI")) with
        | null ->
            let destination: COSObject | null =
                match referenced.getKey(atom "Dest") with
                | null -> action |?> _.getKey(atom "D")
                | destination when destination.empty () -> action |?> _.getKey(atom "D")
                | destination -> destination
            readDestination destination context
        | uri -> Some(uriTarget uri))

/// Resolves one of a structure element's direct (non-structure-element) children —
/// either a plain MCID integer (content on the element's own/inherited page) or an
/// explicit PDMCRDictionary (content on a different page, e.g. artifacts spanning a
/// page boundary) — to its extracted visible text. Object references (PDOBJRDictionary,
/// e.g. a Link's associated annotation) have no marked-content text of their own.
let private resolveContentInfo (child: obj) (ownPage: int option) context =
    let find page (mcid: java.lang.Long | null) =
        match page, mcid with
        | Some page, NonNull mcid -> lookup context.TextIndex (page, int (mcid.longValue ()))
        | _ -> None

    match child with
    | :? PDMCRDictionary as mcr ->
        let page = pageNumberOfKey context (mcr.getPageObjectKey ()) |> Option.orElse ownPage
        find page (mcr.getMCID ())
    | :? COSObject as cos when not (isNull (cos.getType ())) && cos.getType().isNumber() && ownPage.IsSome ->
        find ownPage (cos.getInteger ())
    | _ -> None

// ---- Link target fallbacks ----

let private linkTargetByGeometry (geometry: StructureGeometry) context =
    context.LinkAnnotationTargets
    |> Seq.filter (fun target -> target.Geometry.PageNumber = geometry.PageNumber)
    |> Seq.map (fun target -> target.Target, overlapRatio geometry target.Geometry, area geometry / area target.Geometry)
    |> Seq.filter (fun (_, score, areaRatio) -> areaRatio <= 4.0 && score >= 0.5)
    |> Seq.sortByDescending (fun (_, score, _) -> score)
    |> Seq.tryHead
    |> Option.map (fun (target, _, _) -> target)

let rec private firstDescendantLinkTarget (nodes: StructureNode seq) =
    nodes
    |> Seq.tryPick (fun node ->
        if not (isNull node.LinkUri) || node.LinkDestinationPageNumber.HasValue || not (isNull node.LinkDestinationDescription) then
            Some
                { Uri = node.LinkUri
                  DestinationPageNumber = Option.ofNullable node.LinkDestinationPageNumber
                  DestinationDescription = node.LinkDestinationDescription
                  Destination = Option.ofObj node.LinkDestination }
        else
            firstDescendantLinkTarget node.Children)

// ---- iText link annotation index ----

let private geometryFromRect pageNumber (rect: PdfArray | null) =
    match rect with
    | null -> None
    | rect when rect.Size() < 4 -> None
    | rect ->
        let at (i: int) = rect.GetAsNumber(i).DoubleValue()
        Some(normalizedGeometry pageNumber (at 0) (at 1) (at 2) (at 3))

let private formatDestinationCoordinates (destination: PdfObject) =
    match destination with
    | :? PdfArray as array when array.Size() >= 2 ->
        let parts = [ for i in 1 .. array.Size() - 1 -> array.Get i |?> _.ToString() |> Option.ofObj |> Option.defaultValue "?" ]
        $""" ({String.Join(' ', parts)})"""
    | _ -> ""

let private readITextLinkTarget (annotation: PdfDictionary) (pageNumberByObject: IReadOnlyDictionary<int, int>) =
    let action = annotation.GetAsDictionary(PdfName.A)
    match action |?> _.GetAsString(PdfName.URI) |?> _.ToUnicodeString() with
    | NonNull uri when not (String.IsNullOrWhiteSpace uri) -> Some(uriTarget uri)
    | _ ->
        let destination =
            match annotation.Get(PdfName.Dest) with
            | null -> action |?> _.Get(PdfName.D)
            | destination -> destination

        match destination with
        | null -> None
        | destination ->
            let pageObject =
                match destination with
                | :? PdfArray as array when array.Size() > 0 -> array.Get 0
                | _ -> destination

            let reference: PdfIndirectReference | null =
                match pageObject with
                | :? PdfIndirectReference as reference -> reference
                | :? PdfDictionary as dictionary -> dictionary.GetIndirectReference()
                | _ -> null

            let pageNumber = reference |> Option.ofObj |> Option.bind (fun r -> lookup pageNumberByObject (r.GetObjNumber()))
            let description =
                match pageNumber with
                | None -> $"Internal destination: {destination}"
                | Some page -> $"Internal destination: page {page}{formatDestinationCoordinates destination}"
            let destinationOf page =
                match destination with
                | :? PdfArray as array when array.Size() > 1 && not (isNull (array.GetAsName 1)) ->
                    let number i =
                        if i < array.Size() then array.GetAsNumber i |> Option.ofObj |> Option.map _.DoubleValue() else None
                    Some(linkDestination page ((array.GetAsName 1).GetValue()) number)
                | _ -> None
            Some
                { Uri = null
                  DestinationPageNumber = pageNumber
                  DestinationDescription = description
                  Destination = pageNumber |> Option.bind destinationOf }

let private buildLinkAnnotationIndex (pdfPath: string) =
    use document = new PdfDocument(new PdfReader(pdfPath))
    let pageNumberByObject = StructureTreeObjects.pageNumberIndex document
    [ for pageNumber in 1 .. document.GetNumberOfPages() do
          match document.GetPage(pageNumber).GetPdfObject().GetAsArray(PdfName.Annots) with
          | null -> ()
          | annotations ->
              for i in 0 .. annotations.Size() - 1 do
                  match annotations.GetAsDictionary i with
                  | annotation when not (isNull annotation) && PdfName.Link.Equals(annotation.GetAsName(PdfName.Subtype)) ->
                      match geometryFromRect pageNumber (annotation.GetAsArray(PdfName.Rect)),
                            readITextLinkTarget annotation pageNumberByObject with
                      | Some geometry, Some target -> yield { Geometry = geometry; Target = target }
                      | _ -> ()
                  | _ -> () ]

// ---- The walk ----

/// Maps each page's underlying COS object key (e.g. "12 0 obj") to its 1-based
/// page number, so structure elements (which only know their owning page via an
/// indirect object reference) can be resolved back to a page number.
let private pageNumberIndex (doc: PDDocument) : IReadOnlyDictionary<string, int> =
    let index = Dictionary<string, int>()
    let pages = doc.getPages ()
    for i in 0 .. pages.size () - 1 do
        match pages.get i with
        | :? PDPage as page ->
            match page.getObject () |?> _.getKey() with
            | null -> ()
            | key -> index[key.toString ()] <- i + 1
        | _ -> ()
    index

let private resolveStructureTag (elem: PDStructElem) =
    // Prefer the semantic structure role (/S), not the object dictionary type
    // (/Type /StructElem). Some valid tagged PDFs use /S /Form with /Type
    // /StructElem; if we prefer getType() first, those controls display as the
    // generic implementation object "StructElem" instead of the screen-reader
    // role "Form".
    let candidates: (unit -> string | null) list =
        [ fun () -> elem.getCOSStructureType () |?> _.getName() |?> _.getValue()
          fun () -> elem.getStructureType () |?> _.getType() |?> _.getValue()
          fun () -> elem.getType () |?> _.getValue() ]

    candidates
    |> List.tryPick (fun read -> Option.ofObj (read ()))
    |> Option.defaultValue "?"

let private actualTextOf (elem: PDStructElem) : string | null =
    match elem.getActualText () with
    | null -> null
    | actualText when actualText.empty () -> null
    | actualText ->
        try
            actualText.getString ()
        with :? java.lang.Exception ->
            null

let rec private walkChildren (node: PDStructTreeNode) context inheritedPage depth : StructureNode array =
    if depth >= MaxStructureDepth then
        [||]
    else
        // Non-structure-element children are marked-content references and object
        // references (e.g. to a Link annotation) — the tree's actual leaves, i.e. the
        // tagged page content itself. They carry no hierarchy/semantic info of their
        // own, so they aren't represented as nodes; toStructureNode resolves them to
        // visible text and counts them via ContentLeafCount.
        [| for child in javaSeq (node.getChildren ()) do
               match child with
               | :? PDStructElem as elem ->
                   context.OnElement()
                   toStructureNode elem context inheritedPage depth
               | _ -> () |]

and private toStructureNode (elem: PDStructElem) context inheritedPage depth : StructureNode =
    let tag = resolveStructureTag elem
    let role =
        match elem.getRoleMapToSameNamespaceTag () with
        | role when role = tag -> null
        | role -> role

    // A structure element's own page is only explicit via /Pg on itself or an
    // ancestor; page-less elements (most containers) inherit their nearest
    // ancestor's page for the purpose of resolving their *own* marked content
    // (an element's /K array of direct MCID integers is always relative to
    // whichever page it/its ancestor declared).
    let ownPage = pageNumberOfKey context (elem.getPageObjectNumber ()) |> Option.orElse inheritedPage

    let leaves =
        javaSeq (elem.getChildren ())
        |> Seq.filter (fun child -> not (child :? PDStructElem))
        |> List.ofSeq

    let contentInfos = leaves |> List.choose (fun child -> resolveContentInfo child ownPage context)

    // Many writers leave /Pg off the element and put it on each marked-content reference
    // instead, so fall back to the page its own content is on (when that's one page).
    let displayPage =
        ownPage
        |> Option.orElseWith (fun () ->
            leaves
            |> List.choose (function
                | :? PDMCRDictionary as mcr -> pageNumberOfKey context (mcr.getPageObjectKey ())
                | _ -> None)
            |> List.distinct
            |> function
                | [ page ] -> Some page
                | _ -> None)
    let contentTexts = contentInfos |> List.map _.Text |> List.filter (not << String.IsNullOrEmpty)
    let formField = leaves |> List.tryPick (fun child -> readFormFieldInfo child ownPage)
    // A link (or another annotation's element) holding only its annotation is where the annotation is.
    let linkRect =
        match ownPage with
        | Some page ->
            leaves
            |> List.tryPick (fun child ->
                referencedOtherAnnotation child |> Option.bind (fun referenced -> geometryFromCosRect page (referenced.getKey(atom "Rect"))))
        | None -> None

    let children = walkChildren elem context ownPage (depth + 1)

    let geometry =
        (contentInfos |> List.choose _.Geometry) @ (children |> Seq.choose (fun child -> Option.ofObj child.Geometry) |> List.ofSeq)
        |> StructureTreeObjects.unionGeometry
        |> Option.orElse (formField |> Option.bind _.Geometry)
        |> Option.orElse linkRect
        |> Option.orElseWith (fun () -> layoutBBoxGeometry elem ownPage)

    let linkTarget =
        leaves
        |> List.tryPick (fun child -> readLinkTargetInfo child context)
        |> Option.orElseWith (fun () ->
            match geometry with
            | Some geometry when tag = Tags.Link -> linkTargetByGeometry geometry context
            | _ -> None)
        |> Option.orElseWith (fun () -> if tag = Tags.TOCI then firstDescendantLinkTarget children else None)

    { Tag = tag
      Role = role
      AlternateDescription = nullIfEmpty (elem.getAlternateDescription ())
      ActualText = nullIfEmpty (actualTextOf elem)
      Content = if contentTexts.IsEmpty then null else String.Join(" ", contentTexts)
      FormFieldName = formField |> Option.map _.Name |> Option.toObj
      FormFieldDescription = formField |> Option.map _.Description |> Option.toObj
      FormFieldType = formField |> Option.map _.Type |> Option.toObj
      FormFieldValue = formField |> Option.map _.Value |> Option.toObj
      LinkUri = linkTarget |> Option.map _.Uri |> Option.toObj
      LinkDestinationPageNumber = linkTarget |> Option.bind _.DestinationPageNumber |> Option.toNullable
      LinkDestinationDescription = linkTarget |> Option.map _.DestinationDescription |> Option.toObj
      LinkDestination = linkTarget |> Option.bind _.Destination |> Option.toObj
      LinkDescription = leaves |> List.tryPick readLinkDescription |> Option.toObj
      Geometry = Option.toObj geometry
      Language = nullIfEmpty (elem.getLang () |?> _.toString())
      PageNumber = Option.toNullable displayPage
      ContentLeafCount = leaves.Length
      ColSpan = cellSpan "ColSpan" tag elem
      RowSpan = cellSpan "RowSpan" tag elem
      Children = children }

/// Reports reading progress to an optional sink: stage changes always, updates within a
/// stage at most ten times a second.
type private ReadReporter(progress: IProgress<StructureReadProgress> | null) =
    let stopwatch = Diagnostics.Stopwatch.StartNew()
    let mutable lastStage = enum<StructureReadStage> -1
    let mutable lastReport = TimeSpan.Zero

    member _.Report(stage: StructureReadStage, page: int, pageCount: int, elements: int) =
        match progress with
        | null -> ()
        | progress ->
            let now = stopwatch.Elapsed
            if stage <> lastStage || now - lastReport >= TimeSpan.FromMilliseconds 100.0 then
                lastStage <- stage
                lastReport <- now
                progress.Report
                    { Stage = stage
                      Page = page
                      PageCount = pageCount
                      Elements = elements
                      Elapsed = now }

/// Reads the structure tree of the PDF at pdfPath, or null if it has no StructTreeRoot,
/// reporting to progress (if given) as it goes.
let read (pdfPath: string) (progress: IProgress<StructureReadProgress> | null) : StructureNode | null =
    let reporter = ReadReporter progress
    let doc = PDDocument(pdfPath)
    try
        match doc.getStructTreeRoot () with
        | null -> null
        | structTreeRoot ->
            let mutable elements = 0
            // Text and link targets come from separate parses (PdfPig, iText). If either
            // fails (e.g. a PDF quirk that library can't parse but veraPDF tolerates),
            // degrade gracefully: keep the structure tree itself, just without that detail.
            let context =
                { PageNumberByKey = pageNumberIndex doc
                  TextIndex =
                    try
                        MarkedContentTextIndex.buildReporting pdfPath (fun page pageCount ->
                            reporter.Report(StructureReadStage.ReadingText, page, pageCount, 0))
                    with _ ->
                        MarkedContentTextIndex.Index()
                  LinkAnnotationTargets =
                    try
                        reporter.Report(StructureReadStage.ReadingLinks, 0, 0, 0)
                        buildLinkAnnotationIndex pdfPath
                    with _ ->
                        []
                  OnElement =
                    fun () ->
                        elements <- elements + 1
                        reporter.Report(StructureReadStage.BuildingTree, 0, 0, elements) }

            let tagged = walkChildren structTreeRoot context None 0
            // And what its pages draw outside every tag, for a person to mark (see UntaggedContent).
            let outside = try UntaggedContent.nodes pdfPath with _ -> []
            // (Read on its own: if it can't be, the tags are shown without it.)
            let disconnected = try DisconnectedContent.nodes pdfPath context.TextIndex with _ -> []
            let children = Array.concat [tagged; Array.ofList outside; Array.ofList disconnected]
            { Tag = "StructTreeRoot"
              Role = null
              AlternateDescription = null
              ActualText = null
              Content = null
              FormFieldName = null
              FormFieldDescription = null
              FormFieldType = null
              FormFieldValue = null
              LinkUri = null
              LinkDestinationPageNumber = Nullable()
              LinkDestinationDescription = null
              LinkDestination = null
              LinkDescription = null
              Geometry = children |> Seq.choose (fun child -> Option.ofObj child.Geometry) |> StructureTreeObjects.unionGeometry |> Option.toObj
              Language = null
              PageNumber = Nullable()
              ContentLeafCount = 0
              ColSpan = 1
              RowSpan = 1
              Children = children }
    finally
        doc.close ()
