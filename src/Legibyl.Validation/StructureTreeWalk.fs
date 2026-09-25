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
      DestinationDescription: string | null }

type private LinkAnnotationTarget = { Geometry: StructureGeometry; Target: LinkTargetInfo }

type private WalkContext =
    { PageNumberByKey: IReadOnlyDictionary<string, int>
      TextIndex: MarkedContentTextIndex.Index
      LinkAnnotationTargets: LinkAnnotationTarget list }

let private atom (value: string) = ASAtom.getASAtom value

let private atomValue (value: ASAtom | null) = value |?> _.getValue()

let private normalizedGeometry pageNumber left bottom right top =
    { PageNumber = pageNumber
      Left = min left right
      Bottom = min bottom top
      Right = max left right
      Top = max bottom top }

/// Unions geometries that are all on one page; None if there are none or they span pages.
let private unionGeometry (geometries: StructureGeometry seq) =
    match List.ofSeq geometries with
    | [] -> None
    | first :: _ as list when list |> List.forall (fun g -> g.PageNumber = first.PageNumber) ->
        Some
            { PageNumber = first.PageNumber
              Left = list |> List.map _.Left |> List.min
              Bottom = list |> List.map _.Bottom |> List.min
              Right = list |> List.map _.Right |> List.max
              Top = list |> List.map _.Top |> List.max }
    | _ -> None

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

let rec private bboxFromAttributes (attributes: COSObject | null) pageNumber =
    match attributes with
    | null -> None
    | attributes when attributes.empty () -> None
    | attributes when attributes.knownKey(atom "BBox").booleanValue() ->
        geometryFromCosRect pageNumber (attributes.getKey(atom "BBox"))
    | attributes ->
        let rec tryItem i =
            if i >= cosSize attributes then
                None
            else
                let found =
                    try
                        Ok(bboxFromAttributes (attributes.at i) pageNumber)
                    with :? java.lang.Exception ->
                        // Not an array-like attributes object; leave geometry unavailable.
                        Error()
                match found with
                | Ok(Some geometry) -> Some geometry
                | Ok None -> tryItem (i + 1)
                | Error() -> None
        tryItem 0

let private layoutBBoxGeometry (elem: PDStructElem) (pageNumber: int option) =
    pageNumber |> Option.bind (bboxFromAttributes (elem.getKey(atom "A")))

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

let private readFormFieldInfo (child: obj) (ownPage: int option) =
    referencedAnnotation child "Widget"
    |> Option.map (fun referenced ->
        let value =
            match nullIfEmpty (referenced.getStringKey(atom "V")) with
            | null -> atomValue (referenced.getNameKey(atom "V"))
            | value -> value

        { Name = nullIfEmpty (referenced.getStringKey(atom "T"))
          Description = nullIfEmpty (referenced.getStringKey(atom "TU"))
          Type = atomValue (referenced.getNameKey(atom "FT"))
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
        Some { Uri = null; DestinationPageNumber = pageNumber; DestinationDescription = description }

let private uriTarget (uri: string) =
    { Uri = uri; DestinationPageNumber = None; DestinationDescription = $"URI: {uri}" }

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
                  DestinationDescription = node.LinkDestinationDescription }
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
            Some { Uri = null; DestinationPageNumber = pageNumber; DestinationDescription = description }

let private buildLinkAnnotationIndex (pdfPath: string) =
    use document = new PdfDocument(new PdfReader(pdfPath))
    let pageNumberByObject = StructureChecks.pageNumberIndex document
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
               | :? PDStructElem as elem -> toStructureNode elem context inheritedPage depth
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
    let contentTexts = contentInfos |> List.map _.Text |> List.filter (not << String.IsNullOrEmpty)
    let formField = leaves |> List.tryPick (fun child -> readFormFieldInfo child ownPage)

    let children = walkChildren elem context ownPage (depth + 1)

    let geometry =
        (contentInfos |> List.choose _.Geometry) @ (children |> Seq.choose (fun child -> Option.ofObj child.Geometry) |> List.ofSeq)
        |> unionGeometry
        |> Option.orElse (formField |> Option.bind _.Geometry)
        |> Option.orElseWith (fun () -> layoutBBoxGeometry elem ownPage)

    let linkTarget =
        leaves
        |> List.tryPick (fun child -> readLinkTargetInfo child context)
        |> Option.orElseWith (fun () ->
            match geometry with
            | Some geometry when tag = "Link" -> linkTargetByGeometry geometry context
            | _ -> None)
        |> Option.orElseWith (fun () -> if tag = "TOCI" then firstDescendantLinkTarget children else None)

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
      Geometry = Option.toObj geometry
      Language = nullIfEmpty (elem.getLang () |?> _.toString())
      PageNumber = Option.toNullable ownPage
      ContentLeafCount = leaves.Length
      Children = children }

/// Reads the structure tree of the PDF at pdfPath, or null if it has no StructTreeRoot.
let read (pdfPath: string) : StructureNode | null =
    let doc = PDDocument(pdfPath)
    try
        match doc.getStructTreeRoot () with
        | null -> null
        | structTreeRoot ->
            // Text and link targets come from separate parses (PdfPig, iText). If either
            // fails (e.g. a PDF quirk that library can't parse but veraPDF tolerates),
            // degrade gracefully: keep the structure tree itself, just without that detail.
            let context =
                { PageNumberByKey = pageNumberIndex doc
                  TextIndex =
                    try
                        MarkedContentTextIndex.build pdfPath
                    with _ ->
                        MarkedContentTextIndex.Index()
                  LinkAnnotationTargets =
                    try
                        buildLinkAnnotationIndex pdfPath
                    with _ ->
                        [] }

            let children = walkChildren structTreeRoot context None 0
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
              Geometry = children |> Seq.choose (fun child -> Option.ofObj child.Geometry) |> unionGeometry |> Option.toObj
              Language = null
              PageNumber = Nullable()
              ContentLeafCount = 0
              Children = children }
    finally
        doc.close ()
