/// Works out the page region a veraPDF finding refers to, from its location context, so
/// it can be highlighted like Legibyl's own warnings.
module internal Legibyl.Validation.FindingGeometry

open System
open System.Text.RegularExpressions
open iText.Kernel.Pdf
open Legibyl.Validation.Interop
open Legibyl.Validation.StructureTreeObjects

let private mcidPattern = Regex(@"\{mcid:(\d+)\}", RegexOptions.Compiled)

/// Content inside a form XObject numbers its MCIDs separately from the page, so the
/// page's (page, MCID) index can't be trusted for it.
let private isInsideXObject (context: string) =
    context.Contains("xobject", StringComparison.OrdinalIgnoreCase)

/// "pages[0](…)/contentStream[0](…)/content[10]{mcid:22}": the marked content's glyphs.
let private markedContentGeometry context (locationContext: string) (location: PdfLocation) =
    let matches = mcidPattern.Matches locationContext
    if matches.Count = 0 || not location.PageNumber.HasValue || isInsideXObject locationContext then
        None
    else
        let mcid = int matches[matches.Count - 1].Groups[1].Value
        lookup context.MarkedContent (location.PageNumber.Value, mcid) |> Option.bind _.Geometry

/// ".../K[3](12 0 obj SEP P)" or ".../annots[0](9 0 obj PDLinkAnnot)": the object itself.
let private objectGeometry context (document: PdfDocument) (location: PdfLocation) =
    if not location.ObjectNumber.HasValue || location.ObjectNumber.Value >= document.GetNumberOfPdfObjects() then
        None
    else
        let locationPage = Option.ofNullable location.PageNumber
        match document.GetPdfObject(location.ObjectNumber.Value) with
        | :? PdfDictionary as dictionary when not (isNull (dictionary.GetAsArray(PdfName.Rect))) ->
            let page =
                locationPage
                |> Option.orElseWith (fun () ->
                    dictionary.GetAsDictionary(PdfName.P) |?> _.GetIndirectReference()
                    |> Option.ofObj
                    |> Option.bind (fun reference -> lookup context.PageNumberByObject (reference.GetObjNumber())))
            page |> Option.bind (fun page -> rectGeometry page dictionary)
        | :? PdfDictionary as dictionary when not (isNull (dictionary.GetAsName(PdfName.S))) ->
            let page = ancestorPage dictionary context |> Option.orElse locationPage
            subtreeGeometry dictionary context page
        | _ -> None

/// "…/contentStream[0](…)/content[3]/contentItem[1]": a page's own content outside every tag.
let private unmarkedItemPattern =
    Regex(@"/contentStream\[0\][^/]*/content\[(\d+)\]/contentItem\[(\d+)\]$", RegexOptions.Compiled)

/// Where a piece of content outside every tag is drawn (see UntaggedContent.itemGeometry).
let private unmarkedGeometry (document: PdfDocument) (locationContext: string) (location: PdfLocation) =
    let item = unmarkedItemPattern.Match locationContext
    if item.Success && location.PageNumber.HasValue && not (isInsideXObject locationContext) then
        UntaggedContent.itemGeometry document location.PageNumber.Value (int item.Groups[1].Value) (int item.Groups[2].Value)
    else
        None

/// Best-effort page region for a veraPDF finding; None when its location doesn't pin
/// down anything drawn on a page (document-level checks, whole pages, etc.).
let locate context (document: PdfDocument) (violation: RuleViolation) : StructureGeometry option =
    match violation.LocationContext, violation.Location with
    | NonNull locationContext, NonNull location ->
        markedContentGeometry context locationContext location
        |> Option.orElseWith (fun () -> objectGeometry context document location)
        |> Option.orElseWith (fun () -> unmarkedGeometry document locationContext location)
    | _ -> None

/// The element a structure-tree path names ("StructTreeRoot/K[0]/K[3]", as Legibyl's warnings
/// give; the first of two joined with " + "), if it's there.
let private elementAtPath (document: PdfDocument) (path: string) : PdfDictionary option =
    let first = path.Split(" + ").[0]
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> None
    | root when first.StartsWith("StructTreeRoot", StringComparison.Ordinal) ->
        let kid (parent: PdfDictionary) (index: int) : PdfDictionary option =
            match resolve (parent.Get PdfName.K) with
            | :? PdfArray as kids when index < kids.Size() ->
                match resolve (kids.Get index) with
                | :? PdfDictionary as child -> Some child
                | _ -> None
            | :? PdfDictionary as only when index = 0 -> Some only
            | _ -> None
        first.Split('/', StringSplitOptions.RemoveEmptyEntries)
        |> Array.skip 1
        |> Array.fold
            (fun (current: PdfDictionary option) segment ->
                let m = Regex.Match(segment, @"^K\[(\d+)\]$")
                match current with
                | Some parent when m.Success -> kid parent (int m.Groups[1].Value)
                | _ -> None)
            (Some root)
        |> Option.filter (fun element -> not (obj.ReferenceEquals(element, root)))
    | _ -> None

/// What each document's tags hold, read from the structure tree: for a parent tree that's
/// missing or incomplete, where iText can't find a piece of content's element.
let private taggedByDocument = System.Runtime.CompilerServices.ConditionalWeakTable<PdfDocument, ParentTree.Tagged>()

/// The element that owns a page's marked content, by its MCID.
let private ownerOf (document: PdfDocument) context (page: int) (mcid: int) : PdfDictionary option =
    let fromStructureTree () =
        let tagged = taggedByDocument.GetValue(document, fun document -> ParentTree.tagged document context)
        match tagged.MarkedContent.TryGetValue((page, mcid)) with
        | true, element -> Some element
        | _ -> None
    match document.GetStructTreeRoot() with
    | null -> None
    | root when page >= 1 && page <= document.GetNumberOfPages() ->
        match root.GetPageMarkedContentReferences(document.GetPage page) with
        | null -> fromStructureTree ()
        | references ->
            references
            |> Seq.tryFind (fun reference -> reference.GetMcid() = mcid)
            |> Option.bind (fun reference ->
                match reference.GetParent() with
                | :? iText.Kernel.Pdf.Tagging.PdfStructElem as element -> Some(element.GetPdfObject())
                | _ -> None)
            |> Option.orElseWith fromStructureTree
    | _ -> None

/// What a finding's location holds, named (see RuleViolation.Element): the element that owns its
/// marked content, the structure element it is, or, for page content in no marked content, that
/// it isn't in the tags. None when that isn't known.
let describe context (document: PdfDocument) (violation: RuleViolation) : string option =
    match violation.LocationContext, violation.Location with
    | NonNull locationContext, NonNull location ->
        let page = Option.ofNullable location.PageNumber
        let matches = mcidPattern.Matches locationContext
        if locationContext.StartsWith("StructTreeRoot", StringComparison.Ordinal) then
            elementAtPath document locationContext |> Option.map (fun element -> describeElement element context (ancestorPage element context))
        elif matches.Count > 0 && page.IsSome && not (isInsideXObject locationContext) then
            let mcid = int matches[matches.Count - 1].Groups[1].Value
            match ownerOf document context page.Value mcid with
            | Some element -> Some(describeElement element context page)
            | None ->
                // Marked, but no element has it: what's drawn there.
                let text =
                    lookup context.MarkedContent (page.Value, mcid)
                    |> Option.map (fun content -> Regex.Replace(content.Text, @"\s+", " ").Trim())
                    |> Option.defaultValue ""
                match text with
                | "" -> Some "content not in the tags"
                | text when text.Length > 40 -> Some $"content not in the tags \"{text.Substring(0, 40).TrimEnd()}…\""
                | text -> Some $"content not in the tags \"{text}\""
        elif location.ObjectNumber.HasValue
             && (location.ObjectType |> Option.ofObj |> Option.exists (fun t -> t.StartsWith("SE", StringComparison.Ordinal)))
             && location.ObjectNumber.Value < document.GetNumberOfPdfObjects() then
            match document.GetPdfObject(location.ObjectNumber.Value) with
            | :? PdfDictionary as element when not (isNull (element.GetAsName PdfName.S)) ->
                Some(describeElement element context (ancestorPage element context |> Option.orElse page))
            | _ -> None
        elif page.IsSome && locationContext.Contains("contentItem", StringComparison.Ordinal) && not (isInsideXObject locationContext) then
            Some "content not in the tags"
        else
            None
    | _ -> None
