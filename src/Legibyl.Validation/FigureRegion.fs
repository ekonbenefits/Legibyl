/// Replace Structure with Figure from Region: what's drawn in a rectangle a person draws on a
/// tagged page (a chart tagged as dozens of paragraphs and spans, say) made one Figure with the
/// alt text they give, the page drawn the same. Each piece of content (a marked-content sequence,
/// or a text object, path or image outside any) goes in whole when most of each of its painted
/// boxes is inside the rectangle, and is left out otherwise; whatever can't be changed safely is
/// refused, and said. The Figure gets the box of what it draws (its Layout /BBox), and goes where
/// its first content was read.
module internal Legibyl.Validation.FigureRegion

open System
open System.Collections.Generic
open System.IO
open System.Text
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Tagging
open Legibyl.Validation.ContentOperations
open Legibyl.Validation.StructureTreeObjects

[<Literal>]
let Tag = "FigureRegion"

/// Tags that hold others and say nothing of their own (a section, a part of the document): a
/// Figure can go among their kids, and they're left when emptied.
let private containers = set [ "Document"; "DocumentFragment"; "Part"; "Art"; "Sect"; "Div"; "Aside"; "NonStruct" ]

/// A table's and a list's structure: kept even when emptied (an empty cell is still a cell), and a
/// Figure replacing all of one's content goes inside it.
let private frames = set [ "Table"; "THead"; "TBody"; "TFoot"; "TR"; "TH"; "TD"; "L"; "LI"; "Lbl"; "LBody" ]

let private holds (tag: string) = containers.Contains tag || frames.Contains tag

type Part =
    { First: int
      Last: int
      Mcid: int option
      Marked: bool
      /// Its marked content had replacement text (/ActualText), which the alt text replaces.
      Described: bool
      /// Its marked content's language (/Lang), if it said.
      Lang: string option }

type Plan =
    { Parts: Part list
      Bytes: byte array
      Operations: List<Operation>
      Owners: (int * PdfDictionary) list
      /// What the selected content draws: the Figure's Layout /BBox.
      Box: iText.Kernel.Geom.Rectangle option
      /// The first MCID free on the page: past every one its content uses, in the tags or not.
      NextMcid: int
      /// The language the selected content and the tags it empties say, if they say one.
      Lang: string option
      Summary: string }

let private intersects (region: StructureGeometry) (box: iText.Kernel.Geom.Rectangle) =
    float (box.GetRight()) >= region.Left && float (box.GetLeft()) <= region.Right
    && float (box.GetTop()) >= region.Bottom && float (box.GetBottom()) <= region.Top

let private contains (region: StructureGeometry) (box: iText.Kernel.Geom.Rectangle) =
    float (box.GetLeft()) >= region.Left && float (box.GetRight()) <= region.Right
    && float (box.GetBottom()) >= region.Bottom && float (box.GetTop()) <= region.Top

/// Each painted box is assessed separately: a sparse text object or marked sequence must not
/// qualify just because its large aggregate bounds overlap the selection.
let private coverage (region: StructureGeometry) (box: iText.Kernel.Geom.Rectangle) =
    let left, bottom, right, top =
        float (box.GetLeft()), float (box.GetBottom()), float (box.GetRight()), float (box.GetTop())
    if not ([left; bottom; right; top] |> List.forall Double.IsFinite) || right < left || top < bottom then None
    elif right = left || top = bottom then Some(if contains region box then 1.0 else 0.0)
    else
        let width = right - left
        let height = top - bottom
        let coveredWidth = max 0.0 (min region.Right right - max region.Left left)
        let coveredHeight = max 0.0 (min region.Top top - max region.Bottom bottom)
        Some(coveredWidth * coveredHeight / (width * height))

/// A Figure's Layout /BBox (in its /A, a dictionary or an array of them).
let private layoutBox (element: PdfDictionary) =
    let attributes =
        match element.Get(PdfName.A, true) with
        | :? PdfDictionary as a -> [ a ]
        | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do match array.Get(i, true) with :? PdfDictionary as a -> a | _ -> () ]
        | _ -> []
    attributes
    |> List.tryPick (fun a ->
        match a.GetAsArray PdfName.BBox with
        | NonNull box when box.Size() = 4 -> Some [| for i in 0..3 -> box.GetAsNumber(i).DoubleValue() |]
        | _ -> None)

/// Whether the edit's Figure is there already (a copy exported with it, exported again): a Figure
/// on its page with its alt text, whose box (Layout /BBox) is centred in the region. (Its box can
/// reach outside the region: content goes in whole when most of it is inside.)
let already (document: PdfDocument) context (edit: ElementEdit) =
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> false
    | root ->
        walk root context
        |> Seq.exists (fun visit ->
            let e = visit.Element
            visit.Page = Some edit.Page
            && PdfName.Figure.Equals(e.GetAsName PdfName.S)
            && (match e.GetAsString PdfName.Alt with NonNull alt -> alt.ToUnicodeString() = edit.Alt | Null -> false)
            && (match layoutBox e with
                | Some [| l; b; r; t |] ->
                    let x, y = (l + r) / 2.0, (b + t) / 2.0
                    x >= edit.Box.Left && x <= edit.Box.Right && y >= edit.Box.Bottom && y <= edit.Box.Top
                | _ -> false))

/// Whole marked-content sequences and unmarked drawing units are indivisible. Include one only
/// when every actual painted box in it is more than half covered by the selection.
let plan (document: PdfDocument) context (edit: ElementEdit) : Result<Plan, string> =
    try
        if edit.Page < 1 || edit.Page > document.GetNumberOfPages() then
            Error "the page doesn't exist"
        elif edit.Box.PageNumber <> edit.Page
             || not ([edit.Box.Left; edit.Box.Bottom; edit.Box.Right; edit.Box.Top] |> List.forall Double.IsFinite)
             || edit.Box.Right <= edit.Box.Left || edit.Box.Top <= edit.Box.Bottom then
            Error "the region isn't a rectangle on its page"
        elif String.IsNullOrWhiteSpace edit.Alt then
            Error "enter alt text describing the figure"
        elif (not (isNull edit.Retag) && edit.Retag <> "Figure")
             || not (isNull edit.Text) || edit.Exclude || edit.Move.IsSome || not edit.MergeWith.IsEmpty || not edit.Cells.IsEmpty then
            Error "this edit only takes a page, a region and alt text"
        elif isNull (document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot) then
            Error "the PDF has no tags: Suggest Structure… tags it from its layout"
        else
            let page = document.GetPage edit.Page
            if page.GetRotation() <> 0 then
                Error "turned (rotated) pages aren't supported yet"
            elif page.GetCropBox().GetLeft() <> 0f || page.GetCropBox().GetBottom() <> 0f then
                Error "pages whose visible area is offset (their crop box) aren't supported yet"
            else
                let bytes = page.GetContentBytes()
                let resources = page.GetResources()
                let operations, _ = operationsOf bytes resources
                let parts = ResizeArray<Part>()
                let mutable why = None
                let fail reason = if why.IsNone then why <- Some reason
                let hasMissingXObject =
                    operations
                    |> Seq.exists (fun op ->
                        if op.Name <> "Do" then false
                        else
                            match op.Operands with
                            | [| :? PdfName as name |] ->
                                match resources.GetResource PdfName.XObject with
                                | null -> true
                                | xobjects -> isNull (xobjects.GetAsStream name)
                            | _ -> true)
                if hasMissingXObject then
                    fail "something on the page can't be measured (an image or form that can't be read), so what's in the region isn't known"
                let drawn = if hasMissingXObject then Dictionary<int, List<Drawn>>() else drawnForRegion bytes resources
                let owners = (ParentTree.tagged document context).MarkedContent
                let beyond = ResizeArray<iText.Kernel.Geom.Rectangle>()
                // What the selected content draws, for the Figure's box.
                let selectedBoxes = ResizeArray<iText.Kernel.Geom.Rectangle>()
                // Every operator some piece covers, considered or not: what's drawn and covered by
                // none can't be told in or out.
                let covered = HashSet<int>()
                // (A shading is measured by the clip it's painted in: see DrawnByOperator.)
                for index in 0 .. operations.Count - 1 do
                    let op = operations[index]
                    if (op.Name = "Do" || op.Name = inlineImage || op.Name = "sh" || textShowing.Contains op.Name) && not (drawn.ContainsKey index) then
                        fail "something on the page can't be measured (an image or form that can't be read), so what's in the region isn't known"
                let inspect first last mcid marked described lang supported =
                    for k in first .. last do covered.Add k |> ignore
                    let boxes =
                        [ for i in first .. last do
                              match drawn.TryGetValue i with
                              | true, found -> yield! found |> Seq.map _.Box
                              | _ -> () ]
                    let coverages = boxes |> List.map (coverage edit.Box)
                    if coverages |> List.exists Option.isNone then
                        fail "something on the page can't be measured (an image or form that can't be read), so what's in the region isn't known"
                    elif not boxes.IsEmpty && (coverages |> List.forall (Option.exists (fun amount -> amount > 0.5))) then
                        if not supported then
                            fail "some content in the region is marked in a way that can't be changed safely (marked inside other marked content, or with properties beyond its description)"
                        else
                            match mcid with
                            | Some id when not (owners.ContainsKey((edit.Page, id))) ->
                                fail "some content in the region is marked as tagged, but isn't in any tag"
                            | _ ->
                                parts.Add { First = first; Last = last; Mcid = mcid; Marked = marked; Described = described; Lang = lang }
                                selectedBoxes.AddRange boxes
                                beyond.AddRange(boxes |> List.filter (not << contains edit.Box))
                let mutable i = 0
                while i < operations.Count do
                    let op = operations[i]
                    if op.Name = "BMC" || op.Name = "BDC" then
                        let first = i
                        let mutable depth = 1
                        let mutable nested = false
                        i <- i + 1
                        while i < operations.Count && depth > 0 do
                            match operations[i].Name with
                            | "BMC" | "BDC" -> depth <- depth + 1; nested <- true
                            | "EMC" -> depth <- depth - 1
                            | _ -> ()
                            i <- i + 1
                        if depth <> 0 then fail "the page's marked content isn't closed properly"
                        else
                            let artifact = op.Operands.Length > 0 && PdfName.Artifact.Equals(op.Operands[0])
                            let mcid = mcidOf resources op
                            let properties =
                                if op.Name <> "BDC" || op.Operands.Length < 2 then None
                                else
                                    match op.Operands[1] with
                                    | :? PdfDictionary as d -> Some d
                                    | :? PdfName as name ->
                                        resources.GetResource PdfName.Properties |> Option.ofObj
                                        |> Option.bind (fun all -> all.GetAsDictionary name |> Option.ofObj)
                                    | _ -> None
                            let appearanceSafe =
                                properties
                                |> Option.forall (fun p ->
                                    p.KeySet()
                                    |> Seq.forall (fun key ->
                                        [PdfName.MCID; PdfName.ActualText; PdfName.Alt; PdfName.Lang; PdfName.Type; PdfName.Subtype; PdfName.BBox; PdfName "Attached"]
                                        |> List.exists key.Equals))
                            let described =
                                properties
                                |> Option.exists (fun p ->
                                    match p.GetAsString PdfName.ActualText with
                                    | NonNull text -> not (String.IsNullOrWhiteSpace(text.ToUnicodeString()))
                                    | Null -> false)
                            let lang =
                                properties
                                |> Option.bind (fun p -> Option.ofObj (p.GetAsString PdfName.Lang))
                                |> Option.map _.ToUnicodeString()
                                |> Option.filter (String.IsNullOrWhiteSpace >> not)
                            inspect first (i - 1) mcid true described lang (not nested && appearanceSafe && (artifact || mcid.IsSome))
                    elif op.Name = "EMC" then
                        fail "the page's marked content isn't closed properly"
                        i <- i + 1
                    else i <- i + 1
                // Reuse the existing safe boundaries for text objects, paths and images.
                if why.IsNone then
                    for piece in UntaggedContent.piecesOn page edit.Page do
                        for unit in piece.Units do
                            inspect unit.First unit.Last None false false None true
                // Something drawn in the region that no piece covers (text outside marked content,
                // in a text object that also holds some): it can't be told in or out, so it's refused.
                if why.IsNone then
                    for index in 0 .. operations.Count - 1 do
                        let op = operations[index]
                        let paints =
                            op.Name = "Do" || op.Name = inlineImage || op.Name = "sh" || textShowing.Contains op.Name
                            || (pathPainting.Contains op.Name && op.Name <> "n")
                        if paints && not (covered.Contains index) then
                            match drawn.TryGetValue index with
                            | true, found when found |> Seq.exists (fun d -> intersects edit.Box d.Box) ->
                                fail "some of what's drawn in the region is marked only in part (text in a text object that's partly marked, say), so it can't be told in or out"
                            | _ -> ()
                let selected = List.ofSeq parts
                // A form may carry its own tags, shared across invocations. Don't relabel it
                // through a page MCID and silently orphan its stream parent-tree entries.
                for part in selected do
                    for i in part.First .. part.Last do
                        let op = operations[i]
                        if op.Name = "Do" && op.Operands.Length > 0 then
                            match op.Operands[0] with
                            | :? PdfName as name ->
                                let xobjects = resources.GetResource PdfName.XObject
                                if not (isNull xobjects) then
                                    match xobjects.GetAsStream name with
                                    | NonNull stream when PdfName.Form.Equals(stream.GetAsName PdfName.Subtype) ->
                                        fail "the region includes a form (a drawing reused from elsewhere, which can carry tags of its own), which needs a person to check"
                                    | _ -> ()
                            | _ -> fail "an image or form on the page can't be read"
                let ids = selected |> List.choose _.Mcid
                if ids.Length <> (List.distinct ids).Length then fail "some content in the region is marked twice"
                let occurrences =
                    operations |> Seq.choose (mcidOf resources) |> Seq.countBy id |> dict
                for id in ids do
                    if occurrences[id] <> 1 then
                        fail "some content in the region shares its mark with other content on the page"
                // Require exactly one page-content reference in the tree, not just the last
                // dictionary in a lookup which might hide duplicate ownership.
                let ownerPairs = ids |> List.map (fun id -> id, owners[(edit.Page, id)])
                for owner, selectedIds in ownerPairs |> List.groupBy snd |> List.map (fun (owner, pairs) -> owner, pairs |> List.map fst |> Set.ofList) do
                    let tag = standardTag owner context |> Option.ofObj |> Option.map _.GetValue() |> Option.defaultValue ""
                    if not (holds tag) then
                        let remaining =
                            PdfStructElem(owner).GetKids()
                            |> Seq.exists (fun kid ->
                                match kid with
                                | :? PdfMcr as mcr ->
                                    not (selectedIds.Contains(mcr.GetMcid())
                                         && obj.ReferenceEquals(mcr.GetPageObject(), page.GetPdfObject()))
                                | _ -> true)
                        if remaining then
                            fail "the region takes only part of a paragraph or other tag: make it cover all of that tag's content, or none of it"
                let visits = walk (document.GetStructTreeRoot().GetPdfObject()) context |> List.ofSeq
                for id, owner in ownerPairs do
                    let references =
                        visits |> Seq.collect (fun v -> PdfStructElem(v.Element).GetKids())
                        |> Seq.filter (fun kid ->
                            match kid with
                            | :? PdfMcr as mcr -> mcr.GetMcid() = id && obj.ReferenceEquals(mcr.GetPageObject(), page.GetPdfObject())
                            | _ -> false)
                        |> Seq.length
                    if references <> 1 then fail "some content in the region is in more than one tag"
                    if not (isNull (owner.Get PdfName.ActualText) && isNull (owner.Get PdfName.Alt)) then
                        let hasRemaining =
                            PdfStructElem(owner).GetKids()
                            |> Seq.exists (fun kid ->
                                match kid with
                                | :? PdfMcr as mcr -> not (ids |> List.contains (mcr.GetMcid())) || not (obj.ReferenceEquals(mcr.GetPageObject(), page.GetPdfObject()))
                                | _ -> true)
                        if hasRemaining then fail "a tag in the region has alt or replacement text that also covers content outside it"
                    let mutable parent = PdfStructElem(owner).GetParent()
                    let mutable child = owner
                    let mutable depth = 0
                    while parent :? PdfStructElem && depth < 200 do
                        let element = parent :?> PdfStructElem
                        if not (element.GetKids() |> Seq.exists (fun k -> match k with :? PdfStructElem as e -> obj.ReferenceEquals(e.GetPdfObject(), child) | _ -> false)) then
                            fail "the tags around the region are inconsistent (a tag names another parent than the one holding it)"
                        if not (isNull (element.GetPdfObject().Get PdfName.ActualText)) then
                            fail "a tag around the region has replacement text, which would hide the Figure's alt text"
                        child <- element.GetPdfObject()
                        parent <- element.GetParent()
                        depth <- depth + 1
                    if depth = 200 then fail "the tags around the region loop back on themselves"
                    elif not (parent :? PdfStructTreeRoot) then
                        fail "a tag around the region doesn't name its parent"
                    elif not ((parent :?> PdfStructTreeRoot).GetKids() |> Seq.exists (fun k -> match k with :? PdfStructElem as e -> obj.ReferenceEquals(e.GetPdfObject(), child) | _ -> false)) then
                        fail "the tags around the region are inconsistent (a tag names another parent than the one holding it)"
                for annotation in page.GetAnnotations() do
                    match annotation.GetRectangle() with
                    | NonNull rect when intersects edit.Box (rect.ToRectangle()) ->
                        fail "the region includes a link, form field or other annotation, which a Figure can't hold"
                    | _ -> ()
                match why with
                | Some reason -> Error reason
                | None when selected.IsEmpty -> Error "nothing drawn was found in the region"
                | None ->
                    let text = UntaggedContent.textIn page edit.Box |> Option.defaultValue "(its text couldn't be read)"
                    let coordinates (box: iText.Kernel.Geom.Rectangle) =
                        [ box.GetLeft(); box.GetBottom(); box.GetRight(); box.GetTop() ]
                        |> List.map (fun value -> float value |> fun n -> n.ToString("0.##", Globalization.CultureInfo.InvariantCulture))
                        |> String.concat ", "
                    let extended =
                        if beyond.Count = 0 then ""
                        else
                            let boxes = beyond |> Seq.map (fun box -> $"[{coordinates box}]") |> String.concat "; "
                            $" Some of it reaches outside the rectangle (each piece goes in whole when most of it is inside), at left, bottom, right, top in points: {boxes}."
                    // Replacement text the alt text takes the place of: on the marked content, or on a tag
                    // whose content all goes (and the tag with it).
                    let ownerText =
                        ownerPairs
                        |> List.exists (fun (_, owner) ->
                            match owner.GetAsString PdfName.ActualText with
                            | NonNull text -> not (String.IsNullOrWhiteSpace(text.ToUnicodeString()))
                            | Null -> false)
                    let replaced =
                        if ownerText || (selected |> List.exists _.Described) then " Replacement text on some of it is replaced by your alt text." else ""
                    // Its language: what the selected content and the tags it empties say, if one.
                    let langs =
                        [ yield! selected |> List.choose _.Lang
                          for _, owner in ownerPairs do
                              match owner.GetAsString PdfName.Lang with
                              | NonNull lang when not (String.IsNullOrWhiteSpace(lang.ToUnicodeString())) -> lang.ToUnicodeString()
                              | _ -> () ]
                        |> List.distinct
                    let lang = match langs with [ one ] -> Some one | _ -> None
                    let languages =
                        if langs.Length > 1 then
                            $""" Parts of it say different languages ({String.Join(", ", langs)}): the Figure says none, so its alt text is read in the document's."""
                        else ""
                    let union =
                        selectedBoxes
                        |> Seq.reduce (fun a b ->
                            iText.Kernel.Geom.Rectangle.GetCommonRectangle([| a; b |]))
                    // Past every MCID the page's content uses (in the tags or not), and the tags'.
                    let used =
                        Seq.append occurrences.Keys (owners.Keys |> Seq.filter (fun (p, _) -> p = edit.Page) |> Seq.map snd)
                    let nextMcid = if Seq.isEmpty used then 0 else Seq.max used + 1
                    Ok { Parts = selected; Bytes = bytes; Operations = operations; Owners = ownerPairs
                         Box = Some union
                         NextMcid = nextMcid
                         Lang = lang
                         Summary =
                           $"{selected.Length} piece(s) of content, {ids.Length} of them tagged.{extended}{replaced}{languages} Text in the region: {text}" }
    with
    | :? iText.Kernel.Exceptions.PdfException as ex ->
        Error $"the page content couldn't be read safely: {ex.Message}"
    | :? IOException as ex ->
        Error $"the page content couldn't be read safely: {ex.Message}"
    | :? UnauthorizedAccessException as ex ->
        Error $"the page content couldn't be read safely: {ex.Message}"

let apply (document: PdfDocument) context (edit: ElementEdit) =
    if already document context edit then false
    else
        match plan document context edit with
        | Error reason ->
            context.Notes.Add $"Your Figure from region on page {edit.Page} wasn't applied: {reason}."
            false
        | Ok plan ->
            let page = document.GetPage edit.Page
            let root = document.GetStructTreeRoot()
            let tagOf (dictionary: PdfDictionary) = standardTag dictionary context |> Option.ofObj |> Option.map _.GetValue() |> Option.defaultValue ""
            let figure = PdfStructElem(document, PdfName.Figure, page)
            figure.SetAlt(PdfString(edit.Alt, iText.IO.Font.PdfEncodings.UNICODE_BIG)) |> ignore
            // The language its content said, kept on the Figure (its alt text is in it).
            plan.Lang |> Option.iter (fun lang -> figure.GetPdfObject().Put(PdfName.Lang, PdfString lang) |> ignore)
            // Where it's drawn: its Layout /BBox, which a Figure on one page has (ISO 32000, 14.8.5.4.3).
            match plan.Box with
            | Some box ->
                let layout = PdfDictionary()
                layout.Put(PdfName.O, PdfName.Layout) |> ignore
                let edges = [| box.GetLeft(); box.GetBottom(); box.GetRight(); box.GetTop() |] |> Array.map (fun v -> Math.Round(float v, 2))
                layout.Put(PdfName.BBox, PdfArray(edges)) |> ignore
                figure.GetPdfObject().Put(PdfName.A, layout) |> ignore
            | None -> ()
            let isMcrOf (ids: Set<int>) (kid: IStructureNode) =
                match kid with
                | :? PdfMcr as mcr -> ids.Contains(mcr.GetMcid()) && obj.ReferenceEquals(mcr.GetPageObject(), page.GetPdfObject())
                | _ -> false
            let isElement (dictionary: PdfDictionary) (kid: IStructureNode) =
                match kid with
                | :? PdfStructElem as e -> obj.ReferenceEquals(e.GetPdfObject(), dictionary)
                | _ -> false
            let selectedIds = plan.Owners |> List.map fst |> Set.ofList
            // Into a tag's place among its parent's kids (before or after it), or the root's.
            let besides (dictionary: PdfDictionary) after =
                match PdfStructElem(dictionary).GetParent() with
                | :? PdfStructElem as parent ->
                    let index = parent.GetKids() |> Seq.tryFindIndex (isElement dictionary) |> Option.defaultValue (parent.GetKids().Count)
                    parent.AddKid(index + (if after then 1 else 0), figure) |> ignore
                | _ ->
                    let index = root.GetKids() |> Seq.tryFindIndex (isElement dictionary) |> Option.defaultValue (root.GetKids().Count)
                    root.AddKid(index + (if after then 1 else 0), figure) |> ignore
            // The Figure goes where its first content was read: the first selected content met going
            // through each tag's kids in order (the tag holding it, and its place there).
            let rec firstIn (element: PdfStructElem) depth : (PdfStructElem * int) option =
                if depth > 200 then None
                else
                    element.GetKids()
                    |> Seq.indexed
                    |> Seq.tryPick (fun (index, kid) ->
                        match kid with
                        | kid when isMcrOf selectedIds kid -> Some(element, index)
                        | :? PdfStructElem as child -> firstIn child (depth + 1)
                        | _ -> None)
            let first =
                root.GetKids()
                |> Seq.tryPick (fun kid -> match kid with :? PdfStructElem as top -> firstIn top 0 | _ -> None)
            match first with
            | Some(owner, index) when holds (tagOf (owner.GetPdfObject())) ->
                // A section, cell or item holding the content itself: at that content's place.
                owner.AddKid(index, figure) |> ignore
            | Some(owner, _) ->
                // A tag all of whose content it takes: in that tag's place.
                besides (owner.GetPdfObject()) false
            | None ->
                // Only content outside the tags: after what was drawn before it.
                let resources = page.GetResources()
                let start = (plan.Parts |> List.minBy _.First).First
                match UntaggedContent.elementBefore document context edit.Page plan.Operations resources start with
                | Some(at, previous) when holds (tagOf previous) ->
                    let element = PdfStructElem previous
                    let ids = mcidOf resources plan.Operations[at] |> Option.toList |> Set.ofList
                    match element.GetKids() |> Seq.tryFindIndex (isMcrOf ids) with
                    | Some index -> element.AddKid(index + 1, figure) |> ignore
                    | None -> element.AddKid(element.GetKids().Count, figure) |> ignore
                | Some(_, previous) -> besides previous true
                | None ->
                    // Nothing before it: first, in the top tag if that's a section (a Document, say).
                    match root.GetKids() |> Seq.tryHead with
                    | Some(:? PdfStructElem as top) when holds (tagOf (top.GetPdfObject())) -> top.AddKid(0, figure) |> ignore
                    | _ -> root.AddKid(0, figure) |> ignore
            let replacements = Dictionary<int, string>()
            let after = Dictionary<int, string>()
            let mutable nextMcid = plan.NextMcid
            for part in plan.Parts |> List.sortBy _.First do
                let id =
                    match part.Mcid with
                    | Some id ->
                        let owner = PdfStructElem(snd (plan.Owners |> List.find (fun (mcid, _) -> mcid = id)))
                        let index = owner.GetKids() |> Seq.findIndex (isMcrOf (Set.singleton id))
                        owner.RemoveKid(index, true) |> ignore
                        let dictionary = PdfDictionary()
                        dictionary.Put(PdfName.Type, PdfName.MCR) |> ignore
                        dictionary.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
                        dictionary.Put(PdfName.MCID, PdfNumber id) |> ignore
                        figure.AddKid(PdfMcrDictionary(dictionary, figure)) |> ignore
                        id
                    | None ->
                        // Numbered past every MCID the page uses: iText only knows those in the tags.
                        let id = nextMcid
                        nextMcid <- nextMcid + 1
                        figure.AddKid(PdfMcrNumber(PdfNumber id, figure)) |> ignore
                        id
                replacements[part.First] <- $"\n/Figure <</MCID {id}>> BDC\n"
                if not part.Marked then after[part.Last] <- "\nEMC\n"
            use output = new MemoryStream()
            let write (s: string) = let b = Encoding.ASCII.GetBytes s in output.Write(b, 0, b.Length)
            for i in 0 .. plan.Operations.Count - 1 do
                let op = plan.Operations[i]
                match replacements.TryGetValue i with
                | true, replacement ->
                    write replacement
                    if not (plan.Parts |> List.exists (fun p -> p.First = i && p.Marked)) then
                        output.Write(plan.Bytes, int op.Start, int (op.End - op.Start))
                | _ -> output.Write(plan.Bytes, int op.Start, int (op.End - op.Start))
                match after.TryGetValue i with true, text -> write text | _ -> ()
            let last = if plan.Operations.Count = 0 then 0L else plan.Operations[plan.Operations.Count - 1].End
            output.Write(plan.Bytes, int last, plan.Bytes.Length - int last)
            page.GetPdfObject().Put(PdfName.Contents, PdfStream(output.ToArray()).MakeIndirect document) |> ignore
            // Tags this emptied go, and the tags around them it emptied too; but never a section, nor
            // a table's or list's structure (an empty cell is still a cell), nor anything with content left.
            for _, dictionary in plan.Owners |> List.distinctBy snd do
                let mutable owner = PdfStructElem dictionary
                let mutable pruning = true
                while pruning && owner.GetKids().Count = 0 && not (holds (tagOf (owner.GetPdfObject()))) do
                    match owner.GetParent() with
                    | :? PdfStructElem as parent ->
                        parent.RemoveKid owner |> ignore
                        owner <- parent
                    | :? PdfStructTreeRoot as top ->
                        // A tag straight under the root (iText's root has no RemoveKid).
                        let element = owner.GetPdfObject()
                        match resolve (top.GetPdfObject().Get PdfName.K) with
                        | :? PdfArray as kids ->
                            for i in kids.Size() - 1 .. -1 .. 0 do
                                if obj.ReferenceEquals(resolve (kids.Get i), element) then kids.Remove i
                        | :? PdfDictionary as only when obj.ReferenceEquals(only, element) -> top.GetPdfObject().Remove PdfName.K |> ignore
                        | _ -> ()
                        pruning <- false
                    | _ -> pruning <- false
            true
