/// Person-selected content ownership, not a retag of the wrappers around a chart.
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

let private marker = PdfName "LegibylFigureRegion"

type Part =
    { First: int
      Last: int
      Mcid: int option
      Marked: bool }

type Plan =
    { Parts: Part list
      Bytes: byte array
      Operations: List<Operation>
      Owners: (int * PdfDictionary) list
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

let already (document: PdfDocument) context (edit: ElementEdit) =
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> false
    | root ->
        walk root context
        |> Seq.exists (fun visit ->
            let e = visit.Element
            match e.GetAsArray marker, e.GetAsString PdfName.Alt with
            | NonNull box, NonNull alt when box.Size() = 5 && alt.ToUnicodeString() = edit.Alt ->
                let expected = [| float edit.Page; edit.Box.Left; edit.Box.Bottom; edit.Box.Right; edit.Box.Top |]
                // Reviews save boxes to a tenth of a point. A carried edit must still
                // recognise the exact region originally drawn before that roundtrip.
                expected |> Array.mapi (fun i n -> Math.Round(box.GetAsNumber(i).DoubleValue(), 1) = Math.Round(n, 1)) |> Array.forall id
            | _ -> false)

/// Whole marked-content sequences and unmarked drawing units are indivisible. Include one only
/// when every actual painted box in it is more than half covered by the selection.
let plan (document: PdfDocument) context (edit: ElementEdit) : Result<Plan, string> =
    try
        if edit.Page < 1 || edit.Page > document.GetNumberOfPages() then
            Error "the page doesn't exist"
        elif edit.Box.PageNumber <> edit.Page
             || not ([edit.Box.Left; edit.Box.Bottom; edit.Box.Right; edit.Box.Top] |> List.forall Double.IsFinite)
             || edit.Box.Right <= edit.Box.Left || edit.Box.Top <= edit.Box.Bottom then
            Error "the region must be a finite rectangle on the selected page"
        elif String.IsNullOrWhiteSpace edit.Alt then
            Error "enter alt text describing the figure"
        elif (not (isNull edit.Retag) && edit.Retag <> "Figure")
             || not (isNull edit.Text) || edit.Exclude || edit.Move.IsSome || not edit.MergeWith.IsEmpty || not edit.Cells.IsEmpty then
            Error "a Figure region edit accepts its page, box, Figure tag and alt text only; use separate edits for other changes"
        elif isNull (document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot) then
            Error "this operation needs a tagged PDF; use Suggest Structure for an untagged PDF"
        else
            let page = document.GetPage edit.Page
            if page.GetRotation() <> 0 then
                Error "rotated pages aren't supported for this operation"
            elif page.GetCropBox().GetLeft() <> 0f || page.GetCropBox().GetBottom() <> 0f then
                Error "pages with an offset crop box aren't supported for this operation"
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
                    fail "a painted item's extent couldn't be read; unknown content can't be treated as empty"
                let drawn = if hasMissingXObject then Dictionary<int, List<Drawn>>() else drawnForRegion bytes resources
                let owners = (ParentTree.tagged document context).MarkedContent
                let beyond = ResizeArray<iText.Kernel.Geom.Rectangle>()
                for index in 0 .. operations.Count - 1 do
                    let op = operations[index]
                    if op.Name = "sh" then
                        fail "this page contains shading whose painted extent is unknown; it needs manual review"
                    elif (op.Name = "Do" || op.Name = inlineImage || textShowing.Contains op.Name) && not (drawn.ContainsKey index) then
                        fail "a painted item's extent couldn't be read; unknown content can't be treated as empty"
                let inspect first last mcid marked supported =
                    let boxes =
                        [ for i in first .. last do
                              match drawn.TryGetValue i with
                              | true, found -> yield! found |> Seq.map _.Box
                              | _ -> () ]
                    let coverages = boxes |> List.map (coverage edit.Box)
                    if coverages |> List.exists Option.isNone then
                        fail "a painted item's extent couldn't be read; unknown content can't be treated as empty"
                    elif not boxes.IsEmpty && (coverages |> List.forall (Option.exists (fun amount -> amount > 0.5))) then
                        if not supported then fail "the region includes nested or unsupported marked content; its ownership can't be changed safely"
                        else
                            match mcid with
                            | Some id when not (owners.ContainsKey((edit.Page, id))) ->
                                fail "selected marked content has no unambiguous structure owner"
                            | _ ->
                                parts.Add { First = first; Last = last; Mcid = mcid; Marked = marked }
                                beyond.AddRange(boxes |> List.filter (not << contains edit.Box))
                let mutable i = 0
                while i < operations.Count do
                    let op = operations[i]
                    if op.Name = "sh" then
                        fail "this page contains shading whose painted extent is unknown; it needs manual review"
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
                        if depth <> 0 then fail "the page has unbalanced marked content"
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
                            inspect first (i - 1) mcid true (not nested && appearanceSafe && (artifact || mcid.IsSome))
                    elif op.Name = "EMC" then
                        fail "the page has unbalanced marked content"
                        i <- i + 1
                    else i <- i + 1
                // Reuse the existing safe boundaries for text objects, paths and images.
                if why.IsNone then
                    for piece in UntaggedContent.piecesOn page edit.Page do
                        for unit in piece.Units do
                            inspect unit.First unit.Last None false true
                let selected = List.ofSeq parts
                // A form may carry its own tags, shared across invocations. Don't relabel it
                // through a page MCID and silently orphan its stream parent-tree entries.
                for part in selected do
                    for i in part.First .. part.Last do
                        let op = operations[i]
                        if op.Name = "sh" then fail "selected shading has an unknown painted extent"
                        if op.Name = "Do" && op.Operands.Length > 0 then
                            match op.Operands[0] with
                            | :? PdfName as name ->
                                let xobjects = resources.GetResource PdfName.XObject
                                if not (isNull xobjects) then
                                    match xobjects.GetAsStream name with
                                    | NonNull stream when PdfName.Form.Equals(stream.GetAsName PdfName.Subtype) ->
                                        fail "the region includes a form XObject; shared or internally tagged forms need manual review"
                                    | _ -> ()
                            | _ -> fail "an image or form resource couldn't be read"
                let ids = selected |> List.choose _.Mcid
                if ids.Length <> (List.distinct ids).Length then fail "an MCID occurs more than once in the selected content"
                let occurrences =
                    operations |> Seq.choose (mcidOf resources) |> Seq.countBy id |> dict
                for id in ids do
                    if occurrences[id] <> 1 then
                        fail "selected content shares an MCID with another sequence on the page; its ownership needs manual review"
                // Require exactly one page-content reference in the tree, not just the last
                // dictionary in a lookup which might hide duplicate ownership.
                let ownerPairs = ids |> List.map (fun id -> id, owners[(edit.Page, id)])
                let containers = set ["Document"; "Part"; "Art"; "Sect"; "Div"; "NonStruct"]
                for owner, selectedIds in ownerPairs |> List.groupBy snd |> List.map (fun (owner, pairs) -> owner, pairs |> List.map fst |> Set.ofList) do
                    let tag = standardTag owner context |> Option.ofObj |> Option.map _.GetValue() |> Option.defaultValue ""
                    if not (containers.Contains tag) then
                        let remaining =
                            PdfStructElem(owner).GetKids()
                            |> Seq.exists (fun kid ->
                                match kid with
                                | :? PdfMcr as mcr ->
                                    not (selectedIds.Contains(mcr.GetMcid())
                                         && obj.ReferenceEquals(mcr.GetPageObject(), page.GetPdfObject()))
                                | _ -> true)
                        if remaining then
                            fail "the region selects only part of a semantic element; enlarge it to include all of that element's content or review the reading order manually"
                let visits = walk (document.GetStructTreeRoot().GetPdfObject()) context |> List.ofSeq
                for id, owner in ownerPairs do
                    let references =
                        visits |> Seq.collect (fun v -> PdfStructElem(v.Element).GetKids())
                        |> Seq.filter (fun kid ->
                            match kid with
                            | :? PdfMcr as mcr -> mcr.GetMcid() = id && obj.ReferenceEquals(mcr.GetPageObject(), page.GetPdfObject())
                            | _ -> false)
                        |> Seq.length
                    if references <> 1 then fail "selected content has ambiguous or unreadable ownership"
                    if not (isNull (owner.Get PdfName.ActualText) && isNull (owner.Get PdfName.Alt)) then
                        let hasRemaining =
                            PdfStructElem(owner).GetKids()
                            |> Seq.exists (fun kid ->
                                match kid with
                                | :? PdfMcr as mcr -> not (ids |> List.contains (mcr.GetMcid())) || not (obj.ReferenceEquals(mcr.GetPageObject(), page.GetPdfObject()))
                                | _ -> true)
                        if hasRemaining then fail "an owner has alt or replacement text covering content outside the selection; it needs manual review"
                    let mutable parent = PdfStructElem(owner).GetParent()
                    let mutable child = owner
                    let mutable depth = 0
                    while parent :? PdfStructElem && depth < 200 do
                        let element = parent :?> PdfStructElem
                        if not (element.GetKids() |> Seq.exists (fun k -> match k with :? PdfStructElem as e -> obj.ReferenceEquals(e.GetPdfObject(), child) | _ -> false)) then
                            fail "a selected element's parent pointer disagrees with its reading-order location"
                        if not (isNull (element.GetPdfObject().Get PdfName.ActualText)) then
                            fail "a wrapper around selected content has replacement text; it needs manual review"
                        child <- element.GetPdfObject()
                        parent <- element.GetParent()
                        depth <- depth + 1
                    if depth = 200 then fail "selected content has a cyclic or excessively deep parent chain"
                    elif not (parent :? PdfStructTreeRoot) then
                        fail "selected content has a missing or unreadable parent pointer"
                    elif not ((parent :?> PdfStructTreeRoot).GetKids() |> Seq.exists (fun k -> match k with :? PdfStructElem as e -> obj.ReferenceEquals(e.GetPdfObject(), child) | _ -> false)) then
                        fail "a selected element's parent pointer disagrees with its reading-order location"
                for annotation in page.GetAnnotations() do
                    match annotation.GetRectangle() with
                    | NonNull rect when intersects edit.Box (rect.ToRectangle()) ->
                        fail "the region includes an annotation or form field; its interactive meaning needs manual review"
                    | _ -> ()
                match why with
                | Some reason -> Error reason
                | None when selected.IsEmpty -> Error "no readable painted content was found in the region"
                | None ->
                    let text = UntaggedContent.textIn page edit.Box |> Option.defaultValue "(text couldn't be read)"
                    let coordinates (box: iText.Kernel.Geom.Rectangle) =
                        [ box.GetLeft(); box.GetBottom(); box.GetRight(); box.GetTop() ]
                        |> List.map (fun value -> float value |> fun n -> n.ToString("0.##", Globalization.CultureInfo.InvariantCulture))
                        |> String.concat ", "
                    let extended =
                        if beyond.Count = 0 then ""
                        else
                            let boxes = beyond |> Seq.map (fun box -> $"[{coordinates box}] pt") |> String.concat "; "
                            " Painted bounding box(es) extending beyond the rectangle, from whole items"
                            + " with each box more than 50% covered (left, bottom, right, top): "
                            + boxes + "."
                    Ok { Parts = selected; Bytes = bytes; Operations = operations; Owners = ownerPairs
                         Summary = $"{selected.Length} complete content piece(s), including {ids.Length} tagged piece(s).{extended} Text in the region: {text}" }
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
            let figure = PdfStructElem(document, PdfName.Figure, page)
            figure.SetAlt(PdfString(edit.Alt, iText.IO.Font.PdfEncodings.UNICODE_BIG)) |> ignore
            let savedBox = [|float edit.Page; edit.Box.Left; edit.Box.Bottom; edit.Box.Right; edit.Box.Top|] |> Array.map (fun value -> Math.Round(value, 1))
            figure.GetPdfObject().Put(marker, PdfArray(savedBox)) |> ignore
            // The first selected owner determines the reading-order slot. Its siblings and
            // unrelated paragraphs stay put, even when other owners are in distant branches.
            let visits = walk (root.GetPdfObject()) context |> List.ofSeq
            let first =
                visits |> List.tryFind (fun v -> plan.Owners |> List.exists (fun (_, o) -> obj.ReferenceEquals(o, v.Element)))
            let placeNear (dictionary: PdfDictionary) after =
                let owner = PdfStructElem dictionary
                let mutable branch = owner
                let mutable parentNode = owner.GetParent()
                let containers = set ["Document"; "Part"; "Art"; "Sect"; "Div"; "NonStruct"]
                let tag dictionary = standardTag dictionary context |> Option.ofObj |> Option.map _.GetValue() |> Option.defaultValue ""
                if containers.Contains(tag dictionary) then
                    let index =
                        if after then owner.GetKids().Count
                        else
                            owner.GetKids()
                            |> Seq.tryFindIndex (fun k -> match k with :? PdfMcr as mcr -> plan.Owners |> List.exists (fun (id, _) -> id = mcr.GetMcid()) | _ -> false)
                            |> Option.defaultValue 0
                    owner.AddKid(index, figure) |> ignore
                else
                    while parentNode :? PdfStructElem && not (containers.Contains(tag ((parentNode :?> PdfStructElem).GetPdfObject()))) do
                        branch <- parentNode :?> PdfStructElem
                        parentNode <- branch.GetParent()
                    match parentNode with
                    | :? PdfStructElem as parent ->
                        let index = parent.GetKids() |> Seq.findIndex (fun k -> match k with :? PdfStructElem as e -> obj.ReferenceEquals(e.GetPdfObject(), branch.GetPdfObject()) | _ -> false)
                        parent.AddKid(index + (if after then 1 else 0), figure) |> ignore
                    | _ -> root.AddKid(0, figure) |> ignore
            match first with
            | Some visit -> placeNear visit.Element false
            | None ->
                match UntaggedContent.elementBefore document context edit.Page plan.Operations (page.GetResources()) (plan.Parts |> List.minBy _.First).First with
                | Some(_, previous) -> placeNear previous true
                | None ->
                    match root.GetKids() |> Seq.tryHead with
                    | Some(:? PdfStructElem as documentElement) -> documentElement.AddKid(0, figure) |> ignore
                    | _ -> root.AddKid(0, figure) |> ignore
            let replacements = Dictionary<int, string>()
            let after = Dictionary<int, string>()
            for part in plan.Parts |> List.sortBy _.First do
                let id =
                    match part.Mcid with
                    | Some id ->
                        let owner = PdfStructElem(snd (plan.Owners |> List.find (fun (mcid, _) -> mcid = id)))
                        let index = owner.GetKids() |> Seq.findIndex (fun kid -> match kid with :? PdfMcr as mcr -> mcr.GetMcid() = id && obj.ReferenceEquals(mcr.GetPageObject(), page.GetPdfObject()) | _ -> false)
                        owner.RemoveKid(index, true) |> ignore
                        let dictionary = PdfDictionary()
                        dictionary.Put(PdfName.Type, PdfName.MCR) |> ignore
                        dictionary.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
                        dictionary.Put(PdfName.MCID, PdfNumber id) |> ignore
                        figure.AddKid(PdfMcrDictionary(dictionary, figure)) |> ignore
                        id
                    | None ->
                        let mcr = PdfMcrNumber(page, figure)
                        figure.AddKid mcr |> ignore
                        mcr.GetMcid()
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
            // Prune only owners and ancestors emptied by this operation. Unrelated siblings
            // and wrappers with any remaining content are left intact.
            for _, dictionary in plan.Owners |> List.distinctBy snd do
                let mutable owner = PdfStructElem dictionary
                let mutable pruning = true
                while pruning && owner.GetKids().Count = 0 do
                    match owner.GetParent() with
                    | :? PdfStructElem as parent ->
                        parent.RemoveKid owner |> ignore
                        owner <- parent
                    | _ -> pruning <- false
            true
