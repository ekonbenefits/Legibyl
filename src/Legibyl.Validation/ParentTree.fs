/// The structure tree's parent tree (/StructTreeRoot /ParentTree): for each page, which element
/// each piece of its marked content (by MCID) belongs to, and for each tagged annotation, which
/// element holds it. Readers and veraPDF go from content to its tag through it, so without it
/// every tag reads as untagged (Pages writes PDFs with it null, and no /StructParents on the
/// pages). This finds when it's missing or incomplete, and rebuilds it from the structure tree.
module internal Legibyl.Validation.ParentTree

open System.Collections.Generic
open System
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Tagging
open Legibyl.Validation.StructureTreeObjects

/// What the structure tree tags, by where it is.
type Tagged =
    { /// (page, MCID) -> the element whose content it is.
      MarkedContent: IReadOnlyDictionary<int * int, PdfDictionary>
      /// Annotations (and other objects) the tree holds by /OBJR, by object number.
      Objects: IReadOnlyDictionary<int, PdfDictionary>
      /// Every tag in the tags (reached from the root).
      Reachable: IReadOnlySet<PdfDictionary> }

/// What's tagged, read from the structure tree itself rather than the parent tree.
let tagged (document: PdfDocument) context : Tagged =
    let markedContent = Dictionary<int * int, PdfDictionary>()
    let objects = Dictionary<int, PdfDictionary>()
    let reachable = HashSet<PdfDictionary>(HashIdentity.Reference)
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> ()
    | root ->
        for visit in walk root context do
            let element = visit.Element
            reachable.Add element |> ignore
            if not (isNull (element.GetAsName PdfName.S)) then
                // Its content: by MCID, by marked-content reference (/MCR, maybe on a page of its
                // own), or by object reference (/OBJR, an annotation).
                let kid (kid: PdfObject | null) =
                    match resolve kid with
                    | :? PdfNumber as number ->
                        visit.Page |> Option.iter (fun page -> markedContent[(page, number.IntValue())] <- element)
                    | :? PdfDictionary as reference when isNull (reference.GetAsName PdfName.S) ->
                        match reference.GetAsNumber PdfName.MCID, reference.GetAsDictionary PdfName.Obj with
                        | NonNull number, _ when isNull (reference.Get PdfName.Stm) ->
                            structElemPage reference context visit.Page
                            |> Option.iter (fun page -> markedContent[(page, number.IntValue())] <- element)
                        | _, NonNull obj ->
                            match obj.GetIndirectReference() with
                            | NonNull ref' -> objects[ref'.GetObjNumber()] <- element
                            | Null -> ()
                        | _ -> ()
                    | _ -> ()
                match resolve (element.Get PdfName.K) with
                | :? PdfArray as kids ->
                    for i in 0 .. kids.Size() - 1 do
                        kid (kids.Get i)
                | only -> kid only
    { MarkedContent = markedContent; Objects = objects; Reachable = reachable }

/// A number tree's entries, following /Kids.
let private numbers (tree: PdfDictionary) : IReadOnlyDictionary<int, PdfObject> =
    let entries = Dictionary<int, PdfObject>()
    let rec read (node: PdfDictionary) depth =
        match node.GetAsArray PdfName.Nums with
        | null -> ()
        | nums ->
            for i in 0 .. 2 .. nums.Size() - 2 do
                match nums.GetAsNumber i with
                | null -> ()
                | key -> entries[key.IntValue()] <- nums.Get(i + 1)
        match node.GetAsArray PdfName.Kids with
        | NonNull kids when depth < 32 ->
            for i in 0 .. kids.Size() - 1 do
                match kids.GetAsDictionary i with
                | null -> ()
                | kid -> read kid (depth + 1)
        | _ -> ()
    read tree 0
    entries

/// The page content the parent tree ties to another tag than the one holding it (forward, what
/// tagged read): (page, MCID, the tag it's tied to, the tag holding it, if any). None for a PDF
/// with no tags.
let rec inconsistentWith (forward: Tagged) (document: PdfDocument) =
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> []
    | _ -> inconsistentIn forward document

and private inconsistentIn (forward: Tagged) (document: PdfDocument) =
    let root = document.GetStructTreeRoot().GetPdfObject()
    // Content tied to a tag in the tags, though not where its content was found (no page
    // anywhere, say), isn't tied to a tag that's gone, and is left alone.
    let reachable = forward.Reachable
    let entries =
        match root.GetAsDictionary PdfName.ParentTree with
        | null -> Dictionary<int, PdfObject>() :> IReadOnlyDictionary<_, _>
        | tree -> numbers tree
    [ for page in 1 .. document.GetNumberOfPages() do
          match document.GetPage(page).GetPdfObject().GetAsNumber PdfName.StructParents with
          | NonNull key ->
              match entries.TryGetValue(key.IntValue()) with
              | true, (:? PdfArray as slots) ->
                  for mcid in 0 .. slots.Size() - 1 do
                      match slots.GetAsDictionary mcid with
                      | NonNull owner ->
                          match forward.MarkedContent.TryGetValue((page, mcid)) with
                          | true, expected when obj.ReferenceEquals(owner, expected) -> ()
                          | true, expected -> yield page, mcid, owner, Some expected
                          | _ when reachable.Contains owner -> ()
                          | _ -> yield page, mcid, owner, None
                      | _ -> ()
              | _ -> ()
          | _ -> () ]

let inconsistent (document: PdfDocument) context = inconsistentWith (tagged document context) document

/// Empty/whitespace text is not enough: a sequence may also paint an image or a path.
/// Only sequences actually present on a readable page, with no painting, can be discarded.
let private emptyContent (document: PdfDocument) (context: Context) (page: int) =
    let pdfPage = document.GetPage page
    let resources = pdfPage.GetResources()
    let operations, _ = ContentOperations.operationsOf (pdfPage.GetContentBytes()) resources
    let found = HashSet<int>()
    let painted = HashSet<int>()
    // Content that shows text: empty only if its text is read as whitespace (a glyph that can't
    // be read, with no ToUnicode say, reads as nothing, and may well draw something).
    let shown = HashSet<int>()
    let mutable inside: int option list = []
    let drawing = Set.union (ContentOperations.pathPainting |> Set.remove "n") (set ["Do"; "sh"; ContentOperations.inlineImage])
    let showing = set [ "Tj"; "TJ"; "'"; "\"" ]
    for operation in operations do
        match operation.Name with
        | "BDC" ->
            let mcid = ContentOperations.mcidOf resources operation
            mcid |> Option.iter (found.Add >> ignore)
            inside <- mcid :: inside
            let properties =
                if operation.Operands.Length < 2 then null
                else
                    match operation.Operands[1] with
                    | :? PdfDictionary as properties -> properties
                    | :? PdfName as name ->
                        match resources.GetResource PdfName.Properties with
                        | null -> null
                        | all -> all.GetAsDictionary name
                    | _ -> null
            if not (isNull properties)
               && [PdfName.ActualText; PdfName.Alt] |> List.exists (fun key ->
                   match (properties |> nonNull).GetAsString key with
                   | null -> false
                   | text -> not (String.IsNullOrWhiteSpace(text.ToUnicodeString()))) then
                for mcid in inside do mcid |> Option.iter (painted.Add >> ignore)
        | "BMC" -> inside <- None :: inside
        | "EMC" -> inside <- (match inside with _ :: rest -> rest | [] -> [])
        | name when drawing.Contains name ->
            for mcid in inside do mcid |> Option.iter (painted.Add >> ignore)
        | name when showing.Contains name ->
            for mcid in inside do mcid |> Option.iter (shown.Add >> ignore)
        | _ -> ()
    fun mcid ->
        found.Contains mcid && not (painted.Contains mcid)
        && not (context.MarkedContent.Unreadable.Contains page)
        && (match context.MarkedContent.TryGetValue((page, mcid)) with
            | true, content when shown.Contains mcid -> content.Text.Length > 0 && String.IsNullOrWhiteSpace content.Text
            | true, content -> String.IsNullOrWhiteSpace content.Text
            | _ -> not (shown.Contains mcid))

let private hasDescription (owner: PdfDictionary) =
    [PdfName.ActualText; PdfName.Alt]
    |> List.exists (fun key ->
        match owner.GetAsString key with
        | null -> false
        | text -> not (String.IsNullOrWhiteSpace(text.ToUnicodeString())))

/// Content still tied, by the parent tree, to a tag that isn't in the tags any more: by page and
/// that tag, its MCIDs, and whether Rebuild the parent tree unties it (marking it as not read):
/// the tag said it was an artifact, or it has no description and none of its content draws
/// anything. Decided for the tag as a whole, as the warning (Structure-17) and Structure… show it.
type Disconnected =
    { Page: int
      Owner: PdfDictionary
      Mcids: int list
      /// The tag that's gone was an artifact.
      Artifact: bool
      Untied: bool }

/// Content tied to a tag that's gone, from inconsistent's answer (all).
let disconnectedFrom (all: (int * int * PdfDictionary * PdfDictionary option) list) (document: PdfDocument) context : Disconnected list =
    let empty = Dictionary<int, int -> bool>()
    let emptyOn page =
        match empty.TryGetValue page with
        | true, test -> test
        | _ ->
            let test = emptyContent document context page
            empty[page] <- test
            test
    let gone = all |> List.filter (fun (_, _, _, expected) -> expected.IsNone)
    // Each tag that's gone decided over all its content, on every page, then shown a page at a time.
    let owners = List<PdfDictionary>()
    let byOwner = Dictionary<PdfDictionary, ResizeArray<int * int>>(HashIdentity.Reference)
    for page, mcid, owner, _ in gone do
        match byOwner.TryGetValue owner with
        | true, content -> content.Add((page, mcid))
        | _ ->
            owners.Add owner
            byOwner[owner] <- ResizeArray [ page, mcid ]
    [ for owner in owners do
          let content = List.ofSeq byOwner[owner]
          let artifact = PdfName.Artifact.Equals(owner.GetAsName PdfName.S)
          let untied =
              artifact || (not (hasDescription owner) && content |> List.forall (fun (page, mcid) -> emptyOn page mcid))
          for page, onPage in content |> List.groupBy fst do
              { Page = page
                Owner = owner
                Mcids = onPage |> List.map snd
                Artifact = artifact
                Untied = untied } ]

let disconnected (document: PdfDocument) context = disconnectedFrom (inconsistent document context) document context

/// Takes a page's MCID out of iText's parent tree (which it writes on close), whatever tag it's
/// tied to: by tying it to a tag of the moment, then untying it. That tag is never in the tags,
/// and dispose frees it, so it isn't written either.
type private Forgetter(document: PdfDocument) =
    let transient =
        lazy
            (let transient = PdfStructElem(document, PdfName.Span)
             transient.GetPdfObject().Put(PdfName.P, document.GetStructTreeRoot().GetPdfObject()) |> ignore
             transient)
    member _.Forget (page: int) (mcid: int) =
        let transient = transient.Value
        transient.GetPdfObject().Put(PdfName.Pg, document.GetPage(page).GetPdfObject()) |> ignore
        transient.AddKid(PdfMcrNumber(PdfNumber mcid, transient)) |> ignore
        transient.RemoveKid 0 |> ignore
    interface IDisposable with
        member _.Dispose() =
            if transient.IsValueCreated then
                match transient.Value.GetPdfObject().GetIndirectReference() with
                | null -> ()
                | reference -> reference.SetFree()

/// Whether a tag can be taken out of the tags, its content then marked as not read: it and the
/// tags below it have no description, and their content is on the page and draws nothing (no
/// text that reads as anything but whitespace, no path, image or shading). So pruning a tag
/// with no text can't hide a drawing it holds. Each page is read once, for every tag asked about.
let discardable (document: PdfDocument) context : PdfDictionary -> int option -> bool =
    let pages = Dictionary<int, int -> bool>()
    let empty page mcid =
        match pages.TryGetValue page with
        | true, test -> test mcid
        | _ ->
            let test = emptyContent document context page
            pages[page] <- test
            test mcid
    let rec visit (kid: PdfObject | null) page depth =
        if depth > 100 then false
        else
            match resolve kid with
            | null -> true
            | :? PdfNumber as number -> page |> Option.exists (fun page -> empty page (number.IntValue()))
            | :? PdfArray as array -> Seq.init (array.Size()) array.Get |> Seq.forall (fun kid -> visit kid page (depth + 1))
            | :? PdfDictionary as dictionary when not (isNull (dictionary.GetAsName PdfName.S)) ->
                not (hasDescription dictionary)
                && visit (dictionary.Get PdfName.K) (structElemPage dictionary context page) (depth + 1)
            | _ -> false
    fun element inheritedPage -> visit element inheritedPage 0

/// What's missing: whether the parent tree is, and how many pages with tagged content and tagged
/// objects it has no entry for.
type Missing =
    { TreeMissing: bool
      Pages: int
      Objects: int
      Associations: int }

/// What the parent tree lacks, from what tagged and inconsistentWith read (forward, all).
let findWith (forward: Tagged) (all: (int * int * PdfDictionary * PdfDictionary option) list) (document: PdfDocument) : Missing option =
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> None
    | root ->
        let tagged = forward
        let pages = tagged.MarkedContent.Keys |> Seq.map fst |> Seq.distinct |> List.ofSeq
        let tree = match resolve (root.Get PdfName.ParentTree) with :? PdfDictionary as tree -> Some tree | _ -> None
        let entries = tree |> Option.map numbers
        let has (key: PdfNumber | null) =
            match key, entries with
            | NonNull key, Some entries -> entries.ContainsKey(key.IntValue())
            | _ -> false
        // Each page's tagged MCIDs and the tags holding them, grouped once.
        let onPage =
            tagged.MarkedContent
            |> Seq.groupBy (fun (KeyValue((page, _), _)) -> page)
            |> Seq.map (fun (page, content) -> page, List.ofSeq content)
            |> dict
        let pagesMissing =
            pages |> List.filter (fun page ->
                let key = document.GetPage(page).GetPdfObject().GetAsNumber PdfName.StructParents
                if not (has key) then true
                else
                    let slots =
                        match entries.Value[key |> nonNull |> _.IntValue()] |> resolve with
                        | :? PdfArray as slots -> Some slots
                        | _ -> None
                    (match onPage.TryGetValue page with | true, content -> content | _ -> [])
                    |> List.exists (fun (KeyValue((_, mcid), owner)) ->
                        match slots with
                        | Some slots when mcid >= 0 && mcid < slots.Size() -> not (obj.ReferenceEquals(slots.GetAsDictionary mcid, owner))
                        | _ -> true))
        let objectsMissing =
            tagged.Objects.Keys
            |> Seq.filter (fun number ->
                match document.GetPdfObject number with
                | :? PdfDictionary as obj ->
                    let key = obj.GetAsNumber PdfName.StructParent
                    not (has key)
                    || not (Object.ReferenceEquals(resolve entries.Value[(key |> nonNull).IntValue()], tagged.Objects[number]))
                | _ -> false)
            |> Seq.length
        let associations = all
        if pagesMissing.IsEmpty && objectsMissing = 0 && associations.IsEmpty then
            None
        else
            Some
                { TreeMissing = tree.IsNone
                  Pages = pagesMissing.Length
                  Objects = objectsMissing
                  Associations = associations.Length }

/// Rebuilds the parent tree from the structure tree: registers each element's marked content
/// with iText again, which writes the parent tree, the pages' /StructParents and
/// /ParentTreeNextKey when the document is saved. iText reads the marked content it knows from
/// the parent tree, so without one it would write an empty one. Tagged annotations whose
/// /StructParent the tree has no entry for get a new one. Returns what it did, if anything.
/// What the parent tree lacks, if the structure tree tags any content it doesn't resolve.
let find (document: PdfDocument) context : Missing option =
    let forward = tagged document context
    findWith forward (inconsistentWith forward document) document

let rebuild (document: PdfDocument) context : string list =
    let forward = tagged document context
    let all = inconsistentWith forward document
    match findWith forward all document with
    | None -> []
    | Some missing ->
        let mutable discarded = 0
        let byPage = Dictionary<int, HashSet<int>>()
        let kept = SortedDictionary<int, int>()
        use forgetter = new Forgetter(document)
        // Content tied to the wrong tag: untied here, and tied to the right one below.
        for page, mcid, _, expected in all do
            if expected.IsSome then forgetter.Forget page mcid
        // Content tied to a tag that's gone: untied, and marked as not read, a tag at a time.
        for gone in disconnectedFrom all document context do
            if gone.Untied then
                for mcid in gone.Mcids do
                    forgetter.Forget gone.Page mcid
                    match byPage.TryGetValue gone.Page with
                    | true, mcids -> mcids.Add mcid |> ignore
                    | _ -> byPage[gone.Page] <- HashSet [ mcid ]
                    discarded <- discarded + 1
            else
                kept[gone.Page] <- (match kept.TryGetValue gone.Page with | true, n -> n + gone.Mcids.Length | _ -> gone.Mcids.Length)
        for KeyValue(page, count) in kept do
            let why =
                if context.MarkedContent.Unreadable.Contains page then "the page's text couldn't be read, so whether they draw anything isn't known"
                else "they draw something or have a description"
            context.Notes.Add
                $"Page {page}: {count} piece(s) of content are tied to a tag that isn't in the tags any more, and were kept as they are: {why} (Structure… shows them, under Tied to tags that are gone). Their tag needs putting back where the PDF was made, or in a PDF editor's tags panel."
        ContentArtifacts.mark document byPage
        let entries =
            match resolve ((document.GetStructTreeRoot().GetPdfObject()).Get PdfName.ParentTree) with
            | :? PdfDictionary as tree -> numbers tree
            | _ -> Dictionary<int, PdfObject>() :> IReadOnlyDictionary<_, _>
        let rec visit (element: PdfStructElem) depth =
            if depth < 200 then
                let kids = element.GetKids() |> Seq.toArray
                for i in 0 .. kids.Length - 1 do
                    match kids[i] with
                    | :? PdfObjRef as reference ->
                        match (reference.GetPdfObject() :?> PdfDictionary).GetAsDictionary PdfName.Obj with
                        | NonNull obj when
                            (match obj.GetAsNumber PdfName.StructParent with
                             | NonNull key ->
                                 match entries.TryGetValue(key.IntValue()) with
                                 | true, owner -> not (Object.ReferenceEquals(resolve owner, element.GetPdfObject()))
                                 | _ -> true
                             | Null -> true)
                            ->
                            element.RemoveKid i |> ignore
                            obj.Remove PdfName.StructParent |> ignore
                            let annotation = iText.Kernel.Pdf.Annot.PdfAnnotation.MakeAnnotation obj
                            element.AddKid(i, PdfObjRef(annotation, element, document.GetNextStructParentIndex())) |> ignore
                        | _ -> ()
                    | :? PdfMcr as mcr ->
                        element.RemoveKid(i, true) |> ignore
                        element.AddKid(i, mcr) |> ignore
                    | :? PdfStructElem as child ->
                        // iText needs each element's parent (/P) to move its content; some files leave it out.
                        if isNull (child.GetPdfObject().Get PdfName.P) then child.GetPdfObject().Put(PdfName.P, element.GetPdfObject()) |> ignore
                        visit child (depth + 1)
                    | _ -> ()
        let root = document.GetStructTreeRoot()
        for kid in root.GetKids() do
            match kid with
            | :? PdfStructElem as element ->
                if isNull (element.GetPdfObject().Get PdfName.P) then element.GetPdfObject().Put(PdfName.P, root.GetPdfObject()) |> ignore
                visit element 0
            | _ -> ()
        let what =
            [ if missing.Pages > 0 then (if missing.Pages = 1 then "1 page's content" else $"{missing.Pages} pages' content")
              if missing.Objects > 0 then (if missing.Objects = 1 then "1 tagged annotation" else $"{missing.Objects} tagged annotations") ]
            |> String.concat " and "
        [ if missing.Pages > 0 || missing.Objects > 0 then
              if missing.TreeMissing then $"Rebuilt the missing parent tree, so the tags are found for {what}."
              else $"Completed the parent tree, so the tags are found for {what}."
          if discarded > 0 then
              $"Untied {discarded} piece(s) of content from tags that aren't in the tags any more, marking them as not read (they draw nothing, or their tag said they were artifacts); the page looks the same." ]
