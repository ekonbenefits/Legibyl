/// The structure tree's parent tree (/StructTreeRoot /ParentTree): for each page, which element
/// each piece of its marked content (by MCID) belongs to, and for each tagged annotation, which
/// element holds it. Readers and veraPDF go from content to its tag through it, so without it
/// every tag reads as untagged (Pages writes PDFs with it null, and no /StructParents on the
/// pages). This finds when it's missing or incomplete, and rebuilds it from the structure tree.
module internal Legibyl.Validation.ParentTree

open System.Collections.Generic
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Tagging
open Legibyl.Validation.StructureTreeObjects

/// What the structure tree tags, by where it is.
type Tagged =
    { /// (page, MCID) -> the element whose content it is.
      MarkedContent: IReadOnlyDictionary<int * int, PdfDictionary>
      /// Annotations (and other objects) the tree holds by /OBJR, by object number.
      Objects: IReadOnlyDictionary<int, PdfDictionary> }

/// What's tagged, read from the structure tree itself rather than the parent tree.
let tagged (document: PdfDocument) context : Tagged =
    let markedContent = Dictionary<int * int, PdfDictionary>()
    let objects = Dictionary<int, PdfDictionary>()
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> ()
    | root ->
        for visit in walk root context do
            let element = visit.Element
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
    { MarkedContent = markedContent; Objects = objects }

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

/// What's missing: whether the parent tree is, and how many pages with tagged content and tagged
/// objects it has no entry for.
type Missing =
    { TreeMissing: bool
      Pages: int
      Objects: int }

/// What the parent tree lacks, if the structure tree tags any content it doesn't resolve.
let find (document: PdfDocument) context : Missing option =
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> None
    | root ->
        let tagged = tagged document context
        let pages = tagged.MarkedContent.Keys |> Seq.map fst |> Seq.distinct |> List.ofSeq
        let tree = match resolve (root.Get PdfName.ParentTree) with :? PdfDictionary as tree -> Some tree | _ -> None
        let entries = tree |> Option.map numbers
        let has (key: PdfNumber | null) =
            match key, entries with
            | NonNull key, Some entries -> entries.ContainsKey(key.IntValue())
            | _ -> false
        let pagesMissing =
            pages |> List.filter (fun page -> not (has (document.GetPage(page).GetPdfObject().GetAsNumber PdfName.StructParents)))
        let objectsMissing =
            tagged.Objects.Keys
            |> Seq.filter (fun number ->
                match document.GetPdfObject number with
                | :? PdfDictionary as obj -> not (has (obj.GetAsNumber PdfName.StructParent))
                | _ -> false)
            |> Seq.length
        if pagesMissing.IsEmpty && objectsMissing = 0 then
            None
        else
            Some
                { TreeMissing = tree.IsNone
                  Pages = pagesMissing.Length
                  Objects = objectsMissing }

/// Rebuilds the parent tree from the structure tree: registers each element's marked content
/// with iText again, which writes the parent tree, the pages' /StructParents and
/// /ParentTreeNextKey when the document is saved. iText reads the marked content it knows from
/// the parent tree, so without one it would write an empty one. Tagged annotations whose
/// /StructParent the tree has no entry for get a new one. Returns what it did, if anything.
let rebuild (document: PdfDocument) context : string list =
    match find document context with
    | None -> []
    | Some missing ->
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
                             | NonNull key -> not (entries.ContainsKey(key.IntValue()))
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
        [ if missing.TreeMissing then $"Rebuilt the missing parent tree, so the tags are found for {what}."
          else $"Completed the parent tree, so the tags are found for {what}." ]
