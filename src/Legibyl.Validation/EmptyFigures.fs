/// Figures tagged around nothing: an authoring app's empty marked-content sequence ("/MCID 16 BDC
/// EMC") given a Figure tag. It fails for having no alt text, but there's nothing on the page for
/// alt text to describe, so the repair is to take the tag out.
module internal Legibyl.Validation.EmptyFigures

open System
open System.Collections.Generic
open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

/// Operators that put something on a page: text, a painted path, an image (inline or not), a
/// form XObject, a shading.
let private drawing =
    Set.unionMany [ ContentOperations.textShowing; ContentOperations.pathPainting |> Set.remove "n"; set [ "Do"; "sh"; "BI" ] ]

/// A page's marked content by MCID: those there, and those that draw something (inside it, or
/// inside marked content inside it).
let private pageContent (page: PdfPage) =
    let resources = page.GetResources()
    let operations, _ = ContentOperations.operationsOf (page.GetContentBytes()) resources
    let found = HashSet<int>()
    let drawn = HashSet<int>()
    let mutable inside: int option list = []
    for operation in operations do
        match operation.Name with
        | "BDC" ->
            let mcid = ContentOperations.mcidOf resources operation
            mcid |> Option.iter (found.Add >> ignore)
            inside <- mcid :: inside
        | "BMC" -> inside <- None :: inside
        | "EMC" -> inside <- (match inside with _ :: rest -> rest | [] -> [])
        | name when drawing.Contains name ->
            for mcid in inside do
                mcid |> Option.iter (drawn.Add >> ignore)
        | _ -> ()
    found, drawn

let private hasText (element: PdfDictionary) (key: PdfName) =
    match element.GetAsString key with
    | null -> false
    | text -> not (String.IsNullOrWhiteSpace(text.ToUnicodeString()))

/// The figures that draw nothing: a Figure without alt text or replacement text, holding only
/// marked content on its page that's there and draws nothing (no elements, annotations, or content
/// in a form XObject).
let visits (document: PdfDocument) context : StructElemVisit list =
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> []
    | root ->
        let pages = Dictionary<int, HashSet<int> * HashSet<int>>()
        let contentOf page =
            match pages.TryGetValue page with
            | true, content -> content
            | _ ->
                let content = pageContent (document.GetPage page)
                pages[page] <- content
                content
        let drawsNothing (page: int option) (mcid: int) =
            match page with
            | Some page when page >= 1 && page <= document.GetNumberOfPages() ->
                let found, drawn = contentOf page
                found.Contains mcid && not (drawn.Contains mcid)
            | _ -> false
        let kidEmpty (page: int option) (kid: PdfObject | null) =
            match resolve kid with
            | :? PdfNumber as mcid -> drawsNothing page (mcid.IntValue())
            | :? PdfDictionary as reference when PdfName.MCR.Equals(reference.GetAsName PdfName.Type) && isNull (reference.Get PdfName.Stm) ->
                match reference.GetAsNumber PdfName.MCID with
                | null -> false
                | mcid -> drawsNothing (structElemPage reference context page) (mcid.IntValue())
            | _ -> false
        [ for visit in walk root context do
              let element = visit.Element
              if PdfName.Figure.Equals(standardTag element context)
                 && not (hasText element PdfName.Alt)
                 && not (hasText element PdfName.ActualText) then
                  let empty =
                      match resolve (element.Get PdfName.K) with
                      | null -> true
                      | :? PdfArray as kids -> Seq.init (kids.Size()) kids.Get |> Seq.forall (kidEmpty visit.Page)
                      | kid -> kidEmpty visit.Page kid
                  if empty then
                      yield visit ]

/// The figures that draw nothing (see visits).
let find (document: PdfDocument) context : PdfDictionary list = visits document context |> List.map _.Element

/// Their object numbers, to mark the findings about them fixable.
let objectNumbers (document: PdfDocument) context : Set<int> =
    set
        [ for element in find document context do
              match element.GetIndirectReference() with
              | null -> ()
              | reference -> yield reference.GetObjNumber() ]

/// Takes each figure that draws nothing out of its parent's /K.
let remove (document: PdfDocument) context : string list =
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot, find document context with
    | null, _
    | _, [] -> []
    | root, empty ->
        let isEmpty (kid: PdfObject | null) =
            match resolve kid with
            | :? PdfDictionary as element -> empty |> List.exists (fun e -> obj.ReferenceEquals(e, element))
            | _ -> false
        let mutable removed = 0
        // Parents first, each once (walk visits every element below the root).
        for parent in root :: [ for visit in walk root context -> visit.Element ] |> List.distinct do
            match resolve (parent.Get PdfName.K) with
            | :? PdfArray as kids ->
                for index in kids.Size() - 1 .. -1 .. 0 do
                    if isEmpty (kids.Get index) then
                        kids.Remove index
                        removed <- removed + 1
            | kid when isEmpty kid ->
                parent.Remove PdfName.K |> ignore
                removed <- removed + 1
            | _ -> ()
        match removed with
        | 0 -> []
        | count -> [ $"Removed {count} figure tag(s) that draw nothing." ]
