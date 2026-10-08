/// PDF/UA-2 (8.8) wants every link, bookmark and go-to action within a document to point to
/// structure: a structure destination (/SD, new in PDF 2.0) naming the element it goes to, not
/// only a page. For a tagged PDF 2.0 file, this finds the element each one lands on (the first
/// block element at or below where it lands on its page, or the page's first) and adds /SD,
/// keeping /D, the page, for readers without it. PDF 1.x files are left alone: /SD is a PDF 2.0
/// key their readers would ignore, and PDF/UA-1 doesn't ask for it.
module internal Legibyl.Validation.LinkTargets

open System
open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

/// What a destination can land on: blocks a reader reads from, not groupings (Document, Sect,
/// Div) or text inside a block (Span, Link).
let private targetTags =
    set
        [ Tags.H; Tags.H1; Tags.H2; Tags.H3; Tags.H4; Tags.H5; Tags.H6; Tags.Title; Tags.P; Tags.L; Tags.Table; Tags.Figure; Tags.Formula
          Tags.Caption; Tags.BlockQuote; Tags.Code; Tags.Note; Tags.FENote; Tags.TOC; Tags.Index; Tags.Aside ]

/// An element a destination can land on, and where its content starts.
type private Candidate =
    { Element: PdfDictionary
      Page: int
      Top: float }

/// What holds a destination.
type Holder =
    /// A link annotation (/Dest, or a go-to action's /D).
    | Link of annotation: PdfDictionary
    /// A bookmark (outline item), the same way.
    | Bookmark of item: PdfDictionary
    /// The catalog's /OpenAction: where the document opens.
    | OpenAction

/// A destination to point at an element, and the element.
type Target =
    { Holder: Holder
      /// The destination as it is: an explicit one ([page /XYZ left top zoom]) or a name.
      Destination: PdfObject
      /// Its explicit form, looked up if it's a name.
      Explicit: PdfArray
      Element: PdfDictionary }

/// Whether this document is one to point: PDF 2.0 or later, and tagged.
let applies (document: PdfDocument) =
    document.GetPdfVersion().CompareTo PdfVersion.PDF_2_0 >= 0
    && not (isNull (document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot))

/// A named destination's explicit form, from the catalog's /Names /Dests tree or its older
/// /Dests dictionary.
let private named (document: PdfDocument) (name: string) : PdfObject option =
    let fromTree =
        document.GetCatalog().GetNameTree(PdfName.Dests).GetNames()
        |> Seq.tryFind (fun entry -> entry.Key.ToUnicodeString() = name)
        |> Option.map _.Value
    let fromDictionary () =
        match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.Dests with
        | null -> None
        | dests -> dests.Get(PdfName name) |> Option.ofObj
    fromTree |> Option.orElseWith fromDictionary

/// A destination's explicit form: [page /Kind ...], directly, in a dictionary's /D (as named
/// destinations may be), or by name.
let rec private explicitOf (document: PdfDocument) (depth: int) (destination: PdfObject | null) : PdfArray option =
    if depth > 4 then None
    else
        match resolve destination with
        | :? PdfArray as array when array.Size() >= 2 -> Some array
        | :? PdfDictionary as dictionary -> explicitOf document (depth + 1) (dictionary.Get PdfName.D)
        | :? PdfString as name -> named document (name.ToUnicodeString()) |> Option.bind (explicitOf document (depth + 1))
        | :? PdfName as name -> named document (name.GetValue()) |> Option.bind (explicitOf document (depth + 1))
        | _ -> None

/// The page an explicit destination goes to (1-based), and how far down, if it says.
let private landing (document: PdfDocument) (explicit: PdfArray) : (int * float option) option =
    let page =
        match resolve (explicit.Get 0) with
        | :? PdfDictionary as page ->
            match document.GetPageNumber page with
            | n when n > 0 -> Some n
            | _ -> None
        // A page index, as some writers use.
        | :? PdfNumber as index -> Some(index.IntValue() + 1)
        | _ -> None
    let numberAt i =
        match resolve (if i < explicit.Size() then explicit.Get i else null) with
        | :? PdfNumber as number -> Some(number.GetValue())
        | _ -> None
    let top =
        match explicit.GetAsName 1 |> Option.ofObj |> Option.map _.GetValue() with
        | Some "XYZ" -> numberAt 3
        | Some "FitH"
        | Some "FitBH" -> numberAt 2
        | Some "FitR" -> numberAt 5
        | _ -> None
    page |> Option.map (fun page -> page, top)

/// The element on the page the destination lands on: the first at or just below its top (a
/// little leeway, as a destination sits a little above what it's for), or, with no top or
/// nothing below it, the page's first or last.
let private landsOn (candidates: Candidate list) (page: int, top: float option) =
    let onPage = candidates |> List.filter (fun c -> c.Page = page)
    match top with
    | Some top ->
        match onPage |> List.filter (fun c -> c.Top <= top + 2.0) with
        | [] -> List.tryLast onPage
        // Highest first; of equals, the first in reading order (a list before its items).
        | below -> below |> List.sortByDescending _.Top |> List.tryHead
    | None -> List.tryHead onPage
    |> Option.map _.Element

let private goTo (holder: PdfDictionary) =
    match holder.GetAsDictionary PdfName.A with
    | NonNull action when PdfName.GoTo.Equals(action.GetAsName PdfName.S) -> Some action
    | _ -> None

/// The destination a link or bookmark holds, if it goes within the document and doesn't
/// point to structure yet.
let private destinationOf (holder: PdfDictionary) : PdfObject option =
    match goTo holder with
    | Some action when isNull (action.Get PdfName.SD) -> action.Get PdfName.D |> Option.ofObj
    | Some _ -> None
    | None when isNull (holder.Get PdfName.A) -> holder.Get PdfName.Dest |> Option.ofObj
    | None -> None

let private bookmarks (document: PdfDocument) : PdfDictionary list =
    // Outlines can loop; each item once.
    let seen = Collections.Generic.HashSet<obj>(Collections.Generic.ReferenceEqualityComparer.Instance)
    let rec items (item: PdfDictionary | null) =
        [ match item with
          | NonNull item when seen.Add item ->
              yield item
              yield! items (item.GetAsDictionary PdfName.First)
              yield! items (item.GetAsDictionary PdfName.Next)
          | _ -> () ]
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.Outlines with
    | null -> []
    | outlines -> items (outlines.GetAsDictionary PdfName.First)

let private links (document: PdfDocument) : PdfDictionary list =
    [ for pageNumber in 1 .. document.GetNumberOfPages() do
          match document.GetPage(pageNumber).GetPdfObject().GetAsArray PdfName.Annots with
          | null -> ()
          | annotations ->
              for i in 0 .. annotations.Size() - 1 do
                  match annotations.GetAsDictionary i with
                  | NonNull annotation when PdfName.Link.Equals(annotation.GetAsName PdfName.Subtype) -> yield annotation
                  | _ -> () ]

/// Every link, bookmark and open action within the document that doesn't point to structure,
/// with the element it lands on; those it can't place are left out. Empty for a document this
/// doesn't apply to.
let find (document: PdfDocument) context : Target list =
    if not (applies document) then
        []
    else
        let root = document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot
        let candidates =
            lazy
                (walk root context
                 |> Seq.choose (fun visit ->
                     match standardTag visit.Element context with
                     | NonNull tag when targetTags.Contains(tag.GetValue()) ->
                         textGeometry visit.Element context visit.Page
                         |> Option.map (fun geometry -> { Element = visit.Element; Page = geometry.PageNumber; Top = geometry.Top })
                     | _ -> None)
                 |> List.ofSeq)
        let openAction =
            match document.GetCatalog().GetPdfObject().Get PdfName.OpenAction |> resolve with
            | :? PdfArray as array -> Some(array :> PdfObject)
            | :? PdfDictionary as action when PdfName.GoTo.Equals(action.GetAsName PdfName.S) && isNull (action.Get PdfName.SD) ->
                action.Get PdfName.D |> Option.ofObj
            | _ -> None
        let holders =
            [ for link in links document do
                  match destinationOf link with
                  | Some destination -> yield Link link, destination
                  | None -> ()
              for item in bookmarks document do
                  match destinationOf item with
                  | Some destination -> yield Bookmark item, destination
                  | None -> ()
              match openAction with
              | Some destination -> yield OpenAction, destination
              | None -> () ]
        [ for holder, destination in holders do
              match explicitOf document 0 destination with
              | Some explicit ->
                  match landing document explicit |> Option.bind (landsOn candidates.Value) with
                  | Some element ->
                      yield
                          { Holder = holder
                            Destination = destination
                            Explicit = explicit
                            Element = element }
                  | None -> ()
              | None -> () ]

/// The link annotations among targets, by object number, so validation can mark their
/// findings fixable.
let linkNumbers (targets: Target list) : Set<int> =
    set
        [ for target in targets do
              match target.Holder with
              | Link annotation ->
                  match annotation.GetIndirectReference() with
                  | null -> ()
                  | reference -> yield reference.GetObjNumber()
              | _ -> () ]

/// The structure destination: the explicit one with the element in place of the page.
let private structureDestination (document: PdfDocument) (target: Target) =
    if isNull (target.Element.GetIndirectReference()) then
        target.Element.MakeIndirect document |> ignore
    let array = PdfArray()
    array.Add target.Element
    for i in 1 .. target.Explicit.Size() - 1 do
        array.Add(target.Explicit.Get i)
    array

/// A go-to action with the destination as it was (/D) and the structure destination (/SD).
let private goToAction (document: PdfDocument) (target: Target) =
    let action = PdfDictionary()
    action.Put(PdfName.S, PdfName.GoTo) |> ignore
    action.Put(PdfName.D, target.Destination) |> ignore
    action.Put(PdfName.SD, structureDestination document target) |> ignore
    action

/// Points each target at its element; returns the lines saying what changed.
let apply (document: PdfDocument) (targets: Target list) : string list =
    let point (holder: PdfDictionary) (target: Target) =
        match goTo holder with
        | Some action -> action.Put(PdfName.SD, structureDestination document target) |> ignore
        | None ->
            holder.Remove PdfName.Dest |> ignore
            holder.Put(PdfName.A, goToAction document target) |> ignore
    let mutable links = 0
    let mutable bookmarks = 0
    let mutable opens = false
    for target in targets do
        match target.Holder with
        | Link annotation ->
            point annotation target
            links <- links + 1
        | Bookmark item ->
            point item target
            bookmarks <- bookmarks + 1
        | OpenAction ->
            let catalog = document.GetCatalog().GetPdfObject()
            match catalog.Get PdfName.OpenAction |> resolve with
            | :? PdfDictionary as action -> action.Put(PdfName.SD, structureDestination document target) |> ignore
            | _ -> catalog.Put(PdfName.OpenAction, goToAction document target) |> ignore
            opens <- true
    [ if links > 0 then $"Pointed {links} link(s) within the document at the tags they go to."
      if bookmarks > 0 then $"Pointed {bookmarks} bookmark(s) at the tags they go to."
      if opens then "Pointed where the document opens at the tag there." ]
