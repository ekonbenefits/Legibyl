/// Reading a tagged PDF's raw structure tree objects via iText: resolving pages, text,
/// and page geometry for structure elements. Shared by LegibylChecks (warnings) and
/// StructureFixes (fixes).
module internal Legibyl.Validation.StructureTreeObjects

open System
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open iText.Kernel.Pdf
open Legibyl.Validation.Interop

type Context =
    { PageNumberByObject: IReadOnlyDictionary<int, int>
      MarkedContent: MarkedContentTextIndex.Index
      /// The StructTreeRoot's /RoleMap: custom tag -> the tag it stands for.
      RoleMap: IReadOnlyDictionary<PdfName, PdfName>
      /// The document's language (the catalog's /Lang), if it says.
      Language: string option
      /// What fixes found and couldn't change (a font left as it is), said to the user as not
      /// applied: a note doesn't make a change.
      Notes: List<string> }

/// Maps each page's indirect object number to its 1-based page number.
let pageNumberIndex (document: PdfDocument) : IReadOnlyDictionary<int, int> =
    let index = Dictionary<int, int>()
    for pageNumber in 1 .. document.GetNumberOfPages() do
        match document.GetPage(pageNumber).GetPdfObject().GetIndirectReference() with
        | null -> ()
        | reference -> index[reference.GetObjNumber()] <- pageNumber
    index

let private roleMap (document: PdfDocument) : IReadOnlyDictionary<PdfName, PdfName> =
    let map = Dictionary<PdfName, PdfName>()
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> ()
    | structTreeRoot ->
        match structTreeRoot.GetAsDictionary(PdfName.RoleMap) with
        | null -> ()
        | roles ->
            for key in roles.KeySet() do
                match roles.GetAsName key with
                | null -> ()
                | role -> map[key] <- role
    map

/// Pages as a list for a message: "3", "3, 5 and 8".
let pageList (pages: int seq) =
    match List.ofSeq pages |> List.map string |> List.rev with
    | [] -> ""
    | [ one ] -> one
    | last :: rest -> $"""{String.Join(", ", List.rev rest)} and {last}"""

/// A context for document with its pages' text already read (markedContent).
let createContextWith (document: PdfDocument) (markedContent: MarkedContentTextIndex.Index) =
    { PageNumberByObject = pageNumberIndex document
      MarkedContent = markedContent
      RoleMap = roleMap document
      Language =
        match document.GetCatalog().GetPdfObject().GetAsString PdfName.Lang with
        | NonNull language when not (String.IsNullOrWhiteSpace(language.ToUnicodeString())) -> Some(language.ToUnicodeString().Trim())
        | _ -> None
      Notes = List<string>() }

let createContext (document: PdfDocument) (pdfPath: string) =
    createContextWith document (MarkedContentTextIndex.build pdfPath)

/// Unions geometries that are all on one page; None if there are none or they span pages.
let unionGeometry (geometries: StructureGeometry seq) =
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

let resolve (obj: PdfObject | null) : PdfObject | null =
    match obj with
    | :? PdfIndirectReference as reference -> reference.GetRefersTo()
    | obj -> obj

let structElemPage (dictionary: PdfDictionary) context (inheritedPage: int option) =
    match dictionary.GetAsDictionary(PdfName.Pg) |?> _.GetIndirectReference() with
    | null -> inheritedPage
    | reference -> lookup context.PageNumberByObject (reference.GetObjNumber()) |> Option.orElse inheritedPage

/// A structure element's page: its own /Pg, or else its nearest ancestor's (following /P).
let ancestorPage (dictionary: PdfDictionary) context =
    let rec climb (dictionary: PdfDictionary | null) depth =
        match dictionary with
        | null -> None
        | _ when depth > 200 -> None
        | dictionary ->
            match structElemPage dictionary context None with
            | Some page -> Some page
            | None -> climb (dictionary.GetAsDictionary(PdfName.P)) (depth + 1)
    climb dictionary 0

/// The language a structure element's text is in: its own /Lang, else its nearest ancestor's
/// (following /P), else the document's.
let languageOf (dictionary: PdfDictionary) context =
    let rec climb (dictionary: PdfDictionary | null) depth =
        match dictionary with
        | null -> context.Language
        | _ when depth > 200 -> context.Language
        | dictionary ->
            match dictionary.GetAsString PdfName.Lang with
            | NonNull language when not (String.IsNullOrWhiteSpace(language.ToUnicodeString())) -> Some(language.ToUnicodeString().Trim())
            | _ -> climb (dictionary.GetAsDictionary(PdfName.P)) (depth + 1)
    climb dictionary 0

/// An annotation's /Rect as geometry on pageNumber.
let rectGeometry pageNumber (annotation: PdfDictionary) =
    match annotation.GetAsArray(PdfName.Rect) with
    | NonNull rect when rect.Size() >= 4 ->
        let at (i: int) = rect.GetAsNumber(i) |> Option.ofObj |> Option.map _.DoubleValue()
        match at 0, at 1, at 2, at 3 with
        | Some x1, Some y1, Some x2, Some y2 ->
            Some
                { PageNumber = pageNumber
                  Left = min x1 x2
                  Bottom = min y1 y2
                  Right = max x1 x2
                  Top = max y1 y2 }
        | _ -> None
    | _ -> None

let private actualText (dictionary: PdfDictionary) =
    dictionary.GetAsString(PdfName.ActualText) |?> _.ToUnicodeString()

/// The element's tag after following the /RoleMap (e.g. LibreOffice's "Text body" -> P).
let standardTag (dictionary: PdfDictionary) context : PdfName | null =
    let rec follow (tag: PdfName) depth =
        match lookup context.RoleMap tag with
        | Some role when depth < 16 && not (role.Equals tag) -> follow role (depth + 1)
        | _ -> tag
    match dictionary.GetAsName(PdfName.S) with
    | null -> null
    | tag -> follow tag 0

let isParagraph (dictionary: PdfDictionary) context = PdfName.P.Equals(standardTag dictionary context)

let isTocItem (dictionary: PdfDictionary) context = PdfName.TOCI.Equals(standardTag dictionary context)

let private markedContent context (mcid: PdfNumber) pageNumber =
    lookup context.MarkedContent (pageNumber, mcid.IntValue())

/// Whether obj has no visible text anywhere below it (and no /ActualText standing in for it).
let rec isSubtreeEmpty (obj: PdfObject | null) context (inheritedPage: int option) =
    match resolve obj with
    | null -> true
    | :? PdfArray as array -> Seq.init (array.Size()) array.Get |> Seq.forall (fun kid -> isSubtreeEmpty kid context inheritedPage)
    // (On a page whose text couldn't be read, what it draws is unknown: not empty.)
    | :? PdfNumber as mcid when inheritedPage.IsSome ->
        not (context.MarkedContent.Unreadable.Contains inheritedPage.Value)
        && markedContent context mcid inheritedPage.Value
           |> Option.forall (fun content -> String.IsNullOrWhiteSpace content.Text)
    | :? PdfDictionary as dictionary ->
        if not (PdfName.StructElem.Equals(dictionary.GetAsName(PdfName.Type))) && isNull (dictionary.GetAsName(PdfName.S)) then
            false
        elif not (String.IsNullOrWhiteSpace(actualText dictionary)) then
            false
        else
            let ownPage = structElemPage dictionary context inheritedPage
            match dictionary.Get(PdfName.K) with
            | null -> true
            | kids -> isSubtreeEmpty kids context ownPage
    | _ -> false

/// The text a screen reader would read for obj: its /ActualText, or else its marked content.
let rec extractText (obj: PdfObject | null) context (inheritedPage: int option) : string =
    match resolve obj with
    | null -> ""
    | :? PdfArray as array ->
        let sb = StringBuilder()
        for i in 0 .. array.Size() - 1 do
            sb.Append(' ').Append(extractText (array.Get i) context inheritedPage) |> ignore
        sb.ToString()
    | :? PdfNumber as mcid when inheritedPage.IsSome ->
        markedContent context mcid inheritedPage.Value |> Option.map _.Text |> Option.defaultValue ""
    // A marked-content reference (/MCR): its MCID, on its own /Pg or the tag's page.
    | :? PdfDictionary as reference when
        isNull (reference.GetAsName PdfName.S) && not (isNull (reference.GetAsNumber PdfName.MCID)) && isNull (reference.Get PdfName.Stm)
        ->
        match structElemPage reference context inheritedPage with
        | Some page -> markedContent context (reference.GetAsNumber PdfName.MCID) page |> Option.map _.Text |> Option.defaultValue ""
        | None -> ""
    | :? PdfDictionary as dictionary ->
        match actualText dictionary with
        | NonNull text when not (String.IsNullOrWhiteSpace text) -> text
        | _ -> extractText (dictionary.Get(PdfName.K)) context (structElemPage dictionary context inheritedPage)
    | _ -> ""

/// An element as a finding's location names it: its tag, in words, and the start of its text (or
/// its alt text), e.g. paragraph (P) "The garden in spring…".
let describeElement (element: PdfDictionary) context (inheritedPage: int option) =
    let tag = standardTag element context |> Option.ofObj |> Option.map _.GetValue() |> Option.defaultValue "element"
    let friendly = PdfLocationParser.friendlyTag tag
    let name =
        if friendly.Contains '(' || String.Equals(friendly, tag, StringComparison.OrdinalIgnoreCase) then friendly
        else $"{friendly} ({tag})"
    let text =
        let own = Text.RegularExpressions.Regex.Replace(extractText element context (structElemPage element context inheritedPage), @"\s+", " ").Trim()
        if own <> "" then own
        else element.GetAsString PdfName.Alt |> Option.ofObj |> Option.map (fun alt -> alt.ToUnicodeString().Trim()) |> Option.defaultValue ""
    match text with
    | "" -> name
    | text when text.Length > 40 -> $"{name} \"{text.Substring(0, 40).TrimEnd()}…\""
    | text -> $"{name} \"{text}\""

let rec private collectGeometries (obj: PdfObject | null) context (inheritedPage: int option) : StructureGeometry list =
    match resolve obj with
    | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do yield! collectGeometries (array.Get i) context inheritedPage ]
    | :? PdfNumber as mcid when inheritedPage.IsSome ->
        markedContent context mcid inheritedPage.Value |> Option.bind _.Geometry |> Option.toList
    | :? PdfDictionary as dictionary ->
        let page = structElemPage dictionary context inheritedPage
        match dictionary.GetAsDictionary(PdfName.Obj), page with
        // An object reference (/OBJR), e.g. a link's or form field's annotation.
        | NonNull annotation, Some page -> rectGeometry page annotation |> Option.toList
        | NonNull _, None -> []
        // A marked-content reference (/MCR) names its MCID in /MCID (some writers put it in
        // /K), and possibly its own /Pg; /K recurses into nested structure elements too.
        | Null, Some page when not (isNull (dictionary.GetAsNumber PdfName.MCID)) ->
            markedContent context (dictionary.GetAsNumber PdfName.MCID) page |> Option.bind _.Geometry |> Option.toList
        | Null, _ -> collectGeometries (dictionary.Get(PdfName.K)) context page
    | _ -> []

/// Best-effort bounding box of everything drawn under obj, if it's all on one page.
let subtreeGeometry (obj: PdfObject | null) context (inheritedPage: int option) =
    collectGeometries obj context inheritedPage |> unionGeometry

let rec private collectTextGeometries (obj: PdfObject | null) context (inheritedPage: int option) : StructureGeometry list =
    match resolve obj with
    | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do yield! collectTextGeometries (array.Get i) context inheritedPage ]
    | :? PdfNumber as mcid when inheritedPage.IsSome ->
        markedContent context mcid inheritedPage.Value |> Option.bind _.Geometry |> Option.toList
    | :? PdfDictionary as dictionary when isNull (dictionary.GetAsDictionary(PdfName.Obj)) ->
        let page = structElemPage dictionary context inheritedPage
        match dictionary.GetAsNumber PdfName.MCID, page with
        // A marked-content reference (/MCR) with its MCID in /MCID.
        | NonNull mcid, Some page -> markedContent context mcid page |> Option.bind _.Geometry |> Option.toList
        | _ -> collectTextGeometries (dictionary.Get(PdfName.K)) context page
    | _ -> []

/// Like subtreeGeometry, but only what's drawn: annotation rectangles (/OBJR) are left out,
/// since a link's rectangle often spans the whole line whatever its text's indent.
let textGeometry (obj: PdfObject | null) context (inheritedPage: int option) =
    collectTextGeometries obj context inheritedPage |> unionGeometry

/// A table-of-contents entry's own text without its dot leaders and printed page number:
/// "Scope of this document ..... 3" becomes "Scope of this document". Text without leaders
/// is kept as-is. Returns None when nothing usable is left.
let tocEntryText (text: string) =
    let collapsed = Regex.Replace(text, @"\s+", " ").Trim()
    // Dot/underscore/middle-dot leaders, whether or not they're spaced out.
    let leaders = Regex(@"[\s]*(?:[.\u00B7_]\s?){3,}[\s]*")
    // A trailing number is only a printed page number if leaders ran up to it. Without
    // that evidence it's part of the link's own text, as in "Appendix 2" or "Form 5500".
    let described =
        if leaders.IsMatch collapsed then
            Regex.Replace(leaders.Replace(collapsed, " "), @"\s+\d{1,4}$", "")
        else
            collapsed
    let description = described.Trim([| ' '; '.'; '\u00B7'; '_'; '-'; '\u2013'; '\u2014' |]).Trim()
    if String.IsNullOrWhiteSpace description then None else Some description

/// The annotations a structure element points at through its own /OBJR children. Nested
/// structure elements are left alone — they describe themselves.
let ownAnnotations (element: PdfDictionary) =
    let ofObject (obj: PdfObject | null) =
        match resolve obj with
        | :? PdfDictionary as dictionary -> dictionary.GetAsDictionary(PdfName.Obj) |> Option.ofObj
        | _ -> None

    match resolve (element.Get(PdfName.K)) with
    | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do yield! Option.toList (ofObject (array.Get i)) ]
    | :? PdfDictionary as dictionary -> Option.toList (ofObject dictionary)
    | _ -> []

/// One dictionary in the structure tree, as seen while walking it in reading order.
type StructElemVisit =
    { Element: PdfDictionary
      /// Where the element sits, e.g. "StructTreeRoot/K[0]/K[3]".
      Path: string
      /// The element's page: its own /Pg, or its nearest ancestor's.
      Page: int option
      /// The element that follows this one in its parent's /K array, if it's a dictionary.
      NextSibling: StructElemVisit option }

/// Every dictionary below the StructTreeRoot, parents before children.
let walk (structTreeRoot: PdfDictionary) context : StructElemVisit seq =
    // Each element once: tags that loop back on themselves (or a tag in two places) would
    // otherwise be walked forever. Per walk through, so walking again sees them all again.
    let rec visitKids (visited: HashSet<PdfDictionary>) (kids: PdfObject | null) path inheritedPage : StructElemVisit seq =
        match resolve kids with
        | :? PdfArray as array ->
            let items =
                [| for i in 0 .. array.Size() - 1 ->
                       match resolve (array.Get i) with
                       | :? PdfDictionary as dictionary when not (visited.Contains dictionary) -> Some(dictionary, $"{path}[{i}]")
                       | _ -> None |]

            let visitAt i =
                items[i]
                |> Option.map (fun (dictionary, itemPath) ->
                    { Element = dictionary
                      Path = itemPath
                      Page = structElemPage dictionary context inheritedPage
                      NextSibling = None })

            seq {
                for i in 0 .. items.Length - 1 do
                    match visitAt i with
                    | Some visit when visited.Add visit.Element ->
                        let visit = { visit with NextSibling = if i + 1 < items.Length then visitAt (i + 1) else None }
                        yield visit
                        yield! visitKids visited (visit.Element.Get(PdfName.K)) $"{visit.Path}/K" visit.Page
                    | _ -> ()
            }
        | :? PdfDictionary as dictionary when not (visited.Contains dictionary) ->
            let visit =
                { Element = dictionary
                  Path = path
                  Page = structElemPage dictionary context inheritedPage
                  NextSibling = None }
            seq {
                if visited.Add dictionary then
                    yield visit
                    yield! visitKids visited (dictionary.Get(PdfName.K)) $"{path}/K" visit.Page
            }
        | _ -> Seq.empty

    seq {
        let visited = HashSet<PdfDictionary>(HashIdentity.Reference)
        visited.Add structTreeRoot |> ignore
        yield! visitKids visited (structTreeRoot.Get(PdfName.K)) "StructTreeRoot/K" (structElemPage structTreeRoot context None)
    }
