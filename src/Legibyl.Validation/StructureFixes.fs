/// Structural repairs that work on the raw PDF objects via iText: removing empty /P
/// elements, setting annotation tab order, describing links, and naming the document.
/// The warnings that point users at these problems live in LegibylChecks.
module internal Legibyl.Validation.StructureFixes

open System
open System.IO
open System.Text.RegularExpressions
open iText.IO.Font
open iText.Kernel.Pdf
open iText.Kernel.XMP
open Legibyl.Validation.Interop
open Legibyl.Validation.StructureTreeObjects

/// Removes empty /P elements below dictionary; returns how many were removed.
let rec private removeEmptyParagraphsInside (dictionary: PdfDictionary) context inheritedPage : int =
    match dictionary.Get(PdfName.K) with
    | null -> 0
    | kids ->
        let pageNumber = structElemPage dictionary context inheritedPage
        match shouldRemoveEmptyParagraph kids context pageNumber with
        | true, removedFromChild ->
            dictionary.Remove(PdfName.K) |> ignore
            removedFromChild + 1
        | false, _ -> removeEmptyParagraphsFromKids kids context pageNumber

and private removeEmptyParagraphsFromKids (kids: PdfObject) context inheritedPage : int =
    match resolve kids with
    | :? PdfArray as array ->
        let mutable removed = 0
        for i in array.Size() - 1 .. -1 .. 0 do
            let remove, childRemoved = shouldRemoveEmptyParagraph (array.Get i) context inheritedPage
            if remove then
                array.Remove i
                removed <- removed + 1
            removed <- removed + childRemoved
        removed
    | :? PdfDictionary as dictionary -> removeEmptyParagraphsInside dictionary context inheritedPage
    | _ -> 0

/// Cleans up obj's own children first, then reports whether obj itself is an empty /P
/// (plus how many were removed below it).
and private shouldRemoveEmptyParagraph (obj: PdfObject | null) context inheritedPage : bool * int =
    match resolve obj with
    | :? PdfDictionary as dictionary ->
        let pageNumber = structElemPage dictionary context inheritedPage
        let removedFromChildren = removeEmptyParagraphsInside dictionary context pageNumber
        isParagraph dictionary context && isSubtreeEmpty dictionary context pageNumber, removedFromChildren
    | _ -> false, 0

// ---- Link /Contents ----

/// A PdfString that survives non-ASCII text.
let private textString (value: string) =
    if value |> Seq.forall (fun c -> c < '\u0080') then PdfString value
    else PdfString(value, PdfEncodings.UNICODE_BIG)

let private isBlankString (value: PdfString | null) =
    match value with
    | null -> true
    | value -> String.IsNullOrWhiteSpace(value.ToUnicodeString())

/// Turns a link's own visible text into an alternate description. Ordinary links keep
/// their text as-is; a table-of-contents entry ("Scope of this document ..... 3") loses
/// its dot leaders and printed page number to become "Scope of this document".
/// Returns None when nothing usable is left.
let private describeLinkText (text: string) =
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
let private ownAnnotations (element: PdfDictionary) =
    let ofObject (obj: PdfObject | null) =
        match resolve obj with
        | :? PdfDictionary as dictionary -> dictionary.GetAsDictionary(PdfName.Obj) |> Option.ofObj
        | _ -> None

    match resolve (element.Get(PdfName.K)) with
    | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 do yield! Option.toList (ofObject (array.Get i)) ]
    | :? PdfDictionary as dictionary -> Option.toList (ofObject dictionary)
    | _ -> []

/// Gives every /Link structure element's annotation a /Contents taken from the link's own
/// text, so screen readers can announce where the link goes. Returns how many were set.
let private describeLinks (structTreeRoot: PdfDictionary) context =
    let mutable described = 0
    for visit in walk structTreeRoot context do
        if PdfName.Link.Equals(standardTag visit.Element context) then
            match ownAnnotations visit.Element |> List.filter (fun a -> isBlankString (a.GetAsString PdfName.Contents)) with
            | [] -> ()
            | annotations ->
                match describeLinkText (extractText visit.Element context visit.Page) with
                | None -> ()
                | Some description ->
                    for annotation in annotations do
                        annotation.Put(PdfName.Contents, textString description) |> ignore
                        described <- described + 1
    described

// ---- Document title ----

/// The document's XMP `dc:title`, if it has one. Best-effort: a malformed XMP packet
/// should not stop the rest of the repairs.
let private xmpTitle (document: PdfDocument) =
    try
        match document.GetXmpMetadata() with
        | null -> None
        | xmp ->
            match xmp.GetLocalizedText(XMPConst.NS_DC, "title", null, XMPConst.X_DEFAULT) with
            | null -> None
            | property ->
                match property.GetValue() with
                | NonNull value when not (String.IsNullOrWhiteSpace value) -> Some value
                | _ -> None
    with _ ->
        None

/// Gives the document a /Title and asks readers to show it instead of the file name.
/// A title the document already carries is kept — in the Info dictionary, or in XMP,
/// where it only needs copying across.
///
/// A title is never invented. Guessing one from the file name would satisfy dc:title
/// while telling a screen reader user something untrue about the document, so a
/// document that carries no title anywhere keeps reporting that as a failure.
let private nameDocument (document: PdfDocument) =
    let info = document.GetDocumentInfo()
    let titleSet =
        if not (String.IsNullOrWhiteSpace(info.GetTitle())) then
            None
        else
            xmpTitle document
            |> Option.map (fun title ->
                info.SetTitle title |> ignore
                title)

    let catalog = document.GetCatalog().GetPdfObject()
    let preferences =
        match catalog.GetAsDictionary(PdfName.ViewerPreferences) with
        | null ->
            let preferences = PdfDictionary()
            catalog.Put(PdfName.ViewerPreferences, preferences) |> ignore
            preferences
        | preferences -> preferences

    let displayDocTitleSet =
        if PdfBoolean.TRUE.Equals(preferences.Get(PdfName.DisplayDocTitle)) then
            false
        else
            preferences.Put(PdfName.DisplayDocTitle, PdfBoolean.TRUE) |> ignore
            true

    titleSet, displayDocTitleSet

// ---- Running the fixes ----

/// The structural repairs Legibyl makes itself, writing the result to outputPath.
/// Returns one plain-language line per repair that actually changed something, so a
/// document that already had a given fix applied reports nothing for it.
let applyFixes (inputPath: string) (outputPath: string) (options: PdfFixOptions) : string list =
    use reader = new PdfReader(inputPath)
    use writer = new PdfWriter(outputPath)
    use document = new PdfDocument(reader, writer)
    let context = createContext document inputPath
    let structTreeRoot = document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot)

    let mutable tabsFixed = 0
    if options.HasFlag PdfFixOptions.AnnotationTabs then
        for pageNumber in 1 .. document.GetNumberOfPages() do
            let page = document.GetPage(pageNumber).GetPdfObject()
            match page.GetAsArray(PdfName.Annots) with
            | null -> ()
            | annotations when annotations.Size() = 0 -> ()
            | _ ->
                if not (PdfName.S.Equals(page.GetAsName(PdfName.Tabs))) then
                    page.Put(PdfName.Tabs, PdfName.S) |> ignore
                    tabsFixed <- tabsFixed + 1

    let emptyParagraphsRemoved =
        match structTreeRoot with
        | NonNull structTreeRoot when options.HasFlag PdfFixOptions.RemoveEmptyParagraphs ->
            removeEmptyParagraphsInside structTreeRoot context None
        | _ -> 0

    let linksDescribed =
        match structTreeRoot with
        | NonNull structTreeRoot when options.HasFlag PdfFixOptions.LinkContents -> describeLinks structTreeRoot context
        | _ -> 0

    let titleSet, displayDocTitleSet =
        if options.HasFlag PdfFixOptions.DocumentTitle then
            nameDocument document
        else
            None, false

    [ if tabsFixed > 0 then $"Set /Tabs /S on {tabsFixed} page(s) containing annotations."
      if emptyParagraphsRemoved > 0 then $"Removed {emptyParagraphsRemoved} empty /P structure element(s)."
      if linksDescribed > 0 then $"Described {linksDescribed} link annotation(s) from their own text."
      match titleSet with
      | Some title -> $"Set the document title to \"{title}\"."
      | None -> ()
      if displayDocTitleSet then "Set /DisplayDocTitle so readers show the document title rather than the file name." ]
