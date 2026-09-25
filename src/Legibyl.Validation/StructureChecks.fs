/// Structure-tree checks and repairs that work on the raw PDF objects via iText:
/// empty /P detection and removal, likely split paragraphs, and annotation tab order.
module internal Legibyl.Validation.StructureChecks

open System
open System.Collections.Generic
open System.IO
open System.Text
open System.Text.RegularExpressions
open iText.Kernel.Pdf
open Legibyl.Validation.Interop

type Context =
    { PageNumberByObject: IReadOnlyDictionary<int, int>
      MarkedContent: MarkedContentTextIndex.Index }

/// Maps each page's indirect object number to its 1-based page number.
let pageNumberIndex (document: PdfDocument) : IReadOnlyDictionary<int, int> =
    let index = Dictionary<int, int>()
    for pageNumber in 1 .. document.GetNumberOfPages() do
        match document.GetPage(pageNumber).GetPdfObject().GetIndirectReference() with
        | null -> ()
        | reference -> index[reference.GetObjNumber()] <- pageNumber
    index

let private createContext (document: PdfDocument) (pdfPath: string) =
    { PageNumberByObject = pageNumberIndex document
      MarkedContent = MarkedContentTextIndex.build pdfPath }

let private resolve (obj: PdfObject | null) : PdfObject | null =
    match obj with
    | :? PdfIndirectReference as reference -> reference.GetRefersTo()
    | obj -> obj

let private structElemPage (dictionary: PdfDictionary) context (inheritedPage: int option) =
    match dictionary.GetAsDictionary(PdfName.Pg) |?> _.GetIndirectReference() with
    | null -> inheritedPage
    | reference -> lookup context.PageNumberByObject (reference.GetObjNumber()) |> Option.orElse inheritedPage

let private actualText (dictionary: PdfDictionary) =
    dictionary.GetAsString(PdfName.ActualText) |?> _.ToUnicodeString()

let private isParagraph (dictionary: PdfDictionary) = PdfName.P.Equals(dictionary.GetAsName(PdfName.S))

let private markedContentText context (mcid: PdfNumber) pageNumber =
    lookup context.MarkedContent (pageNumber, mcid.IntValue()) |> Option.map _.Text

let rec private isSubtreeEmpty (obj: PdfObject | null) context (inheritedPage: int option) =
    match resolve obj with
    | null -> true
    | :? PdfArray as array -> Seq.init (array.Size()) array.Get |> Seq.forall (fun kid -> isSubtreeEmpty kid context inheritedPage)
    | :? PdfNumber as mcid when inheritedPage.IsSome ->
        markedContentText context mcid inheritedPage.Value
        |> Option.forall String.IsNullOrWhiteSpace
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
        isParagraph dictionary && isSubtreeEmpty dictionary context pageNumber, removedFromChildren
    | _ -> false, 0

/// Sets /Tabs /S on annotated pages and/or removes empty /P structure elements, writing
/// the result to outputPath. Returns (pages whose tab order was set, paragraphs removed).
let applyFixes (inputPath: string) (outputPath: string) (options: PdfFixOptions) : int * int =
    use reader = new PdfReader(inputPath)
    use writer = new PdfWriter(outputPath)
    use document = new PdfDocument(reader, writer)
    let context = createContext document inputPath

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
        if options.HasFlag PdfFixOptions.RemoveEmptyParagraphs then
            match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
            | null -> 0
            | structTreeRoot -> removeEmptyParagraphsInside structTreeRoot context None
        else
            0

    tabsFixed, emptyParagraphsRemoved

let private legibylWarning message (errorMessage: string | null) locationContext testNumber fixOptions =
    { Specification = "Legibyl"
      Clause = "Structure"
      TestNumber = testNumber
      Passed = true
      Message = message
      ErrorMessage = errorMessage
      LocationContext = locationContext
      Severity = FindingSeverity.Warning
      FixOptions = fixOptions }

let rec private addEmptyParagraphWarnings (obj: PdfObject | null) path context inheritedPage (warnings: ResizeArray<RuleViolation>) =
    match resolve obj with
    | :? PdfArray as array ->
        for i in 0 .. array.Size() - 1 do
            addEmptyParagraphWarnings (array.Get i) $"{path}[{i}]" context inheritedPage warnings
    | :? PdfDictionary as dictionary ->
        let pageNumber = structElemPage dictionary context inheritedPage
        if isParagraph dictionary && isSubtreeEmpty dictionary context pageNumber then
            warnings.Add(
                legibylWarning
                    "Empty semantic /P structure element. It can add meaningless stops to the screen-reader structure tree and can be removed by the Fix dialog."
                    null
                    path
                    1
                    PdfFixOptions.RemoveEmptyParagraphs
            )
        match dictionary.Get(PdfName.K) with
        | null -> ()
        | kids -> addEmptyParagraphWarnings kids $"{path}/K" context pageNumber warnings
    | _ -> ()

let rec private extractStructureText (obj: PdfObject | null) context (inheritedPage: int option) : string =
    match resolve obj with
    | null -> ""
    | :? PdfArray as array ->
        let sb = StringBuilder()
        for i in 0 .. array.Size() - 1 do
            sb.Append(' ').Append(extractStructureText (array.Get i) context inheritedPage) |> ignore
        sb.ToString()
    | :? PdfNumber as mcid when inheritedPage.IsSome ->
        markedContentText context mcid inheritedPage.Value |> Option.defaultValue ""
    | :? PdfDictionary as dictionary ->
        match actualText dictionary with
        | NonNull text when not (String.IsNullOrWhiteSpace text) -> text
        | _ -> extractStructureText (dictionary.Get(PdfName.K)) context (structElemPage dictionary context inheritedPage)
    | _ -> ""

let private normalizeText (text: string) = Regex.Replace(text, @"\s+", " ").Trim()

let private isLikelyStandaloneParagraph (text: string) =
    text.Length <= 3
    || Regex.IsMatch(text, @"^(SECTION|ARTICLE)\b", RegexOptions.IgnoreCase)
    || Regex.IsMatch(text, @"^\d+(\.\d+)*\s+\S+")
    || Regex.IsMatch(text, @"^[a-z]\.\s+\S+", RegexOptions.IgnoreCase)

let private isLikelySplitParagraph (currentText: string) (nextText: string) =
    let startsWith (prefix: string) = nextText.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)

    currentText.Length >= 20
    && nextText.Length >= 10
    && not (isLikelyStandaloneParagraph currentText || isLikelyStandaloneParagraph nextText)
    && not (".:;!?…".Contains currentText[currentText.Length - 1])
    && (Char.IsLower nextText[0]
        || startsWith "and "
        || startsWith "or "
        || startsWith "to "
        || startsWith "of "
        || startsWith "the ")

let private addLikelySplitParagraphWarningsForArray (array: PdfArray) path context inheritedPage (warnings: ResizeArray<RuleViolation>) =
    for i in 0 .. array.Size() - 2 do
        match resolve (array.Get i), resolve (array.Get(i + 1)) with
        | (:? PdfDictionary as current), (:? PdfDictionary as next) when isParagraph current && isParagraph next ->
            let currentPage = structElemPage current context inheritedPage
            let nextPage = structElemPage next context inheritedPage
            let onDifferentPages = currentPage.IsSome && nextPage.IsSome && currentPage <> nextPage
            if not onDifferentPages then
                let currentText = normalizeText (extractStructureText current context currentPage)
                let nextText = normalizeText (extractStructureText next context nextPage)
                if isLikelySplitParagraph currentText nextText then
                    warnings.Add(
                        legibylWarning
                            "Likely split paragraph: adjacent /P structure elements look like visual line fragments from the same sentence. This is best fixed in the source document if possible."
                            null
                            $"{path}[{i}] + {path}[{i + 1}]"
                            2
                            PdfFixOptions.None
                    )
        | _ -> ()

let rec private addLikelySplitParagraphWarnings (obj: PdfObject | null) path context inheritedPage (warnings: ResizeArray<RuleViolation>) =
    match resolve obj with
    | :? PdfArray as array ->
        addLikelySplitParagraphWarningsForArray array path context inheritedPage warnings
        for i in 0 .. array.Size() - 1 do
            addLikelySplitParagraphWarnings (array.Get i) $"{path}[{i}]" context inheritedPage warnings
    | :? PdfDictionary as dictionary ->
        match dictionary.Get(PdfName.K) with
        | null -> ()
        | kids -> addLikelySplitParagraphWarnings kids $"{path}/K" context (structElemPage dictionary context inheritedPage) warnings
    | _ -> ()

/// Legibyl's own structure warnings (empty /P elements, likely split paragraphs), which
/// veraPDF doesn't report.
let warnings (pdfPath: string) : RuleViolation list =
    let warnings = ResizeArray<RuleViolation>()
    try
        use document = new PdfDocument(new PdfReader(pdfPath))
        let context = createContext document pdfPath
        match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
        | null -> ()
        | structTreeRoot ->
            match structTreeRoot.Get(PdfName.K) with
            | null -> ()
            | kids ->
                let pageNumber = structElemPage structTreeRoot context None
                addEmptyParagraphWarnings kids "StructTreeRoot/K" context pageNumber warnings
                addLikelySplitParagraphWarnings kids "StructTreeRoot/K" context pageNumber warnings
    with
    | :? iText.Kernel.Exceptions.PdfException
    | :? IOException as ex ->
        warnings.Add(
            legibylWarning
                "Could not inspect the structure tree for empty paragraph tags."
                ex.Message
                "StructTreeRoot"
                1
                PdfFixOptions.RemoveEmptyParagraphs
        )
    List.ofSeq warnings
