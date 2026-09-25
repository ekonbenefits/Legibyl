/// Structural repairs that work on the raw PDF objects via iText: removing empty /P
/// elements and setting annotation tab order. The warnings that point users at these
/// problems live in LegibylChecks.
module internal Legibyl.Validation.StructureFixes

open iText.Kernel.Pdf
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
