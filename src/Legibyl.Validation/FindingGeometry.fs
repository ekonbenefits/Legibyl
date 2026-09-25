/// Works out the page region a veraPDF finding refers to, from its location context, so
/// it can be highlighted like Legibyl's own warnings.
module internal Legibyl.Validation.FindingGeometry

open System
open System.Text.RegularExpressions
open iText.Kernel.Pdf
open Legibyl.Validation.Interop
open Legibyl.Validation.StructureTreeObjects

let private mcidPattern = Regex(@"\{mcid:(\d+)\}", RegexOptions.Compiled)

/// Content inside a form XObject numbers its MCIDs separately from the page, so the
/// page's (page, MCID) index can't be trusted for it.
let private isInsideXObject (context: string) =
    context.Contains("xobject", StringComparison.OrdinalIgnoreCase)

/// "pages[0](…)/contentStream[0](…)/content[10]{mcid:22}": the marked content's glyphs.
let private markedContentGeometry context (locationContext: string) (location: PdfLocation) =
    let matches = mcidPattern.Matches locationContext
    if matches.Count = 0 || not location.PageNumber.HasValue || isInsideXObject locationContext then
        None
    else
        let mcid = int matches[matches.Count - 1].Groups[1].Value
        lookup context.MarkedContent (location.PageNumber.Value, mcid) |> Option.bind _.Geometry

/// ".../K[3](12 0 obj SEP P)" or ".../annots[0](9 0 obj PDLinkAnnot)": the object itself.
let private objectGeometry context (document: PdfDocument) (location: PdfLocation) =
    if not location.ObjectNumber.HasValue || location.ObjectNumber.Value >= document.GetNumberOfPdfObjects() then
        None
    else
        let locationPage = Option.ofNullable location.PageNumber
        match document.GetPdfObject(location.ObjectNumber.Value) with
        | :? PdfDictionary as dictionary when not (isNull (dictionary.GetAsArray(PdfName.Rect))) ->
            let page =
                locationPage
                |> Option.orElseWith (fun () ->
                    dictionary.GetAsDictionary(PdfName.P) |?> _.GetIndirectReference()
                    |> Option.ofObj
                    |> Option.bind (fun reference -> lookup context.PageNumberByObject (reference.GetObjNumber())))
            page |> Option.bind (fun page -> rectGeometry page dictionary)
        | :? PdfDictionary as dictionary when not (isNull (dictionary.GetAsName(PdfName.S))) ->
            let page = ancestorPage dictionary context |> Option.orElse locationPage
            subtreeGeometry dictionary context page
        | _ -> None

/// Best-effort page region for a veraPDF finding; None when its location doesn't pin
/// down anything drawn on a page (document-level checks, whole pages, etc.).
let locate context (document: PdfDocument) (violation: RuleViolation) : StructureGeometry option =
    match violation.LocationContext, violation.Location with
    | NonNull locationContext, NonNull location ->
        markedContentGeometry context locationContext location
        |> Option.orElseWith (fun () -> objectGeometry context document location)
    | _ -> None
