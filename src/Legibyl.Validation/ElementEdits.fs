/// Writes the edits a person made (see ElementEdit) to the elements they were made for: each
/// edit's element is the one with its tag on its page whose box overlaps its box most.
module Legibyl.Validation.ElementEdits

open System
open iText.IO.Font
open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

/// How much an element's box must overlap an edit's to be its element: their intersection over
/// their union, as for a region in Suggest Structure. Repairs and regenerated PDFs move things a
/// little; different elements with the same tag rarely overlap this much.
[<Literal>]
let SameElementOverlap = 0.6

/// The intersection over union of two boxes.
let overlap (a: StructureGeometry) (b: StructureGeometry) =
    let area (box: StructureGeometry) = max 0.0 (box.Right - box.Left) * max 0.0 (box.Top - box.Bottom)
    let intersection = max 0.0 (min a.Right b.Right - max a.Left b.Left) * max 0.0 (min a.Top b.Top - max a.Bottom b.Bottom)
    let union = area a + area b - intersection
    if union <= 0.0 then 0.0 else intersection / union

/// Each edit with its element, for the edits whose element is found.
let internal find (document: PdfDocument) context (edits: ElementEdit list) : (ElementEdit * PdfDictionary) list =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> []
    | _ when edits.IsEmpty -> []
    | structTreeRoot ->
        let wanted = edits |> List.map (fun e -> e.Page, e.Tag) |> set
        // Only elements an edit could be for, with their boxes.
        let candidates =
            [ for visit in walk structTreeRoot context do
                  match standardTag visit.Element context, visit.Page with
                  | NonNull tag, Some page when wanted.Contains((page, tag.GetValue())) ->
                      match subtreeGeometry visit.Element context visit.Page with
                      | Some box when box.PageNumber = page -> yield tag.GetValue(), box, visit.Element
                      | _ -> ()
                  | _ -> () ]
        edits
        |> List.choose (fun edit ->
            candidates
            |> List.filter (fun (tag, box, _) -> tag = edit.Tag && box.PageNumber = edit.Page)
            |> List.map (fun (_, box, element) -> element, overlap box edit.Box)
            |> List.filter (fun (_, o) -> o >= SameElementOverlap)
            |> List.sortByDescending snd
            |> List.tryHead
            |> Option.map (fun (element, _) -> edit, element))

let private textOf (element: PdfDictionary) (key: PdfName) =
    element.GetAsString key |> Option.ofObj |> Option.map _.ToUnicodeString()

/// What edit changes on element: each key with its new value, where that differs from what it has.
let private changes (edit: ElementEdit, element: PdfDictionary) =
    [ match edit.Alt with
      | NonNull alt when textOf element PdfName.Alt <> Some alt -> yield PdfName.Alt, alt
      | _ -> ()
      match edit.Text with
      | NonNull text when textOf element PdfName.ActualText <> Some text -> yield PdfName.ActualText, text
      | _ -> () ]

/// A PdfString that survives non-ASCII text.
let private textString (value: string) =
    if value |> Seq.forall (fun c -> c < '\u0080') then PdfString value
    else PdfString(value, PdfEncodings.UNICODE_BIG)

/// Writes the edits that change something; says how many (and how many elements weren't found),
/// or nothing if none changed anything.
let internal apply (document: PdfDocument) context (edits: ElementEdit list) : string list =
    let found = find document context edits
    let applied =
        [ for pair in found do
              let changes = changes pair
              for key, value in changes do
                  (snd pair).Put(key, textString value) |> ignore
              if not changes.IsEmpty then yield () ]
    let missing = edits.Length - found.Length
    [ if not applied.IsEmpty then
          if missing = 0 then
              $"Applied {applied.Length} of your edits (alt text and replacement text)."
          else
              $"Applied {applied.Length} of your edits (alt text and replacement text); {missing} weren't, as no element with their tag is in about the same place." ]

/// How many of edits would change pdfPath: their element is found and doesn't already say it.
let pending (pdfPath: string) (edits: ElementEdit list) : int =
    if edits.IsEmpty then
        0
    else
        use document = new PdfDocument(new PdfReader(pdfPath))
        find document (createContext document pdfPath) edits |> List.filter (not << List.isEmpty << changes) |> List.length
