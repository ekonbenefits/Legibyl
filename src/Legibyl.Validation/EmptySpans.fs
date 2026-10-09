/// Span tags around nothing: no content and no tags inside, with at most a space for replacement
/// text (an authoring app's spacer between words, "/S /Span /ActualText ( ) /K []"). A screen reader
/// has nothing in one to read, and veraPDF fails each as a redundant span (WCAG 2.2, 4.1.2). The fix
/// takes them out, keeping the space between the words beside them where it's needed.
module internal Legibyl.Validation.EmptySpans

open System
open System.Collections.Generic
open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

let private textOf (element: PdfDictionary) (key: PdfName) =
    match element.GetAsString key with
    | null -> ""
    | text -> text.ToUnicodeString()

/// Tags whose text runs on into the text around them, so a space at the edge of one can still be
/// between two words.
let private inlineTags =
    set [ "Span"; "Link"; "Quote"; "Note"; "Reference"; "BibEntry"; "Code"; "Annot"; "Em"; "Strong"; "Sub"
          "Ruby"; "RB"; "RT"; "RP"; "Warichu"; "WT"; "WP" ]

/// A Span with nothing in it (no /K, or an empty one), no alt text, expansion or ID, and replacement
/// text that's nothing or only spaces.
let private isEmptySpan context (element: PdfDictionary) =
    PdfName.Span.Equals(standardTag element context)
    && (match resolve (element.Get PdfName.K) with
        | null -> true
        | :? PdfArray as kids -> kids.Size() = 0
        | _ -> false)
    && String.IsNullOrWhiteSpace(textOf element PdfName.ActualText)
    && String.IsNullOrWhiteSpace(textOf element PdfName.Alt)
    && String.IsNullOrWhiteSpace(textOf element PdfName.E)
    && isNull (element.Get PdfName.ID)

/// Where the space a run of empty spans stood for goes: nowhere (there was none, or the words
/// beside them are already apart), onto the end of the replacement text of the tag before them,
/// or onto the start of the one after them.
type private Carry =
    | Dropped
    | Append of PdfDictionary * string
    | Prepend of PdfDictionary * string

/// A run of empty spans side by side in their parent, and what's done with it: Some carry to take
/// them out, None to leave them (a space between words that can't be kept another way).
type private Run =
    { Parent: PdfDictionary
      Spans: PdfDictionary list
      Carry: Carry option }

let private runs (document: PdfDocument) context : Run list =
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> []
    | root ->
        let parents =
            seq {
                yield root, None
                for visit in walk root context do
                    if not (isNull (visit.Element.GetAsName PdfName.S)) then
                        yield visit.Element, visit.Page
            }
        [ for parent, page in parents do
              let kids: PdfObject list =
                  match resolve (parent.Get PdfName.K) with
                  | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 -> array.Get i ]
                  | :? PdfDictionary as only -> [ only ]
                  | _ -> []
              let kids = Array.ofList kids
              let emptyAt i =
                  match resolve kids[i] with
                  | :? PdfDictionary as element when isEmptySpan context element -> Some element
                  | _ -> None
              let isInline =
                  match standardTag parent context with
                  | null -> false
                  | tag -> inlineTags.Contains(tag.GetValue())
              let mutable i = 0
              while i < kids.Length do
                  match emptyAt i with
                  | None -> i <- i + 1
                  | Some _ ->
                      let first = i
                      while i < kids.Length && (emptyAt i).IsSome do
                          i <- i + 1
                      let spans = [ for j in first .. i - 1 -> (emptyAt j).Value ]
                      let space =
                          spans
                          |> List.map (fun span -> textOf span PdfName.ActualText)
                          |> List.tryFind (fun text -> text <> "")
                      let before = if first > 0 then Some kids[first - 1] else None
                      let after = if i < kids.Length then Some kids[i] else None
                      let ownText (kid: PdfObject) =
                          match resolve kid with
                          | :? PdfDictionary as element when not (String.IsNullOrWhiteSpace(textOf element PdfName.ActualText)) ->
                              Some element
                          | _ -> None
                      // What's read for a kid, as it runs on into the next (extractText puts a
                      // space between an array's items, which would hide whether there's one).
                      let rec readText (kid: PdfObject | null) page depth =
                          match resolve kid with
                          | _ when depth > 100 -> ""
                          | :? PdfArray as array -> String.Join("", [ for j in 0 .. array.Size() - 1 -> readText (array.Get j) page (depth + 1) ])
                          | :? PdfDictionary as element when not (isNull (element.GetAsName PdfName.S)) ->
                              match textOf element PdfName.ActualText with
                              | text when not (String.IsNullOrWhiteSpace text) -> text
                              | _ -> readText (element.Get PdfName.K) (structElemPage element context page) (depth + 1)
                          | other -> extractText other context page
                      let carry =
                          match space, before, after with
                          | None, _, _ -> Some Dropped
                          // At the edge of a block (a paragraph, a cell), a space separates nothing.
                          | Some _, None, _
                          | Some _, _, None -> if isInline then None else Some Dropped
                          | Some space, Some before, Some after ->
                              let beforeText = readText before page 0
                              let afterText = readText after page 0
                              if beforeText <> "" && Char.IsWhiteSpace beforeText[beforeText.Length - 1]
                                 || afterText <> "" && Char.IsWhiteSpace afterText[0] then
                                  Some Dropped
                              else
                                  match ownText before, ownText after with
                                  | Some element, _ -> Some(Append(element, space))
                                  | None, Some element -> Some(Prepend(element, space))
                                  | None, None -> None
                      yield { Parent = parent; Spans = spans; Carry = carry } ]

let private objectNumbers (spans: PdfDictionary seq) =
    set [ for span in spans do
              match span.GetIndirectReference() with
              | null -> ()
              | reference -> yield reference.GetObjNumber() ]

/// The object numbers of the empty spans the fix takes out: so validation can say which of
/// veraPDF's redundant spans it fixes.
let removable (document: PdfDocument) context : Set<int> =
    runs document context |> List.filter _.Carry.IsSome |> List.collect _.Spans |> objectNumbers

let private withText (value: string) =
    if value |> Seq.forall (fun c -> c < '\u0080') then PdfString value
    else PdfString(value, iText.IO.Font.PdfEncodings.UNICODE_BIG)

/// Takes the empty spans out of their parents, the space they stood for onto the replacement text
/// beside them where the words would otherwise run together. Those it can't keep the space for
/// (the text beside them isn't replacement text, which can't take one) are left, and said.
let remove (document: PdfDocument) context : string list =
    let all = runs document context
    let left = all |> List.filter _.Carry.IsNone |> List.sumBy _.Spans.Length
    if left > 0 then
        context.Notes.Add
            $"Left {left} empty span tag(s) that stand for a space between words: the text beside them isn't replacement text, so the space couldn't go there instead."
    let taken = all |> List.filter _.Carry.IsSome
    let mutable moved = 0
    for run in taken do
        match run.Carry with
        | Some(Append(element, space)) ->
            element.Put(PdfName.ActualText, withText (textOf element PdfName.ActualText + space)) |> ignore
            moved <- moved + 1
        | Some(Prepend(element, space)) ->
            element.Put(PdfName.ActualText, withText (space + textOf element PdfName.ActualText)) |> ignore
            moved <- moved + 1
        | _ -> ()
        let out = HashSet<PdfDictionary>(run.Spans, HashIdentity.Reference)
        match resolve (run.Parent.Get PdfName.K) with
        | :? PdfArray as kids ->
            for i in kids.Size() - 1 .. -1 .. 0 do
                match resolve (kids.Get i) with
                | :? PdfDictionary as kid when out.Contains kid -> kids.Remove i
                | _ -> ()
        | :? PdfDictionary as only when out.Contains only -> run.Parent.Remove PdfName.K |> ignore
        | _ -> ()
    match taken |> List.sumBy _.Spans.Length with
    | 0 -> []
    | count ->
        [ $"Removed {count} span tag(s) with nothing in them."
          if moved > 0 then
              $"Kept the space {moved} of them stood for between words, in the replacement text of the tag beside it." ]
