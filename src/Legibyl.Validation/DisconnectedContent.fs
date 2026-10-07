/// Reverse-only tags are evidence, not another reading-order hierarchy.
module internal Legibyl.Validation.DisconnectedContent

open System
open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

let private node tag content page geometry leaves children : StructureNode =
    { Tag = tag
      Role = null
      AlternateDescription = null
      ActualText = null
      Content = content
      FormFieldName = null
      FormFieldDescription = null
      FormFieldType = null
      FormFieldValue = null
      LinkUri = null
      LinkDestinationPageNumber = Nullable()
      LinkDestinationDescription = null
      LinkDestination = null
      LinkDescription = null
      Geometry = geometry
      Language = null
      PageNumber = Nullable page
      ContentLeafCount = leaves
      ColSpan = 1
      RowSpan = 1
      Children = Array.ofList children }

let nodes (pdfPath: string) =
    use document = new PdfDocument(new PdfReader(pdfPath))
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> []
    | _ ->
        let context = createContext document pdfPath
        ParentTree.inconsistent document context
        |> List.filter (fun (_, _, _, expected) -> expected.IsNone)
        |> List.groupBy (fun (page, _, owner, _) ->
            page, (match owner.GetIndirectReference() with null -> owner.GetHashCode() | reference -> reference.GetObjNumber()))
        |> List.groupBy (fun ((page, _), _) -> page)
        |> List.map (fun (page, groups) ->
            let children =
                [ for (_, objectNumber), associations in groups do
                      let _, _, owner, _ = associations.Head
                      let mcids = associations |> List.map (fun (_, mcid, _, _) -> mcid)
                      let content =
                          [ for mcid in mcids do
                                match context.MarkedContent.TryGetValue((page, mcid)) with
                                | true, content -> content
                                | _ -> () ]
                      let geometry = content |> Seq.choose _.Geometry |> unionGeometry |> Option.toObj
                      let role = match owner.GetAsName PdfName.S with null -> "unknown" | role -> role.GetValue()
                      let ids = mcids |> List.map string |> String.concat ", "
                      let details =
                          [ $"Disconnected <{role}>, object {objectNumber}, MCID(s) {ids}. Not in the reading order; no position is inferred."
                            if context.MarkedContent.Unreadable.Contains page then "The page's text could not be read."
                            yield! content |> List.map _.Text |> List.filter (String.IsNullOrWhiteSpace >> not) |> List.distinct
                            for key, label in [PdfName.Alt, "alt"; PdfName.ActualText, "replacement text"] do
                                match owner.GetAsString key with
                                | null -> ()
                                | text -> $"{label}: {text.ToUnicodeString()}" ]
                          |> String.concat "\n"
                      yield node "DisconnectedTag" details page geometry mcids.Length [] ]
            node "Disconnected" "Disconnected associations — outside the reading order. Review these separately; Rebuild the parent tree clears only demonstrably empty or explicitly decorative content and reports meaningful content it cannot place." page null 0 children)
