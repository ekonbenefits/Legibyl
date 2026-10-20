/// Content still tied, by the parent tree, to a tag that isn't in the tags any more (see
/// ParentTree.disconnected), for Structure… to show after the tags: for each page with any, a
/// "Disconnected" node holding a "DisconnectedTag" node per tag that's gone, with its content's
/// text, description and place on the page. To look at, not to edit or move: nothing says where
/// among its neighbours that tag was read, so showing it doesn't put it back.
module internal Legibyl.Validation.DisconnectedContent

open System
open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

let private node kind tag content page geometry leaves children : StructureNode =
    { Tag = tag
      Kind = kind
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

/// The nodes for pdfPath, whose pages' text is already read (text: StructureTreeWalk.read's).
let nodes (pdfPath: string) (text: MarkedContentTextIndex.Index) =
    use document = PdfFiles.openToRead pdfPath
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> []
    | _ ->
        let context = createContextWith document text
        ParentTree.disconnected document context
        |> List.groupBy _.Page
        |> List.map (fun (page, gone) ->
            let children =
                [ for tag in gone do
                      let content =
                          [ for mcid in tag.Mcids do
                                match context.MarkedContent.TryGetValue((page, mcid)) with
                                | true, content -> content
                                | _ -> () ]
                      let geometry = content |> Seq.choose _.Geometry |> unionGeometry |> Option.toObj
                      let name = match tag.Owner.GetAsName PdfName.S with null -> "a tag" | name -> $"<{name.GetValue()}>"
                      let details =
                          [ $"Tied to {name}, which isn't in the tags."
                            (if tag.Artifact then "Its tag said it was decoration: Rebuild the parent tree marks it as not read."
                             elif tag.Untied then "It draws nothing: Rebuild the parent tree marks it as not read."
                             else "It's kept: its tag needs putting back where the PDF was made, or in a PDF editor.")
                            if context.MarkedContent.Unreadable.Contains page then "The page's text couldn't be read."
                            yield! content |> List.map _.Text |> List.filter (String.IsNullOrWhiteSpace >> not) |> List.distinct
                            for key, label in [ PdfName.Alt, "Alt text"; PdfName.ActualText, "Replacement text" ] do
                                match tag.Owner.GetAsString key with
                                | null -> ()
                                | text -> $"{label}: {text.ToUnicodeString()}" ]
                          |> String.concat "\n"
                      yield node StructureNodeKind.DisconnectedTag "DisconnectedTag" details page geometry tag.Mcids.Length [] ]
            node StructureNodeKind.Disconnected "Disconnected" $"Page {page}: tied to tags that are gone" page null 0 children)
