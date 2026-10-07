/// Writes the edits a person made (see ElementEdit) to the elements they were made for: each
/// edit's element is the one with its tag on its page whose box overlaps its box most.
module Legibyl.Validation.ElementEdits

open System
open iText.IO.Font
open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

/// How much an element's box must overlap an edit's to be its element: their intersection over
/// their union, as for a region in Suggest Structure. Fixes and regenerated PDFs move things a
/// little; different elements with the same tag rarely overlap this much.
[<Literal>]
let SameElementOverlap = 0.6

// ---- Suggested regions ----

/// The edit for region (made for the suggestion as found: same page and tag, about the same box).
let private editFor (edits: ElementEdit list) (region: LayoutRegion) =
    edits
    |> List.filter (fun e -> e.MergeWith.IsEmpty && e.Page = region.Geometry.PageNumber && e.Tag = region.Tag)
    |> List.map (fun e -> e, Boxes.overlap e.Box region.Geometry)
    |> List.filter (fun (_, o) -> o >= SameElementOverlap)
    |> List.sortByDescending snd
    |> List.tryHead
    |> Option.map fst

/// Suggested regions with edits layered on: excluded ones left out, retagged ones given their
/// new tag (a list item's numbering goes when it stops being one), alt text for what's then a
/// figure, and corrected text for what OCR read (ActualText; blank for nothing to read).
let onRegions (edits: ElementEdit list) (regions: LayoutRegion seq) : LayoutRegion list =
    [ for region in regions do
          match editFor edits region with
          | Some edit when edit.Exclude -> ()
          | Some edit ->
              let region =
                  match edit.Retag with
                  | NonNull tag -> { region with Tag = tag; ListNumbering = (if tag = "LI" then region.ListNumbering else null) }
                  | Null -> region
              let region =
                  match edit.Alt with
                  | NonNull alt when region.Tag = "Figure" && not (String.IsNullOrWhiteSpace alt) -> { region with Alt = alt }
                  | _ -> region
              match edit.Text with
              | NonNull text when not (isNull region.ActualText) ->
                  yield { region with ActualText = (if String.IsNullOrWhiteSpace text then null else text) }
              | _ -> yield region
          | None -> yield region ]

// ---- Merging paragraphs, and figures ----

/// The elements with tag (P, or Figure) on page, each with its parent, its position in its
/// parent's /K, and its box.
let private elementsOn (structTreeRoot: PdfDictionary) context (page: int) (tag: string) =
    [ for visit in walk structTreeRoot context do
          match resolve (visit.Element.Get(PdfName.K)) with
          | :? PdfArray as kids ->
              for i in 0 .. kids.Size() - 1 do
                  match resolve (kids.Get i) with
                  | :? PdfDictionary as kid when PdfName(tag).Equals(standardTag kid context) ->
                      let elementPage =
                          ancestorPage kid context
                          |> Option.orElseWith (fun () -> subtreeGeometry kid context None |> Option.map _.PageNumber)
                      match elementPage with
                      | Some p when p = page ->
                          match subtreeGeometry kid context (Some p) with
                          | Some box -> yield visit.Element, i, box
                          | None -> ()
                      | _ -> ()
                  | _ -> ()
          | _ -> () ]

/// The paragraphs (or figures) a merge edit is for, as a run to join: each found by its box, all
/// under one parent, one after another. None when they aren't (so, once joined, it's in the PDF).
let internal mergeRun (document: PdfDocument) context (edit: ElementEdit) : ParagraphJoins.JoinRun option =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> None
    | _ when edit.MergeWith.IsEmpty -> None
    | structTreeRoot ->
        let paragraphs = elementsOn structTreeRoot context edit.Page edit.Tag
        let found =
            (edit.Box :: edit.MergeWith)
            |> List.map (fun box ->
                paragraphs
                |> List.map (fun (parent, index, own) -> (parent, index), Boxes.overlap own box)
                |> List.filter (fun (_, o) -> o >= SameElementOverlap)
                |> List.sortByDescending snd
                |> List.tryHead
                |> Option.map fst)
        match found with
        | Some(parent, first) :: _ when found |> List.forall Option.isSome ->
            let found = found |> List.choose id
            let consecutive = found |> List.mapi (fun i (p, index) -> obj.ReferenceEquals(p, parent) && index = first + i) |> List.forall id
            if consecutive then Some { Parent = parent; Indexes = List.map snd found; Page = edit.Page } else None
        | _ -> None

// ---- Alt text and replacement text ----

/// The box of what a merge makes: all its elements' boxes together.
let private mergedBox (merge: ElementEdit) =
    let boxes = merge.Box :: merge.MergeWith
    { merge.Box with
        Left = boxes |> List.map _.Left |> List.min
        Bottom = boxes |> List.map _.Bottom |> List.min
        Right = boxes |> List.map _.Right |> List.max
        Top = boxes |> List.map _.Top |> List.max }

/// Where an edit's element can be: its own box, and, for an edit made on the first of a merge
/// (alt text for figures drawn in pieces, say), the box of what the merge makes of them.
let private boxesOf (merges: ElementEdit list) (edit: ElementEdit) =
    edit.Box
    :: [ for merge in merges do
             if merge.Page = edit.Page && merge.Tag = edit.Tag && Boxes.overlap merge.Box edit.Box >= SameElementOverlap then
                 mergedBox merge ]

/// Each edit with its element, for the edits whose element is found (in any of the places boxes
/// gives for it).
let private findIn (document: PdfDocument) context (boxes: ElementEdit -> StructureGeometry list) (edits: ElementEdit list) : (ElementEdit * PdfDictionary) list =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> []
    | _ when edits.IsEmpty -> []
    | structTreeRoot ->
        // An element retagged already has its new tag.
        let tagsOf (e: ElementEdit) = [ e.Tag; yield! Option.toList (Option.ofObj e.Retag) ]
        let wanted = edits |> List.collect (fun e -> tagsOf e |> List.map (fun tag -> e.Page, tag)) |> set
        // Only elements an edit could be for, with their boxes: by their tag, or (one of the PDF's
        // own names, as Structure… shows it) by their name.
        let candidates =
            [ for visit in walk structTreeRoot context do
                  let names =
                      [ match standardTag visit.Element context with NonNull tag -> tag.GetValue() | Null -> ()
                        match visit.Element.GetAsName PdfName.S with NonNull name -> name.GetValue() | Null -> () ]
                      |> List.distinct
                  // Its page: its own (or an ancestor's), else, for a table (which often has none of
                  // its own), that of what it holds. (Not for other tags: a wrapper without a page
                  // would match as well as the element inside it, and come first.)
                  let page =
                      visit.Page
                      |> Option.orElseWith (fun () ->
                          if List.contains "Table" names && wanted |> Set.exists (fun (_, tag) -> tag = "Table") then
                              subtreeGeometry visit.Element context None |> Option.map _.PageNumber
                          else
                              None)
                  match page with
                  | Some page when names |> List.exists (fun name -> wanted.Contains((page, name))) ->
                      match subtreeGeometry visit.Element context (Some page) with
                      | Some box when box.PageNumber = page -> yield names, box, visit.Element
                      | _ -> ()
                  | _ -> () ]
        edits
        |> List.choose (fun edit ->
            candidates
            |> List.filter (fun (names, box, _) -> names |> List.exists (fun name -> List.contains name (tagsOf edit)) && box.PageNumber = edit.Page)
            |> List.map (fun (_, box, element) -> element, boxes edit |> List.map (Boxes.overlap box) |> List.max)
            |> List.filter (fun (_, o) -> o >= SameElementOverlap)
            |> List.sortByDescending snd
            |> List.tryHead
            |> Option.map (fun (element, _) -> edit, element))

/// Each edit with its element, for the edits whose element is found.
let internal find (document: PdfDocument) context (edits: ElementEdit list) : (ElementEdit * PdfDictionary) list =
    findIn document context (fun edit -> [ edit.Box ]) edits

let private textOf (element: PdfDictionary) (key: PdfName) =
    element.GetAsString key |> Option.ofObj |> Option.map _.ToUnicodeString()

/// Whether an edit is for a link, whose "alt text" is its description: its annotation's
/// /Contents, which is what readers announce, rather than the element's /Alt.
let private isLink (edit: ElementEdit) = edit.Tag = "Link" || edit.Retag = "Link"

/// Whether an edit is for another annotation's element (a comment, a stamp, a highlight), whose
/// "alt text" is its description, its annotation's /Contents, as a link's is.
let private isAnnotation (edit: ElementEdit) = edit.Tag = "Annot" || edit.Retag = "Annot"

/// Whether an edit is for a form field's element, whose "alt text" is its tooltip: what a screen
/// reader says when it reaches the field.
let private isFormField (edit: ElementEdit) = edit.Tag = "Form" || edit.Retag = "Form"

/// The field a widget annotation is for: the widget itself, where it's the field too (it has a
/// name, or no parent), else its parent.
let private fieldOf (widget: PdfDictionary) =
    match widget.GetAsDictionary PdfName.Parent with
    | NonNull parent when isNull (widget.Get PdfName.T) -> parent
    | _ -> widget

/// A field's widgets: itself, where it's one, and its kids that are (a field shown in several
/// places has one each).
let private widgetsOf (field: PdfDictionary) =
    [ if PdfName.Widget.Equals(field.GetAsName PdfName.Subtype) then field
      match field.GetAsArray PdfName.Kids with
      | NonNull kids ->
          for i in 0 .. kids.Size() - 1 do
              match kids.GetAsDictionary i with
              | NonNull kid when PdfName.Widget.Equals(kid.GetAsName PdfName.Subtype) -> kid
              | _ -> ()
      | Null -> () ]

/// What edit changes on element: each dictionary, key and new value, where that differs from
/// what it has.
let private changes (edit: ElementEdit, element: PdfDictionary) =
    [ match edit.Retag with
      | NonNull tag when (element.GetAsName PdfName.S |> Option.ofObj |> Option.map _.GetValue()) <> Some tag -> yield element, PdfName.S, tag
      | _ -> ()
      // What it holds says where its "alt text" goes (whatever its tag is called: one of the
      // PDF's own names mapped to Annot or Form too): a form field's widget, its tooltip; another
      // annotation, its description; else the element's /Alt.
      let annotations = ownAnnotations element
      let widgets = annotations |> List.filter (fun a -> PdfName.Widget.Equals(a.GetAsName PdfName.Subtype))
      let others = annotations |> List.filter (fun a -> not (PdfName.Widget.Equals(a.GetAsName PdfName.Subtype)))
      match edit.Alt with
      | NonNull alt when isLink edit || isAnnotation edit || (not others.IsEmpty && widgets.IsEmpty) ->
          for annotation in others do
              if textOf annotation PdfName.Contents <> Some alt then yield annotation, PdfName.Contents, alt
      | NonNull alt when isFormField edit || not widgets.IsEmpty ->
          // Its field's tooltip (/TU, PDF/UA-1), and every widget of the field's /Contents
          // (PDF/UA-2's description of each); a signature's on its element too (its picture's
          // alt text).
          let fields = widgets |> List.map fieldOf |> List.distinctBy (fun f -> f.GetIndirectReference() |> Option.ofObj |> Option.map (fun r -> r.GetObjNumber()) |> Option.defaultValue (f.GetHashCode()))
          for field in fields do
              if textOf field PdfName.TU <> Some alt then yield field, PdfName.TU, alt
              for widget in List.distinct (widgetsOf field @ (widgets |> List.filter (fun w -> obj.ReferenceEquals(fieldOf w, field)))) do
                  if textOf widget PdfName.Contents <> Some alt then yield widget, PdfName.Contents, alt
          if fields |> List.exists (fun f -> PdfName.Sig.Equals(f.GetAsName PdfName.FT)) && textOf element PdfName.Alt <> Some alt then
              yield element, PdfName.Alt, alt
      | NonNull alt when textOf element PdfName.Alt <> Some alt -> yield element, PdfName.Alt, alt
      | _ -> ()
      match edit.Text with
      | NonNull text when textOf element PdfName.ActualText <> Some text -> yield element, PdfName.ActualText, text
      | _ -> () ]

// ---- Not read: an element made an artifact ----

/// Whether an edit makes its element an artifact: decoration, not read (Not Read in Structure…).
let private isArtifact (edit: ElementEdit) = edit.Retag = "Artifact"

/// An element's marked content, by page: its MCIDs and those of what's inside it (content in a
/// form XObject, by a marked-content reference with /Stm, is left).
let private markedContentOf (element: PdfDictionary) context =
    let found = Collections.Generic.Dictionary<int, Collections.Generic.HashSet<int>>()
    let add page mcid =
        match found.TryGetValue page with
        | true, mcids -> mcids.Add mcid |> ignore
        | _ -> found[page] <- Collections.Generic.HashSet [ mcid ]
    let rec visit (element: PdfDictionary) (page: int option) depth =
        let page = structElemPage element context page
        let kid (kid: PdfObject | null) =
            match resolve kid with
            | :? PdfNumber as mcid -> page |> Option.iter (fun page -> add page (mcid.IntValue()))
            | :? PdfDictionary as child when not (isNull (child.GetAsName PdfName.S)) && depth < 100 -> visit child page (depth + 1)
            | :? PdfDictionary as reference when not (isNull (reference.GetAsNumber PdfName.MCID)) && isNull (reference.Get PdfName.Stm) ->
                structElemPage reference context page |> Option.iter (fun page -> add page (reference.GetAsNumber(PdfName.MCID).IntValue()))
            | _ -> ()
        match resolve (element.Get PdfName.K) with
        | :? PdfArray as kids ->
            for i in 0 .. kids.Size() - 1 do
                kid (kids.Get i)
        | only -> kid only
    visit element (ancestorPage element context) 0
    found

/// Makes elements artifacts: their marked content (/Tag <</MCID n>> BDC) becomes /Artifact BMC,
/// so it's still drawn but not read, and they leave the structure tree. Through iText, which
/// then leaves their content out of the parent tree.
let private makeArtifacts (document: PdfDocument) context (elements: PdfDictionary list) =
    let byPage = Collections.Generic.Dictionary<int, Collections.Generic.HashSet<int>>()
    for element in elements do
        for KeyValue(page, mcids) in markedContentOf element context do
            match byPage.TryGetValue page with
            | true, all -> all.UnionWith mcids
            | _ -> byPage[page] <- Collections.Generic.HashSet mcids
    for KeyValue(pageNumber, mcids) in byPage do
        let page = document.GetPage pageNumber
        let bytes = page.GetContentBytes()
        let resources = page.GetResources()
        let operations, length = ContentOperations.operationsOf bytes resources
        use output = new IO.MemoryStream()
        let mutable from = 0L
        for operation in operations do
            match ContentOperations.mcidOf resources operation with
            | Some mcid when mcids.Contains mcid ->
                output.Write(bytes, int from, int (operation.Start - from))
                let artifact = Text.Encoding.ASCII.GetBytes "\n/Artifact BMC\n"
                output.Write(artifact, 0, artifact.Length)
                from <- operation.End
            | _ -> ()
        output.Write(bytes, int from, int (length - from))
        page.GetPdfObject().Put(PdfName.Contents, PdfStream(output.ToArray()).MakeIndirect document) |> ignore
    let root = document.GetStructTreeRoot()
    for element in elements do
        let structElem = iText.Kernel.Pdf.Tagging.PdfStructElem element
        // Its content first, so iText forgets it; then the element itself.
        for i in (structElem.GetKids() |> Seq.length) - 1 .. -1 .. 0 do
            structElem.RemoveKid i |> ignore
        match resolve (element.Get PdfName.P) with
        | :? PdfDictionary as parent when obj.ReferenceEquals(parent, root.GetPdfObject()) ->
            match resolve (parent.Get PdfName.K) with
            | :? PdfArray as kids ->
                for i in kids.Size() - 1 .. -1 .. 0 do
                    if obj.ReferenceEquals(resolve (kids.Get i), element) then kids.Remove i
            | _ -> parent.Remove PdfName.K |> ignore
        | :? PdfDictionary as parent ->
            let parentElem = iText.Kernel.Pdf.Tagging.PdfStructElem parent
            let index =
                parentElem.GetKids()
                |> Seq.tryFindIndex (fun kid ->
                    match kid with
                    | :? iText.Kernel.Pdf.Tagging.PdfStructElem as kid -> obj.ReferenceEquals(kid.GetPdfObject(), element)
                    | _ -> false)
            index |> Option.iter (fun index -> parentElem.RemoveKid index |> ignore)
        | _ -> ()

/// A PdfString that survives non-ASCII text.
let private textString (value: string) =
    if value |> Seq.forall (fun c -> c < '\u0080') then PdfString value
    else PdfString(value, PdfEncodings.UNICODE_BIG)

// ---- Content outside every tag ----

/// The tag an edit of content outside every tag has (see UntaggedContent): such an edit's
/// retag is what a person chose, Artifact (not read), Figure or P.
[<Literal>]
let UntaggedTag = "Untagged"

let private isUntagged (edit: ElementEdit) = edit.Tag = UntaggedTag

/// Each edit of content outside the tags with its piece, for those whose piece is still there.
let private untaggedPieces (document: PdfDocument) (edits: ElementEdit list) =
    [ for edit in edits do
          match edit.Retag, UntaggedContent.find document edit.Page edit.Box SameElementOverlap with
          | NonNull _, Some piece -> yield edit, piece
          | _ -> () ]

// ---- Moves among siblings ----

/// Each parent's children that moves reorder, where they change its order: the parent's /K, the
/// slots its elements are in (their index and entry), and its entries in their new order. Other
/// kids (marked content, annotations) keep their places.
let private reorders (document: PdfDocument) context (moved: (ElementEdit * PdfDictionary) list) =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> []
    | _ when moved.IsEmpty -> []
    | structTreeRoot ->
        let moveOf = Collections.Generic.Dictionary<PdfDictionary, ElementMove>(HashIdentity.Reference)
        for edit, element in moved do
            edit.Move |> Option.iter (fun move -> moveOf[element] <- move)
        let seen = Collections.Generic.HashSet<PdfDictionary>(HashIdentity.Reference)
        let parents = structTreeRoot :: [ for visit in walk structTreeRoot context -> visit.Element ] |> List.filter seen.Add
        [ for parent in parents do
              match resolve (parent.Get PdfName.K) with
              | :? PdfArray as kids ->
                  let slots =
                      [ for i in 0 .. kids.Size() - 1 do
                            match resolve (kids.Get i) with
                            | :? PdfDictionary as kid when not (isNull (kid.GetAsName PdfName.S)) -> yield i, kids.Get i, kid
                            | _ -> () ]
                  if slots |> List.exists (fun (_, _, kid) -> moveOf.ContainsKey kid) then
                      let ordered =
                          SiblingOrder.reorder
                              [ for _, entry, kid in slots ->
                                    (entry, kid),
                                    subtreeGeometry kid context (ancestorPage kid context),
                                    (match moveOf.TryGetValue kid with
                                     | true, move -> Some move
                                     | _ -> None) ]
                      if ordered |> List.map snd |> List.forall2 (fun (_, _, kid) moved -> obj.ReferenceEquals(kid, moved)) slots |> not then
                          yield kids, slots, ordered
              | _ -> () ]

/// The elements whose place moves change.
let private movedElements (document: PdfDocument) context (moved: (ElementEdit * PdfDictionary) list) =
    let changed = Collections.Generic.HashSet<PdfDictionary>(HashIdentity.Reference)
    for _, slots, ordered in reorders document context moved do
        for (_, _, before), (_, after) in List.zip slots ordered do
            if not (obj.ReferenceEquals(before, after)) then changed.Add after |> ignore
    changed

/// Puts moved elements in their places; returns how many moved.
let private moveElements (document: PdfDocument) context (moved: (ElementEdit * PdfDictionary) list) =
    let mutable count = 0
    for kids, slots, ordered in reorders document context moved do
        for (index, _, before), (entry, after) in List.zip slots ordered do
            if not (obj.ReferenceEquals(before, after)) then
                kids.Set(index, entry) |> ignore
                count <- count + 1
    count

/// Writes the edits that change something, merges first (joining paragraphs moves nothing else);
/// says how many (and how many elements weren't found), or nothing if none changed anything.
/// What table rebuilds change besides the rebuild: each as an edit without its cells, for those
/// that change anything else.
let private otherChanges (tables: ElementEdit list) =
    [ for edit in tables do
          if not (isNull edit.Alt && isNull edit.Text && isNull edit.Retag && edit.Move.IsNone && not edit.Exclude) then
              { edit with Cells = [] } ]

/// A person-confirmed conversion of consecutive paragraph siblings into a table.
let private isParagraphTable (edit: ElementEdit) =
    edit.Tag = "P" && edit.Retag = "Table" && not edit.MergeWith.IsEmpty && not edit.Cells.IsEmpty

/// Whether the review's checked grid is already the table at the merged paragraphs' location.
let private paragraphTableIsBuilt document context (edit: ElementEdit) =
    let tableEdit =
        { edit with
            Tag = "Table"
            Box = mergedBox edit
            MergeWith = []
            Retag = null
            Cells = edit.Cells }
    find document context [ tableEdit ]
    |> List.exists (fun (_, table) -> TableRebuild.rebuilt table context edit.Page edit.Cells)

let internal apply (document: PdfDocument) context (edits: ElementEdit list) : string list =
    let figures, edits = edits |> List.partition (fun e -> e.Tag = FigureRegion.Tag)
    // Original refusals are reported by unapplied. A valid operation can also be refused
    // after another edit changed its content; apply reports that in the context's notes.
    let figures =
        figures |> List.filter (fun e -> FigureRegion.already document context e || (FigureRegion.plan document context e |> Result.isOk))
    // Content outside every tag first: it looks up the elements drawn before it, as they are.
    let outside, edits = edits |> List.partition isUntagged
    // (Text asked to be made decoration isn't: unapplied says why.)
    let outsideWritten, _ =
        UntaggedContent.apply
            document
            context
            [ for edit, piece in untaggedPieces document outside ->
                  piece, (if edit.Retag = "Artifact" then UntaggedContent.NotRead else UntaggedContent.TagAs(edit.Retag |> nonNull, edit.Alt)) ]
    let paragraphTables, edits = edits |> List.partition isParagraphTable
    let converted =
        [ for edit in paragraphTables do
              if not (paragraphTableIsBuilt document context edit) then
                match mergeRun document context edit with
                | Some run ->
                    match ParagraphTableRebuild.convert document context run edit.Cells with
                    | Some count -> yield $"Converted {count} paragraph(s) on page {edit.Page} to a table with the checked rows and cells."
                    | None -> ()
                | None -> () ]
    // Tables rebuilt from their page first: what's in them then moves with them. What else their
    // edits change (a tag, alt text, a move) is changed as any edit's is, after.
    let tables, edits = edits |> List.partition (fun e -> not e.Cells.IsEmpty)
    let edits = edits @ otherChanges tables
    let rebuilt =
        [ for edit, table in find document context tables do
              if TableRebuild.refusal table context edit.Page = None && not (TableRebuild.rebuilt table context edit.Page edit.Cells) then
                  TableRebuild.rebuild document context table edit.Page edit.Cells |> ignore
                  yield () ]
    let merges, edits = edits |> List.partition (fun e -> not e.MergeWith.IsEmpty)
    let runs = merges |> List.choose (mergeRun document context)
    ParagraphJoins.join runs |> ignore
    let found = findIn document context (boxesOf merges) edits
    // Those made artifacts leave the tags; the rest are changed where they are.
    let artifacts, found = found |> List.partition (fst >> isArtifact)
    let seen = Collections.Generic.HashSet<obj>(Collections.Generic.ReferenceEqualityComparer.Instance)
    makeArtifacts document context [ for _, element in artifacts do if seen.Add element then element ]
    let applied =
        [ for pair in found do
              let changes = changes pair
              for dictionary, key, value in changes do
                  // A tag is a name; the rest are text.
                  dictionary.Put(key, (if key.Equals PdfName.S then PdfName value :> PdfObject else textString value)) |> ignore
              if not changes.IsEmpty then yield () ]
    // Last, once nothing else will change what's where: elements moved among their siblings.
    let moved = moveElements document context (found |> List.filter (fun (e, _) -> e.Move.IsSome))
    let count = applied.Length + runs.Length + artifacts.Length + outsideWritten
    let figuresWritten = figures |> List.filter (FigureRegion.apply document context) |> List.length
    // Those that don't apply are said by unapplied, one by one.
    [ if rebuilt.Length > 0 then $"Rebuilt {rebuilt.Length} table(s)' rows and cells from the page, as you checked them."
      if figuresWritten > 0 then $"Replaced structure in {figuresWritten} selected region(s) with a Figure and your alt text, preserving the artwork."
      yield! converted
      if count > 0 then $"Applied {count} of your edits (alt text, replacement text, tags, merges and decoration not read)."
      if moved > 0 then $"Put {moved} element(s) in the reading order you gave them." ]

// ---- The document's title ----

/// The document's title where readers and PDF/UA look for it: the Info dictionary's /Title and
/// the XMP metadata's dc:title (when it has XMP; the metadata fix writes XMP from Info
/// otherwise), shown in place of the file name (/DisplayDocTitle).
let private titleChanges (document: PdfDocument) (title: string) =
    let info = document.GetDocumentInfo().GetTitle() |> Option.ofObj
    let xmp =
        match document.GetXmpMetadataBytes() with
        | null -> None
        | bytes ->
            try Some(iText.Kernel.XMP.XMPMetaFactory.ParseFromBuffer bytes) with _ -> None
    let xmpTitle =
        xmp
        |> Option.bind (fun meta ->
            try meta.GetLocalizedText(iText.Kernel.XMP.XMPConst.NS_DC, "title", "", "x-default") |> Option.ofObj |> Option.map _.GetValue()
            with _ -> None)
    let preferences = document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.ViewerPreferences
    let shown = not (isNull preferences) && PdfBoolean.TRUE.Equals(preferences.Get PdfName.DisplayDocTitle)
    {| Info = info <> Some title
       Xmp = (match xmp with Some _ -> xmpTitle <> Some title | None -> false)
       Shown = not shown
       Meta = xmp |}

/// Whether the title a person gave isn't pdfPath's yet.
let titlePending (pdfPath: string) (title: string) =
    use document = new PdfDocument(new PdfReader(pdfPath))
    let changes = titleChanges document title
    changes.Info || changes.Xmp || changes.Shown

/// Gives the document the title a person gave (Structure…'s Document title, or a review's
/// "title"), shown in place of the file name; says so, or nothing if it has it already.
let internal applyTitle (document: PdfDocument) (title: string) : string list =
    let changes = titleChanges document title
    if changes.Info then document.GetDocumentInfo().SetTitle title |> ignore
    match changes.Meta with
    | Some meta when changes.Xmp ->
        meta.SetLocalizedText(iText.Kernel.XMP.XMPConst.NS_DC, "title", "", "x-default", title)
        document.SetXmpMetadata meta
    | _ -> ()
    if changes.Shown then
        let catalog = document.GetCatalog().GetPdfObject()
        let preferences =
            match catalog.GetAsDictionary PdfName.ViewerPreferences with
            | null ->
                let preferences = PdfDictionary()
                catalog.Put(PdfName.ViewerPreferences, preferences) |> ignore
                preferences
            | preferences -> preferences
        preferences.Put(PdfName.DisplayDocTitle, PdfBoolean.TRUE) |> ignore
    [ if changes.Info || changes.Xmp || changes.Shown then $"Titled the document \"{title}\", as you gave it, shown in place of the file name." ]

/// An edit, as a report names it: its tag, page and box.
let describe (edit: ElementEdit) =
    let n (value: float) = value.ToString("0.#", Globalization.CultureInfo.InvariantCulture)
    let b = edit.Box
    $"<{edit.Tag}> on page {edit.Page} at [{n b.Left}, {n b.Bottom}, {n b.Right}, {n b.Top}]"

/// Why each of edits that won't change pdfPath won't: its element isn't found, it's in the PDF
/// already, or the paragraphs to merge aren't next to each other. Empty when they all apply.
let unapplied (pdfPath: string) (edits: ElementEdit list) : string list =
    if edits.IsEmpty then
        []
    else
        use document = new PdfDocument(new PdfReader(pdfPath))
        let context = createContext document pdfPath
        let figures, edits = edits |> List.partition (fun e -> e.Tag = FigureRegion.Tag)
        let outside, edits = edits |> List.partition isUntagged
        let outsideFound = untaggedPieces document outside
        let paragraphTables, edits = edits |> List.partition isParagraphTable
        let tables, edits = edits |> List.partition (fun e -> not e.Cells.IsEmpty)
        let edits = edits @ otherChanges tables
        let tablesFound = find document context tables
        let merges, edits = edits |> List.partition (fun e -> not e.MergeWith.IsEmpty)
        let found = findIn document context (boxesOf merges) edits
        let moves = found |> List.filter (fun (e, _) -> e.Move.IsSome)
        let moving = movedElements document context moves
        [ for edit in figures do
              if FigureRegion.already document context edit then
                  $"Your Figure from region on page {edit.Page} changed nothing: it is in this PDF already."
              else
                  match FigureRegion.plan document context edit with
                  | Error reason -> $"Your Figure from region on page {edit.Page} wasn't applied: {reason}."
                  | Ok _ -> ()
          for edit in tables do
              match tablesFound |> List.tryFind (fun (e, _) -> obj.ReferenceEquals(e, edit)) with
              | None -> $"Your rebuild of the {describe edit} wasn't applied: no <{edit.Tag}> is in about that place."
              | Some(_, table) ->
                  match TableRebuild.refusal table context edit.Page with
                  | Some why -> $"Your rebuild of the {describe edit} wasn't applied: {why}."
                  | None when TableRebuild.rebuilt table context edit.Page edit.Cells -> $"Your rebuild of the {describe edit} changed nothing: its rows and cells are those already."
                  | None -> ()
          for edit in paragraphTables do
              if paragraphTableIsBuilt document context edit then
                  $"Your conversion of the paragraphs from the {describe edit} changed nothing: they already make this table."
              else
                  match mergeRun document context edit with
                  | None ->
                      $"Your conversion of the paragraphs from the {describe edit} wasn't applied: they aren't consecutive paragraphs under one parent here."
                  | Some run when not (ParagraphTableRebuild.canConvert context run edit.Cells) ->
                      $"Your conversion of the paragraphs from the {describe edit} wasn't applied: their content can't be assigned safely to the checked cells."
                  | Some _ -> ()
          for edit in outside do
              match outsideFound |> List.tryFind (fun (e, _) -> obj.ReferenceEquals(e, edit)) with
              | Some(_, piece) when piece.Kind = UntaggedContent.Text && edit.Retag = "Artifact" ->
                  $"Your edit making text outside the tags on page {edit.Page} decoration wasn't applied: text isn't hidden. Tag it as a paragraph (P) instead."
              | Some _ -> ()
              | None when not (isNull edit.Retag) ->
                  $"Your edit for content outside the tags on page {edit.Page} wasn't applied: nothing outside the tags is in about that place (it may be tagged, or decoration, already)."
              | None -> ()
          for edit in edits do
              match found |> List.tryFind (fun (e, _) -> obj.ReferenceEquals(e, edit)) with
              | None when isArtifact edit ->
                  $"Your edit making the {describe edit} decoration wasn't applied: no <{edit.Tag}> is in about that place (it may be decoration already)."
              | None ->
                  $"Your edit for the {describe edit} wasn't applied: no <{edit.Tag}> is in about that place (its box overlapping at least 60%%). Check its page, tag and box against legibyl structure."
              | Some(_, element) as pair when (changes pair.Value).IsEmpty && not (edit.Move.IsSome && moving.Contains element) ->
                  if edit.Move.IsSome then $"Your move of the {describe edit} changed nothing: it's in that place in the reading order already."
                  else $"Your edit for the {describe edit} changed nothing: the PDF says that already."
              | Some _ -> ()
          for merge in merges do
              if (mergeRun document context merge).IsNone then
                  let what = if merge.Tag = "Figure" then "figures" else "paragraphs"
                  $"Your merge of {merge.MergeWith.Length + 1} {what} from the {describe merge} wasn't applied: they aren't {what} next to each other under one parent here (or they're merged already)." ]

/// How many of edits would change pdfPath: their element is found and doesn't already say it, or
/// the paragraphs to merge are still apart.
let pending (pdfPath: string) (edits: ElementEdit list) : int =
    if edits.IsEmpty then
        0
    else
        use document = new PdfDocument(new PdfReader(pdfPath))
        let context = createContext document pdfPath
        let figures, edits = edits |> List.partition (fun e -> e.Tag = FigureRegion.Tag)
        let outside, edits = edits |> List.partition isUntagged
        let paragraphTables, edits = edits |> List.partition isParagraphTable
        let tables, edits = edits |> List.partition (fun e -> not e.Cells.IsEmpty)
        // A table's other changes are counted as an edit of their own, unless its rebuild is.
        let rebuilding =
            find document context tables
            |> List.filter (fun (edit, table) -> TableRebuild.refusal table context edit.Page = None && not (TableRebuild.rebuilt table context edit.Page edit.Cells))
            |> List.map fst
        let edits =
            edits @ (tables |> List.filter (fun t -> not (rebuilding |> List.exists (fun r -> obj.ReferenceEquals(r, t)))) |> otherChanges)
        let merges, edits = edits |> List.partition (fun e -> not e.MergeWith.IsEmpty)
        let conversions =
            paragraphTables
            |> List.filter (fun edit ->
                not (paragraphTableIsBuilt document context edit)
                && (mergeRun document context edit
                    |> Option.exists (fun run -> ParagraphTableRebuild.canConvert context run edit.Cells)))
        (figures |> List.filter (fun e -> not (FigureRegion.already document context e) && (FigureRegion.plan document context e |> Result.isOk)) |> List.length)
        + rebuilding.Length
        + conversions.Length
        +
        // (Text asked to be made decoration never is.)
        (untaggedPieces document outside |> List.filter (fun (e, piece) -> not (piece.Kind = UntaggedContent.Text && e.Retag = "Artifact")) |> List.length)
        + (merges |> List.choose (mergeRun document context) |> List.length)
        + (let found = findIn document context (boxesOf merges) edits
           let moving = movedElements document context (found |> List.filter (fun (e, _) -> e.Move.IsSome))
           found |> List.filter (fun ((_, element) as pair) -> not (changes pair).IsEmpty || moving.Contains element) |> List.length)

/// A safe, read-only preview of a person-selected Figure region. Refusals explain what
/// needs review; Export and CLI --edits run the same preflight again.
let figureRegionPreview (pdfPath: string) (box: StructureGeometry) : Result<string, string> =
    try
        use document = new PdfDocument(new PdfReader(pdfPath))
        let context = createContext document pdfPath
        let edit: ElementEdit =
            { Page = box.PageNumber; Tag = FigureRegion.Tag; Box = box; Alt = "preview"; Text = null
              MergeWith = []; Retag = "Figure"; Exclude = false; Move = None; Cells = [] }
        FigureRegion.plan document context edit |> Result.map _.Summary
    with ex -> Error $"the PDF couldn't be read: {ex.Message}"
