/// Web addresses a page shows as text that nothing links (Structure-18), and linking them
/// (LinkUrls). PDF/UA doesn't ask for it, but someone using a screen reader or a keyboard can't
/// follow an address that isn't a link.
///
/// The text index finds the addresses each page's text shows (see WebAddressText); here, those
/// that are web addresses (LinkTitles.webAddress) and that nothing links: text more than half
/// under a link annotation is linked already.
module internal Legibyl.Validation.UnlinkedUrls

open System
open System.Collections.Generic
open iText.IO.Font
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Annot
open iText.Kernel.Pdf.Tagging
open Legibyl.Validation.StructureTreeObjects

/// How the fix gets an address's text on its own, to tag it as a link.
type Cut =
    /// It's all of its marked content already.
    | Whole
    /// Punctuation drawn at the end of its last piece, after it ("." in "…/dol-diversification."),
    /// is split off into a piece of its own first, read after the link.
    | SplitOff of string
    /// It's drawn in the middle of one piece, with other text: the text before it stays in the
    /// piece, and it and the text after it are carved out into pieces of their own.
    | Around

/// An address on a page that nothing links.
type Candidate =
    { Page: int
      /// As the page shows it.
      Text: string
      /// Where it goes: Text, with "www." taken to be https.
      Address: string
      Geometry: StructureGeometry
      /// Where it is, as its warning says (and a dismissal names it by): the path of the element
      /// its text is in, then the address; or its page, then the address.
      Location: string
      /// The marked content (MCIDs, in reading order) that's all of it, and the element they
      /// belong to, side by side among its kids, when the fix can tag it as a link: then they move
      /// under a new Link element.
      Run: (int list * PdfDictionary) option
      /// How the fix gets its text on its own, when it can tag it.
      Cut: Cut
      /// Why the fix leaves it, when it does.
      Left: string option }

/// The page's link annotations' boxes.
let private linkBoxes (page: PdfPage) =
    [ for annotation in page.GetAnnotations() do
          if PdfName.Link.Equals(annotation.GetSubtype()) then
              match annotation.GetRectangle() with
              | null -> ()
              | rectangle ->
                  let box = rectangle.ToRectangle()
                  yield float (box.GetLeft()), float (box.GetBottom()), float (box.GetRight()), float (box.GetTop()) ]

/// Whether more than half of the smaller of the two boxes is in both.
let private mostlyUnder (geometry: StructureGeometry) (left, bottom, right, top) =
    let width = min geometry.Right right - max geometry.Left left
    let height = min geometry.Top top - max geometry.Bottom bottom
    let area (w: float) (h: float) = max 0.0 w * max 0.0 h
    let smaller = min (area (geometry.Right - geometry.Left) (geometry.Top - geometry.Bottom)) (area (right - left) (top - bottom))
    smaller > 0.0 && width > 0.0 && height > 0.0 && width * height / smaller > 0.5

/// The owner's kid that is mcid on page: a bare MCID (on the owner's page, its own or an
/// ancestor's), or an MCR to that page (not to a Form XObject's stream). MCIDs are numbered page
/// by page, so the number alone could be another page's. By index in its /K.
let private kidIndex (owner: PdfDictionary) (mcid: int) (page: int) context =
    let kids =
        match owner.Get PdfName.K with
        | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 -> array.Get i ]
        | NonNull kid -> [ kid ]
        | Null -> []
    let ownerPage = ancestorPage owner context
    kids
    |> List.tryFindIndex (fun kid ->
        match kid with
        | :? PdfNumber as number -> number.IntValue() = mcid && ownerPage = Some page
        | :? PdfDictionary as reference when isNull (reference.GetAsName PdfName.S) && isNull (reference.Get PdfName.Stm) ->
            match reference.GetAsNumber PdfName.MCID with
            | NonNull number -> number.IntValue() = mcid && structElemPage reference context ownerPage = Some page
            | Null -> false
        | _ -> false)

/// Whether the element or one above it is a link (by its standard tag).
let private inLink (element: PdfDictionary) context =
    let rec climb (element: PdfDictionary | null) depth =
        match element with
        | NonNull element when depth < 200 && not (isNull (element.GetAsName PdfName.S)) ->
            PdfName.Link.Equals(standardTag element context) || climb (element.GetAsDictionary PdfName.P) (depth + 1)
        | _ -> false
    climb element 0

/// How the punctuation at the end of a page's piece of marked content would be split off (see
/// MarkedContentSplit), numbered past the MCIDs the tags give the page.
let private splitPlan (document: PdfDocument) (context: Context) (page: int) (mcid: int) (tail: string) =
    let used = (ParentTree.tagged document context).MarkedContent.Keys |> Seq.filter (fun (p, _) -> p = page) |> Seq.map snd
    MarkedContentSplit.plan (document.GetPage page) mcid tail used

/// How an address drawn in the middle of a page's piece of marked content would be carved out
/// (see MarkedContentSplit), numbered past the MCIDs the tags give the page.
let private carving (document: PdfDocument) (context: Context) (page: int) (mcid: int) (text: string) =
    let used = (ParentTree.tagged document context).MarkedContent.Keys |> Seq.filter (fun (p, _) -> p = page) |> Seq.map snd
    MarkedContentSplit.around (document.GetPage page) mcid text used

/// Why an address drawn with other text in one piece isn't carved out.
let private notCarved why = $"it's part of a longer piece of text, which can't be cut around it: {why}"

/// The addresses pages show as text that nothing links, in page order.
let find (document: PdfDocument) (context: Context) : Candidate list =
    let tagged = lazy (ParentTree.tagged document context)
    let splitPlan = splitPlan document context
    let carving = carving document context
    // Each element's path, as Legibyl's warnings give it ("StructTreeRoot/K[0]/K[3]").
    let paths =
        lazy
            (let paths = Dictionary<PdfDictionary, string>(HashIdentity.Reference)
             match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
             | null -> ()
             | root ->
                 for visit in walk root context do
                     paths.TryAdd(visit.Element, visit.Path) |> ignore
             paths)
    let isTagged = not (isNull (document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot))
    [ for KeyValue(pageNumber, found) in context.MarkedContent.Addresses |> Seq.sortBy _.Key do
          if pageNumber <= document.GetNumberOfPages() then
              let links = lazy (linkBoxes (document.GetPage pageNumber))
              for address in found do
                  match LinkTitles.webAddress address.Text with
                  | Some target ->
                      let geometry =
                          { PageNumber = pageNumber
                            Left = address.Left
                            Bottom = address.Bottom
                            Right = address.Right
                            Top = address.Top }
                      if not (links.Value |> List.exists (mostlyUnder geometry)) then
                          let owners =
                              [ for mcid in address.Mcids ->
                                    match tagged.Value.MarkedContent.TryGetValue((pageNumber, mcid)) with
                                    | true, owner when mcid >= 0 -> Some owner
                                    | _ -> None ]
                          // The one element all its pieces belong to, if they're all tagged.
                          let owner =
                              match owners with
                              | Some first :: rest when rest |> List.forall (function Some other -> obj.ReferenceEquals(other, first) | None -> false) ->
                                  Some(address.Mcids, first)
                              | _ -> None
                          // Its pieces' places among the owner's kids, when each is there.
                          let indexes (mcids: int list, owner) = mcids |> List.map (fun mcid -> kidIndex owner mcid pageNumber context)
                          let sideBySide (indexes: int option list) =
                              indexes |> List.pairwise |> List.forall (function Some a, Some b -> b = a + 1 | _ -> false)
                          let refused why = None, Whole, Some why
                          let run, cut, left =
                              match owner with
                              | _ when not isTagged -> refused "the PDF isn't tagged: tagging it comes first"
                              | None when owners |> List.exists Option.isNone -> refused "its text isn't in the tags"
                              | None ->
                                  refused "it's drawn in more than one piece, in different tags, which would have to be joined to tag it as a link"
                              | Some _ when address.Interleaved ->
                                  refused "a piece of it is read on both sides of another, so its pieces can't be tagged in the address's order"
                              | Some(mcids, _) when not address.WholeRun && address.Tail.IsNone && mcids.Length > 1 ->
                                  refused "it's part of a longer piece of text, which would have to be split to tag the address alone"
                              | Some(_, owner) when inLink owner context ->
                                  refused "it's tagged as a link already, with no link annotation to follow"
                              | Some run when indexes run |> List.exists Option.isNone ->
                                  refused "its tag holds it in a way the fix doesn't change"
                              | Some run when not (sideBySide (indexes run)) ->
                                  refused "its pieces aren't side by side in its tag, in the order they're read"
                              | Some(mcids, _ as run) ->
                                  match address.Tail with
                                  | Some tail ->
                                      match splitPlan pageNumber (List.last mcids) tail with
                                      | Ok _ -> Some run, SplitOff tail, None
                                      | Error why -> refused $"the \"{tail}\" after it is drawn in the same piece, and can't be split off: {why}"
                                  | None when address.WholeRun -> Some run, Whole, None
                                  | None ->
                                      match carving pageNumber (List.head mcids) address.Text with
                                      | Ok _ -> Some run, Around, None
                                      | Error why -> refused (notCarved why)
                          let location =
                              match owner |> Option.bind (fun (_, owner) -> match paths.Value.TryGetValue owner with | true, path -> Some path | _ -> None) with
                              | Some path -> $"{path} + {address.Text}"
                              | None -> $"Page {pageNumber}: {address.Text}"
                          yield
                              { Page = pageNumber
                                Text = address.Text
                                Address = target
                                Geometry = geometry
                                Location = location
                                Run = run
                                Cut = cut
                                Left = left }
                  | None -> () ]

/// Links each address the fix can: a borderless link annotation over it, going to it, described
/// by it, and a Link element in its tag's place holding its text and the annotation. Those it
/// can't are said, and why.
let apply (document: PdfDocument) (context: Context) =
    let candidates = find document context
    let mutable linked = 0
    // Where the rest of a piece an address was carved out of is now: by its page and MCID.
    let carvedRest = Dictionary<int * int, int>()
    for candidate in candidates do
        match candidate.Run, candidate.Left with
        | Some(found, owner), _ ->
            let page = document.GetPage candidate.Page
            let element = PdfStructElem owner
            // An address carved out of a piece earlier on leaves the rest of the piece in one of
            // its own: another address in it is there now.
            let mcids =
                match candidate.Cut, found with
                | Around, [ mcid ] ->
                    match carvedRest.TryGetValue((candidate.Page, mcid)) with
                    | true, rest -> [ rest ]
                    | _ -> found
                | _ -> found
            match mcids |> List.map (fun mcid -> kidIndex owner mcid candidate.Page context) with
            | Some index :: _ as indexes when indexes |> List.forall Option.isSome ->
              // Its text on its own first: the punctuation after it split off into a piece of its
              // own, or it carved out of its piece. What's kept before it (the piece's own MCID),
              // what the link holds, and the piece after it.
              let cut =
                  match candidate.Cut with
                  | SplitOff tail ->
                      match splitPlan document context candidate.Page (List.last mcids) tail with
                      | Ok plan -> Ok(false, mcids, Some(MarkedContentSplit.split document page plan))
                      | Error why -> Error $"the \"{tail}\" after it is drawn in the same piece, and can't be split off: {why}"
                  | Around ->
                      match carving document context candidate.Page (List.head mcids) candidate.Text with
                      | Ok plan ->
                          let pieces = MarkedContentSplit.carve document page plan
                          match pieces.After with
                          | Some rest -> carvedRest[(candidate.Page, List.head found)] <- rest
                          | None -> ()
                          Ok(pieces.Before, [ pieces.Address ], pieces.After)
                      | Error why -> Error(notCarved why)
                  | Whole -> Ok(false, mcids, None)
              match cut with
              | Error why -> context.Notes.Add $"Page {candidate.Page}: \"{candidate.Text}\" isn't linked: {why}."
              | Ok(before, linkMcids, after) ->
                // iText unties the content by its owner's own page; one it takes from an ancestor
                // (the address's page, as kidIndex found) is written on it first.
                if isNull (owner.GetAsDictionary PdfName.Pg) then owner.Put(PdfName.Pg, page.GetPdfObject()) |> ignore
                // The pieces are side by side (see find): taken out from the last, so the others
                // keep their places. (Text kept before it stays.)
                if not before then
                    for i in List.rev (List.choose id indexes) do
                        element.RemoveKid i |> ignore
                let index = if before then index + 1 else index
                let link = PdfStructElem(document, PdfName.Link, page)
                element.AddKid(index, link) |> ignore
                for mcid in linkMcids do
                    link.AddKid(PdfMcrNumber(PdfNumber mcid, link)) |> ignore
                let g = candidate.Geometry
                let annotation = PdfLinkAnnotation(iText.Kernel.Geom.Rectangle(float32 g.Left, float32 g.Bottom, float32 (g.Right - g.Left), float32 (g.Top - g.Bottom)))
                annotation.SetAction(iText.Kernel.Pdf.Action.PdfAction.CreateURI candidate.Address) |> ignore
                // No border, so the page looks the same.
                annotation.SetBorder(PdfArray([| 0.0; 0.0; 0.0 |])) |> ignore
                annotation.SetFlags PdfAnnotation.PRINT |> ignore
                annotation.SetContents(PdfString(candidate.Text, PdfEncodings.UNICODE_BIG)) |> ignore
                // Not tagged by iText, which would give it a Link of its own at the top: it's this one's.
                page.AddAnnotation(-1, annotation, false) |> ignore
                link.AddKid(PdfObjRef(annotation, link, document.GetNextStructParentIndex())) |> ignore
                // The text after it (the punctuation split off, the rest of its piece), read after
                // the link, in its tag as it was.
                after |> Option.iter (fun id -> element.AddKid(index + 1, PdfMcrNumber(PdfNumber id, element)) |> ignore)
                // Its annotations in the order of the tags, as PDF/UA asks of a page with them.
                page.GetPdfObject().Put(PdfName.Tabs, PdfName.S) |> ignore
                linked <- linked + 1
            | _ -> context.Notes.Add $"Page {candidate.Page}: \"{candidate.Text}\" isn't linked: its tag holds it in a way the fix doesn't change."
        | None, Some why -> context.Notes.Add $"Page {candidate.Page}: \"{candidate.Text}\" isn't linked: {why}."
        | None, None -> ()
    match linked with
    | 0 -> []
    | 1 -> [ "Linked 1 web address shown as text, to where it goes." ]
    | n -> [ $"Linked {n} web addresses shown as text, each to where it goes." ]
