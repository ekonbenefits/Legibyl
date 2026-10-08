namespace Legibyl.Validation

open System

type FindingSeverity =
    | Failure = 0
    | Warning = 1

/// Legibyl's fixes. Each flag is named as the CLI's --fixes and a review's "fixes" name it, in
/// kebab-case (FontGlyphs is font-glyphs; see FixNames); its doc says what it does. The
/// values are what a saved validation stores, so they never change.
[<Flags>]
type PdfFixOptions =
    | None = 0
    /// Writes the metadata PDF/UA asks for, and its PDF/UA identifier (XMP), with veraPDF's
    /// metadata fixer.
    | Metadata = 1
    /// Makes the Tab key follow the reading order on pages with annotations (/Tabs /S).
    | Tabs = 2
    /// Removes paragraph tags with nothing in them.
    | EmptyParagraphs = 4
    /// Describes table-of-contents links with their own text.
    | LinkContents = 8
    /// Sets a document title the author wrote (in Info or XMP), shown in the title bar.
    | Title = 16
    /// Tags paragraphs the table of contents lists as headings (H1-H6); see TocHeadings.
    | HeadingsFromToc = 32
    /// Tags runs of paragraphs that start with list markers as lists; see ParagraphLists.
    | Lists = 64
    /// Like Title, but when the document carries no title anywhere, falls back to
    /// the text of a short first page (a title page); see StructureFixes.titlePageText.
    | TitleFromFirstPage = 128
    /// Adds bookmarks for the H1 and H2 headings when there are none; see Bookmarks.
    | Bookmarks = 256
    /// Removes table-of-contents entries (TOCI) with nothing in them, like EmptyParagraphs.
    | EmptyTocEntries = 512
    /// Tags an untagged document from each page's layout (regions found by a layout detector),
    /// before the other fixes run; see LayoutTagging. Needs an IPageLayoutSource.
    | TagFromLayout = 1024
    /// Declares the document's language (/Lang), detected from its text, when it has none;
    /// see DocumentLanguage.
    | Language = 2048
    /// Writes /CIDToGIDMap /Identity on embedded TrueType CID fonts that leave it out (the
    /// mapping readers already use then), which PDF/UA requires.
    | FontGlyphs = 4096
    /// With TagFromLayout: adds a link over each QR code that encodes a web address, going
    /// there, tagged as a Link holding the figure.
    | QrLinks = 8192
    /// Describes web links without a description (/Contents), and QR codes that encode web
    /// addresses, by the titles of the pages they go to, fetched from the internet (safely;
    /// see LinkTitles). Needs an IPageTitleSource.
    | LinkTitlesFromWeb = 16384
    /// Joins neighbouring paragraphs whose text a layout detector finds in one block of text
    /// (a paragraph tagged line by line); see ParagraphJoins. Needs an IPageLayoutSource.
    | JoinSplitParagraphs = 32768
    /// Writes the edits a person made (alt text, replacement text; see ElementEdit) to the
    /// elements they were made for. Needs the edits (VeraPdfValidator.Edits).
    | ApplyEdits = 65536
    /// In a tagged PDF 2.0 file, points links, bookmarks and go-to actions within the document at
    /// the elements they land on too (a structure destination, /SD), as PDF/UA-2 asks; see
    /// LinkTargets. Does nothing to a PDF 1.x file.
    | LinkTargets = 131072
    /// Rebuilds the structure tree's parent tree (which content and annotations each tag holds)
    /// where it's missing or inconsistent, and clears obsolete empty/decorative associations
    /// without discarding meaningful content; see ParentTree. Runs first.
    | ParentTree = 262144
    /// Removes figures tagged around nothing (see EmptyFigures): they fail for having no alt text,
    /// with nothing on the page to describe.
    | EmptyFigures = 524288
    /// Makes embedded fonts' dictionaries agree with their font programs and CMaps, without
    /// changing what's drawn: removes a subset font's glyph list (CharSet, CIDSet) where it's
    /// incomplete, sets a composite font's CIDSystemInfo to its CMap's, and an embedded CMap's
    /// /WMode to its program's.
    | FontDictionaries = 1048576
    /// Writes which characters fonts draw (a ToUnicode map), where codes the pages use say none
    /// a reader can map, from the fonts' own glyph names and Unicode tables (see ToUnicodeMaps),
    /// so a screen reader can read their text. Codes nothing says are left out.
    | FontUnicode = 2097152
    /// Gives a tagged table's header cells (TH) without one a Scope, so a screen reader can tell
    /// which cells each heads: Column in a header row, Row for one heading its row.
    | TableHeaderScope = 4194304
    /// Embeds fonts the pages draw with but don't embed, from the open fonts Legibyl brings (URW's
    /// base 35 and Liberation), where one has every glyph the pages draw, each as wide as the PDF
    /// says (see FontEmbedding): Helvetica, Times, Courier, Symbol and ZapfDingbats, and Arial,
    /// Times New Roman and Courier New. Another font is embedded from the same font installed on
    /// this computer, if its licence lets it be. Others are left, and reported.
    | EmbedFonts = 8388608
    /// Maps the PDF's own tag names to standard tags in its role map, by what they say (Heading 1
    /// is H1, Body Text P, Image Figure; see RoleMaps), and repairs a broken map: a standard tag
    /// mapped, or a circle. A name that doesn't say is left, and reported.
    | RoleMap = 16777216
    /// Gives a text block replacement text assembled from its Span children when their boundary
    /// splits a word; only applies where every span's text can be recovered exactly.
    | JoinSplitWords = 33554432
    /// Puts the tags at the top of the structure under a single Document tag, one element that's the
    /// whole document for a screen reader to start from (as PDF 2.0 asks for); nothing else moves.
    /// See DocumentRoot.
    | DocumentRoot = 67108864
    /// Links web addresses a page shows as text that nothing links: a link annotation over each,
    /// going to it, and a Link tag holding its text and the annotation (see UnlinkedUrls). Only
    /// an address that's all of its tagged text; others are left, and reported.
    | LinkUrls = 134217728
    | All = 268435455

/// The PDF/UA standard to check a document against. Which applies comes from the file's PDF
/// version (see PdfUaTarget): PDF/UA-1 is defined on PDF 1.7, PDF/UA-2 on PDF 2.0.
type PdfProfile =
    /// PDF/UA-1 (ISO 14289-1), for PDF 1.x.
    | PdfUa1 = 0
    /// PDF/UA-2 (ISO 14289-2), for PDF 2.0.
    | PdfUa2 = 1

/// Mirrors org.verapdf.pdfa.results.MetadataFixerResult.RepairStatus, and keeps veraPDF's name for it
/// (elsewhere Legibyl says fix, not repair).
type RepairStatus =
    /// Metadata fix was carried out successfully.
    | Success = 0
    /// Metadata fix was attempted but failed.
    | FixError = 1
    /// The fixer could not determine any action that could fix the document.
    | WontFix = 2
    /// No action was taken because the file was already valid.
    | NoAction = 3
    /// A false PDF/A or PDF/UA conformance claim was removed.
    | IdRemoved = 4

/// Thrown when the underlying veraPDF (Java, via IKVM) validation engine
/// fails to open or validate a PDF document. Wraps the original Java
/// exception's message so callers never need to reference IKVM/Java types.
type VeraPdfValidationException =
    inherit Exception
    new(message: string) = { inherit Exception(message) }
    new(message: string, innerException: exn) = { inherit Exception(message, innerException) }

/// Best-effort page geometry for a semantic structure node, in PDF user-space
/// coordinates (origin at the bottom-left of the page).
type StructureGeometry =
    { PageNumber: int
      Left: float
      Bottom: float
      Right: float
      Top: float }

/// Comparing boxes on a page, as an edit finds what it was made for again.
module Boxes =
    /// The box at least 2 pt each way: a hairline's (a rule tagged as a figure, say) has no
    /// width or no height, so would overlap nothing, not even itself.
    let visible (box: StructureGeometry) =
        let grow (size: float) = max 0.0 (2.0 - size) / 2.0
        let dx, dy = grow (box.Right - box.Left), grow (box.Top - box.Bottom)
        { box with Left = box.Left - dx; Right = box.Right + dx; Bottom = box.Bottom - dy; Top = box.Top + dy }

    /// The intersection over union of two boxes, each at least 2 pt each way.
    let overlap (a: StructureGeometry) (b: StructureGeometry) =
        let a, b = visible a, visible b
        let area (box: StructureGeometry) = max 0.0 (box.Right - box.Left) * max 0.0 (box.Top - box.Bottom)
        let intersection = max 0.0 (min a.Right b.Right - max a.Left b.Left) * max 0.0 (min a.Top b.Top - max a.Bottom b.Bottom)
        let union = area a + area b - intersection
        if union <= 0.0 then 0.0 else intersection / union

/// A change a person made to a structure element (in Structure…) or to a suggested region
/// (in Suggest Structure, or a layout's edits): its alt text, its replacement text, its tag,
/// paragraphs merged into it, or (for a region) leaving it out. What it changes is left as it
/// was found; the edit is layered on top. The element is found again by its page, its tag and about the same box,
/// so the edit still applies after fixes have rearranged the structure tree.
/// Where an element goes among its siblings (Structure's Move Up and Move Down): what it now
/// follows.
type ElementMove =
    /// First among them.
    | First
    /// After the sibling in about this place.
    | After of StructureGeometry

/// A table's cell as a rebuild gives it: a header cell (TH) or a data cell (TD), where it is.
type TableCell = { Tag: string; Box: StructureGeometry }

type ElementEdit =
    { Page: int
      /// Its standard tag, e.g. "Figure". "FigureRegion" reserves Box as a selected
      /// rectangle: replace complete content within it with one Figure, requiring Alt.
      Tag: string
      Box: StructureGeometry
      /// Alt text to write (/Alt); null to leave it.
      Alt: string | null
      /// Replacement text to write (/ActualText); "" for nothing to read, null to leave it.
      Text: string | null
      /// Paragraphs to merge into this one (a P), by box: its neighbours after it, in order.
      /// Empty for none.
      MergeWith: StructureGeometry list
      /// The tag to give it instead (e.g. "H1" for a heading found as "H"); null to leave it.
      Retag: string | null
      /// For a suggested region: leave it out of the structure (its text joins the nearest region
      /// kept; anything else it holds is decoration).
      Exclude: bool
      /// Where it goes among its siblings, if it's moved.
      Move: ElementMove option
      /// For a table: its rows and cells rebuilt as these cells (from the page, see TableRebuild);
      /// empty to leave it.
      Cells: TableCell list }

/// The order of an element's children once some are moved (see ElementMove): those moved are
/// taken out and put where their moves say, the rest keeping the order they were read in. The
/// Structure window shows this order, and the ApplyEdits fix writes it.
module SiblingOrder =
    /// siblings, as read, each with its box (None for one with no place on a page) and its move,
    /// if it's moved: in the order the moves make. A move whose sibling isn't found leaves that
    /// one where it was; several put after one sibling (or first) go in the order they were read.
    let reorder (siblings: ('T * StructureGeometry option * ElementMove option) list) : 'T list =
        let items = Array.ofList siblings
        let n = items.Length
        let boxOf i = let _, box, _ = items[i] in box
        // Where each moved one goes: Some -1 for first, Some j for after sibling j; None to stay.
        let target =
            Array.init n (fun i ->
                let _, _, move = items[i]
                match move with
                | Some First -> Some -1
                | Some(After place) ->
                    [ for j in 0 .. n - 1 do
                          match boxOf j with
                          | Some box when j <> i && box.PageNumber = place.PageNumber -> yield j, Boxes.overlap box place
                          | _ -> () ]
                    |> List.filter (fun (_, o) -> o >= 0.6)
                    |> List.sortByDescending snd
                    |> List.tryHead
                    |> Option.map fst
                | None -> None)
        let order = ResizeArray<int>([ for i in 0 .. n - 1 do if target[i].IsNone then i ])
        // After each place, how many moved ones are in already, so the next goes after them.
        let placedAfter = Collections.Generic.Dictionary<int, int>()
        let pending = ResizeArray<int>([ for i in 0 .. n - 1 do if target[i].IsSome then i ])
        let mutable progress = true
        while pending.Count > 0 && progress do
            progress <- false
            for i in List.ofSeq pending do
                let place = target[i].Value
                if place = -1 || order.Contains place then
                    let already = match placedAfter.TryGetValue place with | true, count -> count | _ -> 0
                    order.Insert((if place = -1 then 0 else order.IndexOf place + 1) + already, i)
                    placedAfter[place] <- already + 1
                    pending.Remove i |> ignore
                    progress <- true
        // Moves after each other in a circle: where they were, at the end.
        order.AddRange pending
        [ for i in order -> let item, _, _ = items[i] in item ]

/// A region of a page to tag, e.g. one a layout detector found (see Legibyl.Layout).
type LayoutRegion =
    { /// The tag it suggests: "H1", "H", "P", "LI", "Figure", "Table", "Caption", "Note",
      /// "Formula", "Lbl", or "Artifact" for page furniture like running headers.
      Tag: string
      Geometry: StructureGeometry
      /// For an "LI", its list's /ListNumbering (e.g. "Disc"); consecutive items form one list.
      ListNumbering: string | null
      /// The region's text when it's drawn as outlines or pixels rather than as text (read by
      /// OCR, in English): written as the element's /ActualText (with /Lang en), with the
      /// drawing as its content.
      ActualText: string | null
      /// For a "Figure" that's a QR code, what it encodes: its alt text says so, and the
      /// QrCodeLinks fix links it when it's a web address.
      QrText: string | null
      /// For a "Figure": alt text a person wrote for it (in Suggest Structure). Written as its
      /// /Alt, ahead of any Legibyl would derive.
      Alt: string | null }

/// Where the LinkTitlesFromWeb fix gets the titles of the web pages links go to.
type IPageTitleSource =
    /// The titles of the pages at addresses (absolute http(s) addresses), keyed by address.
    /// An address whose title couldn't be read is left out.
    abstract PageTitles: addresses: string seq -> Collections.Generic.IReadOnlyDictionary<string, string>

/// A region tagging from the layout turned into an element: the tag it wrote, and the box of
/// what the element holds (null if it holds nothing drawn). An edit made for the region belongs
/// to that element in the tagged copy.
type TaggedRegion =
    { Region: LayoutRegion
      Tag: string
      Box: StructureGeometry | null }

/// Where the TagFromLayout fix gets each page's regions.
type IPageLayoutSource =
    /// pageNumber's regions (1-based), in reading order.
    abstract PageRegions: pdfPath: string * pageNumber: int -> Collections.Generic.IReadOnlyList<LayoutRegion>

    /// A description for a link annotation that has none (/Contents), e.g. written or checked in
    /// Suggest Structure: for the link at rectangle on pageNumber. Null to leave it without one.
    abstract LinkDescription: pdfPath: string * pageNumber: int * rectangle: StructureGeometry -> string | null

    /// Titles already read for web pages (e.g. in Suggest Structure), by address, for QR codes'
    /// descriptions; null for none.
    abstract PageTitles: Collections.Generic.IReadOnlyDictionary<string, string> | null

    /// A title to give the document (e.g. written or checked in Suggest Structure); null to
    /// leave its title as it is.
    abstract DocumentTitle: string | null

/// A human-friendly interpretation of a veraPDF <c>locationContext</c> string
/// (e.g. <c>root/document[0]/pages[1](7 0 obj PDPage)/annots[0](9 0 obj PDLinkAnnot)</c>),
/// which otherwise requires knowledge of veraPDF's internal object model to read.
type PdfLocation =
    { /// 1-based page number, if the location is anchored to a page.
      PageNumber: Nullable<int>
      /// The raw veraPDF/PDF object type of the deepest object on the path (e.g. "PDLinkAnnot").
      ObjectType: string | null
      /// The PDF indirect object number of the deepest object on the path, if any.
      ObjectNumber: Nullable<int>
      /// If the deepest object is an annotation, its 0-based position within the page's /Annots array
      /// (i.e. the index from an "annots[N]" path segment). Used to look up its /Rect for region highlighting.
      AnnotationIndex: Nullable<int>
      /// A readable "Document → Page 2 → Link annotation" style path.
      Breadcrumb: string }

    override this.ToString() =
        if this.PageNumber.HasValue then $"Page {this.PageNumber.Value} — {this.Breadcrumb}"
        else this.Breadcrumb

/// Where an internal link sends the reader: a page, and the part of it its destination names.
/// Which coordinates are set depends on the kind: /XYZ gives the view's left and top edges,
/// /FitH its top, /FitV its left, /FitR a whole rectangle, and /Fit none. A null coordinate is
/// left to the reader (e.g. /XYZ null 700 null keeps the current left edge).
type LinkDestination =
    { PageNumber: int
      /// The destination's kind: "XYZ", "Fit", "FitH", "FitV", "FitR", "FitB", "FitBH" or "FitBV".
      Kind: string
      Left: Nullable<float>
      Top: Nullable<float>
      Right: Nullable<float>
      Bottom: Nullable<float> }

/// One node of a tagged PDF's logical structure tree (the Catalog's StructTreeRoot) —
/// the same semantic hierarchy (headings, paragraphs, lists, tables, links, etc.) and
/// reading order that assistive technology (screen readers) uses to present the
/// document to a user. Built by walking veraPDF's own PD model, so it reflects
/// exactly what the validator itself sees.
type StructureNode =
    { /// The structure element's type, resolved to its standard/base tag name where possible
      /// (e.g. "H1", "P", "L", "Table", "TH"). This is the value that determines the
      /// element's semantic role to a screen reader.
      Tag: string
      /// If this element uses a custom (non-standard) tag that is role-mapped to a
      /// different standard tag within the same namespace, the mapped-to tag name;
      /// otherwise null (no distinct mapping worth reporting).
      Role: string | null
      /// The element's Alt text (e.g. image/figure/link alt text), if any.
      AlternateDescription: string | null
      /// The element's ActualText (a plain-text substitute for its content), if any.
      ActualText: string | null
      /// The actual visible text drawn under this element's own directly-owned marked
      /// content (extracted from the page's text glyphs, matched to this element by
      /// page + MCID) — the "what does a screen reader actually read here" text. Null if
      /// this element owns no text-bearing marked content directly (e.g. it's a purely
      /// structural container whose text lives entirely in its children, or it owns only
      /// non-text content like a raw image). Does not include descendant elements' text,
      /// to avoid duplicating text that already appears under a child node.
      Content: string | null
      /// For /Form structure elements, the referenced widget annotation's field name (/T), if any.
      FormFieldName: string | null
      /// For /Form structure elements, the referenced widget annotation's tooltip/user description (/TU), if any.
      FormFieldDescription: string | null
      /// For /Form structure elements, the referenced widget annotation's field type (/FT), e.g. "Tx" or "Btn".
      FormFieldType: string | null
      /// For /Form structure elements, the referenced widget annotation's current value (/V), if any.
      FormFieldValue: string | null
      /// For /Link structure elements, the external URI target, if any.
      LinkUri: string | null
      /// For /Link structure elements, the target page number for an internal destination, if resolvable.
      LinkDestinationPageNumber: Nullable<int>
      /// For /Link structure elements, a human-readable internal destination summary, if any.
      LinkDestinationDescription: string | null
      /// For /Link structure elements, where an internal destination goes on its page, if resolvable.
      LinkDestination: LinkDestination | null
      /// For /Link structure elements, the link's description (its annotation's /Contents), if any:
      /// what a screen reader announces for it. The same for another annotation's element (/Annot:
      /// a comment, a stamp, a highlight…).
      LinkDescription: string | null
      /// Best-effort same-page geometry covered by this node's directly-owned content and
      /// child nodes, if determinable. Null when the node has no geometric content or
      /// spans multiple pages.
      Geometry: StructureGeometry | null
      /// The element's Lang tag (e.g. "en-US"), if explicitly set on this element.
      Language: string | null
      /// The 1-based page this element's content appears on, if determinable. Null for
      /// purely structural container elements that don't directly own any page content.
      PageNumber: Nullable<int>
      /// Number of marked-content/annotation references directly under this element (i.e.
      /// actual tagged page content it owns), not counting nested structure elements.
      ContentLeafCount: int
      /// For a table cell (TH, TD), how many columns and rows it spans (its /ColSpan and
      /// /RowSpan attributes); 1 otherwise, or when it doesn't say.
      ColSpan: int
      RowSpan: int
      /// Child structure elements, in document (reading) order.
      Children: Collections.Generic.IReadOnlyList<StructureNode> }

    /// Where it is, as an edit names it (with its Tag): the page its geometry is on...
    member this.Page =
        match this.Geometry with
        | NonNull geometry -> Nullable geometry.PageNumber
        | Null -> Nullable()

    /// ...and that geometry as [left, bottom, right, top], in points, as layouts and edits have it.
    member this.Box: float array | null =
        match this.Geometry with
        | NonNull g -> [| g.Left; g.Bottom; g.Right; g.Top |] |> Array.map (fun v -> Math.Round(v, 1))
        | Null -> null

    /// The element as it's said (a screen reader names a tree item by its item's text): its tag,
    /// the start of what it says, and its page.
    override this.ToString() =
        let said =
            [ this.Content; this.ActualText; this.AlternateDescription; this.LinkDescription ]
            |> List.tryPick (fun text -> match text with NonNull text when not (String.IsNullOrWhiteSpace text) -> Some(text.Trim()) | _ -> None)
            |> Option.map (fun text -> if text.Length > 80 then " \"" + text.Substring(0, 80) + "…\"" else " \"" + text + "\"")
            |> Option.defaultValue ""
        let page = if this.PageNumber.HasValue then $", page {this.PageNumber.Value}" else ""
        $"<{this.Tag}>{said}{page}"
