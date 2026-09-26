namespace Legibyl.Validation

open System

type FindingSeverity =
    | Failure = 0
    | Warning = 1

[<Flags>]
type PdfFixOptions =
    | None = 0
    | Metadata = 1
    | AnnotationTabs = 2
    | RemoveEmptyParagraphs = 4
    | LinkContents = 8
    | DocumentTitle = 16
    /// Tags paragraphs the table of contents lists as headings (H1-H6); see TocHeadings.
    | HeadingsFromToc = 32
    /// Tags runs of paragraphs that start with list markers as lists; see ParagraphLists.
    | ListsFromParagraphs = 64
    /// Like DocumentTitle, but when the document carries no title anywhere, falls back to
    /// the text of a short first page (a title page); see StructureFixes.titlePageText.
    | TitleFromFirstPage = 128
    /// Adds bookmarks for the H1 and H2 headings when there are none; see Bookmarks.
    | BookmarksFromHeadings = 256
    /// Removes table-of-contents entries (TOCI) with nothing in them, like RemoveEmptyParagraphs.
    | RemoveEmptyTocEntries = 512
    /// Tags an untagged document from each page's layout (regions found by a layout detector),
    /// before the other fixes run; see LayoutTagging. Needs an IPageLayoutSource.
    | TagFromLayout = 1024
    /// Declares the document's language (/Lang), detected from its text, when it has none;
    /// see DocumentLanguage.
    | LanguageFromText = 2048
    /// Writes /CIDToGIDMap /Identity on embedded TrueType CID fonts that leave it out (the
    /// mapping readers already use then), which PDF/UA requires.
    | FontCidToGidMap = 4096
    /// With TagFromLayout: adds a link over each QR code that encodes a web address, going
    /// there, tagged as a Link holding the figure.
    | QrCodeLinks = 8192
    /// Describes web links without a description (/Contents), and QR codes that encode web
    /// addresses, by the titles of the pages they go to, fetched from the internet (safely;
    /// see LinkTitles). Needs an IPageTitleSource.
    | LinkTitlesFromWeb = 16384
    | All = 32767

/// The PDF/A or PDF/UA validation flavour to check a document against.
/// Mirrors a subset of org.verapdf.pdfa.flavours.PDFAFlavour.
type PdfProfile =
    /// PDF/UA-1 (ISO 14289-1) — the primary supported profile.
    | PdfUa1 = 0
    /// PDF/A-1b (basic conformance).
    | PdfA1B = 1
    /// PDF/A-2b (basic conformance).
    | PdfA2B = 2
    /// PDF/A-2u (Unicode conformance).
    | PdfA2U = 3
    /// PDF/A-3b (basic conformance, embedded files allowed).
    | PdfA3B = 4
    /// PDF/A-4.
    | PdfA4 = 5

/// Mirrors org.verapdf.pdfa.results.MetadataFixerResult.RepairStatus.
type RepairStatus =
    /// Metadata repair was carried out successfully.
    | Success = 0
    /// Metadata repair was attempted but failed.
    | FixError = 1
    /// The fixer could not determine any action that could repair the document.
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
      /// Child structure elements, in document (reading) order.
      Children: Collections.Generic.IReadOnlyList<StructureNode> }
