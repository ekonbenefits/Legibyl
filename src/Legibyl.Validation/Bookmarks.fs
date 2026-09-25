/// Bookmarks (the document outline) for documents that tag headings but have none. Bookmarks
/// let readers jump between sections from the navigation pane. LegibylChecks warns when they're
/// missing; the BookmarksFromHeadings fix adds one per H1 and H2, H2s nested under the H1 before
/// them. A document that already has any outline is left alone, and so is one that doesn't
/// declare its language: PDF/UA needs the language of bookmark text to be known, and guessing
/// one isn't ours to do.
module internal Legibyl.Validation.Bookmarks

open System
open System.Text.RegularExpressions
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Navigation
open Legibyl.Validation.StructureTreeObjects

/// A heading to bookmark.
type Heading =
    { Level: int
      Title: string
      Page: int
      Geometry: StructureGeometry option }

/// A document with headings and no outline.
type Missing =
    { /// Every heading tag (H, H1-H6) in the document.
      HeadingCount: int
      /// Where the first one is, for the warning.
      FirstHeadingPath: string
      /// The H1 and H2 headings that can be bookmarked (they have text and a page).
      Bookmarkable: Heading list
      /// Whether the document declares its language (/Lang), which bookmark text needs.
      LanguageDeclared: bool }

let private hasOutline (document: PdfDocument) =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.Outlines) with
    | null -> false
    | outlines -> not (isNull (outlines.Get(PdfName.First)))

let private headingLevel (tag: PdfName | null) =
    match tag with
    | null -> None
    | tag ->
        match tag.GetValue() with
        | "H" -> Some 0 // a heading, but its level comes from nesting, not its tag
        | value when Regex.IsMatch(value, "^H[1-6]$") -> Some(int value[1..])
        | _ -> None

/// The document's headings, if it has some and no bookmarks.
let find (document: PdfDocument) context : Missing option =
    match document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.StructTreeRoot) with
    | null -> None
    | _ when hasOutline document -> None
    | structTreeRoot ->
        let headings =
            [ for visit in walk structTreeRoot context do
                  match headingLevel (standardTag visit.Element context) with
                  | Some level -> yield visit, level
                  | None -> () ]
        match headings with
        | [] -> None
        | (first, _) :: _ ->
            Some
                { HeadingCount = headings.Length
                  FirstHeadingPath = first.Path
                  Bookmarkable =
                    [ for visit, level in headings do
                          if level = 1 || level = 2 then
                              let geometry = textGeometry visit.Element context visit.Page
                              let page = visit.Page |> Option.orElse (geometry |> Option.map _.PageNumber)
                              let title = Regex.Replace(extractText visit.Element context visit.Page, @"\s+", " ").Trim()
                              match page with
                              | Some page when title <> "" ->
                                  yield
                                      { Level = level
                                        Title = title
                                        Page = page
                                        Geometry = geometry }
                              | _ -> () ]
                  LanguageDeclared =
                    match document.GetCatalog().GetPdfObject().GetAsString(PdfName.Lang) with
                    | null -> false
                    | language -> not (String.IsNullOrWhiteSpace(language.ToUnicodeString())) }

/// Adds a bookmark per heading, H2s under the H1 before them; returns how many were added.
let add (document: PdfDocument) (headings: Heading list) =
    let root = document.GetOutlines(false)
    let mutable currentH1: PdfOutline option = None
    for heading in headings do
        let parent =
            match heading.Level, currentH1 with
            | 2, Some h1 -> h1
            | _ -> root
        let bookmark = parent.AddOutline heading.Title
        let page = document.GetPage heading.Page
        let destination =
            match heading.Geometry with
            // A little above the heading, so it isn't cut off at the top of the window.
            | Some geometry -> PdfExplicitDestination.CreateXYZ(page, float32 geometry.Left, float32 geometry.Top + 12f, 0f)
            | None -> PdfExplicitDestination.CreateFit page
        bookmark.AddDestination destination
        if heading.Level = 1 then
            currentH1 <- Some bookmark
    headings.Length
