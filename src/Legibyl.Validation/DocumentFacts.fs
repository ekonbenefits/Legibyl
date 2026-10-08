/// What a person checking a PDF that passes needs to see at a glance (see the Pass finding's
/// checklist in the GUI): its title and language, as written, and how many figures, headings,
/// tables and links its tags have.
module Legibyl.Validation.DocumentFacts

open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

type Facts =
    { /// Its own title (Info, then XMP), if it has one.
      Title: string option
      /// Its /Lang, and the language's name if known ("en", "English").
      Language: (string * string option) option
      Figures: int
      /// Figures with alt text.
      FiguresWithAlt: int
      /// Headings by level, in order: ("H1", 1), ("H2", 4)…
      Headings: (string * int) list
      Tables: int
      Links: int }

/// The facts of the PDF at pdfPath.
let read (pdfPath: string) : Facts =
    use document = new PdfDocument(new PdfReader(pdfPath))
    let title =
        DocumentTitles.candidates pdfPath
        |> List.tryFind (fun c -> c.Source = "document info" || c.Source = "XMP metadata")
        |> Option.map _.Title
    let language =
        match document.GetCatalog().GetPdfObject().GetAsString PdfName.Lang with
        | NonNull lang when not (System.String.IsNullOrWhiteSpace(lang.ToUnicodeString())) ->
            let tag = lang.ToUnicodeString().Trim()
            // "en-US" by its language, English, where the region isn't known.
            let named (tag: string) = Some(DocumentLanguage.nameOf tag) |> Option.filter (fun n -> n <> tag && n <> "")
            Some(tag, named tag |> Option.orElseWith (fun () -> named (tag.Split('-').[0])))
        | _ -> None
    let tags =
        match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
        | null -> []
        | root ->
            let context = createContext document pdfPath
            [ for visit in walk root context do
                  match standardTag visit.Element context with
                  | NonNull tag -> yield tag.GetValue(), visit.Element
                  | Null -> () ]
    let count tag = tags |> List.filter (fun (t, _) -> t = tag) |> List.length
    let figures = tags |> List.filter (fun (t, _) -> t = Tags.Figure)
    { Title = title
      Language = language
      Figures = figures.Length
      FiguresWithAlt = figures |> List.filter (fun (_, e) -> not (isNull (e.Get PdfName.Alt))) |> List.length
      Headings =
        [ for level in Tags.H :: Tags.headings do
              match count level with
              | 0 -> ()
              | n -> yield level, n ]
      Tables = count Tags.Table
      Links = count Tags.Link }
