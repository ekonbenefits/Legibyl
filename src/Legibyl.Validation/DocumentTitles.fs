/// Titles a document could have, from the same places the title fixes take them.
module Legibyl.Validation.DocumentTitles

open System
open iText.Kernel.Pdf

/// A title, and where it came from ("document info", "XMP metadata", "first page").
type TitleCandidate = { Title: string; Source: string }

/// The document's own title (Info, then XMP), then a short first page's text (a title page),
/// in the order the DocumentTitle and TitleFromFirstPage fixes would use them. Distinct titles
/// only.
let candidates (pdfPath: string) : TitleCandidate list =
    use document = PdfFiles.openToRead pdfPath
    let info = document.GetDocumentInfo().GetTitle() |> Option.ofObj |> Option.filter (String.IsNullOrWhiteSpace >> not)
    [ yield! info |> Option.map (fun title -> { Title = title.Trim(); Source = "document info" }) |> Option.toList
      yield! StructureFixes.xmpTitle document |> Option.map (fun title -> { Title = title.Trim(); Source = "XMP metadata" }) |> Option.toList
      yield! StructureFixes.titlePageText document |> Option.map (fun title -> { Title = title; Source = "first page" }) |> Option.toList ]
    |> List.distinctBy _.Title
