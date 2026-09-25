/// Parses veraPDF's <c>locationContext</c> strings — a slash-separated walk of its internal
/// object model, e.g. <c>root/document[0]/pages[1](7 0 obj PDPage)/annots[0](9 0 obj PDLinkAnnot)</c>
/// — into a page number and a plain-English breadcrumb, so users don't need to know veraPDF's
/// object model to figure out where in the document a failure is.
module Legibyl.Validation.PdfLocationParser

open System
open System.Collections.Generic
open System.Text.RegularExpressions

// Each path segment looks like: name[index](objNum genNum obj Type...)
// The parenthesised part and/or the [index] are optional.
let private segmentPattern =
    Regex(@"^(?<name>[A-Za-z]+)(\[(?<idx>\d+)\])?(\((?<num>\d+)\s+\d+\s+obj\s+(?<type>.+)\))?$", RegexOptions.Compiled)

let private camelCasePattern = Regex("(?<=[a-z0-9])(?=[A-Z])", RegexOptions.Compiled)

let private ignoreCaseMap (pairs: (string * string) list) =
    let map = Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
    for key, value in pairs do
        map[key] <- value
    map

let private tagFriendlyNames =
    ignoreCaseMap
        [ "Document", "document root"
          "Table", "table"
          "TR", "table row (TR)"
          "TH", "table header cell (TH)"
          "TD", "table cell (TD)"
          "Figure", "figure"
          "Link", "link"
          "L", "list"
          "LI", "list item"
          "Lbl", "list item label"
          "LBody", "list item body"
          "P", "paragraph"
          "Span", "text span"
          "Div", "division"
          "Sect", "section"
          "Art", "article"
          "H1", "heading (H1)"
          "H2", "heading (H2)"
          "H3", "heading (H3)"
          "H4", "heading (H4)"
          "H5", "heading (H5)"
          "H6", "heading (H6)"
          "Caption", "caption"
          "THead", "table header group"
          "TBody", "table body group"
          "TFoot", "table footer group" ]

let private typeOverrides =
    ignoreCaseMap
        [ "PDPage", "page"
          "PDLinkAnnot", "link annotation"
          "PDStructTreeRoot", "structure tree"
          "PDSemanticContentStream", "content stream"
          "PDAnnot", "annotation"
          "PDWidgetAnnot", "form field annotation" ]

let private nameOverrides =
    ignoreCaseMap
        [ "document", "document"
          "annots", "annotation"
          "contentStream", "content stream"
          "content", "content"
          "contentItem", "content item"
          "structtreeroot", "structure tree"
          "k", "child element" ]

let private isName expected (name: string) =
    String.Equals(name, expected, StringComparison.OrdinalIgnoreCase)

let private capitalize (value: string) =
    if value.Length = 0 then value else string (Char.ToUpperInvariant value[0]) + value[1..]

let private humanizeType (typeName: string) =
    match typeOverrides.TryGetValue typeName with
    | true, overrideValue -> overrideValue
    | _ ->
        // Structure element types look like "SETable Table" / "SETD TD" — the second
        // word is the actual PDF structure tag, which is far more meaningful to a user
        // than the internal "SE..." class name.
        match typeName.Split(' ', 2) with
        | [| prefix; tag |] when prefix.StartsWith("SE", StringComparison.Ordinal) ->
            match tagFriendlyNames.TryGetValue tag with
            | true, friendly -> friendly
            | _ -> $"{tag} element"
        | _ ->
            let stripped = if typeName.StartsWith("PD", StringComparison.Ordinal) then typeName[2..] else typeName
            camelCasePattern.Replace(stripped, " $1").ToLowerInvariant()

let private describeSegment (name: string) (index: int option) (typeName: string option) =
    match index, typeName with
    | Some index, _ when isName "pages" name -> $"Page {index + 1}"
    | _, Some typeName -> capitalize (humanizeType typeName)
    | _ ->
        match nameOverrides.TryGetValue name with
        | true, overrideName -> capitalize overrideName
        | _ -> capitalize name

/// Parses a veraPDF locationContext string. Returns null if <paramref name="locationContext"/>
/// is null/empty or doesn't look like a recognisable veraPDF path (falls back gracefully
/// rather than throwing, since this is a best-effort readability aid).
let Parse (locationContext: string | null) : PdfLocation | null =
    match locationContext with
    | null -> null
    | context when String.IsNullOrWhiteSpace context -> null
    | context ->
        let mutable pageNumber = None
        let mutable objectType = None
        let mutable objectNumber = None
        let mutable annotationIndex = None
        let breadcrumb = ResizeArray<string>()

        for segment in context.Split('/', StringSplitOptions.RemoveEmptyEntries) do
            if not (isName "root" segment) then
                let m = segmentPattern.Match segment
                if not m.Success then
                    breadcrumb.Add segment
                else
                    let group (name: string) =
                        let g = m.Groups[name]
                        if g.Success then Some g.Value else None

                    let name = m.Groups["name"].Value
                    let index = group "idx" |> Option.map int
                    let number = group "num" |> Option.map int
                    let typeName = group "type"

                    match index with
                    | Some i when isName "pages" name -> pageNumber <- Some(i + 1) // veraPDF's page index is 0-based.
                    | Some i when isName "annots" name -> annotationIndex <- Some i
                    | _ -> ()

                    if number.IsSome then
                        objectNumber <- number
                        objectType <- typeName

                    breadcrumb.Add(describeSegment name index typeName)

        if breadcrumb.Count = 0 then
            breadcrumb.Add context

        { PageNumber = Option.toNullable pageNumber
          ObjectType = Option.toObj objectType
          ObjectNumber = Option.toNullable objectNumber
          AnnotationIndex = Option.toNullable annotationIndex
          Breadcrumb = String.Join(" → ", breadcrumb) }
