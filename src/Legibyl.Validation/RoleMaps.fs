/// The role map: which standard tag each of a PDF's own tag names stands for (the structure tree
/// root's /RoleMap), which a screen reader follows to tell a heading from a paragraph. PDF/UA
/// wants every name a tag uses mapped, in the end, to a standard tag (7.1-5; 8.2.4-1), no mapping
/// going round in a circle (7.1-6; 8.2.4-2), and no standard tag mapped to another (7.1-7;
/// 8.2.4-3, 8.2.4-4). Here: which standard tag a name says it is, by the names common writers
/// use, and the map repaired by them. A name that doesn't say is left for a person.
module Legibyl.Validation.RoleMaps

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open iText.Kernel.Pdf

/// PDF's standard structure types (ISO 32000-1, 14.8.4): what a role map has to end at, and
/// mustn't map from.
let standardTags =
    set [ "Document"; "Part"; "Art"; "Sect"; "Div"; "BlockQuote"; "Caption"; "TOC"; "TOCI"; "Index"; "NonStruct"; "Private"
          "P"; "H"; "H1"; "H2"; "H3"; "H4"; "H5"; "H6"; "L"; "LI"; "Lbl"; "LBody"
          "Table"; "TR"; "TH"; "TD"; "THead"; "TBody"; "TFoot"
          "Span"; "Quote"; "Note"; "Reference"; "BibEntry"; "Code"; "Link"; "Annot"; "Ruby"; "RB"; "RT"; "RP"; "Warichu"; "WT"; "WP"
          "Figure"; "Formula"; "Form" ]

/// The standard tag each common writer's name stands for, by the name with its case, spaces and
/// punctuation taken away.
let private known =
    dict [
        // Paragraphs: Word's, LibreOffice's, InDesign's.
        for name in [ "bodytext"; "textbody"; "normal"; "normalweb"; "para"; "paragraph"; "text"; "standard"; "default"; "defaultparagraphstyle"; "firstparagraph"; "bodytextindent"; "plaintext" ] do
            name, "P"
        "title", "H1"
        "heading", "H"
        // Lists.
        "list", "L"
        for name in [ "listitem"; "listparagraph"; "listbullet"; "listnumber"; "bullet"; "bulleted"; "numberedlist"; "bulletedlist"; "listcontinue" ] do
            name, "LI"
        for name in [ "listlabel"; "label"; "bulletlabel" ] do
            name, "Lbl"
        for name in [ "listbody"; "listitembody" ] do
            name, "LBody"
        // Captions and notes.
        for name in [ "caption"; "figurecaption"; "tablecaption" ] do
            name, "Caption"
        for name in [ "footnote"; "footnotetext"; "endnote"; "endnotetext"; "note" ] do
            name, "Note"
        // Figures: Word writes InlineShape for a picture.
        for name in [ "image"; "picture"; "graphic"; "graphics"; "photo"; "chart"; "drawing"; "illustration"; "inlineshape"; "figure" ] do
            name, "Figure"
        // Tables.
        "table", "Table"
        for name in [ "tablerow"; "row" ] do
            name, "TR"
        for name in [ "tablecell"; "cell"; "tabledata" ] do
            name, "TD"
        for name in [ "tableheader"; "tableheading"; "tableheadercell"; "headercell" ] do
            name, "TH"
        for name in [ "tablehead"; "tableheaderrow" ] do
            name, "THead"
        "tablebody", "TBody"
        "tablefoot", "TFoot"
        // Contents and index.
        for name in [ "toc"; "tableofcontents"; "contents" ] do
            name, "TOC"
        for name in [ "tocentry"; "tocitem"; "contentsentry" ] do
            name, "TOCI"
        "index", "Index"
        // Blocks.
        for name in [ "section" ] do
            name, "Sect"
        for name in [ "article" ] do
            name, "Art"
        for name in [ "division"; "div"; "textbox" ] do
            name, "Div"
        for name in [ "blockquote"; "blocktext"; "quotation" ] do
            name, "BlockQuote"
        // Inline.
        for name in [ "span"; "emphasis"; "strong"; "em"; "bold"; "italic"; "characterstyle" ] do
            name, "Span"
        for name in [ "link"; "hyperlink" ] do
            name, "Link"
        for name in [ "code"; "sourcecode"; "preformatted"; "htmlpreformatted" ] do
            name, "Code"
        for name in [ "formula"; "equation"; "math" ] do
            name, "Formula"
    ]

/// The standard tag a PDF's own tag name says it is, if it does: a heading by its level
/// ("Heading 1", "Heading1", "H1Title", "Überschrift 2"), a TOC entry by its ("TOC 1"), else a
/// name common writers use ("Body Text", "Normal", "Bullet", "List Paragraph", "Image",
/// "Footnote", "TableCell"…), any case and spacing, or a standard tag in another case ("figure"
/// is Figure). None for a name that doesn't say.
let guess (name: string) : string option =
    let plain = Regex.Replace(name.ToLowerInvariant(), "[^\\p{L}\\p{N}]", "")
    let level = Regex.Match(plain, "^(heading|head|h|überschrift|titre|titulo|título)([1-6])(?![0-9])")
    if standardTags.Contains name then
        Some name
    elif level.Success then
        Some $"H{level.Groups[2].Value}"
    elif Regex.IsMatch(plain, "^(toc|contents)[1-9]$") then
        Some "TOCI"
    else
        match known.TryGetValue plain with
        | true, tag -> Some tag
        | _ -> standardTags |> Seq.tryFind (fun tag -> tag.ToLowerInvariant() = plain)

/// The tag names a document's structure elements use (in the PDF 1.7 namespace: not those an /NS
/// puts in another), each with how many use it.
let usedNames (document: PdfDocument) : Map<string, int> =
    let counts = Dictionary<string, int>()
    let seen = HashSet<PdfDictionary>(HashIdentity.Reference)
    let rec visit (element: PdfDictionary) =
        if seen.Add element then
            match element.GetAsName PdfName.S with
            | null -> ()
            | tag when isNull (element.Get PdfName.NS) ->
                let name = tag.GetValue()
                counts[name] <- (match counts.TryGetValue name with | true, n -> n + 1 | _ -> 1)
            | _ -> ()
            match element.Get(PdfName.K, true) with
            | :? PdfArray as kids ->
                for i in 0 .. kids.Size() - 1 do
                    match kids.Get(i, true) with
                    | :? PdfDictionary as kid when not (isNull (kid.Get PdfName.S)) -> visit kid
                    | _ -> ()
            | :? PdfDictionary as kid when not (isNull (kid.Get PdfName.S)) -> visit kid
            | _ -> ()
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> ()
    | root ->
        match root.Get(PdfName.K, true) with
        | :? PdfArray as kids ->
            for i in 0 .. kids.Size() - 1 do
                match kids.Get(i, true) with
                | :? PdfDictionary as kid -> visit kid
                | _ -> ()
        | :? PdfDictionary as kid -> visit kid
        | _ -> ()
    counts |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq

/// What repairing a role map did: the names mapped by what they say (each with the tag and how
/// many elements it serves), the standard tags' mappings removed, the circles broken (each by its
/// names), and the names
/// still not mapped to a standard tag, which don't say which (each with how many use it).
type Repair =
    { Mapped: (string * string * int) list
      Remapped: string list
      Circles: string list list
      Unknown: (string * int) list }

/// Repairs a document's role map: removes a mapping of a standard tag (to another: a standard tag
/// means itself); breaks a circle by mapping each of its names to the standard tag it says it is,
/// or, if it doesn't say, by removing its mapping; and maps each name the elements use that
/// doesn't end at a standard tag by what it, or the name its mapping ends at, says. A name that
/// doesn't say is left, and listed. Nothing for a document without a structure tree.
let repair (document: PdfDocument) : Repair =
    let none = { Mapped = []; Remapped = []; Circles = []; Unknown = [] }
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> none
    | root ->
        let used = usedNames document
        let map =
            match root.GetAsDictionary PdfName.RoleMap with
            | null -> PdfDictionary()
            | map -> map
        let target (name: string) = match map.GetAsName(PdfName name) with null -> None | role -> Some(role.GetValue())
        // A standard tag mapped: it means itself.
        let remapped = [ for key in List.ofSeq (map.KeySet()) do if standardTags.Contains(key.GetValue()) then key.GetValue() ]
        for name in remapped do
            map.Remove(PdfName name) |> ignore
        // The names in a circle: following the map from one comes back to it.
        let inCircle (name: string) =
            let rec follow (current: string) (visited: Set<string>) =
                match target current with
                | Some next when next = name -> true
                | Some next when not (visited.Contains next) && not (standardTags.Contains next) -> follow next (visited.Add next)
                | _ -> false
            follow name (set [ name ])
        let circling = [ for key in List.ofSeq (map.KeySet()) do if inCircle (key.GetValue()) then key.GetValue() ]
        // Each circle, by its names (in order, from its first): one circle each, however many.
        let circles =
            circling
            |> List.map (fun name ->
                let rec round (current: string) (names: string list) =
                    match target current with
                    | Some next when next <> name -> round next (next :: names)
                    | _ -> List.rev names
                round name [ name ] |> List.sort)
            |> List.distinct
        for name in circling do
            match guess name with
            | Some tag -> map.Put(PdfName name, PdfName tag) |> ignore
            | None -> map.Remove(PdfName name) |> ignore
        // Where a name's mapping ends: a standard tag, or a name not mapped.
        let rec ends (name: string) (depth: int) =
            if standardTags.Contains name || depth > 64 then name
            else match target name with Some next -> ends next (depth + 1) | None -> name
        // Each name mapped, with its tag.
        let mapped = Dictionary<string, string>()
        let unknown = ResizeArray<string * int>()
        let record (key: string) (tag: string) (_: int) = mapped[key] <- tag
        for KeyValue(name, uses) in used do
            let last = ends name 0
            if not (standardTags.Contains last) then
                // What the name it ends at says (which mends every name ending there), else its own.
                match guess last, guess name with
                | Some tag, _ ->
                    map.Put(PdfName last, PdfName tag) |> ignore
                    record last tag uses
                | None, Some tag ->
                    map.Put(PdfName name, PdfName tag) |> ignore
                    record name tag uses
                | None, None -> unknown.Add(name, uses)
        // (A /RoleMap that isn't a dictionary is replaced.)
        if map.Size() > 0 && isNull (root.GetAsDictionary PdfName.RoleMap) then root.Put(PdfName.RoleMap, map) |> ignore
        // How many elements a mapping serves: those whose name's mapping, as it is now, goes through it.
        let serves (key: string) =
            let rec through (name: string) (depth: int) =
                name = key || (depth < 64 && not (standardTags.Contains name) && match target name with Some next -> through next (depth + 1) | None -> false)
            used |> Seq.sumBy (fun (KeyValue(name, uses)) -> if through name 0 then uses else 0)
        { Mapped = [ for KeyValue(name, tag) in mapped -> name, tag, serves name ] |> List.sort
          Remapped = remapped
          Circles = circles
          Unknown = List.ofSeq unknown }

/// Writes the standard tag a person chose for each of a document's own tag names to its role map
/// (a choice of a tag that isn't standard, or for a name that is, is passed over), replacing
/// what it had; returns those it changed. Nothing for a document without a structure tree.
let choose (document: PdfDocument) (chosen: Map<string, string>) : (string * string) list =
    match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
    | null -> []
    | root ->
        let map =
            match root.GetAsDictionary PdfName.RoleMap with
            | null ->
                let map = PdfDictionary()
                root.Put(PdfName.RoleMap, map) |> ignore
                map
            | map -> map
        let changed =
            [ for KeyValue(name, tag) in chosen do
                  if standardTags.Contains tag && not (standardTags.Contains name) then
                      match map.GetAsName(PdfName name) with
                      | NonNull current when current.GetValue() = tag -> ()
                      | _ ->
                          map.Put(PdfName name, PdfName tag) |> ignore
                          yield name, tag ]
        if map.Size() = 0 then root.Remove PdfName.RoleMap |> ignore
        changed

/// How many of the tags a person chose for the PDF at pdfPath's own tag names it doesn't have
/// in its role map yet (see choose).
let pending (pdfPath: string) (chosen: Map<string, string>) : int =
    if chosen.IsEmpty then
        0
    else
        use document = new PdfDocument(new PdfReader(pdfPath))
        let map =
            match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot with
            | null -> null
            | root -> root.GetAsDictionary PdfName.RoleMap
        chosen
        |> Seq.filter (fun (KeyValue(name, tag)) ->
            standardTags.Contains tag
            && not (standardTags.Contains name)
            && (match map with
                | null -> true
                | map -> match map.GetAsName(PdfName name) with NonNull current -> current.GetValue() <> tag | Null -> true))
        |> Seq.length
