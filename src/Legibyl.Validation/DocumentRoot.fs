/// A tagged PDF's structure starting with a single Document tag: the way into its tags that PDF
/// 2.0 asks for (PDF/UA-2's 8.2.5.2, and ISO 32005's rows for what the structure tree root may
/// hold), and that checkers like PAC look for in any PDF. Tags at the top without one (a Part, say,
/// or several) are put under a new Document; nothing else moves, so what's read, and in what
/// order, is the same.
module internal Legibyl.Validation.DocumentRoot

open iText.Kernel.Pdf
open Legibyl.Validation.StructureTreeObjects

/// The PDF 2.0 namespace's name (ISO 32000-2, 14.7.4.2).
[<Literal>]
let Pdf2Namespace = "http://iso.org/pdf2/ssn"

/// What the top of the structure holds.
type Top =
    /// A single Document tag (in PDF 2.0's namespace, in a PDF 2.0 file).
    | Fine
    /// Tags to put under a new Document: their tags, in order.
    | Wrap of tags: string list
    /// A single Document tag, in a PDF 2.0 file, not in PDF 2.0's namespace.
    | Namespace
    /// Something that isn't a tag (content, an annotation's reference), or a tag that only maps to
    /// Document in a PDF 2.0 file: left, and said.
    | Left of reason: string

let private isPdf2 (document: PdfDocument) = document.GetPdfVersion().CompareTo(PdfVersion.PDF_2_0) >= 0

let private rootOf (document: PdfDocument) = document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.StructTreeRoot

/// The root's kids, as they're kept (references stay references, so they can be moved).
let private kidsOf (root: PdfDictionary) : PdfObject list =
    match root.Get PdfName.K with
    | null -> []
    | :? PdfArray as kids -> [ for i in 0 .. kids.Size() - 1 -> kids.Get(i, false) ]
    | _ -> [ root.Get(PdfName.K, false) ]

/// Whether element is in PDF 2.0's namespace.
let private inPdf2Namespace (element: PdfDictionary) =
    match element.GetAsDictionary PdfName.NS with
    | null -> false
    | ns -> match ns.GetAsString PdfName.NS with null -> false | name -> name.ToUnicodeString() = Pdf2Namespace

/// What the top of document's structure holds; None if it has no tags.
let find (document: PdfDocument) context : Top option =
    match rootOf document with
    | null -> None
    | root ->
        let kids = kidsOf root |> List.map resolve
        let element (kid: PdfObject | null) =
            match kid with
            | :? PdfDictionary as d when not (isNull (d.GetAsName PdfName.S)) -> Some d
            | _ -> None
        match kids with
        | [] -> None
        | _ when kids |> List.exists (element >> Option.isNone) ->
            Some(Left "the top of its structure holds content or an annotation directly, not only tags")
        | _ ->
            let elements = kids |> List.choose element
            // A standard tag means itself, whatever a (broken) role map says of it; another name, what
            // the role map makes it.
            let tag (e: PdfDictionary) =
                match e.GetAsName(PdfName.S).GetValue() with
                | name when RoleMaps.standardTags.Contains name -> name
                | _ -> standardTag e context |> Option.ofObj |> Option.map _.GetValue() |> Option.defaultValue ""
            match elements with
            | [ single ] when tag single = "Document" ->
                if not (isPdf2 document) then Some Fine
                elif not (PdfName.Document.Equals(single.GetAsName PdfName.S)) then
                    Some(Left $"its top tag, {single.GetAsName(PdfName.S).GetValue()}, only maps to Document, which PDF 2.0 doesn't count as one")
                elif inPdf2Namespace single then Some Fine
                else Some Namespace
            | _ -> Some(Wrap [ for e in elements -> e.GetAsName(PdfName.S).GetValue() ])

/// PDF 2.0's namespace in the structure tree root's /Namespaces, added if it isn't there.
let private pdf2Namespace (document: PdfDocument) (root: PdfDictionary) =
    let namespaces =
        match root.GetAsArray PdfName.Namespaces with
        | null ->
            let namespaces = PdfArray()
            root.Put(PdfName.Namespaces, namespaces) |> ignore
            namespaces
        | namespaces -> namespaces
    let existing =
        [ for i in 0 .. namespaces.Size() - 1 do
              match namespaces.GetAsDictionary i with
              | null -> ()
              | ns ->
                  match ns.GetAsString PdfName.NS with
                  | NonNull name when name.ToUnicodeString() = Pdf2Namespace -> ns
                  | _ -> () ]
    match existing with
    | ns :: _ -> ns
    | [] ->
        let ns = PdfDictionary()
        ns.Put(PdfName.Type, PdfName.Namespace) |> ignore
        ns.Put(PdfName.NS, PdfString Pdf2Namespace) |> ignore
        ns.MakeIndirect document |> ignore
        namespaces.Add ns
        ns

/// Puts the tags at the top of document's structure under a single Document tag (in PDF 2.0's
/// namespace, in a PDF 2.0 file), or gives a Document tag already there that namespace. What it
/// can't, it says (context.Notes).
let apply (document: PdfDocument) context : string list =
    match find document context, rootOf document with
    | Some(Wrap tags), NonNull root ->
        let kids = kidsOf root
        let wrapper = PdfDictionary()
        wrapper.Put(PdfName.Type, PdfName.StructElem) |> ignore
        wrapper.Put(PdfName.S, PdfName.Document) |> ignore
        wrapper.Put(PdfName.P, root) |> ignore
        let array = PdfArray()
        for kid in kids do
            array.Add kid
        wrapper.Put(PdfName.K, array) |> ignore
        wrapper.MakeIndirect document |> ignore
        if isPdf2 document then wrapper.Put(PdfName.NS, pdf2Namespace document root) |> ignore
        for kid in kids do
            match resolve kid with
            | :? PdfDictionary as element -> element.Put(PdfName.P, wrapper) |> ignore
            | _ -> ()
        root.Put(PdfName.K, wrapper) |> ignore
        match tags with
        | [ tag ] -> [ $"Put the {tag} tag at the top of the structure under a Document tag." ]
        | _ -> [ $"""Put the {tags.Length} tags at the top of the structure ({System.String.Join(", ", tags)}) under a single Document tag.""" ]
    | Some Namespace, NonNull root ->
        match resolve (kidsOf root).Head with
        | :? PdfDictionary as document' ->
            document'.Put(PdfName.NS, pdf2Namespace document root) |> ignore
            [ "Gave the Document tag at the top of the structure PDF 2.0's namespace." ]
        | _ -> []
    | Some(Left reason), _ ->
        context.Notes.Add $"The tags weren't put under a single Document tag: {reason}. Fix it where the PDF was made, or in a PDF editor's tags panel."
        []
    | _ -> []
