/// Joining PDFs that already pass PDF/UA into one, keeping each one's tags as they are, and
/// checking the result. iText's PdfMerger, with tags kept, copies the pages, annotations and
/// structure, and joins the parent trees; this adds what it doesn't do:
///
/// - each input becomes a part of the output (Sect or Part for PDF/UA-1, DocumentFragment for
///   PDF/UA-2) under one Document, with its own /Lang where it differs from the output's;
/// - a custom tag two inputs map differently is renamed in the later one (iText keeps the first
///   mapping, so the later one's elements would read as something else), and so are a class
///   (/ClassMap) two inputs define differently, a structure element ID (/IDTree) and a named
///   destination two inputs both use (iText keeps the last, so links into the earlier one would
///   go to the later one);
/// - the role map, class map, ID tree and namespaces of them all, which iText drops or keeps
///   only the first of;
/// - the document level: /Lang, /MarkInfo, /DisplayDocTitle, the title and the PDF/UA claim in
///   the XMP metadata, and an outline entry for each part with that input's own outline below.
///
/// What it can't join yet (forms, optional content, attached files, an incomplete parent tree)
/// it refuses, saying why; see refusals. No fixes are run on the joined file.
module Legibyl.Validation.Append

open System
open System.Collections.Generic
open System.IO
open System.Text
open iText.Kernel.Pdf
open iText.Kernel.Utils
open iText.Kernel.XMP

/// What to join with: the output's title and language (else the first input's), and the tag each
/// input's part gets (else Sect for PDF/UA-1, DocumentFragment for PDF/UA-2).
type Options =
    { Title: string option
      Language: string option
      PartTag: string option }

    static member Default = { Title = None; Language = None; PartTag = None }

/// Why files can't be joined: one of them (File), or the set of them (File null).
type Refusal = { File: string | null; Reason: string }

/// The tags a part can be, by standard.
let partTags (profile: PdfProfile) =
    match profile with
    | PdfProfile.PdfUa2 -> [ "DocumentFragment"; "Sect"; "Part" ]
    | _ -> [ "Sect"; "Part" ]

// ---- Reading the inputs ----

/// What's read from an input before it's joined.
type private Input =
    { File: string
      Version: PdfVersion
      /// Its title: its XMP dc:title, else its Info /Title, else its file name.
      Title: string
      /// Its language: its catalog's /Lang.
      Language: string option
      /// Whether it carries a review Legibyl saved in it (see EmbeddedReview).
      HasReview: bool
      Pages: int
      /// Its own namespaces (PDF 2.0), by name: what each maps its tags to (/RoleMapNS).
      Namespaces: Map<string, string> }

let private textString (value: string) =
    if value |> Seq.forall (fun c -> c < '\u0080') then PdfString value else PdfString(value, iText.IO.Font.PdfEncodings.UNICODE_BIG)

let private text (value: string | null) =
    match value with
    | NonNull value when not (String.IsNullOrWhiteSpace value) -> Some(value.Trim())
    | _ -> None

let private titleOf (document: PdfDocument) (file: string) =
    let xmp =
        try
            match document.GetXmpMetadata() with
            | null -> None
            | meta ->
                match meta.GetLocalizedText(XMPConst.NS_DC, "title", null, XMPConst.X_DEFAULT) with
                | null -> None
                | property -> text (property.GetValue())
        with _ ->
            None
    xmp
    |> Option.orElse (text (document.GetDocumentInfo().GetTitle()))
    |> Option.defaultWith (fun () -> string (Path.GetFileNameWithoutExtension file))

/// Whether a page or the document has attached files: an /EmbeddedFiles name tree, or a file
/// attachment annotation.
let private hasAttachments (document: PdfDocument) =
    let named =
        match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.Names with
        | null -> false
        | names -> not (isNull (names.Get PdfName.EmbeddedFiles)) && document.GetCatalog().GetNameTree(PdfName.EmbeddedFiles).GetNames().Count > 0
    named
    || seq { 1 .. document.GetNumberOfPages() }
       |> Seq.exists (fun page ->
           document.GetPage(page).GetAnnotations() |> Seq.exists (fun annotation -> PdfName.FileAttachment.Equals(annotation.GetSubtype())))

/// The structure tree's namespaces (PDF 2.0, /Namespaces), by name: what each maps its tags to
/// (/RoleMapNS: a tag to a tag, or to a tag in another namespace, named), written out to
/// compare by.
let private namespaces (root: PdfDictionary) : Map<string, string> =
    let name (ns: PdfDictionary | null) =
        match ns with
        | NonNull ns -> match ns.GetAsString PdfName.NS with null -> "" | uri -> uri.ToUnicodeString()
        | Null -> ""
    match root.GetAsArray PdfName.Namespaces with
    | null -> Map.empty
    | list ->
        [ for i in 0 .. list.Size() - 1 do
              match list.GetAsDictionary i with
              | null -> ()
              | ns ->
                  let roles =
                      match ns.GetAsDictionary PdfName.RoleMapNS with
                      | null -> ""
                      | map ->
                          [ for key in map.KeySet() |> Seq.sortBy _.GetValue() ->
                                let role =
                                    match map.Get key with
                                    | :? PdfName as tag -> tag.GetValue()
                                    | :? PdfArray as target when target.Size() >= 1 ->
                                        let tag = match target.GetAsName 0 with null -> "" | tag -> tag.GetValue()
                                        $"{tag} in {name (target.GetAsDictionary 1)}"
                                    | _ -> ""
                                $"{key.GetValue()} -> {role}" ]
                          |> String.concat "; "
                  name ns, roles ]
        |> Map.ofList

/// PdfMerger copies the forward hierarchy, not orphaned elements still named by the parent
/// tree. Check both directions before it can discard those content associations. /P alone
/// cannot tell us where an orphan belongs in its parent's reading order.
let private structureProblem (document: PdfDocument) (root: PdfDictionary) =
    let entries = Dictionary<int, PdfObject>()
    let expected = Dictionary<int * int, PdfDictionary>()
    let seen = HashSet<PdfDictionary>(HashIdentity.Reference)
    let mutable problem = None
    let fail detail =
        if problem.IsNone then
            problem <-
                Some
                    $"Its tag hierarchy and parent tree disagree ({detail}). Joining would lose or change content tags. Run Rebuild the parent tree (parent-tree) and append the fixed copy: it resolves missing associations and obsolete empty or decorative entries. Meaningful content outside the hierarchy is kept and reported; restore its place and verify reading order before retrying. Append does not guess reading order or run fixes."
    let same (a: PdfDictionary) (b: PdfDictionary) =
        obj.ReferenceEquals(a, b)
        || (not (isNull (a.GetIndirectReference())) && a.GetIndirectReference().Equals(b.GetIndirectReference()))
    let rec numbers (node: PdfDictionary) depth =
        if depth > 64 || not (seen.Add node) then
            fail "the parent number tree is cyclic or too deep"
        else
            match node.GetAsArray PdfName.Nums with
            | null -> ()
            | nums ->
                for i in 0 .. 2 .. nums.Size() - 2 do
                    match nums.GetAsNumber i with
                    | null -> fail "a parent tree key is not a number"
                    | key ->
                        if entries.ContainsKey(key.IntValue()) then fail $"parent tree key {key.IntValue()} is repeated"
                        else entries[key.IntValue()] <- nums.Get(i + 1)
            match node.GetAsArray PdfName.Kids with
            | null -> ()
            | kids ->
                for i in 0 .. kids.Size() - 1 do
                    match kids.GetAsDictionary i with
                    | null -> fail "a parent number tree child is not a dictionary"
                    | child -> numbers child (depth + 1)
    match root.GetAsDictionary PdfName.ParentTree with
    | null -> ()
    | tree -> numbers tree 0
    seen.Clear()
    let add (holder: PdfDictionary | null) (mcid: int) (element: PdfDictionary) =
        match holder with
        | null -> fail $"MCID {mcid} has no page or stream"
        | holder ->
            match holder.GetAsNumber PdfName.StructParents with
            | null -> fail $"MCID {mcid} has no StructParents key"
            | key ->
                let address = key.IntValue(), mcid
                if expected.ContainsKey address then fail $"parent tree key {fst address}, MCID {mcid} has multiple structure references"
                else expected[address] <- element
    let objects = Dictionary<int, PdfDictionary>()
    let rec visit (element: PdfDictionary) (parent: PdfDictionary) (page: PdfDictionary | null) depth =
        if depth > 512 || not (seen.Add element) then
            fail "the tag hierarchy is cyclic, shared or too deep"
        else
            match element.GetAsDictionary PdfName.P with
            | NonNull own when same own parent -> ()
            | _ -> fail "a structure element's parent pointer disagrees with its position in the hierarchy"
            let page = match element.GetAsDictionary PdfName.Pg with null -> page | own -> own
            let rec kid (value: PdfObject | null) =
                match StructureTreeObjects.resolve value with
                | :? PdfArray as kids -> for i in 0 .. kids.Size() - 1 do kid (kids.Get i)
                | :? PdfNumber as mcid -> add page (mcid.IntValue()) element
                | :? PdfDictionary as child when not (isNull (child.GetAsName PdfName.S)) -> visit child element page (depth + 1)
                | :? PdfDictionary as reference ->
                    match reference.GetAsNumber PdfName.MCID, reference.GetAsDictionary PdfName.Obj with
                    | NonNull mcid, _ ->
                        match reference.GetAsStream PdfName.Stm with
                        | NonNull stream -> add stream (mcid.IntValue()) element
                        | Null ->
                            let holder = match reference.GetAsDictionary PdfName.Pg with null -> page | own -> own
                            add holder (mcid.IntValue()) element
                    | Null, NonNull obj ->
                        match obj.GetAsNumber PdfName.StructParent with
                        | null -> fail "a tagged object has no StructParent key"
                        | key ->
                            if objects.ContainsKey(key.IntValue()) then fail $"parent tree key {key.IntValue()} has multiple object references"
                            else objects[key.IntValue()] <- element
                    | _ -> ()
                | _ -> ()
            kid (element.Get PdfName.K)
    let rec top (value: PdfObject | null) =
        match StructureTreeObjects.resolve value with
        | :? PdfArray as kids -> for i in 0 .. kids.Size() - 1 do top (kids.Get i)
        | :? PdfDictionary as element -> visit element root null 0
        | _ -> ()
    top (root.Get PdfName.K)
    let matches (value: PdfObject | null) (element: PdfDictionary) =
        match StructureTreeObjects.resolve value with
        | :? PdfDictionary as actual -> same actual element
        | _ -> false
    for KeyValue((key, mcid), element) in expected do
        match entries.TryGetValue key with
        | true, (:? PdfArray as array) when mcid >= 0 && mcid < array.Size() && matches (array.Get mcid) element -> ()
        | _ -> fail $"parent tree key {key}, MCID {mcid} does not point to its reachable tag"
    for KeyValue(key, element) in objects do
        match entries.TryGetValue key with
        | true, value when matches value element -> ()
        | _ -> fail $"parent tree key {key} does not point to its reachable object tag"
    for KeyValue(key, value) in entries do
        match StructureTreeObjects.resolve value with
        | :? PdfArray as array ->
            for mcid in 0 .. array.Size() - 1 do
                let value = StructureTreeObjects.resolve (array.Get mcid)
                if not (isNull value) && not (value :? PdfNull) then
                    match expected.TryGetValue((key, mcid)) with
                    | true, element when matches value element -> ()
                    | _ -> fail $"parent tree key {key}, MCID {mcid} names a tag not reachable at that address from StructTreeRoot/K"
        | :? PdfDictionary as element ->
            match objects.TryGetValue key with
            | true, expected when same element expected -> ()
            | _ -> fail $"parent tree key {key} names an object tag not reachable from StructTreeRoot/K"
        | _ -> fail $"parent tree key {key} has an invalid value"
    problem

/// Reads file, or why it can't be joined.
let private read (file: string) : Result<Input, Refusal list> =
    let refuse reason = { File = file; Reason = reason }
    let encrypted = "It's encrypted: save a copy without a password, then join that."
    if not (File.Exists file) then
        Error [ refuse "It doesn't exist." ]
    else
        try
            use reader = new PdfReader(file)
            use document = new PdfDocument(reader)
            let catalog = document.GetCatalog().GetPdfObject()
            let fields =
                match catalog.GetAsDictionary PdfName.AcroForm with
                | null -> 0
                | form ->
                    match form.GetAsArray PdfName.Fields with
                    | null -> 0
                    | fields -> fields.Size()
            let problems =
                [ // Opened without its password (one asked only to change it), it can be read, not
                  // written again.
                  if reader.IsEncrypted() then
                      refuse encrypted
                  if isNull (catalog.GetAsDictionary PdfName.StructTreeRoot) then
                      refuse "It isn't tagged: it has no structure tree to keep."
                  else
                      let context = StructureTreeObjects.createContext document file
                      match ParentTree.find document context with
                      | Some missing when missing.TreeMissing -> refuse "Its parent tree (which ties the page content to its tags) is missing. Run Rebuild the parent tree (parent-tree) and append the fixed copy."
                      | _ ->
                          match structureProblem document (catalog.GetAsDictionary PdfName.StructTreeRoot |> nonNull) with
                          | Some reason -> refuse reason
                          | None -> ()
                  if fields > 0 then
                      refuse "It has a form: field names can collide across the files, so forms aren't joined yet."
                  if not (isNull (catalog.Get PdfName.OCProperties)) then
                      refuse "It has optional content (layers): their names and visibility would need merging, so it isn't joined yet."
                  if hasAttachments document then
                      refuse "It has attached files, which aren't joined yet." ]
            if not problems.IsEmpty then
                Error problems
            else
                Ok
                    { File = file
                      Version = document.GetPdfVersion()
                      Title = titleOf document file
                      Language = text (match catalog.GetAsString PdfName.Lang with null -> null | lang -> lang.ToUnicodeString())
                      HasReview = not (isNull (catalog.Get(PdfName "LBYL_Review")))
                      Pages = document.GetNumberOfPages()
                      Namespaces = namespaces (catalog.GetAsDictionary PdfName.StructTreeRoot |> nonNull) }
        with
        // Without iText's cryptography (Legibyl doesn't ship it), an encrypted file can't be opened.
        | :? iText.Kernel.Exceptions.PdfException as ex when ex.Message = iText.Kernel.Exceptions.KernelExceptionMessageConstant.PDF_ENCRYPTION ->
            Error [ refuse encrypted ]
        | :? iText.Commons.Exceptions.ITextException
        | :? IOException
        | :? UnauthorizedAccessException as ex -> Error [ refuse $"It couldn't be read: {ex.Message}" ]

/// The standard the joined file is for: PDF/UA-2 if the inputs are PDF 2.0, else PDF/UA-1.
let private profileOf (version: PdfVersion) =
    if version.CompareTo PdfVersion.PDF_2_0 >= 0 then PdfProfile.PdfUa2 else PdfProfile.PdfUa1

let private versionName (version: PdfVersion) = version.ToString().Replace("PDF-", "")

/// Why the files can't be joined as they are, before any are validated: each one's problems,
/// then whether they mix PDF 1.x and 2.0, or map a namespace's tags differently. Empty when they
/// can be.
let refusals (files: string list) : Refusal list =
    let read = files |> List.map read
    let each = read |> List.collect (function Error refusals -> refusals | Ok _ -> [])
    let inputs = read |> List.choose Result.toOption
    let profiles = inputs |> List.map (fun input -> profileOf input.Version) |> List.distinct
    [ yield! each
      if profiles.Length > 1 then
          let listed =
              inputs
              |> List.map (fun input -> $"{Path.GetFileName input.File} (PDF {versionName input.Version})")
          yield
              { File = null
                Reason =
                  $"""PDF 2.0 files (PDF/UA-2) and earlier ones (PDF/UA-1) can't be joined into one: {String.Join(", ", listed)}.""" }
      // The merge keeps one of each namespace (the first file's), so a later file's tags in a
      // namespace it maps differently would read as the first's do.
      let seen = Dictionary<string, Input>()
      for input in inputs do
          for KeyValue(ns, roles) in input.Namespaces do
              match seen.TryGetValue ns with
              | true, earlier when earlier.Namespaces[ns] <> roles ->
                  yield
                      { File = input.File
                        Reason =
                          $"Its namespace {ns} maps its tags differently from {Path.GetFileName earlier.File}'s, so its tags would read as that file's do; files like that aren't joined yet." }
              | true, _ -> ()
              | false, _ -> seen[ns] <- input ]

// ---- The structure tree, raw ----

/// Every structure element under root's /K, in reading order (not MCRs or OBJRs).
let private elements (root: PdfDictionary) : PdfDictionary list =
    let seen = HashSet<PdfDictionary>(HashIdentity.Reference)
    let found = List<PdfDictionary>()
    let rec visit (node: PdfObject | null) depth =
        match node with
        | :? PdfDictionary as element when depth < 512 && seen.Add element ->
            if not (isNull (element.GetAsName PdfName.S)) then
                found.Add element
                match element.Get PdfName.K with
                | :? PdfArray as kids ->
                    for i in 0 .. kids.Size() - 1 do
                        visit (kids.Get i) (depth + 1)
                | kid -> visit kid (depth + 1)
        | :? PdfArray as kids when depth = 0 ->
            for i in 0 .. kids.Size() - 1 do
                visit (kids.Get i) 1
        | _ -> ()
    visit (root.Get PdfName.K) 0
    List.ofSeq found

/// The element dictionaries among an element's /K.
let private kidElements (element: PdfDictionary) : PdfDictionary list =
    let kids =
        match element.Get PdfName.K with
        | :? PdfArray as kids -> [ for i in 0 .. kids.Size() - 1 -> kids.Get i ]
        | NonNull kid -> [ kid ]
        | Null -> []
    kids |> List.choose (function :? PdfDictionary as kid when not (isNull (kid.GetAsName PdfName.S)) -> Some kid | _ -> None)

/// The standard tag an element's /S stands for, through the role map.
let private standardTag (roleMap: PdfDictionary | null) (element: PdfDictionary) =
    let rec follow (name: PdfName) depth =
        match roleMap with
        | NonNull map when depth < 16 ->
            match map.GetAsName name with
            | null -> name
            | role -> follow role (depth + 1)
        | _ -> name
    match element.GetAsName PdfName.S with
    | null -> None
    | name -> Some((follow name 0).GetValue())

/// An input's elements that become its part: the children of its one Document element, if its
/// root holds just that, else its root's own elements; and that Document, if there is one.
let private partSource (root: PdfDictionary) =
    match kidElements root with
    | [ only ] when standardTag (root.GetAsDictionary PdfName.RoleMap) only = Some "Document" -> kidElements only, Some only
    | tops -> tops, None

/// A PDF object written out to compare by: what two class definitions say.
let rec private canonical (value: PdfObject | null) depth : string =
    match value with
    | _ when depth > 16 -> "…"
    | :? PdfDictionary as dictionary ->
        let entries =
            dictionary.KeySet()
            |> Seq.sortBy _.GetValue()
            |> Seq.map (fun key -> $"/{key.GetValue()} {canonical (dictionary.Get key) (depth + 1)}")
        "<<" + String.Join(" ", entries) + ">>"
    | :? PdfArray as array -> "[" + String.Join(" ", [ for i in 0 .. array.Size() - 1 -> canonical (array.Get i) (depth + 1) ]) + "]"
    | :? PdfString as text -> "(" + text.ToUnicodeString() + ")"
    | NonNull other -> string other
    | Null -> "null"

// ---- Preparing each input ----

/// What's been taken by the inputs before: tag names (and what each maps to), classes (and what
/// each defines), IDs and named destinations.
type private Taken =
    { RoleMap: Dictionary<string, string>
      Classes: Dictionary<string, string>
      Ids: HashSet<string>
      Destinations: HashSet<string> }

/// An input made ready to join: a copy of it with its clashing names renamed and its part's
/// elements marked, and what was renamed.
type private Prepared =
    { Input: Input
      Bytes: byte array
      /// The language its part's Document element gave it, else its own.
      Language: string option
      /// Custom tags renamed: old name -> new.
      RenamedTags: Map<string, string>
      Notes: string list }

/// The key a part's elements are marked with while they're joined (removed after).
let private partKey = PdfName "LBYL_AppendPart"

/// A name not in used: name with "-2", "-3"… added.
let private fresh (used: string -> bool) (name: string) =
    Seq.initInfinite (fun n -> $"{name}-{n + 2}") |> Seq.find (used >> not)

/// Renames what clashes with the inputs before, in a copy of input: custom tags mapped
/// differently, classes defined differently, IDs and named destinations; and marks the elements
/// that will be its part.
let private prepare (taken: Taken) (index: int) (input: Input) : Prepared =
    use stream = new MemoryStream()
    let writer = new PdfWriter(stream)
    writer.SetCloseStream false
    let notes = List<string>()
    let name = Path.GetFileName input.File
    let mutable renamedTags = Map.empty
    let mutable language = input.Language
    do
        use document = new PdfDocument(new PdfReader(input.File), writer)
        let root = document.GetStructTreeRoot().GetPdfObject()
        let all = elements root

        // Custom tags mapped differently from an input before. A tag mapped to one that's
        // renamed maps differently too, so this goes on until nothing more is.
        match root.GetAsDictionary PdfName.RoleMap with
        | null -> ()
        | roleMap ->
            let map = Dictionary<string, string>()
            for key in roleMap.KeySet() do
                match roleMap.GetAsName key with
                | null -> ()
                | role -> map[key.GetValue()] <- role.GetValue()
            let renames = Dictionary<string, string>()
            let renamed (tag: string) = match renames.TryGetValue tag with | true, other -> other | _ -> tag
            let used (tag: string) = taken.RoleMap.ContainsKey tag || map.ContainsKey tag || renames.ContainsValue tag || RoleMaps.standardTags.Contains tag
            let mutable changed = true
            while changed do
                changed <- false
                for KeyValue(tag, role) in List.ofSeq map do
                    if not (renames.ContainsKey tag) then
                        match taken.RoleMap.TryGetValue tag with
                        | true, before when before <> renamed role ->
                            renames[tag] <- fresh used tag
                            changed <- true
                        | _ -> ()
            if renames.Count > 0 then
                let rewritten = PdfDictionary()
                for KeyValue(tag, role) in map do
                    rewritten.Put(PdfName(renamed tag), PdfName(renamed role)) |> ignore
                root.Put(PdfName.RoleMap, rewritten) |> ignore
                for element in all do
                    match element.GetAsName PdfName.S with
                    | NonNull tag when renames.ContainsKey(tag.GetValue()) -> element.Put(PdfName.S, PdfName(renames[tag.GetValue()])) |> ignore
                    | _ -> ()
                for KeyValue(tag, other) in renames do
                    notes.Add $"Renamed the tag <{tag}> in {name} to <{other}>: an earlier file maps <{tag}> to something else."
                renamedTags <- renames |> Seq.map (fun pair -> pair.Key, pair.Value) |> Map.ofSeq
            for KeyValue(tag, role) in map do
                taken.RoleMap[renamed tag] <- renamed role

        // Classes defined differently from an input before.
        match root.GetAsDictionary(PdfName "ClassMap") with
        | null -> ()
        | classMap ->
            let renames = Dictionary<string, string>()
            let defined = classMap.KeySet() |> Seq.map _.GetValue() |> Set.ofSeq
            for key in List.ofSeq (classMap.KeySet()) do
                let definition = canonical (classMap.Get key) 0
                match taken.Classes.TryGetValue(key.GetValue()) with
                | true, before when before <> definition ->
                    let other = fresh (fun c -> taken.Classes.ContainsKey c || defined.Contains c || renames.ContainsValue c) (key.GetValue())
                    renames[key.GetValue()] <- other
                    classMap.Put(PdfName other, classMap.Get(key, false)) |> ignore
                    classMap.Remove key |> ignore
                | _ -> ()
            if renames.Count > 0 then
                let rename (value: PdfObject | null) : PdfObject | null =
                    match value with
                    | :? PdfName as c when renames.ContainsKey(c.GetValue()) -> PdfName(renames[c.GetValue()])
                    | other -> other
                for element in all do
                    match element.Get PdfName.C with
                    | :? PdfArray as classes ->
                        for i in 0 .. classes.Size() - 1 do
                            match rename (classes.Get i) with
                            | NonNull renamed -> classes.Set(i, renamed) |> ignore
                            | Null -> ()
                    | :? PdfName as c when renames.ContainsKey(c.GetValue()) -> element.Put(PdfName.C, PdfName(renames[c.GetValue()])) |> ignore
                    | _ -> ()
                for KeyValue(c, other) in renames do
                    notes.Add $"Renamed the class {c} in {name} to {other}: an earlier file defines {c} differently."
            for key in classMap.KeySet() do
                taken.Classes[key.GetValue()] <- canonical (classMap.Get key) 0

        // IDs an input before has, and the table headers (/Headers) that name them.
        let ids = Dictionary<string, string>()
        let mine = all |> List.choose (fun e -> match e.GetAsString PdfName.ID with null -> None | id -> Some(id.ToUnicodeString())) |> Set.ofList
        for element in all do
            match element.GetAsString PdfName.ID with
            | null -> ()
            | id ->
                let value = id.ToUnicodeString()
                if taken.Ids.Contains value then
                    let used (i: string) = taken.Ids.Contains i || mine.Contains i || ids.ContainsValue i
                    let prefixed = $"p{index + 1}-{value}"
                    let other = if used prefixed then fresh used prefixed else prefixed
                    ids[value] <- other
                    element.Put(PdfName.ID, PdfString other) |> ignore
        if ids.Count > 0 then
            let renameHeaders (attributes: PdfDictionary) =
                match attributes.GetAsArray(PdfName "Headers") with
                | null -> ()
                | headers ->
                    for i in 0 .. headers.Size() - 1 do
                        match headers.GetAsString i with
                        | NonNull header when ids.ContainsKey(header.ToUnicodeString()) -> headers.Set(i, PdfString ids[header.ToUnicodeString()]) |> ignore
                        | _ -> ()
            for element in all do
                match element.Get PdfName.A with
                | :? PdfDictionary as attributes -> renameHeaders attributes
                | :? PdfArray as list ->
                    for i in 0 .. list.Size() - 1 do
                        match list.Get i with
                        | :? PdfDictionary as attributes -> renameHeaders attributes
                        | _ -> ()
                | _ -> ()
            notes.Add $"Renamed {ids.Count} element ID(s) in {name}: an earlier file has the same ones."
        for element in all do
            match element.GetAsString PdfName.ID with
            | null -> ()
            | id -> taken.Ids.Add(id.ToUnicodeString()) |> ignore

        // Named destinations an input before has, and what goes to them: links, other actions,
        // the outline.
        let destinations = document.GetCatalog().GetNameTree PdfName.Dests
        // Each name as text, and its key as the file has it (its bytes, which text written
        // again may not match).
        let keys = destinations.GetNames().Keys |> Seq.map (fun key -> key.ToUnicodeString(), key) |> Map.ofSeq
        let names = keys |> Map.keys |> Set.ofSeq
        let destRenames = Dictionary<string, string>()
        for destination in names do
            if taken.Destinations.Contains destination then
                destRenames[destination] <- fresh (fun d -> taken.Destinations.Contains d || names.Contains d || destRenames.ContainsValue d) destination
        if destRenames.Count > 0 then
            for KeyValue(old, renamed) in destRenames do
                let value = destinations.GetNames()[keys[old]]
                destinations.RemoveEntry keys[old]
                destinations.AddEntry(textString renamed, value)
            let renameDest (holder: PdfDictionary) (key: PdfName) =
                match holder.Get key with
                | :? PdfString as d when destRenames.ContainsKey(d.ToUnicodeString()) -> holder.Put(key, textString destRenames[d.ToUnicodeString()]) |> ignore
                | :? PdfName as d when destRenames.ContainsKey(d.GetValue()) -> holder.Put(key, textString destRenames[d.GetValue()]) |> ignore
                | _ -> ()
            let rec renameAction (action: PdfDictionary | null) depth =
                match action with
                | NonNull action when depth < 32 ->
                    renameDest action PdfName.D
                    match action.Get PdfName.Next with
                    | :? PdfDictionary as next -> renameAction next (depth + 1)
                    | :? PdfArray as nexts ->
                        for i in 0 .. nexts.Size() - 1 do
                            renameAction (nexts.GetAsDictionary i) (depth + 1)
                    | _ -> ()
                | _ -> ()
            // The actions for events (/AA: a page opening, an annotation entered, the document
            // closing…), by event.
            let renameEvents (holder: PdfDictionary) =
                match holder.GetAsDictionary PdfName.AA with
                | null -> ()
                | events ->
                    for key in events.KeySet() do
                        renameAction (events.GetAsDictionary key) 0
            let renameIn (holder: PdfDictionary) =
                renameDest holder PdfName.Dest
                renameAction (holder.GetAsDictionary PdfName.A) 0
                renameEvents holder
            for page in 1 .. document.GetNumberOfPages() do
                renameEvents (document.GetPage(page).GetPdfObject())
                for annotation in document.GetPage(page).GetAnnotations() do
                    renameIn (annotation.GetPdfObject())
            renameAction (document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.OpenAction) 0
            renameEvents (document.GetCatalog().GetPdfObject())
            let rec renameOutlines (item: PdfDictionary | null) depth =
                match item with
                | NonNull item when depth < 10000 ->
                    renameIn item
                    renameOutlines (item.GetAsDictionary PdfName.First) (depth + 1)
                    renameOutlines (item.GetAsDictionary PdfName.Next) (depth + 1)
                | _ -> ()
            match document.GetCatalog().GetPdfObject().GetAsDictionary PdfName.Outlines with
            | null -> ()
            | outlines -> renameOutlines (outlines.GetAsDictionary PdfName.First) 0
            document.GetCatalog().GetPdfObject().GetAsDictionary(PdfName.Names) |> Option.ofObj |> Option.iter (fun names -> names.SetModified() |> ignore)
            notes.Add $"Renamed {destRenames.Count} named destination(s) in {name}: an earlier file has the same names."
        for destination in names do
            taken.Destinations.Add(if destRenames.ContainsKey destination then destRenames[destination] else destination) |> ignore

        // Its part's elements, marked to find after joining; and the language its Document
        // element gave it.
        let source, documentElement = partSource root
        for element in source do
            element.Put(partKey, PdfNumber index) |> ignore
        documentElement
        |> Option.bind (fun element -> match element.GetAsString PdfName.Lang with null -> None | lang -> text (lang.ToUnicodeString()))
        |> Option.iter (fun lang -> language <- Some lang)
    { Input = input
      Bytes = stream.ToArray()
      Language = language
      RenamedTags = renamedTags
      Notes = List.ofSeq notes }

// ---- Joining ----

/// A part of the joined file: the input it came from, where its pages are, and its tags renamed.
type Part =
    { File: string
      FirstPage: int
      Pages: int
      /// Custom tags renamed in it: old name -> new.
      RenamedTags: Map<string, string> }

/// What join wrote.
type Joined =
    { Output: string
      Profile: PdfProfile
      Title: string
      Language: string option
      Parts: Part list
      /// What was renamed, and what wasn't carried over.
      Notes: string list }

/// The joined file's outline: an entry for each part, going to its first page, with the input's
/// own outline (its top entries, counted per part) below it.
let private outlineParts (document: PdfDocument) (profile: PdfProfile) (parts: (string * int * int * PdfDictionary option) list) =
    let catalog = document.GetCatalog().GetPdfObject()
    let outlines =
        match catalog.GetAsDictionary PdfName.Outlines with
        | null ->
            let outlines = PdfDictionary()
            outlines.Put(PdfName.Type, PdfName.Outlines) |> ignore
            outlines.MakeIndirect document |> ignore
            catalog.Put(PdfName.Outlines, outlines) |> ignore
            outlines
        | outlines -> outlines
    let tops =
        let rec chain (item: PdfDictionary | null) acc =
            match item with
            | NonNull item when List.length acc < 100000 -> chain (item.GetAsDictionary PdfName.Next) (item :: acc)
            | _ -> List.rev acc
        chain (outlines.GetAsDictionary PdfName.First) []
    // How many entries an open item shows below it.
    let visible (item: PdfDictionary) =
        match item.GetAsNumber PdfName.Count with
        | NonNull count when count.IntValue() > 0 -> count.IntValue()
        | _ -> 0
    let link (parent: PdfDictionary) (items: PdfDictionary list) =
        items
        |> List.iteri (fun i item ->
            item.Put(PdfName.Parent, parent) |> ignore
            item.Remove PdfName.Prev |> ignore
            item.Remove PdfName.Next |> ignore
            if i > 0 then item.Put(PdfName.Prev, items[i - 1]) |> ignore
            if i < items.Length - 1 then item.Put(PdfName.Next, items[i + 1]) |> ignore)
        match items with
        | [] ->
            parent.Remove PdfName.First |> ignore
            parent.Remove PdfName.Last |> ignore
            parent.Remove PdfName.Count |> ignore
        | _ ->
            parent.Put(PdfName.First, items.Head) |> ignore
            parent.Put(PdfName.Last, List.last items) |> ignore
            parent.Put(PdfName.Count, PdfNumber(items |> List.sumBy (fun item -> 1 + visible item))) |> ignore
    let mutable remaining = tops
    let entries =
        [ for title, firstPage, count, element in parts ->
              let own = List.truncate count remaining
              remaining <- List.skip own.Length remaining
              let entry = PdfDictionary()
              entry.Put(PdfName.Title, textString title) |> ignore
              let destination = PdfArray([ document.GetPage(firstPage).GetPdfObject() :> PdfObject; PdfName.Fit ] |> ResizeArray)
              match profile, element with
              | PdfProfile.PdfUa2, Some element ->
                  // PDF/UA-2 asks for a structure destination: the part itself, with the page as
                  // the fallback for readers before PDF 2.0.
                  let action = PdfDictionary()
                  action.Put(PdfName.S, PdfName.GoTo) |> ignore
                  action.Put(PdfName.D, destination) |> ignore
                  action.Put(PdfName.SD, PdfArray([ element :> PdfObject; PdfName.Fit ] |> ResizeArray)) |> ignore
                  entry.Put(PdfName.A, action) |> ignore
              | _ -> entry.Put(PdfName.Dest, destination) |> ignore
              entry.MakeIndirect document |> ignore
              link entry own
              entry ]
    // (Any left over, which shouldn't be, stay at the top after the parts.)
    link outlines (entries @ remaining)

// ---- After the merge ----
//
// iText rebuilds the parent tree from what it registered as it opened a tagged file whenever it
// saves one, and the merge leaves what it registered of the content tagged inside Form XObjects
// wrong (see rebuildParentTree). So the joined file is finished with its structure tree hidden from
// iText: under a key of the catalog's own, the same length as /StructTreeRoot so it can be renamed
// in the merge's output where it's written, and put back after.

let private hiddenRoot = PdfName "LBYL_StrucTree"

/// The merge's output with its catalog's /StructTreeRoot renamed to hiddenRoot, in the catalog
/// object only: found by where the file's cross-reference table says it is, and not in an object
/// stream (the merge writes it as it is). Its "/StructTreeRoot n 0 R" keeps its length, so every
/// other object stays where the table says it is.
let private hideStructure (bytes: byte array) =
    let catalog =
        use document = new PdfDocument(new PdfReader(new MemoryStream(bytes)))
        nonNull (document.GetCatalog().GetPdfObject().GetIndirectReference())
    let text = Encoding.Latin1.GetString bytes
    let start = int (catalog.GetOffset())
    let finish = if catalog.GetObjStreamNumber() = 0 && start > 0 then text.IndexOf("endobj", start, StringComparison.Ordinal) else -1
    let entry =
        if finish < 0 then null
        else RegularExpressions.Regex(@"/StructTreeRoot(?=\s+\d+\s+\d+\s+R)").Match(text, start, finish - start)
    match entry with
    | NonNull entry when entry.Success ->
        let renamed = Array.copy bytes
        Encoding.Latin1.GetBytes("/" + hiddenRoot.GetValue()).CopyTo(renamed, entry.Index)
        renamed
    | _ -> invalidOp "The joined file's structure tree couldn't be found to finish it."

/// The structure tree's root, hidden from iText (see hideStructure).
let private structRoot (document: PdfDocument) = document.GetCatalog().GetPdfObject().GetAsDictionary hiddenRoot |> nonNull

/// The parent tree, rebuilt from the structure tree. The merge copies a Form XObject's
/// /StructParents (the parent tree entry its own marked content, an MCR to its stream, is found
/// by) as it was in its file, where in the joined file it's another's number: the XObject gets no
/// entry, and the one it names may be overwritten. So each page with marked content, each tagged
/// annotation (by /OBJR) and each such XObject gets a number of its own, in that order, and its
/// entry: for content, the elements it belongs to by MCID; for an annotation, its element.
let private rebuildParentTree (document: PdfDocument) =
    let root = structRoot document
    let content = Dictionary<PdfDictionary, SortedDictionary<int, PdfDictionary>>(HashIdentity.Reference)
    let annotations = List<PdfDictionary * PdfDictionary>()
    let add (holder: PdfDictionary) (mcid: int) (element: PdfDictionary) =
        if not (content.ContainsKey holder) then content[holder] <- SortedDictionary()
        content[holder][mcid] <- element
    let seen = HashSet<PdfDictionary>(HashIdentity.Reference)
    let rec visit (element: PdfDictionary) (page: PdfDictionary | null) depth =
        if depth < 512 && seen.Add element then
            let page = match element.GetAsDictionary PdfName.Pg with null -> page | own -> own
            let kid (kid: PdfObject | null) =
                match kid with
                | :? PdfNumber as mcid ->
                    match page with
                    | NonNull page -> add page (mcid.IntValue()) element
                    | Null -> ()
                | :? PdfDictionary as child when not (isNull (child.GetAsName PdfName.S)) -> visit child page (depth + 1)
                | :? PdfDictionary as reference ->
                    match reference.GetAsNumber PdfName.MCID, reference.GetAsStream PdfName.Stm, reference.GetAsDictionary PdfName.Obj with
                    | NonNull mcid, NonNull stream, _ -> add stream (mcid.IntValue()) element
                    | NonNull mcid, Null, _ ->
                        match reference.GetAsDictionary PdfName.Pg |> Option.ofObj |> Option.orElse (Option.ofObj page) with
                        | Some page -> add page (mcid.IntValue()) element
                        | None -> ()
                    | Null, _, NonNull annotation -> annotations.Add(annotation, element)
                    | _ -> ()
                | _ -> ()
            match element.Get PdfName.K with
            | :? PdfArray as kids ->
                for i in 0 .. kids.Size() - 1 do
                    kid (kids.Get i)
            | only -> kid only
    for top in kidElements root do
        visit top null 0
    let nums = PdfArray()
    let mutable next = 0
    let entry (value: PdfObject) =
        nums.Add(PdfNumber next)
        nums.Add value
        next <- next + 1
        next - 1
    let byMcid (elements: SortedDictionary<int, PdfDictionary>) =
        let array = PdfArray()
        for mcid in 0 .. Seq.max elements.Keys do
            match elements.TryGetValue mcid with
            | true, element -> array.Add element
            | _ -> array.Add PdfNull.PDF_NULL
        array.MakeIndirect document
    for number in 1 .. document.GetNumberOfPages() do
        let page = document.GetPage(number).GetPdfObject()
        match content.TryGetValue page with
        | true, elements -> page.Put(PdfName.StructParents, PdfNumber(entry (byMcid elements))) |> ignore
        | _ -> page.Remove PdfName.StructParents |> ignore
    for annotation, element in annotations do
        annotation.Put(PdfName.StructParent, PdfNumber(entry element)) |> ignore
    for KeyValue(holder, elements) in content do
        if holder :? PdfStream then
            holder.Put(PdfName.StructParents, PdfNumber(entry (byMcid elements))) |> ignore
    let tree = PdfDictionary()
    tree.Put(PdfName.Nums, nums) |> ignore
    tree.MakeIndirect document |> ignore
    root.Put(PdfName.ParentTree, tree) |> ignore
    root.Put(PdfName.ParentTreeNextKey, PdfNumber next) |> ignore

/// Each part's elements, marked by prepare, moved into an element of their own (tag) under the
/// output's Document, with its language where it differs from the document's.
let private wrapParts (document: PdfDocument) (tag: string) (languages: string option list) (outputLanguage: string option) =
    let root = structRoot document
    let all = elements root
    let marked =
        all
        |> List.choose (fun element ->
            match element.GetAsNumber partKey with
            | null -> None
            | index -> Some(index.IntValue(), element))
    // Where the parts go: the element the marked ones are in (the Document the merge made), or a
    // new Document if they're straight under the root.
    let container, created =
        match marked with
        | (_, first) :: _ ->
            match first.GetAsDictionary PdfName.P with
            | NonNull parent when not (isNull (parent.GetAsName PdfName.S)) -> parent, false
            | _ ->
                let documentElement = PdfDictionary()
                documentElement.Put(PdfName.Type, PdfName.StructElem) |> ignore
                documentElement.Put(PdfName.S, PdfName.Document) |> ignore
                documentElement.Put(PdfName.P, root) |> ignore
                documentElement.Put(PdfName.K, root.Get(PdfName.K, false)) |> ignore
                documentElement.MakeIndirect document |> ignore
                documentElement, true
        | [] -> root, false
    // The merge keeps the first file's Document element as the joined file's, with its /Lang: the
    // document's language is the catalog's, and each part says its own.
    if not created then container.Remove PdfName.Lang |> ignore
    let namespace' = container.Get PdfName.NS
    let partOf =
        marked
        |> List.groupBy fst
        |> List.map (fun (index, items) ->
            let part = PdfDictionary()
            part.Put(PdfName.Type, PdfName.StructElem) |> ignore
            part.Put(PdfName.S, PdfName tag) |> ignore
            part.Put(PdfName.P, container) |> ignore
            match namespace' with
            | NonNull ns -> part.Put(PdfName.NS, ns) |> ignore
            | Null -> ()
            match List.tryItem index languages |> Option.flatten with
            | Some lang when Some lang <> outputLanguage -> part.Put(PdfName.Lang, textString lang) |> ignore
            | _ -> ()
            let kids = PdfArray()
            for _, element in items do
                element.Remove partKey |> ignore
                element.Put(PdfName.P, part) |> ignore
                kids.Add element
            part.Put(PdfName.K, kids) |> ignore
            part.MakeIndirect document |> ignore
            index, (part, items |> List.map snd))
    let partFor = Dictionary<PdfDictionary, PdfDictionary>(HashIdentity.Reference)
    for _, (part, items) in partOf do
        for item in items do
            partFor[item] <- part
    let find (element: PdfDictionary) = match partFor.TryGetValue element with | true, part -> Some part | _ -> None
    // The container's kids, each part in place of its first element and the rest left out.
    let kids = PdfArray()
    let added = HashSet<PdfDictionary>(HashIdentity.Reference)
    let existing =
        match container.Get PdfName.K with
        | :? PdfArray as array -> [ for i in 0 .. array.Size() - 1 -> array.Get i ]
        | NonNull kid -> [ kid ]
        | Null -> []
    for kid in existing do
        match kid with
        | :? PdfDictionary as element ->
            match find element with
            | Some part -> if added.Add part then kids.Add part
            | None -> kids.Add kid
        | other -> kids.Add other
    // (Parts whose elements weren't the container's kids go at the end.)
    for _, (part, _) in partOf do
        if added.Add part then kids.Add part
    container.Put(PdfName.K, kids) |> ignore
    if created then
        root.Put(PdfName.K, container) |> ignore
    partOf |> List.map (fun (index, (part, _)) -> index, part) |> Map.ofList

/// The role map, class map, ID tree and namespaces of all the inputs, on the joined file's root.
let private combine (document: PdfDocument) (roleMap: Dictionary<string, string>) (prepared: Prepared list) =
    let root = structRoot document
    if roleMap.Count > 0 then
        let map = PdfDictionary()
        for KeyValue(tag, role) in roleMap do
            map.Put(PdfName tag, PdfName role) |> ignore
        root.Put(PdfName.RoleMap, map) |> ignore
    let classMap = PdfDictionary()
    for input in prepared do
        use source = new PdfDocument(new PdfReader(new MemoryStream(input.Bytes)))
        match source.GetStructTreeRoot().GetPdfObject().GetAsDictionary(PdfName "ClassMap") with
        | null -> ()
        | classes ->
            for key in classes.KeySet() do
                if not (classMap.ContainsKey key) then
                    classMap.Put(key, classes.Get(key).CopyTo(document)) |> ignore
    if classMap.Size() > 0 then root.Put(PdfName "ClassMap", classMap) |> ignore else root.Remove(PdfName "ClassMap") |> ignore
    let all = elements root
    let ids =
        all
        |> List.choose (fun element -> match element.GetAsString PdfName.ID with null -> None | id -> Some(id, element))
        |> List.distinctBy (fun (id, _) -> id.ToUnicodeString())
        |> List.sortWith (fun (a, _) (b, _) ->
            let a, b = a.GetValueBytes(), b.GetValueBytes()
            let rec compare i =
                if i >= a.Length || i >= b.Length then a.Length - b.Length
                elif a[i] <> b[i] then int a[i] - int b[i]
                else compare (i + 1)
            compare 0)
    if ids.IsEmpty then
        root.Remove(PdfName "IDTree") |> ignore
    else
        let names = PdfArray()
        for id, element in ids do
            names.Add(PdfString(id.GetValueBytes()))
            names.Add element
        let tree = PdfDictionary()
        tree.Put(PdfName.Names, names) |> ignore
        root.Put(PdfName "IDTree", tree) |> ignore
    let namespaces =
        all
        |> List.choose (fun element -> match element.GetAsDictionary PdfName.NS with null -> None | ns -> Some ns)
        |> List.distinctBy (fun ns -> match ns.GetIndirectReference() with null -> System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode ns | ref' -> ref'.GetObjNumber())
    if not namespaces.IsEmpty then
        let array = PdfArray()
        for ns in namespaces do
            array.Add ns
        root.Put(PdfName.Namespaces, array) |> ignore

/// The named destinations the merge doesn't carry: it copies those a link or outline entry in the
/// file goes to, not those only links from elsewhere use (file.pdf#nameddest=chapter). Each goes
/// to its page in the joined file, as it's placed there; prepare has renamed those that clash.
let private carryDestinations (document: PdfDocument) (prepared: Prepared list) (parts: Part list) =
    let tree = document.GetCatalog().GetNameTree PdfName.Dests
    let have = HashSet<string>(tree.GetNames().Keys |> Seq.map _.ToUnicodeString())
    for input, part in List.zip prepared parts do
        use source = new PdfDocument(new PdfReader(new MemoryStream(input.Bytes)))
        for KeyValue(key, value) in List.ofSeq (source.GetCatalog().GetNameTree(PdfName.Dests).GetNames()) do
            let destination: PdfArray | null =
                match value with
                | :? PdfArray as explicitDestination -> explicitDestination
                | :? PdfDictionary as holder -> holder.GetAsArray PdfName.D
                | _ -> null
            match destination with
            | NonNull destination when destination.Size() >= 1 && not (have.Contains(key.ToUnicodeString())) ->
                match destination.GetAsDictionary 0 with
                | NonNull page when source.GetPageNumber page >= 1 ->
                    let carried = PdfArray()
                    carried.Add(document.GetPage(part.FirstPage + source.GetPageNumber page - 1).GetPdfObject())
                    // The rest (/XYZ left top zoom, /Fit…) is names, numbers and nulls.
                    for i in 1 .. destination.Size() - 1 do
                        carried.Add(destination.Get i)
                    tree.AddEntry(PdfString(key.GetValueBytes()), carried)
                    have.Add(key.ToUnicodeString()) |> ignore
                | _ -> ()
            | _ -> ()

/// The document level: its language, that it's tagged, its title (shown in place of the file
/// name) and its PDF/UA claim.
let private documentLevel (document: PdfDocument) (profile: PdfProfile) (title: string) (language: string option) =
    let catalog = document.GetCatalog().GetPdfObject()
    match language with
    | Some lang -> catalog.Put(PdfName.Lang, textString lang) |> ignore
    | None -> ()
    let markInfo =
        match catalog.GetAsDictionary PdfName.MarkInfo with
        | null ->
            let markInfo = PdfDictionary()
            catalog.Put(PdfName.MarkInfo, markInfo) |> ignore
            markInfo
        | markInfo -> markInfo
    markInfo.Put(PdfName.Marked, PdfBoolean.TRUE) |> ignore
    let preferences =
        match catalog.GetAsDictionary PdfName.ViewerPreferences with
        | null ->
            let preferences = PdfDictionary()
            catalog.Put(PdfName.ViewerPreferences, preferences) |> ignore
            preferences
        | preferences -> preferences
    preferences.Put(PdfName.DisplayDocTitle, PdfBoolean.TRUE) |> ignore
    document.GetDocumentInfo().SetTitle title |> ignore
    let xmp = XMPMetaFactory.Create()
    xmp.SetLocalizedText(XMPConst.NS_DC, "title", null, XMPConst.X_DEFAULT, title)
    language
    |> Option.iter (fun lang ->
        xmp.AppendArrayItem(XMPConst.NS_DC, "language", Options.PropertyOptions(Options.PropertyOptions.ARRAY), lang, null))
    xmp.SetProperty(XMPConst.NS_PDF, "Producer", document.GetDocumentInfo().GetProducer())
    match profile with
    | PdfProfile.PdfUa2 ->
        xmp.SetProperty(XMPConst.NS_PDFUA_ID, "part", "2")
        xmp.SetProperty(XMPConst.NS_PDFUA_ID, "rev", MetadataTransfer.PdfUa2Revision)
    | _ -> xmp.SetProperty(XMPConst.NS_PDFUA_ID, "part", "1")
    document.SetXmpMetadata xmp

/// Joins files, in order, into output, as the module's comment says. Call refusals first: this
/// assumes there are none. options.PartTag must be one of partTags.
let join (files: string list) (output: string) (options: Options) : Joined =
    let inputs =
        files |> List.map (fun file -> match read file with Ok input -> input | Error refusals -> invalidArg (nameof files) refusals.Head.Reason)
    let version = inputs |> List.map _.Version |> List.reduce (fun a b -> if a.CompareTo b >= 0 then a else b)
    let profile = profileOf version
    let taken =
        { RoleMap = Dictionary<string, string>()
          Classes = Dictionary<string, string>()
          Ids = HashSet<string>()
          Destinations = HashSet<string>() }
    let prepared = inputs |> List.mapi (fun index input -> prepare taken index input)
    let title = options.Title |> Option.defaultValue inputs.Head.Title
    let language = options.Language |> Option.orElse prepared.Head.Language
    let tag = options.PartTag |> Option.defaultValue (partTags profile).Head

    // The merge: pages, annotations, tags and outlines, counting each input's top outline entries.
    let merged, outlineCounts =
        use stream = new MemoryStream()
        let writer = new PdfWriter(stream, WriterProperties().SetPdfVersion version)
        writer.SetCloseStream false
        let document = new PdfDocument(writer)
        document.SetTagged() |> ignore
        let merger = PdfMerger(document, PdfMergerProperties().SetMergeTags(true).SetMergeOutlines(true))
        let counts = List<int>()
        try
            for input in prepared do
                use source = new PdfDocument(new PdfReader(new MemoryStream(input.Bytes)))
                let before = document.GetOutlines(false).GetAllChildren().Count
                merger.Merge(source, 1, source.GetNumberOfPages()) |> ignore
                counts.Add(document.GetOutlines(false).GetAllChildren().Count - before)
        finally
            merger.Close()
        stream.ToArray(), List.ofSeq counts

    // Then what the merge doesn't do.
    let parts =
        prepared
        |> List.mapi (fun index input -> index, input)
        |> List.fold
            (fun (acc, first) (_, input) ->
                { File = input.Input.File; FirstPage = first; Pages = input.Input.Pages; RenamedTags = input.RenamedTags } :: acc,
                first + input.Input.Pages)
            ([], 1)
        |> fst
        |> List.rev
    do
        use document = new PdfDocument(new PdfReader(new MemoryStream(hideStructure merged)), new PdfWriter(output))
        let elements = wrapParts document tag (prepared |> List.map _.Language) language
        combine document taken.RoleMap prepared
        carryDestinations document prepared parts
        outlineParts
            document
            profile
            (List.map3 (fun (input: Prepared) (part: Part) count -> input.Input.Title, part.FirstPage, count) prepared parts outlineCounts
             |> List.mapi (fun index (title, first, count) -> title, first, count, Map.tryFind index elements))
        documentLevel document profile title language
        rebuildParentTree document
        let catalog = document.GetCatalog().GetPdfObject()
        catalog.Put(PdfName.StructTreeRoot, structRoot document) |> ignore
        catalog.Remove hiddenRoot |> ignore
    { Output = output
      Profile = profile
      Title = title
      Language = language
      Parts = parts
      Notes =
        [ for input in prepared do
              yield! input.Notes
              if input.Input.HasReview then
                  yield $"The review saved in {Path.GetFileName input.Input.File} wasn't carried over: its edits name pages by number." ] }

// ---- Checking the joined file's structure ----

/// How an input's structure differs from its part of the joined file's: by element, in reading
/// order, what each says (its tag, text, alt text, replacement text, link) and its page.
let differences (part: Part) (index: int) (input: StructureNode) (joined: StructureNode) : string list =
    let name = Path.GetFileName part.File
    // The elements, not the content outside them the reader adds (see UntaggedContent.nodes).
    let children (node: StructureNode) = node.Children |> Seq.filter (fun child -> child.Tag <> "NotInTags") |> List.ofSeq
    // An input's part: its Document's children, else its root's.
    let ownParts (root: StructureNode) =
        match children root with
        | [ only ] when only.Tag = "Document" -> children only
        | tops -> tops
    let joinedPart =
        match ownParts joined |> List.tryItem index with
        | Some part -> children part
        | None -> []
    let found = List<string>()
    let say (path: string) (what: string) (was: string | null) (is: string | null) =
        let quoted (value: string | null) = match value with NonNull value -> "\"" + value + "\"" | Null -> "none"
        if found.Count < 20 then
            found.Add $"{name}: {path}: {what} was {quoted was}, is {quoted is} in the joined file."
    let renamed (tag: string) = part.RenamedTags |> Map.tryFind tag |> Option.defaultValue tag
    let rec compare (path: string) (inputs: StructureNode list) (outputs: StructureNode list) =
        if inputs.Length <> outputs.Length then
            say path "the number of elements" (string inputs.Length) (string outputs.Length)
        for i, (a, b) in List.zip (List.truncate outputs.Length inputs) (List.truncate inputs.Length outputs) |> List.indexed do
            let here = $"{path} › <{b.Tag}> {i + 1}"
            let check what (x: string | null) (y: string | null) = if not (String.Equals(x, y)) then say here what x y
            check "the tag" (renamed a.Tag) b.Tag
            check "the role" (match a.Role with NonNull role -> renamed role | Null -> null) b.Role
            check "the text" a.Content b.Content
            check "the alt text" a.AlternateDescription b.AlternateDescription
            check "the replacement text" a.ActualText b.ActualText
            check "the link" a.LinkUri b.LinkUri
            check "the link's description" a.LinkDescription b.LinkDescription
            let page (number: Nullable<int>) (offset: int) : string | null = if number.HasValue then string (number.Value + offset) else null
            check "the page" (page a.PageNumber (part.FirstPage - 1)) (page b.PageNumber 0)
            compare here (children a) (children b)
    compare $"part {index + 1}" (ownParts input) joinedPart
    List.ofSeq found

// ---- Validating before and after ----

/// What appending did: why it didn't (Refusals), else what it wrote (Joined) and how that checks
/// out (After, and Differences from the inputs' structures).
type Outcome =
    { Refusals: Refusal list
      /// Each input's validation, once they're all readable (empty before that).
      Inputs: (string * ValidationReport) list
      Joined: Joined option
      After: ValidationReport option
      /// Where a part's structure doesn't read as its input's did (see differences).
      Differences: string list }

    /// Whether the joined file was written and passes: PDF/UA, and its structure as the inputs'.
    member this.Passes =
        this.Joined.IsSome && (this.After |> Option.exists _.IsCompliant) && this.Differences.IsEmpty

/// Appends files into output: refuses (writing nothing) if they can't be joined or any fails
/// PDF/UA; else joins them and checks the result with veraPDF and Legibyl's warnings, and each
/// part's structure against its input's. stage is told what it's doing, as it goes.
let run (validator: VeraPdfValidator) (files: string list) (output: string) (options: Options) (stage: string -> unit) : Outcome =
    let none = { Refusals = []; Inputs = []; Joined = None; After = None; Differences = [] }
    stage "Reading the files…"
    // As the app's other path checks: case matters on Linux only.
    let comparison = if OperatingSystem.IsLinux() then StringComparison.Ordinal else StringComparison.OrdinalIgnoreCase
    let same (a: string) (b: string) = String.Equals(Path.GetFullPath a, Path.GetFullPath b, comparison)
    let overwritten =
        [ for file in files |> List.distinct do
              if same file output then
                  { File = file; Reason = "The joined file would be written over it: save the joined file somewhere else." } ]
    match overwritten @ refusals files with
    | _ :: _ as refused -> { none with Refusals = refused }
    | [] ->
        let checks =
            [ for file in files ->
                  stage $"Validating {Path.GetFileName file}…"
                  let profile, _ = PdfUaTarget.detect file
                  file,
                  (try Ok(validator.Validate(file, profile)) with :? VeraPdfValidationException as ex -> Error ex.Message) ]
        let failing =
            [ for file, check in checks do
                  match check with
                  | Error message -> { File = file; Reason = $"It couldn't be validated: {message}" }
                  | Ok report when not report.IsCompliant ->
                      let failed = if report.FailedCount = 1 then "1 check fails" else $"{report.FailedCount} checks fail"
                      { File = file; Reason = $"It doesn't pass {PdfUaTarget.displayName report.Profile}: {failed}." }
                  | Ok _ -> () ]
        let inputs = checks |> List.choose (fun (file, check) -> check |> Result.toOption |> Option.map (fun report -> file, report))
        if not failing.IsEmpty then
            { none with Refusals = failing; Inputs = inputs }
        else
            stage "Joining…"
            // Joined and checked beside output, which it then replaces: a file already there
            // stays as it was if anything goes wrong on the way.
            let folder = Path.GetDirectoryName(Path.GetFullPath output) |> nonNull
            let scratch = Path.Combine(folder, $".{Path.GetFileNameWithoutExtension output}.{Guid.NewGuid():N}.pdf")
            try
                let joined = join files scratch options
                stage $"Validating {Path.GetFileName output}…"
                let after = validator.Validate(scratch, joined.Profile)
                stage "Comparing the structure with the files'…"
                let differences =
                    match validator.ReadStructureTree scratch with
                    | Null -> [ "The joined file has no structure tree." ]
                    | NonNull tree ->
                        joined.Parts
                        |> List.indexed
                        |> List.collect (fun (index, part) ->
                            match validator.ReadStructureTree part.File with
                            | NonNull input -> differences part index input tree
                            | Null -> [])
                File.Move(scratch, output, true)
                { none with
                    Inputs = inputs
                    Joined = Some { joined with Output = output }
                    After = Some { after with FilePath = output }
                    Differences = differences }
            finally
                if File.Exists scratch then File.Delete scratch
