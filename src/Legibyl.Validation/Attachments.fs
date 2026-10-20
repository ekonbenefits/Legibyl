/// The files attached to a PDF, and its media clips (a video or a sound a Screen annotation
/// plays), which aren't in the page's tags, so have no element to describe: what PDF/UA asks of
/// each, and edits that give it. An attached file needs its file name, as /F and /UF (7.11), and
/// a description, /Desc (PDF/UA-2's 8.14.1); a media clip its content type, /CT, and a
/// description, /Alt (7.18.6.2).
module Legibyl.Validation.Attachments

open System
open System.Text.Json.Serialization
open iText.IO.Font
open iText.Kernel.Pdf

/// What an attachment is: a file attached to the PDF, or a media clip.
type Kind =
    | File
    | Media

/// An attached file or media clip, as found: what an edit is for it by (Key: where it is, so no
/// two have one: an attached file in the PDF's list of them by its name there, one on a page, or
/// a media clip, by its annotation's place on its page), what it's called, what it is, and what
/// it has.
type Attachment =
    { Key: string
      /// What it's called: an attached file's name in the PDF's list, else its file name; a media
      /// clip's name, else its file's.
      Name: string
      Kind: Kind
      /// Its file name: /UF, else /F (an attached file's, or a media clip's data's).
      FileName: string option
      /// Whether it has the file names PDF/UA asks for (an attached file's /F and /UF).
      Named: bool
      /// Its description: an attached file's /Desc, a media clip's /Alt.
      Description: string option
      /// A media clip's content type (/CT), e.g. "video/mp4".
      ContentType: string option
      /// Where it is: "page 2", or "the document".
      Where: string }

/// An edit of an attachment, as a review keeps it: what to write (null to leave it).
[<CLIMutable>]
type AttachmentEdit =
    { /// An attached file's file name (its /F and /UF).
      [<JsonPropertyName "fileName">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      FileName: string | null
      /// Its description (an attached file's /Desc, a media clip's /Alt).
      [<JsonPropertyName "description">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      Description: string | null
      /// A media clip's content type (/CT).
      [<JsonPropertyName "contentType">]
      [<JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)>]
      ContentType: string | null }

let private text (dictionary: PdfDictionary) (key: PdfName) =
    match dictionary.GetAsString key with
    | NonNull value when not (String.IsNullOrWhiteSpace(value.ToUnicodeString())) -> Some(value.ToUnicodeString())
    | _ -> None

/// A file specification's file name: /UF, else /F (a string, or a dictionary's).
let private fileNameOf (spec: PdfObject | null) =
    match spec with
    | :? PdfDictionary as spec -> text spec PdfName.UF |> Option.orElse (text spec PdfName.F)
    | :? PdfString as name when name.ToUnicodeString() <> "" -> Some(name.ToUnicodeString())
    | _ -> None

/// A media clip's description: the text of its /Alt (pairs of a language and a text).
let private altOf (clip: PdfDictionary) =
    match clip.GetAsArray PdfName.Alt with
    | NonNull pairs ->
        [ for i in 1..2 .. pairs.Size() - 1 do
              match pairs.GetAsString i with
              | NonNull value when value.ToUnicodeString() <> "" -> value.ToUnicodeString()
              | _ -> () ]
        |> List.tryHead
    | Null -> None

/// The attached files (in the PDF's list of them, and on pages' file attachment annotations) and
/// media clips (played by pages' Screen annotations) in document, each with its dictionary.
let private findIn (document: PdfDocument) : (Attachment * PdfDictionary) list =
    let seen = Collections.Generic.HashSet<PdfDictionary>(HashIdentity.Reference)
    let file (key: string) (name: string) (where: string) (spec: PdfDictionary) =
        if seen.Add spec then
            Some(
                { Key = key
                  Name = name
                  Kind = File
                  FileName = fileNameOf spec
                  Named = (text spec PdfName.F).IsSome && (text spec PdfName.UF).IsSome
                  Description = text spec PdfName.Desc
                  ContentType = None
                  Where = where },
                spec
            )
        else
            None
    let listed =
        [ for KeyValue(name, spec) in document.GetCatalog().GetNameTree(PdfName.EmbeddedFiles).GetNames() do
              match spec with
              | :? PdfIndirectReference as reference ->
                  match reference.GetRefersTo() with
                  | :? PdfDictionary as spec -> yield! Option.toList (file $"the document: {name.ToUnicodeString()}" (name.ToUnicodeString()) "the document" spec)
                  | _ -> ()
              | :? PdfDictionary as spec -> yield! Option.toList (file $"the document: {name.ToUnicodeString()}" (name.ToUnicodeString()) "the document" spec)
              | _ -> () ]
    let onPages =
        [ for number in 1 .. document.GetNumberOfPages() do
              let where = $"page {number}"
              for i, annotation in Seq.indexed (document.GetPage(number).GetAnnotations()) do
                  let dictionary = annotation.GetPdfObject()
                  match dictionary.GetAsName PdfName.Subtype with
                  | subtype when PdfName.FileAttachment.Equals subtype ->
                      match dictionary.GetAsDictionary PdfName.FS with
                      | NonNull spec ->
                          let name = fileNameOf spec |> Option.defaultValue $"attachment {i + 1}"
                          yield! Option.toList (file $"{where}, attachment {i + 1}" name where spec)
                      | Null -> ()
                  | subtype when PdfName.Screen.Equals subtype ->
                      // Its rendition actions: its own (/A), those its events start (/AA), and
                      // those either goes on to (/Next).
                      let rec actions (action: PdfObject | null) (depth: int) : PdfDictionary list =
                          match action with
                          | :? PdfDictionary as action when depth < 16 ->
                              action :: actions (action.Get(PdfName.Next, true)) (depth + 1)
                          | :? PdfArray as next when depth < 16 -> [ for j in 0 .. next.Size() - 1 do yield! actions (next.Get(j, true)) (depth + 1) ]
                          | _ -> []
                      let started =
                          actions (dictionary.Get(PdfName.A, true)) 0
                          @ (match dictionary.GetAsDictionary PdfName.AA with
                             | NonNull events -> [ for key in events.KeySet() do yield! actions (events.Get(key, true)) 0 ]
                             | Null -> [])
                      let clips =
                          [ for action in started do
                                match action.GetAsDictionary PdfName.R |> Option.ofObj |> Option.bind (fun r -> Option.ofObj (r.GetAsDictionary PdfName.C)) with
                                | Some clip when seen.Add clip -> clip
                                | _ -> () ]
                      for k, clip in List.indexed clips do
                          let name = text clip PdfName.N
                          let dataName = fileNameOf (clip.Get(PdfName.D, true))
                          let place = if k = 0 then $"{where}, media {i + 1}" else $"{where}, media {i + 1}.{k + 1}"
                          yield
                              { Key = place
                                Name = name |> Option.orElse dataName |> Option.defaultValue $"media {i + 1}"
                                Kind = Media
                                FileName = dataName
                                Named = true
                                Description = altOf clip
                                ContentType = text clip PdfName.CT
                                Where = where },
                              clip
                  | _ -> () ]
    listed @ onPages

/// The attached files and media clips in the PDF at pdfPath.
let list (pdfPath: string) : Attachment list =
    use document = PdfFiles.openToRead pdfPath
    findIn document |> List.map fst

/// What an edit changes on an attachment's dictionary: each key and its new value, where it
/// differs from what it has.
let private changes (attachment: Attachment, dictionary: PdfDictionary) (edit: AttachmentEdit) =
    [ match attachment.Kind, edit.FileName with
      | File, NonNull name when not (String.IsNullOrWhiteSpace name) ->
          if text dictionary PdfName.F <> Some name then PdfName.F, (PdfString name :> PdfObject)
          if text dictionary PdfName.UF <> Some name then PdfName.UF, (PdfString(name, PdfEncodings.UNICODE_BIG) :> PdfObject)
      | _ -> ()
      match attachment.Kind, edit.Description with
      | File, NonNull description when not (String.IsNullOrWhiteSpace description) && text dictionary PdfName.Desc <> Some description ->
          PdfName.Desc, (PdfString(description, PdfEncodings.UNICODE_BIG) :> PdfObject)
      | Media, NonNull description when not (String.IsNullOrWhiteSpace description) && altOf dictionary <> Some description ->
          // A multi-language text: no language (the document's), then the text.
          PdfName.Alt, (PdfArray([| PdfString "" :> PdfObject; PdfString(description, PdfEncodings.UNICODE_BIG) |]) :> PdfObject)
      | _ -> ()
      match attachment.Kind, edit.ContentType with
      | Media, NonNull contentType when not (String.IsNullOrWhiteSpace contentType) && text dictionary PdfName.CT <> Some contentType ->
          PdfName.CT, (PdfString contentType :> PdfObject)
      | _ -> () ]

/// Writes the edits for document's attachments (by their keys); says what it wrote.
let apply (document: PdfDocument) (edits: Map<string, AttachmentEdit>) : string list =
    if edits.IsEmpty then
        []
    else
        [ for attachment, dictionary in findIn document do
              match edits.TryFind attachment.Key with
              | Some edit ->
                  let changed = changes (attachment, dictionary) edit
                  for key, value in changed do
                      dictionary.Put(key, value) |> ignore
                  if not changed.IsEmpty then
                      let what = if attachment.Kind = File then "attached file" else "media clip"
                      yield $"Wrote the {what} \"{attachment.Name}\"'s " + String.Join(" and ", changed |> List.map (fun (key, _) -> key.GetValue()) |> List.distinct) + ", as given."
              | None -> () ]

/// How many of the edits the PDF at pdfPath doesn't have yet.
let pending (pdfPath: string) (edits: Map<string, AttachmentEdit>) : int =
    if edits.IsEmpty then
        0
    else
        use document = PdfFiles.openToRead pdfPath
        findIn document
        |> List.filter (fun (attachment, dictionary) ->
            match edits.TryFind attachment.Key with
            | Some edit -> not (changes (attachment, dictionary) edit).IsEmpty
            | None -> false)
        |> List.length

/// A content type for a file name, by its extension; None for one it doesn't know.
let contentTypeFor (fileName: string) =
    match (string (IO.Path.GetExtension fileName)).ToLowerInvariant() with
    | ".mp4" | ".m4v" -> Some "video/mp4"
    | ".mov" -> Some "video/quicktime"
    | ".webm" -> Some "video/webm"
    | ".avi" -> Some "video/x-msvideo"
    | ".mpg" | ".mpeg" -> Some "video/mpeg"
    | ".mp3" -> Some "audio/mpeg"
    | ".m4a" -> Some "audio/mp4"
    | ".wav" -> Some "audio/wav"
    | ".ogg" -> Some "audio/ogg"
    | ".swf" -> Some "application/x-shockwave-flash"
    | _ -> None
