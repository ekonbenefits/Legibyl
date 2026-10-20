/// Carries veraPDF's metadata fix over into a PDF saved by iText.
///
/// veraPDF saves its fix as an incremental update with a classic xref table, even when the
/// original uses xref streams (and with a /Prev one byte past the original startxref). iText
/// can't follow that chain, so it rebuilds the xref by scanning the file, and on files with
/// object streams or linearization that rebuild loses or mixes up objects, or fails outright.
/// So rather than letting iText read veraPDF's output, this reads the fixed XMP and Info
/// dictionary from it with veraPDF's own parser and writes them into the input through a
/// normal iText save.
module internal Legibyl.Validation.MetadataTransfer

open System
open System.IO
open iText.IO.Font
open iText.Kernel.Pdf
open iText.Kernel.XMP
open org.verapdf.cos
open org.verapdf.pd
open Legibyl.Validation.Interop

type private InfoValue =
    | Text of string
    /// A value of some other type, left as the input has it.
    | Other

let private readAll (stream: java.io.InputStream) =
    use bytes = new MemoryStream()
    let buffer = Array.zeroCreate<byte> 8192
    let mutable read = stream.read buffer
    while read > 0 do
        bytes.Write(buffer, 0, read)
        read <- stream.read buffer
    bytes.ToArray()

/// The fixed XMP packet (decoded) and Info entries, as veraPDF's parser sees them.
let private readFixed (fixedPath: string) =
    let document = PDDocument(fixedPath)
    try
        let xmp =
            match document.getMetadata () with
            | null -> None
            | metadata ->
                let stream = metadata.getCOSStream().getData COSStream.FilterFlags.DECODE
                try
                    Some(readAll stream)
                finally
                    stream.close ()

        let info =
            match document.getDocument().getTrailer().getInfo () with
            | null -> None
            | info when info.getType () <> COSObjType.COS_DICT -> None
            | info ->
                Some(
                    Map
                        [ for key in javaSeq (info.getKeySet ()) do
                              let key = key :?> org.verapdf.``as``.ASAtom
                              let value = info.getKey key
                              key.getValue (),
                              if value.getType () = COSObjType.COS_STRING then Text(value.getString ()) else Other ]
                )

        xmp, info
    finally
        document.close ()

let private textString (value: string) =
    if value |> Seq.forall (fun c -> c < '\u0080') then PdfString value
    else PdfString(value, PdfEncodings.UNICODE_BIG)

/// The PDF/UA-2 identifier's revision: the year of ISO 14289-2's edition.
[<Literal>]
let PdfUa2Revision = "2024"

/// veraPDF writes a PDF/UA-2 identifier with a pdfuaid:rev that isn't the edition's year
/// ("0000", or 2020, PDF 2.0's). This puts in the year of the edition it's checked against,
/// unless it names that or a later one; returns whether it did.
let private revisePdfUa2 (xmp: XMPMeta) =
    let ns = XMPConst.NS_PDFUA_ID
    let names (edition: string | null) =
        match edition with
        | NonNull edition when edition.Length = 4 ->
            match Int32.TryParse edition with
            | true, year -> year >= int PdfUa2Revision
            | _ -> false
        | _ -> false
    if xmp.DoesPropertyExist(ns, "part") && xmp.GetPropertyString(ns, "part") = "2" && not (names (xmp.GetPropertyString(ns, "rev"))) then
        xmp.SetProperty(ns, "rev", PdfUa2Revision)
        true
    else
        false

/// Writes inputPath to outputPath with its XMP metadata and Info dictionary replaced by
/// those in fixedPath (veraPDF's metadata-fixer output for the same input); returns whether
/// it also corrected the PDF/UA-2 identifier's revision.
let apply (inputPath: string) (fixedPath: string) (outputPath: string) =
    let xmp, info = readFixed fixedPath
    // Its bytes read before the writer starts the output, which may be the same file; each its
    // own, so both are closed if the document can't be opened.
    use reader = PdfFiles.reader inputPath
    use writer = new PdfWriter(outputPath)
    use document = new PdfDocument(reader, writer)

    let revised =
        match xmp with
        | Some bytes ->
            let metadata = XMPMetaFactory.ParseFromBuffer bytes
            let revised = revisePdfUa2 metadata
            document.SetXmpMetadata metadata
            revised
        | None -> false

    info
    |> Option.iter (fun entries ->
        match document.GetTrailer().GetAsDictionary(PdfName.Info) with
        | null ->
            // iText makes its own Info dictionary when the input has none; fill that one.
            for KeyValue(key, value) in entries do
                match value with
                | Text text -> document.GetDocumentInfo().SetMoreInfo(key, text) |> ignore
                | Other -> ()
        | infoDictionary ->
            for key in List.ofSeq (infoDictionary.KeySet()) do
                if not (entries.ContainsKey(key.GetValue())) then
                    infoDictionary.Remove key |> ignore
            for KeyValue(key, value) in entries do
                match value with
                | Text text -> infoDictionary.Put(PdfName key, textString text) |> ignore
                | Other -> ())

    revised
