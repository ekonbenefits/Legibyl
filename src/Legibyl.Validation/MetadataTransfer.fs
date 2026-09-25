/// Carries veraPDF's metadata repair over into a PDF saved by iText.
///
/// veraPDF saves its repair as an incremental update with a classic xref table, even when the
/// original uses xref streams (and with a /Prev one byte past the original startxref). iText
/// can't follow that chain, so it rebuilds the xref by scanning the file, and on files with
/// object streams or linearization that rebuild loses or mixes up objects, or fails outright.
/// So rather than letting iText read veraPDF's output, this reads the repaired XMP and Info
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

/// The repaired XMP packet (decoded) and Info entries, as veraPDF's parser sees them.
let private readRepaired (repairedPath: string) =
    let document = PDDocument(repairedPath)
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

/// Writes inputPath to outputPath with its XMP metadata and Info dictionary replaced by
/// those in repairedPath (veraPDF's metadata-fixer output for the same input).
let apply (inputPath: string) (repairedPath: string) (outputPath: string) =
    let xmp, info = readRepaired repairedPath
    use document = new PdfDocument(new PdfReader(inputPath), new PdfWriter(outputPath))

    xmp |> Option.iter (fun bytes -> document.SetXmpMetadata(XMPMetaFactory.ParseFromBuffer bytes))

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
