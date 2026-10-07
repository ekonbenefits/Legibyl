/// Changes only marked-content delimiters: the drawing operators remain byte-identical.
module internal Legibyl.Validation.ContentArtifacts

open System.Collections.Generic
open System.IO
open System.Text
open iText.Kernel.Pdf

let mark (document: PdfDocument) (byPage: Dictionary<int, HashSet<int>>) =
    for KeyValue(pageNumber, mcids) in byPage do
        let page = document.GetPage pageNumber
        let bytes = page.GetContentBytes()
        let resources = page.GetResources()
        let operations, length = ContentOperations.operationsOf bytes resources
        use output = new MemoryStream()
        let mutable from = 0L
        for operation in operations do
            match ContentOperations.mcidOf resources operation with
            | Some mcid when mcids.Contains mcid ->
                output.Write(bytes, int from, int (operation.Start - from))
                let artifact = Encoding.ASCII.GetBytes "\n/Artifact BMC\n"
                output.Write(artifact, 0, artifact.Length)
                from <- operation.End
            | _ -> ()
        output.Write(bytes, int from, int (length - from))
        page.GetPdfObject().Put(PdfName.Contents, PdfStream(output.ToArray()).MakeIndirect document) |> ignore
