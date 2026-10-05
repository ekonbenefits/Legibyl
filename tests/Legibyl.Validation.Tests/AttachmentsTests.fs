module Legibyl.Validation.Tests.AttachmentsTests

open Xunit
open FsUnit.Xunit
open iText.Kernel.Geom
open iText.Kernel.Pdf
open iText.Kernel.Pdf.Annot
open iText.Kernel.Pdf.Filespec
open Legibyl.Validation
open Legibyl.Validation.Attachments
open Legibyl.Validation.Tests.TestSupport

/// A PDF of version with notes.txt attached as change leaves it, and with a page whose Screen
/// annotation plays a media clip (its data tour.mp4) given clip's entries.
let private create (path: string) (version: PdfVersion) (change: PdfDictionary -> unit) (clip: (PdfName * PdfObject) list) =
    use document = new PdfDocument(new PdfWriter(path, WriterProperties().SetPdfVersion version))
    let page = document.AddNewPage PageSize.LETTER
    let spec = PdfFileSpec.CreateEmbeddedFileSpec(document, System.Text.Encoding.UTF8.GetBytes "Bring gloves.", null, "notes.txt", null, null)
    change (spec.GetPdfObject() :?> PdfDictionary)
    document.AddFileAttachment("notes.txt", spec)
    let data = PdfDictionary()
    data.Put(PdfName.Type, PdfName.Filespec) |> ignore
    data.Put(PdfName.F, PdfString "tour.mp4") |> ignore
    let media = PdfDictionary()
    media.Put(PdfName.Type, PdfName.MediaClip) |> ignore
    media.Put(PdfName.S, PdfName.MCD) |> ignore
    media.Put(PdfName.D, data) |> ignore
    for key, value in clip do
        media.Put(key, value) |> ignore
    let rendition = PdfDictionary()
    rendition.Put(PdfName.Type, PdfName.Rendition) |> ignore
    rendition.Put(PdfName.S, PdfName.MR) |> ignore
    rendition.Put(PdfName.C, media) |> ignore
    let action = PdfDictionary()
    action.Put(PdfName.Type, PdfName.Action) |> ignore
    action.Put(PdfName.S, PdfName.Rendition) |> ignore
    action.Put(PdfName.R, rendition) |> ignore
    let screen = PdfScreenAnnotation(Rectangle(72f, 500f, 200f, 120f))
    screen.SetContents "The garden tour" |> ignore
    screen.GetPdfObject().Put(PdfName.A, action) |> ignore
    page.AddAnnotation screen |> ignore

let private failed (profile: PdfProfile) (path: string) =
    VeraPdfValidator().Validate(path, profile).Findings
    |> Seq.filter (fun f -> not f.Passed && f.Severity = FindingSeverity.Failure)
    |> Seq.map _.RuleId
    |> Set.ofSeq

let private fix (input: string) (output: string) (profile: PdfProfile) (edits: (string * AttachmentEdit) list) =
    let validator = VeraPdfValidator()
    validator.Attachments <- System.Collections.Generic.Dictionary<string, AttachmentEdit>(dict edits)
    validator.Fix(input, profile, output, PdfFixOptions.ApplyEdits)

[<Fact>]
let ``an attached file and a media clip are found, with what they have`` () =
    use input = new TempPdf()
    create input.Path PdfVersion.PDF_1_7 (fun spec -> spec.Remove PdfName.UF |> ignore) [ PdfName.N, PdfString "Garden tour" ]

    let found = Attachments.list input.Path |> List.map (fun a -> a.Key, a.Kind, a.FileName, a.Named, a.Description, a.ContentType, a.Where)

    let expected =
        [ "notes.txt", File, Some "notes.txt", false, (None: string option), (None: string option), "the document"
          "Garden tour", Media, Some "tour.mp4", true, None, None, "page 1" ]
    found = expected |> should be True

[<Fact>]
let ``an attached file is given its file names and a description`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    create input.Path PdfVersion.PDF_2_0 (fun spec -> spec.Remove PdfName.UF |> ignore) [ PdfName.N, PdfString "Garden tour"; PdfName.CT, PdfString "video/mp4" ]
    let before = failed PdfProfile.PdfUa2 input.Path
    before |> should contain "8.14.1-1"

    let result =
        fix input.Path output.Path PdfProfile.PdfUa2 [ "notes.txt", { FileName = "notes.txt"; Description = "What to bring"; ContentType = null } ]

    failed PdfProfile.PdfUa2 output.Path |> should not' (contain "8.14.1-1")
    let file = Attachments.list output.Path |> List.find (fun a -> a.Kind = File)
    file.Named |> should be True
    file.Description |> should equal (Some "What to bring")
    result.AppliedFixes |> should contain "Wrote the attached file \"notes.txt\"'s UF and Desc, as given."

[<Fact>]
let ``an attached file without its file names fails PDF/UA-1, and passes with them`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    create input.Path PdfVersion.PDF_1_7 (fun spec -> spec.Remove PdfName.UF |> ignore) [ PdfName.N, PdfString "Garden tour"; PdfName.CT, PdfString "video/mp4" ]
    failed PdfProfile.PdfUa1 input.Path |> should contain "7.11-1"

    fix input.Path output.Path PdfProfile.PdfUa1 [ "notes.txt", { FileName = "notes.txt"; Description = null; ContentType = null } ] |> ignore

    failed PdfProfile.PdfUa1 output.Path |> should not' (contain "7.11-1")

[<Fact>]
let ``a media clip is given its content type and a description`` () =
    use input = new TempPdf()
    use output = new TempPdf()
    create input.Path PdfVersion.PDF_1_7 ignore [ PdfName.N, PdfString "Garden tour" ]
    let before = failed PdfProfile.PdfUa1 input.Path
    before |> should contain "7.18.6.2-1"
    before |> should contain "7.18.6.2-2"

    fix input.Path output.Path PdfProfile.PdfUa1 [ "Garden tour", { FileName = null; Description = "A walk round the garden in spring"; ContentType = "video/mp4" } ]
    |> ignore

    let after = failed PdfProfile.PdfUa1 output.Path
    after |> should not' (contain "7.18.6.2-1")
    after |> should not' (contain "7.18.6.2-2")
    let clip = Attachments.list output.Path |> List.find (fun a -> a.Kind = Media)
    (clip.ContentType, clip.Description) |> should equal (Some "video/mp4", Some "A walk round the garden in spring")
    Attachments.contentTypeFor "tour.MP4" |> should equal (Some "video/mp4")
    Attachments.contentTypeFor "notes.txt" |> should equal None
