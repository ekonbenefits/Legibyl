module Legibyl.Validation.Tests.LinkTitlesTests

open System
open System.Net
open System.Threading
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open Legibyl.Validation

[<Theory>]
[<InlineData("https://example.com/apply", "https://example.com/apply")>]
[<InlineData("http://example.com", "http://example.com")>]
[<InlineData("www.example.com/apply", "https://www.example.com/apply")>]
[<InlineData("  https://example.com  ", "https://example.com")>]
let ``a web address is an absolute http(s) one, or www. taken as https`` (text: string, expected: string) =
    LinkTitles.webAddress text |> should equal (Some expected)

[<Theory>]
[<InlineData("ftp://example.com/file")>]
[<InlineData("file:///etc/passwd")>]
[<InlineData("javascript:alert(1)")>]
[<InlineData("mailto:someone@example.com")>]
[<InlineData("https://user:secret@example.com/")>]
[<InlineData("/relative/path")>]
[<InlineData("Call us on 555-0100")>]
let ``anything else isn't a web address, nor is one carrying credentials`` (text: string) =
    LinkTitles.webAddress text |> should equal (None: string option)

[<Theory>]
[<InlineData("127.0.0.1")>]
[<InlineData("127.1.2.3")>]
[<InlineData("0.0.0.0")>]
[<InlineData("10.1.2.3")>]
[<InlineData("172.16.0.1")>]
[<InlineData("172.31.255.255")>]
[<InlineData("192.168.1.1")>]
[<InlineData("169.254.169.254")>]
[<InlineData("100.64.0.1")>]
[<InlineData("192.0.0.8")>]
[<InlineData("192.0.2.1")>]
[<InlineData("198.18.0.1")>]
[<InlineData("224.0.0.1")>]
[<InlineData("255.255.255.255")>]
[<InlineData("::1")>]
[<InlineData("::")>]
[<InlineData("fe80::1")>]
[<InlineData("fc00::1")>]
[<InlineData("fd12:3456::1")>]
[<InlineData("ff02::1")>]
[<InlineData("::ffff:127.0.0.1")>]
[<InlineData("::ffff:169.254.169.254")>]
[<InlineData("64:ff9b::7f00:1")>]
[<InlineData("2001:db8::1")>]
[<InlineData("2001::1")>]
[<InlineData("2002:0a00:0001::1")>]
let ``loopback, private, link-local and reserved addresses aren't public`` (address: string) =
    LinkTitles.isPublic (IPAddress.Parse address) |> should be False

[<Theory>]
[<InlineData("93.184.215.14")>]
[<InlineData("8.8.8.8")>]
[<InlineData("172.32.0.1")>]
[<InlineData("::ffff:8.8.8.8")>]
[<InlineData("2606:4700:4700::1111")>]
[<InlineData("2002:0808:0808::1")>]
let ``internet addresses are public`` (address: string) =
    LinkTitles.isPublic (IPAddress.Parse address) |> should be True

[<Fact>]
let ``a page's title is its title element, entities decoded and whitespace collapsed`` () =
    LinkTitles.titleIn "<html><head><TITLE lang=\"en\">\n  Apply &amp; enrol\t for   benefits </TITLE></head></html>"
    |> should equal (Some "Apply & enrol for benefits")

[<Fact>]
let ``a page without a title, or with an empty one, has none`` () =
    LinkTitles.titleIn "<html><body>Hello</body></html>" |> should equal (None: string option)
    LinkTitles.titleIn "<title> ​ </title>" |> should equal (None: string option)

[<Fact>]
let ``control and invisible formatting characters are taken out of a title`` () =
    // A right-to-left override would make the text read as something else on screen.
    LinkTitles.cleanTitle "Pay‮gnp.exe​ now\u0007" |> should equal (Some "Paygnp.exe now")

[<Fact>]
let ``a long title is cut at a word`` () =
    let title = String.replicate 30 "benefits "
    let cleaned = (LinkTitles.cleanTitle title).Value
    cleaned.Length |> should be (lessThanOrEqualTo 121)
    cleaned |> should endWith "benefits…"

[<Fact>]
let ``a link is described by its page's title and its own host`` () =
    let titles = readOnlyDict [ "https://www.example.com/apply", "Apply for benefits" ]
    LinkTitles.linkDescription titles "https://www.example.com/apply" |> should equal (Some "Apply for benefits (www.example.com)")

[<Theory>]
[<InlineData("example.com")>]
[<InlineData("www.example.com")>]
[<InlineData("https://example.com/")>]
let ``a title that only repeats the address says nothing`` (title: string) =
    let titles = readOnlyDict [ "https://example.com/", title ]
    LinkTitles.linkDescription titles "https://example.com/" |> should equal (None: string option)

/// Serves one page with a title on this machine, counting the requests it gets.
type private LocalPage() =
    let listener = new HttpListener()
    let port = 20000 + System.Random.Shared.Next 20000
    let mutable requests = 0
    do
        listener.Prefixes.Add $"http://127.0.0.1:{port}/"
        listener.Start()
        Task.Run(fun () ->
            try
                while listener.IsListening do
                    let context = listener.GetContext()
                    Interlocked.Increment &requests |> ignore
                    let body = Text.Encoding.UTF8.GetBytes "<title>Internal admin</title>"
                    context.Response.ContentType <- "text/html"
                    context.Response.OutputStream.Write(body, 0, body.Length)
                    context.Response.Close()
            with _ -> ())
        |> ignore
    member _.Port = port
    member _.Requests = requests
    interface System.IDisposable with
        member _.Dispose() = listener.Close()

[<Fact>]
let ``page titles are never read from this machine`` () =
    use page = new LocalPage()
    let titles: IPageTitleSource = LinkTitles.WebPageTitles()

    let found = titles.PageTitles [ $"http://127.0.0.1:{page.Port}/"; $"http://localhost:{page.Port}/"; $"http://[::1]:{page.Port}/" ]

    found.Count |> should equal 0
    page.Requests |> should equal 0

[<Fact>]
let ``a QR code's alt text says where it links, named by the page's title when that's known`` () =
    let titles = readOnlyDict [ "https://example.com/apply", "Apply for benefits" ]
    LinkTitles.qrAltText titles None "https://example.com/apply" |> should equal ("QR code linking to Apply for benefits (https://example.com/apply)", "en")
    LinkTitles.qrAltText titles None "www.example.com/other" |> should equal ("QR code linking to https://www.example.com/other", "en")
    LinkTitles.qrAltText titles None "WIFI:S:Guest;;" |> should equal ("QR code: WIFI:S:Guest;;", "en")

[<Fact>]
let ``a QR code's alt text and its link's description are in the document's language, where its pack has them`` () =
    let titles = readOnlyDict [ "https://example.com/solicitar", "Solicitar beneficios" ]
    LinkTitles.qrAltText titles (Some "es-MX") "https://example.com/solicitar"
    |> should equal ("Código QR que enlaza a Solicitar beneficios (https://example.com/solicitar)", "es")
    LinkTitles.qrAltText titles (Some "es") "WIFI:S:Invitados;;" |> should equal ("Código QR: WIFI:S:Invitados;;", "es")
    LinkTitles.qrLinkContents titles (Some "es") "https://example.com/otra"
    |> should equal ("Enlace del código QR a https://example.com/otra", "es")
    // A language with no pack: Legibyl's English, said to be English.
    LinkTitles.qrAltText titles (Some "ja") "WIFI:S:Guest;;" |> should equal ("QR code: WIFI:S:Guest;;", "en")
