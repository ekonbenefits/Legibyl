/// Describes web links (and QR codes that encode web addresses) by the titles of the pages
/// they go to, for the opt-in LinkTitlesFromWeb fix.
///
/// Fetching an address a document names is fetching something a stranger chose, so it's done
/// defensively: only http(s) addresses without credentials in them; only to public internet
/// addresses, checked on every connection (redirects included) against the address actually
/// connected to, so neither a redirect nor DNS rebinding can reach this machine, the local
/// network or a cloud metadata service; no proxy (it would resolve the name itself), cookies
/// or credentials; a few redirects; a short timeout; only HTML, and only its first few hundred
/// kilobytes. The title that comes back is text the page's owner chose, so control and
/// invisible formatting characters (bidi overrides, zero-width spaces) are removed, it's cut
/// short, and a description always names the link's own host too, so a page can't pass itself
/// off as another site.
module Legibyl.Validation.LinkTitles

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Net
open System.Net.Http
open System.Net.Sockets
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open Legibyl.Validation.Interop

/// The web address text names, if it's one: an absolute http(s) address, or one starting
/// "www." (as qr-link-pdf treats them), taken to be https. Addresses with a user name or
/// password in them aren't: fetching one would send the credentials along.
let webAddress (text: string) =
    let text = text.Trim()
    let candidate = if text.StartsWith("www.", StringComparison.OrdinalIgnoreCase) then "https://" + text else text
    match Uri.TryCreate(candidate, UriKind.Absolute) with
    | true, NonNull uri when (uri.Scheme = Uri.UriSchemeHttps || uri.Scheme = Uri.UriSchemeHttp) && uri.UserInfo = "" && uri.Host <> "" ->
        Some candidate
    | _ -> None

/// Whether address is on the public internet. IPv4 addresses in the private, loopback,
/// link-local (including 169.254.169.254, cloud metadata), shared (CGNAT), documentation,
/// benchmarking, multicast and reserved ranges aren't. For IPv6 only global unicast
/// (2000::/3) is, less documentation, Teredo, and 6to4 wrapping an IPv4 address that isn't.
let rec isPublic (address: IPAddress) =
    if address.IsIPv4MappedToIPv6 then
        isPublic (address.MapToIPv4())
    else
        let b = address.GetAddressBytes()
        match address.AddressFamily with
        | AddressFamily.InterNetwork ->
            not (
                b[0] = 0uy
                || b[0] = 10uy
                || b[0] = 127uy
                || (b[0] = 100uy && b[1] >= 64uy && b[1] <= 127uy)
                || (b[0] = 169uy && b[1] = 254uy)
                || (b[0] = 172uy && b[1] >= 16uy && b[1] <= 31uy)
                || (b[0] = 192uy && b[1] = 0uy && (b[2] = 0uy || b[2] = 2uy))
                || (b[0] = 192uy && b[1] = 88uy && b[2] = 99uy)
                || (b[0] = 192uy && b[1] = 168uy)
                || (b[0] = 198uy && (b[1] = 18uy || b[1] = 19uy))
                || (b[0] = 198uy && b[1] = 51uy && b[2] = 100uy)
                || (b[0] = 203uy && b[1] = 0uy && b[2] = 113uy)
                || b[0] >= 224uy
            )
        | AddressFamily.InterNetworkV6 ->
            let globalUnicast = b[0] &&& 0xE0uy = 0x20uy
            let documentation = b[0] = 0x20uy && b[1] = 0x01uy && b[2] = 0x0Duy && b[3] = 0xB8uy
            let teredo = b[0] = 0x20uy && b[1] = 0x01uy && b[2] = 0uy && b[3] = 0uy
            let sixToFour = b[0] = 0x20uy && b[1] = 0x02uy
            globalUnicast
            && not documentation
            && not teredo
            && (not sixToFour || isPublic (IPAddress(b[2..5])))
        | _ -> false

/// The longest title used; longer ones are cut at a word.
let private maxTitleLength = 120

/// title as it can safely be read out: control characters as spaces, invisible formatting
/// characters (bidi overrides, zero-width spaces) removed, whitespace collapsed, and cut to
/// maxTitleLength. None if nothing's left.
let cleanTitle (title: string) =
    let visible =
        title
        |> String.map (fun c -> if Char.IsControl c then ' ' else c)
        |> String.filter (fun c -> CharUnicodeInfo.GetUnicodeCategory c <> UnicodeCategory.Format)
    let text = Regex.Replace(visible, @"\s+", " ").Trim()
    if text = "" then
        None
    elif text.Length <= maxTitleLength then
        Some text
    else
        let cut = text.Substring(0, maxTitleLength)
        let cut = if Char.IsHighSurrogate cut[cut.Length - 1] then cut.Substring(0, cut.Length - 1) else cut
        let atWord = match cut.LastIndexOf ' ' with | i when i > maxTitleLength / 2 -> cut.Substring(0, i) | _ -> cut
        Some(atWord.TrimEnd() + "…")

/// The title of an HTML page: its <title>, entities decoded, cleaned (see cleanTitle).
let titleIn (html: string) =
    let found =
        try
            Regex.Match(html, @"<title\b[^>]*>(.*?)</title\s*>", RegexOptions.IgnoreCase ||| RegexOptions.Singleline, TimeSpan.FromSeconds 1.0)
        with :? RegexMatchTimeoutException ->
            Match.Empty
    match if found.Success then WebUtility.HtmlDecode found.Groups[1].Value else null with
    | NonNull title -> cleanTitle title
    | Null -> None

/// The title found for address, if it says more than the address itself does.
let titleFor (titles: IReadOnlyDictionary<string, string>) (address: string) =
    match titles.TryGetValue address with
    | true, title ->
        let host = Uri(address).IdnHost
        let same (other: string) = String.Equals(title.TrimEnd('/'), other.TrimEnd('/'), StringComparison.OrdinalIgnoreCase)
        if same address || same host || same ("www." + host) || same (host.Replace("www.", "")) then None else Some title
    | _ -> None

/// A link's description from the title of the page it goes to, with the link's own host,
/// e.g. "Apply for benefits (example.com)".
let linkDescription (titles: IReadOnlyDictionary<string, string>) (address: string) =
    titleFor titles address |> Option.map (fun title -> $"{title} ({Uri(address).IdnHost})")

/// A QR code's alt text, as tagging writes it and the Suggest Structure preview shows it: where
/// it links (named by the page's title, when titles has it), or else what it says.
let qrAltText (titles: IReadOnlyDictionary<string, string>) (qrText: string) =
    match webAddress qrText with
    | Some address ->
        match titleFor titles address with
        | Some title -> $"QR code linking to {title} ({address})"
        | None -> $"QR code linking to {address}"
    | None -> $"QR code: {qrText}"

/// The most of a page read, looking for its title.
let private maxBytes = 256 * 1024

/// How long one page (redirects included) gets.
let private timeout = TimeSpan.FromSeconds 10.0

/// The most pages fetched for one document, and at once.
let private maxAddresses = 100
let private maxConcurrent = 8

/// Connects only to public internet addresses (see isPublic): the name is resolved here, every
/// address it resolves to has to be public, and the connection is made to those addresses, so
/// what was checked is what's connected to. Called for every connection, redirects included.
let private connectToPublicAddress (context: SocketsHttpConnectionContext) (token: CancellationToken) : ValueTask<Stream> =
    ValueTask<Stream>(
        task {
            let! addresses = Dns.GetHostAddressesAsync(context.DnsEndPoint.Host, token)
            if addresses.Length = 0 || not (addresses |> Array.forall isPublic) then
                raise (HttpRequestException $"{context.DnsEndPoint.Host} isn't a public internet address.")
            let socket = new Socket(SocketType.Stream, ProtocolType.Tcp, NoDelay = true)
            try
                do! socket.ConnectAsync(addresses, context.DnsEndPoint.Port, token)
                return new NetworkStream(socket, true) :> Stream
            with ex ->
                socket.Dispose()
                return raise ex
        }
    )

let private createClient () =
    let handler =
        new SocketsHttpHandler(
            ConnectCallback = Func<_, _, _>(connectToPublicAddress),
            UseProxy = false,
            UseCookies = false,
            Credentials = null,
            PreAuthenticate = false,
            AllowAutoRedirect = true,
            MaxAutomaticRedirections = 5,
            AutomaticDecompression = DecompressionMethods.All,
            ConnectTimeout = timeout
        )
    let client = new HttpClient(handler, true, Timeout = timeout)
    client.DefaultRequestHeaders.UserAgent.ParseAdd "Legibyl (PDF accessibility checker; reading link titles)"
    client.DefaultRequestHeaders.Accept.ParseAdd "text/html, application/xhtml+xml;q=0.9"
    client

/// The encoding a page's bytes are in: its Content-Type's charset, or a <meta> charset near the
/// top, or UTF-8.
let private encodingOf (charset: string | null) (bytes: byte array) (count: int) =
    let named (name: string) =
        try Some(Encoding.GetEncoding(name.Trim(' ', '"', '\''))) with _ -> None
    let fromMeta () =
        let head = Encoding.ASCII.GetString(bytes, 0, min count 4096)
        let found = Regex.Match(head, @"<meta[^>]+charset\s*=\s*[""']?([\w-]+)", RegexOptions.IgnoreCase)
        if found.Success then named found.Groups[1].Value else None
    match charset with
    | NonNull charset when charset <> "" -> named charset
    | _ -> fromMeta ()
    |> Option.defaultValue Encoding.UTF8

/// The title of the page at address, or None for anything short of a readable HTML page.
let private fetchTitle (client: HttpClient) (address: string) : Task<string option> =
    task {
        try
            use cancel = new CancellationTokenSource(timeout)
            use request = new HttpRequestMessage(HttpMethod.Get, address)
            use! response = client.SendAsync(request, HttpCompletionOption.ResponseHeadersRead, cancel.Token)
            let contentType = response.Content.Headers.ContentType
            let mediaType = contentType |?> _.MediaType
            if not response.IsSuccessStatusCode || (mediaType <> "text/html" && mediaType <> "application/xhtml+xml") then
                return None
            else
                use! stream = response.Content.ReadAsStreamAsync cancel.Token
                let buffer = Array.zeroCreate<byte> maxBytes
                let mutable count = 0
                let mutable finished = false
                while not finished && count < buffer.Length do
                    let! read = stream.ReadAsync(buffer.AsMemory(count), cancel.Token)
                    if read = 0 then finished <- true else count <- count + read
                let encoding = encodingOf (contentType |?> _.CharSet) buffer count
                return titleIn (encoding.GetString(buffer, 0, count))
        with _ ->
            return None
    }

/// Reads the titles of web pages from the internet, safely (see the top of this file).
type WebPageTitles() =
    interface IPageTitleSource with
        member _.PageTitles(addresses) =
            let wanted = addresses |> Seq.choose webAddress |> Seq.distinct |> Seq.truncate maxAddresses |> Array.ofSeq
            let titles = Dictionary<string, string>()
            if wanted.Length > 0 then
                // On the thread pool, so waiting here can't deadlock a UI thread's context.
                Task
                    .Run<(string * string option) array>(fun () ->
                        task {
                            use client = createClient ()
                            use slots = new SemaphoreSlim(maxConcurrent)
                            let one address =
                                task {
                                    do! slots.WaitAsync()
                                    try
                                        let! title = fetchTitle client address
                                        return address, title
                                    finally
                                        slots.Release() |> ignore
                                }
                            return! Task.WhenAll(wanted |> Array.map one)
                        })
                    .GetAwaiter()
                    .GetResult()
                |> Array.iter (fun (address, title) -> title |> Option.iter (fun title -> titles[address] <- title))
            titles :> IReadOnlyDictionary<string, string>
