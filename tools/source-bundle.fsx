// Gathers the corresponding source for a Legibyl release: Legibyl's own (AGPL-3.0-or-later), and
// that of every dependency it's built with and ships, at the versions pinned, so it can be
// provided when asked (AGPL section 6 for iText and Legibyl, GPL for veraPDF; the permissive ones
// alongside, so nothing has to be hunted down later). Run it from the repository, after building,
// at the commit the release is built from:
//
//   dotnet fsi tools/source-bundle.fsx                 sources into artifacts/source/legibyl-<version>/
//   dotnet fsi tools/source-bundle.fsx --dry-run       checks every source can be found, fetches nothing
//   dotnet fsi tools/source-bundle.fsx --all           also the .NET runtime's and the build tools'
//   dotnet fsi tools/source-bundle.fsx --out <folder>
//   dotnet fsi tools/source-bundle.fsx --only itext,verapdf   just those (any part of a name), and Legibyl
//   dotnet fsi tools/source-bundle.fsx --fingerprint   prints the dependencies' fingerprint, fetches nothing
//   dotnet fsi tools/source-bundle.fsx --own           Legibyl's own source only (its dependencies' looked up)
//
// The dependencies' source changes far less often than Legibyl, so it's released on its own, once
// for each set of dependencies, and each release of Legibyl points to it (#233). The set is named
// by a fingerprint of the manifest's lines but Legibyl's own (below): deps-<12 hex digits>, the
// same for the same dependencies, whatever else changed. It's in the manifest, and --fingerprint
// prints it last.
//
// Then it packs them for GitHub releases (each file there must be under 2 GiB), into release/
// beside the folder: legibyl-<version>-source.tar (Legibyl's own, and the manifest), for the
// release of Legibyl; and deps-<fingerprint>/, for the dependencies' own release: their source,
// deps-<fingerprint>-source.tar in parts under 1.9 GiB (.part01...), with the manifest,
// SHA256SUMS and how to join them. (Not for --dry-run, --fingerprint or --only; just the first for
// --own.) It fails if any source can't be found.
//
// What's fetched, and from where, is in the folder's MANIFEST.md, a line for each component: its
// version, its licence, and the source (or why there's none). It fetches:
//
//   - Legibyl's own source: `git archive` of HEAD (warning if the working tree has changes);
//   - each NuGet package the app and the CLI restore to (their obj/project.assets.json), from the
//     repository and commit its package records (else a tag for its version, or what's known for
//     the few that record neither), once for packages from the same commit, with the git
//     submodules it has at that commit (IKVM's OpenJDK, SkiaSharp's Skia, ANGLE's third-party
//     libraries...): permissively licensed code too, since it's part of the work as shipped;
//   - veraPDF's apps at the jar's version, and a sources jar from Maven Central for each library
//     bundled in it (its META-INF/maven pom.properties);
//   - PDFium at the version the bblanchon.PDFium packages are built from;
//   - the models', tessdata's and fonts' pinned files are listed (they're data, already pinned, not built).
//
// Left out unless --all (but listed): the .NET runtime and base libraries (dotnet/runtime,
// dotnet/dotnet: system libraries, several GB), packages only used to build (Dotnet.Bundle,
// IKVM's MSBuild tools, Avalonia's build services), and submodules that are build tooling, not
// part of what's built (compilers, depot_tools, code generators, test suites: see isTooling). A
// file already fetched isn't fetched again.
// Set GITHUB_TOKEN (or be signed in to `gh`) to resolve submodules past GitHub's anonymous limit.

#r "System.Formats.Tar"

open System
open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Net
open System.Net.Http
open System.Formats.Tar
open System.Security.Cryptography
open System.Text.Json
open System.Text.RegularExpressions

let args = fsi.CommandLineArgs |> Array.skip 1 |> List.ofArray
let fingerprintOnly = List.contains "--fingerprint" args
let ownOnly = List.contains "--own" args
let dryRun = List.contains "--dry-run" args || fingerprintOnly
/// Whether the dependencies' sources are only looked up, not fetched.
let lookupOnly = dryRun || ownOnly
let everything = List.contains "--all" args
let only =
    match args |> List.tryFindIndex ((=) "--only") with
    | Some i when i + 1 < args.Length -> args[i + 1].Split(',') |> Array.map (fun s -> s.Trim().ToLowerInvariant()) |> List.ofArray
    | _ -> []
if not only.IsEmpty && (fingerprintOnly || ownOnly) then
    failwith "--fingerprint and --own are of every dependency: not with --only."
/// Whether a component is wanted (with --only, whether its name has one of those in it).
let wanted (name: string) = only.IsEmpty || only |> List.exists (fun part -> name.ToLowerInvariant().Contains part)
let root = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, ".."))

let run (file: string) (arguments: string) =
    let info = ProcessStartInfo(file, arguments, RedirectStandardOutput = true, RedirectStandardError = true, WorkingDirectory = root)
    use p = Process.Start info
    let output = p.StandardOutput.ReadToEnd()
    p.WaitForExit()
    if p.ExitCode = 0 then Some(output.Trim()) else None

let property (file: string) (name: string) =
    let text = File.ReadAllText(Path.Combine(root, file))
    let m = Regex.Match(text, $"<{name}>([^<]*)</{name}>")
    if m.Success then m.Groups[1].Value.Trim() else failwith $"No {name} in {file}"

let version = property "Directory.Build.props" "VeraPdfVersion" + "." + property "Directory.Build.props" "VersionBuild"
let commit = run "git" "rev-parse HEAD" |> Option.defaultValue "unknown"
let out =
    match args |> List.tryFindIndex ((=) "--out") with
    | Some i when i + 1 < args.Length -> Path.GetFullPath args[i + 1]
    | _ -> Path.Combine(root, "artifacts", "source", $"legibyl-{version}")

// ---- Fetching ----

let token =
    match Environment.GetEnvironmentVariable "GITHUB_TOKEN" with
    | null
    | "" -> run "gh" "auth token"
    | t -> Some t

let http =
    let client = new HttpClient(Timeout = TimeSpan.FromMinutes 30.0)
    client.DefaultRequestHeaders.UserAgent.ParseAdd "legibyl-source-bundle"
    client

let request (url: string) =
    let message = new HttpRequestMessage(HttpMethod.Get, url)
    match token with
    | Some t when url.StartsWith "https://api.github.com/" || url.StartsWith "https://codeload.github.com/" ->
        message.Headers.Authorization <- Headers.AuthenticationHeaderValue("Bearer", t)
    | _ -> ()
    message

/// Whether url answers (a GET whose body isn't read).
let exists (url: string) =
    try
        use response = http.Send(request url, HttpCompletionOption.ResponseHeadersRead)
        response.IsSuccessStatusCode
    with _ -> false

let text (url: string) =
    try
        use response = http.Send(request url)
        if response.IsSuccessStatusCode then Some(response.Content.ReadAsStringAsync().Result) else None
    with _ -> None

/// f's result, trying it up to four times while it fails, waiting longer each time: servers
/// (googlesource, GitLab) answer 503 now and then.
let retrying (f: unit -> Result<'T, string>) =
    let rec attempt n =
        match f () with
        | Error why when n < 4 ->
            eprintfn $"  ({why}: trying again)"
            Threading.Thread.Sleep(TimeSpan.FromSeconds(5.0 * 3.0 ** float (n - 1)))
            attempt (n + 1)
        | result -> result
    attempt 1

/// Saves url as file under out (unless it's there already, or it's only looked up); its size in bytes.
let fetch (url: string) (file: string) : Result<string, string> =
    let path = Path.Combine(out, file)
    if File.Exists path then
        Ok file
    elif lookupOnly then
        retrying (fun () -> if exists url then Ok file else Error $"not found: {url}")
    else
      retrying (fun () ->
        try
            Directory.CreateDirectory(Path.GetDirectoryName path |> string) |> ignore
            use response = http.Send(request url, HttpCompletionOption.ResponseHeadersRead)
            if not response.IsSuccessStatusCode then
                Error $"{int response.StatusCode} from {url}"
            else
                let partial = path + ".partial"
                do
                    use source = response.Content.ReadAsStream()
                    use target = File.Create partial
                    source.CopyTo target
                File.Move(partial, path, true)
                printfn $"  fetched {file} ({FileInfo(path).Length / 1_000_000L} MB)"
                Ok file
        with ex -> Error $"{ex.Message} ({url})")

/// Where some repositories really are, when what refers to them names an old mirror.
let private moved =
    Map.ofList [ ("https://github.com/Mesa3D/mesa", "https://gitlab.freedesktop.org/mesa/mesa.git") ]

/// A git repository not on GitHub, at a commit or branch, as a tarball: fetched shallowly with git
/// and archived (servers like googlesource won't archive a large tree over HTTP).
let fetchGit (url: string) (reference: string) (file: string) : Result<string, string> =
    let url = Map.tryFind (url.TrimEnd('/')) moved |> Option.defaultValue url
    let path = Path.Combine(out, file)
    if File.Exists path then
        Ok file
    elif lookupOnly then
        retrying (fun () ->
            match run "git" $"ls-remote {url}" with
            | Some _ -> Ok file
            | None -> Error $"can't reach {url}")
    else
      retrying (fun () ->
        let scratch = Path.Combine(Path.GetTempPath(), $"legibyl-source-{Guid.NewGuid():N}")
        try
            Directory.CreateDirectory scratch |> ignore
            Directory.CreateDirectory(Path.GetDirectoryName path |> string) |> ignore
            let git (arguments: string) =
                let info = ProcessStartInfo("git", arguments, RedirectStandardOutput = true, RedirectStandardError = true, WorkingDirectory = scratch)
                use p = Process.Start info
                p.StandardOutput.ReadToEnd() |> ignore
                p.StandardError.ReadToEnd() |> ignore
                p.WaitForExit()
                p.ExitCode = 0
            if git "init -q" && git $"fetch -q --depth 1 {url} {reference}" && git $"archive --format=tar.gz -o \"{path}\" FETCH_HEAD" then
                printfn $"  fetched {file} ({FileInfo(path).Length / 1_000_000L} MB)"
                Ok file
            else
                Error $"git couldn't fetch {reference} from {url}"
        finally
            try Directory.Delete(scratch, true) with _ -> ())

// ---- The manifest ----

type Entry =
    { Component: string
      Version: string
      Licence: string
      Source: string
      /// The file fetched, relative to out; or why there's none.
      Fetched: Result<string, string> }

let entries = ResizeArray<Entry>()

// ---- Legibyl ----

printfn $"Legibyl {version} at {commit} -> {out}"
if not dryRun then Directory.CreateDirectory out |> ignore
match run "git" "status --porcelain" with
| Some changes when changes <> "" -> eprintfn "Warning: the working tree has changes; the archive is of HEAD (the last commit), without them."
| _ -> ()
let own =
    let file = $"legibyl/legibyl-{version}-{commit.Substring(0, min 12 commit.Length)}.tar.gz"
    let path = Path.Combine(out, file)
    if dryRun || File.Exists path then
        Ok file
    else
        Directory.CreateDirectory(Path.GetDirectoryName path |> string) |> ignore
        match run "git" $"archive --format=tar.gz --prefix=legibyl-{version}/ -o \"{path}\" HEAD" with
        | Some _ -> Ok file
        | None -> Error "git archive failed"
entries.Add
    { Component = "Legibyl"
      Version = $"{version} ({commit})"
      Licence = "AGPL-3.0-or-later"
      Source = "this repository, at the release's commit"
      Fetched = own }

// ---- GitHub sources, with their submodules ----

/// A GitHub repository's owner and name, from a repository URL.
let github (url: string) =
    let url = if url.Contains "go.microsoft.com/fwlink/?linkid=868515" then "https://github.com/mono/SkiaSharp" else url
    // GitHub itself, not a mirror whose path names it (chromium.googlesource.com/external/github.com/...).
    let m = Regex.Match(url, @"^(?:https?://|git://|git@)(?:www\.)?github\.com[/:]([^/]+)/([^/]+?)(?:\.git)?/?$")
    // GitHub's names aren't case-sensitive (one package says Microsoft/onnxruntime, another microsoft/).
    if m.Success then Some(m.Groups[1].Value.ToLowerInvariant(), m.Groups[2].Value.ToLowerInvariant()) else None

let fetched = Collections.Generic.Dictionary<string, Result<string, string>>()

/// Whether a submodule's path is build tooling rather than code built into what ships: build
/// scripts, toolchains, code generators' Python libraries, test suites (ANGLE's and Skia's have
/// many).
let isTooling (path: string) =
    let path = path.ToLowerInvariant()
    Regex.IsMatch(path, @"(^|/)(build|buildtools|testing|tools|test|tests)(/|$)")
    || Regex.IsMatch(
        path,
        @"(^|/)(depot_tools|clang|rust|jinja2|markupsafe|python-markdown|six|nasm|catapult|googletest|benchmark|android_[^/]*|llvm-build|gn|ninja|buildutils[^/]*|[^/]*conform[^/]*|[^/]*_cts)(/|$)"
    )

/// A repository at a commit (or tag), as a tarball, and its submodules at the commits it has them
/// at (depth deep), but for build tooling.
let rec fetchGithub (owner: string) (repo: string) (reference: string) (depth: int) : Result<string, string> =
    let key = $"{owner}/{repo}@{reference}"
    match fetched.TryGetValue key with
    | true, result -> result
    | _ ->
        let safe = Regex.Replace(reference, "[^A-Za-z0-9._-]", "_")
        let file = $"github/{owner}-{repo}-{safe}.tar.gz"
        // A commit GitHub won't archive (a mirror without it, say) is fetched with git instead.
        let result =
            match fetch $"https://codeload.github.com/{owner}/{repo}/tar.gz/{reference}" file with
            | Ok file -> Ok file
            | Error _ -> fetchGit $"https://github.com/{owner}/{repo}" reference file
        fetched[key] <- result
        if depth > 0 && Result.isOk result then
            match text $"https://raw.githubusercontent.com/{owner}/{repo}/{reference}/.gitmodules" with
            | Some modules ->
                for m in Regex.Matches(modules, @"path\s*=\s*(\S+)") do
                    let path = m.Groups[1].Value
                    // Build tooling is listed without asking GitHub's API about it (it has a limit).
                    if not everything && isTooling path then
                        entries.Add
                            { Component = $"  submodule {path} of {owner}/{repo}"
                              Version = ""
                              Licence = "see its repository"
                              Source = $"https://github.com/{owner}/{repo}/tree/{reference}/{path}"
                              Fetched = Error "build tooling, not part of what's built (--all fetches it)" }
                    else
                    match text $"https://api.github.com/repos/{owner}/{repo}/contents/{path}?ref={reference}" with
                    | Some json ->
                        use document = JsonDocument.Parse json
                        let element = document.RootElement
                        let mutable kind = Unchecked.defaultof<JsonElement>
                        let mutable url = Unchecked.defaultof<JsonElement>
                        if element.ValueKind = JsonValueKind.Object
                           && element.TryGetProperty("type", &kind) && kind.GetString() = "submodule"
                           && element.TryGetProperty("submodule_git_url", &url) then
                            let sha = element.GetProperty("sha").GetString() |> string
                            let remote = string (url.GetString())
                            let safe = Regex.Replace(path, "[^A-Za-z0-9._-]", "_")
                            let skip = not everything && isTooling path
                            let sub, source =
                                match github remote with
                                | Some(o, r) ->
                                    (if skip then Error "build tooling, not part of what's built (--all fetches it)"
                                     else fetchGithub o r sha (depth - 1)),
                                    $"https://github.com/{o}/{r}/tree/{sha}"
                                | None ->
                                    (if skip then Error "build tooling, not part of what's built (--all fetches it)"
                                     else fetchGit remote sha $"git/{owner}-{repo}-{safe}-{sha.Substring(0, 12)}.tar.gz"),
                                    $"{remote} at {sha}"
                            entries.Add
                                { Component = $"  submodule {path} of {owner}/{repo}"
                                  Version = sha.Substring(0, 12)
                                  Licence = "see its repository"
                                  Source = source
                                  Fetched = sub }
                    | None -> eprintfn $"Warning: couldn't read submodule {path} of {owner}/{repo} (GitHub's API limit? set GITHUB_TOKEN)."
            | None -> ()
        result

// ---- NuGet packages ----

type Package =
    { Id: string
      Version: string
      Licence: string
      Repository: string option
      Commit: string option
      Project: string option
      BuildOnly: bool }

let nugetRoot =
    match Environment.GetEnvironmentVariable "NUGET_PACKAGES" with
    | null
    | "" -> Path.Combine(Environment.GetFolderPath Environment.SpecialFolder.UserProfile, ".nuget", "packages")
    | path -> path

/// Packages only used to build, never shipped.
let buildTools = set [ "DotNet.Bundle"; "Avalonia.BuildServices" ]

let packages =
    [ for project in [ "src/Legibyl.Gui"; "src/Legibyl.Cli" ] do
          let assets = Path.Combine(root, project, "obj", "project.assets.json")
          if not (File.Exists assets) then failwith $"No {assets}: build Legibyl first (dotnet build)."
          use document = JsonDocument.Parse(File.ReadAllText assets)
          for library in document.RootElement.GetProperty("libraries").EnumerateObject() do
              if library.Value.GetProperty("type").GetString() = "package" then
                  yield library.Name ]
    |> List.distinct
    |> List.map (fun key ->
        let id, version = let parts = key.Split '/' in parts[0], parts[1]
        let folder = Path.Combine(nugetRoot, id.ToLowerInvariant(), version.ToLowerInvariant())
        let nuspec = Directory.GetFiles(folder, "*.nuspec") |> Array.head |> File.ReadAllText
        let find (pattern: string) =
            let m = Regex.Match(nuspec, pattern)
            if m.Success then Some m.Groups[1].Value else None
        let repository = Regex.Match(nuspec, @"<repository\s[^>]*>")
        let attribute name =
            if repository.Success then
                let m = Regex.Match(repository.Value, $"{name}=\"([^\"]*)\"")
                if m.Success then Some m.Groups[1].Value else None
            else None
        { Id = id
          Version = version
          Licence =
            find @"<license[^>]*>([^<]*)</license>"
            |> Option.orElse (find @"<licenseUrl>([^<]*)</licenseUrl>")
            |> Option.defaultValue "see the package"
          Repository = attribute "url"
          Commit = attribute "commit"
          Project = find @"<projectUrl>([^<]*)</projectUrl>"
          BuildOnly =
            buildTools.Contains id
            || id.StartsWith "IKVM.MSBuild"
            || nuspec.Contains "<developmentDependency>true</developmentDependency>" })
    |> List.sortBy _.Id

/// Where to find the source of the few packages that record no commit and have no tag for their
/// version: (repository, commit or tag), by package and version, with how it's known.
let knownCommits =
    Map.ofList
        [ // The commit whose LanguageDetection.csproj has <PackageId>LanguageDetection.NETStandard</PackageId>
          // and <Version>1.3.1</Version> (the one before it has 1.2.0); the repository has no tag for it.
          ("LanguageDetection.NETStandard", "1.3.1"), ("https://github.com/TechnikEmpire/language-detection", "d10b91a41c2f31c7da82ffea3124f4472f2b8f46")
          // From .NET Core's host repository, tagged for its release.
          ("Microsoft.DotNet.PlatformAbstractions", "1.1.0"), ("https://github.com/dotnet/core-setup", "refs/tags/v1.1.0") ]

/// The .NET runtime's and base libraries' repositories: system libraries, and huge.
let isSystem (owner: string, repo: string) =
    owner = "dotnet" && (repo = "runtime" || repo = "dotnet" || repo = "corefx" || repo = "extensions")

printfn $"{packages.Length} NuGet packages"
for package in packages |> List.filter (fun p -> wanted p.Id) do
    let at = entries.Count
    let place =
        Map.tryFind (package.Id, package.Version) knownCommits
        |> Option.map fst
        |> Option.orElse package.Repository
        |> Option.orElse package.Project
        |> Option.bind github
    // iText's .NET packages say only https://itextpdf.com/ (itext.commons) or an old name.
    let place =
        if package.Id.StartsWith "itext" then Some("itext", "itext-dotnet") else place
    let fetched =
        match place with
        | _ when package.BuildOnly && not everything -> Error "used only to build, not shipped (--all fetches it)"
        | Some place when isSystem place && not everything -> Error ".NET's own libraries, part of the platform (--all fetches them)"
        | Some(owner, repo) ->
            match package.Commit |> Option.orElse (Map.tryFind (package.Id, package.Version) knownCommits |> Option.map snd) with
            | Some commit -> fetchGithub owner repo commit 2
            | None ->
                // A tag for its version: these are the ways projects name them.
                let candidates =
                    [ package.Version; $"v{package.Version}"; $"v{package.Version}.0"; $"{package.Version}.0"; $"release-{package.Version}"; $"{repo}-{package.Version}" ]
                match candidates |> List.tryFind (fun tag -> exists $"https://codeload.github.com/{owner}/{repo}/tar.gz/refs/tags/{tag}") with
                | Some tag -> fetchGithub owner repo $"refs/tags/{tag}" 2
                | None -> Error $"no commit recorded, and no tag for {package.Version} in {owner}/{repo}: find its source there"
        | None -> Error "no repository recorded: see its project page"
    entries.Insert(
      at,
        { Component = package.Id
          Version = package.Version
          Licence = package.Licence
          Source =
            match place, package.Commit |> Option.orElse (Map.tryFind (package.Id, package.Version) knownCommits |> Option.map snd) with
            | Some(owner, repo), Some commit ->
                let at = commit.Replace("refs/tags/", "")
                $"https://github.com/{owner}/{repo}/tree/{at}"
            | Some(owner, repo), None -> $"https://github.com/{owner}/{repo}"
            | None, _ -> package.Repository |> Option.orElse package.Project |> Option.defaultValue ""
          Fetched = fetched })

// ---- veraPDF and the libraries in its jar ----

let veraPdf = property "Directory.Build.props" "VeraPdfVersion"
let jar = Path.Combine(root, "src", "Legibyl.Validation", "obj", "verapdf", $"greenfield-apps-{veraPdf}.jar")
if wanted "verapdf" then
    let at = entries.Count
    let apps = fetchGithub "veraPDF" "veraPDF-apps" $"refs/tags/v{veraPdf}" 1
    entries.Insert(
        at,
        { Component = "veraPDF apps (greenfield-apps jar)"
          Version = veraPdf
          Licence = "GPL-3.0-or-later OR MPL-2.0"
          Source = $"https://github.com/veraPDF/veraPDF-apps/tree/v{veraPdf}"
          Fetched = apps })
    if not (File.Exists jar) then failwith $"No {jar}: build Legibyl first (dotnet build)."
    use archive = ZipFile.OpenRead jar
    for entry in archive.Entries do
        if entry.FullName.EndsWith "pom.properties" then
            use reader = new StreamReader(entry.Open())
            let properties = reader.ReadToEnd()
            let value name = Regex.Match(properties, $"(?m)^{name}=(.*)$").Groups[1].Value.Trim()
            let group, artifact, release = value "groupId", value "artifactId", value "version"
            let url = $"https://repo1.maven.org/maven2/{group.Replace('.', '/')}/{artifact}/{release}/{artifact}-{release}-sources.jar"
            entries.Add
                { Component = $"  {group}:{artifact} (in the veraPDF jar)"
                  Version = release
                  Licence = if group.StartsWith "org.verapdf" then "GPL-3.0-or-later OR MPL-2.0" else "see its POM"
                  Source = url
                  Fetched = fetch url $"maven/{group}-{artifact}-{release}-sources.jar" }

// ---- PDFium ----

let pdfium = Regex.Match(File.ReadAllText(Path.Combine(root, "Directory.Packages.props")), "<PdfiumVersion>([^<]*)</PdfiumVersion>").Groups[1].Value
let branch = pdfium.Split('.') |> Array.last
if wanted "pdfium" then
    entries.Add
        { Component = "PDFium (the bblanchon.PDFium native libraries' source)"
          Version = $"chromium/{branch}"
          Licence = "BSD-3-Clause (some Apache-2.0)"
          Source = $"https://pdfium.googlesource.com/pdfium/+/refs/heads/chromium/{branch}"
          Fetched =
            fetchGit "https://pdfium.googlesource.com/pdfium" $"refs/heads/chromium/{branch}" $"pdfium/pdfium-chromium-{branch}.tar.gz"
            |> Result.map (fun file -> $"{file} (its third-party code comes from its DEPS when it's built: see pdfium-binaries' build scripts, in its repository's archive)") }

// ---- Data: the models, tessdata and fonts (pinned files, not built) ----

let layoutRevision = property "Directory.Build.props" "LayoutModelRevision"
let tableRevision = property "Directory.Build.props" "TableModelRevision"
let tessdata = property "Directory.Build.props" "TessdataVersion"
let urwFonts = property "Directory.Build.props" "UrwFontsVersion"
let liberationFonts = property "Directory.Build.props" "LiberationFontsVersion"
for name, version, licence, source in
    [ "PP-DocLayoutV3 layout model", layoutRevision, "Apache-2.0", $"https://huggingface.co/PaddlePaddle/PP-DocLayoutV3/tree/{layoutRevision}"
      "Table Transformer model (8-bit ONNX)", tableRevision, "MIT", $"https://huggingface.co/Xenova/table-transformer-structure-recognition/tree/{tableRevision}"
      "Table Transformer (training and model code)", "1.0", "MIT", "https://github.com/microsoft/table-transformer"
      "tessdata_fast eng.traineddata", tessdata, "Apache-2.0", $"https://github.com/tesseract-ocr/tessdata_fast/tree/{tessdata}"
      "tessdata_fast spa.traineddata", tessdata, "Apache-2.0", $"https://github.com/tesseract-ocr/tessdata_fast/tree/{tessdata}"
      "URW base 35 fonts (TrueType)", urwFonts, "AGPL-3.0 with a font exception", $"https://github.com/ArtifexSoftware/urw-base35-fonts/tree/{urwFonts}"
      "Liberation fonts (TrueType)", liberationFonts, "OFL-1.1", $"https://github.com/liberationfonts/liberation-fonts/releases/tag/{liberationFonts}" ] do
    entries.Add
        { Component = name
          Version = version
          Licence = licence
          Source = source
          Fetched = Error "data, bundled as pinned (Directory.Build.props): not built from source" }

// ---- The manifest ----

let cell (s: string) = s.Replace("|", "\\|")
let row (e: Entry) =
    let here = match e.Fetched with Ok file -> $"`{file}`" | Error why -> $"— {why}"
    $"| {cell e.Component} | {cell e.Version} | {cell e.Licence} | {cell e.Source} | {cell here} |"
/// The dependencies' fingerprint: their rows (all but Legibyl's own), sorted, hashed.
let fingerprint =
    let rows = entries |> Seq.filter (fun e -> e.Component <> "Legibyl") |> Seq.map row |> Seq.sort |> String.concat "\n"
    let hash = Convert.ToHexString(SHA256.HashData(Text.Encoding.UTF8.GetBytes rows)).ToLowerInvariant()
    $"deps-{hash.Substring(0, 12)}"
let today = DateTime.UtcNow.ToString "yyyy-MM-dd"
let dry = if lookupOnly then " (its dependencies looked up: not fetched)" else ""
let lines =
    [ $"# Source for Legibyl {version}"
      ""
      $"Built from commit `{commit}`. Gathered by `tools/source-bundle.fsx` on {today}{dry}."
      $"Each component, at the version Legibyl {version} is built with; the file here with its source, or why there's none."
      ""
      $"The dependencies' source (all but Legibyl's own) is the release `{fingerprint}`: the fingerprint of their rows below, the same for every release of Legibyl built with them."
      ""
      "| Component | Version | Licence | Source | Here |"
      "|---|---|---|---|---|" ]
    @ [ for e in entries -> row e ]
Directory.CreateDirectory out |> ignore
File.WriteAllLines(Path.Combine(out, "MANIFEST.md"), lines)
let missing = entries |> Seq.filter (fun e -> match e.Fetched with Error why -> not (why.StartsWith "data") && not (why.Contains "--all") | Ok _ -> false) |> List.ofSeq
let manifest = Path.Combine(out, "MANIFEST.md")
printfn $"Wrote {manifest}: {entries.Count} components, {missing.Length} without source here."
for e in missing do
    match e.Fetched with
    | Error why -> printfn $"  {e.Component.Trim()} {e.Version}: {why}"
    | Ok _ -> ()
if not missing.IsEmpty then
    eprintfn "Some sources couldn't be found (above): not packed, and no fingerprint."
    exit 1
printfn $"The dependencies' fingerprint: {fingerprint}"
if fingerprintOnly then
    printfn $"{fingerprint}"
    exit 0

// ---- Packing for a release ----

/// Writes to files named base.part01, base.part02..., each at most size bytes.
type private Parts(path: string, size: int64) =
    inherit Stream()
    let written = ResizeArray<string>()
    let mutable current: FileStream | null = null
    let mutable inCurrent = 0L
    let next () =
        match current with
        | NonNull file -> file.Dispose()
        | Null -> ()
        let name = $"{path}.part{written.Count + 1:D2}"
        written.Add name
        current <- File.Create name
        inCurrent <- 0L
    member _.Files = List.ofSeq written
    override _.CanRead = false
    override _.CanSeek = false
    override _.CanWrite = true
    override _.Length = raise (NotSupportedException())
    override _.Position
        with get () = raise (NotSupportedException())
        and set _ = raise (NotSupportedException())
    override _.Flush() = match current with NonNull file -> file.Flush() | Null -> ()
    override _.Read(_, _, _) = raise (NotSupportedException())
    override _.Seek(_, _) = raise (NotSupportedException())
    override _.SetLength _ = raise (NotSupportedException())
    override _.Write(buffer: byte array, offset: int, count: int) =
        let mutable offset = offset
        let mutable count = count
        while count > 0 do
            if isNull current || inCurrent = size then next ()
            let n = int (min (int64 count) (size - inCurrent))
            (nonNull current).Write(buffer, offset, n)
            inCurrent <- inCurrent + int64 n
            offset <- offset + n
            count <- count - n
    override _.Dispose(disposing: bool) =
        if disposing then
            match current with
            | NonNull file -> file.Dispose()
            | Null -> ()
        base.Dispose disposing

if not dryRun && only.IsEmpty then
    let release = Path.Combine(Path.GetDirectoryName out |> string, "release")
    Directory.CreateDirectory release |> ignore
    let name = $"legibyl-{version}"
    // What the manifest lists, once each: not whatever an earlier run left in the folder.
    let listed =
        entries
        |> Seq.choose (fun e -> match e.Fetched with Ok file -> Some((file.Split " (").[0]) | Error _ -> None)
        |> Seq.distinct
        |> Seq.map (fun file -> Path.Combine(out, file))
        |> Seq.filter File.Exists
        |> Seq.sort
        |> List.ofSeq
    let files (folder: string) =
        let prefix = Path.Combine(out, folder) + string Path.DirectorySeparatorChar
        listed |> List.filter (fun file -> file.StartsWith prefix)
    let manifest = Path.Combine(out, "MANIFEST.md")
    let add (writer: TarWriter) (top: string) (file: string) =
        let relative = Path.GetRelativePath(out, file).Replace(Path.DirectorySeparatorChar, '/')
        writer.WriteEntry(file, $"{top}/{relative}")
    let sums (files: string list) =
        [ for file in files ->
              use stream = File.OpenRead file
              $"{Convert.ToHexString(SHA256.HashData stream).ToLowerInvariant()}  {Path.GetFileName file}" ]
    let gib (bytes: int64) = float bytes / 1073741824.0
    // Legibyl's own, with the manifest: small, one file, for the release of Legibyl.
    let own = Path.Combine(release, $"{name}-source.tar")
    do
        use stream = File.Create own
        use writer = new TarWriter(stream, TarEntryFormat.Pax, false)
        for file in manifest :: files "legibyl" do add writer $"{name}-source" file
    printfn ""
    printfn $"Legibyl's own source, for its release: {own} ({gib (FileInfo(own).Length):F2} GiB)"
    if not ownOnly then
        // The dependencies', for their own release: one tar in parts under 1.9 GiB, with the
        // manifest, their checksums and how to join them.
        let folder = Path.Combine(release, fingerprint)
        if Directory.Exists folder then Directory.Delete(folder, true)
        Directory.CreateDirectory folder |> ignore
        let partSize = 1900L * 1024L * 1024L
        let parts = new Parts(Path.Combine(folder, $"{fingerprint}-source.tar"), partSize)
        do
            use writer = new TarWriter(parts, TarEntryFormat.Pax, true)
            for file in manifest :: (List.concat [ files "github"; files "git"; files "maven"; files "pdfium" ]) do add writer $"{fingerprint}-source" file
        parts.Flush()
        let partFiles = parts.Files
        parts.Dispose()
        File.Copy(manifest, Path.Combine(folder, "MANIFEST.md"))
        File.WriteAllLines(Path.Combine(folder, "SHA256SUMS"), sums (partFiles @ [ Path.Combine(folder, "MANIFEST.md") ]))
        let partNames = partFiles |> List.map Path.GetFileName |> String.concat " "
        let windowsParts = partFiles |> List.map Path.GetFileName |> String.concat "+"
        File.WriteAllLines(
            Path.Combine(folder, "SOURCE-README.md"),
            [ $"# Legibyl's dependencies' source: {fingerprint}"
              ""
              $"The source of everything Legibyl is built with and ships, for each release of Legibyl that names `{fingerprint}`: every release built with this set of dependencies. (Gathered at Legibyl {version}, commit `{commit}`.) Each release of Legibyl has its own source, `legibyl-<version>-source.tar`."
              ""
              $"- `MANIFEST.md` lists every component: its version, licence, and where its source is in the tar."
              $"- `{fingerprint}-source.tar.part*`: their source, one tar in {partFiles.Length} parts (each under 2 GiB, for GitHub). Join them, then unpack:"
              ""
              "```sh"
              $"cat {partNames} > {fingerprint}-source.tar"
              $"tar xf {fingerprint}-source.tar"
              "```"
              ""
              $"(On Windows: `copy /b {windowsParts} {fingerprint}-source.tar`.) `SHA256SUMS` has each file's checksum." ])
        let total = partFiles |> List.sumBy (fun f -> FileInfo(f).Length)
        printfn $"The dependencies' source, for the release {fingerprint}: {folder}, {partFiles.Length} parts, {gib total:F2} GiB in all"
        for file in partFiles do printfn $"  {Path.GetFileName file}: {gib (FileInfo(file).Length):F2} GiB"
        let largest =
            listed
            |> List.map (fun f -> f, FileInfo(f).Length)
            |> List.sortByDescending snd
            |> List.truncate 8
        printfn "Largest sources:"
        for file, size in largest do printfn $"  {Path.GetRelativePath(out, file)}: {float size / 1048576.0:F0} MB"
