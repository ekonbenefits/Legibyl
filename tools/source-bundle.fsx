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
//   - the models' and tessdata's pinned files are listed (they're data, already pinned, not built).
//
// Left out unless --all (but listed): the .NET runtime and base libraries (dotnet/runtime,
// dotnet/dotnet: system libraries, several GB), packages only used to build (Dotnet.Bundle,
// IKVM's MSBuild tools, Avalonia's build services), and submodules that are build tooling, not
// part of what's built (compilers, depot_tools, code generators, test suites: see isTooling). A
// file already fetched isn't fetched again.
// Set GITHUB_TOKEN (or be signed in to `gh`) to resolve submodules past GitHub's anonymous limit.

open System
open System.Diagnostics
open System.IO
open System.IO.Compression
open System.Net
open System.Net.Http
open System.Text.Json
open System.Text.RegularExpressions

let args = fsi.CommandLineArgs |> Array.skip 1 |> List.ofArray
let dryRun = List.contains "--dry-run" args
let everything = List.contains "--all" args
let only =
    match args |> List.tryFindIndex ((=) "--only") with
    | Some i when i + 1 < args.Length -> args[i + 1].Split(',') |> Array.map (fun s -> s.Trim().ToLowerInvariant()) |> List.ofArray
    | _ -> []
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

/// Saves url as file under out (unless it's there already, or it's a dry run); its size in bytes.
let fetch (url: string) (file: string) : Result<string, string> =
    let path = Path.Combine(out, file)
    if File.Exists path then
        Ok file
    elif dryRun then
        if exists url then Ok file else Error $"not found: {url}"
    else
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
        with ex -> Error $"{ex.Message} ({url})"

/// Where some repositories really are, when what refers to them names an old mirror.
let private moved =
    Map.ofList [ "https://github.com/Mesa3D/mesa", "https://gitlab.freedesktop.org/mesa/mesa.git" ]

/// A git repository not on GitHub, at a commit or branch, as a tarball: fetched shallowly with git
/// and archived (servers like googlesource won't archive a large tree over HTTP).
let fetchGit (url: string) (reference: string) (file: string) : Result<string, string> =
    let url = Map.tryFind (url.TrimEnd('/')) moved |> Option.defaultValue url
    let path = Path.Combine(out, file)
    if File.Exists path then
        Ok file
    elif dryRun then
        match run "git" $"ls-remote {url}" with
        | Some _ -> Ok file
        | None -> Error $"can't reach {url}"
    else
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
            try Directory.Delete(scratch, true) with _ -> ()

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
    let m = Regex.Match(url, @"github\.com[/:]([^/]+)/([^/]+?)(?:\.git)?/?$")
    if m.Success then Some(m.Groups[1].Value, m.Groups[2].Value) else None

let fetched = Collections.Generic.Dictionary<string, Result<string, string>>()

/// Whether a submodule's path is build tooling rather than code built into what ships: build
/// scripts, toolchains, code generators' Python libraries, test suites (ANGLE's and Skia's have
/// many).
let isTooling (path: string) =
    let path = path.ToLowerInvariant()
    Regex.IsMatch(path, @"(^|/)(build|buildtools|testing|tools|test|tests)(/|$)")
    || Regex.IsMatch(
        path,
        @"(^|/)(depot_tools|clang|rust|jinja2|markupsafe|python-markdown|six|nasm|catapult|googletest|benchmark|android_[^/]*|llvm-build|gn|ninja|[^/]*conform[^/]*|[^/]*_cts)(/|$)"
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

// ---- Data: the models and tessdata (pinned files, not built) ----

let layoutRevision = property "Directory.Build.props" "LayoutModelRevision"
let tableRevision = property "Directory.Build.props" "TableModelRevision"
let tessdata = property "Directory.Build.props" "TessdataVersion"
for name, version, licence, source in
    [ "PP-DocLayoutV3 layout model", layoutRevision, "Apache-2.0", $"https://huggingface.co/PaddlePaddle/PP-DocLayoutV3/tree/{layoutRevision}"
      "Table Transformer model (8-bit ONNX)", tableRevision, "MIT", $"https://huggingface.co/Xenova/table-transformer-structure-recognition/tree/{tableRevision}"
      "Table Transformer (training and model code)", "1.0", "MIT", "https://github.com/microsoft/table-transformer"
      "tessdata_fast eng.traineddata", tessdata, "Apache-2.0", $"https://github.com/tesseract-ocr/tessdata_fast/tree/{tessdata}" ] do
    entries.Add
        { Component = name
          Version = version
          Licence = licence
          Source = source
          Fetched = Error "data, bundled as pinned (Directory.Build.props): not built from source" }

// ---- The manifest ----

let cell (s: string) = s.Replace("|", "\\|")
let today = DateTime.UtcNow.ToString "yyyy-MM-dd"
let dry = if dryRun then " (a dry run: nothing fetched)" else ""
let lines =
    [ $"# Source for Legibyl {version}"
      ""
      $"Built from commit `{commit}`. Gathered by `tools/source-bundle.fsx` on {today}{dry}."
      $"Each component, at the version Legibyl {version} is built with; the file here with its source, or why there's none."
      ""
      "| Component | Version | Licence | Source | Here |"
      "|---|---|---|---|---|" ]
    @ [ for e in entries ->
            let here = match e.Fetched with Ok file -> $"`{file}`" | Error why -> $"— {why}"
            $"| {cell e.Component} | {cell e.Version} | {cell e.Licence} | {cell e.Source} | {cell here} |" ]
Directory.CreateDirectory out |> ignore
File.WriteAllLines(Path.Combine(out, "MANIFEST.md"), lines)
let missing = entries |> Seq.filter (fun e -> match e.Fetched with Error why -> not (why.StartsWith "data") && not (why.Contains "--all") | Ok _ -> false) |> List.ofSeq
let manifest = Path.Combine(out, "MANIFEST.md")
printfn $"Wrote {manifest}: {entries.Count} components, {missing.Length} without source here."
for e in missing do
    match e.Fetched with
    | Error why -> printfn $"  {e.Component.Trim()} {e.Version}: {why}"
    | Ok _ -> ()
