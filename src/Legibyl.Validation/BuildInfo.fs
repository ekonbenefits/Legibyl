/// What this build is: its version and copyright, and the versions of what it's built on, as `legibyl --version`
/// and the GUI's About window say them (both from Directory.Build.props, not typed in).
module Legibyl.Validation.BuildInfo

open System.Reflection

/// This build's version, e.g. "1.28.1.0" (Version in Directory.Build.props).
let version = string (typeof<VeraPdfValidator>.Assembly.GetName().Version)

/// The line `legibyl --version` prints, which a bug report quotes: "legibyl 1.28.1.0 (vendored
/// veraPDF engine 1.28.1)".
let versionLine = $"legibyl {version} (vendored veraPDF engine {VeraPdfValidator.EngineVersion})"

/// Legibyl's own copyright, e.g. "Copyright © 2026 Ekon Benefits" (Copyright in
/// Directory.Build.props).
let copyright =
    match typeof<VeraPdfValidator>.Assembly.GetCustomAttribute<AssemblyCopyrightAttribute>() with
    | NonNull attribute -> attribute.Copyright
    | Null -> ""

/// Where to write to its copyright holder.
let contact = "opensource@ekonbenefits.com"

/// A library's version, from its assembly: the package version it was built as (without the
/// commit some add), else its assembly version.
let libraryVersion (assembly: Assembly) =
    match assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>() with
    | NonNull attribute when not (System.String.IsNullOrWhiteSpace attribute.InformationalVersion) ->
        attribute.InformationalVersion.Split('+').[0]
    | _ -> string (assembly.GetName().Version)

/// A value this assembly was stamped with, if it was (from Directory.Build.props, or the build).
let private stampedValue (key: string) =
    typeof<VeraPdfValidator>.Assembly.GetCustomAttributes<AssemblyMetadataAttribute>()
    |> Seq.tryFind (fun a -> a.Key = key)
    |> Option.bind (fun a -> Option.ofObj a.Value)
    |> Option.filter (System.String.IsNullOrWhiteSpace >> not)

/// A version this assembly was stamped with (from Directory.Build.props).
let private stamped (key: string) = stampedValue key |> Option.defaultValue "unknown"

/// The release on GitHub with the source of everything this build is built with but Legibyl itself:
/// deps-<fingerprint> (see tools/source-bundle.fsx). Only a released build has one (the release
/// workflow sets DependencySource); one built here has none.
let dependencySource = stampedValue "DependencySource"

/// That release's page.
let dependencySourceAddress =
    dependencySource |> Option.map (fun release -> $"https://github.com/ekonbenefits/Legibyl/releases/tag/{release}")

/// Where to get it, as `legibyl --version` says it; None without one.
let dependencySourceLine = dependencySourceAddress |> Option.map (fun address -> $"Its dependencies' source: {address}")

/// What validating and fixing run on, each with its version and what it does here.
let validationComponents () =
    [ "veraPDF", VeraPdfValidator.EngineVersion, "validation against PDF/UA"
      "IKVM.NET", libraryVersion typeof<IKVM.Runtime.JVM>.Assembly, "runs veraPDF's Java in .NET"
      "iText", libraryVersion typeof<iText.Kernel.Pdf.PdfDocument>.Assembly, "reading and fixing PDFs"
      "PdfPig", libraryVersion typeof<UglyToad.PdfPig.PdfDocument>.Assembly, "reading pages' text and drawing"
      "URW base 35 fonts", stamped "UrwFontsVersion", "embedded in place of Helvetica, Times, Courier, Symbol and ZapfDingbats"
      "Liberation fonts", stamped "LiberationFontsVersion", "embedded in place of Arial, Times New Roman and Courier New" ]
