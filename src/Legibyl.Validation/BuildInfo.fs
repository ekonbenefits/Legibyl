/// What this build is: its version, and the versions of what it's built on, as `legibyl --version`
/// and the GUI's About window say them (both from Directory.Build.props, not typed in).
module Legibyl.Validation.BuildInfo

open System.Reflection

/// This build's version, e.g. "1.28.1.0" (Version in Directory.Build.props).
let version = string (typeof<VeraPdfValidator>.Assembly.GetName().Version)

/// The line `legibyl --version` prints, which a bug report quotes: "legibyl 1.28.1.0 (vendored
/// veraPDF engine 1.28.1)".
let versionLine = $"legibyl {version} (vendored veraPDF engine {VeraPdfValidator.EngineVersion})"

/// A library's version, from its assembly: the package version it was built as (without the
/// commit some add), else its assembly version.
let libraryVersion (assembly: Assembly) =
    match assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>() with
    | NonNull attribute when not (System.String.IsNullOrWhiteSpace attribute.InformationalVersion) ->
        attribute.InformationalVersion.Split('+').[0]
    | _ -> string (assembly.GetName().Version)

/// What validating and repairing run on, each with its version and what it does here.
let validationComponents () =
    [ "veraPDF", VeraPdfValidator.EngineVersion, "validation against PDF/UA"
      "IKVM.NET", libraryVersion typeof<IKVM.Runtime.JVM>.Assembly, "runs veraPDF's Java in .NET"
      "iText", libraryVersion typeof<iText.Kernel.Pdf.PdfDocument>.Assembly, "reading and repairing PDFs"
      "PdfPig", libraryVersion typeof<UglyToad.PdfPig.PdfDocument>.Assembly, "reading pages' text and drawing" ]
