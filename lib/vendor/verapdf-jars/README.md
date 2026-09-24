# Vendored veraPDF jars

This directory contains a vendored copy of the veraPDF "greenfield-apps" fat
jar, used as the Java validation engine loaded into the .NET process via
[IKVM.NET](https://ikvm.org/).

## Provenance

| Field            | Value |
|------------------|-------|
| Artifact         | `org.verapdf.apps:greenfield-apps` |
| Version          | `1.28.1` |
| Source           | Maven Central: `https://repo1.maven.org/maven2/org/verapdf/apps/greenfield-apps/1.28.1/greenfield-apps-1.28.1.jar` |
| File             | `greenfield-apps-1.28.1.jar` |
| SHA-256          | `916049DB6C6335A4A4C624E098499CFF152EF523F65693A0D835AEC635B6DCBC` |

This is a "fat jar" (jar-with-dependencies) built by veraPDF's own release
process; it bundles `validation-model`, `core`, `parser`, `pdf-model`,
`verapdf-xmp-core`, `feature-reporting`, `metadata-fixer`, the `gui`
module, and third-party dependencies (Jackson, JAXB/`javax.xml.bind`,
`javax.activation`, etc.) into a single archive. Using the single fat jar
avoids having to hand-resolve veraPDF's Maven dependency graph.

## Why this exact version

`1.28.1` is the latest version of `org.verapdf.apps:greenfield-apps`
published to Maven Central at the time this project was started (the
veraPDF GitHub `master` branch was at an unreleased `1.31.0-SNAPSHOT` /
`1.31.1` parent, which is not available on Central). veraPDF's parent POM
pins `<java.version>1.8</java.version>`, matching IKVM's Java SE 8
compatibility target.

## Verified compatibility with IKVM 8.16.1

Confirmed via a standalone spike (`spike/IkvmSpike` in this repo's history)
referencing this jar with `<IkvmReference>`:
- Build succeeds with 0 errors (some non-fatal `IKVM0101`/`IKVM0117`
  warnings for a Java 9 multi-release-jar class and an internal
  `javax.activation` reflection path in JAXB — neither is on the PDF/UA
  validation code path).
- `VeraGreenfieldFoundryProvider.initialise()` + `Foundries.defaultInstance()`
  + `createParser`/`createValidator(PDFAFlavour.PDFUA_1, false)` +
  `validator.validate(parser)` correctly validate real PDF/UA-1 corpus
  files (`veraPDF-corpus` `PDF_UA-1/7.1 General/*.pdf`), returning accurate
  `isCompliant()` and per-rule `TestAssertion` failure messages.

## Licensing

veraPDF is dual-licensed under GPLv3+ and MPLv2+ (see `/licenses` at the
repo root). Redistributing this jar with the application requires
including those license texts and this attribution notice — see
`/licenses/NOTICE.md`.

## Updating the jar

To update to a newer veraPDF release:
1. Check https://search.maven.org/artifact/org.verapdf.apps/greenfield-apps
   for the latest version.
2. Download the new jar, recompute its SHA-256, and update this README.
3. Re-run the IKVM build (`dotnet build`) and the validation test suite to
   confirm the new jar still loads and validates correctly under IKVM.
4. Update `VersionPrefix` in `Directory.Build.props` (repo root) and
   `ProfileNames.VendoredEngineVersion` (`src/Legibyl.Cli/ProfileNames.cs`)
   to match the new jar version, and reset `VersionBuild` to `0` — see
   "Versioning" in the root README for why the tool's version is tied to
   this jar's version.
