# veraPDF jar

The veraPDF "greenfield-apps" fat jar is the Java validation engine loaded
into the .NET process via [IKVM.NET](https://ikvm.org/). It is **not**
committed to this repo: `src/Legibyl.Validation/Legibyl.Validation.fsproj`
downloads it from Maven Central on the first build, verifies it against
`VeraPdfSha256` in `Directory.Build.props`, and caches it at
`src/Legibyl.Validation/obj/verapdf/`. The build fails on a checksum mismatch.

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

## WCAG 2.2 checks

Compiled into veraPDF's assembly with it (the `Compile` list of its
`IkvmReference`), since veraPDF finds its WCAG model by name
(`Class.forName("org.verapdf.gf.model.impl.sa.GFSAPDFDocument")`). Each is
downloaded from Maven Central and checked against its SHA-256 in
`src/Legibyl.Validation/Legibyl.Validation.fsproj` (`VeraPdfWcagJar` items).

| Artifact | Version | SHA-256 |
|---|---|---|
| `org.verapdf:wcag-validation` | `1.28.1` (VeraPdfVersion) | `56C45553D313A369BE7D027CE539B40B119783B910E1CB51B12CCC99A61B769A` |
| `org.verapdf:wcag-algorithms` | `1.28.1` (VeraPdfVersion) | `D03E7493D51AB770B3B33FC3EA7E730D6ABABD8E63B46DBFBD6F8BE7FA11D4DA` |

`wcag-algorithms` goes in without its `ContrastRatioConsumer`, which drew pages
with Apache PDFBox: the build takes it out (`RepackWcagAlgorithms`), and
Legibyl's copy of it, which draws them with PDFium, goes in instead
(`src/Legibyl.Validation/java/`). So PDFBox and the image readers it would need
aren't fetched or shipped. Jackson is left to veraPDF's own copy in the fat jar
(a newer one clashed).
veraPDF's `WCAG-2-2-Machine.xml` profile, from its validation-profiles repository
at tag `v1.28.1`, is committed unmodified in `src/Legibyl.Validation/Profiles/`
(SHA-256 `9AF0D629A83BC3B7BA0409C9CA07FE48059BBA916B78262BE8977F8DA7A9A1C4`).

To upgrade veraPDF, take its WCAG jars and the profile at the same version and
tag, update the pins, and carry the new release's `ContrastRatioConsumer`
changes over to Legibyl's copy (see `src/Legibyl.Validation/java/README.md`).

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

1. Check https://search.maven.org/artifact/org.verapdf.apps/greenfield-apps
   for the latest version.
2. In `Directory.Build.props`, set `VeraPdfVersion` to the new version and
   reset `VersionBuild` to `0` (`VersionPrefix` and the CLI's `--version`
   engine string follow `VeraPdfVersion` automatically).
3. Run `dotnet build`. It will fail with a SHA-256 mismatch that prints the
   new jar's actual hash. Confirm that hash independently (e.g. against
   the `.sha256`/`.sha1` published next to the jar on Maven Central), then set
   `VeraPdfSha256` to it and update the Provenance table above.
4. Re-run `dotnet build` and `dotnet test` to confirm the new jar still loads
   and validates correctly under IKVM.
