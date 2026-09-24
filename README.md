# veraPDF Tool

A .NET (WPF) desktop GUI for PDF/UA (and PDF/A) validation, powered by the
real [veraPDF](https://verapdf.org/) Java validation engine embedded via
[IKVM.NET](https://ikvm.org/) — no separate JDK/JRE install or external
process required.

## How it works

veraPDF's `greenfield-apps` fat jar (bundling its Greenfield validation
core) is vendored under `lib/vendor/verapdf-jars/` and referenced from the
`VeraPdfTool.Validation` project via IKVM's `<IkvmReference>` MSBuild item,
which translates the Java bytecode into a .NET assembly at build time. The
`VeraPdfTool.Validation` project exposes a small, plain-C# API
(`IPdfValidator.Validate(path, profile) -> ValidationReport` and
`IPdfFixer.Fix(path, profile, outputPath) -> FixResult`) that fully
isolates all Java/IKVM types from the rest of the app. `IPdfFixer` wraps
veraPDF's metadata fixer, which repairs XMP metadata only (missing
metadata stream, PDF/A|PDF/UA identifier, false conformance claims,
Info-dictionary sync) — it does not repair structural/tagging issues.

Each failed check carries a `locationContext` — a slash-separated walk of
veraPDF's internal object model (e.g.
`root/document[0]/pages[1](9 0 obj PDLinkAnnot)`) pinpointing exactly where
in the document the problem is. `PdfLocationParser` turns this into a page
number and a plain-English breadcrumb (e.g. "Page 2 — Document → Link
annotation"), used by the GUI's results grid (see below) and available on
`RuleViolation.Location`/`PageNumber` for any future consumer.

## Solution layout

```
VeraPdfTool.slnx
src/
  VeraPdfTool.Validation/   C# facade over the veraPDF Java validation core (IKVM)
  VeraPdfTool.Rendering/    Renders a page image with a highlight box around a failure's region (PdfPig + SkiaSharp)
  VeraPdfTool.Gui/          WPF UI: file picker, profile selector, results grid
  VeraPdfTool.Cli/          Command-line tool (verapdf-tool.exe), JSON-first, agent-friendly
tests/
  VeraPdfTool.Validation.Tests/   xUnit tests using real PDF/UA-1 corpus samples
installer/
  VeraPdfTool.Gui.Installer/      WiX v6 MSI installer for the GUI (VeraPdfToolSetup.msi)
lib/vendor/verapdf-jars/    Vendored veraPDF jar + provenance/checksum README
licenses/                   GPLv3+/MPLv2+ license texts and third-party NOTICE
spike/IkvmSpike/            Standalone feasibility spike (kept for reference)
```

## Building & running

Requires the .NET 8 SDK.

```powershell
dotnet build VeraPdfTool.slnx
dotnet run --project src\VeraPdfTool.Gui\VeraPdfTool.Gui.csproj
```

## Testing

```powershell
dotnet test tests\VeraPdfTool.Validation.Tests\VeraPdfTool.Validation.Tests.csproj
```

## Publishing (self-contained)

```powershell
dotnet publish src\VeraPdfTool.Gui\VeraPdfTool.Gui.csproj -c Release -r win-x64 --self-contained true -o publish\gui
```

The published output bundles IKVM's translated JRE (`IKVM.Java.dll`), so
the app runs standalone with no external Java installation.

## Windows installer (MSI)

`installer/VeraPdfTool.Gui.Installer` builds a WiX v6 MSI (`VeraPdfToolSetup.msi`)
that installs the self-contained GUI to `Program Files\Ekon Benefits\veraPDF Tool`,
with Start Menu and Desktop shortcuts. It publishes the GUI itself as part of
the build (no separate publish step needed):

```powershell
dotnet build installer\VeraPdfTool.Gui.Installer\VeraPdfTool.Gui.Installer.wixproj -c Release
```

The MSI is written to `artifacts\msi\en-US\VeraPdfToolSetup.msi`. Installing
to `Program Files` requires administrator privileges (standard per-machine
MSI behavior). The installer also bundles the `LICENSE`/`licenses/` files
so the license texts and third-party attribution are available on the
installed machine, not just in this source repository.

## Supported profiles

PDF/UA-1 (default), PDF/A-1b, PDF/A-2b, PDF/A-2u, PDF/A-3b, PDF/A-4.

## GUI results view

The results grid is sorted by page number by default (unlocated failures —
those with no `pages[N]` in their context, such as document-level metadata
checks — sort to the end). Each row shows a friendly "Document → Page 2 →
Link annotation" style location instead of the raw veraPDF context string
(still available as a tooltip for power users). Selecting a row with a
known page enables **Open at Page…**, which launches the PDF in your
default viewer at that page via a `#page=N` link fragment — this is
best-effort, since not every PDF viewer honors it (Adobe Reader, Edge, and
SumatraPDF do; some others ignore it and just open at page 1).

For failures anchored to an annotation (links, form fields — the most
common failure type with an exact, checkable region), **Show Region…**
renders that page as an image with a highlight box drawn around the
failing annotation, using [PdfPig](https://github.com/UglyToad/PdfPig) to
read its `/Rect` and
[PdfPig.Rendering.Skia](https://github.com/BobLd/PdfPig.Rendering.Skia) +
SkiaSharp to rasterise the page and draw the box (`VeraPdfTool.Rendering`
project). Most structural/tagging failures (untagged paragraphs, missing
table header scope, etc.) aren't attached to page geometry in veraPDF's
model, so for those the full page is still shown for context, with a
banner noting the exact region isn't available.

## Command-line tool (for scripts / AI agents)

`VeraPdfTool.Cli` builds a standalone `verapdf-tool` executable intended for
use in scripts or by an AI agent iterating on PDF/UA compatibility fixes.
Output defaults to structured JSON on stdout, and the **process exit code**
tells you the outcome without needing to parse output:

| Exit code | Meaning |
|-----------|---------|
| `0`  | All files validated and are compliant. |
| `1`  | All files validated successfully, but at least one is non-compliant. |
| `2`  | At least one file could not be validated (missing/corrupt/encrypted). |
| `64` | Invalid command-line usage. |

```powershell
dotnet run --project src\VeraPdfTool.Cli\VeraPdfTool.Cli.csproj -- <file.pdf> [<file2.pdf> ...] [options]
```

Or after building, run the exe directly:
`src\VeraPdfTool.Cli\bin\Debug\net8.0\verapdf-tool.exe <file.pdf>`

Options:
- `--profile <name>` — profile to validate against (default `pdfua1`). Run
  `--list-profiles` for all accepted names (`pdfua1`, `pdfa1b`, `pdfa2b`,
  `pdfa2u`, `pdfa3b`, `pdfa4`).
- `--format <json|text>` — output format (default `json`).
- `--output <path>` — write output to a file instead of stdout.
- `--fix` — also attempt to repair the file's XMP **metadata** via veraPDF's
  metadata fixer, writing a repaired copy next to the original as
  `<name>.fixed<ext>`. Only metadata is repaired (missing metadata stream,
  missing PDF/A|PDF/UA identifier, false conformance claims, Info-dictionary
  sync) — it does **not** fix structural/tagging issues (untagged content,
  missing alt-text, table header scope, etc). Note that veraPDF's fixer only
  adds a missing identifier/metadata stream when *every* failing check is a
  metadata-only check per its `PDFUA_1`/`PDFA_*` "processed objects" list; if
  the file has any structural failures too, it reports `WontFix` and leaves
  metadata largely untouched (see `fixAppliedFixes`/`fixStatus` in the output
  to see exactly what happened).
- `--fix-output <path>` — use with `--fix` and exactly one input file to
  write the repaired copy to an exact path instead of the auto-derived name.
- `--list-profiles` — print supported profile names and exit `0`.
- `--version` — print tool build + vendored veraPDF engine version and exit `0`.
- `--help` / `-h` — print usage and exit `0`.

Multiple PDF paths can be passed at once (batch mode); output is always a
JSON array (one object per file) or one text line/block per file.

Example JSON output (one non-compliant file):

```json
[
  {
    "file": "document.pdf",
    "profile": "pdfua1",
    "flavourId": "ua1",
    "isCompliant": false,
    "totalAssertions": 794,
    "failedCount": 2,
    "failures": [
      {
        "specification": "ISO 14289-1:2014",
        "clause": "7.1",
        "testNumber": 1,
        "message": "Content marked as Artifact should not be present inside tagged content",
        "errorMessage": null,
        "locationContext": null
      }
    ],
    "error": null
  }
]
```

If a file can't be validated at all (missing, corrupt, encrypted), its
entry has `"error": "<message>"` and the other fields are `null` — check
`error` first before reading `isCompliant`.

## Versioning

All projects share a single version, set centrally in `Directory.Build.props`:
the first three components (`Major.Minor.Patch`) track the **vendored veraPDF
engine version** (currently `1.28.1`), and the fourth component is a
build/revision number that increments for tool-only releases (CLI/GUI/facade
changes that don't touch the vendored jar). When the vendored jar is
upgraded, the revision resets to `0` and the base is updated to match the new
jar version. Run `verapdf-tool --version` to see both numbers at once
(e.g. `verapdf-tool 1.28.1.0 (vendored veraPDF engine 1.28.1)`).

## Licensing

This project's own code (everything outside `lib/vendor/`) is licensed
under the **Mozilla Public License 2.0** — see `LICENSE`.

It bundles the veraPDF `greenfield-apps` jar, which is separately
dual-licensed by the veraPDF Consortium under GPLv3+ / MPLv2+ (see
`licenses/LICENSE.GPL` and `licenses/LICENSE.MPL`), and uses IKVM.NET
(Apache 2.0). See `licenses/NOTICE.md` for full third-party attribution.
