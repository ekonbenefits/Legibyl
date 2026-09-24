# Legibyl

An easy-to-use wrapper around [veraPDF](https://verapdf.org/): a .NET (WPF)
desktop GUI plus a scriptable CLI for PDF/UA (and PDF/A) validation, powered
by the real veraPDF Java validation engine embedded via
[IKVM.NET](https://ikvm.org/) — no separate JDK/JRE install or external
process required.

## How it works

veraPDF's `greenfield-apps` fat jar (bundling its Greenfield validation
core) is vendored under `lib/vendor/verapdf-jars/` and referenced from the
`Legibyl.Validation` project via IKVM's `<IkvmReference>` MSBuild item,
which translates the Java bytecode into a .NET assembly at build time. The
`Legibyl.Validation` project exposes a small, plain-C# API
(`IPdfValidator.Validate(path, profile) -> ValidationReport`,
`IPdfFixer.Fix(path, profile, outputPath) -> FixResult`, and
`IStructureTreeReader.ReadStructureTree(path) -> StructureNode?`) that fully
isolates all Java/IKVM types from the rest of the app. `IPdfFixer` combines
veraPDF's metadata fixer with a small iText-based structural repair pass for
selected safe object-level fixes, such as setting page `/Tabs /S` when a
page has annotations and removing empty semantic `/P` structure elements.
`IStructureTreeReader` walks veraPDF's PD model's `StructTreeRoot` (the
tagged logical structure tree) directly, independent of running a
validation pass.

Each failed check carries a `locationContext` — a slash-separated walk of
veraPDF's internal object model (e.g.
`root/document[0]/pages[1](9 0 obj PDLinkAnnot)`) pinpointing exactly where
in the document the problem is. `PdfLocationParser` turns this into a page
number and a plain-English breadcrumb (e.g. "Page 2 — Document → Link
annotation"), used by the GUI's results grid (see below) and available on
`RuleViolation.Location`/`PageNumber` for any future consumer.

## Solution layout

```
Legibyl.slnx
src/
  Legibyl.Validation/   C# facade over the veraPDF Java validation core (IKVM)
  Legibyl.Rendering/    Renders a page image with a highlight box around a failure's region (PdfPig + SkiaSharp)
  Legibyl.Gui/          WPF UI: file picker, profile selector, results grid
  Legibyl.Cli/          Command-line tool (legibyl.exe), JSON-first, agent-friendly
tests/
  Legibyl.Validation.Tests/   xUnit tests using real PDF/UA-1 corpus samples
installer/
  Legibyl.Gui.Installer/      WiX v6 MSI installer for the GUI (LegibylSetup.msi)
lib/vendor/verapdf-jars/    Vendored veraPDF jar + provenance/checksum README
licenses/                   AGPLv3+/GPLv3+/MPLv2+ license texts and third-party NOTICE
spike/IkvmSpike/            Standalone feasibility spike (kept for reference)
```

## Building & running

Requires the .NET 8 SDK.

```powershell
dotnet build Legibyl.slnx
dotnet run --project src\Legibyl.Gui\Legibyl.Gui.csproj
```

## Testing

```powershell
dotnet test tests\Legibyl.Validation.Tests\Legibyl.Validation.Tests.csproj
```

## Publishing (self-contained)

```powershell
dotnet publish src\Legibyl.Gui\Legibyl.Gui.csproj -c Release -r win-x64 --self-contained true -o publish\gui
```

The published output bundles IKVM's translated JRE (`IKVM.Java.dll`), so
the app runs standalone with no external Java installation.

## Windows installer (MSI)

`installer/Legibyl.Gui.Installer` builds a WiX v6 MSI (`LegibylSetup.msi`)
that installs the self-contained GUI to `Program Files\Ekon Benefits\Legibyl`,
with Start Menu and Desktop shortcuts. It publishes the GUI itself as part of
the build (no separate publish step needed):

```powershell
dotnet build installer\Legibyl.Gui.Installer\Legibyl.Gui.Installer.wixproj -c Release
```

The MSI is written to `artifacts\msi\en-US\LegibylSetup.msi`. Installing
to `Program Files` requires administrator privileges (standard per-machine
MSI behavior). The installer also bundles the `LICENSE`/`licenses/` files
so the license texts and third-party attribution are available on the
installed machine, not just in this source repository.

## Supported profiles

PDF/UA-1 (default), PDF/A-1b, PDF/A-2b, PDF/A-2u, PDF/A-3b, PDF/A-4.

## GUI results view

The results list is sorted by page number by default (unlocated failures —
those with no `pages[N]` in their context, such as document-level metadata
checks — sort to the end). It keeps only the short clause, test, page, and
friendly location columns visible. Selecting a row shows its full, wrapped
message, friendly location, raw veraPDF location context, and page preview
in a resizable details panel beside the list. All detail text is selectable,
and **Copy Finding** copies the selected rule, message, error detail, and
both location forms. **Copy Summary** and **Save Report** sit below the
results list and include the same location information for every finding.
Legibyl also adds warning rows for fixable cleanup issues that veraPDF does
not report directly, such as empty semantic `/P` structure elements, and
marks known repairable rows in the **Fixable** column. It also warns about
adjacent sibling `/P` tags that look like visual line fragments from the same
sentence; those are intentionally warning-only because the safest repair is
usually in the source document (for example, the original Word file) rather
than rewriting paragraph semantics in the PDF after export.

For failures anchored to an annotation (links, form fields — the most
common failure type with an exact, checkable region), the details panel
automatically renders that page with a highlight box around the failing
annotation. The image scales to the available panel space rather than
opening at 100% zoom. Rendering uses
[PdfPig](https://github.com/UglyToad/PdfPig) to read its `/Rect` and
[PdfPig.Rendering.Skia](https://github.com/BobLd/PdfPig.Rendering.Skia) +
SkiaSharp to rasterise the page and draw the box (`Legibyl.Rendering`
project). Most structural/tagging failures (untagged paragraphs, missing
table header scope, etc.) aren't attached to page geometry in veraPDF's
model, so for those the full page is still shown for context, with a
banner noting the exact region isn't available.

## Document structure (semantic hierarchy)

**View Structure…** shows the PDF's tagged logical structure tree — the
same heading/paragraph/list/table/link hierarchy and reading order that a
screen reader uses to present the document (the Catalog's `StructTreeRoot`).
It's read directly from veraPDF's own PD model (independent of a
validation pass), so it reflects exactly what the validator itself sees.
The default **Layout** tab renders that hierarchy like an HTML document with
visible borders around block elements, so nesting is obvious at a glance
(headings, lists, tables, links, figures, etc. use different colors).
Semantic tables are rendered as an actual grid: `Table` contains visual rows
from `TR`, and each row lays out `TH`/`TD` cells horizontally. Each box/cell
includes the semantic tag, page, language/role metadata, `Alt` or
`ActualText` when present, the element's own extracted visible text matched
by page + MCID, and for `/Form` elements the referenced widget annotation's
field name (`/T`), tooltip/user description (`/TU`), field type (`/FT`), and
current value (`/V`) when present. For `/Link` elements, Legibyl also reports
known targets: external URI actions and internal `/Dest`/GoTo destinations
such as `Internal destination: page 4 (/XYZ 33 690 0)`. Some PDFs attach the
destination only to the underlying link annotation, so Legibyl follows tagged
annotation references and uses a conservative same-page geometry fallback
when needed. Table-of-contents entries (`TOCI`) render as row-like layout
items and inherit the first descendant link target when the PDF stores the
click target on a separate child link fragment. The **Tree** tab keeps the compact outline view. Layout blocks
are selectable; when a selected node has same-page
geometry (from marked-content text glyph/image/path bounds, `/A /BBox`
layout attributes, or form widget rectangles), the side preview renders that
page and highlights the best-effort area the node covers. Pure containers,
content without a usable rectangle, or multi-page nodes show a note instead.
If the file has no structure tree at all (completely untagged), the button
reports that instead of an empty tree.
**Export JSON…** in that window saves the whole hierarchy — including each
node's `content` text, geometry, link targets, and form field metadata — as
JSON, the same shape produced by the CLI's `--structure` flag. This is intended for AI agents
or other tooling that needs to understand what text and interactive controls
are present under each semantic role, not just the raw PDF/UA rule failures.

## Command-line tool (for scripts / AI agents)

`Legibyl.Cli` builds a standalone `legibyl` executable intended for
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
dotnet run --project src\Legibyl.Cli\Legibyl.Cli.csproj -- <file.pdf> [<file2.pdf> ...] [options]
```

Or after building, run the exe directly:
`src\Legibyl.Cli\bin\Debug\net8.0\legibyl.exe <file.pdf>`

Options:
- `--profile <name>` — profile to validate against (default `pdfua1`). Run
  `--list-profiles` for all accepted names (`pdfua1`, `pdfa1b`, `pdfa2b`,
  `pdfa2u`, `pdfa3b`, `pdfa4`).
- `--format <json|text>` — output format (default `json`).
- `--output <path>` — write output to a file instead of stdout.
- `--fix` — also attempt to repair the file's XMP **metadata** via veraPDF's
  metadata fixer, writing a repaired copy next to the original as
  `<name>.fixed<ext>`. The CLI currently keeps the conservative metadata-only
  behavior (missing metadata stream, missing PDF/A|PDF/UA identifier, false
  conformance claims, Info-dictionary sync). The GUI **Fix…** dialog also
  exposes selectable structural cleanup options for page annotation tab order
  (`/Tabs /S`) and empty semantic `/P` structure elements; structural fixes
  run before the veraPDF metadata fixer so metadata is repaired against the
  already-cleaned copy in the same operation. Fixed copies are finalized with
  an iText rewrite and parsed again before being returned, so stale
  cross-reference/startxref offsets from intermediate repair output are not
  handed to the user. Intermediate repair files are created under the
  operating system temp directory, not beside the source PDF or requested
  output path. Empty `/P` cleanup is visible as a warning and fixable option,
  but it is unchecked by default in the GUI so users opt into structure-tree
  deletion. Neither path adds missing alt text, retags untagged content, or
  infers table headers.
- `--fix-output <path>` — use with `--fix` and exactly one input file to
  write the repaired copy to an exact path instead of the auto-derived name.
- `--structure` — include each file's tagged logical structure tree (the
  semantic heading/paragraph/list/table/link hierarchy a screen reader
  uses) in the output, under a `structure` key. Each node includes a
  `content` property containing the visible text directly owned by that
  semantic element (matched by page + MCID), plus `alt`, `actualText`,
  language, page, role-map, best-effort same-page `geometry`, and children.
  `/Form` nodes also include widget field metadata (`formFieldName`,
  `formFieldDescription`, `formFieldType`, `formFieldValue`) resolved from
  the referenced annotation's `/T`, `/TU`, `/FT`, and `/V`. In `--format text`
  mode this prints an indented outline with `text="..."`, `field="..."`, and
  `tooltip="..."` details instead.
  Absent/`null` `structure` with no `structureError` means the file has no
  structure tree at all (it's completely untagged).
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
jar version. Run `legibyl --version` to see both numbers at once
(e.g. `legibyl 1.28.1.0 (vendored veraPDF engine 1.28.1)`).

## Licensing

This project's own code (everything outside `lib/vendor/`) is licensed
under the **GNU Affero General Public License v3.0 or later** — see
`LICENSE`.

It bundles the veraPDF `greenfield-apps` jar, which is separately
dual-licensed by the veraPDF Consortium under GPLv3+ / MPLv2+ (see
`licenses/LICENSE.GPL` and `licenses/LICENSE.MPL`), and uses IKVM.NET
(Apache 2.0). See `licenses/NOTICE.md` for full third-party attribution.
