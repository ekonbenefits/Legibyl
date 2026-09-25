# Legibyl

An easy-to-use wrapper around [veraPDF](https://verapdf.org/): a .NET (Avalonia)
desktop GUI plus a scriptable CLI for PDF/UA (and PDF/A) validation, powered
by the real veraPDF Java validation engine embedded via
[IKVM.NET](https://ikvm.org/) — no separate JDK/JRE install or external
process required.

## How it works

veraPDF's `greenfield-apps` fat jar (bundling its Greenfield validation
core) is downloaded from Maven Central on the first build (pinned by version
and SHA-256 in `Directory.Build.props`, cached under `obj/`) and referenced
from the `Legibyl.Validation` project via IKVM's `<IkvmReference>` MSBuild item,
which translates the Java bytecode into a .NET assembly at build time. The
`Legibyl.Validation` project exposes a small, plain .NET API
(`IPdfValidator.Validate(path, profile) -> ValidationReport`,
`IPdfFixer.Fix(path, profile, outputPath) -> FixResult`, and
`IStructureTreeReader.ReadStructureTree(path) -> StructureNode?`) that fully
isolates all Java/IKVM types from the rest of the app. `IPdfFixer` combines
veraPDF's metadata fixer with a small iText-based structural repair pass for
selected safe object-level fixes, such as setting page `/Tabs /S` when a
page has annotations, describing table-of-contents links from their own
text, naming the document, and removing empty semantic `/P` structure elements.
`IStructureTreeReader` walks veraPDF's PD model's `StructTreeRoot` (the
tagged logical structure tree) directly, independent of running a
validation pass.

Each failed check carries a `locationContext` — a slash-separated walk of
veraPDF's internal object model (e.g.
`root/document[0]/pages[1](9 0 obj PDLinkAnnot)`) pinpointing exactly where
in the document the problem is. `PdfLocationParser` turns this into a page
number and a plain-English breadcrumb (e.g. "Page 2 — Document → Link
annotation"), used by the GUI's results grid (see below) and available on
`RuleViolation.Location`/`PageNumber` for any future consumer. Where the
location pins down something drawn on the page — marked content (`{mcid:N}`),
a structure element, or an annotation — Legibyl also works out its bounding
box (`RuleViolation.Geometry`, in `FindingGeometry.fs`).

## Solution layout

```
Legibyl.slnx
src/
  Legibyl.Validation/   F# facade over the veraPDF Java validation core (IKVM)
  Legibyl.Rendering/    Renders a page image with a highlight box around a failure's region (PdfPig + SkiaSharp)
  Legibyl.Gui/          Avalonia + FuncUI UI (Windows/macOS/Linux): file picker, profile selector, results grid
  Legibyl.Cli/          Command-line tool (legibyl.exe), JSON-first, agent-friendly
tests/
  Legibyl.Validation.Tests/   xUnit v3 + FsUnit tests using real PDF/UA-1 corpus samples
  Legibyl.Gui.Tests/          xUnit v3 + FsUnit tests of the GUI on Avalonia's headless platform
tools/
  Legibyl.Screenshots/        Dev-only: saves GUI screenshots without a display (headless)
installer/
  Legibyl.Gui.Installer/      WiX v6 MSI installer for the GUI (LegibylSetup.msi)
lib/vendor/verapdf-jars/    veraPDF jar provenance/upgrade README (jar is fetched at build time)
licenses/                   AGPLv3+/GPLv3+/MPLv2+/Apache-2.0/MIT license texts and third-party NOTICE
```

## Building & running

Requires the .NET 8 SDK. The GUI, CLI, and tests build and run on Windows,
macOS, and Linux; only the MSI installer project is Windows-only, so the
solution build skips it (build it explicitly, see below).

```powershell
dotnet build Legibyl.slnx
dotnet run --project src/Legibyl.Gui/Legibyl.Gui.fsproj
```

## Testing

```powershell
dotnet test Legibyl.slnx
```

`Legibyl.Validation.Tests` covers validation, fixes, and rendering. It also
checks veraPDF against the PDF Association's labeled PDF/UA-1 technique
samples (see `tests/Legibyl.Validation.Tests/SampleCorpus/README.md`).
`Legibyl.Gui.Tests` opens sample PDFs in the real `MainWindow` on Avalonia's
headless platform (rendered with Skia, no display needed) and checks what's
on screen, e.g. that no results-grid column header is clipped.

## Screenshots

`tools/Legibyl.Screenshots` saves a PNG of the GUI with a PDF open, using the
same headless setup as the GUI tests, so it works over SSH or with the
display asleep. It shares `HeadlessWindow.fs` with `Legibyl.Gui.Tests`.

```powershell
dotnet run --project tools/Legibyl.Screenshots -- <pdf> <out.png> [--theme light|dark] [--select <row>] [--size 1200x720]
```

`--select` picks the results row (0-based) whose details and page preview
are shown; by default it's the first. Screenshots are 1× scale.

## Publishing (self-contained)

```powershell
dotnet publish src/Legibyl.Gui/Legibyl.Gui.fsproj -c Release -r win-x64 --self-contained true -o publish/gui
```

Use `-r osx-arm64`, `-r osx-x64`, or `-r linux-x64` for other platforms.

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

## macOS app

The GUI project builds a self-contained `Legibyl.app` with
[Dotnet.Bundle](https://github.com/egramtel/dotnet-bundle) (Info.plist values
are in `Legibyl.Gui.fsproj`, icon in `assets/Legibyl.icns`). Run it on a Mac:

```sh
dotnet msbuild src/Legibyl.Gui/Legibyl.Gui.fsproj -t:BundleApp -restore -p:Configuration=Release -p:RuntimeIdentifier=osx-arm64 -p:SelfContained=true -p:PublishDir=../../artifacts/macos/osx-arm64/
```

The app is written to `artifacts/macos/osx-arm64/Legibyl.app`; use
`osx-x64` for Intel Macs. It isn't signed or notarized yet, so macOS blocks
the first launch of a downloaded copy: allow it under System Settings >
Privacy & Security > **Open Anyway**, or run
`xattr -dr com.apple.quarantine Legibyl.app`. To share it as a disk image:

```sh
hdiutil create -volname Legibyl -srcfolder artifacts/macos/osx-arm64/Legibyl.app -ov -format UDZO artifacts/macos/Legibyl-osx-arm64.dmg
```

## Supported profiles

PDF/UA-1 (default), PDF/A-1b, PDF/A-2b, PDF/A-2u, PDF/A-3b, PDF/A-4.

## GUI results view

While a file validates, the progress bar fills page by page ("Page 57 of
675"): veraPDF checks the pages in order, and that's nearly all of its time.
Beside it are the checks run so far, how many rules have failed, and the time
elapsed. veraPDF doesn't publish where it is, so Legibyl reads it from the
validator's private `objectsContext` field; if a future veraPDF renames it,
the bar just stays indeterminate (and a test fails). Very large documents can
take minutes.

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
than rewriting paragraph semantics in the PDF after export. These warnings
(reported as `Legibyl` / `Structure-N`) are all defined in
`src/Legibyl.Validation/LegibylChecks.fs`; its header explains how to add a
new one.

For documents that tag no headings at all but carry a linked table of
contents, Legibyl also recovers the heading hierarchy from it
(`TocHeadings.fs`): each entry gives a heading's text, the page it's on (its
link's destination), and its level (its indent, grouped into clearly
separated tiers, H1 for the least indented). A body paragraph is only taken
to be that heading when both its text and its page match, and only one
paragraph does. Each such paragraph gets a fixable warning (Structure-3);
entries that match nothing or several paragraphs are reported (Structure-4);
and if the indents don't form clear levels, or the headings that could be
matched would start below H1 or skip a level, nothing is recovered and one
warning says why (Structure-5). The retagging is a separate Fix dialog option,
off by default.

Lists tagged as plain paragraphs are detected too (`ParagraphLists.fs`,
Structure-6): two or more sibling paragraphs on one page, at one indent, that
each start with a list marker — the same bullet (`•`, `◦`, `▪`, or a dash
followed by a space), or ordinals of one style and punctuation that count up by
one (`(1)` `(2)`, `a.` `b.`, `(i)` `(ii)`). Paragraphs in a table of contents are
left alone. The matching Fix dialog option (off by default) wraps each run in
`L` > `LI` > `LBody` around the original paragraphs and sets `/ListNumbering`
from the markers. Only the structure tree changes, never the drawn page, so the
document looks exactly the same; the marker stays part of each item's text
rather than being split into an `Lbl`, since that would mean editing the page's
content.

A document that tags headings but has no bookmarks (document outline) gets a
warning (`Bookmarks.fs`, Structure-7): readers can't jump between its sections
from the navigation pane. The matching fix adds a bookmark for each H1 and H2,
titled with the heading's text and pointing at it, with each H2 under the H1
before it. It never touches a document that already has bookmarks, and it
skips one that doesn't declare its language (`/Lang`), because PDF/UA needs the
language of bookmark text to be known and Legibyl won't guess it; the warning
says so instead.

Warnings can be dismissed from a row's right-click menu (or the keyboard's
context-menu key): they drop out of the list, the banner's warning count, and
Copy Summary/Save Report, and a **Show N dismissed** button beside those shows
them again, dimmed, so they can be restored. Dismissals are kept in memory for
the open PDF; **Remember dismissals for this PDF** in the same menu saves them
to `<name>.pdf.legibyl.json` next to it, which is loaded automatically when the
PDF is opened again (turning it off deletes that file). Failures can't be
dismissed. The CLI doesn't read these files.

When a finding has a known region — its marked content, the structure
element's content, or an annotation's `/Rect` (see `RuleViolation.Geometry`
above; Legibyl's own warnings carry one too) — the details panel
automatically renders that page with a highlight box around it. The image
scales to the available panel space rather than opening at 100% zoom.
Rendering uses [PdfPig](https://github.com/UglyToad/PdfPig) and
[PdfPig.Rendering.Skia](https://github.com/BobLd/PdfPig.Rendering.Skia) +
SkiaSharp to rasterise the page and draw the box (`Legibyl.Rendering`
project). Findings without a region (document-level metadata, whole-page
checks, structure elements with no drawn content) show the full page for
context when a page is known, with a banner noting the exact region isn't
available.

## Document structure (semantic hierarchy)

**View Structure…** shows the PDF's tagged logical structure tree — the
same heading/paragraph/list/table/link hierarchy and reading order that a
screen reader uses to present the document (the Catalog's `StructTreeRoot`).
It's read directly from veraPDF's own PD model (independent of a
validation pass), so it reflects exactly what the validator itself sees.
Reading it is mostly extracting each page's tagged text, so the progress bar
fills page by page while it loads. The last structure read is kept, so
reopening the window for the same, unchanged file is immediate.
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
click target on a separate child link fragment. The **Tree** tab keeps the compact outline view. So large documents open
quickly, the Layout tab draws about 2,000 blocks up front: it shows whole
levels while they fit and collapses the rest behind "▸ N child elements"
buttons that expand on click (most documents fit and show in full). Layout
blocks are selectable; when a selected node has same-page
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
dotnet run --project src/Legibyl.Cli/Legibyl.Cli.fsproj -- <file.pdf> [<file2.pdf> ...] [options]
```

Or after building, run the exe directly:
`src\Legibyl.Cli\bin\Debug\net8.0\legibyl.exe <file.pdf>`

Options:
- `--profile <name>` — profile to validate against (default `pdfua1`). Run
  `--list-profiles` for all accepted names (`pdfua1`, `pdfa1b`, `pdfa2b`,
  `pdfa2u`, `pdfa3b`, `pdfa4`).
- `--format <json|text>` — output format (default `json`).
- `--output <path>` — write output to a file instead of stdout.
- `--fix` — also attempt to repair the file, writing a repaired copy next to
  the original as `<name>.fixed<ext>`. It applies the repairs that need no
  guesswork about what the document means: XMP **metadata** via veraPDF's
  metadata fixer (missing metadata stream, missing PDF/A|PDF/UA identifier,
  false conformance claims, Info-dictionary sync); page annotation tab order
  (`/Tabs /S`); a **description for table-of-contents links that have none**,
  taken from the entry's own visible text with its dot leaders and printed page
  number stripped, so `Scope of this document ..... 3` becomes `Scope of this
  document`. Only a link inside a `TOC`/`TOCI` (at any depth) is described,
  because only there does its text reliably name where it goes. Elsewhere a
  link often reads "click here" or "read more"; copying that into `/Contents`
  would make the failure stop reporting while a screen reader user still hears
  nothing useful, so other links keep failing for a person to describe (and
  aren't marked fixable); and a **document title** plus `/DisplayDocTitle`,
  so readers show the title rather than the file name. A title is only ever
  moved from somewhere the author actually put it — the Info dictionary, or
  XMP `dc:title` — and never invented. Guessing one from the **file name**
  would satisfy `dc:title` while misdescribing the document to a screen reader
  user, so Legibyl does not do it at all. A file with no title anywhere
  therefore still reports the `dc:title` failure after `--fix`, rather than
  being quietly given a made-up one — unless you opt in with
  `--title-from-first-page` (or the matching Fix dialog option, off by
  default): then a document with no title in Info or XMP is titled from the
  text of its first page, as long as more pages follow and that page is short
  enough to be a title page (at most 30 words and 200 characters). The GUI dialog
  exposes the same repairs as individual checkboxes, plus removal of empty
  semantic `/P` structure elements, retagging headings recovered from the table
  of contents, and tagging marked paragraphs as lists (all off by default), and
  adding bookmarks for H1 and H2 headings; the CLI deliberately applies none of
  these.
  Structural fixes run
  before the veraPDF metadata fixer so metadata is repaired against the
  already-cleaned copy in the same operation. veraPDF saves its metadata
  repair as an incremental update that iText can't read reliably (a classic
  xref section chained onto xref streams, with an off-by-one `/Prev`), so
  Legibyl reads the repaired XMP and Info dictionary back with veraPDF's own
  parser and writes them into the input with iText instead. Intermediate
  repair files are created under the operating system temp directory, not
  beside the source PDF or requested output path. Empty `/P` cleanup is
  visible as a warning and fixable option, but it is unchecked by default in
  the GUI so users opt into structure-tree deletion. Neither path adds
  missing alt text, retags untagged content, infers headings, or infers table
  headers.
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
        "locationContext": "root/document[0]/pages[0](11 0 obj PDPage)/contentStream[0](21 0 obj PDSemanticContentStream)/content[10]{mcid:22}/contentItem[0]{mcid:22}",
        "region": {
          "pageNumber": 1,
          "left": 70.96,
          "bottom": 73.47,
          "right": 540.56,
          "top": 104.85
        },
        "isFixable": false,
        "fixOptions": null
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
changes that don't touch the vendored jar). `VersionPrefix` is derived from
`VeraPdfVersion`, so upgrading the jar updates the base automatically; reset
`VersionBuild` to `0` when you do. Run `legibyl --version` to see both numbers at once
(e.g. `legibyl 1.28.1.0 (vendored veraPDF engine 1.28.1)`).

## Licensing

This project's own code (everything outside `lib/vendor/`) is licensed
under the **GNU Affero General Public License v3.0 or later** — see
`LICENSE`.

It bundles the veraPDF `greenfield-apps` jar, which is separately
dual-licensed by the veraPDF Consortium under GPLv3+ / MPLv2+ (see
`licenses/LICENSE.GPL` and `licenses/LICENSE.MPL`), and uses IKVM.NET
(Apache 2.0). See `licenses/NOTICE.md` for full third-party attribution.
