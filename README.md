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
(`IPdfValidator.Validate(path, profile) -> ValidationReport`) that fully
isolates all Java/IKVM types from the rest of the app.

## Solution layout

```
VeraPdfTool.slnx
src/
  VeraPdfTool.Validation/   C# facade over the veraPDF Java validation core (IKVM)
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
