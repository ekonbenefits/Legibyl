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
tests/
  VeraPdfTool.Validation.Tests/   xUnit tests using real PDF/UA-1 corpus samples
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

## Supported profiles

PDF/UA-1 (default), PDF/A-1b, PDF/A-2b, PDF/A-2u, PDF/A-3b, PDF/A-4.

## Licensing

This project's own code (everything outside `lib/vendor/`) is licensed
under the **Mozilla Public License 2.0** — see `LICENSE`.

It bundles the veraPDF `greenfield-apps` jar, which is separately
dual-licensed by the veraPDF Consortium under GPLv3+ / MPLv2+ (see
`licenses/LICENSE.GPL` and `licenses/LICENSE.MPL`), and uses IKVM.NET
(Apache 2.0). See `licenses/NOTICE.md` for full third-party attribution.
