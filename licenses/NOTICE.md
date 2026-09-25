# Third-Party Notices

**Legibyl's own source code is licensed under the GNU Affero General
Public License v3.0 or later — see the `LICENSE` file at the repository
root and `LICENSE.AGPL` in this directory.**

This application also bundles the `greenfield-apps-1.28.1.jar` Java
archive from the **veraPDF** project (https://verapdf.org /
https://github.com/veraPDF), loaded into the .NET process via
[IKVM.NET](https://ikvm.org/) so that the application can call veraPDF's
PDF/A and PDF/UA validation engine directly.

Copyright (c) 2015-2026, veraPDF Consortium <info@verapdf.org>. All rights
reserved.

veraPDF is free software, dual-licensed (recipient's choice) under:

- The GNU General Public License, version 3 or later ("GPLv3+") — see
  `LICENSE.GPL` in this directory, or
  <https://www.gnu.org/licenses/gpl-3.0.en.html>.
- The Mozilla Public License, version 2.0 or later ("MPLv2+") — see
  `LICENSE.MPL` in this directory, or <https://mozilla.org/MPL/2.0/>.

See `lib/vendor/verapdf-jars/README.md` for the exact artifact, version,
and checksum of the bundled jar, and provenance/attribution details.

**Corresponding source code** for this exact jar version is publicly
available, unmodified from upstream, at:
<https://github.com/veraPDF/veraPDF-apps/tree/v1.28.1> (and the
`greenfield-apps` module's dependencies are likewise all public open-source
projects resolvable from that tag's Maven POMs). This repository does not
modify the vendored jar in any way: the build downloads it unmodified from
Maven Central and verifies its SHA-256 checksum (`VeraPdfSha256` in
`Directory.Build.props`).

veraPDF's bundled fat jar in turn includes third-party dependencies
(Jackson databind/core/annotations, JAXB API/Core/Runtime
(`javax.xml.bind`, `org.glassfish.jaxb`), `javax.activation`, and others)
under their own respective open-source licenses. Consult the individual
project pages for full license texts of those transitive dependencies.

## IKVM.NET

This application also uses [IKVM.NET](https://github.com/ikvmnet/ikvm),
licensed under the Apache License, Version 2.0, to execute the veraPDF
Java bytecode from .NET.

## PdfPig and PdfPig.Rendering.Skia

Legibyl uses PdfPig in two places: the validation layer reads each page's
marked-content text by MCID so the exported semantic structure tree can
include the visible content under each tag, and the GUI's selected-finding
preview renders the page containing a failure with a highlight box around
it. These features use:

- [PdfPig](https://github.com/UglyToad/PdfPig) — a pure managed C# PDF
  parser (no native/JNI dependencies), licensed under the Apache License,
  Version 2.0. Used here to read marked-content/MCID text and annotation
  geometry (`/Rect`) directly from the PDF's object model.
- [PdfPig.Rendering.Skia](https://github.com/BobLd/PdfPig.Rendering.Skia)
  — a PdfPig extension that rasterises pages to
  [SkiaSharp](https://github.com/mono/SkiaSharp) bitmaps, licensed under
  the Apache License, Version 2.0. SkiaSharp itself is MIT-licensed.

Both are used unmodified via NuGet package references (no vendored/bundled
source), so no additional source-availability obligations apply beyond the
license texts themselves.

## iText Core

Legibyl uses [iText Core for .NET](https://github.com/itext/itext-dotnet)
to write selected best-effort PDF repairs that require mutating existing PDF
dictionaries or structure-tree arrays, such as setting page `/Tabs /S` on
pages with annotations and removing empty `/P` structure elements.

iText Core is used via the `itext` NuGet package under the GNU Affero
General Public License v3.0 or later. This is compatible with Legibyl's own
AGPL-3.0-or-later license. It is used unmodified.
