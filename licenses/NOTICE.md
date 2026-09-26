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
licensed under the Apache License, Version 2.0 (full text in
`LICENSE.Apache` in this directory), to execute the veraPDF Java bytecode
from .NET.

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
  the Apache License, Version 2.0. SkiaSharp itself is MIT-licensed (see
  below).

Both are used unmodified via NuGet package references (no vendored/bundled
source), so no additional source-availability obligations apply beyond the
license texts themselves. The full Apache License 2.0 text is in
`LICENSE.Apache` in this directory.

PdfPig.Rendering.Skia ships the following NOTICE, reproduced unmodified as
the Apache License requires:

```text
Apache PDFBox
Copyright 2014 The Apache Software Foundation

This product includes software developed at
The Apache Software Foundation (http://www.apache.org/).

Based on source code originally developed in the PDFBox and 
FontBox projects.

Copyright (c) 2002-2007, www.pdfbox.org

Based on source code originally developed in the PaDaF project.
Copyright (c) 2010 Atos Worldline SAS

Includes the Adobe Glyph List
Copyright 1997, 1998, 2002, 2007, 2010 Adobe Systems Incorporated.

Includes the Zapf Dingbats Glyph List
Copyright 2002, 2010 Adobe Systems Incorporated.

Includes OSXAdapter
Copyright (C) 2003-2007 Apple, Inc., All Rights Reserved
```

## PaddleOCR (layout detection)

The GUI's **Suggest Structure** preview uses
[PaddleOCR for .NET](https://github.com/theolivenbaum/PaddleOCR) (the
`PaddleOCR` NuGet package), a pure C# port of
[PaddleOCR](https://github.com/PaddlePaddle/PaddleOCR), licensed under the
Apache License, Version 2.0 (full text in `LICENSE.Apache` in this directory).
It's used unmodified via a NuGet package reference. Its layout detection model,
PP-DocLayoutV3 (Apache License 2.0, by PaddlePaddle), isn't shipped with
Legibyl: the app downloads it only when the user asks, into PaddleOCR's own
model cache.

## Tesseract OCR

The **Suggest Structure** preview and the tag-from-layout fix read text drawn as
outlines or pixels with [Tesseract](https://github.com/tesseract-ocr/tesseract),
via the `Tesseract.CrossPlatform` NuGet package (a fork of
[charlesw/tesseract](https://github.com/charlesw/tesseract)'s .NET wrapper) and
its `Tesseract.Native` binaries (Tesseract and Leptonica, built with vcpkg), all
licensed under the Apache License, Version 2.0 (Leptonica under its own
BSD-2-Clause-style license). They're used unmodified via NuGet package references.
The app also ships Tesseract's English model, `eng.traineddata` from
[tessdata_fast](https://github.com/tesseract-ocr/tessdata_fast) (Apache License
2.0), unmodified.

## LanguageDetection

The **Declare the document's language** fix detects a document's language with
[LanguageDetection.NETStandard](https://github.com/TechnikEmpire/language-detection)
(a .NET port of Nakatani Shuyo's language-detection, with profiles generated from
Wikipedia abstracts), licensed under the Apache License, Version 2.0, used
unmodified via a NuGet package reference.

## ZXing.Net and qr-link-pdf

QR codes are read with [ZXing.Net](https://github.com/micjahn/ZXing.Net),
licensed under the Apache License, Version 2.0, used unmodified via a NuGet
package reference. How pages are scanned for them (a pyramid of scales, each
also inverted) and which payloads are linked (absolute http(s) addresses, and
`www.` taken as https) follow [qr-link-pdf](https://github.com/jbtule/qr-link-pdf)
by Jay Tuley (AGPL-3.0); no code is copied from it.

## iText Core

Legibyl uses [iText Core for .NET](https://github.com/itext/itext-dotnet)
to write selected best-effort PDF repairs that require mutating existing PDF
dictionaries or structure-tree arrays, such as setting page `/Tabs /S` on
pages with annotations and removing empty `/P` structure elements.

iText Core is used via the `itext` NuGet package under the GNU Affero
General Public License v3.0 or later. This is compatible with Legibyl's own
AGPL-3.0-or-later license. It is used unmodified.

## MIT-licensed components

The GUI and CLI also use the following components, each licensed under the
MIT License (full text in `LICENSE.MIT` in this directory). All are used
unmodified via NuGet package references.

- [F# Core Library (FSharp.Core)](https://github.com/dotnet/fsharp) — the
  F# runtime library used by all of Legibyl's projects.
  Copyright (c) Microsoft Corporation.
- [Avalonia](https://github.com/AvaloniaUI/Avalonia) (`Avalonia`,
  `Avalonia.Desktop`) and
  [Avalonia.Controls.DataGrid](https://github.com/AvaloniaUI/Avalonia.Controls.DataGrid)
  — the cross-platform UI framework for the GUI.
  Copyright (c) AvaloniaUI OÜ.
- [Avalonia.FuncUI](https://github.com/fsprojects/Avalonia.FuncUI) — the
  F# view and component layer the GUI is written with.
  Copyright (c) 2019 Josua Jäger.
- [AvaloniaCommunity.FuncUI.Bindings.DataGrid](https://www.nuget.org/packages/AvaloniaCommunity.FuncUI.Bindings.DataGrid)
  — FuncUI bindings for the DataGrid. Package author: JaggerJo (the
  package metadata gives no separate copyright line).
- [Semi.Avalonia](https://github.com/irihitech/Semi.Avalonia)
  (`Semi.Avalonia`, `Semi.Avalonia.DataGrid`) — the GUI's light/dark theme.
  Copyright (c) 2022 IRIHI Technology.
- [FSharp.SystemCommandLine](https://github.com/JordanMarr/FSharp.SystemCommandLine)
  — the CLI's argument parser.
  Copyright (c) 2022 Jordan Marr.
- [System.CommandLine](https://github.com/dotnet/command-line-api) — the
  command-line parsing library FSharp.SystemCommandLine builds on.
  Copyright © .NET Foundation and Contributors.
- [SkiaSharp and HarfBuzzSharp](https://github.com/mono/SkiaSharp) — 2D
  graphics and text shaping, used by Avalonia and PdfPig.Rendering.Skia.
  Copyright (c) 2015-2016 Xamarin, Inc. Copyright (c) 2017-2018 Microsoft
  Corporation.

These packages in turn depend on other open-source packages under their own
licenses; consult the individual project pages for those.
