# Third-Party Notices

Legibyl is Copyright © 2026 Ekon Benefits <opensource@ekonbenefits.com>.

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

## veraPDF's WCAG 2.2 checks

Compiled into the same assembly as veraPDF (each downloaded from Maven Central,
pinned by SHA-256 in `src/Legibyl.Validation/Legibyl.Validation.fsproj`; see
`lib/vendor/verapdf-jars/README.md`):

- **veraPDF `wcag-validation` and `wcag-algorithms` 1.28.1**
  (<https://github.com/veraPDF/veraPDF-validation>,
  <https://github.com/veraPDF/veraPDF-wcag-algs>), and veraPDF's
  `WCAG-2-2-Machine.xml` validation profile at its v1.28.1 tag
  (<https://github.com/veraPDF/veraPDF-validation-profiles>), committed unmodified
  as `src/Legibyl.Validation/Profiles/WCAG-2-2-Machine.xml`. Copyright (c)
  2015-2026, veraPDF Consortium; dual-licensed GPLv3+ / MPLv2+, as veraPDF above.

`wcag-algorithms` is modified: one class, `ContrastRatioConsumer`, is taken out
of its jar at build time and replaced by Legibyl's modified copy, which draws
pages with PDFium instead of Apache PDFBox. Its source is
`src/Legibyl.Validation/java/org/verapdf/wcag/algorithms/semanticalgorithms/consumers/ContrastRatioConsumer.java`,
under veraPDF's licence (GPL-3.0-or-later or MPL-2.0, as its header says); the
hook it calls (`src/Legibyl.Validation/java/legibyl/`) is Legibyl's own,
AGPL-3.0 or later. Both are compiled into `src/Legibyl.Validation/java/wcag-contrast.jar`.
The rest of `wcag-algorithms`, and `wcag-validation`, are unmodified. Apache
PDFBox and the image readers veraPDF's class used aren't included.

## IKVM.NET

This application also uses [IKVM.NET](https://github.com/ikvmnet/ikvm) (the
`IKVM` NuGet package, used unmodified) to execute the veraPDF Java bytecode
from .NET. IKVM itself is copyright Jeroen Frijters, Windward Studios, Jerome
Haltom, Shad Storhaug, and contributors, under a zlib-style license. Its Java
class library, `IKVM.Java.dll`, is part of OpenJDK and GNU Classpath, copyright
Oracle and/or its affiliates and the Free Software Foundation, under the GNU
General Public License, version 2, with the "Classpath" exception. All of
these, with the full GPLv2 and the Classpath exception, are in `LICENSE.IKVM`
in this directory, as IKVM ships them. IKVM.ByteCode, which IKVM uses, is
copyright Jerome Haltom, under the MIT License.

## PdfPig

Legibyl uses [PdfPig](https://github.com/UglyToad/PdfPig), a pure managed C#
PDF parser licensed under the Apache License, Version 2.0, to read what's on a
page: each page's marked-content text by MCID (so the exported structure tree
includes the visible content under each tag), words and their positions for
layout suggestions, and annotation geometry (`/Rect`). It's used unmodified via
a NuGet package reference. The full Apache License 2.0 text is in
`LICENSE.Apache` in this directory.

## PDFium

Pages are drawn (for previews, layout detection, OCR and QR scanning) with
[PDFium](https://pdfium.googlesource.com/pdfium/), Chrome's PDF renderer,
copyright The PDFium Authors, licensed under the BSD 3-Clause License, with
some parts under the Apache License, Version 2.0; its license is in
`LICENSE.PDFium` in this directory. The native libraries come unmodified from
[pdfium-binaries](https://github.com/bblanchon/pdfium-binaries) by Benoît
Blanchon (the `bblanchon.PDFium.*` NuGet packages, Apache License 2.0), called
through Legibyl's own small binding (`Pdfium.fs`).

PdfPig derives in part from Apache PDFBox, whose NOTICE is reproduced
unmodified here:

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
[PP-DocLayoutV3](https://huggingface.co/PaddlePaddle/PP-DocLayoutV3) (copyright
PaddlePaddle Authors, Apache License 2.0), ships with Legibyl in its `models`
folder, unmodified: the build fetches it at a fixed revision, checked against
its SHA-256 (`LayoutModelRevision` in `Directory.Build.props`). A build without
it downloads it when the user first asks.

## Table Transformer and ONNX Runtime (tables' rows and cells)

**Suggest Structure** and the tag-from-layout fix find a table's rows, columns,
header and spanning cells with Microsoft's
[Table Transformer](https://github.com/microsoft/table-transformer)
structure-recognition model (copyright Microsoft Corporation, MIT License),
in [Xenova's 8-bit ONNX export](https://huggingface.co/Xenova/table-transformer-structure-recognition)
of version 1.0. The model ships with Legibyl in its `models` folder,
unmodified: the build fetches it at a fixed revision, checked against its
SHA-256 (`TableModelRevision` in `Directory.Build.props`); a build without it
downloads it when the user first asks. It's run with
[ONNX Runtime](https://github.com/microsoft/onnxruntime) (the
`Microsoft.ML.OnnxRuntime` NuGet package, copyright Microsoft Corporation, MIT
License), used unmodified via a NuGet package reference. The MIT License's full
text is in `LICENSE.MIT` in this directory, and ONNX Runtime's own third-party
notices, as it ships them, in `ThirdPartyNotices.OnnxRuntime.txt`.

## Tesseract OCR

The **Suggest Structure** preview and the tag-from-layout fix read text drawn as
outlines or pixels with [Tesseract](https://github.com/tesseract-ocr/tesseract),
via the `Tesseract.CrossPlatform` NuGet package (a fork of
[charlesw/tesseract](https://github.com/charlesw/tesseract)'s .NET wrapper) and
its `Tesseract.Native` binaries (Tesseract and Leptonica, built with vcpkg), all
licensed under the Apache License, Version 2.0 (Leptonica under its own
BSD-2-Clause-style license). They're used unmodified via NuGet package references.
The native binaries also build in other libraries (libpng, libjpeg-turbo,
libtiff, libwebp, giflib, OpenJPEG, zlib, zstd, lz4, bzip2, liblzma, libarchive,
libxml2, curl, OpenSSL and, on macOS, mbedTLS): Tesseract's, Leptonica's and
each of theirs licenses, as the packages ship them, are in
`NOTICE.Tesseract-native.txt` in this directory.
The app also ships Tesseract's English and Spanish models, `eng.traineddata` and
`spa.traineddata` from [tessdata_fast](https://github.com/tesseract-ocr/tessdata_fast)
(Apache License 2.0), unmodified, in its built-in language packs.

## URW base 35 and Liberation fonts

The **Embed fonts** fix embeds, in place of fonts a PDF uses without embedding
them, open fonts made to the same measure, which Legibyl ships unmodified in
its `fonts` folder (the fix embeds a subset of one in the PDF it writes):

- **URW's base 35 fonts** (Nimbus Sans, Nimbus Roman, Nimbus Mono PS, Standard
  Symbols PS and D050000L), release 20200910 of
  [urw-base35-fonts](https://github.com/ArtifexSoftware/urw-base35-fonts),
  copyright (URW)++ Design & Development, licensed under
  the GNU Affero General Public License, version 3 (`LICENSE.AGPL` in this
  directory), with this exemption: "As a special exception, permission is
  granted to include these font programs in a Postscript or PDF file that
  consists of a document that contains text to be displayed or printed using
  this font, regardless of the conditions or license applying to the document
  itself."
- **Liberation fonts** (Sans, Serif and Mono), version 2.1.5 of
  [liberation-fonts](https://github.com/liberationfonts/liberation-fonts),
  digitized data copyright Google Corporation, copyright Red Hat, Inc., with
  Reserved Font Names Arimo, Tinos, Cousine and Liberation, licensed under the
  SIL Open Font License, Version 1.1 (`LICENSE.OFL` in this directory).

The build fetches each unmodified, checked against its SHA-256
(`Legibyl.Validation.fsproj` and `LiberationFontsSha256` in
`Directory.Build.props`).

## LanguageDetection

The **Declare the document's language** fix detects a document's language with
[LanguageDetection.NETStandard](https://github.com/TechnikEmpire/language-detection)
(a .NET port of Nakatani Shuyo's language-detection, with profiles generated from
Wikipedia abstracts), licensed under the Apache License, Version 2.0, used
unmodified via a NuGet package reference.

## YamlDotNet

The plain-language explanations of PDF/UA failures (`RuleGuide.yaml`) are read
with [YamlDotNet](https://github.com/aaubry/YamlDotNet), licensed under the MIT
License, used unmodified via a NuGet package reference.

## Markdig

The GUI's Licenses window draws this notice from its Markdown, parsed with
[Markdig](https://github.com/xoofx/markdig), copyright Alexandre Mutel,
licensed under the BSD 2-Clause License (full text in `LICENSE.Markdig` in this
directory), used unmodified via a NuGet package reference.

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
AGPL-3.0-or-later license. It is used unmodified. iText's own notices, as its
package ships them, are in `NOTICE.iText.txt` in this directory.

## ANGLE

On Windows, the GUI draws with [ANGLE](https://chromium.googlesource.com/angle/angle)
(the `Avalonia.Angle.Windows.Natives` NuGet package), copyright The ANGLE
Project Authors, licensed under the BSD 3-Clause License (full text in
`LICENSE.ANGLE` in this directory), used unmodified.

## MIT-licensed components

The GUI and CLI also use the following components, each licensed under the
MIT License (full text in `LICENSE.MIT` in this directory). All are used
unmodified via NuGet package references.

- [F# Core Library (FSharp.Core)](https://github.com/dotnet/fsharp) — the
  F# runtime library used by all of Legibyl's projects.
  Copyright (c) Microsoft Corporation.
- [Avalonia](https://github.com/AvaloniaUI/Avalonia) (`Avalonia`,
  `Avalonia.Desktop`) — the cross-platform UI framework for the GUI.
  Copyright (c) AvaloniaUI OÜ.
- [Avalonia.FuncUI](https://github.com/fsprojects/Avalonia.FuncUI) — the
  F# view and component layer the GUI is written with.
  Copyright (c) 2019 Josua Jäger.
- [Semi.Avalonia](https://github.com/irihitech/Semi.Avalonia)
  (`Semi.Avalonia`) — the GUI's light/dark theme.
  Copyright (c) 2022 IRIHI Technology.
- [FSharp.SystemCommandLine](https://github.com/JordanMarr/FSharp.SystemCommandLine)
  — the CLI's argument parser.
  Copyright (c) 2022 Jordan Marr.
- [System.CommandLine](https://github.com/dotnet/command-line-api) — the
  command-line parsing library FSharp.SystemCommandLine builds on.
  Copyright © .NET Foundation and Contributors.
- [SkiaSharp and HarfBuzzSharp](https://github.com/mono/SkiaSharp) — 2D
  graphics and text shaping, used by Avalonia and for drawing pages' highlights.
  Copyright (c) 2015-2016 Xamarin, Inc. Copyright (c) 2017-2018 Microsoft
  Corporation.
- [Irihi.Avalonia.Shared](https://github.com/irihitech/Irihi.Avalonia.Shared)
  — shared helpers Semi.Avalonia uses. Copyright (c) IRIHI Technology.
- [Tmds.DBus.Protocol](https://github.com/tmds/Tmds.DBus) — D-Bus, which
  Avalonia uses on Linux. Copyright (c) Tom Deseyn.
- [MicroCom.Runtime](https://github.com/kekekeks/MicroCom) — COM interop
  Avalonia uses. Copyright (c) 2021 Nikita Tsukanov.
- [Newtonsoft.Json](https://github.com/JamesNK/Newtonsoft.Json) — JSON, used by
  a dependency. Copyright (c) 2007 James Newton-King.
- [IKVM.ByteCode](https://github.com/ikvmnet/ikvm.bytecode) — Java class file
  reading, used by IKVM. Copyright (c) 2026 Jerome Haltom.

These packages in turn depend on other open-source packages under their own
licenses; consult the individual project pages for those.
