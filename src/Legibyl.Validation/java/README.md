# Java compiled into veraPDF's assembly

`wcag-contrast.jar` lets veraPDF's WCAG contrast check (1.4.3) draw pages with PDFium, the
renderer the rest of Legibyl uses, instead of Apache PDFBox. It's built from the source here:

- `legibyl/wcag/`: the hook. `PageRasterizer` opens a PDF and `RasterDocument` draws its pages;
  `PageRasterizers.set` is given .NET's (`ContrastRasterizer`, in `VeraPdfValidator.fs`) before
  veraPDF starts. Legibyl's own code, AGPL-3.0-or-later.
- `org/verapdf/wcag/algorithms/semanticalgorithms/consumers/ContrastRatioConsumer.java`:
  veraPDF's class, from veraPDF-wcag-algs v1.28.1, with its PDFBox calls (loading the PDF,
  registering the JPEG 2000 and JBIG2 readers, `PDFRenderer.renderImageWithDPI`) replaced by the
  hook's. Everything else, how it crops each line of text and works out its contrast, is
  veraPDF's. It keeps veraPDF's licence (GPL-3.0-or-later or MPL-2.0; see its header).

The build takes veraPDF's own `ContrastRatioConsumer` classes out of `wcag-algorithms` (the
`RepackWcagAlgorithms` target in `Legibyl.Validation.fsproj`), so this is the only one IKVM
sees: it isn't left to the order IKVM reads the jars in, which its cache doesn't always keep.

Why: PDFBox and what it reads images with (fontbox, commons-logging, jai-imageio, jbig2) needn't
be shipped, nor a stand-in for jai-imageio's JPEG 2000 reader, whose JJ2000 licence Legibyl can't
take; PDFium draws JPEG 2000 images itself, so text over one isn't measured against white. It
needs no headless AWT, and draws a page in a millisecond or two where PDFBox took 40 to 70.
Contrast is measured as before: pages drawn at 144 dpi, without smoothing (as veraPDF has
PDFBox draw them), sized as PDFBox sizes them.

When veraPDF is updated, compare the new release's `ContrastRatioConsumer` with the 1.28.1 one
and carry its changes over. (Later releases draw pages in `ImagesUtils` instead, on PDFBox 3.)

The jar is committed so building Legibyl needs no JDK. To rebuild it (Java 8 bytecode, as IKVM
expects; wcag-algorithms is in `obj/verapdf/` after a build):

```sh
cd src/Legibyl.Validation/java
rm -rf classes && mkdir classes
javac --release 8 -cp ../obj/verapdf/wcag-algorithms-1.28.1.jar -d classes $(find legibyl org -name '*.java')
(cd classes && jar --create --file ../wcag-contrast.jar .)
rm -rf classes
```
