# Java stand-ins compiled into veraPDF's assembly

`jpeg2000-stub.jar` holds `com.github.jaiimageio.jpeg2000.impl.J2KImageReaderSpi`, from the
source here. veraPDF's WCAG checks (`wcag-algorithms`) register that class by name, not through
Image I/O's service lookup: `ImagesUtils`'s constructor calls
`registry.registerServiceProvider(new J2KImageReaderSpi())` before the contrast check renders
pages. Without the class, that throws `NoClassDefFoundError`, which nothing catches, and
validation fails. (PDFBox itself looks a JPEG 2000 reader up, and does without one.) The real one, in jai-imageio-jpeg2000, is under the JJ2000
license, whose terms grant no rights for products that don't conform to the JPEG 2000 standard;
Legibyl doesn't ship it. This one reads nothing, so a JPEG 2000 image is left out of the page as
the contrast check renders it.

The jar is committed so building Legibyl needs no JDK. To rebuild it (Java 8 bytecode, as IKVM
expects):

```sh
cd src/Legibyl.Validation/java
rm -rf classes && mkdir classes
javac --release 8 -d classes com/github/jaiimageio/jpeg2000/impl/J2KImageReaderSpi.java
(cd classes && jar --create --file ../jpeg2000-stub.jar .)
rm -rf classes
```
