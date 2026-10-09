// Legibyl's stand-in for jai-imageio-jpeg2000's J2KImageReaderSpi (see README.md here).
// SPDX-License-Identifier: AGPL-3.0-or-later
package com.github.jaiimageio.jpeg2000.impl;

import java.util.Locale;
import javax.imageio.ImageReader;
import javax.imageio.spi.ImageReaderSpi;
import javax.imageio.stream.ImageInputStream;

/**
 * Reads no JPEG 2000 images. veraPDF's WCAG checks register this reader when they render pages
 * for the contrast check; jai-imageio-jpeg2000's own is under the JJ2000 license, which Legibyl
 * doesn't ship. A JPEG 2000 image is then left out of the page as it's rendered there.
 */
public class J2KImageReaderSpi extends ImageReaderSpi {
    public J2KImageReaderSpi() {
        super("Legibyl", "1.0", new String[] { "legibyl-no-jpeg2000" }, new String[0], new String[0],
            J2KImageReaderSpi.class.getName(), new Class<?>[] { ImageInputStream.class }, null,
            false, null, null, null, null, false, null, null, null, null);
    }

    @Override
    public boolean canDecodeInput(Object source) {
        return false;
    }

    @Override
    public ImageReader createReaderInstance(Object extension) {
        throw new UnsupportedOperationException("Legibyl doesn't read JPEG 2000 images here.");
    }

    @Override
    public String getDescription(Locale locale) {
        return "No JPEG 2000 reader (Legibyl's stand-in)";
    }
}
