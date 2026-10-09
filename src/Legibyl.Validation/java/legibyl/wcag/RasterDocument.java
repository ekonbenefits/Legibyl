// Legibyl's hook for drawing pages in veraPDF's WCAG contrast check (see README.md here).
// SPDX-License-Identifier: AGPL-3.0-or-later
package legibyl.wcag;

import java.io.Closeable;
import java.io.IOException;

/** A PDF opened by a PageRasterizer. */
public interface RasterDocument extends Closeable {
	/**
	 * Draws the 0-based page on white at dpi pixels an inch, its crop box turned as the page says,
	 * as PDFBox's PDFRenderer.renderImageWithDPI does: the pixels are 0xAARRGGBB, a row at a time
	 * from the top, and size gets the width and the height.
	 */
	int[] render(int pageIndex, int dpi, int[] size) throws IOException;
}
