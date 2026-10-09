// Legibyl's hook for drawing pages in veraPDF's WCAG contrast check (see README.md here).
// SPDX-License-Identifier: AGPL-3.0-or-later
package legibyl.wcag;

import java.io.IOException;

/** Draws a PDF's pages for WCAG's contrast check, in place of PDFBox (set with PageRasterizers.set). */
public interface PageRasterizer {
	/** Opens the PDF at path, once for each contrast pass, to draw its pages from. */
	RasterDocument open(String path) throws IOException;
}
