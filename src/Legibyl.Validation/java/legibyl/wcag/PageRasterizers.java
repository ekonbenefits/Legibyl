// Legibyl's hook for drawing pages in veraPDF's WCAG contrast check (see README.md here).
// SPDX-License-Identifier: AGPL-3.0-or-later
package legibyl.wcag;

import java.io.IOException;

/** The PageRasterizer WCAG's contrast check draws pages with, set once for the process. */
public final class PageRasterizers {
	private static volatile PageRasterizer current;

	private PageRasterizers() {
	}

	public static void set(PageRasterizer rasterizer) {
		current = rasterizer;
	}

	public static PageRasterizer get() {
		return current;
	}

	public static RasterDocument open(String path) throws IOException {
		PageRasterizer rasterizer = current;
		if (rasterizer == null) {
			throw new IOException("No page rasterizer is set for WCAG's contrast check.");
		}
		return rasterizer.open(path);
	}
}
