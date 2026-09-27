# Sample corpus files

`pdfua1-pass.pdf` and `pdfua1-fail.pdf` are copied from the veraPDF public
test corpus:

- Source: https://github.com/veraPDF/veraPDF-corpus
- Path: `PDF_UA-1/7.1 General/7.1-t01-pass-a.pdf` and
  `PDF_UA-1/7.1 General/7.1-t01-fail-a.pdf`
- License: [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/)

Used here, unmodified, to assert expected pass/fail PDF/UA-1 validation
outcomes in `VeraPdfValidatorTests`.

`pdfua2-pass.pdf` and `pdfua2-fail.pdf` are PDF 2.0 files from the same
corpus, for PDF/UA-2:

- Commit: `bb75f4f0073d9350dfd058c0162a367e6fadf25e` (2026-09-16)
- Path: `PDF_UA-2/5 Version identification/5-t02-pass-a.pdf` and
  `PDF_UA-2/5 Version identification/5-t02-fail-a.pdf`
- License: CC BY 4.0, as above

Used unmodified in `VeraPdfValidatorTests` to check that a PDF 2.0 file is
checked against PDF/UA-2, and that the pass sample complies and the fail
sample doesn't.

`pages-clipped-paragraphs.pdf` was made for this project by exporting a
three-paragraph document from Apple Pages (with blank lines between
paragraphs, which Pages drops rather than tagging as empty `/P`s). Its first
paragraph's marked content also contains a page-sized clipping rectangle
(`re W n`), which `VeraPdfValidatorTests` uses to check that clipping paths
don't inflate a finding's region.

`libreoffice-empty-paragraph.pdf` was made for this project by converting a
small HTML file with LibreOffice (`soffice --convert-to pdf` with
`UseTaggedPDF`). Its paragraphs use the custom tag "Text body", role-mapped
to `/P`, and one of them contains only a non-breaking space, drawn with an
embedded TrueType font whose glyph box is empty. `PdfFixerTests` uses it to
check that empty-paragraph detection follows the RoleMap and that the
warning gets a one-character region.

## Fonts

`fonts/Lato-Regular.ttf` is Lato, by Łukasz Dziedzic, from Google Fonts
(https://github.com/google/fonts, `ofl/lato/`, commit
`f3b885d5590e307e02542f1a724cec55d567fdaa`), under the SIL Open Font License
1.1 (`fonts/OFL.txt`). Tests that make PDF/UA-2 files embed it: PDF/UA-2
requires every font embedded, and the standard 14 fonts iText uses by default
aren't.

## Techniques for Accessible PDF

`techniques-for-accessible-pdf/` holds the sample PDFs from the PDF
Association's Techniques for Accessible PDF:

- Source: https://github.com/pdf-association/techniques-for-accessible-pdf
- Commit: `3e31095d8aeea941eccbbb766a23369374c2e9a6` (2026-08-30)
- License: [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/),
  copyright PDF Association, Inc. See `techniques-for-accessible-pdf/LICENSE`.

Only each technique's PDF and `technique-info.yaml`, plus
`config/matterhorn.yaml` and the license, are copied, unmodified and at their
upstream paths. `technique-info.yaml` labels a sample Pass or Fail and lists
the Matterhorn Protocol checkpoints it covers; `matterhorn.yaml` says whether
each checkpoint is machine-checkable (M) or needs a person (H).

`TechniqueSampleTests` validates every sample against the standard its PDF
version calls for, as the app does. All of them are PDF 1.5–1.7, so that's
PDF/UA-1:

- Pass samples must be compliant.
- Fail samples covering a machine-checkable checkpoint must fail some rule
  besides 5-1. Every Fail sample leaves out the PDF/UA identifier, so 5-1
  alone doesn't show the sample's defect was found. Samples veraPDF misses
  are skipped via a known-gaps list in the tests.
- Fail samples covering only human-checkable checkpoints are skipped.

To update, copy the same files from a newer upstream commit and update the
commit above.
