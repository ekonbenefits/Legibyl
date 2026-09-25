# Sample corpus files

`pdfua1-pass.pdf` and `pdfua1-fail.pdf` are copied from the veraPDF public
test corpus:

- Source: https://github.com/veraPDF/veraPDF-corpus
- Path: `PDF_UA-1/7.1 General/7.1-t01-pass-a.pdf` and
  `PDF_UA-1/7.1 General/7.1-t01-fail-a.pdf`
- License: [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/)

Used here, unmodified, to assert expected pass/fail PDF/UA-1 validation
outcomes in `VeraPdfValidatorTests`.

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
