/// What PDF/UA-1's failures mean in plain language, and what to do about those no auto fix
/// repairs: veraPDF says what the standard requires ("Content shall be marked as Artifact or
/// tagged as real content"), which says little to someone who doesn't know PDF's insides. Covers
/// the rules a document commonly fails; others have only veraPDF's own text.
module Legibyl.Validation.RuleGuide

/// A failure explained: what it means for a reader, and, where no auto fix repairs it, what to
/// do instead.
type Guide = { Meaning: string; WhatToDo: string }

let private guide meaning whatToDo = Some { Meaning = meaning; WhatToDo = whatToDo }

/// For a failure fixed in the tool that made the PDF, or a PDF editor.
let private inTheSourceText =
    "fix it where the PDF was made, and export it again with tags (in Word, say, \"Document structure tags for accessibility\"), or in a PDF editor's tags panel."

let private inTheSource = "F" + inTheSourceText.Substring 1

let private fonts =
    "Export it again with fonts embedded (most tools have an option for it), or fix the fonts with a PDF editor's preflight tools."

/// The guide for a PDF/UA-1 (ISO 14289-1) rule, by its clause and test number.
let forRule (clause: string) (test: int) : Guide option =
    match clause, test with
    | "5", _ ->
        guide
            "The PDF doesn't say in its metadata that it follows PDF/UA, so tools that look for accessible PDFs won't treat it as one."
            "Choose Repair XMP metadata in Auto Fixes… (veraPDF only adds it once every other failure is fixed)."
    | "6.2", 1 ->
        guide
            "The PDF isn't marked as tagged, so assistive technology may ignore its tags."
            "Tagging it sets this. For a PDF with tags already, export it again from the tool that made it with tags turned on."
    | "7.1", 1 ->
        guide
            "Something marked as decoration (an artifact, which isn't read) is inside tagged content, so it's both skipped and part of the text."
            inTheSource
    | "7.1", 2 ->
        guide
            "Tagged content is inside something marked as decoration (an artifact), so a screen reader may skip content that should be read."
            inTheSource
    | "7.1", 3 ->
        guide
            "Some of the page (text, images or lines) is neither tagged nor marked as decoration, so a screen reader can't tell whether to read it."
            // A PDF with no tags has this fixed by Suggest Structure… (so it isn't shown there).
            ("The tool that made the PDF left this out of its tags: " + inTheSourceText)
    | "7.1", 4 ->
        guide
            "The PDF says its tags may be unreliable (Suspects), so assistive technology may not trust them."
            "Export it again from the tool that made it, with tags turned on."
    | "7.1", 5 ->
        guide
            "The PDF uses tag names of its own without saying which standard tag each means, so a screen reader can't tell what they are (a heading, a list, a paragraph)."
            ("Add each name to the role map in a PDF editor, or " + inTheSourceText)
    | "7.1", (6 | 7) ->
        guide
            "The PDF's role map, which says what its own tag names mean, is broken: it goes round in a circle, or redefines a standard tag."
            ("Correct the role map in a PDF editor, or " + inTheSourceText)
    | "7.1", 8 ->
        guide "The PDF has no metadata (XMP) for its title, language and PDF/UA claim to go in." "Choose Repair XMP metadata in Auto Fixes…."
    | "7.1", 9 ->
        guide
            "The PDF has no title in its metadata, so a screen reader and the reader's title bar name it by its file name."
            "Choose Document title in Auto Fixes… if it has a title elsewhere, or Title from the first page; or set the title in the tool that made it."
    | "7.1", 10 ->
        guide
            "The reader shows the file name in its title bar instead of the document's title."
            "Choose Document title in Auto Fixes…."
    | "7.1", 11 ->
        guide "The PDF has no tags at all, so a screen reader has nothing to go on but the raw text, in whatever order it's drawn." "Suggest Structure… tags it from its pages' layout."
    | "7.1", 12 ->
        guide "A tag in the PDF's structure doesn't say what it belongs to, so the structure is broken there." inTheSource
    | "7.2", (2 | 21 | 22 | 23 | 24 | 25 | 30 | 31 | 32 | 33 | 34) ->
        guide
            "The language of some text isn't declared (the page's text, or alt text, a bookmark, a link's description, the metadata), so a screen reader may read it with the wrong voice and pronunciation."
            "Choose Declare the language in Auto Fixes… (it needs enough text to tell the language), or set the document's language in the tool that made it."
    | "7.2", 29 ->
        guide "A language is declared, but not as a language code a screen reader understands (like en-US)." inTheSource
    | "7.2", (17 | 18 | 19 | 20 | 40) ->
        guide
            "A list's tags are nested wrongly (an item outside a list, say), so a screen reader can't announce it as a list with its number of items."
            inTheSource
    | "7.2", (26 | 27 | 28) ->
        guide "A table of contents' tags are nested wrongly, so a screen reader can't announce it as one." inTheSource
    | "7.2", (3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 36 | 37 | 38 | 39 | 41 | 42 | 43) ->
        guide
            "A table's tags are nested or shaped wrongly (a cell outside a row, rows of different lengths), so a screen reader can't move through it or say which header goes with a cell."
            "Fix the table in the tool that made the PDF (mark its header row, avoid merged cells where you can) and export it again, or in a PDF editor's table tools."
    | "7.3", 1 ->
        guide
            "A figure has no alt text, so a screen reader can only say it's an image."
            "In Structure…, right-click the figure and choose Edit Alt Text…: say what it's for here, in a sentence. If it's only decoration, it should be marked as an artifact instead, in a PDF editor or where the PDF was made."
    | "7.4.2", 1 ->
        guide
            "The headings don't start with H1, or skip levels (an H3 straight after an H1), so the outline a screen reader navigates by has gaps."
            "In Structure…, select a heading and give it the right level with the Tag picker."
    | "7.4.4", _ ->
        guide
            "The headings mix numbered levels (H1 to H6) with plain H, or a section has more than one plain H, so a screen reader can't work out the outline."
            "In Structure…, give the headings numbered levels with the Tag picker."
    | "7.5", _ ->
        guide
            "A table's header cells don't say whether they head a row or a column, so a screen reader can't announce the right header with each cell."
            "Mark the header row or column in the tool that made the PDF and export it again, or set each header cell's scope in a PDF editor's table tools."
    | "7.7", 1 ->
        guide "A formula has no alt text, so a screen reader can't read it." "In Structure…, right-click the formula and choose Edit Alt Text…, and write it out as it would be read aloud."
    | "7.9", _ ->
        guide "A note (a footnote, say) has no ID, or shares one, so its reference can't point to it." inTheSource
    | "7.18.1", 1 ->
        guide "An annotation (a comment or stamp, say) isn't in the tags, so a screen reader won't find it in reading order." inTheSource
    | "7.18.1", 2 ->
        guide
            "An annotation has no description, so a screen reader can't say what it is."
            "Give it a description (its Contents) in a PDF editor, or remove it if it isn't needed."
    | "7.18.1", 3 ->
        guide
            "A form field has no tooltip, which is what a screen reader says when it's reached."
            "Give the field a tooltip in a PDF editor's form tools, or in the tool that made the form."
    | "7.18.3", 1 ->
        guide
            "A page with links or other annotations doesn't set the keyboard's tab order to follow the structure."
            "Choose Tab order for annotations in Auto Fixes…."
    | "7.18.4", _ ->
        guide "A form field isn't tagged as a form field, so a screen reader doesn't announce it as one." inTheSource
    | "7.18.5", 1 ->
        guide "A link isn't tagged as a link, so a screen reader doesn't announce it as one." inTheSource
    | "7.18.5", 2 ->
        guide
            "A link has no description, so a screen reader can only read out its address, or nothing."
            "In Structure…, right-click the link and choose Edit Link Description…. Auto Fixes… can describe table-of-contents links, and web links from their pages' titles."
    | "7.20", 2 ->
        guide "Content drawn from a shared graphic (a form XObject) isn't in the tags." inTheSource
    | "7.21.3.2", 1 ->
        guide
            "A font leaves out how its character codes map to its glyphs; readers assume the usual mapping, but the standard wants it said."
            "Choose Font glyph mapping in Auto Fixes…."
    | "7.21.4.1", 1 -> guide "A font isn't embedded, so the text may look different, or not read the same, on another computer." fonts
    | ("7.21.3.1" | "7.21.3.3" | "7.21.4.1" | "7.21.4.2" | "7.21.5" | "7.21.6" | "7.21.8"), _ ->
        guide "A font in the PDF is incomplete or inconsistent, which can make its text display or read wrongly." fonts
    | "7.21.7", _ ->
        guide
            "Some text uses a font that doesn't say which characters it draws, so a screen reader (or copying the text) gets nothing, or the wrong letters."
            fonts
    | _ -> None

/// The guide for a finding, if it's a PDF/UA-1 failure this covers.
let forFinding (violation: RuleViolation) : Guide option =
    if violation.Specification.StartsWith "ISO 14289-1" then forRule violation.Clause violation.TestNumber else None
