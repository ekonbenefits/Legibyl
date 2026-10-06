---
name: add-check
description: How to add a Legibyl warning (a "Structure-N" check) or an auto fix to Legibyl correctly — every place each one has to be wired in, the reporting rules, and the tests. Use when adding or changing a check in LegibylChecks.fs or a fix in StructureFixes.fs.
---

# Adding a Legibyl warning or auto fix

Legibyl's warnings are its own checks, reported beside veraPDF's failures as `Legibyl` /
`Structure-N`. Its auto fixes are the `PdfFixOptions` that Export… and `legibyl fix` run. Each one
is wired into several places, and a missed one is a bug that reviews keep finding. Work through
the list below; the header of `src/Legibyl.Validation/LegibylChecks.fs` has the short version for
warnings.

Only build a new **structural** fix (retagging, role mapping, marking artifacts, anything that
changes what the tags say) when the user asks for it. Don't propose one.

## A warning

1. **Its number.** Take the next unused `Structure-N` (look at `all`, `run` and `messageFor` in
   `LegibylChecks.fs`). Never reuse or renumber one: dismissed warnings are kept as
   `Legibyl Structure-N`, and a saved report reads a warning's message back by its number, so a
   reused number dismisses the other warning too and reads back with its message.
2. **Its message is fixed.** A `[<Literal>]` message, and an entry for its number in `messageFor`,
   so a saved report reads it back as itself. Anything that varies (pages, names, counts, a
   library's reason) goes in `errorMessage`, the warning's detail: `"Page(s) 3 and 5: …"`.
3. **Where it runs.** Per element: an `inspect` function in `all`. Whole document (pages, the
   table of contents, the parent tree): a `…Findings` function yielded in `run`.
4. **Where it points.** A location the GUI can show (the element's path), and geometry where
   there is one, so it lands on the page.
5. **Unknown isn't empty.** Text comes from `MarkedContentTextIndex`. A page PdfPig couldn't read
   is in `context.MarkedContent.Unreadable` and has no entries, so a missing entry there means
   "unknown", not "no text" (see `isSubtreeEmpty`). A check that acts on missing text must ask.
6. **Its fix,** if it has one: set its `FixOptions`, so the warning shows as fixable.
7. **The README's table of checks** ("Check | Warns about | Auto fix"): a row for it.

## An auto fix

1. **Its flag.** The next power of two in `PdfFixOptions` (`Types.fs`), with a doc comment, and
   `All` raised to match (`All` is every bit set: 2^n − 1).
2. **Its name.** Add it to `FixNames.order`, near where it runs (the order names are listed in,
   which has drifted from the order they run in places: `link-contents` is listed after the font
   fixes and runs before them). Its CLI name is its flag's in kebab-case (`FontGlyphs` is `font-glyphs`). Then decide:
   - `FixNames.safe`: only fixes that need no guesswork about the document's meaning (the CLI
     runs `safe` by default, and the Auto Fixes dialog starts with it ticked);
   - `FixNames.all` leaves out fixes that reach outside the file (`link-titles-from-web`);
   - `FixNames.usesLayout`, if it runs the layout model (a download the first time).
3. **Its description.** One line in `PdfFixDescriptions` (`Results.fs`); `legibyl list fixes` and
   the warnings' "fixable" text use it.
4. **Where it runs.** Most fixes go in `StructureFixes.orderedFixes`, in the place that's right
   for what other fixes see. Say why in the numbered list of the order ("Legibyl's own fixes, in
   the order they run", earlier in `StructureFixes.fs`), or beside its entry. Not everything
   runs there: tagging an untagged PDF (`TagFromLayout`, with `QrLinks`) runs before it, in
   `LayoutTagging`, and veraPDF's metadata fixer runs after it (under `Metadata`, which also has
   a step of Legibyl's own in `orderedFixes`); both stages are in `VeraPdfValidator.Fix`. A fix
   that belongs with one of those goes in that stage.
5. **If it goes by page text** (`extractText`, `isSubtreeEmpty`, the text index), add it to
   `readsText` in `StructureFixes.fs`, so a page whose text couldn't be read is said once and the
   fix isn't also reported as having had nothing to do.
6. **What it reports.** It returns one line per kind of change it made, and an empty list when
   it changed nothing (that's how "nothing in this PDF needed it" is worked out, and how a file
   needing nothing gets no copy written). What it found but couldn't change goes in
   `context.Notes`, which the user sees among what wasn't applied.
7. **The rules it fixes,** if it fixes veraPDF failures. Map each one, for PDF/UA-1 and PDF/UA-2,
   in `fixesByRule` (`VeraPdfValidator.fs`): `ua1 "7.1-5" PdfFixOptions.RoleMap`. A fix for a
   Legibyl warning only (joining split paragraphs, say) has no rules here: its warning's
   `FixOptions` points to it.
8. **The rule guide,** for those rules. Their `whatToDo` in `RuleGuide.yaml` names it the way the dialog
   does ("Choose Map the PDF's own tag names in Auto Fixes…"), before the other ways to fix it.
   Use a YAML anchor (`&name`, then `*name`) when several rules share the advice.
9. **The Auto Fixes dialog.** A choice in `Dialogs.fs` (its title and a sentence of what it does,
   in the user's terms), and its title in `FixDialogTests`.
10. **The README's fix table**, between `<!-- fixes:start -->` and `<!-- fixes:end -->`: a row in
    `FixNames.order`'s order (a listing order: it isn't exactly the order fixes run), exactly as
    `FixNames.fixes ()` gives it (its name, its `PdfFixDescriptions` line,
    and notes: `safe`, `uses the layout models`…).

`FixNamesTests` (in the CLI tests) catch some of these: a flag missing from `FixNames.order`, a
wrong `All`, a README fix table that doesn't match, and "repair" in what people read. The rest
(the rule map, the rule guide, `readsText`, the README table of checks) nothing checks, so go through them.

## Wording

- User-facing text says "fix", never "repair". `FixNamesTests` checks this; only veraPDF's own
  "repair status" is allowed.
- Say what it means for someone reading the PDF, not the PDF object ("a screen reader can't
  tell a heading from a paragraph", not just "/S isn't standard").
- Something left alone is said, and why: never a silent skip.

## Tests

- Build synthetic PDFs in the test (iText). Never use real documents: the repo goes public, and
  the user's own files stay on their machine.
- The test project can't see internal modules (`LegibylChecks`, `StructureFixes`,
  `StructureTreeObjects`…). Test through the public API: `VeraPdfValidator().Validate`, `.Fix`
  and `.ReadStructureTree`.
- A new test file for each check or fix, added to `Legibyl.Validation.Tests.fsproj`.
- For a fix, check:
  - for a fix of veraPDF failures: veraPDF fails the rule before and passes it after. For a fix of
    a Legibyl warning only: the warning is there before and gone after (and veraPDF's results
    are no worse);
  - a fix that shouldn't change how the page looks renders it pixel-identically (see `render` in
    `FontDictionariesTests.fs`, with `PdfRegionRenderer`);
  - a file that's already fine is left alone (`WasFixed` false, no copy written);
  - what it can't fix is reported, not dropped.
- For a guard (a case that has to be left alone), take the guard out once and see the test fail,
  so you know the test proves it.
- Before pushing, run all three suites: `dotnet test tests/Legibyl.Validation.Tests`,
  `tests/Legibyl.Cli.Tests` and `tests/Legibyl.Gui.Tests`.
