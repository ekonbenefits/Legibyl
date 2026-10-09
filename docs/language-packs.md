# Language packs

Legibyl ships with English and Spanish. A *language pack* adds what it needs to know of
another language, as data, with no code and nothing to build. Write one for your language,
drop it in a folder, and Legibyl picks it up the next time it starts.

Without a pack, Legibyl still checks and fixes a PDF in any language. It can tell which
language a text is in for 53 languages without a pack. A pack adds what it can't work out:

| In the pack | What Legibyl does with it |
|---|---|
| `ocr` | Its [Tesseract](https://github.com/tesseract-ocr/tessdata_fast) model reads scanned pages and text drawn as outlines in the language. Without it, they're read with English's model. |
| `phrases` | Text Legibyl writes into PDFs itself (a QR code's alt text, and the description of the link it adds over it) is written in the language, and marked as that language. Without it, the text is in English, marked as English. |
| `sentenceContinuations` | Words that start the rest of a sentence. A paragraph starting with one is warned about as likely split from the one before. |
| `standaloneHeadings` | Words that start a paragraph standing on its own ("SECTION 3"), which isn't joined to the one before. |

## Where packs go

A pack is a folder named for its language's tag (`fr`, `pt-BR`), holding a `language.yaml`.
Legibyl reads packs from these folders, in this order:

1. its own `languages` folder, beside the app: English and Spanish;
2. your own: `%LOCALAPPDATA%\Legibyl\languages` on Windows,
   `~/Library/Application Support/Legibyl/languages` on a Mac, and
   `~/.local/share/Legibyl/languages` on Linux;
3. any folders named by the `LEGIBYL_LANGUAGES` environment variable, separated as `PATH`
   separates folders (`;` on Windows, `:` elsewhere). Use this for a folder of packs an
   organisation shares.

`legibyl list languages` shows the packs found, the folders they're read from, and what's
wrong with any of them.

When more than one folder has a pack for a language, they're read as one:
- the later one's model replaces the earlier's;
- the later one's phrases replace the earlier's, one by one;
- its words are added to the earlier's;
- its name is used, if it gives one.

So, to add a word to Spanish, write a pack with just that word in your own folder.

A language's pack is used for its regional variants too: an `fr` pack is used for `fr-CA`,
unless there's an `fr-CA` pack.

## language.yaml

```yaml
# A language pack for French.
tag: fr          # as a PDF's /Lang gives it; the folder's name if left out
name: French     # in English

# Tesseract's model, in this folder: tessdata_fast's or tessdata_best's fra.traineddata.
ocr: fra.traineddata

# What Legibyl writes into PDFs itself. Each keeps the {names} the English has.
phrases:
  qrCodeLinkingTo: "Code QR menant à {title} ({address})"
  qrCodeLinkingToAddress: "Code QR menant à {address}"
  qrCode: "Code QR : {text}"
  qrCodeLinkTo: "Lien du code QR vers {title} ({address})"
  qrCodeLinkToAddress: "Lien du code QR vers {address}"

# A paragraph that starts with one of these likely goes on from the one before.
sentenceContinuations: [et, ou, de, du, des, la, le, les, en, que, pour]

# A paragraph that starts with one of these stands on its own.
standaloneHeadings: [SECTION, ARTICLE]
```

Every field can be left out. The phrases, in English, are:

| Key | English |
|---|---|
| `qrCodeLinkingTo` | QR code linking to {title} ({address}) |
| `qrCodeLinkingToAddress` | QR code linking to {address} |
| `qrCode` | QR code: {text} |
| `qrCodeLinkTo` | QR code link to {title} ({address}) |
| `qrCodeLinkToAddress` | QR code link to {address} |

The first three are a QR code's alt text: where it links (with the title of the page it goes
to, when Legibyl has read it), or what it says when it isn't a web address. The last two
describe the link Legibyl adds over a QR code.

The built-in packs, in the app's `languages` folder (and the repository's), are examples.

## When something's wrong

A pack Legibyl can't read is left out. A phrase that isn't one Legibyl writes, or that
loses one of its `{names}`, is left out on its own. Neither stops Legibyl working. They're
listed by `legibyl list languages`, which then exits with 1.
