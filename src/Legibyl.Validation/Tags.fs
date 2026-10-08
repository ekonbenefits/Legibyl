/// Structure tags by name: the standard structure types of PDF 1.7 (ISO 32000-1, 14.8.4) and
/// PDF 2.0 (ISO 32000-2, 14.8.4), so a misspelt tag doesn't compile instead of quietly never
/// matching. Literals, so a match can use them as patterns (| Tags.Figure -> …). A tag is still a
/// string: a PDF's own tag names, mapped to these through its role map, are legal too.
module Legibyl.Validation.Tags

// Grouping.

[<Literal>]
let Document = "Document"

[<Literal>]
let DocumentFragment = "DocumentFragment"

[<Literal>]
let Part = "Part"

[<Literal>]
let Art = "Art"

[<Literal>]
let Sect = "Sect"

[<Literal>]
let Div = "Div"

[<Literal>]
let Aside = "Aside"

[<Literal>]
let NonStruct = "NonStruct"

[<Literal>]
let Private = "Private"

[<Literal>]
let BlockQuote = "BlockQuote"

[<Literal>]
let TOC = "TOC"

[<Literal>]
let TOCI = "TOCI"

[<Literal>]
let Index = "Index"

// Blocks of text.

[<Literal>]
let P = "P"

[<Literal>]
let H = "H"

[<Literal>]
let H1 = "H1"

[<Literal>]
let H2 = "H2"

[<Literal>]
let H3 = "H3"

[<Literal>]
let H4 = "H4"

[<Literal>]
let H5 = "H5"

[<Literal>]
let H6 = "H6"

[<Literal>]
let Title = "Title"

[<Literal>]
let Caption = "Caption"

[<Literal>]
let Note = "Note"

[<Literal>]
let FENote = "FENote"

[<Literal>]
let Code = "Code"

// Lists.

[<Literal>]
let L = "L"

[<Literal>]
let LI = "LI"

[<Literal>]
let Lbl = "Lbl"

[<Literal>]
let LBody = "LBody"

// Tables.

[<Literal>]
let Table = "Table"

[<Literal>]
let TR = "TR"

[<Literal>]
let TH = "TH"

[<Literal>]
let TD = "TD"

[<Literal>]
let THead = "THead"

[<Literal>]
let TBody = "TBody"

[<Literal>]
let TFoot = "TFoot"

// Inline.

[<Literal>]
let Span = "Span"

[<Literal>]
let Quote = "Quote"

[<Literal>]
let Reference = "Reference"

[<Literal>]
let BibEntry = "BibEntry"

[<Literal>]
let Link = "Link"

[<Literal>]
let Annot = "Annot"

[<Literal>]
let Em = "Em"

[<Literal>]
let Strong = "Strong"

[<Literal>]
let Sub = "Sub"

[<Literal>]
let Ruby = "Ruby"

[<Literal>]
let RB = "RB"

[<Literal>]
let RT = "RT"

[<Literal>]
let RP = "RP"

[<Literal>]
let Warichu = "Warichu"

[<Literal>]
let WT = "WT"

[<Literal>]
let WP = "WP"

// Illustrations.

[<Literal>]
let Figure = "Figure"

[<Literal>]
let Formula = "Formula"

[<Literal>]
let Form = "Form"

/// PDF 2.0's structure type for content that isn't part of what's read (and the marked-content
/// tag of the same name, in a page's content). Legibyl also retags to it, to make an element's
/// content an artifact.
[<Literal>]
let Artifact = "Artifact"

/// PDF 1.7's standard structure types (ISO 32000-1, 14.8.4): what a role map has to end at, and
/// mustn't map from.
let standard =
    set [ Document; Part; Art; Sect; Div; BlockQuote; Caption; TOC; TOCI; Index; NonStruct; Private
          P; H; H1; H2; H3; H4; H5; H6; L; LI; Lbl; LBody
          Table; TR; TH; TD; THead; TBody; TFoot
          Span; Quote; Note; Reference; BibEntry; Code; Link; Annot; Ruby; RB; RT; RP; Warichu; WT; WP
          Figure; Formula; Form ]

/// The tags a review's edit can retag an element to: a standard one, or Artifact for a suggested
/// region that's page furniture.
let retagTargets = standard |> Set.add Artifact

/// The tags that only group others: when the tags are one of these at the top, the document's
/// top-level elements are the ones inside it.
let wrappers = set [ Document; Part; Art; Sect; Div ]

/// The headings, H1 to H6 by level.
let headings = [ H1; H2; H3; H4; H5; H6 ]
