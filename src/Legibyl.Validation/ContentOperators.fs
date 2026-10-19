/// A page's content-stream operators by name (ISO 32000-1, Annex A), so a misspelt one doesn't
/// compile. Literals, so a match can use them as patterns (| ContentOperators.BDC -> …).
module internal Legibyl.Validation.ContentOperators

// Marked content.

[<Literal>]
let BMC = "BMC"

[<Literal>]
let BDC = "BDC"

[<Literal>]
let EMC = "EMC"

// Text objects, the graphics state and compatibility sections.

[<Literal>]
let BT = "BT"

[<Literal>]
let ET = "ET"

[<Literal>]
let q = "q"

[<Literal>]
let Q = "Q"

[<Literal>]
let BX = "BX"

[<Literal>]
let EX = "EX"

// Positioning text.

/// Moves to the start of the next line, by an offset from this one's start.
[<Literal>]
let Td = "Td"

/// As Td, and sets the leading.
[<Literal>]
let TD = "TD"

/// To the start of the next line, by the leading.
[<Literal>]
let TStar = "T*"

[<Literal>]
let Tm = "Tm"

// Showing text.

[<Literal>]
let Tj = "Tj"

[<Literal>]
let TJ = "TJ"

/// ': to the next line, then show text.
[<Literal>]
let NextLineTj = "'"

/// ": set the word and character spacing, to the next line, then show text.
[<Literal>]
let SpacedNextLineTj = "\""

// Images, form XObjects and shadings.

[<Literal>]
let Do = "Do"

/// The end of an inline image (BI, its dictionary, ID, its data, EI): iText's parser reads it
/// as one operation, by its last operator.
[<Literal>]
let EI = "EI"

[<Literal>]
let sh = "sh"

// Path construction, and clipping.

[<Literal>]
let m = "m"

[<Literal>]
let l = "l"

[<Literal>]
let c = "c"

[<Literal>]
let v = "v"

[<Literal>]
let y = "y"

[<Literal>]
let h = "h"

[<Literal>]
let re = "re"

[<Literal>]
let W = "W"

[<Literal>]
let WStar = "W*"

// Path painting.

[<Literal>]
let S = "S"

[<Literal>]
let s = "s"

[<Literal>]
let f = "f"

[<Literal>]
let F = "F"

[<Literal>]
let fStar = "f*"

[<Literal>]
let B = "B"

[<Literal>]
let BStar = "B*"

[<Literal>]
let b = "b"

[<Literal>]
let bStar = "b*"

/// Ends a path without painting it.
[<Literal>]
let n = "n"
