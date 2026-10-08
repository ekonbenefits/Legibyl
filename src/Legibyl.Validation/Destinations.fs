/// The kinds of destination a link or bookmark can go to (ISO 32000-1, 12.3.2.2): how the page
/// is shown when it gets there. Literals, so a match can use them as patterns.
module Legibyl.Validation.Destinations

/// A point (left, top), at a zoom.
[<Literal>]
let XYZ = "XYZ"

/// The whole page.
[<Literal>]
let Fit = "Fit"

/// The page's width, from a top.
[<Literal>]
let FitH = "FitH"

/// The page's height, from a left.
[<Literal>]
let FitV = "FitV"

/// A rectangle (left, bottom, right, top).
[<Literal>]
let FitR = "FitR"

/// The page's content.
[<Literal>]
let FitB = "FitB"

/// The width of the page's content, from a top.
[<Literal>]
let FitBH = "FitBH"

/// The height of the page's content, from a left.
[<Literal>]
let FitBV = "FitBV"
