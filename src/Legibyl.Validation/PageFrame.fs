/// A page as it's shown: its visible area (crop box) turned as its /Rotate says (clockwise, a
/// multiple of 90). A scan is often stored sideways and turned for display. Boxes in the
/// structure are in the page's own space (PDF user space); a rendering, and the rows and
/// lines a reader sees, are in the page's shown, upright space: points from the shown page's
/// bottom-left corner, y up.
module internal Legibyl.Validation.PageFrame

type Frame =
    { Left: float
      Bottom: float
      Right: float
      Top: float
      /// Clockwise: 0, 90, 180 or 270.
      Rotation: int }

let create (left: float) (bottom: float) (right: float) (top: float) (rotation: int) =
    { Left = left; Bottom = bottom; Right = right; Top = top; Rotation = ((rotation % 360) + 360) % 360 }

let private turnedSideways frame = frame.Rotation = 90 || frame.Rotation = 270

/// The shown page's width and height, in points.
let shownSize frame =
    let width, height = frame.Right - frame.Left, frame.Top - frame.Bottom
    if turnedSideways frame then height, width else width, height

/// A point in the page's space, in the shown page's.
let toUpright frame (x: float, y: float) =
    match frame.Rotation with
    | 90 -> y - frame.Bottom, frame.Right - x
    | 180 -> frame.Right - x, frame.Top - y
    | 270 -> frame.Top - y, x - frame.Left
    | _ -> x - frame.Left, y - frame.Bottom

/// A point in the shown page's space, in the page's.
let fromUpright frame (u: float, v: float) =
    match frame.Rotation with
    | 90 -> frame.Right - v, frame.Bottom + u
    | 180 -> frame.Right - u, frame.Top - v
    | 270 -> frame.Left + v, frame.Top - u
    | _ -> frame.Left + u, frame.Bottom + v

/// The matrix (a b c d e f) taking the shown page's space to the page's: what's drawn in it
/// reads upright.
let uprightMatrix frame =
    let e, f = fromUpright frame (0.0, 0.0)
    let a, b = fromUpright frame (1.0, 0.0)
    let c, d = fromUpright frame (0.0, 1.0)
    a - e, b - f, c - e, d - f, e, f

let private boxOf pageNumber ((x1: float, y1: float), (x2: float, y2: float)) : StructureGeometry =
    { PageNumber = pageNumber
      Left = min x1 x2
      Right = max x1 x2
      Bottom = min y1 y2
      Top = max y1 y2 }

/// A box in the page's space, in the shown page's.
let boxToUpright frame (g: StructureGeometry) =
    boxOf g.PageNumber (toUpright frame (g.Left, g.Bottom), toUpright frame (g.Right, g.Top))

/// A box in the shown page's space, in the page's.
let boxFromUpright frame (g: StructureGeometry) =
    boxOf g.PageNumber (fromUpright frame (g.Left, g.Bottom), fromUpright frame (g.Right, g.Top))

/// A box in a rendering of the shown page (pixels, top-left origin, scale pixels per point:
/// left, top, right, bottom), in the page's space.
let fromPixels frame pageNumber (scale: float) (left: float, top: float, right: float, bottom: float) =
    let _, height = shownSize frame
    boxFromUpright
        frame
        { PageNumber = pageNumber
          Left = left / scale
          Right = right / scale
          Top = height - top / scale
          Bottom = height - bottom / scale }

/// A box in the page's space, in a rendering of the shown page: left, top, right, bottom pixels.
let toPixels frame (scale: float) (g: StructureGeometry) =
    let _, height = shownSize frame
    let u = boxToUpright frame g
    u.Left * scale, (height - u.Top) * scale, u.Right * scale, (height - u.Bottom) * scale
