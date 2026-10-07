/// The Structure window's page preview: the elements a page outlines and numbers, in reading
/// order, and which one a point on the preview is over.
module internal Legibyl.Gui.StructurePreview


open System
open System.IO
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.Builder
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Types
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform.Storage
open Avalonia.VisualTree
open Legibyl.Layout
open Legibyl.Rendering
open Legibyl.Validation
open Legibyl.Gui.StructureColors

/// The elements a page's overview outlines, in reading order (each node's children in the
/// order children gives): those that own content on it, outermost first (one inside another
/// that does is within that one's outline).
let pageElements (children: StructureNode -> StructureNode list) (root: StructureNode) (page: int) : StructureNode list =
    let owns (node: StructureNode) =
        node.ContentLeafCount > 0
        && (match node.Geometry with
            | NonNull geometry -> geometry.PageNumber = page
            | Null -> false)
    let rec collect (node: StructureNode) : StructureNode list =
        if owns node then [ node ] else [ for child in children node do yield! collect child ]
    [ for child in root.Children do yield! collect child ]

/// The first page node's content is on: its own, else its first descendant's.
let rec firstPage (node: StructureNode) : int option =
    match node.Geometry with
    | NonNull geometry -> Some geometry.PageNumber
    | Null -> node.Children |> Seq.tryPick firstPage


/// Pixels per point the page preview is rendered at.
let previewScale = 2.0f


/// The smallest element on page whose region holds (x, y), in PDF points: the innermost of
/// nested ones (a link in its paragraph, an item in its list).
let nodeAt (root: StructureNode) (page: int) (x: float) (y: float) =
    let rec all (node: StructureNode) : StructureNode seq =
        seq {
            yield node
            for child in node.Children do
                yield! all child
        }
    all root
    |> Seq.choose (fun node ->
        match node.Geometry with
        | NonNull g when g.PageNumber = page && x >= g.Left && x <= g.Right && y >= g.Bottom && y <= g.Top ->
            Some(node, (g.Right - g.Left) * (g.Top - g.Bottom))
        | _ -> None)
    |> Seq.sortBy snd
    |> Seq.tryHead
    |> Option.map fst

let private onPreview (image: Image) (bitmap: Bitmap) (position: Point) (clamp: bool) =
    let size = bitmap.Size
    let bounds = image.Bounds.Size
    if size.Width <= 0.0 || size.Height <= 0.0 || bounds.Width <= 0.0 || bounds.Height <= 0.0 then
        None
    else
        let fit = min (bounds.Width / size.Width) (bounds.Height / size.Height)
        let x = (position.X - (bounds.Width - size.Width * fit) / 2.0) / fit
        let y = (position.Y - (bounds.Height - size.Height * fit) / 2.0) / fit
        let x, y = if clamp then Math.Clamp(x, 0.0, size.Width), Math.Clamp(y, 0.0, size.Height) else x, y
        if x < 0.0 || y < 0.0 || x > size.Width || y > size.Height then
            None
        else
            // The bitmap's pixels to points; PDF's y runs up from the bottom.
            let pixels = float bitmap.PixelSize.Width / size.Width
            let scale = float previewScale
            Some(x * pixels / scale, (size.Height - y) * pixels / scale)

/// Where on its page, in PDF points, a pointer is over a page preview rendered at
/// previewScale; None off the page (the image is scaled to fit, centred).
let pointOnPage (image: Image) (bitmap: Bitmap) (position: Point) = onPreview image bitmap position false

/// pointOnPage, with a pointer off the page taken to its nearest edge (a drag past the page).
let pointOnPageClamped (image: Image) (bitmap: Bitmap) (position: Point) = onPreview image bitmap position true

/// Where a point on its page, in PDF points, is on a page preview of the given size showing
/// bitmap: the inverse of pointOnPage.
let pageToPreview (bounds: Size) (bitmap: Bitmap) (x: float, y: float) =
    let size = bitmap.Size
    let fit = min (bounds.Width / size.Width) (bounds.Height / size.Height)
    let pixels = float bitmap.PixelSize.Width / size.Width
    let scale = float previewScale
    Point((bounds.Width - size.Width * fit) / 2.0 + x * scale / pixels * fit,
          (bounds.Height - size.Height * fit) / 2.0 + (size.Height - y * scale / pixels) * fit)
