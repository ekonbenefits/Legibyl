/// Reviews saved and read as the app does (see DocumentReview), to set up and check tests.
module Legibyl.Gui.Tests.Reviews

open Legibyl.Gui

/// pdf's review as it's read when the PDF is opened: from the file next to it (and any carried
/// inside it).
let read (pdf: string) = DocumentReviews().For pdf

/// Saves pdf's review with change made to it, as Save does.
let save (pdf: string) (change: DocumentReview -> unit) =
    let review = read pdf
    change review
    review.Save pdf

/// Saves edits as pdf's review's edits.
let saveEdits (pdf: string) (edits: RegionEdits.Edit list) = save pdf (fun review -> review.Edits <- edits)
