/// What the progress row under the file row shows while something runs.
module Legibyl.Gui.BusyStatus

open Legibyl.Validation

/// What the progress row under the file row is showing.
type BusyProgress =
    /// Validating, and what: "Validating", or "Validating the fixed copy".
    | Validating of activity: string * ValidationProgress
    | ReadingStructure of StructureReadProgress
    /// Downloading the layout model for tagging from the layout: the fraction done.
    | DownloadingLayoutModel of float
    /// Fixing: which step, and the page within it where there are pages.
    | Fixing of FixProgress
    /// Appending PDFs (see Append.run): what it's doing, "Validating garden.pdf…".
    | Appending of stage: string

/// What the progress row says is happening: always the activity first ("Validating: page 3
/// of 10", "Fixing, step 2 of 4: …").
let statusText (progress: BusyProgress) : string =
    match progress with
    | Validating(activity, progress) ->
        let status =
            match progress.Stage with
            | ValidationStage.CheckingPages -> $"{activity}: page {progress.Page:N0} of {progress.PageCount:N0}"
            | ValidationStage.Finishing -> $"{activity}: finishing up…"
            | _ -> $"{activity}: reading the PDF…"
        status
    | ReadingStructure progress ->
        let status =
            match progress.Stage with
            | StructureReadStage.ReadingText -> $"Reading the structure: page text {progress.Page:N0} of {progress.PageCount:N0}"
            | StructureReadStage.ReadingLinks -> "Reading the structure: links…"
            | _ -> "Reading the structure: building the tree…"
        status
    | DownloadingLayoutModel fraction -> $"Downloading the layout model: {fraction:P0}"
    | Appending stage -> $"Appending: {stage}"
    | Fixing progress ->
        let step = $"Fixing, step {progress.Step} of {progress.Steps}: "
        let pages = progress.Page > 0 && progress.PageCount > 0
        let status =
            match progress.Stage with
            | FixStage.CheckingOriginal when pages -> $"{step}checking the original, page {progress.Page:N0} of {progress.PageCount:N0}"
            | FixStage.CheckingOriginal -> $"{step}checking the original…"
            | FixStage.Tagging when pages -> $"{step}finding page {progress.Page:N0} of {progress.PageCount:N0}'s layout, to tag it"
            | FixStage.Tagging -> $"{step}tagging from the layout…"
            // Only joining split paragraphs reports pages while fixing.
            | FixStage.Fixing when pages -> $"{step}finding page {progress.Page:N0} of {progress.PageCount:N0}'s layout, to join split paragraphs"
            | FixStage.Fixing -> $"{step}applying the fixes…"
            | _ -> $"{step}fixing metadata with veraPDF…"
        status
