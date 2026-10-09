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
    /// Something with no steps to count, said as it is: "Opening garden.pdf…".
    | Working of status: string

/// What the progress row says is happening: always the activity first ("Validating: page 3
/// of 10", "Fixing, step 2 of 4: …").
let statusText (progress: BusyProgress) : string =
    match progress with
    | Validating(activity, progress) ->
        let status =
            match progress.Stage with
            | ValidationStage.CheckingPages -> $"{activity}: page {progress.Page:N0} of {progress.PageCount:N0}"
            | ValidationStage.Finishing -> $"{activity}: finishing up…"
            // veraPDF doesn't say which page it's on here, so no page count.
            | ValidationStage.CheckingWcag -> $"{activity}: checking {WcagSpecification.Name} (contrast, text size and more)…"
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
    | Working status -> status
    | Fixing progress ->
        let step = $"Fixing, step {progress.Step} of {progress.Steps}: "
        let pages = progress.Page > 0 && progress.PageCount > 0
        let status =
            match progress.Stage with
            | FixStage.CheckingOriginal when pages -> $"{step}checking the original, page {progress.Page:N0} of {progress.PageCount:N0}"
            | FixStage.CheckingOriginal -> $"{step}checking the original…"
            | FixStage.CheckingWcag -> $"{step}checking the original against {WcagSpecification.Name} (contrast, text size and more)…"
            | FixStage.Tagging when pages -> $"{step}finding page {progress.Page:N0} of {progress.PageCount:N0}'s layout, to tag it"
            | FixStage.Tagging -> $"{step}tagging from the layout…"
            // Only joining split paragraphs reports pages while fixing.
            | FixStage.Fixing when pages -> $"{step}finding page {progress.Page:N0} of {progress.PageCount:N0}'s layout, to join split paragraphs"
            | FixStage.Fixing -> $"{step}applying the fixes…"
            | _ -> $"{step}fixing metadata with veraPDF…"
        status

/// What a screen reader is told of it (see MainWindow's progress row): the activity, and how far
/// it's got a tenth at a time ("Validating: 40%"), not every page, which would be too much to hear.
let announcement (progress: BusyProgress) : string =
    let tenth (done': float) = $"{int (max 0.0 (min 1.0 done') * 10.0) * 10}%%"
    let pages (page: int) (count: int) = if count > 0 then Some(tenth (float (page - 1) / float count)) else None
    match progress with
    | Validating(activity, progress) when progress.Stage = ValidationStage.CheckingPages ->
        match pages progress.Page progress.PageCount with
        | Some done' -> $"{activity}: {done'}"
        | None -> statusText (Validating(activity, progress))
    | ReadingStructure progress when progress.Stage = StructureReadStage.ReadingText ->
        match pages progress.Page progress.PageCount with
        | Some done' -> $"Reading the structure: {done'}"
        | None -> statusText (ReadingStructure progress)
    | DownloadingLayoutModel fraction -> $"Downloading the layout model: {tenth fraction}"
    | Fixing progress when progress.Page > 0 && progress.PageCount > 0 ->
        $"Fixing, step {progress.Step} of {progress.Steps}: {tenth (float (progress.Page - 1) / float progress.PageCount)}"
    | Fixing progress -> $"Fixing, step {progress.Step} of {progress.Steps}"
    | progress -> statusText progress
