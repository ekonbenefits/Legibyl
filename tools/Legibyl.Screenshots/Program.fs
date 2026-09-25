/// Saves a screenshot of the Legibyl window with a PDF open, without needing a display.
module Legibyl.Screenshots.Program

open System
open Avalonia
open Avalonia.Styling
open Legibyl.Screenshots

let private usage =
    """Usage: dotnet run --project tools/Legibyl.Screenshots -- <pdf> <out.png> [options]

Options:
  --theme light|dark   Theme variant (default: light).
  --select <n>         Show result row n (0-based) instead of the first.
  --size <w>x<h>       Window size in pixels (default: 1200x720)."""

type private Options =
    { Pdf: string
      Output: string
      Theme: ThemeVariant
      Row: int option
      Width: float
      Height: float }

let rec private parse (options: Options) (args: string list) =
    match args with
    | [] -> Ok options
    | "--theme" :: "light" :: rest -> parse { options with Theme = ThemeVariant.Light } rest
    | "--theme" :: "dark" :: rest -> parse { options with Theme = ThemeVariant.Dark } rest
    | "--select" :: n :: rest ->
        match Int32.TryParse n with
        | true, row when row >= 0 -> parse { options with Row = Some row } rest
        | _ -> Error $"--select expects a row number, got '{n}'."
    | "--size" :: size :: rest ->
        match size.Split 'x' with
        | [| w; h |] ->
            match Double.TryParse w, Double.TryParse h with
            | (true, width), (true, height) -> parse { options with Width = width; Height = height } rest
            | _ -> Error $"--size expects <w>x<h>, got '{size}'."
        | _ -> Error $"--size expects <w>x<h>, got '{size}'."
    | option :: _ -> Error $"Unknown option '{option}'."

[<EntryPoint>]
let main argv =
    let parsed =
        match List.ofArray argv with
        | pdf :: output :: rest ->
            parse
                { Pdf = IO.Path.GetFullPath pdf
                  Output = IO.Path.GetFullPath output
                  Theme = ThemeVariant.Light
                  Row = None
                  Width = 1200.0
                  Height = 720.0 }
                rest
        | _ -> Error "Expected a PDF path and an output path."

    match parsed with
    | Error message ->
        eprintfn $"{message}\n\n{usage}"
        64
    | Ok options ->
        HeadlessWindow.HeadlessApp.BuildAvaloniaApp().SetupWithoutStarting() |> ignore
        match Application.Current with
        | null -> ()
        | app -> app.RequestedThemeVariant <- options.Theme

        let window = HeadlessWindow.show options.Width options.Height
        HeadlessWindow.openPdf window options.Pdf
        options.Row |> Option.iter (HeadlessWindow.selectRow window)
        HeadlessWindow.savePng window options.Output
        printfn $"Saved {options.Output}"
        0
