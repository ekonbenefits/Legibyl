/// One headless Avalonia session (and UI thread) for the whole test run: Avalonia allows
/// only one application per process, so every test module shares it.
module Legibyl.Gui.Tests.HeadlessSession

open System
open System.Threading
open Legibyl.Screenshots

let private session =
    lazy (Avalonia.Headless.HeadlessUnitTestSession.StartNew(typeof<HeadlessWindow.HeadlessApp>))

let onUiThread (test: unit -> unit) =
    session.Value.Dispatch(Action test, CancellationToken.None).GetAwaiter().GetResult()
