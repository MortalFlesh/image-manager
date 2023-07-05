module internal Utils

open System
open System.IO

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Tools.Git

let tee f a =
    f a
    a

let skipOn option action p =
    if p.Context.Arguments |> Seq.contains option
    then Trace.tracefn "Skipped ..."
    else action p

let createProcess exe arg dir =
    CreateProcess.fromRawCommandLine exe arg
    |> CreateProcess.withWorkingDirectory dir
    |> CreateProcess.ensureExitCode

let run proc arg dir =
    proc arg dir
    |> Proc.run
    |> ignore

let orFail = function
    | Error e -> raise e
    | Ok ok -> ok

let stringToOption = function
    | null | "" -> None
    | string -> Some string

[<RequireQualifiedAccess>]
module Dotnet =
    let dotnet = createProcess "dotnet"

    let run command dir = try run dotnet command dir |> Ok with e -> Error e
    let runInRoot command = run command "."
    let runOrFail command dir = run command dir |> orFail
    let runInRootOrFail command = run command "." |> orFail
