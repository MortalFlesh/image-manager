namespace MF.ImageManager.Command

open Microsoft.Extensions.Logging
open System.IO
open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ImageManager
open MF.Utils
open MF.Utils.Logging
open MF.ImageManager.Prepare

[<RequireQualifiedAccess>]
module CachePreload =
    let arguments = [
        Argument.optional "target" "Directory you want to add to cache. Alternatively you can pass a config option and use a config file (<c:yellow>source</c> field will be used as source in that case)." None
    ]

    let options = CommonOptions.all @ [
        Option.optional "config" (Some "c") "If set, config file will be used (other options set directly, will override a config values)." None
    ]

    let private run (io: MF.ConsoleApplication.IO) loggerFactory ffmpeg target = asyncResult {
        let! files =
            target
            |> Finder.findAllFilesInDir io loggerFactory ffmpeg

        do! files
            |> Hash.Cache.init io loggerFactory

        return "Done"
    }

    let execute = ExecuteAsyncResult <| fun ((input, output): IO) ->
        asyncResult {
            use loggerFactory =
                if output.IsDebug() then "vvv"
                elif output.IsVeryVerbose() then "vv"
                else "v"
                |> LogLevel.parse
                |> LoggerFactory.create "CachePreload"

            let target = input |> Input.Argument.asString "target"

            let! (parsedConfig: Config option) =
                match input with
                | Input.Option.Has "config" (OptionValue.ValueOptional (Some config)) ->
                    config
                    |> File.ReadAllText
                    |> Config.parse TargetDirMode.Exclude
                    |> Result.map Some
                | _ -> Ok None
                |> AsyncResult.ofResult
                |> AsyncResult.mapError List.singleton

            let! target =
                target
                |> Option.orElse (
                    parsedConfig
                    |> Option.bind (function
                        | { Source = [] } -> None
                        | { Source = [ singleSource ] } -> Some singleSource
                        | _ -> None
                    )
                )
                |> Result.ofOption [ PrepareError.ErrorMessage "Target dir must be passed as an argument or in a config as a single source value." ]

            let! ffmpeg =
                match input with
                | Input.Option.Has CommonOptions.FFMpeg (OptionValue.ValueOptional value) -> FFMpeg.init value
                | _ -> Ok FFMpeg.Empty
                |> AsyncResult.ofResult
                |> AsyncResult.mapError List.singleton

            if output.IsVerbose() then
                output.Message <| sprintf "FFMpeg: %A" ffmpeg

            let! message = target |> run (input, output) loggerFactory ffmpeg

            output.Success message

            return ExitCode.Success
        }
        |> AsyncResult.waitAfterFinish output 2000
        |> AsyncResult.mapError (Errors.map "Cache Preload Command" output PrepareError.format)
