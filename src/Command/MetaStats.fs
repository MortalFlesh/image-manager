namespace MF.ImageManager.Command

open System.IO
open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ImageManager
open MF.ImageManager.Prepare
open MF.Utils

[<RequireQualifiedAccess>]
module MetaStatsCommand =
    let arguments = [
        Argument.required "target" "Directory you want to check."
    ]

    let options = [
        Option.optional "ffmpeg" None "FFMpeg path in the current dir" None
    ]

    let private run output ignoreWarnings ffmpeg target = asyncResult {
        let! images =
            target
            |> Finder.findAllImagesInDir output ignoreWarnings ffmpeg None

        let model =
            images
            |> List.groupBy (fun image -> image.Metadata |> Map.tryFind "Model")
            |> List.map (fun (k, v) -> k, v |> List.length)

        model
        |> List.sortBy snd
        |> List.map (fun (model, count) -> [ model |> Option.defaultValue "-"; string count ])
        |> output.Table [ "Model"; "Count" ]

        return "Done"
    }

    let execute ((input, output): IO) =
        asyncResult {
            let target = input |> Input.getArgumentValue "target"

            let! ffmpeg =
                match input with
                | Input.HasOption "ffmpeg" (OptionValue.ValueOptional value) -> FFMpeg.init value
                | _ -> Ok FFMpeg.Empty
                |> AsyncResult.ofResult
                |> AsyncResult.mapError List.singleton

            let ignoreWarnings = true

            return! target |> run output ignoreWarnings ffmpeg
        }
        |> Async.RunSynchronously
        |> function
            | Ok message ->
                output.Success message
                ExitCode.Success
            | Error errors ->
                errors
                |> List.iter (function
                    | PrepareError.Exception e -> output.Error e.Message
                    | PrepareError.ErrorMessage message -> output.Error message
                )

                ExitCode.Error
