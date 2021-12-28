namespace MF.ImageManager.Command

open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ImageManager

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
            |> List.groupBy Image.model
            |> List.map (fun (k, v) -> k, v |> List.length)

        model
        |> List.sortBy snd
        |> List.map (fun (model, count) -> [ model |> Option.defaultValue "-"; string count ])
        |> output.Table [ "Model"; "Count" ]

        if output.IsDebug() then
            images
            |> List.collect (fun i ->
                let firstMeta, meta =
                    let format (k, v) =
                        $"<c:yellow>{k |> MetaAttribute.value}</c>: {v}"

                    match i.Metadata |> Map.toList with
                    | [] -> "<c:red>---</c>", []
                    | [ one ] -> one |> format, []
                    | first :: rest -> first |> format, rest |> List.map format

                [
                    yield [
                        $"<c:cyan>{i.Name}</c>"
                        $"<c:gray>{i.FullPath}</c>"
                        firstMeta
                    ]
                    yield! meta |> List.map (fun m -> [ ""; ""; m ])
                ]
            )
            |> output.Table [ "Image"; "Path"; "Meta" ]

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

            if output.IsVerbose() then
                output.Message <| sprintf "FFMpeg: %A" ffmpeg

            let ignoreWarnings = false

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
