namespace MF.ImageManager.Command

open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ImageManager
open MF.Utils
open MF.Utils.Logging

[<RequireQualifiedAccess>]
module MetaStatsCommand =
    let arguments = [
        Argument.required "target" "Directory you want to check."
    ]

    let options = [
        Option.optional "ffmpeg" None "FFMpeg path in the current dir" None
        Progress.noProgressOption
    ]

    let private run ((_, output as io): MF.ConsoleApplication.IO) loggerFactory ffmpeg target = asyncResult {
        let! files =
            target
            |> Finder.findAllFilesInDir io loggerFactory ffmpeg

        files
        |> List.groupBy File.model
        |> List.map (fun (k, v) -> k, v |> List.length)
        |> List.sortBy snd
        |> List.map (fun (model, count) -> [ model |> Option.defaultValue "-"; string count ])
        |> output.Table [ "Model"; "Count" ]

        let filter items =
            if output.IsDebug() then items
            else items |> List.filter (snd >> (fun x -> x > 1))

        (*
        let crypt v = v, v |> Crypt.md5 // or other alg

        files
        |> List.groupBy (File.hash >> Option.map Hash.value >> Option.defaultValue "-" >> crypt)
        |> List.map (fun (k, v) -> k, v |> List.length)
        |> (tee (fun all ->
            if (all |> List.distinctBy (fst >> fst) |> List.length) <> (all |> List.distinctBy (fst >> snd) |> List.length) then
                output.Error "There is error in this algorithm"
            else
                output.Success "Algorithm seems to be ok"
        ))
        |> filter
        |> List.sortBy fst
        |> List.map (fun ((hash, crypted), count) -> [ hash; crypted; string count ])
        |> fun all -> all |> output.Table [ $"Hash ({all.Length})"; "Hash<crypted>"; "Count" ] *)

        files
        |> List.groupBy (File.hash >> Option.map Hash.value >> Option.defaultValue "-")
        |> List.map (fun (k, v) -> k, v |> List.length)
        |> filter
        |> List.sortBy fst
        |> List.map (fun (hash, count) -> [ hash; string count ])
        |> fun all -> all |> output.Table [ $"Hash ({all.Length})"; "Count" ]

        if output.IsDebug() then
            let separator =
                let separator = "<c:gray>---</c>"
                [ separator; separator; separator; separator ]

            files
            |> List.collect (fun i ->
                let firstMeta, meta =
                    let format (k, v) =
                        $"<c:yellow>{k |> MetaAttribute.value}</c>: {v}"

                    match i |> FileMetadata.load |> Result.map Map.toList with
                    | Ok [] -> "<c:red>---</c>", []
                    | Error error -> $"<c:red>{error}</c>", []
                    | Ok [ one ] -> one |> format, []
                    | Ok (first :: rest) -> first |> format, rest |> List.map format

                [
                    yield [
                        $"<c:cyan>{i.Name |> FileName.value}</c>"

                        $"<c:gray>{i.FullPath}</c>"
                        firstMeta
                    ]
                    yield! meta |> List.map (fun m -> [ ""; ""; ""; m ])
                    yield separator
                ]
            )
            |> output.Table [ "Image"; "Hash"; "Path"; "Meta" ]

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

            use loggerFactory =
                if output.IsDebug() then "vvv"
                elif output.IsVeryVerbose() then "vv"
                else "v"
                |> LogLevel.parse
                |> LoggerFactory.create "MetaStats"

            return! target |> run (input, output) loggerFactory ffmpeg
        }
        |> AsyncResult.waitAfterFinish output 2000
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
