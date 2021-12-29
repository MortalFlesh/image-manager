namespace MF.ImageManager.Command

open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ImageManager
open MF.Utils.Utils

[<RequireQualifiedAccess>]
module MetaStatsCommand =
    let arguments = [
        Argument.required "target" "Directory you want to check."
    ]

    let options = [
        Option.optional "ffmpeg" None "FFMpeg path in the current dir" None
    ]

    let private run output ignoreWarnings ffmpeg target = asyncResult {
        let! files =
            target
            |> Finder.findAllFilesInDir output ignoreWarnings ffmpeg None

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

                    match i.Metadata |> Map.toList with
                    | [] -> "<c:red>---</c>", []
                    | [ one ] -> one |> format, []
                    | first :: rest -> first |> format, rest |> List.map format

                [
                    yield [
                        $"<c:cyan>{i.Name}</c>"

                        match i.Hash with
                        | Some (Hash hash) -> $"[<c:magenta>{hash.Length}</c>] <c:dark-yellow>{hash}</c>"
                        | _ -> $"<c:red>---</c>"

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
                    | PrepareError.NotImageOrVideo path -> output.Error $"File {path} is not an image or a video."
                )

                ExitCode.Error
