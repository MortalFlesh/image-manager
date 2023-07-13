namespace MF.ImageManager.Command

open Microsoft.Extensions.Logging
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

    let options = CommonOptions.all

    let private run ((_, output as io): MF.ConsoleApplication.IO) loggerFactory ffmpeg target = asyncResult {
        let! files =
            target
            |> Finder.findAllFilesInDir io loggerFactory ffmpeg

        (* //! count by model
        files
        |> List.groupBy File.model
        |> List.map (fun (k, v) -> k, v |> List.length)
        |> List.sortBy snd
        |> List.map (fun (model, count) -> [ model |> Option.defaultValue "-"; string count ])
        |> output.Table [ "Model"; "Count" ] *)

        let filter items =
            if output.IsDebug() then items
            else items |> List.filter (snd >> (fun x -> x > 1))

        (* //! compute hash
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

        if output.IsVeryVerbose() then
            let separator =
                let separator = "<c:gray>---</c>"
                [ separator; separator; separator; separator ]

            files
            |> List.collect (fun i ->
                let metadata = i |> FileMetadata.load io

                let firstMeta, meta =
                    let format (k, v) =
                        $"<c:yellow>{k |> MetaAttribute.value}</c>: {v}"

                    match metadata |> Result.map Map.toList with
                    | Ok [] -> "<c:red>---</c>", []
                    | Error error -> $"<c:red>{error}</c>", []
                    | Ok [ one ] -> one |> format, []
                    | Ok (first :: rest) -> first |> format, rest |> List.map format

                let currentHash = i.Name |> FileName.hash
                let correctHash = metadata |> Result.map (Hash.calculate i.Type) |> Result.toOption |> Option.map Hash.value

                [
                    yield [
                        $"<c:cyan>{i.Name |> FileName.value}</c>"

                        match currentHash, correctHash with
                        | Some current, Some correct when current = correct -> $"<c:cyan>{current}</c> (<c:green>OK</c>)"
                        | Some current, Some correct when current <> correct -> $"<c:cyan>{current}</c> <> <c:cyan>{correct}</c> (<c:orange>Is different</c>)"
                        | Some current, None -> $"<c:cyan>{current}</c> (<c:orange>Can not be recalculated</c>)"
                        | None, Some correct -> $"<c:cyan>{correct}</c> (<c:orange>Should be hashed</c>)"
                        | _ -> $"- (<c:red>There is no hash and can not be calculated</c>)"

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

    let execute = ExecuteAsyncResult <| fun ((input, output): IO) ->
        asyncResult {
            use loggerFactory =
                if output.IsDebug() then "vvv"
                elif output.IsVeryVerbose() then "vv"
                else "v"
                |> LogLevel.parse
                |> LoggerFactory.create "MetaStats"

            let target = input |> Input.Argument.value "target"

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
        |> AsyncResult.mapError (Errors.map "Meta Stats Command" output PrepareError.format)
