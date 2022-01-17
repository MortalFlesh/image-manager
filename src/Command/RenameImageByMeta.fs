namespace MF.ImageManager.Command

open System
open System.IO
open Microsoft.Extensions.Logging
open FSharp.Data
open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ImageManager
open MF.Utils
open MF.Utils.Progress
open MF.Utils.Logging

[<RequireQualifiedAccess>]
module RenameImageByMeta =
    type private ConfigSchema = JsonProvider<".dist.rename.json">

    type Config = {
        Rename: RenameDefinition list
    }

    and RenameDefinition = {
        Prefix: string
        Conditions: MetaCondition list
    }

    and MetaCondition =
        | IsModel of string

        with
            /// Checks whether the metadata map contains a value which meets the condition
            member this.Meets(metadata: Map<MetaAttribute, string>) =
                match this with
                | IsModel model -> metadata |> Map.tryFind Model = Some model

    type ExecuteMode =
        | DryRun
        | Execute

    [<RequireQualifiedAccess>]
    module private Config =
        let parse path =
            let config =
                path
                |> File.ReadAllText
                |> ConfigSchema.Parse

            {
                Rename =
                    config
                    |> Seq.choose (fun item ->
                        let conditions =
                            item.Meta
                            |> Seq.choose (fun meta ->
                                match meta.Model with
                                | null | "" -> None
                                | model -> Some (IsModel model)
                            )
                            |> Seq.toList

                        match conditions with
                        | [] -> None
                        | conditions ->
                            Some {
                                Prefix = item.Prefix
                                Conditions = conditions
                            }
                    )
                    |> Seq.toList
            }

    let arguments = [
        Argument.required "config" "Path to configuration file."
        Argument.required "target" "Directory with images."
    ]

    let options = [
        Option.noValue "dry-run" None "If set, target directory will NOT be touched in anyway and images will only be sent to stdout."
    ]

    [<RequireQualifiedAccess>]
    type RenameFile =
        | Original of File
        | Renamed of File

        with
            member this.Name =
                match this with
                | Original i
                | Renamed i -> i.Name

    [<RequireQualifiedAccess>]
    module private File =
        let (|IsMatching|_|) (condition: MetaCondition) (image: File) =
            match image with
            | { Metadata = metadata } when condition.Meets metadata -> Some IsMatching
            | _ -> None

        let isMathing condition = function
            | IsMatching condition -> true
            | _ -> false

        let addPrefix prefix (image: File) =
            let prefixedName = $"{prefix}{image.Name}"

            { image
                with
                    Name = prefixedName
                    FullPath = image.FullPath.Replace(image.Name, prefixedName)
            }

        let replace (files: RenameFile * RenameFile) = asyncResult {
            match files with
            | RenameFile.Original oldImage, RenameFile.Renamed newImage
            | RenameFile.Renamed newImage, RenameFile.Original oldImage
                -> return File.Move(oldImage.FullPath, newImage.FullPath, false)
            | _ -> return! AsyncResult.ofError $"You must replace old image with a new one. You sent {files}."
        }

    let private run output loggerFactory (config: Config) executeMode target = asyncResult {
        let! images =
            target
            |> Finder.findAllFilesInDir output loggerFactory FFMpeg.empty None

        output.NewLine()

        if output.IsVeryVerbose() then
            images
            |> List.groupBy File.model
            |> List.map (fun (k, v) -> k, v |> List.length)
            |> List.sortBy snd
            |> List.map (fun (model, count) -> [ model |> Option.defaultValue "-"; string count ])
            |> output.Table [ "Model"; "Count" ]
            |> output.NewLine

        let prefixByModelTable =
            config.Rename
            |> List.collect (fun rename ->
                rename.Conditions
                |> List.map (function
                    | IsModel model -> model, rename.Prefix
                )
            )
            |> Map.ofList

        output.Message $"Renaming images in <c:cyan>{target}</c> by metadata"

        let renameImagesProgress = new Progress(output, "Rename images")

        let renames =
            let logger = loggerFactory.CreateLogger("Rename")
            use prepareRenamesProgress = new Progress(output, "Prepare renames")

            images
            |> tee (List.length >> sprintf "  ├──> Choosing images[<c:magenta>%i</c>] to rename ..." >> output.Message)
            |> tee (List.length >> prepareRenamesProgress.Start)
            |> List.choose (fun image ->
                maybe {
                    let! imageTakenBy = image |> File.model
                    let! prefix = prefixByModelTable |> Map.tryFind imageTakenBy

                    if image.Name.StartsWith prefix then
                        // skip images, which already has a prefix
                        return! None

                    return
                        asyncResult {
                            let renamedImage =
                                image
                                |> File.addPrefix prefix
                                |> RenameFile.Renamed

                            match executeMode with
                            | DryRun ->
                                output.Message $"  ├──> Rename image <c:cyan>{image.Name}</c> to <c:yellow>{renamedImage.Name}</c>"
                            | Execute ->
                                do!
                                    (RenameFile.Original image, renamedImage)
                                    |> File.replace

                            return prefix, imageTakenBy
                        }
                        |> AsyncResult.mapError (fun e -> prefix, imageTakenBy, e)
                        |> AsyncResult.tee (ignore >> renameImagesProgress.Advance)
                        |> AsyncResult.teeError ((fun e -> logger.LogError("Rename image {image} failed with {error}", e)) >> renameImagesProgress.Advance)
                }
                |> Option.teeNone (fun _ -> if output.IsDebug() then output.Message $"  ├──> Renaming image <c:cyan>{image.Name}</c> is <c:dark-yellow>skipped</c>.")
                |> tee (ignore >> prepareRenamesProgress.Advance)
            )

        let handle =
            if output.IsDebug()
            then Async.Sequential
            else Async.Parallel

        let results =
            renames
            |> tee (List.length >> sprintf "  ├──> Renaming images[<c:magenta>%i</c>] <c:yellow>in parallel</c> ..." >> output.Message)
            |> tee (List.length >> renameImagesProgress.Start)
            |> handle
            |> Async.RunSynchronously
            |> tee (ignore >> renameImagesProgress.Finish)
            |> tee (Seq.length >> sprintf "  └──> Renaming images[<c:magenta>%i</c>] finished." >> output.Message)

        results
        |> Seq.choose (function
            | Ok renamed -> Some renamed
            | Error _ -> None
        )
        |> Seq.countBy id
        |> Seq.map (fun ((prefix, takenBy), count) -> [ prefix; takenBy; string count ])
        |> Seq.toList
        |> output.Table [ "Prefix"; "Model"; "Count" ]

        let errors =
            results
            |> Seq.choose (function
                | Ok _ -> None
                | Error error -> Some error
            )
            |> Seq.toList

        match errors with
        | [] -> ()
        | errors ->
            errors
            |> List.countBy (fun (prefix, model, _) -> prefix, model)
            |> List.map (fun ((prefix, takenBy), count) -> [ prefix; takenBy; string count ])
            |> output.Table [ "Prefix"; "Model"; "Error" ]
            |> output.NewLine

            errors
            |> List.map (fun (prefix, takenBy, error) -> $"{prefix} ({takenBy}): {error}\n")
            |> Errors.show output

        return "Done"
    }

    let execute ((input, output): IO) =
        asyncResult {
            let config =
                input
                |> Input.getArgumentValue "config"
                |> Config.parse

            let executeMode =
                match input with
                | Input.IsSetOption "dry-run" _ -> DryRun
                | _ -> Execute

            let target = input |> Input.getArgumentValue "target"

            use loggerFactory =
                if output.IsDebug() then "vvv"
                elif output.IsVeryVerbose() then "vv"
                else "v"
                |> LogLevel.parse
                |> LoggerFactory.create "RenameByMeta"

            return! target |> run output loggerFactory config executeMode
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
