namespace MF.ImageManager.Command

open System.IO
open Microsoft.Extensions.Logging
open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ErrorHandling.AsyncResult.Operators
open MF.ImageManager
open MF.ImageManager.Prepare
open MF.Utils
open MF.Utils.Progress
open MF.Utils.Logging

[<RequireQualifiedAccess>]
module FindSameImages =
    let arguments = [
        Argument.required "target" "Directory you want to check."
    ]

    let options = CommonOptions.all @ [
        Option.optional "output" (Some "o") "Output directory, where will all same image groups be coppied to." None
        Option.optional "use-path" None "A part of an image full path, which should be kept in copping an same image." None
    ]

    let private printImage ((_, output) as io: IO) image =
        output.Message $"Image <c:cyan>{image.Name |> FileName.value}</c>"
        output.Message $"  <c:gray>> {image.FullPath}</c>"

        if output.IsVerbose() then
            image
            |> FileMetadata.load io
            |> function
                | Ok meta -> meta
                | Error _ -> Map.empty
            |> Map.toList
            |> List.sortBy fst
            |> List.map (fun (k, v) -> [ k |> MetaAttribute.value; v ])
            |> output.Table [ "Meta"; "Value" ]

    let private printGroup ((_, output) as io: IO) name (group: SameImageGroup list) =
        output.SubTitle $"Group {name}"
        match group with
        | [] -> output.Message "No items in this group."
        | group ->
            group
            |> List.iter (fun (groupKey, group) ->
                output.Message $"Same image group by <c:cyan>{groupKey}</c>"
                group
                |> List.iter (printImage io)
                |> output.NewLine
            )
        output.NewLine()

    let private copySameImages ((_, output as io): MF.ConsoleApplication.IO) (adepts: SameImageAdepts) = function
        | None, _ -> ()
        | Some outputDir, (pathPart: string option) ->
            let allAdepts = [
                adepts.ByCompleteHash
                adepts.ByDateTimeOriginal
                adepts.ByGps
                adepts.ByContent
            ]
            let totalAdepts = allAdepts |> List.sumBy List.length
            output.Message $"Copy same images [<c:magenta>{totalAdepts}</c>] to <c:yellow>{outputDir}</c>..."

            let (</>) a b = Path.Combine(a, b)

            use progress = new Progress(io, "Copy adepts")
            progress.Start(totalAdepts)

            let rec checkTarget target =
                if target |> File.Exists
                then
                    let name = target |> Path.GetFileName
                    let path = target |> Path.GetDirectoryName
                    path </> "_" + name |> checkTarget
                else target

            allAdepts
            |> List.concat
            |> List.iter (fun (group, items) ->
                let groupId = (System.Guid.NewGuid()).ToString()
                let first = items |> List.head

                let groupPath =
                    let dirPath =
                        match pathPart, first.FullPath.GetDirectoryName() with
                        | Some part, path when path.Contains part ->
                            match path.Split part |> List.ofSeq with
                            | [] -> path
                            | remove :: _ -> path.Replace(remove, "")
                        | _, path -> path

                    outputDir </> dirPath </> groupId

                groupPath |> Directory.ensure
                if output.IsVerbose() then
                    output.Message $"Copy group to <c:yellow>{groupPath}</c> ..."

                let mutable additionalMessages = []

                items
                |> List.iter (fun image ->
                    let (FullPath sourcePath) = image.FullPath
                    let rawTarget = groupPath </> (image.Name |> FileName.value)
                    let target = rawTarget |> checkTarget

                    if rawTarget <> target && output.IsVeryVerbose() then
                        output.Message $"Target already exists -> renamed to <c:dark-yellow>{target}</c>"
                        additionalMessages <- $"Renamed from {rawTarget} to {target}" :: additionalMessages

                    if output.IsDebug() then output.Message $"Copy {sourcePath} -> {target} ..."

                    File.Copy(sourcePath, target)
                    progress.Advance()
                )

                File.WriteAllLines(
                    groupPath </> "Readme.md",
                    [
                        group
                        "=" |> String.replicate group.Length
                        ""
                        yield!
                            items
                            |> List.collect (fun i ->
                                [
                                    $" - {i.Name}"
                                    $"     - {i.FullPath}"
                                    yield!
                                        i
                                        |> FileMetadata.load io
                                        |> function
                                            | Ok meta -> meta
                                            | Error _ -> Map.empty
                                        |> Map.toList
                                        |> List.map (fun (k, v) -> $"     - {k |> MetaAttribute.value}: {v}")
                                ]
                            )

                        match additionalMessages with
                        | [] -> ()
                        | messages ->
                            ""
                            "Additional messages"
                            "-------------------"
                            ""
                            yield! messages |> List.rev
                    ]
                )
            )

    let private run ((_, output as io): IO) loggerFactory copyTo target = asyncResult {
        let! images =
            target
            |> Finder.findAllFilesInDir io loggerFactory FFMpeg.empty
            <@> List.map PrepareError.format

        if output.IsVeryVerbose() then
            output.NewLine()

            images
            |> List.sortBy File.name
            |> List.iter (printImage io)

        output.Section "Finding same images ..."
        let! sameImages =
            images
            |> RecognizeSameImage.findSameImages io

        match copyTo, output.IsVerbose() with
        | (None, _), _ | _, true ->
            output.Section "Same images"
            sameImages.ByCompleteHash |> printGroup io "CompleteHash"
            sameImages.ByDateTimeOriginal |> printGroup io "DateTimeOriginal"
            sameImages.ByGps |> printGroup io "Gps"
            sameImages.ByContent |> printGroup io "Content"
        | _ -> ()

        copyTo
        |> copySameImages io sameImages

        return "Done"
    }

    let execute = ExecuteAsyncResult <| fun ((input, output): IO) ->
        asyncResult {
            use loggerFactory =
                if output.IsDebug() then "vvv"
                elif output.IsVeryVerbose() then "vv"
                else "v"
                |> LogLevel.parse
                |> LoggerFactory.create "findSameImages"

            let target = input |> Input.Argument.value "target"
            let outputDir =
                match input with
                | Input.Option.Has "output" o -> o |> OptionValue.stringValue
                | _ -> None
            let pathPart =
                match input with
                | Input.Option.Has "use-path" o -> o |> OptionValue.stringValue
                | _ -> None

            outputDir
            |> Option.iter (fun dir ->
                if dir |> Directory.Exists |> not then
                    failwithf "Output dir %A does NOT exists." dir
            )

            // todo - moznost pridat dalsi slozku (pak pustit 20XX + roztridit, ...)

            let! message = target |> run (input, output) loggerFactory (outputDir, pathPart)

            output.Success message

            return ExitCode.Success
        }
        |> AsyncResult.waitAfterFinish output 2000
        |> AsyncResult.mapError (Errors.map "Find Same Images Command" output id)
