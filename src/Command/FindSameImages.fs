namespace MF.ImageManager.Command

open System.IO
open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ErrorHandling.AsyncResult.Operators
open MF.ImageManager
open MF.ImageManager.Prepare
open MF.Utils
open MF.Utils.Progress

[<RequireQualifiedAccess>]
module FindSameImages =
    let arguments = [
        Argument.required "target" "Directory you want to check."
    ]

    let options = [
        Option.required "output" (Some "o") "Output directory, where will all same image groups be coppied to." None
        Option.required "use-path" None "A part of an image full path, which should be kept in copping an same image." None
    ]

    let private printImage output image =
        output.Message $"Image <c:cyan>{image.Name}</c>"
        output.Message $"  <c:gray>> {image.FullPath}</c>"

        if output.IsVerbose() then
            image.Metadata
            |> Map.toList
            |> List.sortBy fst
            |> List.map (fun (k, v) -> [ k |> MetaAttribute.value; v ])
            |> output.Table [ "Meta"; "Value" ]

    let private printGroup output name (group: SameImageGroup list) =
        output.SubTitle $"Group {name}"
        match group with
        | [] -> output.Message "No items in this group."
        | group ->
            group
            |> List.iter (fun (groupKey, group) ->
                output.Message $"Same image group by <c:cyan>{groupKey}</c>"
                group
                |> List.iter (printImage output)
                |> output.NewLine
            )
        output.NewLine()

    let private copySameImages output (adepts: SameImageAdepts) = function
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

            use progress = new Progress(output, "Copy adepts")
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
                        match pathPart, Path.GetDirectoryName(first.FullPath) with
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
                    let rawTarget = groupPath </> image.Name
                    let target = rawTarget |> checkTarget

                    if rawTarget <> target && output.IsVeryVerbose() then
                        output.Message $"Target already exists -> renamed to <c:dark-yellow>{target}</c>"
                        additionalMessages <- $"Renamed from {rawTarget} to {target}" :: additionalMessages

                    if output.IsDebug() then output.Message $"Copy {image.FullPath} -> {target} ..."

                    File.Copy(image.FullPath, target)
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
                                        i.Metadata
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

    let private run output ignoreWarnings cache copyTo target = asyncResult {
        let! images =
            target
            |> Finder.findAllImagesInDir output ignoreWarnings FFMpeg.empty None
            <@> List.map PrepareError.format

        if output.IsVeryVerbose() then
            output.NewLine()

            images
            |> List.sortBy Image.name
            |> List.iter (printImage output)

        output.Section "Finding same images ..."
        let! sameImages =
            images
            |> RecognizeSameImage.findSameImages output cache

        match copyTo, output.IsVerbose() with
        | (None, _), _ | _, true ->
            output.Section "Same images"
            sameImages.ByCompleteHash |> printGroup output "CompleteHash"
            sameImages.ByDateTimeOriginal |> printGroup output "DateTimeOriginal"
            sameImages.ByGps |> printGroup output "Gps"
            sameImages.ByContent |> printGroup output "Content"
        | _ -> ()

        copyTo
        |> copySameImages output sameImages

        return "Done"
    }

    let execute ((input, output): IO) =
        asyncResult {
            let target = input |> Input.getArgumentValue "target"
            let outputDir =
                match input with
                | Input.HasOption "output" o -> o |> OptionValue.stringValue
                | _ -> None
            let pathPart =
                match input with
                | Input.HasOption "use-path" o -> o |> OptionValue.stringValue
                | _ -> None

            outputDir
            |> Option.iter (fun dir ->
                if dir |> Directory.Exists |> not then
                    failwithf "Output dir %A does NOT exists." dir
            )

            let ignoreWarnings = true

            // todo - asi odstranit cache
            // todo - moznost pridat dalsi slozku (pak pustit 20XX + roztridit, ...)

            // todo - cache is not yet working, it would only use all the images not any subset by a directory or anything
            // todo - serialize/deserialize also doesn't work yet
            // let cache = FreshAndCacheResultToFile "hashCache.hshlib"
            // let cache = FromFile "hashCache.hshlib"
            let cache = NoCache

            return! target |> run output ignoreWarnings cache (outputDir, pathPart)
        }
        |> Async.RunSynchronously
        |> function
            | Ok message ->
                output.Success message
                ExitCode.Success
            | Error errors ->
                errors |> List.iter output.Error
                ExitCode.Error
