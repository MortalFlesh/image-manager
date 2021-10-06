namespace MF.ImageManager

[<RequireQualifiedAccess>]
module Finder =
    open System
    open System.IO
    open MF.ConsoleApplication
    open MF.Utils
    open MF.ErrorHandling

    let createImage output ignoreWarnings ffmpeg prefix file = asyncResult {
        // todo - vycistit MetaData.fs - dotahnout, aby se to parsovalo v novem modulu a dateTimeOriginal byl az z parsovanych metadat
        // - parsovat i video

        let! (dateTimeOriginal, metadata) =
            file
            |> MetaData.find output ignoreWarnings ffmpeg

        return {
            Name =
                let originalName = file |> Path.GetFileName

                match prefix with
                | Some (Prefix prefix) -> prefix + originalName
                | _ -> originalName

            FullPath = file |> Path.GetFullPath
            CreatedAt = dateTimeOriginal
            Metadata = metadata
        }
    }

    let findAllImagesInDir output ignoreWarnings ffmpeg prefix dir = asyncResult {
        output.Message $"Searching all images in <c:cyan>{dir}</c>"

        let! files =
            dir
            |> FileSystem.getAllFilesAsync output FileSystem.SearchFiles.IgnoreDotFiles
            |> AsyncResult.ofAsyncCatch (PrepareError.Exception >> List.singleton)

        let progress = output.ProgressStart "Check metadata" files.Length

        let createImages =
            files
            |> tee (List.length >> sprintf "  ├──> found <c:magenta>%i</c> files, <c:yellow>parallely checking metadata ...</c>" >> output.Message)
            |> List.map (createImage output ignoreWarnings ffmpeg prefix >> AsyncResult.tee (fun _ -> progress |> output.ProgressAdvance))

        let! images =
            createImages
            |> AsyncResult.ofParallelAsyncResults PrepareError.Exception
            |> AsyncResult.tee (List.length >> sprintf "  └──> found <c:magenta>%i</c> images with metadata" >> output.Message)

        progress |> output.ProgressFinish

        return images
    }

    let findAllImagesInSource output ignoreWarnings ffmpeg prefix source =
        source
        |> List.distinct
        |> List.map (findAllImagesInDir output ignoreWarnings ffmpeg prefix)
        |> AsyncResult.ofSequentialAsyncResults (PrepareError.Exception >> List.singleton)
        |> AsyncResult.map (List.concat >> List.distinctBy Image.name)
        |> AsyncResult.mapError List.concat

    let findFilesAndDirsToExclude targetDirMode exclude excludeList target =
        let excludeDirs =
            match targetDirMode, exclude with
            | Override, Some excludeDirs -> Some excludeDirs
            | Exclude, Some excludeDirs -> Some (target :: excludeDirs)
            | Exclude, None -> Some [ target ]
            | _ -> None

        match excludeList with
        | Some excludeList ->
            let excludeDirs =
                match excludeDirs with
                | Some excludeDirs -> excludeDirs
                | _ -> []

            let excludeFiles, excludeDirs =
                excludeList
                |> File.ReadAllLines
                |> Seq.fold (fun (excludeFiles, excludeDirs) line ->
                    if line |> File.Exists || line |> Directory.Exists
                        then
                            let attr = line |> File.GetAttributes

                            if attr.HasFlag(FileAttributes.Directory)
                                then excludeFiles, line :: excludeDirs
                                else line :: excludeFiles, excludeDirs
                        else
                            line :: excludeFiles, excludeDirs
                ) ([], excludeDirs)

            excludeFiles, (if excludeDirs |> List.isEmpty then None else Some excludeDirs)
        | None -> [], excludeDirs

    let findExcludedFiles output (excludeFiles, excludeDirs) = asyncResult {
        output.Message "Searching all images to exclude"

        let! excludeFiles =
            match excludeDirs with
            | Some excludeDirs ->
                asyncResult {
                    let excludeDirs =
                        excludeDirs
                        |> List.distinct
                        |> tee (List.length >> sprintf "  ├──> Exclude dirs[<c:red>%i</c>]" >> output.Message)
                        |> tee (List.iter (sprintf "  │      - <c:red>%s</c>" >> output.Message))

                    let! files =
                        excludeDirs
                        |> List.map (FileSystem.getAllFilesAsync output FileSystem.SearchFiles.IgnoreDotFiles)
                        |> AsyncResult.ofSequentialAsyncs PrepareError.Exception

                    return
                        files
                        |> List.concat
                        |> List.map Path.GetFileName
                        |> (@) excludeFiles
                }
            | None ->
                excludeFiles |> AsyncResult.ofSuccess

        return
            excludeFiles
            |> List.distinct
            |> tee (List.length >> sprintf "  └──> Exclude <c:magenta>%i</c> files" >> output.Message >> output.NewLine)
    }

    let findFilesToCopy output excludedFiles allImagesInSource =
        output.Message $"Searching all files to copy"
        output.Message <| sprintf "  ├──> From <c:magenta>%i</c> files from source" (allImagesInSource |> List.length)
        output.Message <| sprintf "  ├──> Exclude <c:red>%i</c> files" (excludedFiles |> List.length)
        if output.IsDebug() then output.List excludedFiles

        allImagesInSource
        |> List.filter (Image.name >> File.notIn excludedFiles)
        |> tee (List.length >> sprintf "  └──> There are <c:green>%i</c> files to copy" >> output.Message >> output.NewLine)
