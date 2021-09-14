namespace MF.ImageManager.Prepare

open System

module Prepare =
    open System.IO
    open System.Collections.Generic
    open MetadataExtractor
    open MF.ConsoleApplication
    open MF.Utils
    open MF.Utils
    open MF.ErrorHandling

    type private FileOrDir =
        | File of string
        | Dir of string

    let private createImage output ffmpeg prefix file = asyncResult {
        let! dateTimeOriginal =
            file
            |> MetaData.dateTimeOriginal output ffmpeg

        return {
            Name =
                let originalName = file |> Path.GetFileName

                match prefix with
                | Some (Prefix prefix) -> prefix + originalName
                | _ -> originalName

            FullPath = file |> Path.GetFullPath
            CreatedAt = dateTimeOriginal
        }
    }

    let private findAllImagesInDir output ffmpeg prefix dir = asyncResult {
        output.Message $"Searching all images in <c:cyan>{dir}</c>"

        let! files =
            dir
            |> FileSystem.getAllFilesAsync output FileSystem.SearchFiles.IgnoreDotFiles
            |> AsyncResult.ofAsyncCatch (PrepareError.Exception >> List.singleton)

        return!
            files
            |> tee (List.length >> sprintf "  ├──> found <c:magenta>%i</c> files, <c:yellow>parallely checking metadata ...</c>" >> output.Message)
            |> List.map (createImage output ffmpeg prefix)
            |> AsyncResult.ofParallelAsyncResults PrepareError.Exception
            |> AsyncResult.tee (List.length >> sprintf "  └──> found <c:magenta>%i</c> images with metadata" >> output.Message)
    }

    let private findAllImagesInSource output config =
        config.Source
        |> List.distinct
        |> List.map (findAllImagesInDir output config.Ffmpeg config.Prefix)
        |> AsyncResult.ofSequentialAsyncResults (PrepareError.Exception >> List.singleton)
        |> AsyncResult.map (List.concat >> List.distinctBy Image.name)
        |> AsyncResult.mapError List.concat

    let private findFilesAndDirsToExclude config =
        let excludeDirs =
            match config.TargetDirMode, config.Exclude with
            | Override, Some excludeDirs -> Some excludeDirs
            | Exclude, Some excludeDirs -> Some (config.Target :: excludeDirs)
            | Exclude, None -> Some [ config.Target ]
            | _ -> None

        match config.ExcludeList with
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

    let private findExcludedFiles output (excludeFiles, excludeDirs) = asyncResult {
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

    let private findFilesToCopy output excludedFiles allImagesInSource =
        output.Message $"Searching all files to copy"
        output.Message <| sprintf "  ├──> From <c:magenta>%i</c> files from source" (allImagesInSource |> List.length)
        output.Message <| sprintf "  ├──> Exclude <c:red>%i</c> files" (excludedFiles |> List.length)
        if output.IsDebug() then output.List excludedFiles

        allImagesInSource
        |> List.filter (Image.name >> File.notIn excludedFiles)
        |> tee (List.length >> sprintf "  └──> There are <c:green>%i</c> files to copy" >> output.Message >> output.NewLine)

    let private copyFiles output config filesToCopy =
        let totalCount = filesToCopy |> List.length
        output.Message $"Copy files[<c:magenta>{totalCount}</c>]"

        let progress =
            match config.TargetDirMode with
            | DryRun -> None
            | _ -> totalCount |> output.ProgressStart "\n" |> Some

        let (/) (a: obj) (b: obj) = Path.Combine(string a, string b)
        let month = sprintf "%02i"

        let targetPath (image: Image) =
            match config, image with
            | { TargetSubdirFallback = None }, { CreatedAt = None }
            | { TargetSubdir = Flat }, _ -> config.Target / image.Name

            | { TargetSubdir = ByMonth; TargetSubdirFallback = Some fallback }, { CreatedAt = None }
            | { TargetSubdir = ByYear; TargetSubdirFallback = Some fallback }, { CreatedAt = None }
            | { TargetSubdir = ByYearAndMonth; TargetSubdirFallback = Some fallback }, { CreatedAt = None } -> config.Target / fallback / image.Name

            | { TargetSubdir = ByMonth }, { CreatedAt = Some createdAt } -> config.Target / (month createdAt.Month) / image.Name
            | { TargetSubdir = ByYear }, { CreatedAt = Some createdAt } -> config.Target / createdAt.Year / image.Name
            | { TargetSubdir = ByYearAndMonth }, { CreatedAt = Some createdAt } -> config.Target / createdAt.Year / (month createdAt.Month) / image.Name

        filesToCopy
        |> List.iter (fun image ->
            let targetPath = image |> targetPath

            match config.TargetDirMode with
            | DryRun ->
                output.Message <| sprintf "  ├── <c:cyan>%s</c> -> <c:green>%s</c>" image.FullPath targetPath
            | _ ->
                targetPath
                |> Path.GetDirectoryName
                |> Directory.ensure

                (image.FullPath, targetPath)
                |> FileSystem.copy

                progress |> Option.iter output.ProgressAdvance
        )

        match config.TargetDirMode with
        | DryRun -> output.Message "  └──> <c:green>Done</c>"
        | _ -> progress |> Option.iter output.ProgressFinish

        output.NewLine()

    let prepareForSorting output config = asyncResult {
        config.Target |> Directory.ensure

        output.NewLine()
        output.SubTitle "Find all images in source"
        let! allImagesInSource = config |> findAllImagesInSource output
        output.NewLine()

        output.SubTitle "Exclude images from source by excluded dirs"
        let exclude = config |> findFilesAndDirsToExclude
        let! excludedFiles = exclude |> findExcludedFiles output

        output.SubTitle "Copy images from source"
        let filesToCopy = allImagesInSource |> findFilesToCopy output excludedFiles

        if output.IsVeryVerbose() then
            output.Message " * All images:"
            output.List (allImagesInSource |> List.map Image.name)

            output.Message " * Files to copy:"
            output.List (filesToCopy |> List.map Image.name)

        filesToCopy |> copyFiles output config

        return "Done"
    }
