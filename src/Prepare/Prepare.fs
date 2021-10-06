namespace MF.ImageManager.Prepare

open System

module Prepare =
    open System.IO
    open System.Collections.Generic
    open MetadataExtractor
    open MF.ImageManager
    open MF.ConsoleApplication
    open MF.Utils
    open MF.ErrorHandling

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

    let prepareForSorting output ignoreWarnings config = asyncResult {
        config.Target |> Directory.ensure

        output.NewLine()
        output.SubTitle "Find all images in source"
        let! allImagesInSource = config.Source |> Finder.findAllImagesInSource output ignoreWarnings config.Ffmpeg config.Prefix
        output.NewLine()

        output.SubTitle "Exclude images from source by excluded dirs"
        let exclude = config.Target |> Finder.findFilesAndDirsToExclude config.TargetDirMode config.Exclude config.ExcludeList
        let! excludedFiles = exclude |> Finder.findExcludedFiles output

        output.SubTitle "Copy images from source"
        let filesToCopy = allImagesInSource |> Finder.findFilesToCopy output excludedFiles

        if output.IsVeryVerbose() then
            output.Message " * All images:"
            output.List (allImagesInSource |> List.map Image.name)

            output.Message " * Files to copy:"
            output.List (filesToCopy |> List.map Image.name)

        filesToCopy |> copyFiles output config

        return "Done"
    }
