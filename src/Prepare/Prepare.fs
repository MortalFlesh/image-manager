namespace MF.ImageManager.Prepare

module Prepare =
    open System.IO
    open MF.ImageManager
    open MF.ConsoleApplication
    open MF.Utils
    open MF.Utils.Progress
    open MF.ErrorHandling

    let private copyFiles output config filesToCopy =
        let totalCount = filesToCopy |> List.length
        output.Message $"Copy files[<c:magenta>{totalCount}</c>]"

        use progress =
            let progress = new Progress(output, "\n")

            match config.TargetDirMode with
            | DryRun -> progress
            | _ ->
                progress.Start(totalCount)
                progress

        let (/) (a: obj) (b: obj) = Path.Combine(string a, string b)
        let month = sprintf "%02i"

        let targetPath (image: File) =
            match config, image |> File.createdAtDateTime with
            | { TargetSubdirFallback = None }, None
            | { TargetSubdir = Flat }, _ -> config.Target / image.Name

            | { TargetSubdir = ByMonth; TargetSubdirFallback = Some fallback }, None
            | { TargetSubdir = ByYear; TargetSubdirFallback = Some fallback }, None
            | { TargetSubdir = ByYearAndMonth; TargetSubdirFallback = Some fallback }, None -> config.Target / fallback / image.Name

            | { TargetSubdir = ByMonth }, Some createdAt -> config.Target / (month createdAt.Month) / image.Name
            | { TargetSubdir = ByYear }, Some createdAt -> config.Target / createdAt.Year / image.Name
            | { TargetSubdir = ByYearAndMonth }, Some createdAt -> config.Target / createdAt.Year / (month createdAt.Month) / image.Name

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

                progress.Advance()
        )

        match config.TargetDirMode with
        | DryRun -> output.Message "  └──> <c:green>Done</c>"
        | _ -> ()

        output.NewLine()

    let prepareForSorting output ignoreWarnings config = asyncResult {
        config.Target |> Directory.ensure

        output.NewLine()
        output.SubTitle "Find all files in source"
        let! allFilesInSource = config.Source |> Finder.findAllFilesInSource output ignoreWarnings config.Ffmpeg config.Prefix
        output.NewLine()

        output.SubTitle "Exclude files from source by excluded dirs"
        let exclude = config.Target |> Finder.findFilesAndDirsToExclude config.TargetDirMode config.Exclude config.ExcludeList
        let! excludedFiles = exclude |> Finder.findExcludedFiles output

        output.SubTitle "Copy files from source"
        let filesToCopy = allFilesInSource |> Finder.findFilesToCopy output excludedFiles

        if output.IsVeryVerbose() then
            output.Message " * All files:"
            output.List (allFilesInSource |> List.map File.name)

            output.Message " * Files to copy:"
            output.List (filesToCopy |> List.map File.name)

        filesToCopy |> copyFiles output config

        return "Done"
    }
