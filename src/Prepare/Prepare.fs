namespace MF.ImageManager.Prepare

module Prepare =
    open System
    open System.IO
    open Microsoft.Extensions.Logging
    open MF.ImageManager
    open MF.ConsoleApplication
    open MF.Utils
    open MF.Utils.Progress
    open MF.ErrorHandling
    open MF.ErrorHandling.AsyncResult.Operators

    [<RequireQualifiedAccess>]
    module private File =
        type private Source = Source of File
        type private Target = Target of File

        let private targetPath output config (Source sourceFile) (Target targetFile): AsyncResult<FullPath, exn> = asyncResult {
            let (/) (a: string) (b: obj) = Path.Combine(a, string b)
            let fileName = targetFile.Name |> FileName.value

            match config with
            | { TargetSubdir = Flat } -> return FullPath (config.Target / fileName)
            | _ ->
                let! createdAt = sourceFile |> File.createdAtDateTimeAsync output

                let month = sprintf "%02i"

                let targetPath =
                    match config, createdAt with
                    | { TargetSubdirFallback = None }, None
                    | { TargetSubdir = Flat }, _ -> config.Target / fileName

                    | { TargetSubdir = ByMonth; TargetSubdirFallback = Some fallback }, None
                    | { TargetSubdir = ByYear; TargetSubdirFallback = Some fallback }, None
                    | { TargetSubdir = ByYearAndMonth; TargetSubdirFallback = Some fallback }, None -> config.Target / fallback / fileName

                    | { TargetSubdir = ByMonth }, Some createdAt -> config.Target / (month createdAt.Month) / fileName
                    | { TargetSubdir = ByYear }, Some createdAt -> config.Target / createdAt.Year / fileName
                    | { TargetSubdir = ByYearAndMonth }, Some createdAt -> config.Target / createdAt.Year / (month createdAt.Month) / fileName

                return FullPath targetPath
        }

        let private createFileToCopy output config sourceFile targetFile = asyncResult {
            let! targetPath =
                targetFile
                |> targetPath output config sourceFile <@> RuntimeError

            let (Source sourceFile) = sourceFile
            let (Target targetFile) = targetFile

            return {
                Source = sourceFile
                Target = { targetFile with FullPath = targetPath }
            }
        }

        let useHashedFile (logger: ILogger) output config: File -> AsyncResult<FileToCopy, PrepareFilesError> = function
            | { Name = Hashed _ } as alreadyHashedFile -> Target alreadyHashedFile |> createFileToCopy output config (Source alreadyHashedFile)

            | { Name = Normal name; Type = fileType } as normalSourceFile ->
                asyncResult {
                    let! (metadata: Map<MetaAttribute, string>) =
                        normalSourceFile
                        |> FileMetadata.loadAsync output
                        >>- (fun e ->
                            logger.LogWarning("There is a problem with loading metadata for file {file}: {error}", normalSourceFile, e)
                            AsyncResult.ofSuccess Map.empty
                        )

                    if metadata.IsEmpty then
                        return! Target normalSourceFile |> createFileToCopy output config (Source normalSourceFile)

                    else
                        let hash = Hash.calculate fileType metadata
                        let extension = name |> Extension.correctFromPath output metadata
                        let hashName = sprintf "%s%s" (hash |> Hash.value) (extension |> Extension.value)

                        let hashedFile = Target {
                            normalSourceFile
                                with
                                    Name = Hashed (hash, extension)
                                    FullPath = normalSourceFile.FullPath.Replace(name, hashName)
                        }

                        return! hashedFile |> createFileToCopy output config (Source normalSourceFile)
                }

    let private copyFiles ((_, output as io): MF.ConsoleApplication.IO) config filesToCopy = asyncResult {
        let totalCount = filesToCopy |> List.length
        output.Message $"Copy files[<c:magenta>{totalCount}</c>]"

        let progress =
            let progress = new Progress(io, "Copy files")

            match config.TargetDirMode with
            | DryRun -> progress
            | _ ->
                progress.Start(totalCount)
                progress

        filesToCopy
        |> List.iter (fun fileToCopy ->
            let sourcePath = fileToCopy.Source.FullPath.Value
            let targetPath = fileToCopy.Target.FullPath.Value

            match config.TargetDirMode with
            | DryRun ->
                output.Message <| sprintf "  ├── <c:cyan>%s</c> -> <c:green>%s</c>" sourcePath targetPath
            | _ ->
                targetPath
                |> Path.GetDirectoryName
                |> Directory.ensure

                (sourcePath, targetPath)
                |> FileSystem.copy

                progress.Advance()
        )

        progress.Finish()

        match config.TargetDirMode with
        | DryRun -> output.Message "  └──> <c:green>Done</c>"
        | _ -> ()

        output.NewLine()
    }

    type private SubTaskDependencies = {
        Config: Config
        IO: IO
        LoggerFactory: ILoggerFactory
        Logger: ILogger
    }

    type private SubTask<'Success> = SubTaskDependencies -> AsyncResult<'Success, PrepareFilesError list>

    [<RequireQualifiedAccess>]
    module private SubTask =
        let findFilesInSource: SubTask<File list> = fun { Config = config; IO = (_, output) as io; LoggerFactory = loggerFactory } -> asyncResult {
            output.NewLine()
            output.SubTitle "Find all files in source"

            let! (allFilePathsInSource: File list) =
                config.Source
                |> Finder.findAllFilesInSource io loggerFactory config.Ffmpeg <@> (List.map PrepareError)

            output.NewLine()

            return allFilePathsInSource
        }

        let filterRelevantSourceFiles (allSourceFiles: File list): SubTask<File list> = fun { Config = config; IO = (_, output) as io; LoggerFactory = loggerFactory } -> asyncResult {
            output.SubTitle $"Filter relevant source files [{allSourceFiles.Length}]"

            let! (relevantSourceFiles: File list) =
                match config.OnlyMonth with
                | Some { Year = year; Month = month } ->
                    asyncResult {
                        let filter (file: File) = async {
                            match file.Name with
                            | Hashed (hash, _) when hash |> Hash.tryGetCreated = Some (year, month) -> return Some file
                            | Hashed _ -> return None
                            | Normal _ when file.FullPath |> Hash.Cache.tryFind |> Option.bind Hash.tryGetCreated = (Some (year, month)) ->
                                if output.IsDebug() then output.Success $"Using cached hash for file {file.Name}"
                                return Some file
                            | _ ->
                            match! file |> File.createdAtDateTimeAsync output with
                            | Some createdAt when createdAt.Year = year && createdAt.Month = month -> return Some file
                            | _ -> return None
                        }

                        use progress = new Progress(io, "Filter relevant files")

                        let! relevantFiles =
                            allSourceFiles
                            |> List.map (filter >> Async.tee (ignore >> progress.Advance))
                            |> tee (List.length >> progress.Start)
                            |> AsyncResult.handleMultipleAsyncs output RuntimeError

                        let relevantFiles = relevantFiles |> List.choose id

                        return relevantFiles
                    }
                | _ -> allSourceFiles |> AsyncResult.ofSuccess

            output.Message $"  └──> Filtered <c:magenta>{relevantSourceFiles.Length}</c> files"
            output.NewLine()

            return relevantSourceFiles
        }

        let useHashForFilesInSource (filesInSource: File list): SubTask<FileToCopy list> = fun { Config = config; IO = (_, output) as io; Logger = logger } -> asyncResult {
            output.SubTitle $"Use hash for files in source [{filesInSource.Length}]"
            let useHashProgress = new Progress(io, "Use hash for files")

            let! hashedFilesInSource =
                filesInSource
                |> List.map (
                    File.useHashedFile logger output config
                    >> Async.tee (ignore >> useHashProgress.Advance)
                )
                |> tee (List.length >> useHashProgress.Start)
                |> AsyncResult.handleMultipleResults output RuntimeError
                |> Async.tee (ignore >> useHashProgress.Finish)

            hashedFilesInSource |> List.length |> sprintf "  └──> hashed <c:magenta>%i</c> files" |> output.Message |> output.NewLine

            return hashedFilesInSource
        }

        let exludeFiles: SubTask<string list> = fun { Config = config; IO = (_, output) as io; LoggerFactory = loggerFactory } -> asyncResult {
            output.SubTitle "Exclude files from source by excluded dirs"
            let exclude = config.Target |> Finder.findFilesAndDirsToExclude config.TargetDirMode config.Exclude config.ExcludeList

            return!
                exclude
                |> Finder.findExcludedFiles io <@> (List.map PrepareError)
        }

        let copyFromFiles hashedFilesInSource excludedFiles: SubTask<unit> = fun { Config = config; IO = (_, output) as io; LoggerFactory = loggerFactory } -> asyncResult {
            output.SubTitle "Copy files from source"
            let filesToCopy = hashedFilesInSource |> Finder.findFilesToCopy output excludedFiles

            if output.IsDebug() then
                output.Message " * All files:"
                output.List (filesToCopy |> List.map (FileToCopy.source >> File.name >> FileName.value))

                output.Message " * Files to copy:"
                output.List (filesToCopy |> List.map (FileToCopy.target >> File.name >> FileName.value))

            do! filesToCopy |> copyFiles io config <@> (List.map PrepareError)
        }

    let prepareForSorting (io: MF.ConsoleApplication.IO) (loggerFactory: ILoggerFactory) (config: Config): AsyncResult<_, PrepareFilesError list> = asyncResult {
        let dependencies = {
            Config = config
            IO = io
            LoggerFactory = loggerFactory
            Logger = loggerFactory.CreateLogger("Prepare for sorting")
        }

        config.Target |> Directory.ensure

        let! allFilesInSource = SubTask.findFilesInSource dependencies
        let! relevantFilesInSource = SubTask.filterRelevantSourceFiles allFilesInSource dependencies
        let! hashedFilesInSource = SubTask.useHashForFilesInSource relevantFilesInSource dependencies
        let! excludedFiles = SubTask.exludeFiles dependencies

        do! SubTask.copyFromFiles hashedFilesInSource excludedFiles dependencies

        return "Done"
    }
