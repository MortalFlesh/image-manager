namespace MF.ImageManager

[<RequireQualifiedAccess>]
module Finder =
    open System
    open System.IO
    open Microsoft.Extensions.Logging
    open MF.ConsoleApplication
    open MF.Utils
    open MF.Utils.Progress
    open MF.ErrorHandling

    type FileTypeToFind =
        | All
        | FindOnly of FileType

    [<RequireQualifiedAccess>]
    module private File =
        let create ((_, output) as io: MF.ConsoleApplication.IO) (loggerFactory: ILoggerFactory) ffmpeg path =
            match path |> FileType.determine with
            | Some fileType ->
                let name =
                    path
                    |> Path.GetFileName
                    |> FileName.tryParse

                let fullPath =
                    path
                    |> Path.GetFullPath
                    |> FullPath

                match name with
                | None -> None
                | Some name ->
                    let loadMetadata =
                        (fileType, fullPath)
                        |> MetaData.find io loggerFactory ffmpeg

                    Some {
                        Type = fileType
                        CacheKey = CacheKey fullPath.Value
                        Name = name
                        FullPath = fullPath
                        Metadata = FileMetadata.Lazy loadMetadata
                    }
            | _ ->
                let logger = loggerFactory.CreateLogger("Finder")
                logger.LogWarning("File {file} is not image nor video (based on extension).", path)
                if output.IsDebug() then output.Message $"  ├──> File <c:cyan>{path}</c> is not image nor video (based on extension)."

                None

        let createAsync io loggerFactory ffmpeg path = async {
            return create io loggerFactory ffmpeg path
        }

    let findAllFilesInDir ((_, output as io): IO) loggerFactory ffmpeg dir = asyncResult {
        output.Message $"Searching for all files in <c:cyan>{dir}</c>"

        let! (files: string list) =
            dir
            |> FileSystem.getAllFilesAsync io FileSystem.SearchFiles.IgnoreDotFiles FileType.is
            |> AsyncResult.ofAsyncCatch (PrepareError.Exception >> List.singleton)

        output.Message $"Initializing found files [<c:magenta>{files.Length}</c>] from <c:cyan>{dir}</c>"

        let createFiles =
            files
            |> List.map (
                File.createAsync io loggerFactory ffmpeg
                >> AsyncResult.ofAsyncCatch PrepareError.Exception
            )

        let! files =
            createFiles
            |> AsyncResult.handleMultipleResults io "Initialize files" PrepareError.Exception

        return
            files
            |> List.choose id
            |> tee (List.length >> sprintf "  └──> found <c:magenta>%i</c> files" >> output.Message)
    }

    let findAllFilesInSource io loggerFactory ffmpeg source =
        source
        |> List.distinct
        |> List.map (findAllFilesInDir io loggerFactory ffmpeg)
        |> AsyncResult.ofSequentialAsyncResults (PrepareError.Exception >> List.singleton)
        |> AsyncResult.map List.concat
        |> AsyncResult.mapError List.concat

    let findFilesAndDirsToExclude targetDirMode exclude excludeList target =
        let excludeDirs =
            match targetDirMode, exclude with
            | Override, Some excludeDirs -> Some excludeDirs

            | Exclude, Some excludeDirs
            | DryRun, Some excludeDirs -> Some (target :: excludeDirs)

            | Exclude, None
            | DryRun, None -> Some [ target ]

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

    let findExcludedFiles ((_, output as io): MF.ConsoleApplication.IO) (excludeFiles, excludeDirs) = asyncResult {
        output.Message "Searching all files to exclude"
        if output.IsDebug() then
            output.Message <| sprintf "  ├──> Excluding dirs %A" excludeDirs
            output.Message <| sprintf "  ├──> Excluding files %A" excludeFiles

        let! excludeFiles =
            match excludeDirs with
            | Some excludeDirs ->
                asyncResult {
                    let excludeDirs =
                        excludeDirs
                        |> List.distinct
                        |> tee (List.length >> sprintf "  ├──> Exclude dirs[<c:red>%i</c>]" >> output.Message)
                        |> tee (List.iter (sprintf "  │      - <c:red>%s</c>" >> output.Message))

                    let! (files: string list list) =
                        excludeDirs
                        |> List.map (FileSystem.getAllFilesAsync io FileSystem.SearchFiles.IgnoreDotFiles FileType.is)
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

    let findFilesToCopy (output: Output) excludedFiles allFilesInSource =
        output.Message $"Searching all files to copy"
        output.Message <| sprintf "  ├──> From <c:magenta>%i</c> files from source" (allFilesInSource |> List.length)
        output.Message <| sprintf "  ├──> Exclude <c:red>%i</c> files" (excludedFiles |> List.length)
        if output.IsDebug() then output.List excludedFiles

        allFilesInSource
        |> List.filter (FileToCopy.target >> File.name >> FileName.value >> File.notIn excludedFiles)
        |> tee (List.length >> sprintf "  └──> There are <c:green>%i</c> files to copy" >> output.Message >> output.NewLine)
