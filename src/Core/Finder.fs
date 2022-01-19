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

    [<RequireQualifiedAccess>]
    module private File =
        let create (io: MF.ConsoleApplication.IO) (loggerFactory: ILoggerFactory) ffmpeg file =
            match file |> FileType.determine with
            | Some fileType ->
                asyncResult {
                    let name =
                        file
                        |> Path.GetFileName
                        |> FileName.tryParse

                    match name with
                    | None -> return None
                    | Some name ->
                        let loadMetadata =
                            fileType
                            |> MetaData.find io loggerFactory ffmpeg

                        return
                            Some {
                                Type = fileType
                                Name = name
                                FullPath = file |> Path.GetFullPath
                                Metadata = FileMetadata.Lazy loadMetadata
                            }
                }
            | _ ->
                asyncResult {
                    let logger = loggerFactory.CreateLogger("Finder")
                    logger.LogWarning("File {file} is not image nor video (based on extension).", file)

                    return None
                }

    let findAllFilesInDir ((_, output as io): MF.ConsoleApplication.IO) loggerFactory ffmpeg dir = asyncResult {
        output.Message $"Searching all images in <c:cyan>{dir}</c>"

        let! (files: string list) =
            dir
            |> FileSystem.getAllFilesAsync io FileSystem.SearchFiles.IgnoreDotFiles
            |> AsyncResult.ofAsyncCatch (PrepareError.Exception >> List.singleton)

        use progress = new Progress(io, "Check metadata")
        progress.Start(files.Length)

        let createFiles =
            files
            |> tee (List.length >> sprintf "  ├──> found <c:magenta>%i</c> files, <c:yellow>parallely checking metadata ...</c>" >> output.Message)
            |> List.map (
                File.create io loggerFactory ffmpeg
                >> AsyncResult.tee (ignore >> progress.Advance)
            )

        let! files =
            createFiles
            |> AsyncResult.handleMultipleResults output PrepareError.Exception
            |> AsyncResult.tee (List.length >> sprintf "  └──> found <c:magenta>%i</c> images with metadata" >> output.Message)

        return files |> List.choose id
    }

    let findAllFilesInSource output loggerFactory ffmpeg source =
        source
        |> List.distinct
        |> List.map (findAllFilesInDir output loggerFactory ffmpeg)
        |> AsyncResult.ofSequentialAsyncResults (PrepareError.Exception >> List.singleton)
        |> AsyncResult.map List.concat
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

    let findExcludedFiles ((_, output as io): MF.ConsoleApplication.IO) (excludeFiles, excludeDirs) = asyncResult {
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

                    let! (files: string list list) =
                        excludeDirs
                        |> List.map (FileSystem.getAllFilesAsync io FileSystem.SearchFiles.IgnoreDotFiles)
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

    let findFilesToCopy output excludedFiles allFilesInSource =
        output.Message $"Searching all files to copy"
        output.Message <| sprintf "  ├──> From <c:magenta>%i</c> files from source" (allFilesInSource |> List.length)
        output.Message <| sprintf "  ├──> Exclude <c:red>%i</c> files" (excludedFiles |> List.length)
        if output.IsDebug() then output.List excludedFiles

        allFilesInSource
        |> List.filter (File.name >> FileName.value >> File.notIn excludedFiles)
        |> tee (List.length >> sprintf "  └──> There are <c:green>%i</c> files to copy" >> output.Message >> output.NewLine)
