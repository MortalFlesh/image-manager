namespace MF.ImageManager.Command

open System
open System.IO
open Microsoft.Extensions.Logging
open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ErrorHandling.AsyncResult.Operators
open MF.ImageManager
open MF.Utils
open MF.Utils.Progress
open MF.Utils.Logging

[<RequireQualifiedAccess>]
module RenameImageByMeta =
    type ExecuteMode =
        | DryRun
        | Execute

    let inline private (</>) a b = Path.Combine(a, b)

    let arguments = [
        Argument.required "target" "Directory with filess."
    ]

    let options = CommonOptions.all @ [
        Option.noValue "dry-run" None "If set, target directory will NOT be touched in anyway and filess will only be sent to stdout."
        Option.noValue CommonOptions.ReHashAgain None "Whether to re-hash already hashed files in again."
    ]

    type RenameFile = {
        Original: File
        Renamed: File
    }

    type FileToRemove =
        FileToRemove of File
        with
            member this.Name =
                let (FileToRemove file) = this
                file.Name
            member this.FullPath =
                let (FileToRemove file) = this
                file.FullPath

    type FileToMove =
        FileToMove of Hash * File
        with
            member this.Name =
                let (FileToMove (_, file)) = this
                file.Name
            member this.FullPath =
                let (FileToMove (_, file)) = this
                file.FullPath
            member this.Target =
                let (FileToMove (hash, file)) = this
                let targetDir = file.FullPath.GetDirectoryName() </> (hash |> Hash.value)
                let targetPath = targetDir </> (file.Name |> FileName.value)
                targetDir, targetPath

    type RenameError =
        | PrepareError of PrepareError
        | NoMetadata of File
        | RuntimeError of exn

    [<RequireQualifiedAccess>]
    module RenameError =
        let format = function
            | PrepareError e -> PrepareError.format e
            | NoMetadata file -> $"File {file.FullPath} has no metadata"
            | RuntimeError e -> $"Renaming ends with runtime error {e}"

    [<RequireQualifiedAccess>]
    module private File =
        /// This should correct dir structure by new hashed file
        /// for example from
        /// - root/2022/07/i_20220701_x.jpg (original file)
        /// - root/2022/07/i_20220504_x.jpg (hashed file)
        /// - root/2022/05/i_20220504_x.jpg (all correct)
        let private correctNestedDirStructure output originalFile hashedFile = asyncResult {
            let! createdAt =
                hashedFile
                |> File.createdAtDateTimeAsync output
                |> AsyncResult.ofAsyncCatch RuntimeError

            return
                match createdAt with
                | Some (createdAt: DateTime) ->
                    let originalPath =
                        (originalFile.FullPath |> FullPath.value).Split Path.DirectorySeparatorChar
                        |> List.ofArray
                        |> List.rev

                    match originalPath with
                    | [] | [ _ ] | [ _; _ ] -> hashedFile
                    | _fileName :: currentMonth :: currentYear :: _ ->
                        let month = sprintf "%02i"

                        let placeholder value = sprintf "%c%s%c" Path.DirectorySeparatorChar value Path.DirectorySeparatorChar

                        { hashedFile
                            with
                                FullPath =
                                    hashedFile.FullPath
                                        .Replace(placeholder currentYear, placeholder (string createdAt.Year))
                                        .Replace(placeholder currentMonth, placeholder (month createdAt.Month))
                        }
                | _ -> hashedFile
        }

        (* let test () =
            let originalPath = "root/2022/07/i_20220701_x.jpg"
            let createdAt = System.DateTime.Parse("2021-02-08")

            let originalPathParts =
                originalPath.Split(System.IO.Path.DirectorySeparatorChar)
                |> List.ofArray
                |> List.rev

            let hashedPath = originalPath.Replace("i_20220701_x.jpg", "i_20210208_x.jpg")

            let fixedPath =
                match originalPathParts with
                | [] | [ _ ] | [ _; _ ] -> hashedPath
                | _fileName :: currentMonth :: currentYear :: _ ->
                    let month = sprintf "%02i"

                    let placeholder value = sprintf "%c%s%c" System.IO.Path.DirectorySeparatorChar value System.IO.Path.DirectorySeparatorChar

                    hashedPath
                        .Replace(placeholder currentYear, placeholder (string createdAt.Year))
                        .Replace(placeholder currentMonth, placeholder (month createdAt.Month))

            [
                sprintf "%s (original)" originalPath
                sprintf "%s (hashed)" hashedPath
                sprintf "%s (fixed)" fixedPath
            ]
            |> List.iter (printfn "- %s")
            () *)

        let private hashFile output file = asyncResult {
            let name = file.Name |> FileName.value

            let! metadata =
                file
                |> FileMetadata.load output
                |> Result.mapError PrepareError

            if metadata.IsEmpty then
                return! AsyncResult.ofError (NoMetadata file)

            let hash = Hash.calculate file.Type metadata
            let extension = name |> Extension.correctFromPath output metadata
            let hashName = sprintf "%s%s" (hash |> Hash.value) (extension |> Extension.value)

            return! correctNestedDirStructure output file {
                file
                    with
                        Name = Hashed (hash, extension)
                        FullPath = file.FullPath.Replace(name, hashName)
            }
        }

        let convertToHash ((input, output): IO): File -> _ =
            let rehash =
                match input with
                | Input.Option.Has CommonOptions.ReHashAgain _ ->
                    output.Note "Re-hash files again"
                    true
                | _ -> false

            function
            | { Name = Hashed _ } when not rehash -> None
            | file -> Some (hashFile output file)

        let replace { Original = original; Renamed = renamed } = async {
            let newPath = renamed.FullPath.Value
            newPath |> Path.GetDirectoryName |> Directory.ensure

            File.Move(original.FullPath.Value, newPath, false)
        }

        let move (toMove: FileToMove) = async {
            let targetDir, targetPath = toMove.Target
            targetDir |> Directory.ensure

            File.Move(toMove.FullPath.Value, targetPath)
        }

        let remove (FileToRemove file) = async {
            File.Delete(file.FullPath.Value)
        }

    type TaskDependencies = {
        IO: MF.ConsoleApplication.IO
        LoggerFactory: ILoggerFactory
        ExecuteMode: ExecuteMode
    }

    type private RunTask<'Input, 'Output> = TaskDependencies -> 'Input list -> AsyncResult<'Output, RenameError list>

    [<RequireQualifiedAccess>]
    module private PrepareRenames =
        let run: RunTask<File, RenameFile list> =
            fun { IO = ((_, output) as io); LoggerFactory = loggerFactory } files -> asyncResult {
                let logger = loggerFactory.CreateLogger("Prepare renaming files")
                use prepareRenamesProgress = new Progress(io, "Prepare renames")
                let convertToHash = File.convertToHash io

                return!
                    files
                    |> tee (List.length >> sprintf "  ├──> <c:yellow>Prepare files</c>[<c:magenta>%i</c>] to rename ..." >> output.Message)
                    |> List.choose (fun file ->
                        maybe {
                            let! convertToHash = file |> convertToHash

                            return
                                asyncResult {
                                    let! hashedFile = convertToHash

                                    return Some {
                                        Original = file
                                        Renamed = hashedFile
                                    }
                                }
                                >>- (function
                                    | NoMetadata file ->
                                        logger.LogWarning("File {file} has no metadata.", file)
                                        AsyncResult.ofSuccess None
                                    | error -> AsyncResult.ofError error
                                )
                        }
                        |> Option.teeNone (fun _ -> if output.IsDebug() then output.Message $"  ├────> Renaming file <c:cyan>{file.Name |> FileName.value}</c> is <c:dark-yellow>skipped</c>.")
                        |> tee (ignore >> prepareRenamesProgress.Advance)
                    )
                    |> tee (List.length >> prepareRenamesProgress.Start)
                    |> AsyncResult.handleMultipleResults output RuntimeError
                    <!> List.choose id
            }

    type AnalyzedResult = {
        ToRename: RenameFile list
        ToRemove: FileToRemove list
        ToMove: FileToMove list
    }

    [<RequireQualifiedAccess>]
    module private Analyze =
        let run: RunTask<RenameFile, AnalyzedResult> =
            fun { IO = ((_, output) as io); ExecuteMode = executeMode } preparedRenames -> asyncResult {
                use analyzeFiles = new Progress(io, "Analyze files")

                let analyzedRenames =
                    preparedRenames
                    |> tee (List.length >> sprintf "  ├──> <c:yellow>Analyze files</c>[<c:magenta>%i</c>] before renaming ..." >> output.Message)
                    |> tee (List.length >> analyzeFiles.Start)
                    |> List.groupBy (fun { Renamed = { FullPath = fullPath } } -> fullPath)
                    |> List.choose (function
                        | (_, []) ->
                            analyzeFiles.Advance()
                            None

                        | (_, [ justRenamed ]) ->
                            analyzeFiles.Advance()
                            Some (Some justRenamed, [], [])

                        | (path, duplicities) ->
                            let sizes =
                                duplicities
                                |> List.map (fun { Original = original } -> original.FullPath, (FileInfo original.FullPath.Value).Length)
                                |> Map.ofList

                            let sizeGroups =
                                duplicities
                                |> List.groupBy (fun d -> sizes[d.Original.FullPath])
                                |> List.map snd

                            let analyzedResult =
                                match sizeGroups with
                                // just fallbacks
                                | [] | [ [] ] -> None

                                // this case is for files with the same hash and size (= exactly one size group)
                                | [ toPick :: toRemove ] ->
                                    let toRemove =
                                        toRemove
                                        |> List.map (fun { Original = original } -> FileToRemove original)

                                    Some (Some toPick, toRemove, [])

                                // this case is for files with the same hash but a different size (= more size groups)
                                | differentSizes ->
                                    let toMove =
                                        differentSizes
                                        |> List.concat
                                        |> List.choose (function
                                            | { Original = original; Renamed = { Name = Hashed (hash, _) } } -> Some (FileToMove (hash, original))
                                            | _ -> None // just fallback, all files should match pattern above
                                        )

                                    Some (None, [], toMove)

                            if output.IsDebug() || executeMode = DryRun then
                                let (toReallyRename, toReallyRemove, toReallyMoveToSubdir) =
                                    match analyzedResult with
                                    | Some (toRename, toRemove, toMove) -> toRename, toRemove, toMove
                                    | _ -> None, [], []

                                output.SubTitle $"[Debug] Analyzing duplicities from path {path}"
                                duplicities
                                |> List.sortByDescending (fun { Original = { FullPath = fullPath } } -> fullPath)
                                |> List.map (fun toRename ->
                                    let action =
                                        match toReallyRename, toReallyRemove, toReallyMoveToSubdir with
                                        | Some { Original = { FullPath = renameFullPath } }, _, _ when renameFullPath = toRename.Original.FullPath -> "<c:yellow>Rename</c>"
                                        | _, toRemove, _ when toRemove |> List.exists (fun f -> f.FullPath = toRename.Original.FullPath) -> "<c:red>Remove</c>"
                                        | _, _, toReallyMoveToSubdir when toReallyMoveToSubdir |> List.exists (fun f -> f.FullPath = toRename.Original.FullPath) -> "<c:cyan>Move to subdir</c>"
                                        | _ -> "<c:gray>Keep unchanged</c>"

                                    [
                                        toRename.Renamed.Name |> FileName.value
                                        toRename.Original.FullPath.Value
                                        (sizes[toRename.Original.FullPath] |> float) / 1024.0 |> string
                                        action
                                    ]
                                )
                                |> output.Table [ "Renamed"; "Original"; "Size (kb)"; "Action" ]

                            analyzeFiles.Advance()
                            analyzedResult
                )

                let (toRename, toRemove, toMove) = analyzedRenames |> List.unzip3

                return {
                    ToRename = toRename |> List.choose id
                    ToRemove = toRemove |> List.concat
                    ToMove = toMove |> List.concat
                }
            }

    [<RequireQualifiedAccess>]
    module private Rename =
        let run: RunTask<RenameFile, unit> =
            fun { IO = ((_, output) as io); ExecuteMode = executeMode; LoggerFactory = loggerFactory } filesToRename -> asyncResult {
                let logger = loggerFactory.CreateLogger("Rename files")
                let renameFilesProgress = new Progress(io, "Rename files")

                let! results =
                    filesToRename
                    |> List.map (fun toRename ->
                        asyncResult {
                            match executeMode with
                            | DryRun -> output.Message $"  ├────> <c:yellow>Rename</c> file <c:cyan>{toRename.Original.Name |> FileName.value}</c> to <c:yellow>{toRename.Renamed.Name |> FileName.value}</c>"
                            | Execute -> do! toRename |> File.replace
                        }
                        |> Async.tee (ignore >> renameFilesProgress.Advance)
                        |> AsyncResult.teeError (fun e -> logger.LogError("Rename file {file} failed with {error}", toRename, e))
                        <@> RuntimeError
                    )
                    |> tee (List.length >> sprintf "  ├──> <c:yellow>Renaming files</c>[<c:magenta>%i</c>] <c:yellow>in parallel</c> ..." >> output.Message)
                    |> tee (List.length >> renameFilesProgress.Start)
                    |> AsyncResult.handleMultipleResultsBy (output.IsDebug() || executeMode = DryRun) RuntimeError
                    |> Async.tee (ignore >> renameFilesProgress.Finish)

                results |> Seq.length |> sprintf "  └──> Renaming files[<c:magenta>%i</c>] finished." |> output.Message

                return ()
            }

    [<RequireQualifiedAccess>]
    module private Move =
        let run: RunTask<FileToMove, unit> =
            fun { IO = ((_, output) as io); ExecuteMode = executeMode; LoggerFactory = loggerFactory } filesToMove -> asyncResult {
                let logger = loggerFactory.CreateLogger("Move files to subdir")
                let moveFilesProgress = new Progress(io, "Move files to subdir")

                let! moveResults =
                    filesToMove
                    |> List.map (fun toMove ->
                        asyncResult {
                            match executeMode with
                            | DryRun -> output.Message $"  ├────> <c:cyan>Move</c> file <c:cyan>{toMove.Name |> FileName.value}</c> from <c:gray>{toMove.FullPath}</c> to <c:cyan>{toMove.Target |> snd}</c>"
                            | Execute -> do! toMove |> File.move
                        }
                        <@> RuntimeError
                        |> AsyncResult.tee moveFilesProgress.Advance
                        |> AsyncResult.teeError ((fun e -> logger.LogError("Move file {file} failed with {error}", toMove, e)) >> moveFilesProgress.Advance)
                    )
                    |> tee (List.length >> sprintf "  ├──> <c:yellow>Move files</c>[<c:magenta>%i</c>] <c:yellow>in parallel</c> ..." >> output.Message)
                    |> tee (List.length >> moveFilesProgress.Start)
                    |> AsyncResult.handleMultipleResultsBy (output.IsDebug() || executeMode = DryRun) RuntimeError
                    |> Async.tee (ignore >> moveFilesProgress.Finish)

                moveResults |> Seq.length |> sprintf "  └──> Moving files[<c:magenta>%i</c>] finished." |> output.Message

                return ()
            }

    [<RequireQualifiedAccess>]
    module private Remove =
        let run: RunTask<FileToRemove, unit> =
            fun { IO = ((_, output) as io); ExecuteMode = executeMode; LoggerFactory = loggerFactory } filesToRemove -> asyncResult {
                let logger = loggerFactory.CreateLogger("Remove files")
                let removeFilesProgress = new Progress(io, "Remove files")

                let! removeResults =
                    filesToRemove
                    |> List.map (fun toRemove ->
                        asyncResult {
                            match executeMode with
                            | DryRun -> output.Message $"  ├────> <c:red>Remove</c> file <c:cyan>{toRemove.Name |> FileName.value}</c> at <c:gray>{toRemove.FullPath}</c>"
                            | Execute -> do! toRemove |> File.remove
                        }
                        <@> RuntimeError
                        |> AsyncResult.tee removeFilesProgress.Advance
                        |> AsyncResult.teeError ((fun e -> logger.LogError("Remove file {file} failed with {error}", toRemove, e)) >> removeFilesProgress.Advance)
                    )
                    |> tee (List.length >> sprintf "  ├──> <c:yellow>Remove files</c>[<c:magenta>%i</c>] <c:yellow>in parallel</c> ..." >> output.Message)
                    |> tee (List.length >> removeFilesProgress.Start)
                    |> AsyncResult.handleMultipleResultsBy (output.IsDebug() || executeMode = DryRun) RuntimeError
                    |> Async.tee (ignore >> removeFilesProgress.Finish)

                removeResults |> Seq.length |> sprintf "  └──> Removing files[<c:magenta>%i</c>] finished." |> output.Message

                return ()
            }

    let private run dependencies ffmpeg target: AsyncResult<string, RenameError list> = asyncResult {
        let (_, output) = dependencies.IO
        let! files =
            target
            |> Finder.findAllFilesInDir dependencies.IO dependencies.LoggerFactory ffmpeg <@> List.map PrepareError

        output.NewLine()
        output.Message $"Rename files in <c:cyan>{target}</c> ..."

        let! preparedRenames = files |> PrepareRenames.run dependencies
        let! analyzedResult = preparedRenames |> Analyze.run dependencies

        do!
            [
                analyzedResult.ToRename |> Rename.run dependencies
                analyzedResult.ToMove |> Move.run dependencies
                analyzedResult.ToRemove |> Remove.run dependencies
            ]
            |> AsyncResult.ofSequentialAsyncResults (RuntimeError >> List.singleton)
            |> AsyncResult.mapError List.concat
            |> AsyncResult.map ignore

        return "Done"
    }

    let execute = ExecuteAsyncResult <| fun ((input, output): IO) ->
        asyncResult {
            use loggerFactory =
                if output.IsDebug() then "vvv"
                elif output.IsVeryVerbose() then "vv"
                else "v"
                |> LogLevel.parse
                |> LoggerFactory.create "RenameByMeta"

            let executeMode =
                match input with
                | Input.Option.IsSet "dry-run" _ -> DryRun
                | _ -> Execute

            let! ffmpeg =
                match input with
                | Input.Option.Has CommonOptions.FFMpeg (OptionValue.ValueOptional value) -> FFMpeg.init value
                | _ -> Ok FFMpeg.Empty
                |> AsyncResult.ofResult
                |> AsyncResult.mapError (PrepareError >> List.singleton)

            if output.IsVerbose() then
                output.Message <| sprintf "FFMpeg: %A" ffmpeg

            let target = input |> Input.Argument.value "target"

            let dependencies = {
                IO = (input, output)
                LoggerFactory = loggerFactory
                ExecuteMode = executeMode
            }

            let! message = target |> run dependencies ffmpeg

            output.Success message

            return ExitCode.Success
        }
        |> AsyncResult.waitAfterFinish output 2000
        |> AsyncResult.mapError (Errors.map "Rename Image Command" output RenameError.format)
