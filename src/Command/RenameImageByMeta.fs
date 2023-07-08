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
        Argument.required "target" "Directory with files."
    ]

    let options = CommonOptions.all @ [
        Option.noValue "dry-run" None "If set, target directory will NOT be touched in anyway and files will only be sent to stdout."
        Option.noValue CommonOptions.ReHashAgain None "Whether to re-hash already hashed files in again."
        Option.noValue "clear-cache" None "If set, processed items will be cleared from cache."
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
        let private correctNestedDirStructure ((_, output as io): IO) originalFile hashedFile = asyncResult {
            let debug msg = if output.IsDebug() then output.Message ("<c:gray>[Debug]</c> %s", msg)

            let! createdAt =
                hashedFile
                |> File.createdAtDateTimeAsync io
                |> AsyncResult.ofAsyncCatch RuntimeError

            return
                match createdAt with
                | Some (createdAt: DateTime) ->
                    let originalPath =
                        (originalFile.FullPath |> FullPath.value).Split Path.DirectorySeparatorChar
                        |> List.ofArray
                        |> List.rev

                    let month = sprintf "%02i"
                    let placeholder value = sprintf "%c%s%c" Path.DirectorySeparatorChar value Path.DirectorySeparatorChar

                    match originalPath with
                    | [] | [ _ ] | [ _; _ ] ->
                        debug $"No need to correct dir structure for <c:cyan>{hashedFile.FullPath.Value}</c>"
                        hashedFile

                    | _fileName :: (Regex @"(\d{2})" [ currentMonth ]) :: (Regex @"(\d{4})" [ currentYear ]) :: _ ->
                        debug $"Correct year/month structure for <c:cyan>{hashedFile.FullPath.Value}</c>"
                        { hashedFile
                            with
                                FullPath =
                                    hashedFile.FullPath
                                        .Replace(placeholder currentYear, placeholder (string createdAt.Year))
                                        .Replace(placeholder currentMonth, placeholder (month createdAt.Month))
                        }

                    | _fileName :: path ->
                        debug $"Create year/month structure for <c:cyan>{hashedFile.FullPath.Value}</c>"
                        { hashedFile
                            with
                                FullPath =
                                    (hashedFile.Name |> FileName.value)
                                    :: month createdAt.Month
                                    :: string createdAt.Year
                                    :: path
                                    |> List.rev
                                    |> String.concat (string Path.DirectorySeparatorChar)
                                    |> FullPath
                        }
                    |> tee (fun file -> debug $" -> Corrected path is <c:green>{file.FullPath.Value}</c>")
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

        let private hashFile ((_, output) as io: IO) file = asyncResult {
            let name = file.Name |> FileName.value

            let! metadata =
                file
                |> FileMetadata.load io
                |> Result.mapError PrepareError

            if metadata.IsEmpty then
                return! AsyncResult.ofError (NoMetadata file)

            let hash = Hash.calculate file.Type metadata
            let extension = name |> Extension.correctFromPath output metadata
            let hashName = sprintf "%s%s" (hash |> Hash.value) (extension |> Extension.value)

            return! correctNestedDirStructure io file {
                file
                    with
                        Name = Hashed (hash, extension)
                        FullPath = file.FullPath.Replace(name, hashName)
            }
        }

        let convertToHash ((input, output as io): IO): File -> _ =
            let rehash =
                match input with
                | Input.Option.Has CommonOptions.ReHashAgain _ ->
                    output.Note "Re-hash files again"
                    true
                | _ -> false

            function
            | { Name = Hashed _ } when not rehash -> None
            | file -> Some (hashFile io file)

        let replace clearFromCache { Original = original; Renamed = renamed } = async {
            let newPath = renamed.FullPath.Value
            newPath |> Path.GetDirectoryName |> Directory.ensure

            File.Move(original.FullPath.Value, newPath, false)
            FullPath newPath |> ProcessedItems.add clearFromCache
        }

        let move clearFromCache (toMove: FileToMove) = async {
            let targetDir, targetPath = toMove.Target
            targetDir |> Directory.ensure

            File.Move(toMove.FullPath.Value, targetPath)
            FullPath targetPath |> ProcessedItems.add clearFromCache
        }

        let remove clearFromCache (FileToRemove file) = async {
            File.Delete(file.FullPath.Value)
            file.FullPath |> ProcessedItems.add clearFromCache
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
                    )
                    |> AsyncResult.handleMultipleResults io "Prepare renaming files" RuntimeError
                    <!> List.choose id
            }

    type AnalyzedResult = {
        ToRename: RenameFile list
        ToRemove: FileToRemove list
        ToMove: FileToMove list
    }

    [<RequireQualifiedAccess>]
    module private Analyze =
        module private SizeGroup =
            type Sizes = Map<FullPath, int64>

            /// Size group is a group of files with the same size = duplicities
            type SizeGroup = (RenameFile list) list

            let findBy (field: File -> FullPath) files: Sizes * SizeGroup =
                let sizes =
                    files
                    |> List.map (fun { Original = original } ->
                        let field = original |> field
                        field, (FileInfo field.Value).Length
                    )
                    |> Map.ofList

                let sizeGroups: SizeGroup =
                    files
                    |> List.groupBy (fun d -> sizes[d.Original |> field])
                    |> List.map snd

                sizes, sizeGroups

            let analyze: SizeGroup -> _ = function
                // just fallbacks
                | [] | [ [] ] -> None

                // this case is for files with the same value (= exactly one file in size group)
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

            let analyzeRenames: SizeGroup -> _ = function
                // just fallbacks
                | [] | [ [] ] -> None

                // this case is for files with the same value (= exactly one file in size group)
                | [ toPick :: toRemove ] ->
                    let toRemove =
                        toRemove
                        |> List.map (fun { Original = original } -> FileToRemove original)

                    Some (Some toPick, toRemove)

                // this case is for files with the same hash but a different size (= more size groups)
                | differentSizes ->
                    (* let toMove =
                        differentSizes
                        |> List.concat
                        |> List.choose (function
                            | { Original = original; Renamed = { Name = Hashed (hash, _) } } -> Some (FileToMove (hash, original))
                            | _ -> None // just fallback, all files should match pattern above
                        )

                    Some (None, [], toMove) *)
                    failwithf "Not implemented - this case is too much to understand without an example.\nDifferent sizes: %A\n" differentSizes

        [<RequireQualifiedAccess>]
        module AnalyzedResult =
            type private DebugFile =
                | File of File
                | ToMoveOriginal of FileToMove
                | ToMoveTarget of FileToMove
                | ToRemove of FileToRemove

            let debug (output: Output) { ToRename = toRename; ToRemove = toRemove; ToMove = toMove } =
                if output.IsVeryVerbose() then
                    let renameSizes, _ = toRename |> SizeGroup.findBy (fun original -> original.FullPath)

                    let isDebug = output.IsDebug()
                    let rec nameOrPath file =
                        if isDebug
                        then
                            match file with
                            | File file -> file.FullPath.Value
                            | ToMoveOriginal file -> file.FullPath.Value
                            | ToMoveTarget file -> file.Target |> snd
                            | ToRemove file -> file.FullPath.Value
                        else
                            match file with
                            | File file -> file.Name |> FileName.value
                            | ToMoveOriginal file -> file.Name |> FileName.value
                            | ToMoveTarget file -> file.Target |> snd
                            | ToRemove file -> file.Name |> FileName.value

                    output.SubTitle "Files to rename"
                    toRename
                    |> List.map (fun file -> [
                        File file.Original |> nameOrPath
                        File file.Renamed |> nameOrPath
                        (renameSizes[file.Original.FullPath] |> float) / 1024.0 |> string
                        "<c:yellow>Rename</c>"
                    ])
                    |> output.Table [ "Original"; "Target"; "Size (kb)"; "Action" ]

                    output.SubTitle "Files to remove"
                    toRemove
                    |> List.map (fun file -> [
                        ToRemove file |> nameOrPath
                        "-"
                        "N/A"
                        "<c:red>Remove</c>"
                    ])
                    |> output.Table [ "Original"; "Target"; "Size (kb)"; "Action" ]

                    output.SubTitle "Files to move"
                    toMove
                    |> List.map (fun file -> [
                        ToMoveOriginal file |> nameOrPath
                        ToMoveTarget file |> nameOrPath
                        "N/A"
                        "<c:cyan>Move to subdir</c>"
                    ])
                    |> output.Table [ "Original"; "Target"; "Size (kb)"; "Action" ]

        let private debugAnalyzedFiles (output: Output) executeMode path duplicities (sizes: SizeGroup.Sizes) (analyzedResult: option<option<RenameFile> * list<FileToRemove> * list<FileToMove>>) =
            if output.IsDebug() || executeMode = DryRun then
                let (toReallyRename, toReallyRemove, toReallyMoveToSubdir) =
                    match analyzedResult with
                    | Some (toRename, toRemove, toMove) -> toRename, toRemove, toMove
                    | _ -> None, [], []

                output.SubTitle $"[Debug] Analyzing duplicities from path {path}"
                |> output.NewLine

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

        let run: RunTask<RenameFile, AnalyzedResult> =
            fun { IO = ((_, output) as io); ExecuteMode = executeMode } preparedRenames -> asyncResult {
                let analyzedRenames =
                    use analyzeFiles = new Progress(io, "Analyze files")

                    preparedRenames
                    |> tee (List.length >> sprintf "  ├──> <c:yellow>Analyze files</c>[<c:magenta>%i</c>] before renaming ..." >> output.Message)
                    |> List.groupBy (fun { Renamed = { FullPath = fullPath } } -> fullPath)
                    |> tee (List.length >> analyzeFiles.Start)
                    |> List.choose (function
                        | (_, []) ->
                            analyzeFiles.Advance()
                            None

                        | (_, [ justRenamed ]) ->
                            analyzeFiles.Advance()
                            Some (Some justRenamed, [], [])

                        | (path, duplicities) ->
                            let sizes, sizeGroups = duplicities |> SizeGroup.findBy (fun original -> original.FullPath)
                            let analyzedResult = sizeGroups |> SizeGroup.analyze

                            analyzedResult |> debugAnalyzedFiles output executeMode path duplicities sizes

                            analyzeFiles.Advance()
                            analyzedResult
                )

                let (toRename, toRemove, toMove) = analyzedRenames |> List.unzip3
                // todo - taky jsem pridal do cache extensionu a je treba projit a zkontrolovat, ze se vsude spravne pouzije

                return {
                    ToRename = toRename |> List.choose id
                    ToRemove = toRemove |> List.concat |> List.distinctBy (fun (FileToRemove original) -> original.FullPath)
                    ToMove = toMove |> List.concat
                }
            }

    [<RequireQualifiedAccess>]
    module private Rename =
        let run clearFromCache: RunTask<RenameFile, unit> =
            fun { IO = ((_, output) as io); ExecuteMode = executeMode; LoggerFactory = loggerFactory } filesToRename -> asyncResult {
                let logger = loggerFactory.CreateLogger("Rename files")

                let! results =
                    filesToRename
                    |> List.map (fun toRename ->
                        asyncResult {
                            match executeMode with
                            | DryRun -> output.Message $"  ├────> <c:yellow>Rename</c> file <c:cyan>{toRename.Original.Name |> FileName.value}</c> to <c:yellow>{toRename.Renamed.Name |> FileName.value}</c>"
                            | Execute -> do! toRename |> File.replace clearFromCache
                        }
                        |> AsyncResult.teeError (fun e -> logger.LogError("Rename file {file} failed with {error}", toRename, e))
                        <@> RuntimeError
                    )
                    |> tee (List.length >> sprintf "  ├──> <c:yellow>Renaming files</c>[<c:magenta>%i</c>] <c:yellow>in parallel</c> ..." >> output.Message)
                    |> AsyncResult.handleMultipleResultsBy io "Rename files" (output.IsDebug() || executeMode = DryRun) RuntimeError

                results |> Seq.length |> sprintf "  └──> Renaming files[<c:magenta>%i</c>] finished." |> output.Message

                return ()
            }

    [<RequireQualifiedAccess>]
    module private Move =
        let run clearFromCache: RunTask<FileToMove, unit> =
            fun { IO = ((_, output) as io); ExecuteMode = executeMode; LoggerFactory = loggerFactory } filesToMove -> asyncResult {
                let logger = loggerFactory.CreateLogger("Move files to subdir")

                let! moveResults =
                    filesToMove
                    |> List.map (fun toMove ->
                        asyncResult {
                            match executeMode with
                            | DryRun -> output.Message $"  ├────> <c:cyan>Move</c> file <c:cyan>{toMove.Name |> FileName.value}</c> from <c:gray>{toMove.FullPath}</c> to <c:cyan>{toMove.Target |> snd}</c>"
                            | Execute -> do! toMove |> File.move clearFromCache
                        }
                        <@> RuntimeError
                        |> AsyncResult.teeError (fun e -> logger.LogError("Move file {file} failed with {error}", toMove, e))
                    )
                    |> tee (List.length >> sprintf "  ├──> <c:yellow>Move files</c>[<c:magenta>%i</c>] <c:yellow>in parallel</c> ..." >> output.Message)
                    |> AsyncResult.handleMultipleResultsBy io "Move files" (output.IsDebug() || executeMode = DryRun) RuntimeError

                moveResults |> Seq.length |> sprintf "  └──> Moving files[<c:magenta>%i</c>] finished." |> output.Message

                return ()
            }

    [<RequireQualifiedAccess>]
    module private Remove =
        let run clearFromCache: RunTask<FileToRemove, unit> =
            fun { IO = ((_, output) as io); ExecuteMode = executeMode; LoggerFactory = loggerFactory } filesToRemove -> asyncResult {
                let logger = loggerFactory.CreateLogger("Remove files")

                let! removeResults =
                    filesToRemove
                    |> List.map (fun toRemove ->
                        asyncResult {
                            match executeMode with
                            | DryRun -> output.Message $"  ├────> <c:red>Remove</c> file <c:cyan>{toRemove.Name |> FileName.value}</c> at <c:gray>{toRemove.FullPath}</c>"
                            | Execute -> do! toRemove |> File.remove clearFromCache
                        }
                        <@> RuntimeError
                        |> AsyncResult.teeError (fun e -> logger.LogError("Remove file {file} failed with {error}", toRemove, e))
                    )
                    |> tee (List.length >> sprintf "  ├──> <c:yellow>Remove files</c>[<c:magenta>%i</c>] <c:yellow>in parallel</c> ..." >> output.Message)
                    |> AsyncResult.handleMultipleResultsBy io "Remove files" (output.IsDebug() || executeMode = DryRun) RuntimeError

                removeResults |> Seq.length |> sprintf "  └──> Removing files[<c:magenta>%i</c>] finished." |> output.Message

                return ()
            }

    let private run dependencies ffmpeg clearCache target: AsyncResult<string, RenameError list> = asyncResult {
        let (_, output) = dependencies.IO
        let! files =
            target
            |> Finder.findAllFilesInDir dependencies.IO dependencies.LoggerFactory ffmpeg <@> List.map PrepareError

        output.NewLine()
        output.Message $"Rename files in <c:cyan>{target}</c> ..."

        let! preparedRenames = files |> PrepareRenames.run dependencies
        let! analyzedResult = preparedRenames |> Analyze.run dependencies

        let clearFromCache = ProcessedItems.create()

        analyzedResult |> Analyze.AnalyzedResult.debug output

        do!
            [
                analyzedResult.ToRename |> Rename.run clearFromCache dependencies
                analyzedResult.ToMove |> Move.run clearFromCache dependencies
                analyzedResult.ToRemove |> Remove.run clearFromCache dependencies
            ]
            |> List.map (AsyncResult.teeError (fun _ -> output.Message "  └──> <c:red>Some errors occured</c>"))
            |> AsyncResult.ofSequentialAsyncResults (RuntimeError >> List.singleton)
            |> AsyncResult.mapError List.concat
            |> AsyncResult.ignore

        if clearCache then
            output.NewLine()
            output.SubTitle "Clear processed items from cache"
            use cacheClearProgress =
                clearFromCache
                |> ProcessedItems.count
                |> output.ProgressStart "Clear processed items from cache"

            do!
                clearFromCache
                |> ProcessedItems.getAll
                |> Seq.toList
                |> List.map (fun path -> asyncResult {
                    do! path |> Hash.Cache.clearItem |> AsyncResult.mapError PrepareError
                    cacheClearProgress.Advance()
                })
                |> AsyncResult.ofParallelAsyncResults RuntimeError
                |> AsyncResult.ignore

            cacheClearProgress.Finish()

            do! Hash.Cache.persistCache output |> AsyncResult.mapError (PrepareError >> List.singleton)

        clearFromCache |> ProcessedItems.clear

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

            let! clearCache =
                match input with
                | Input.Option.IsSet "clear-cache" _ ->
                    asyncResult {
                        do! Hash.Cache.load loggerFactory
                            |> AsyncResult.mapError (PrepareError >> List.singleton)
                        output.Success "Note: Cache for hashes is loaded."
                        return true
                    }
                | _ -> AsyncResult.ofSuccess false

            if output.IsVerbose() then
                output.Message <| sprintf "FFMpeg: %A" ffmpeg

            let target = input |> Input.Argument.value "target"

            let dependencies = {
                IO = (input, output)
                LoggerFactory = loggerFactory
                ExecuteMode = executeMode
            }

            let! message = target |> run dependencies ffmpeg clearCache

            output.Success message

            return ExitCode.Success
        }
        |> AsyncResult.waitAfterFinish output 2000
        |> AsyncResult.mapError (Errors.map "Rename Image Command" output RenameError.format)
