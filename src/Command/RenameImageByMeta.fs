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

    let arguments = [
        Argument.required "target" "Directory with images."
    ]

    let options = [
        Option.noValue "dry-run" None "If set, target directory will NOT be touched in anyway and images will only be sent to stdout."
        noProgressOption
    ]

    type RenameFile = {
        Original: File
        Renamed: File
    }

    type RenameError =
        | PrepareError of PrepareError
        | NoMetadata of File
        | RenameIsAllowedForOriginalAndRenamedFileOnly of (RenameFile * RenameFile)
        | RuntimeError of exn

    [<RequireQualifiedAccess>]
    module RenameError =
        let format = function
            | PrepareError e -> PrepareError.format e
            | NoMetadata file -> $"File {file.FullPath} has no metadata"
            | RenameIsAllowedForOriginalAndRenamedFileOnly files -> $"You must replace old image with a new one. You sent {files}."
            | RuntimeError e -> $"Renaming ends with runtime error {e}"

    [<RequireQualifiedAccess>]
    module private File =
        let convertToHash: File -> _ = function
            | { Name = Hashed _ } -> None
            | { Name = Normal name; Type = fileType } as file ->
                Some (asyncResult {
                    let! metadata =
                        file
                        |> FileMetadata.load
                        |> Result.mapError PrepareError

                    if metadata.IsEmpty then
                        return! AsyncResult.ofError (NoMetadata file)

                    let hash = Hash.calculate fileType metadata
                    let extension = name |> Extension.fromPath
                    let hashName = sprintf "%s%s" (hash |> Hash.value) (extension |> Extension.value)

                    return {
                        file
                            with
                                Name = Hashed (hash, extension)
                                FullPath = file.FullPath.Replace(name, hashName)
                    }
                })

        let replace { Original = original; Renamed = renamed } = async {
            File.Move(original.FullPath, renamed.FullPath, false)
        }

    let private run ((_, output as io): MF.ConsoleApplication.IO) loggerFactory executeMode target: AsyncResult<string, RenameError list> = asyncResult {
        let! images =
            target
            |> Finder.findAllFilesInDir io loggerFactory FFMpeg.empty <@> List.map PrepareError

        output.NewLine()

        if output.IsVeryVerbose() then
            images
            |> List.groupBy File.model
            |> List.map (fun (k, v) -> k, v |> List.length)
            |> List.sortBy snd
            |> List.map (fun (model, count) -> [ model |> Option.defaultValue "-"; string count ])
            |> output.Table [ "Model"; "Count" ]
            |> output.NewLine

        output.Message $"Rename files in <c:cyan>{target}</c> ..."

        let! (preparedRenames: RenameFile list) =
            let logger = loggerFactory.CreateLogger("Prepare renaming files")
            use prepareRenamesProgress = new Progress(io, "Prepare renames")

            images
            |> tee (List.length >> sprintf "  ├──> <c:yellow>Prepare files</c>[<c:magenta>%i</c>] to rename ..." >> output.Message)
            |> tee (List.length >> prepareRenamesProgress.Start)
            |> List.choose (fun image ->
                maybe {
                    let! convertToHash = image |> File.convertToHash

                    return
                        asyncResult {
                            let! hashedImage = convertToHash

                            return Some {
                                Original = image
                                Renamed = hashedImage
                            }
                        }
                        >>- (function
                            | NoMetadata file ->
                                logger.LogWarning("File {file} has no metadata.", file)
                                AsyncResult.ofSuccess None
                            | error -> AsyncResult.ofError error
                        )
                }
                |> Option.teeNone (fun _ -> if output.IsDebug() then output.Message $"  ├────> Renaming file <c:cyan>{image.Name |> FileName.value}</c> is <c:dark-yellow>skipped</c>.")
                |> tee (ignore >> prepareRenamesProgress.Advance)
            )
            |> AsyncResult.handleMultipleResults output RuntimeError
            <!> List.choose id

        let (analyzedRenames: RenameFile list) =
            let logger = loggerFactory.CreateLogger("Analyze files")
            use analyzeFiles = new Progress(io, "Analyze files")

            preparedRenames
            |> tee (List.length >> sprintf "  ├──> <c:yellow>Analyze files</c>[<c:magenta>%i</c>] before renaming ..." >> output.Message)
            |> tee (List.length >> analyzeFiles.Start)
            |> List.groupBy (fun { Renamed = file } -> file.FullPath)
            |> List.choose (function
                | (_, []) ->
                    analyzeFiles.Advance()
                    None

                | (_, [ justRenamed ]) ->
                    analyzeFiles.Advance()
                    Some justRenamed

                | (path, duplicities) ->
                    let sizes =
                        duplicities
                        |> List.map (fun { Original = original } ->
                            original.FullPath, (FileInfo original.FullPath).Length
                        )
                        |> Map.ofList

                    let dimensions =
                        duplicities
                        |> List.choose (function
                            | { Original = { Type = FileType.Image path }} ->
                                match path |> Image.getDimensions with
                                | Ok dimensions -> Some (path, dimensions)
                                | Error e ->
                                    logger.LogWarning("Image {path} has no dimensions due to {error}.", path, e)
                                    None

                            | _ -> None
                        )
                        |> Map.ofList

                    if output.IsDebug() || executeMode = DryRun then
                        output.SubTitle $"[Debug] Analyzing duplicities from path {path}"
                        duplicities
                        |> List.map (fun toRename ->
                            let (height, width) =
                                match dimensions |> Map.tryFind toRename.Original.FullPath with
                                | Some { Height = height; Width = width } -> height, width
                                | _ -> 0, 0

                            [
                                toRename.Renamed.Name |> FileName.value
                                toRename.Original.Name |> FileName.value
                                (sizes[toRename.Original.FullPath] |> float) / 1024.0 |> string
                                string height
                                string width
                            ]
                        )
                        |> output.Table [ "Renamed"; "Original"; "Size (kb)"; "Height"; "Width" ]

                    analyzeFiles.Advance()
                    None
            )   // todo analyze
            // todo - check duplicities
            // - pokud tam je, tak kouknout ktery je vetsi a ten pouzit a zalogovat ten druhy
            // |> AsyncResult.handleMultipleResults output RuntimeError

        // todo - analyza -> zkontrolovat v ramci "proslych souboru" ze tam nejsou duplicity, pripadne ze se uz na te vysledne fullPath nevyskytuji
        // pokud bude nejaky konflikt, tak poresit
        // - zkontrolovat treba contentHash tech duplicit
        // - zkontrolovat velikost (vybrat pak ten vetsi)
        // - pripadne kdyz bude vsechno stejne, tak proste vybrat jen jeden
        // - jinak aspon zalogovat a treba i neprejmenovavat

        let! results =
            let logger = loggerFactory.CreateLogger("Rename files")
            use renameFiles = new Progress(io, "Rename files")

            analyzedRenames
            |> List.map (fun toRename ->
                asyncResult {
                    match executeMode with
                    | DryRun -> output.Message $"  ├────> Rename image <c:cyan>{toRename.Original.Name |> FileName.value}</c> to <c:yellow>{toRename.Renamed.Name |> FileName.value}</c>"
                    | Execute -> do! toRename |> File.replace
                }
                <@> RuntimeError
                |> AsyncResult.tee renameFiles.Advance
                |> AsyncResult.teeError ((fun e -> logger.LogError("Rename image {image} failed with {error}", toRename, e)) >> renameFiles.Advance)
            )
            |> tee (List.length >> sprintf "  ├──> <c:yellow>Renaming images</c>[<c:magenta>%i</c>] <c:yellow>in parallel</c> ..." >> output.Message)
            |> tee (List.length >> renameFiles.Start)
            |> AsyncResult.handleMultipleResultsBy (output.IsDebug() || executeMode = DryRun) RuntimeError

        results |> Seq.length |> sprintf "  └──> Renaming images[<c:magenta>%i</c>] finished." |> output.Message

        return "Done"
    }

    let execute ((input, output): IO) =
        asyncResult {
            let executeMode =
                match input with
                | Input.IsSetOption "dry-run" _ -> DryRun
                | _ -> Execute

            let target = input |> Input.getArgumentValue "target"

            use loggerFactory =
                if output.IsDebug() then "vvv"
                elif output.IsVeryVerbose() then "vv"
                else "v"
                |> LogLevel.parse
                |> LoggerFactory.create "RenameByMeta"

            return! target |> run (input, output) loggerFactory executeMode
        }
        |> AsyncResult.waitAfterFinish output 2000
        |> Async.RunSynchronously
        |> function
            | Ok msg ->
                output.Success msg
                ExitCode.Success
            | Error errors ->
                errors
                |> List.map RenameError.format
                |> Errors.show output

                ExitCode.Error
