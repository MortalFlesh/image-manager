namespace MF.ImageManager.Command

open System.IO
open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ImageManager
open MF.ImageManager.Prepare
open MF.Utils
open MF.Utils.Logging

[<RequireQualifiedAccess>]
module PrepareCommand =
    let arguments = [
        Argument.required "target" "Directory you want to copy files to."
    ]

    let options = [
        Option.requiredArray "source" (Some "s") "Directory you want to search files." (Some [])
        Option.optionalArray "exclude" (Some "e") "Directories you want to exclude from searching." None
        Option.optional "exclude-list" (Some "x") "Text file contains a list of files you want to exclude from searching (<c:yellow>one file at line</c>)." None
        Option.noValue "force" (Some "f") "If set, target directory will NOT be excluded, and images may be overwritten."
        Option.noValue "dry-run" None "If set, target directory will NOT be touched in anyway and images will only be sent to stdout."
        Option.optional "prefix" None "Prefix all images with this static prefix, to allow the same image name from other sources." None
        Option.noValue "year" None "If set, target directory will have a sub-directory with year of the image created date."
        Option.noValue "month" None "If set, target directory will have a sub-directory with month of the image created date."
        Option.optional "fallback" None "If set, it will be used as a sub-directory in target directory for all files, which don't have other specific sub-directory." None
        Option.optional "config" (Some "c") "If set, config file will be used (other options set directly, will override a config values)." None
    ]

    let execute ((input, output): IO) =
        asyncResult {
            let source = input |> Input.getOptionValueAsList "source"
            let target = input |> Input.getArgumentValue "target"
            let exclude =
                match input with
                | Input.HasOption "exclude" _ -> input |> Input.getOptionValueAsList "exclude"
                | _ -> []
            let excludeList =
                match input with
                | Input.HasOption "exclude-list" _ -> input |> Input.getOptionValueAsString "exclude-list"
                | _ -> None
            let prefix =
                match input with
                | Input.HasOption "prefix" (OptionValue.ValueOptional (Some value)) -> Some (Prefix value)
                | _ -> None

            let targetDirMode =
                match input with
                | Input.IsSetOption "force" _ -> Override
                | Input.IsSetOption "dry-run" _ -> DryRun
                | _ -> Exclude

            let! parsedConfig =
                match input with
                | Input.HasOption "config" (OptionValue.ValueOptional (Some config)) ->
                    config
                    |> File.ReadAllText
                    |> Config.parse targetDirMode
                    |> Result.map Some
                | _ -> Ok None
                |> AsyncResult.ofResult
                |> AsyncResult.mapError List.singleton

            let config =
                parsedConfig
                |> Config.combine ({
                    Source = source
                    Prefix = prefix
                    Target = target
                    TargetDirMode = targetDirMode
                    TargetSubdir =
                        match input, input with
                        | Input.IsSetOption "year" _, Input.IsSetOption "month" _ -> ByYearAndMonth
                        | Input.IsSetOption "year" _, _ -> ByYear
                        | Input.IsSetOption "month" _, _ -> ByMonth
                        | _ -> Flat
                    TargetSubdirFallback =
                        match input with
                        | Input.HasOption "fallback" fallback -> fallback |> OptionValue.stringValue
                        | _ -> None
                    Exclude =
                        match exclude with
                        | [] -> None
                        | excludeDirs -> Some excludeDirs
                    ExcludeList =
                        match excludeList with
                        | None -> None
                        | Some excludeList ->
                            if excludeList |> File.Exists |> not then
                                failwithf "File %A you want to exclude from, does not exists." excludeList
                            Some excludeList
                    Ffmpeg = FFMpeg.Empty
                })

            if output.IsVerbose() then
                output.Table ["Source"; "Prefix"; "Target"; "Exclude"; "Exclude list from"; "FFMpeg"]
                    [
                        [
                            config.Source
                            [config.Prefix |> sprintf "%A"]
                            [config.Target]
                            [config.Exclude |> sprintf "%A"]
                            config.ExcludeList |> Option.toList
                            [config.Ffmpeg |> sprintf "%A"]
                        ]
                        |> List.map (sprintf "%A")
                    ]

            output.Section <| sprintf "Prepare images to %s" config.Target
            output.Message <| sprintf "From:\n - %s" (config.Source |> String.concat "\n - ")

            use loggerFactory =
                if output.IsDebug() then "vvv"
                elif output.IsVeryVerbose() then "vv"
                else "v"
                |> LogLevel.parse
                |> LoggerFactory.create "PrepareImages"

            return!
                config
                |> Prepare.prepareForSorting output loggerFactory
        }
        |> Async.RunSynchronously
        |> function
            | Ok message ->
                output.Success message
                ExitCode.Success
            | Error errors ->
                errors
                |> List.iter (function
                    | PrepareError.Exception e -> output.Error e.Message
                    | PrepareError.ErrorMessage message -> output.Error message
                    | PrepareError.NotImageOrVideo path -> output.Error $"File {path} is not an image or a video."
                )

                ExitCode.Error
