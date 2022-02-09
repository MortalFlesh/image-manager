namespace MF.ImageManager.Command

open System
open System.IO
open Microsoft.Extensions.Logging
open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ImageManager
open MF.ImageManager.Prepare
open MF.Utils
open MF.Utils.Logging
open MF.Utils.CommandHelp

[<RequireQualifiedAccess>]
module PrepareCommand =
    let help = commandHelp [
        "The <c:dark-green>{{command.name}}</c> prepares files to manually handle:"
        "        <c:dark-green>dotnet {{command.full_name}}</c> <c:dark-yellow>path-to-target-dir/</c>"
        ""
        "The <c:dark-green>{{command.name}}</c> prepares files from a month (<c:cyan>april</c>) in this current year:"
        "        <c:dark-green>dotnet {{command.full_name}}</c> <c:dark-yellow>path-to-target-dir/</c> <c:dark-yellow>-s source-dir-path</c> <c:dark-yellow>--only-month</c> <c:cyan>4</c>"
    ]

    let arguments = [
        Argument.required "target" "Directory you want to copy files to."
    ]

    let options = [
        Option.requiredArray "source" (Some "s") "Directory you want to search files." (Some [])
        Option.optionalArray "exclude" (Some "e") "Directories you want to exclude from searching." None
        Option.optional "exclude-list" (Some "x") "Text file contains a list of files you want to exclude from searching (<c:yellow>one file at line</c>)." None
        Option.noValue "force" (Some "f") "If set, target directory will NOT be excluded, and files may be overwritten."
        Option.noValue "dry-run" None "If set, target directory will NOT be touched in anyway and files will only be sent to stdout."
        Option.noValue "year" None "If set, target directory will have a sub-directory with year of the file created date."
        Option.noValue "month" None "If set, target directory will have a sub-directory with month of the file created date."
        Option.optional "fallback" None "If set, it will be used as a sub-directory in target directory for all files, which don't have other specific sub-directory." None
        Option.optional "config" (Some "c") "If set, config file will be used (other options set directly, will override a config values)." None
        Option.optional "only-month" None "If set, only photos from this month will be used for a checking. Format is <c:yellow>MM</c> or <c:yellow>YYYY-MM</c>." None
        Option.noValue "only-current-month" None "If set, only photos from the current month will be used for a checking."
        Option.noValue "only-previous-month" None "If set, only photos from the previous month will be used for a checking."
        Option.noValue "use-cache" None "Use cache for file hashes (it must be preloaded by <c:cyan>cache:preload</c> command)."
        Progress.noProgressOption
    ]

    let execute ((input, output): IO) =
        use loggerFactory =
            if output.IsDebug() then "vvv"
            elif output.IsVeryVerbose() then "vv"
            else "v"
            |> LogLevel.parse
            |> LoggerFactory.create "PrepareImages"

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

            let targetDirMode =
                match input with
                | Input.IsSetOption "force" _ -> Override
                | Input.IsSetOption "dry-run" _ -> DryRun
                | _ -> Exclude

            match input with
            | Input.IsSetOption "use-cache" _ ->
                do! Hash.Cache.load loggerFactory
                    |> AsyncResult.mapError (PrepareError >> List.singleton)
                output.Success "Note: Cache for hashes is loaded."
            | _ -> ()

            let onlyMonth =
                match input with
                | Input.HasOption "only-month" _ ->
                    match input |> Input.getOptionValueAsString "only-month" with
                    | Some (Regex @"^(\d{4})\-(\d{2})$" [ year; month ]) -> Some { Year = int year; Month = int month }
                    | Some (Regex @"^(\d{4})\-(\d{1})$" [ year; month ]) -> Some { Year = int year; Month = int month }
                    | Some (Regex @"^(\d{2})$" [ month ]) -> Some { Year = DateTime.Now.Year; Month = int month }
                    | Some (Regex @"^(\d{1})$" [ month ]) -> Some { Year = DateTime.Now.Year; Month = int month }

                    | _ -> None
                | Input.HasOption "only-current-month" _ -> Some { Year = DateTime.Now.Year; Month = DateTime.Now.Month }
                | Input.HasOption "only-previous-month" _ ->
                    let now = DateTime.Now
                    let currentMonth = new DateTime(now.Year, now.Month, 1)
                    let previousMonth = currentMonth.AddMonths(-1)

                    Some { Year = previousMonth.Year; Month = previousMonth.Month }
                | _ -> None

            let! parsedConfig =
                match input with
                | Input.HasOption "config" (OptionValue.ValueOptional (Some config)) ->
                    config
                    |> File.ReadAllText
                    |> Config.parse targetDirMode
                    |> Result.map Some
                | _ -> Ok None
                |> AsyncResult.ofResult
                |> AsyncResult.mapError (PrepareError >> List.singleton)

            let config =
                parsedConfig
                |> Config.combine ({
                    Source = source
                    Target = target
                    OnlyMonth = onlyMonth
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

            if output.IsDebug() then
                output.Message <| sprintf "Config:\n%A\n" config
            elif output.IsVerbose() then
                output.Table ["Source"; "Target"; "Exclude"; "Exclude list from"; "FFMpeg"] [
                    [
                        config.Source
                        [config.Target]
                        [config.Exclude |> sprintf "%A"]
                        config.ExcludeList |> Option.toList
                        [config.Ffmpeg |> sprintf "%A"]
                    ]
                    |> List.map (sprintf "%A")
                ]

            output.Section <| sprintf "Prepare files to %s" config.Target
            output.Message <| sprintf "From:\n - %s" (config.Source |> String.concat "\n - ")

            return!
                config
                |> Prepare.prepareForSorting (input, output) loggerFactory
        }
        |> AsyncResult.waitAfterFinish output 2000
        |> Async.RunSynchronously
        |> function
            | Ok message ->
                output.Success message
                ExitCode.Success
            | Error errors ->
                let logger = loggerFactory.CreateLogger("Prepare Images Command")

                errors
                |> List.map (PrepareFilesError.format >> tee logger.LogError)
                |> Errors.show output

                ExitCode.Error
