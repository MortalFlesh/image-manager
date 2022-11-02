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

    let options = CommonOptions.all @ [
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
    ]

    let execute = ExecuteAsyncResult <| fun (input, output) ->
        asyncResult {
            use loggerFactory =
                if output.IsDebug() then "vvv"
                elif output.IsVeryVerbose() then "vv"
                else "v"
                |> LogLevel.parse
                |> LoggerFactory.create "PrepareImages"

            let source = input |> Input.Option.asList "source"
            let target = input |> Input.Argument.value "target"
            let exclude =
                match input with
                | Input.Option.Has "exclude" _ -> input |> Input.Option.asList "exclude"
                | _ -> []
            let excludeList =
                match input with
                | Input.Option.Has "exclude-list" _ -> input |> Input.Option.asString "exclude-list"
                | _ -> None

            let targetDirMode =
                match input with
                | Input.Option.IsSet "force" _ -> Override
                | Input.Option.IsSet "dry-run" _ -> DryRun
                | _ -> Exclude

            match input with
            | Input.Option.IsSet "use-cache" _ ->
                do! Hash.Cache.load loggerFactory
                    |> AsyncResult.mapError (PrepareError >> List.singleton)
                output.Success "Note: Cache for hashes is loaded."
            | _ -> ()

            let onlyMonth =
                match input with
                | Input.Option.Has "only-month" _ ->
                    match input |> Input.Option.asString "only-month" with
                    | Some (Regex @"^(\d{4})\-(\d{2})$" [ year; month ]) -> Some { Year = int year; Month = int month }
                    | Some (Regex @"^(\d{4})\-(\d{1})$" [ year; month ]) -> Some { Year = int year; Month = int month }
                    | Some (Regex @"^(\d{2})$" [ month ]) -> Some { Year = DateTime.Now.Year; Month = int month }
                    | Some (Regex @"^(\d{1})$" [ month ]) -> Some { Year = DateTime.Now.Year; Month = int month }

                    | _ -> None
                | Input.Option.Has "only-current-month" _ -> Some { Year = DateTime.Now.Year; Month = DateTime.Now.Month }
                | Input.Option.Has "only-previous-month" _ ->
                    let now = DateTime.Now
                    let currentMonth = new DateTime(now.Year, now.Month, 1)
                    let previousMonth = currentMonth.AddMonths(-1)

                    Some { Year = previousMonth.Year; Month = previousMonth.Month }
                | _ -> None

            let! parsedConfig =
                match input with
                | Input.Option.Has "config" (OptionValue.ValueOptional (Some config)) ->
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
                        | Input.Option.IsSet "year" _, Input.Option.IsSet "month" _ -> ByYearAndMonth
                        | Input.Option.IsSet "year" _, _ -> ByYear
                        | Input.Option.IsSet "month" _, _ -> ByMonth
                        | _ -> Flat
                    TargetSubdirFallback =
                        match input with
                        | Input.Option.Has "fallback" fallback -> fallback |> OptionValue.stringValue
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
                output.Table ["Source"; "Target"; "Exclude"; "Exclude list from"; CommonOptions.FFMpeg] [
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

            let! message =
                config
                |> Prepare.prepareForSorting (input, output) loggerFactory

            output.Success message

            return ExitCode.Success
        }
        |> AsyncResult.waitAfterFinish output 2000
        |> AsyncResult.mapError (Errors.map "Prepare Images Command" output PrepareFilesError.format)
