namespace MF.ImageManager.Command

open System.IO
open ErrorHandling
open MF.ImageManager
open MF.ConsoleApplication

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
        Option.noValue "year" None "If set, target directory will have a sub-directory with year of the image created date."
        Option.noValue "month" None "If set, target directory will have a sub-directory with month of the image created date."
        Option.optional "fallback" None "If set, it will be used as a sub-directory in target directory for all files, which don't have other specific sub-directory." None
        Option.optional "config" (Some "c") "If set, config file will be used (other options set directly, will override a config values)." None
    ]

    let execute = ExecuteResult <| fun (input, output) -> result {
        let source = input |> Input.Option.asList "source"
        let target = input |> Input.Argument.value "target"
        let exclude =
            match input with
            | Input.Option.Has "exclude" _ ->
                input |> Input.Option.asList "exclude"
            | _ -> []
        let excludeList =
            match input with
            | Input.Option.Has "exclude-list" _ ->
                input |> Input.Option.asString "exclude-list"
            | _ -> None

        let config =
            let targetDirMode =
                match input with
                | Input.Option.IsSet "force" _ -> Override
                | Input.Option.IsSet "dry-run" _ -> DryRun
                | _ -> Exclude

            match input with
            | Input.Option.Has "config" (OptionValue.ValueOptional (Some config)) ->
                config
                |> File.ReadAllText
                |> PrepareForSorting.parse targetDirMode
                |> Some
            | _ -> None
            |> PrepareForSorting.combine {
                Source = source
                Target = target
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
                FfmpegPath = None   // todo - for now, it is only possible to set it by config file
            }

        if output.IsVerbose() then
            output.Table ["Source"; "Target"; "Exclude"; "Exclude list from"]
                [[source; [target]; exclude; excludeList |> Option.toList] |> List.map (sprintf "%A")]

        output.Section <| sprintf "Prepare images to %s" target
        output.Message <| sprintf "From:\n - %s" (source |> String.concat "\n - ")

        let! message =
            config
            |> Prepare.prepareForSorting output

        output.Success message
        (* |> function
            | Ok message ->
                output.Success message
                ExitCode.Success
            | Error message ->
                output.Error message
                ExitCode.Error *)

        return ExitCode.Success
    }
