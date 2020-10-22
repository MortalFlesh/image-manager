namespace MF.ImageManager.Command

open System.IO
open MF.ConsoleApplication
open MF.ImageManager

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
    ]

    let execute ((input, output): IO) =
        let source = input |> Input.getOptionValueAsList "source"
        let target = input |> Input.getArgumentValue "target"
        let exclude =
            match input with
            | Input.HasOption "exclude" _ ->
                input |> Input.getOptionValueAsList "exclude"
            | _ -> []
        let excludeList =
            match input with
            | Input.HasOption "exclude-list" _ ->
                input |> Input.getOptionValueAsString "exclude-list"
            | _ -> None

        if output.IsVerbose() then
            output.Table ["Source"; "Target"; "Exclude"; "Exclude list from"]
                [[source; [target]; exclude; excludeList |> Option.toList] |> List.map (sprintf "%A")]

        output.Section <| sprintf "Prepare images to %s" target
        output.Message <| sprintf "From:\n - %s" (source |> String.concat "\n - ")

        {
            Source = source
            Target = target
            TargetDirMode =
                match input with
                | Input.IsSetOption "force" _ -> Override
                | Input.IsSetOption "dry-run" _ -> DryRun
                | _ -> Exclude
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
        }
        |> Prepare.prepareForSorting output
        |> function
            | Ok message ->
                output.Success message
                ExitCode.Success
            | Error message ->
                output.Error message
                ExitCode.Error
