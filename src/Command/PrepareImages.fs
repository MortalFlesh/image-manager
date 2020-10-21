namespace MF.ImageManager.Command

open MF.ConsoleApplication
open MF.ImageManager

[<RequireQualifiedAccess>]
module PrepareCommand =
    let arguments = [
        Argument.required "source" "Directory you want to search files."
        Argument.required "target" "Directory you want to copy files to."
        Argument.optionalArray "exclude" "Directories you want to exclude from searching." None
    ]

    let options = [
        Option.noValue "force" (Some "f") "If set, target directory will NOT be excluded, and images may be overwritten."
    ]

    let execute ((input, output): IO) =
        let source = input |> Input.getArgumentValue "source"
        let target = input |> Input.getArgumentValue "target"
        let exclude = input |> Input.getArgumentValueAsList "exclude"

        if output.IsVerbose() then
            output.Table ["Source"; "Target"; "Exclude"] [[source; target; exclude |> sprintf "%A"]]

        output.Section (sprintf "Prepare from %s to %s:" source target)

        {
            Source = source
            Target = target
            TargetDirMode =
                match input with
                | Input.IsSetOption "force" _ -> Override
                | _ -> Exclude
            Exclude =
                match exclude with
                | [] -> None
                | excludeDirs -> Some excludeDirs
        }
        |> Prepare.prepareForSorting output
        |> function
            | Ok message ->
                output.Success message
                ExitCode.Success
            | Error message ->
                output.Error message
                ExitCode.Error
