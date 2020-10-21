namespace MF.ImageManager.Command

open MF.ConsoleApplication
open MF.ImageManager

[<RequireQualifiedAccess>]
module PrepareCommand =
    let arguments = [
        Argument.required "target" "Directory you want to copy files to."
    ]

    let options = [
        Option.requiredArray "source" (Some "s") "Directory you want to search files." (Some [])
        Option.requiredArray "exclude" (Some "e") "Directories you want to exclude from searching." None
        Option.noValue "force" (Some "f") "If set, target directory will NOT be excluded, and images may be overwritten."
    ]

    let execute ((input, output): IO) =
        let source = input |> Input.getOptionValueAsList "source"
        let target = input |> Input.getArgumentValue "target"
        let exclude = input |> Input.getOptionValueAsList "exclude"

        if output.IsVerbose() then
            output.Table ["Source"; "Target"; "Exclude"] [[source; [target]; exclude] |> List.map (sprintf "%A")]

        output.Section (
            sprintf "Prepare to %s from:\n - %s"
                target
                (source |> String.concat "\n - ")
        )

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
