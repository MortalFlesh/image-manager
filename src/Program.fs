// Learn more about F# at http://fsharp.org

open MF.ConsoleApplication
open MF.ImageManager

[<EntryPoint>]
let main argv =
    consoleApplication {
        title "Image Manager"
        info ApplicationInfo.MainTitle

        command "prepare" {
            Description = "Prepares images for sorting"
            Help = None
            Arguments = [
                Argument.required "source" "Directory you want to search files."
                Argument.required "target" "Directory you want to copy files to."
                Argument.optionalArray "exclude" "Directories you want to exclude from searching." None
            ]
            Options = []
            Initialize = None
            Interact = None
            Execute = fun (input, output) ->
                let source = input |> Input.getArgumentValue "source"
                let target = input |> Input.getArgumentValue "target"
                let exclude = input |> Input.getArgumentValueAsList "exclude"

                if output.IsVerbose() then
                    output.Table ["Source"; "Target"; "Exclude"] [[source; target; exclude |> sprintf "%A"]]

                output.Section (sprintf "Prepare from %s to %s:" source target)

                {
                    Source = source
                    Target = target
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
        }
    }
    |> run argv
