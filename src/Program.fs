// Learn more about F# at http://fsharp.org

open System
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
            Arguments = Command.PrepareCommand.arguments
            Options = Command.PrepareCommand.options
            Initialize = None
            Interact = None
            Execute = Command.PrepareCommand.execute
        }

        command "about" {
            Description = "Displays information about the current project."
            Help = None
            Arguments = []
            Options = []
            Initialize = None
            Interact = None
            Execute = fun (_input, output) ->
                let ``---`` = [ "------------------"; "----------------------------------------------------------------------------------------------" ]

                output.Table [ AssemblyVersionInformation.AssemblyProduct ] [
                    [ "Description"; AssemblyVersionInformation.AssemblyDescription ]
                    [ "Version"; AssemblyVersionInformation.AssemblyVersion ]

                    ``---``
                    [ "Environment" ]
                    ``---``
                    [ ".NET Core"; Environment.Version |> sprintf "%A" ]
                    [ "Command Line"; Environment.CommandLine ]
                    [ "Current Directory"; Environment.CurrentDirectory ]
                    [ "Machine Name"; Environment.MachineName ]
                    [ "OS Version"; Environment.OSVersion |> sprintf "%A" ]
                    [ "Processor Count"; Environment.ProcessorCount |> sprintf "%A" ]

                    ``---``
                    [ "Git" ]
                    ``---``
                    [ "Branch"; AssemblyVersionInformation.AssemblyMetadata_gitbranch ]
                    [ "Commit"; AssemblyVersionInformation.AssemblyMetadata_gitcommit ]
                ]

                ExitCode.Success
        }
    }
    |> run argv
