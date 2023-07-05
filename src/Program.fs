// Learn more about F# at http://fsharp.org

open System
open MF.ConsoleApplication
open MF.ImageManager

[<EntryPoint>]
let main argv =
    consoleApplication {
        title AssemblyVersionInformation.AssemblyProduct
        name AssemblyVersionInformation.AssemblyProduct
        description AssemblyVersionInformation.AssemblyDescription
        info ApplicationInfo.MainTitle
        version AssemblyVersionInformation.AssemblyFileVersion

        gitBranch AssemblyVersionInformation.AssemblyMetadata_gitbranch
        gitCommit AssemblyVersionInformation.AssemblyMetadata_gitcommit

        command "prepare" {
            Description = "Prepares images for sorting"
            Help = None
            Arguments = Command.PrepareCommand.arguments
            Options = Command.PrepareCommand.options
            Initialize = None
            Interact = None
            Execute = Command.PrepareCommand.execute
        }
    }
    |> run argv
