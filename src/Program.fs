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
            Arguments = Command.PrepareCommand.arguments
            Options = Command.PrepareCommand.options
            Initialize = None
            Interact = None
            Execute = Command.PrepareCommand.execute
        }
    }
    |> run argv
