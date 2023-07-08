// Learn more about F# at http://fsharp.org

open System
open MF.ConsoleApplication
open MF.ImageManager

[<RequireQualifiedAccess>]
module ImageManager =
    let app = consoleApplication {
        title AssemblyVersionInformation.AssemblyProduct
        name AssemblyVersionInformation.AssemblyProduct
        description AssemblyVersionInformation.AssemblyDescription
        info ApplicationInfo.MainTitle
        version AssemblyVersionInformation.AssemblyFileVersion

        gitBranch AssemblyVersionInformation.AssemblyMetadata_gitbranch
        gitCommit AssemblyVersionInformation.AssemblyMetadata_gitcommit

        command "prepare" {
            Description = "Prepares images for sorting"
            Help = Command.PrepareCommand.help
            Arguments = Command.PrepareCommand.arguments
            Options = Command.PrepareCommand.options
            Initialize = None
            Interact = None
            Execute = Command.PrepareCommand.execute
        }

        command "meta:stats" {
            Description = "Show stats for metadata in directory"
            Help = None
            Arguments = Command.MetaStatsCommand.arguments
            Options = Command.MetaStatsCommand.options
            Initialize = None
            Interact = None
            Execute = Command.MetaStatsCommand.execute
        }

        command "image:rename:meta" {
            Description = "Renames images in given dir by a pattern based on image metadata."
            Help = None
            Arguments = Command.RenameImageByMeta.arguments
            Options = Command.RenameImageByMeta.options
            Initialize = None
            Interact = None
            Execute = Command.RenameImageByMeta.execute
        }

        command "image:find:same" {
            Description = "Find same images by different size, etc."
            Help = None
            Arguments = Command.FindSameImages.arguments
            Options = Command.FindSameImages.options
            Initialize = None
            Interact = None
            Execute = Command.FindSameImages.execute
        }

        command "cache:preload" {
            Description = "Preloads a local cache with hashes for files, you can not or do not want to rename."
            Help = None
            Arguments = Command.CachePreload.arguments
            Options = Command.CachePreload.options
            Initialize = None
            Interact = None
            Execute = Command.CachePreload.execute
        }

        command "cache:clear" {
            Description = "Clears a local cache with hashes for files."
            Help = None
            Arguments = Command.CacheClear.arguments
            Options = Command.CacheClear.options
            Initialize = None
            Interact = None
            Execute = Command.CacheClear.execute
        }

        (* command "test" {
            Description = "Tests a console output"
            Help = None
            Arguments = []
            Options = []
            Initialize = None
            Interact = None
            Execute = fun (input, output) ->

                [ 0 .. 100 ]
                |> List.map (fun i -> async {
                    do! Async.Sleep 300

                    let color =
                        if i <= 25 then "red"
                        elif i <= 50 then "dark-yellow"
                        elif i <= 75 then "yellow"
                        else "green"

                    sprintf "\r<c:gray>[Test]</c> <c:%s>%i</c>%%" color i |> MF.ConsoleStyle.Console.write
                    //Console.Write(sprintf "\r [Test] %i%%" i)
                })
                |> Async.Sequential
                |> Async.RunSynchronously
                |> ignore
                |> Console.WriteLine

                ExitCode.Success
        } *)
    }

[<EntryPoint>]
let main argv =
    ImageManager.app
    |> run argv
