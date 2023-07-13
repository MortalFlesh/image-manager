module MF.ImageManager.TestUtils

open Expecto

open System
open System.IO
open MF.ErrorHandling
open MF.Utils

[<AutoOpen>]
module Utils =
    let okOrFail = function
        | Ok ok -> ok
        | Error error -> failtestf "Fail on %A" error

    let runOrFail xA = xA |> Async.RunSynchronously |> okOrFail

[<RequireQualifiedAccess>]
module FileSystem =
    let inline (</>) a b = Path.Combine(a, b)

    let private testDir (testName: string) =
        testName
            .Replace(" ", "_")
            .Replace("(", "")
            .Replace(")", "")
            .Replace(",", "")
            .Replace("/", "")
            .Replace(@"\", "")

    let private scenarioDir testName = "scenarios" </> (testName |> testDir)

    /// Delete previous scenario dir and create new one at scenarios/{testName}
    let prepareTargetDir (testName: string) =
        let targetDir = scenarioDir testName
        if Directory.Exists(targetDir) then Directory.Delete(targetDir, true)
        Directory.ensure(targetDir)
        targetDir

    /// Copy files from fixtures/{dir} to scenarios/{testName}/...
    let prepareFromFixtures (testName: string) dir =
        let debug = false
        let sourceDir = "Fixtures" </> dir
        let targetDir = prepareTargetDir testName

        if debug then printfn "Source dir %s; Target dir: %s" sourceDir targetDir

        Directory.EnumerateFiles(sourceDir, "*", SearchOption.AllDirectories)
        |> Seq.iter (fun file ->
            if debug then printfn "File: %s" file
            let filePath = file.Replace(sourceDir, "").TrimStart(Path.DirectorySeparatorChar)
            let outputPath = targetDir </> filePath

            outputPath
            |> Path.GetDirectoryName
            |> Directory.ensure

            if debug then printfn " -> Copying %s to %s" file outputPath
            File.Copy(file, outputPath)
        )

        targetDir

    let rec tree testName dir =
        let targetDir = scenarioDir testName
        [
            yield! dir |> Directory.EnumerateFiles
            yield! dir |> Directory.EnumerateDirectories |> Seq.collect (tree testName)
        ]
        |> List.filter (fun f -> not (f.Contains(".DS_Store")))
        |> List.sort
        |> List.map (fun f ->
            f
                .Replace(targetDir, "")
                .Replace(Path.DirectorySeparatorChar, '/')
                .TrimStart('/')
        )

[<RequireQualifiedAccess>]
module ImageManager =
    open MF.ConsoleApplication

    let run argv =
        let argv = argv

        Program.ImageManager.app
        |> runResult argv
