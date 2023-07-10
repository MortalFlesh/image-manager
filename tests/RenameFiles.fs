module MF.ImageManager.RenameTests

open Expecto
open System
open System.IO

open MF.ConsoleApplication
open MF.ImageManager
open MF.ImageManager.TestUtils

open MF.Utils
open MF.ErrorHandling

let okOrFail = function
    | Ok ok -> ok
    | Error error -> failtestf "Fail on %A" error

let runOrFail xA = xA |> Async.RunSynchronously |> okOrFail

type Scenario = {
    Description: string
    InputDir: string
    Expected: string list
    ExtraArgs: string list
}

let provider = [
    {
        Description = "Simple rename heic and jpeg files"
        InputDir = "heic"
        Expected = [
            "i_0.heic"
            "i_20221023T161201_97cfd418.heic"
            "i_20221103T104402_dbaddf33.jpeg"
        ]
        ExtraArgs = []
    }
    {
        Description = "Simple rename heic and jpeg files - also create subdirs"
        InputDir = "heic"
        Expected = [
            "2022/10/i_20221023T161201_97cfd418.heic"
            "2022/11/i_20221103T104402_dbaddf33.jpeg"
            "i_0.heic"
        ]
        ExtraArgs = [
            "--" + CommonOptions.EvenCreateSubDir
        ]
    }
    {
        Description = "Rename and re-hash heic and jpeg files, in the wrong month dir"
        InputDir = "heic-renamed-in-wrong-month"
        Expected = [
            "2022/10/i_20221023T161201_97cfd418.heic"
            "2022/11/i_20221103T104402_dbaddf33.jpeg"
        ]
        ExtraArgs = [
            "--" + CommonOptions.ReHashAgain
        ]
    }
]

[<Tests>]
let renameFilesTest =
    testList "MF.ImageManager - rename files" [
        yield!
            provider
            |> List.map (fun ({ Description = desc; InputDir = dir; Expected = expected; ExtraArgs = extraArgs }) ->
                testCase $"heic correctly - {desc}" <| fun _ ->
                    async {
                        let targetDir = dir |> FileSystem.prepareFromFixtures desc
                        do! Async.Sleep 200

                        let res =
                            [|
                                "image:rename:meta"
                                targetDir
                                "--quiet"

                                yield! extraArgs
                            |]
                            |> ImageManager.run
                            |> okOrFail

                        Expect.equal res ExitCode.Success "Should be success"
                        do! Async.Sleep 200

                        let actual = FileSystem.tree desc targetDir
                        Expect.equal actual expected "Desc"
                    }
                    |> Async.RunSynchronously
                )
    ]
