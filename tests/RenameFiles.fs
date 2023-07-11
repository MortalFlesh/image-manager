module MF.ImageManager.RenameTests

open Expecto
open System
open System.IO

open MF.ConsoleApplication
open MF.ImageManager
open MF.ImageManager.TestUtils

open MF.Utils
open MF.ErrorHandling

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
    {
        Description = "Rename files, in the wrong month dir but keep others in a correct or manual directories"
        InputDir = "partially-renamed-and-sorted"
        Expected = [
            "2022/01/i_20221023T161201_97cfd418.heic"
            "2022/01/i_20221103T104402_dbaddf33.jpeg"
            "2022/04/i_20220401T141648_9afe0fba.jpeg"
            "2022/manual/i_20220416T105242_4ac39dd7.jpeg"
            "2022/manual/i_20221004T162658_b05b7f6b.jpeg"
        ]
        ExtraArgs = []
    }
    {
        Description = "Rename and re-hash files, in the wrong month dir but keep others in a correct or manual directories"
        InputDir = "partially-renamed-and-sorted"
        Expected = [
            "2022/04/i_20220401T141648_9afe0fba.jpeg"
            "2022/10/i_20221023T161201_97cfd418.heic"
            "2022/11/i_20221103T104402_dbaddf33.jpeg"
            "2022/manual/i_20220416T105242_4ac39dd7.jpeg"
            "2022/manual/i_20221004T162658_b05b7f6b.jpeg"
        ]
        ExtraArgs = [
            "--" + CommonOptions.ReHashAgain
        ]
    }
    {
        Description = "Rename and re-hash files, in the wrong month dir but recreate even manual directories"
        InputDir = "partially-renamed-and-sorted"
        Expected = [
            "2022/04/i_20220401T141648_9afe0fba.jpeg"
            "2022/04/i_20220416T105242_4ac39dd7.jpeg"
            "2022/10/i_20221004T162658_b05b7f6b.jpeg"
            "2022/10/i_20221023T161201_97cfd418.heic"
            "2022/11/i_20221103T104402_dbaddf33.jpeg"
        ]
        ExtraArgs = [
            "--" + CommonOptions.ReHashAgain
            "--" + CommonOptions.EvenCreateSubDir
            "--" + CommonOptions.RootDir + "={TARGET_DIR}"
        ]
    }
]

[<Tests>]
let renameFilesTest =
    testList "MF.ImageManager - rename files" [
        yield!
            provider
            |> List.map (fun ({ Description = desc; InputDir = dir; Expected = expected; ExtraArgs = extraArgs }) ->
                testCase desc <| fun _ ->
                    async {
                        let targetDir = dir |> FileSystem.prepareFromFixtures desc
                        do! Async.Sleep 200

                        let res =
                            [|
                                "image:rename:meta"
                                targetDir
                                "--quiet"

                                yield! extraArgs |> List.map (fun s -> s.Replace("{TARGET_DIR}", targetDir))
                            |]
                            |> ImageManager.run
                            |> okOrFail

                        Expect.equal res ExitCode.Success "Should be success"
                        do! Async.Sleep 200

                        let actual = FileSystem.tree desc targetDir
                        Expect.equal actual expected desc
                    }
                    |> Async.RunSynchronously
                )
    ]
