module MF.ImageManager.PreloadCache

open Expecto
open System
open System.IO

open MF.ConsoleApplication
open MF.ImageManager
open MF.ImageManager.TestUtils

open MF.Utils
open MF.ErrorHandling
open FileSystem.Operators

type Scenario = {
    Description: string
    InputDir: string
    Expected: string list
    ExtraArgs: string list
}

let provider = [
    {
        Description = "Simple rename heic and jpeg files"
        InputDir = "iCloudPhotos"
        Expected = [
            "/iCloudPhotos/heic-300x200.heic:i_0.heic"
            "/iCloudPhotos/img_2022_04-1 copy.jpeg:i_20220401T141648_9afe0fba.jpeg"
            "/iCloudPhotos/img_2022_04-1.jpeg:i_20220401T141648_9afe0fba.jpeg"
            "/iCloudPhotos/img_2022_04-2.jpeg:i_20220416T105242_4ac39dd7.jpeg"
            "/iCloudPhotos/puvodne-heic-spravne-jpeg.jpeg:i_20221103T104402_dbaddf33.jpeg"
            "/iCloudPhotos/spatne-heic-spravne-jpeg.heic:i_20221103T104402_dbaddf33.jpeg"
            "/iCloudPhotos/spravne-heic-a-jako-jpeg-nefunguje.jpeg:i_20221023T161201_97cfd418.heic"
            "/iCloudPhotos/spravne-heic.heic:i_20221023T161201_97cfd418.heic"
        ]
        ExtraArgs = []
    }
]

let fixBasePath (dir: string) (path: string) =
    let basePath = dir |> Path.GetFullPath |> Path.GetDirectoryName
    path.Replace(basePath, "")

[<Tests>]
let renameFilesTest =
    testList "MF.ImageManager - preload cache" [
        yield!
            provider
            |> List.map (fun ({ Description = desc; InputDir = dir; Expected = expected; ExtraArgs = extraArgs }) ->
                testCase desc <| fun _ ->
                    async {
                        let targetDir = "Fixtures" </> dir
                        do! Async.Sleep 200

                        let _ =
                            [|
                                "cache:clear"
                                "--quiet"
                            |]
                            |> ImageManager.run
                            |> okOrFail
                        do! Async.Sleep 200

                        let res =
                            [|
                                "cache:preload"
                                targetDir
                                // "--quiet"

                                yield! extraArgs
                            |]
                            |> ImageManager.run
                            |> okOrFail

                        Expect.equal res ExitCode.Success "Should be success"
                        do! Async.Sleep 200

                        let actual =
                            File.ReadAllLines ("." </> ".hash-cache.txt")
                            |> Seq.toList
                            |> List.map (fixBasePath targetDir)
                        Expect.equal actual expected desc
                    }
                    |> Async.RunSynchronously
                )
    ]
