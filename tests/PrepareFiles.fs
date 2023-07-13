module MF.ImageManager.PrepareTests

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
    InputDir: string option
    Expected: string list
    ExtraArgs: string list
}

let config name = sprintf "--config=%s" ("Fixtures" </> name)

// NOTE: it could be nice to test a cache, but it is still in a specific file, which would be shared between tests
//      and it is not worth it to change it now. It could not be parallel.
//      It would need to preload a cache for every test, since it has full path as a key (which would be different for every environment)
let provider = [
    {
        Description = "Simple prepare"
        InputDir = None
        Expected = [
            "2022/04/i_20220401T141648_9afe0fba.jpeg"
            "2022/04/i_20220416T105242_4ac39dd7.jpeg"
            "2022/10/i_20221023T161201_97cfd418.heic"
            "2022/11/i_20221103T104402_dbaddf33.jpeg"
            "fallback/i_0.heic"
        ]
        ExtraArgs = [
            config ".simple-prepare.json"
        ]
    }
    {
        Description = "Simple prepare but with already prepared files"
        InputDir = Some "partially-renamed-and-sorted"
        Expected = [
            "2022/01/i_20221023T161201_97cfd418.heic"       // it was already there, so it is ignored to copy, even if it was previously in the bad month dir
            "2022/01/i_20221103T104402_dbaddf33.jpeg"       // it was already there, so it is ignored to copy, even if it was previously in the bad month dir
            "2022/04/i_20220401T141648_9afe0fba.jpeg"
            "2022/04/i_20220416T105242_4ac39dd7.jpeg"
            "2022/04/img_2022_04-1.jpeg"                    // it was already there, so it is ignored by prepare
            "2022/manual/i_20221004T162658_b05b7f6b.jpeg"   // it was already there, so it is ignored by prepare
            "2022/manual/img_2022_04-2.jpeg"                // it was already there, so it is ignored by prepare
            "fallback/i_0.heic"
        ]
        ExtraArgs = [
            config ".simple-prepare.json"
        ]
    }
]

[<Tests>]
let prepareFilesTest =
    testList "MF.ImageManager - prepare files" [
        yield!
            provider
            |> List.map (fun ({ Description = desc; InputDir = inputDir; Expected = expected; ExtraArgs = extraArgs }) ->
                testCase desc <| fun _ ->
                    async {
                        let targetDir =
                            match inputDir with
                            | Some inputDir -> inputDir |> FileSystem.prepareFromFixtures desc
                            | _ -> FileSystem.prepareTargetDir desc

                        do! Async.Sleep 200

                        let res =
                            [|
                                "prepare"
                                targetDir
                                "--quiet"

                                yield! extraArgs
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
