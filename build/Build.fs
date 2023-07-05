// ========================================================================================================
// === F# / Console Application fake build ======================================================== 3.0.0 =
// --------------------------------------------------------------------------------------------------------
// Options:
//  - no-clean   - disables clean of dirs in the first step (required on CI)
//  - no-lint    - lint will be executed, but the result is not validated
// --------------------------------------------------------------------------------------------------------
// Table of contents:
//      1. Information about project, configuration
//      2. Utilities, DotnetCore functions
//      3. FAKE targets
//      4. FAKE targets hierarchy
// ========================================================================================================

// --------------------------------------------------------------------------------------------------------
// 1. Information about the project to be used at NuGet and in AssemblyInfo files and other FAKE configuration
// --------------------------------------------------------------------------------------------------------

open System
open System.IO

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Tools.Git
open Utils

// --------------------------------------------------------------------------------------------------------
// 1. Information about the project to be used at NuGet and in AssemblyInfo files and other FAKE configuration
// --------------------------------------------------------------------------------------------------------

let project = "Image Manager"
let summary = "Simple console app for managing images."

let changeLog = Some "CHANGELOG.md"
let gitCommit = Information.getCurrentSHA1(".")
let gitBranch = Information.getBranchName(".")

/// Runtime IDs: https://docs.microsoft.com/en-us/dotnet/core/rid-catalog#macos-rids
let runtimeIds =
    [
        "osx-x64"
        "win-x64"
    ]

[<RequireQualifiedAccess>]
module ProjectSources =
    let release =
        !! "./*.fsproj"
        ++ "src/*.fsproj"
        ++ "src/**/*.fsproj"

    let tests =
        !! "tests/*.fsproj"

    let all =
        release
        ++ "tests/*.fsproj"
        ++ "build/*.fsproj"

// --------------------------------------------------------------------------------------------------------
// 2. Targets for FAKE
// --------------------------------------------------------------------------------------------------------

let initTargets () =
    Target.initEnvironment ()

    Target.create "Clean" <| skipOn "no-clean" (fun _ ->
        !! "./**/bin/Release"
        ++ "./**/bin/Debug"
        ++ "./**/obj"
        ++ "./**/.ionide"
        -- "./build/**"
        |> Shell.cleanDirs
    )

    Target.create "AssemblyInfo" (fun _ ->
        let getAssemblyInfoAttributes projectName =
            let now = DateTime.Now

            let release =
                changeLog
                |> Option.bind (fun changeLog ->
                    try ReleaseNotes.parse (System.IO.File.ReadAllLines changeLog |> Seq.filter ((<>) "## Unreleased")) |> Some
                    with _ -> None
                )

            let gitValue initialValue =
                initialValue
                |> stringToOption
                |> Option.defaultValue "unknown"

            [
                AssemblyInfo.Title projectName
                AssemblyInfo.Product project
                AssemblyInfo.Description summary

                match release with
                | Some release ->
                    AssemblyInfo.Version release.AssemblyVersion
                    AssemblyInfo.FileVersion release.AssemblyVersion
                | _ ->
                    AssemblyInfo.Version "1.0"
                    AssemblyInfo.FileVersion "1.0"

                AssemblyInfo.InternalsVisibleTo "tests"
                AssemblyInfo.Metadata("gitbranch", gitBranch |> gitValue)
                AssemblyInfo.Metadata("gitcommit", gitCommit |> gitValue)
                AssemblyInfo.Metadata("createdAt", now.ToString("yyyy-MM-dd HH:mm:ss"))
            ]

        let getProjectDetails (projectPath: string) =
            let projectName = IO.Path.GetFileNameWithoutExtension(projectPath)
            (
                projectPath,
                projectName,
                IO.Path.GetDirectoryName(projectPath),
                (getAssemblyInfoAttributes projectName)
            )

        ProjectSources.all
        |> Seq.map getProjectDetails
        |> Seq.iter (fun (_, _, folderName, attributes) ->
            AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes
        )
    )

    Target.create "Build" (fun _ ->
        ProjectSources.all
        //|> Seq.iter (DotNet.build id)
        |> Seq.iter (Path.getDirectory >> Dotnet.runOrFail "build")
    )

    Target.create "Lint" <| skipOn "no-lint" (fun _ ->
        ProjectSources.all
        ++ "./Build.fsproj"
        |> Seq.iter (fun fsproj ->
            match Dotnet.runInRoot (sprintf "fsharplint lint %s" fsproj) with
            | Ok () -> Trace.tracefn "Lint %s is Ok" fsproj
            | Error e -> raise e
        )
    )

    Target.create "Tests" (fun _ ->
        if ProjectSources.tests |> Seq.isEmpty
        then Trace.tracefn "There are no tests yet."
        else Dotnet.runOrFail "run" "tests"
    )

    let zipRelease releaseDir =
        if releaseDir </> "zipCompiled" |> File.exists
        then
            let zipReleaseProcess = createProcess (releaseDir </> "zipCompiled")

            Trace.tracefn "\nZipping released files in %s ..." releaseDir
            run zipReleaseProcess "" "."
            |> Trace.tracefn "Zip result:\n%A\n"
        else
            Trace.tracefn "\nZip compiled files"
            runtimeIds
            |> List.iter (fun runtimeId ->
                Trace.tracefn " -> zipping %s ..." runtimeId
                let zipFile = sprintf "%s.zip" runtimeId
                IO.File.Delete zipFile
                Zip.zip releaseDir (releaseDir </> zipFile) !!(releaseDir </> runtimeId </> "*")
            )

    Target.create "Release" (fun _ ->
        let releaseDir = Path.getFullName "./dist"

        ProjectSources.release
        |> Seq.collect (fun project -> runtimeIds |> List.collect (fun runtimeId -> [project, runtimeId]))
        |> Seq.iter (fun (project, runtimeId) ->
            sprintf "publish -c Release /p:PublishSingleFile=true -o %s/%s --self-contained -r %s %s" releaseDir runtimeId runtimeId project
            |> Dotnet.runInRootOrFail
        )

        zipRelease releaseDir
    )

    Target.create "Watch" (fun _ ->
        Dotnet.runInRootOrFail "watch run"
    )

    Target.create "Run" (fun _ ->
        Dotnet.runInRootOrFail "run"
    )

    // --------------------------------------------------------------------------------------------------------
    // 3. FAKE targets hierarchy
    // --------------------------------------------------------------------------------------------------------

    [
        "Clean"
            ==> "AssemblyInfo"
            ==> "Build"
            ==> "Lint"
            ==> "Tests"
            ==> "Release"

        "Build"
            ==> "Watch" <=> "Run"
    ]

[<EntryPoint>]
let main args =
    args
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext

    initTargets ()
    |> ignore

    match args with
    | [| "-t"; target |]
    | [| target |] -> Target.runOrDefaultWithArguments target
    | _ -> Target.runOrDefaultWithArguments "Build"

    0 // return an integer exit code
