#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators

// =============================================================================================
// === Build scripts ============================================================== 2019-11-10 =
// ---------------------------------------------------------------------------------------------
// Options:
//  - no-clean   - disables clean of dirs in the first step (required on CI)
//  - no-lint    - lint will be executed, but the result is not validated
// =============================================================================================

let tee f a =
    f a
    a

module private DotnetCore =
    let run cmd workingDir =
        let options =
            DotNet.Options.withWorkingDirectory workingDir
            >> DotNet.Options.withRedirectOutput true

        DotNet.exec options cmd ""

    let runOrFail cmd workingDir =
        run cmd workingDir
        |> tee (fun result ->
            if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir
        )
        |> ignore

    let runInRoot cmd = run cmd "."
    let runInRootOrFail cmd = runOrFail cmd "."

    let installOrUpdateTool tool =
        // Global tool dir must be in PATH - ${PATH}:/root/.dotnet/tools
        let toolCommand action =
            sprintf "tool %s --tool-path ./tools %s" action tool

        match runInRoot (toolCommand "install") with
        | { ExitCode = code } when code <> 0 -> runInRootOrFail (toolCommand "update")
        | _ -> ()

    let execute command args (dir: string) =
        let cmd =
            sprintf "%s/%s"
                (dir.TrimEnd('/'))
                command

        let processInfo = System.Diagnostics.ProcessStartInfo(cmd)
        processInfo.RedirectStandardOutput <- true
        processInfo.RedirectStandardError <- true
        processInfo.UseShellExecute <- false
        processInfo.CreateNoWindow <- true
        processInfo.Arguments <- args |> String.concat " "

        use proc =
            new System.Diagnostics.Process(
                StartInfo = processInfo
            )
        if proc.Start() |> not then failwith "Process was not started."
        proc.WaitForExit()

        if proc.ExitCode <> 0 then failwithf "Command '%s' failed in %s." command dir
        (proc.StandardOutput.ReadToEnd(), proc.StandardError.ReadToEnd())

let skipOn option action p =
    if p.Context.Arguments |> Seq.contains option
    then Trace.tracefn "Skipped ..."
    else action p

Target.create "Clean" <| skipOn "no-clean" (fun _ ->
    !! "./**/bin/Release"
    ++ "./**/bin/Debug"
    ++ "./**/obj"
    |> Shell.cleanDirs
)

Target.create "Build" (fun _ ->
    !! "./**/*.*proj"
    |> Seq.iter (DotNet.build id)
)

Target.create "Lint" (fun p ->
    DotnetCore.installOrUpdateTool "dotnet-fsharplint"

    let checkResult (messages: string list) =
        let rec check: string list -> unit = function
            | [] -> failwithf "Lint does not yield a summary."
            | head::rest ->
                if head.Contains("Summary") then
                    match head.Replace("= ", "").Replace(" =", "").Replace("=", "").Replace("Summary: ", "") with
                    | "0 warnings" -> Trace.tracefn "Lint: OK"
                    | warnings ->
                        if p.Context.Arguments |> List.contains "no-lint"
                        then Trace.traceErrorfn "Lint ends up with %s." warnings
                        else failwithf "Lint ends up with %s." warnings
                else check rest
        messages
        |> List.rev
        |> check

    !! "**/*.fsproj"
    |> Seq.map (fun fsproj ->
        DotnetCore.execute "dotnet-fsharplint" ["-f"; fsproj] "tools"
        |> fst
        |> tee (Trace.tracefn "%s")
        |> String.split '\n'
        |> Seq.toList
    )
    |> Seq.iter checkResult
)

Target.create "Tests" (fun _ ->
    Trace.tracefn "There are no tests yet."
    //DotnetCore.runOrFail "run" "tests"
)

Target.create "Release" (fun _ ->
    // runtime ids: https://docs.microsoft.com/en-us/dotnet/core/rid-catalog#macos-rids

    [
        "osx-x64"
        "win-x64"
    ]
    |> List.iter (fun runtimeId ->
        sprintf "publish -c Release /p:PublishSingleFile=true -o ./dist/%s --self-contained -r %s image-manager.fsproj" runtimeId runtimeId
        |> DotnetCore.runInRootOrFail
    )
)

"Clean"
    ==> "Build"
    ==> "Lint"
    ==> "Tests" <=> "Release"

Target.runOrDefaultWithArguments "Build"
