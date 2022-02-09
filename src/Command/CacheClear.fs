namespace MF.ImageManager.Command

open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ImageManager
open MF.Utils

[<RequireQualifiedAccess>]
module CacheClear =
    let arguments = []
    let options = []

    let private run = asyncResult {
        do! Hash.Cache.clear

        return "Done"
    }

    let execute ((input, output): IO) =
        run
        |> AsyncResult.waitAfterFinish output 2000
        |> Async.RunSynchronously
        |> function
            | Ok message ->
                output.Success message
                ExitCode.Success

            | Error (PrepareError.Exception e) ->
                output.Error e.Message
                ExitCode.Error
            | Error (PrepareError.ErrorMessage message) ->
                output.Error message
                ExitCode.Error
