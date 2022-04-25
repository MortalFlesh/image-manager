namespace MF.ImageManager.Command

open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ImageManager
open MF.Utils
open MF.Utils.Logging

[<RequireQualifiedAccess>]
module CacheClear =
    let arguments = []
    let options = []

    let execute = ExecuteAsyncResult <| fun ((input, output): IO) ->
        asyncResult {
            do! Hash.Cache.clear

            output.Success "Done"

            return ExitCode.Success
        }
        |> AsyncResult.waitAfterFinish output 2000
        |> AsyncResult.mapError (List.singleton >> Errors.map "Cache Clear" output PrepareError.format)
