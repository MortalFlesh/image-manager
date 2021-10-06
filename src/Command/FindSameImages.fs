namespace MF.ImageManager.Command

open System.IO
open MF.ConsoleApplication
open MF.ErrorHandling
open MF.ImageManager
open MF.ImageManager.Prepare
open MF.Utils

[<RequireQualifiedAccess>]
module FindSameImages =
    let arguments = [
        Argument.required "target" "Directory you want to check."
    ]

    let options = []

    let private run output ignoreWarnings target = asyncResult {
        let! images =
            target
            |> Finder.findAllImagesInDir output ignoreWarnings None None

        output.NewLine()

        images
        |> List.iter (fun i ->
            output.Message $"Image <c:cyan>{i.Name}</c>"
            output.Message $"  <c:gray>> {i.FullPath}</c>"

            i.Metadata
            |> Map.toList
            |> List.sortBy fst
            |> List.map (fun (k, v) -> [ k; v ])
            |> output.Table [ "Meta"; "Value" ]
        )

        return "Done"
    }

    let execute ((input, output): IO) =
        asyncResult {
            let target = input |> Input.getArgumentValue "target"

            let ignoreWarnings = true

            return! target |> run output ignoreWarnings
        }
        |> Async.RunSynchronously
        |> function
            | Ok message ->
                output.Success message
                ExitCode.Success
            | Error errors ->
                errors
                |> List.iter (function
                    | PrepareError.Exception e -> output.Error e.Message
                    | PrepareError.ErrorMessage message -> output.Error message
                )

                ExitCode.Error
