namespace MF.ImageManager

type PrepareForSorting = {
    Source: string
    Target: string
    Exclude: string list option
}

type Image = {
    Name: string
    FullPath: string
}

[<RequireQualifiedAccess>]
module Image =
    let name { Name = name } = name

module Prepare =
    open System.IO
    open MF.ConsoleApplication
    open MF.Utils

    let private notIn excludedFiles (image: Image) =
        excludedFiles
        |> Seq.contains image.Name
        |> not

    let private findAllImages dir =
        let ignored = [".DS_Store"]

        [ dir ]
        |> FileSystem.getAllFiles
        |> List.map (fun file ->
            {
                Name = file |> Path.GetFileName
                FullPath = file |> Path.GetFullPath
            }
        )
        |> List.filter (notIn ignored)

    let prepareForSorting output prepare =
        Directory.CreateDirectory(prepare.Target) |> ignore

        output.SubTitle "Find all images in source"
        let allImagesInSource =
            prepare.Source
            |> findAllImages
        output.Message <| sprintf "Found %i images" (allImagesInSource |> Seq.length)

        output.SubTitle "Exclude images from source by excluded dir"
        let filesToCopy =
            match prepare.Exclude with
            | Some excludeDirs ->
                let excludedFiles =
                    excludeDirs
                    |> FileSystem.getAllFiles
                    |> List.distinct

                output.Message <| sprintf "Exclude %i images" (excludedFiles |> List.length)

                let result =
                    allImagesInSource
                    |> List.filter (notIn excludedFiles)

                output.Message <| sprintf "Filtered %i images" (result |> List.length)
                result
            | None -> allImagesInSource

        let progress =
            filesToCopy
            |> List.length
            |> tee (sprintf "Copy images to target (%i)" >> output.SubTitle)
            |> output.ProgressStart ""

        filesToCopy
        |> List.iter (fun image ->
            (image.FullPath, Path.Combine(prepare.Target, image.Name))
            |> FileSystem.copy

            progress |> output.ProgressAdvance
        )
        progress |> output.ProgressFinish

        Ok "Done"
