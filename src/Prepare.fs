namespace MF.ImageManager

type TargetDirMode =
    | Override
    | Exclude

type PrepareForSorting = {
    Source: string list
    Target: string
    TargetDirMode: TargetDirMode
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
        |> List.contains image.Name
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
            |> List.distinct
            |> List.collect findAllImages
            |> List.distinct
        output.Message <| sprintf "Found %i images" (allImagesInSource |> Seq.length)

        output.SubTitle "Exclude images from source by excluded dirs"
        let excludeDirs =
            match prepare.TargetDirMode, prepare.Exclude with
            | Override, Some excludeDirs -> Some excludeDirs
            | Exclude, Some excludeDirs -> Some (prepare.Target :: excludeDirs)
            | Exclude, None -> Some [ prepare.Target ]
            | _ -> None

        let filesToCopy =
            match excludeDirs with
            | Some excludeDirs ->
                let excludedFiles =
                    excludeDirs
                    |> List.distinct
                    |> FileSystem.getAllFiles
                    |> List.map Path.GetFileName
                    |> List.distinct

                if output.IsVeryVerbose() then
                    output.List excludedFiles

                output.Message <| sprintf "Exclude %i images" (excludedFiles |> List.length)

                let result =
                    allImagesInSource
                    |> List.filter (notIn excludedFiles)

                output.Message <| sprintf "Filtered %i images" (result |> List.length)
                result
            | None -> allImagesInSource

        if output.IsVeryVerbose() then
            output.Message "All images:"
            output.List (allImagesInSource |> List.map Image.name)

            output.Message "Files to copy:"
            output.List (filesToCopy |> List.map Image.name)

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
