namespace MF.ImageManager

open System

type TargetDirMode =
    | Override
    | Exclude
    | DryRun

type TargetSubdir =
    | Flat
    | ByMonth
    | ByYear
    | ByYearAndMonth

type PrepareForSorting = {
    Source: string list
    Target: string
    TargetSubdirFallback: string option
    TargetDirMode: TargetDirMode
    TargetSubdir: TargetSubdir
    Exclude: string list option
    ExcludeList: string option
}

[<RequireQualifiedAccess>]
module PrepareForSorting =
    open FSharp.Data

    type private Config = JsonProvider<"src/schema/config.json", SampleIsList = true>

    let parse targetDirMode config =
        let config = Config.Parse config

        {
            Target = ""
            Source = config.Source |> Seq.toList
            TargetSubdirFallback = config.Fallback
            TargetDirMode = targetDirMode
            TargetSubdir =
                match config.TargetSubdir with
                | Some "year-month" -> ByYearAndMonth
                | Some "year" -> ByYear
                | Some "month" -> ByMonth
                | _ -> Flat
            Exclude =
                match config.Exclude |> Seq.toList with
                | [] -> None
                | exclude -> Some exclude
            ExcludeList = config.ExcludeList
        }

    let combine config defaults =
        match defaults with
        | Some defaults ->
            { defaults with
                Target = config.Target
                Source = config.Source @ defaults.Source |> List.distinct
                TargetSubdirFallback =
                    match config.TargetSubdirFallback with
                    | Some targetSubdirFallback -> Some targetSubdirFallback
                    | _ -> defaults.TargetSubdirFallback
                Exclude =
                    match config.Exclude with
                    | None
                    | Some [] -> defaults.Exclude
                    | exclude -> exclude
                ExcludeList =
                    match config.ExcludeList with
                    | Some excludeList -> Some excludeList
                    | _ -> defaults.ExcludeList
            }
        | _ -> config

type Image = {
    Name: string
    FullPath: string
    CreatedAt: DateTime option
}

[<RequireQualifiedAccess>]
module Image =
    let name { Name = name } = name

module Prepare =
    open System.IO
    open System.Collections.Generic
    open MetadataExtractor
    open MF.ConsoleApplication
    open MF.Utils

    type private FileOrDir =
        | File of string
        | Dir of string

    let private notIn excludedFiles item =
        excludedFiles
        |> List.contains item
        |> not

    let private notEndsBy (excludedFiles: string list) (item: string) =
        excludedFiles
        |> List.exists item.EndsWith
        |> not

    let rec private tryFind (dir, tag) (attr: IReadOnlyList<Directory>) =
        attr
        |> Seq.tryFind (fun d -> d.Name = dir)
        |> Option.bind (fun dir ->
            dir.Tags
            |> Seq.tryFind (fun t -> t.HasName && t.Name = tag)
        )

    let private findAllImages output dir =
        let ignored = [".DS_Store"]

        [ dir ]
        |> FileSystem.getAllFiles
        |> List.filter (notEndsBy ignored)
        |> List.choose (fun file ->
            try
                let dateTimeOriginal =
                    file
                    |> ImageMetadataReader.ReadMetadata
                    |> tryFind ("Exif SubIFD", "Date/Time Original")
                    |> Option.bind (fun t -> t.Description |> DateTime.parseExifDateTime)

                Some {
                    Name = file |> Path.GetFileName
                    FullPath = file |> Path.GetFullPath
                    CreatedAt = dateTimeOriginal
                }
            with e ->
                output.Error <| sprintf "File %s could not be parsed due to %A." file e.Message
                if output.IsVerbose() then output.Error <| sprintf "Error:\n%A" e
                None
        )

    let prepareForSorting output prepare =
        Directory.CreateDirectory(prepare.Target) |> ignore

        output.NewLine()
        output.SubTitle "Find all images in source"
        let allImagesInSource =
            prepare.Source
            |> List.distinct
            |> List.collect (findAllImages output)
            |> List.distinctBy Image.name

        output.Message <| sprintf " -> Found %i images" (allImagesInSource |> Seq.length)
        |> output.NewLine

        output.SubTitle "Exclude images from source by excluded dirs"
        let excludeDirs =
            match prepare.TargetDirMode, prepare.Exclude with
            | Override, Some excludeDirs -> Some excludeDirs
            | Exclude, Some excludeDirs -> Some (prepare.Target :: excludeDirs)
            | Exclude, None -> Some [ prepare.Target ]
            | _ -> None

        let excludeFiles, excludeDirs =
            match prepare.ExcludeList with
            | Some excludeList ->
                let excludeDirs =
                    match excludeDirs with
                    | Some excludeDirs -> excludeDirs
                    | _ -> []

                let excludeFiles, excludeDirs =
                    excludeList
                    |> File.ReadAllLines
                    |> Seq.fold (fun (excludeFiles, excludeDirs) f ->
                        let attr = f |> File.GetAttributes

                        if attr.HasFlag(FileAttributes.Directory) then excludeFiles, f :: excludeDirs
                        else f :: excludeFiles, excludeDirs
                    ) ([], excludeDirs)

                excludeFiles, (if excludeDirs |> List.isEmpty then None else Some excludeDirs)
            | None -> [], excludeDirs

        let filesToCopy =
            match excludeDirs with
            | Some excludeDirs ->
                let excludedFiles =
                    excludeDirs
                    |> List.distinct
                    |> FileSystem.getAllFiles
                    |> (@) excludeFiles
                    |> List.map Path.GetFileName
                    |> List.distinct

                if output.IsVeryVerbose() then
                    output.List excludedFiles

                output.Message <| sprintf " -> Exclude %i images" (excludedFiles |> List.length)

                let result =
                    allImagesInSource
                    |> List.filter (Image.name >> notIn excludedFiles)

                output.Message <| sprintf " -> Filtered %i images" (result |> List.length)
                result
            | None -> allImagesInSource

        if output.IsVeryVerbose() then
            output.Message " * All images:"
            output.List (allImagesInSource |> List.map Image.name)

            output.Message " * Files to copy:"
            output.List (filesToCopy |> List.map Image.name)

        output.Message " -> Done"
        |> output.NewLine

        let progress =
            let totalCount =
                filesToCopy
                |> List.length
                |> tee (sprintf "Copy images to target (%i)" >> output.SubTitle)

            match prepare.TargetDirMode with
            | DryRun -> None
            | _ ->
                totalCount
                |> output.ProgressStart "\n"
                |> Some

        let targetPath (image: Image) =
            let (/) (a: obj) (b: obj) = Path.Combine(string a, string b)
            let month = sprintf "%02i"

            match prepare, image with
            | { TargetSubdirFallback = None }, { CreatedAt = None }
            | { TargetSubdir = Flat }, _ -> prepare.Target / image.Name

            | { TargetSubdir = ByMonth; TargetSubdirFallback = Some fallback }, { CreatedAt = None }
            | { TargetSubdir = ByYear; TargetSubdirFallback = Some fallback }, { CreatedAt = None }
            | { TargetSubdir = ByYearAndMonth; TargetSubdirFallback = Some fallback }, { CreatedAt = None } -> prepare.Target / fallback / image.Name

            | { TargetSubdir = ByMonth }, { CreatedAt = Some createdAt } -> prepare.Target / (month createdAt.Month) / image.Name
            | { TargetSubdir = ByYear }, { CreatedAt = Some createdAt } -> prepare.Target / createdAt.Year / image.Name
            | { TargetSubdir = ByYearAndMonth }, { CreatedAt = Some createdAt } -> prepare.Target / createdAt.Year / (month createdAt.Month) / image.Name

        filesToCopy
        |> List.iter (fun image ->
            let targetPath = image |> targetPath

            match prepare.TargetDirMode with
            | DryRun ->
                output.Message <| sprintf " * <c:cyan>%s</c> -> <c:green>%s</c>" image.FullPath targetPath
            | _ ->
                targetPath
                |> Path.GetDirectoryName
                |> Directory.ensure

                (image.FullPath, targetPath)
                |> FileSystem.copy

                progress |> Option.iter output.ProgressAdvance
        )
        progress |> Option.iter output.ProgressFinish

        output.NewLine()

        Ok "Done"
