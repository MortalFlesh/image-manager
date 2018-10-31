module ImageManager.PrepareForSorting

open System.IO
open MF.ConsoleStyle
open ImageManager.Types

type Image = {
    name: string
    fullPath: string
}

let rec private listFilesRecursively dir =
    let listFiles dir =
        let ignored = [".DS_Store"]
        Directory.GetFiles(dir)
        |> Seq.map (fun f -> 
            {
                name = f |> Path.GetFileName
                fullPath = f |> Path.GetFullPath
            }
        )
        |> Seq.filter ((fun i -> ignored |> List.contains i.name) >> not)

    Directory.GetDirectories(dir)
    |> Seq.collect listFilesRecursively
    |> Seq.append (listFiles dir)
    |> List.ofSeq

let private notIn excludedFiles image =
    excludedFiles
    |> Seq.contains image.name
    |> not

let private copy (source, target) =
    File.Copy(source, target, true)

let prepareForSorting prepare =
    Directory.CreateDirectory(prepare.target) |> ignore

    Console.subTitle "Find all images in source"
    let allFilesInSource =
        prepare.source
        |> listFilesRecursively
    Console.message (sprintf "Found %i images" (allFilesInSource |> Seq.length))
    
    Console.subTitle "Exclude images from source by excluded dir"
    let filesToCopy =
        match prepare.exclude with
        | Some excludeDirs ->
            let excludedFiles =
                excludeDirs
                |> List.collect (fun excludeDir ->
                    excludeDir
                    |> listFilesRecursively
                    |> List.map (fun i -> i.name)
                )
                |> List.distinct

            let excludeCount = excludedFiles |> List.length
            Console.message (sprintf "Exclude %i images" excludeCount)
            
            let result =
                allFilesInSource
                |> List.filter (notIn excludedFiles)

            Console.message (sprintf "Filtered %i images" (result |> List.length))
            result
        | None -> allFilesInSource

    let total = filesToCopy |> List.length
    Console.subTitle (sprintf "Copy images to target (%i)" total)
    let progress = total |> Console.progressStart ""

    filesToCopy
    |> List.iter (fun i ->
        progress.Tick(i.fullPath)
        (i.fullPath, Path.Combine(prepare.target, i.name))
        |> copy
    )
    progress |> Console.progressFinish

    ("Done", 0)
