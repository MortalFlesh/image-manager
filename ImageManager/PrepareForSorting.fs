module ImageManager.PrepareForSorting

open ImageManager.Types
open System.IO

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

    Console.SubTitle "Find all images in source"
    let allFilesInSource =
        prepare.source
        |> listFilesRecursively
    Console.Message (sprintf "Found %i images" (allFilesInSource |> Seq.length))
    
    Console.SubTitle "Exclude images from source by excluded dir"
    let filesToCopy =
        match prepare.exclude with
        | Some excludeDir ->
            let excludedFiles =
                excludeDir
                |> listFilesRecursively
                |> List.map (fun i -> i.name)
            
            let excludeCount = excludedFiles |> List.length
            Console.Message (sprintf "Exclude %i images" excludeCount)
            let excludeProgress = Console.ProgressStart "" excludeCount

            let result =
                allFilesInSource
                |> List.filter (
                    (notIn excludedFiles)
                    >>
                    (fun i ->
                        excludeProgress.Tick()
                        i
                    )
                )
            Console.ProgressFinish excludeProgress            
            Console.Message (sprintf "Filtered %i images" (result |> List.length))
            result
        | None -> allFilesInSource

    let total = filesToCopy |> List.length
    Console.SubTitle (sprintf "Copy images to target (%i)" total)
    let progress = total |> Console.ProgressStart ""

    filesToCopy
    |> List.iter (fun i ->
        progress.Tick(i.fullPath)
        (i.fullPath, Path.Combine(prepare.target, i.name))
        |> copy
    )
    progress |> Console.ProgressFinish

    ("Done", 0)
