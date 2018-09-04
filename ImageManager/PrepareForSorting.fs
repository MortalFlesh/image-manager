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

let private notIn excludedFiles image =
    excludedFiles
    |> Seq.map (fun i -> i.name)
    |> Seq.contains image.name
    |> not

let private copy (source, target) =
    File.Copy(source, target, true)

let prepareForSorting prepare =
    Directory.CreateDirectory(prepare.target) |> ignore

    let allFilesInSource =
        prepare.source
        |> listFilesRecursively

    let filesToCopy =
        match prepare.exclude with
        | Some excludeDir ->
            let excludedFiles = excludeDir |> listFilesRecursively
            allFilesInSource |> Seq.filter (notIn excludedFiles)
        | None -> allFilesInSource

    filesToCopy
    |> Seq.map (fun i ->
        (i.fullPath, Path.Combine(prepare.target, i.name))
    )
    //|> Console.Options "Files to copy:"
    |> Seq.iter copy

    ("Done", 0)
