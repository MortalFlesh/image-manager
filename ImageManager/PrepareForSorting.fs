module ImageManager.PrepareForSorting

open ImageManager.Types
open System.IO

type Image = {
    name: string
    fullPath: string
}

let rec listFilesRecursively dir =
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

let notIn excludedFiles image =
    excludedFiles
    |> Seq.map (fun i -> i.name)
    |> Seq.contains image.name
    |> not

let prepareForSorting prepare =
    printfn "from %s to %s:" prepare.source prepare.target

    let allFilesInSource =
        prepare.source
        |> listFilesRecursively

    let files =
        match prepare.exclude with
        | Some excludeDir ->
            let excludedFiles = excludeDir |> listFilesRecursively
            allFilesInSource |> Seq.filter (notIn excludedFiles)
        | None -> allFilesInSource        

    printfn "Files: %d" (Seq.length files)

    files
    |> Seq.map (fun i -> i.fullPath)
    |> Seq.iter (printfn "- %s")

    ("Done", 0)
