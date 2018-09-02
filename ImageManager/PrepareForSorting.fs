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

let prepareForSorting prepare =
    printfn "from %s to %s:" prepare.source prepare.target

    let files =
        prepare.source
        |> listFilesRecursively

    printfn "Files: %d" (Seq.length files)

    files
    |> Seq.map (fun i -> i.fullPath)
    |> Seq.iter (printfn "- %s")

    ("Done", 0)
