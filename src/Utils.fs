namespace MF.Utils

module FileSystem =
    open System.IO

    let rec getAllFiles = function
        | [] -> []
        | directories -> [
            yield! directories |> Seq.collect Directory.EnumerateFiles
            yield! directories |> Seq.collect Directory.EnumerateDirectories |> List.ofSeq |> getAllFiles
        ]

    let copy (source, target) =
        File.Copy(source, target, true)

[<AutoOpen>]
module Utils =
    let tee f a =
        f a
        a
