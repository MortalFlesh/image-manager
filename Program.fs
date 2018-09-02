// Learn more about F# at http://fsharp.org

open System
open ImageManager.Types
open ImageManager.PrepareForSorting

type Command =
| Invalid
| PrepareForSorting of PrepareForSorting

[<EntryPoint>]
let main argv =
    printfn "ImageManager"
    printfn "============"
    
    let command =
        match argv with
        | [|"prepare";source;target|] -> PrepareForSorting { source = source; target = target; exclude = None }
        | [|"prepare";source;target;exclude|] -> PrepareForSorting { source = source; target = target; exclude = Some exclude }
        | _ -> Invalid

    let (message, result) =
        match command with
        | Invalid -> ("Unknown command", 1)
        | PrepareForSorting p -> prepareForSorting p

    printfn "%s" message
    result
