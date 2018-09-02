// Learn more about F# at http://fsharp.org

open System
open ImageManager.Types
open ImageManager.PrepareForSorting

type Command =
| Invalid
| List
| PrepareForSorting of PrepareForSorting

let listCommands () =
    ["prepare";"list"]
    |> List.map (sprintf "- %s")
    |> String.concat "\n"
    |> sprintf "Available commands:\n%s"

[<EntryPoint>]
let main argv =
    printfn "ImageManager"
    printfn "============"
    
    let command =
        match argv with
        | [|"prepare";source;target|] -> PrepareForSorting { source = source; target = target; exclude = None }
        | [|"prepare";source;target;exclude|] -> PrepareForSorting { source = source; target = target; exclude = Some exclude }
        | [|"list"|] -> List
        | _ -> Invalid

    let (message, result) =
        match command with
        | List -> (listCommands(), 0)
        | PrepareForSorting p -> prepareForSorting p
        | Invalid -> ("Unknown command", 1)

    printfn "%s" message
    result
