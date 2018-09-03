// Learn more about F# at http://fsharp.org

open System
open ImageManager.Types
open ImageManager.PrepareForSorting

type Command =
| Invalid
| List
| PrepareForSorting of PrepareForSorting

let listCommands () =
    ["prepare SOURCE TARGET [EXCLUDE]";"list"]
    |> List.map (sprintf "- %s")
    |> String.concat "\n"
    |> sprintf "Available commands:\n%s"

[<EntryPoint>]
let main argv =
    Console.Title "Image Manager"

    let command =
        match argv with
        | [|"prepare";source;target|] -> PrepareForSorting { source = source; target = target; exclude = None }
        | [|"prepare";source;target;exclude|] -> PrepareForSorting { source = source; target = target; exclude = Some exclude }
        | [|"list"|] -> List
        | _ -> Invalid

    let result =
        match command with
        | List ->
            Console.CommandList
            <| []
            <| [
                ("prepare", "Prepares images for sorting")
            ]
            0
        | PrepareForSorting p ->
            let (message, result) = prepareForSorting p
            Console.Message message
            result
        | Invalid ->
            Console.Error "Invalid command"
            1
    Console.NewLine ()
    result
