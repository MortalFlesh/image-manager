// Learn more about F# at http://fsharp.org

open System
open MF.ConsoleStyle
open ImageManager.Types
open ImageManager.PrepareForSorting

type Command =
| Invalid
| List
| PrepareForSorting of PrepareForSorting

[<EntryPoint>]
let main argv =
    Console.title "Image Manager"

    let command =
        match argv with
        | [|"prepare";source;target|] -> PrepareForSorting { source = source; target = target; exclude = None }
        | [|"prepare";source;target;exclude|] -> PrepareForSorting { source = source; target = target; exclude = Some exclude }
        | [|"list"|] -> List
        | _ -> Invalid

    let result =
        match command with
        | List ->
            Console.commandList
            <| [("help", "Display help for a command")]
            <| [
                ("list", "Lists commands")
                ("prepare", "Prepares images for sorting")
            ]
            0
        | PrepareForSorting p ->
            Console.section (sprintf "Prepare from %s to %s:" p.source p.target)

            let (message, result) = prepareForSorting p
            Console.success message
            result
        | Invalid ->
            Console.error "Invalid command"
            1

    Console.newLine ()
    result
