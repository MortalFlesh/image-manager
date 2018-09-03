//namespace MF.Console

module Console

open System.Drawing
open Colorful

type Color =
| Title
| SubTitle
| Section
| Error

let private color = function
| Title -> Color.Cyan
| SubTitle -> Color.Yellow
| Section -> Color.Yellow
| Error -> Color.Red

let Title title =
    Console.WriteAscii(title, color Color.Title)
    Console.WriteLine(String.replicate (title.Length * 6) "=", color Color.Title)
    printfn ""

let Section (section: string) =
    Console.WriteLine(section, color Color.Section)
    Console.WriteLine(String.replicate section.Length "-", color Color.Section)
    printfn ""

let SubTitle (subTitle: string) =
    Console.WriteLine(subTitle, color Color.SubTitle)

let Error (message: string) =
    Console.WriteLine(message, color Color.Error)

let Message message = 
    printfn "%s" message

let List messages =
    messages
    |> Seq.map (sprintf " - %s")
    |> Seq.iter Message

let Messages prefix messages =
    messages
    |> Seq.map (sprintf "%s%s" prefix)
    |> Seq.iter Message

let Indentation =
    String.replicate 4 " "

let Indent value =
    Indentation + value

let Options maxLength title options =
    SubTitle title
    options
    |> Seq.map (fun (command, description) ->
        sprintf "- %-*s %-s" (maxLength + 1) command description
    )
    |> Messages Indentation

let NewLine () =
    Message ""

let CommandList options commands =
    let maxLength =
        options
        |> Seq.append commands
        |> Seq.map fst
        |> Seq.maxBy String.length
        |> String.length

    SubTitle "Usage:"

    "command [options] [arguments]"
    |> Indent
    |> Message

    NewLine ()

    [("help", "Display help for a command")]
    |> Seq.append options
    |> Options maxLength "Options:"

    NewLine ()

    [("list", "Lists commands")]
    |> Seq.append commands
    |> Options maxLength "Available commands:"
