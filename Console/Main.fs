//namespace MF.Console

module Console

open System
open System.Drawing
open Colorful
open ShellProgressBar

type Color =
| Title
| SubTitle
| Section
| Success
| Error

let private color = function
| Title -> Color.Cyan
| SubTitle -> Color.Yellow
| Section -> Color.Yellow
| Success -> Color.LimeGreen
| Error -> Color.Red

let private maxLength options =
    options
    |> Seq.map fst
    |> Seq.maxBy String.length
    |> String.length

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

let Success (message: string) =
    Console.WriteLine(message, color Color.Success)

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

let private optionsList maxLength title options =
    SubTitle title
    options
    |> Seq.map (fun (command, description) ->
        sprintf "- %-*s %-s" (maxLength + 1) command description
    )
    |> Messages Indentation

let Options title options =
    optionsList (options |> maxLength) title options

let NewLine () =
    Message ""

let Ask (question: string) =
    Console.Write(question + " ", color Color.SubTitle)
    Console.ReadLine()

let ProgressStart initialMessage total =
    let options = new ProgressBarOptions()
    options.ForegroundColor <- ConsoleColor.Yellow
    options.ForegroundColorDone <- Nullable<ConsoleColor>(ConsoleColor.DarkGreen)
    options.BackgroundColor <- Nullable<ConsoleColor>(ConsoleColor.DarkGray)
    options.BackgroundCharacter <- Nullable<char>('\u2593')
    options.DisplayTimeInRealTime <- true
    options.ProgressBarOnBottom <- true

    new ProgressBar(total, initialMessage, options)

let ProgressFinish (progress: ProgressBar) =
    progress.Message <- "Finished"
    progress.Dispose()

let CommandList options commands =
    let maxLength =
        options
        |> Seq.append commands
        |> maxLength

    SubTitle "Usage:"

    "command [options] [arguments]"
    |> Indent
    |> Message

    NewLine ()

    options
    |> optionsList maxLength "Options:"

    NewLine ()

    commands
    |> optionsList maxLength "Available commands:"
