namespace MF.ConsoleStyle

[<RequireQualifiedAccess>]
module Console =
    open System
    open System.Drawing
    open Colorful
    open ShellProgressBar

    type private Color =
    | Title
    | SubTitle
    | Section
    | TableHeader
    | Success
    | Error

    let private color = function
    | Title -> Color.Cyan
    | SubTitle -> Color.Yellow
    | Section -> Color.Yellow
    | TableHeader -> Color.DarkGoldenrod
    | Success -> Color.LimeGreen
    | Error -> Color.Red

    let private getMaxLengthForOptions options =
        options
        |> Seq.map fst
        |> Seq.maxBy String.length
        |> String.length

    let private getMaxLengthsPerColumn lines =
        lines
        |> Seq.collect (fun line ->
            line
            |> Seq.mapi (fun i value -> (i, value |> String.length))
        )
        |> Seq.fold (fun lengths (i, length) ->
            let isIn = lengths |> Map.containsKey i

            if (isIn && lengths.[i] < length) || not isIn
            then lengths |> Map.add i length
            else lengths
        ) Map.empty<int,int>
        |> Map.toList
        |> List.map snd

    [<CompiledName("Message")>]
    let message (message: string): unit =
        printfn "%s" message

    [<CompiledName("NewLine")>]
    let newLine (): unit =
        message ""

    [<CompiledName("MainTitle")>]
    let mainTitle (title: string): unit =
        Console.WriteAscii(title, color Color.Title)
        Console.WriteLine(String.replicate (title.Length * 6) "=", color Color.Title)
        newLine()

    [<CompiledName("Title")>]
    let title (title: string): unit =
        Console.WriteLine(title, color Color.Title)
        Console.WriteLine(String.replicate title.Length "=", color Color.Title)
        newLine()

    [<CompiledName("Section")>]
    let section (section: string): unit =
        Console.WriteLine(section, color Color.Section)
        Console.WriteLine(String.replicate section.Length "-", color Color.Section)
        newLine()

    [<CompiledName("SubTitle")>]
    let subTitle (subTitle: string): unit =
        Console.WriteLine(subTitle, color Color.SubTitle)

    [<CompiledName("Error")>]
    let error (message: string): unit =
        Console.WriteLine(message, color Color.Error)

    [<CompiledName("Success")>]
    let success (message: string): unit =
        Console.WriteLine(message, color Color.Success)
        newLine()

    [<CompiledName("Indentation")>]
    let indentation: string =
        String.replicate 4 " "

    [<CompiledName("Indent")>]
    let indent (value: string): string =
        indentation + value

    [<CompiledName("Messages")>]
    let messages (prefix: string) (messages: seq<string>) =
        messages
        |> Seq.map (sprintf "%s%s" prefix)
        |> Seq.iter message

    let private optionsList maxLength title options =
        subTitle title
        options
        |> Seq.map (fun (command, description) ->
            sprintf "- %-*s %-s" (maxLength + 1) command description
        )
        |> messages indentation

    [<CompiledName("Options")>]
    let options (title: string) (options: seq<string * string>): unit =
        optionsList (options |> getMaxLengthForOptions) title options
        newLine()

    [<CompiledName("List")>]
    let list (messages: seq<string>): unit =
        messages
        |> Seq.map (sprintf " - %s")
        |> Seq.iter message    

    [<CompiledName("Ask")>]
    let ask (question: string): string =
        Console.Write(question + " ", color Color.SubTitle)
        Console.ReadLine()

    [<CompiledName("Table")>]
    let table (header: seq<string>) (rows: seq<seq<string>>): unit =
        let isHeader = header |> Seq.isEmpty |> not
        let isRows = rows |> Seq.isEmpty |> not
 
        let row values =
            values
            |> String.concat " "

        let separate lengths =
            lengths
            |> List.map (fun l -> String.replicate l "-")

        let getMaxLengths header rows =
            let values =
                if isRows && isHeader then rows |> Seq.append [header]
                elif isRows then rows
                else [] |> Seq.append [header]
            values
            |> getMaxLengthsPerColumn
            |> List.map ((+) 2)

        let separator lengths =
            lengths
            |> separate
            |> row
            |> message

        let tableRow (lengths: int list) values =
            values
            |> Seq.mapi (fun i v ->
                sprintf " %-*s" (lengths.[i] - 1) v
            )

        let tableHeader lengths header =
            header
            |> tableRow lengths
            |> row
            |> fun r -> Console.WriteLine(r, color Color.TableHeader)
            separator lengths

        let tableRows lengths rows =
            rows
            |> Seq.map ((tableRow lengths) >> row)
            |> Seq.iter message
            separator lengths

        if isHeader || isRows then
            let lengths = getMaxLengths header rows

            separator lengths
            if isHeader then header |> tableHeader lengths
            if isRows then rows |> tableRows lengths
        newLine()

    [<CompiledName("ProgressStart")>]
    let progressStart (initialMessage: string) (total: int): ProgressBar =
        let options = new ProgressBarOptions()
        options.ForegroundColor <- ConsoleColor.Yellow
        options.ForegroundColorDone <- Nullable<ConsoleColor>(ConsoleColor.DarkGreen)
        options.BackgroundColor <- Nullable<ConsoleColor>(ConsoleColor.DarkGray)
        options.BackgroundCharacter <- Nullable<char>('\u2593')
        options.DisplayTimeInRealTime <- true
        options.ProgressBarOnBottom <- true

        new ProgressBar(total, initialMessage, options)

    [<CompiledName("ProgressFinish")>]
    let progressFinish (progress: ProgressBar): unit =
        progress.Message <- "Finished"
        progress.Dispose()

    //
    // Hi-level functions
    //

    [<CompiledName("CommandList")>]
    let commandList (options: seq<string * string>) (commands: seq<string * string>): unit =
        let maxLength =
            options
            |> Seq.append commands
            |> getMaxLengthForOptions

        subTitle "Usage:"

        "command [options] [arguments]"
        |> indent
        |> message
        newLine ()

        options
        |> optionsList maxLength "Options:"
        newLine ()

        commands
        |> optionsList maxLength "Available commands:"
        newLine ()
