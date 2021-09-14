namespace MF.Utils

module FileSystem =
    open System.IO

    let rec getAllFiles = function
        | [] -> []
        | directories -> [
            yield! directories |> Seq.collect Directory.EnumerateFiles
            yield! directories |> Seq.collect Directory.EnumerateDirectories |> List.ofSeq |> getAllFiles
        ]

    let getAllDirectories dir =
        let rec getAllDirs acc: _ -> string list = function
            | [] -> acc |> List.distinct |> List.sort
            | directories ->
                let subDirs = directories |> Seq.collect Directory.EnumerateDirectories |> List.ofSeq
                subDirs |> getAllDirs (acc @ directories)
        [ dir ] |> getAllDirs []

    [<RequireQualifiedAccess>]
    type SearchFiles =
        | IgnoreDotFiles
        | All

    let getAllFilesAsync (output: MF.ConsoleApplication.Output) searchFiles dir = async {
        let prefix = "  <c:gray>[FileSystem] </c>"
        let ignoreDotFiles: string seq -> string seq =
            match searchFiles with
            | SearchFiles.IgnoreDotFiles -> Seq.filter (fun file -> (file |> Path.GetFileName).StartsWith "." |> not)
            | SearchFiles.All -> id

        output.Message $"{prefix}<c:yellow>Get all files in</c> <c:cyan>{dir}</c>"
        let dirs = getAllDirectories dir

        output.Message $"{prefix}  ├──> <c:cyan>Searching</c> for all <c:yellow>files</c> from <c:magenta>{dirs.Length}</c> directories ..."
        let progress = output.ProgressStart "Searching for files" dirs.Length
        let advance a =
            progress |> output.ProgressAdvance
            a

        let files = dirs |> List.collect (Directory.EnumerateFiles >> ignoreDotFiles >> Seq.toList >> advance)
        progress |> output.ProgressFinish
        output.Message $"{prefix}  └──> <c:green>Found</c> <c:magenta>{files.Length}</c> <c:yellow>files</c> ..."

        return files
    }

    let copy (source, target) =
        File.Copy(source, target, true)

[<RequireQualifiedAccess>]
module DateTime =
    open System
    open System.Globalization

    /// https://stackoverflow.com/questions/22568927/how-to-parse-exif-date-time-data
    let parseExifDateTime dateTime =
        DateTime.TryParseExact(
            dateTime,
            "yyyy:MM:dd HH:mm:ss",
            CultureInfo.CurrentCulture,
            DateTimeStyles.None
        )
        |> function
            | true, dateTime -> Some dateTime
            | _ -> None

[<RequireQualifiedAccess>]
module Directory =
    open System.IO

    let ensure dir =
        if dir |> Directory.Exists |> not then
            Directory.CreateDirectory(dir) |> ignore

[<AutoOpen>]
module Regexp =
    open System.Text.RegularExpressions

    // http://www.fssnip.net/29/title/Regular-expression-active-pattern
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some (List.tail [ for g in m.Groups -> g.Value ])
        else None

[<AutoOpen>]
module Utils =
    let tee f a =
        f a
        a

    let (|IsNull|_|) = function
        | value when isNull value -> Some IsNull
        | _ -> None

[<RequireQualifiedAccess>]
module File =
    let notIn excludedFiles item =
        excludedFiles
        |> List.contains item
        |> not

    let notEndsBy (excludedFiles: string list) (item: string) =
        excludedFiles
        |> List.exists item.EndsWith
        |> not
