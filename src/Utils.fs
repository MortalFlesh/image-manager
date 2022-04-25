namespace MF.Utils

module internal Progress =
    open System
    open MF.ConsoleApplication

    type Progress (io: MF.ConsoleApplication.IO, name: string) =
        let (input, output) = io
        let mutable progressBar: _ option = None

        member private __.IsEnabled() = true

        member _.Start(total: int) =
            progressBar <- Some (output.ProgressStart name total)

        member __.Advance() =
            progressBar |> Option.iter output.ProgressAdvance

        member __.Finish() =
            progressBar |> Option.iter output.ProgressFinish
            progressBar <- None

        interface IDisposable with
            member this.Dispose() =
                this.Finish()

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

    let getAllFilesAsync ((_, output as io): MF.ConsoleApplication.IO) searchFiles dir = async {
        let prefix = "  <c:gray>[FileSystem] </c>"
        let ignoreDotFiles: string seq -> string seq =
            match searchFiles with
            | SearchFiles.IgnoreDotFiles -> Seq.filter (fun file -> (file |> Path.GetFileName).StartsWith "." |> not)
            | SearchFiles.All -> id

        output.Message $"{prefix}<c:yellow>Get all files in</c> <c:cyan>{dir}</c>"
        let dirs = getAllDirectories dir

        output.Message $"{prefix}  ├──> <c:cyan>Searching</c> for all <c:yellow>files</c> from <c:magenta>{dirs.Length}</c> directories ..."
        let progress = dirs.Length |> output.ProgressStart "Searching for files"

        let advance a =
            progress.Advance()
            a

        let files =
            dirs
            |> List.collect (Directory.EnumerateFiles >> ignoreDotFiles >> Seq.toList >> advance)

        progress.Finish()
        output.Message $"{prefix}  └──> <c:green>Found</c> <c:magenta>{files.Length}</c> <c:yellow>files</c> ..."

        return files
    }

    let copy (source, target) =
        File.Copy(source, target, true)

[<AutoOpen>]
module Regexp =
    open System.Text.RegularExpressions

    // http://www.fssnip.net/29/title/Regular-expression-active-pattern
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some (List.tail [ for g in m.Groups -> g.Value ])
        else None

[<RequireQualifiedAccess>]
module DateTime =
    open System
    open System.Globalization

    /// https://stackoverflow.com/questions/22568927/how-to-parse-exif-date-time-data
    let parseExifDateTime (dateTime: string) =
        let format =
            match dateTime with
            | Regex @"^(\d{2}\/\d{2}\/\d{4} \d{2}:\d{2}:\d{2})$" [ _ ] -> Some "MM/dd/yyyy HH:mm:ss"
            | Regex @"^(\d{4}:\d{2}:\d{2} \d{2}:\d{2}:\d{2})$" [ _ ] -> Some "yyyy:MM:dd HH:mm:ss"
            | _ -> None

        let parsed =
            match format with
            | Some format -> DateTime.TryParseExact(dateTime, format, CultureInfo.CurrentCulture, DateTimeStyles.None)
            | _ -> DateTime.TryParse(dateTime)

        match parsed with
        | true, dateTime -> Some dateTime
        | _ -> None

    let formatToExif (dateTime: DateTime) =
        dateTime.ToString("yyyy:MM:dd HH:mm:ss")

[<RequireQualifiedAccess>]
module Directory =
    open System.IO

    let ensure dir =
        if dir |> Directory.Exists |> not then
            Directory.CreateDirectory(dir) |> ignore

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

module internal Logging =
    open System
    open Microsoft.Extensions.Logging

    let private normalizeString (string: string) =
        string.Replace(" ", "").ToLowerInvariant()

    [<RequireQualifiedAccess>]
    module LogLevel =
        let parse = normalizeString >> function
            | "trace" | "vvv" -> LogLevel.Trace
            | "debug" | "vv" -> LogLevel.Debug
            | "information" | "v" | "normal" -> LogLevel.Information
            | "warning" -> LogLevel.Warning
            | "error" -> LogLevel.Error
            | "critical" -> LogLevel.Critical
            | "quiet" | "q" | _ -> LogLevel.None

    [<RequireQualifiedAccess>]
    module LoggerFactory =
        open NReco.Logging.File

        let create command level =
            LoggerFactory.Create(fun builder ->
                builder
                    .SetMinimumLevel(level)
                    .AddFile(
                        (command |> normalizeString |> sprintf "logs/log_%s_{0:yyyy}-{0:MM}-{0:dd}.log"),
                        fun c ->
                            c.FormatLogFileName <- fun name -> String.Format(name, DateTime.UtcNow)
                            c.Append <- true
                            c.MinLevel <- LogLevel.Trace
                    )
                |> ignore
            )

[<RequireQualifiedAccess>]
module Errors =
    open System
    open System.IO
    open MF.ConsoleApplication
    open Microsoft.Extensions.Logging
    open Logging

    let private show (output: Output) (errors: string list) =
        if errors.Length > 10 then
            let now = DateTime.Now.ToString("dd-MM-yyyy--HH-mm")
            let file = $"rename-error--{now}.log"
            File.WriteAllLines(file, errors)
            output.Error $"Errors {errors.Length} was saved to \"{file}\" file."
        else
            errors
            |> List.iter output.Error

    let map command (output: Output) format errors =
        use loggerFactory =
            if output.IsDebug() then "vvv"
            elif output.IsVeryVerbose() then "vv"
            else "v"
            |> LogLevel.parse
            |> LoggerFactory.create command

        let logger = loggerFactory.CreateLogger command

        errors
        |> List.map (format >> tee logger.LogError)
        |> show output

        CommandError.Message $"There are {errors.Length} errors"
        |> ConsoleApplicationError.CommandError

[<RequireQualifiedAccess>]
module String =
    open System

    let (|IsEmpty|_|) = function
        | isEmpty when isEmpty |> String.IsNullOrWhiteSpace -> Some ()
        | _ -> None

    let toLower (s: string) = s.ToLowerInvariant()

    let replace (oldValue: string) (newValue: string) (s: string) = s.Replace(oldValue, newValue)

[<RequireQualifiedAccess>]
module AsyncResult =
    open MF.ErrorHandling

    let handleMultipleResultsBy sequential =
        if sequential then AsyncResult.ofSequentialAsyncResults
        else AsyncResult.ofParallelAsyncResults

    let handleMultipleResults (output: MF.ConsoleApplication.Output) =
        handleMultipleResultsBy (output.IsDebug())

    let handleMultipleAsyncsBy sequential =
        if sequential then AsyncResult.ofSequentialAsyncs
        else AsyncResult.ofParallelAsyncs

    let handleMultipleAsyncs (output: MF.ConsoleApplication.Output) =
        handleMultipleAsyncsBy (output.IsDebug())

    let waitAfterFinish (output: MF.ConsoleApplication.Output) sleepFor ar = asyncResult {
        let! result = ar
        output.Message "Waiting ..."
        do! AsyncResult.sleep sleepFor
        return result
    }

/// https://titanwolf.org/Network/Articles/Article?AID=9c8c1045-c819-4827-84c6-9c9977a63bdc
module Crc32 =
    open System

    //Generator polynomial (modulo 2) for the reversed CRC32 algorithm.
    let private sGenerator = uint32 0xEDB88320

    //Generate lookup table
    let private lutIntermediate input =
        if (input &&& uint32 1) <> uint32 0
        then sGenerator ^^^ (input >>> 1)
        else input >>> 1

    let private lutEntry input =
        { 0..7 }
        |> Seq.fold (fun acc x -> lutIntermediate acc) input

    let private crc32lut =
        [ uint32 0 .. uint32 0xFF ]
        |> List.map lutEntry

    let crc32byte (register: uint32) (byte: byte) =
        crc32lut.[Convert.ToInt32((register &&& uint32 0xFF) ^^^ Convert.ToUInt32(byte))] ^^^ (register >>> 8)

    //CRC32 of a byte array
    let crc32 (input : byte[]) =
        let result = Array.fold crc32byte (uint32 0xFFFFFFFF) input
        ~~~result

    //CRC32 from ASCII string
    let crc32OfAscii (inputAscii : string) =
        let bytes = System.Text.Encoding.ASCII.GetBytes(inputAscii)
        crc32 bytes

    //CRC32 from ASCII string
    let crc32OfString = crc32OfAscii >> sprintf "%x"

module CommandHelp =
    let commandHelp lines = lines |> String.concat "\n\n" |> Some

    /// Concat two lines into one line for command help, so they won't be separated by other empty line
    let inline (<+>) line1 line2 = sprintf "%s\n%s" line1 line2
