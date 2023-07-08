namespace MF.Utils

module CommonOptions =
    open MF.ConsoleApplication

    let [<Literal>] DebugMeta = "debug-meta"
    let [<Literal>] DebugCache = "debug-cache"
    let [<Literal>] FFMpeg = "ffmpeg"
    let [<Literal>] OnlyVideo = "only-video"
    let [<Literal>] OnlyImage = "only-image"
    let [<Literal>] PreloadHashedAgain = "preload-hashed"
    let [<Literal>] ReHashAgain = "re-hash"

    let debugMetaOption = Option.noValue DebugMeta None "Whether to show all metadata for files."
    let debugCacheOption = Option.noValue DebugCache None "Whether to show information about cache for files."
    let ffmpegOption = Option.optional FFMpeg None "FFMpeg path in the current dir" None
    let videoOnlyOption = Option.noValue OnlyVideo None "Whether to rename video files only"
    let imageOnlyOption = Option.noValue OnlyImage None "Whether to rename image files only"

    let all = [
        debugMetaOption
        debugCacheOption
        ffmpegOption
        videoOnlyOption
        imageOnlyOption
    ]

module internal Progress =
    open System
    open MF.ConsoleApplication

    type Progress (io: MF.ConsoleApplication.IO, name: string) =
        let (input, output) = io
        let mutable progressBar: _ option = None

        member private __.IsEnabled() = true

        member _.Start(total: int) =
            let progress = output.ProgressStart name total
            if progress.IsAvailable() then
                progressBar <- Some progress

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
    open MF.ConsoleApplication

    module Operators =
        let inline (</>) a b = Path.Combine(a, b)

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

    type private FileFilter = string -> bool

    type Is = {
        IsVideo: string -> bool
        IsImage: string -> bool
    }

    let getAllFilesAsync ((input, output as io): MF.ConsoleApplication.IO) searchFiles is dir = async {
        let prefix = "  <c:gray>[FileSystem] </c>"
        let ignoreDotFiles: FileFilter option =
            match searchFiles with
            | SearchFiles.IgnoreDotFiles -> Some (fun file -> (file |> Path.GetFileName).StartsWith "." |> not)
            | SearchFiles.All -> None

        let findByType: FileFilter option =
            match input with
            | Input.Option.IsSet CommonOptions.OnlyImage _ -> Some is.IsImage
            | Input.Option.IsSet CommonOptions.OnlyVideo _ -> Some is.IsVideo
            | _ -> None

        let filter: FileFilter option =
            let filters =
                [
                    ignoreDotFiles
                    findByType
                ]
                |> List.choose id

            match filters with
            | [] -> None
            | filters -> Some <| fun file ->
                filters
                |> List.fold (fun acc filter -> acc && filter file) true

        output.Message $"{prefix}<c:yellow>Get all files in</c> <c:cyan>{dir}</c>"
        let dirs = getAllDirectories dir

        output.Message $"{prefix}  ├──> <c:cyan>Searching</c> for all <c:yellow>files</c> from <c:magenta>{dirs.Length}</c> directories ..."
        let progress = dirs.Length |> output.ProgressStart "Searching for files"

        let advance a =
            progress.Advance()
            a

        let enumerateFiles =
            match filter with
            | Some filter -> Directory.EnumerateFiles >> Seq.filter filter
            | _ -> Directory.EnumerateFiles

        let files =
            dirs
            |> List.collect (enumerateFiles >> Seq.toList >> advance)

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

    let reformatToExif = parseExifDateTime >> Option.map formatToExif

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
    open Progress
    type private IO = MF.ConsoleApplication.IO

    let rec retry attempts xA =
        if attempts > 0 then
            xA
            |> AsyncResult.bindError (fun _ -> asyncResult {
                do! AsyncResult.sleep 1000
                return! xA
            })
            |> retry (attempts - 1)
        else xA

    let retryMultiple xA = xA |> List.map (retry 5)

    let handleMultipleResultsBy io taskName sequential onError tasks = asyncResult {
        use progress = new Progress(io, taskName)

        return!
            tasks
            |> List.map (Async.tee (ignore >> progress.Advance))
            |> tee (List.length >> progress.Start)
            |> retryMultiple
            |> (
                if sequential
                    then AsyncResult.ofSequentialAsyncResults onError
                    else AsyncResult.ofParallelAsyncResults onError
            )
    }

    let handleMultipleResults ((_, output) as io) taskName =
        handleMultipleResultsBy io taskName (output.IsDebug())

    let handleMultipleAsyncsBy io taskName sequential onError tasks =
        asyncResult {
            use progress = new Progress(io, taskName)

            return!
                tasks
                |> List.map (Async.tee (ignore >> progress.Advance))
                |> tee (List.length >> progress.Start)
                |> (
                    if sequential
                        then AsyncResult.ofSequentialAsyncs onError
                        else AsyncResult.ofParallelAsyncs onError
                )
        }

    let handleMultipleAsyncs ((_, output) as io: IO) taskName =
        handleMultipleAsyncsBy io taskName (output.IsDebug())

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

[<System.Obsolete("Use from ConsoleApplication")>]
module CommandHelp =
    let commandHelp lines = lines |> String.concat "\n\n" |> Some

    /// Concat two lines into one line for command help, so they won't be separated by other empty line
    let inline (<+>) line1 line2 = sprintf "%s\n%s" line1 line2

[<RequireQualifiedAccess>]
module internal ProcessedItems =
    open System.Collections.Concurrent

    type private Storage<'Item> = ConcurrentQueue<'Item>
    type ProcessedItems<'Item> = private ProcessedItems of Storage<'Item>

    let create<'Item> () =
        Storage<'Item>() |> ProcessedItems

    let add<'Item> (ProcessedItems storage) (item: 'Item) =
        storage.Enqueue item

    let count: ProcessedItems<'Item> -> int = fun (ProcessedItems storage) ->
        storage.Count

    let clear: ProcessedItems<'Item> -> unit = fun (ProcessedItems storage) ->
        storage.Clear()

    let getAll<'Item> (ProcessedItems storage): 'Item seq =
        storage.GetEnumerator()
        |> Seq.unfold (fun e ->
            if e.MoveNext() then Some (e.Current, e) else None
        )
