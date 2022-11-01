namespace MF.ImageManager

open System
open System.IO
open MF.ErrorHandling
open MF.Utils

// todo - rename error
[<RequireQualifiedAccess>]
type PrepareError =
    | Exception of exn
    | ErrorMessage of string

[<RequireQualifiedAccess>]
module PrepareError =
    let format = function
        | PrepareError.Exception e -> e.Message
        | PrepareError.ErrorMessage e -> e

type TargetDirMode =
    // | UseSubdir of string    // todo - je treba vic promyslet
    | Override
    | Exclude
    | DryRun

type TargetSubdir =
    | Flat
    | ByMonth
    | ByYear
    | ByYearAndMonth

type MetaAttribute =
    /// "Exif SubIFD" => "Date/Time Original"
    | CreatedAt
    /// "Exif IFD0" => "Model"
    | Model
    /// "GPS" => "GPS Latitude"
    | GpsLatitude
    /// "GPS" => "GPS Longitude"
    | GpsLongitude
    /// "GPS" => "GPS Altitude"
    | GpsAltitude
    /// GPS in ISO6709 format (Latitude+Longitude+Altitude)
    /// see: https://en.wikipedia.org/wiki/ISO_6709
    /// example: +49.6196+018.2302+478.329/
    /// other: https://www.codeproject.com/Articles/151869/Parsing-Latitude-and-Longitude-Information
    | GpsIso6709
    | Other

type FullPath =
    FullPath of string
    with
        member this.Value =
            let (FullPath path) = this
            path

        member this.GetDirectoryName() =
            this.Value |> Path.GetDirectoryName

        member this.Replace((find: string), (replace: string)) =
            this.Value.Replace(find, replace) |> FullPath

[<RequireQualifiedAccess>]
module FullPath =
    let value (FullPath path) = path

[<AutoOpen>]
module ExtensionModule =
    /// File extension, including a leading "."
    type Extension = private Extension of string

    [<RequireQualifiedAccess>]
    module Extension =
        open System.IO

        let private create (extension: string) =
            extension.Trim().ToLower() |> Extension

        let ofParsed = function
            | null | "" -> None
            | withoutLeadingDot when withoutLeadingDot.StartsWith('.') |> not -> None
            | ext -> Some (create ext)

        let fromPath (path: string) =
            path |> Path.GetExtension |> create

        let value (Extension ext) = ext

[<RequireQualifiedAccess>]
module Video =
    /// https://en.wikipedia.org/wiki/Video_file_format
    let formats = Set [
        "webm"
        "mkv"
        "flv"
        "vob"
        "ogb"; "ogg"
        "drc"
        "gifv"
        "mng"
        "avi"
        "MTS"; "M2TS"; "TS"
        "mov"; "qt"
        "wmv"
        "yuv"
        "rm"
        "rmvb"
        "viv"
        "asf"
        "amv"
        "mp4"; "m4p"; "m4v"
        "mpg"; "mp2"; "mpeg"; "m2v"
        "svi"
        "3gp"
        "3g2"
        "mxf"
        "roq"
        "nsv"
        "flv"; "f4v"; "f4p"; "f4a"; "f4b"
    ]

    let private extensions = formats |> Set.map (String.toLower >> (+) ".")

    let isVideoExtension =
        Extension.value >> String.toLower >> extensions.Contains

[<RequireQualifiedAccess>]
module Image =
    /// https://en.wikipedia.org/wiki/Video_file_format
    let formats = Set [
        "gif"
        "jpg"; "jpeg"
        "png"
        "bmp"
        "heic"
    ]

    let private extensions = formats |> Set.map (String.toLower >> (+) ".")

    let isImageExtension =
        Extension.value >> String.toLower >> extensions.Contains

type FileType =
    // todo - rename NoPath to default
    | Image
    | Video

[<RequireQualifiedAccess>]
module FileType =
    let determine (path: string) =
        match path |> Extension.fromPath with
        | extension when extension |> Video.isVideoExtension -> Some Video
        | extension when extension |> Image.isImageExtension -> Some Image

        | _ -> None

type Hash = Hash of string

type FileName =
    | Hashed of Hash * Extension
    | Normal of string

[<RequireQualifiedAccess>]
type FileMetadata =
    | Lazy of AsyncResult<Map<MetaAttribute, string>, PrepareError>

type File = {
    Type: FileType
    Name: FileName
    FullPath: FullPath
    Metadata: FileMetadata
}

type FileToCopy = {
    Source: File
    Target: File
}

[<RequireQualifiedAccess>]
module FileToCopy =
    let source ({ Source = file }) = file
    let target ({ Target = file }) = file

[<RequireQualifiedAccess>]
module FileMetadata =
    open MF.Utils.ConcurrentCache

    type private Metadata = Map<MetaAttribute, string>

    let private cache: Cache<FullPath, Metadata> = Cache.empty()

    let loadAsync (output: MF.ConsoleApplication.Output) (file: File) = asyncResult {
        if output.IsDebug() then
            output.Message $"<c:dark-yellow>[Debug] Loading metadata for file {file.Name}</c>"

        match file.Metadata with
        | FileMetadata.Lazy load ->
            let key = Key file.FullPath

            match cache |> Cache.tryFind key with
            | Some cached ->
                if output.IsDebug() then output.Message "  └──> Metadata loaded from cache"
                return cached
            | _ ->
                if output.IsDebug() then output.Message "  ├──> Load fresh Metadata ..."
                let! fresh = load
                if output.IsDebug() then output.Message "  ├──> Fresh Metadata are loaded"
                cache |> Cache.set key fresh
                if output.IsDebug() then output.Message "  └──> Fresh Metadata stored in cache"

                return fresh
    }

    let load output (file: File) =
        file |> loadAsync output |> Async.RunSynchronously

[<RequireQualifiedAccess>]
module private Crypt =
    open System.Text
    open System.Security.Cryptography

    let private hash compute (v: string) =
        v
        |> Encoding.ASCII.GetBytes
        |> compute
        |> System.BitConverter.ToString
        |> String.replace "-" ""
        |> String.toLower

    let private hashBy alg = hash (HashAlgorithm.Create(alg).ComputeHash)

    let sha1 = hashBy "SHA1"
    let sha256 = hashBy "SHA256"
    let md5 = hashBy "MD5"

    let crc32 = Crc32.crc32OfString

[<RequireQualifiedAccess>]
module Hash =
    open MF.ErrorHandling.Option.Operators

    let value (Hash hash) = hash

    let calculate fileType (metadata: Map<MetaAttribute, string>) =
        let value key = metadata |> Map.tryFind key
        let noSpaces = String.replace " " ""
        let clearChars toReplace string =
            toReplace
            |> List.distinct
            |> List.fold (fun result char -> result |> String.replace char "") string

        let clear =
            [
                match fileType with
                | FileType.Image -> Some "i"
                | FileType.Video -> Some "v"

                value CreatedAt
                >>= DateTime.parseExifDateTime
                <!> fun d -> d.ToString("yyyyMMddTHHmmss")
            ]
            |> List.choose id
            |> String.concat "_"

        let crypted =
            [
                value Model
                <!> noSpaces

                value GpsIso6709
                <??> (maybe {
                    let! latitude = value GpsLatitude
                    let! longitude = value GpsLongitude
                    let! altitude = value GpsAltitude

                    return $"{latitude}--{longitude}--{altitude}"
                })
                <!> noSpaces
            ]
            |> List.choose id
            |> String.concat "_"
            |> Crypt.crc32

        $"{clear}_{crypted}"
        |> clearChars [
            "/"; "\\"; "?"; "%"; "*"; ":"; "|"; "\""; "<"; ">"; "."; ","; ";"; "="
            yield! IO.Path.GetInvalidFileNameChars() |> Seq.map string
        ]
        |> Hash

    let tryParse = function
        | null | "" -> None
        | Regex @"^([i|v])(_.*)(\..*?)$" [ prefix; hash; extension ] ->
            match prefix, extension |> Extension.ofParsed with
            | "i", Some extension when extension |> Image.isImageExtension ->
                Some (Hash $"{prefix}{hash}", extension)

            | "v", Some extension when extension |> Video.isVideoExtension ->
                Some (Hash $"{prefix}{hash}", extension)

            | _ -> None
        | _ -> None

    let tryGetCreated = function
        | Hash (Regex @"^(i|v)_(\d{4})(\d{2})" [ _prefix; year; month ]) -> Some (int year, int month)
        | _ -> None

    [<RequireQualifiedAccess>]
    module Cache =
        open Microsoft.Extensions.Logging
        open MF.Utils.Progress
        open MF.Utils.ConcurrentCache

        let private cache: Cache<FullPath, Hash> = Cache.empty()
        let private cachePath = "./.hash-cache.txt"

        let load (loggerFactory: ILoggerFactory): AsyncResult<unit, PrepareError> =
            asyncResult {
                let! lines = cachePath |> File.ReadAllLinesAsync

                lines
                |> Seq.iter (function
                    | null | "" -> ()
                    | Regex @"^(.*):(.*?)$" [ fullPath; hash ] -> cache |> Cache.set (Key (FullPath fullPath)) (Hash hash)
                    | _ -> ()
                )

                return ()
            }
            |> AsyncResult.mapError PrepareError.Exception
            |> AsyncResult.bindError (fun e ->
                loggerFactory.CreateLogger("Hash.Cache.load").LogWarning("Error: {error}", e |> PrepareError.format)
                AsyncResult.ofSuccess ()
            )

        let clear: AsyncResult<unit, PrepareError> = asyncResult {
            try
                cache |> Cache.clear
                File.Delete cachePath
            with e ->
                return! AsyncResult.ofError (PrepareError.Exception e)
        }

        let init ((_, output: MF.ConsoleApplication.Output) as io) (loggerFactory: ILoggerFactory) (files: File list): AsyncResult<unit, PrepareError list> = asyncResult {
            let logger = loggerFactory.CreateLogger("Cache.init")
            let debugMessage message =
                if output.IsDebug() then output.Message <| sprintf "<c:dark-yellow>[Debug] %s</c>" message

            debugMessage "Loading existing cache ..."
            do! load loggerFactory |> AsyncResult.mapError List.singleton

            debugMessage "Init hashes for files ..."
            let progress = new Progress(io, "Init cache")

            let keys = cache |> Cache.keys |> Set.ofList

            let! (hashes: (FullPath * Hash) list) =
                files
                |> List.filter (fun file -> file.FullPath |> Key |> keys.Contains |> not)
                |> List.choose (function
                    | { Name = Hashed _ } -> None
                    | { FullPath = path; Type = fileType } as file ->
                        Some (asyncResult {
                            let! metadata =
                                file
                                |> FileMetadata.load output

                            if metadata.IsEmpty then
                                logger.LogWarning("File {file} has no metadata.", file)
                                return None
                            else
                                let hash = calculate fileType metadata
                                debugMessage <| sprintf "Calculated hash for </c><c:cyan>%A</c><c:dark-yellow> is </c><c:magenta>%A</c><c:dark-yellow>" path hash
                                return Some (path, hash)
                        })
                )
                |> List.map (Async.tee (ignore >> progress.Advance))
                |> tee (List.length >> progress.Start)
                |> AsyncResult.handleMultipleResults output PrepareError.Exception
                |> AsyncResult.map (List.choose id)
            progress.Finish()

            debugMessage $"Set hashes [{hashes.Length}] to cache ..."
            hashes
            |> List.iter (fun (path, hash) ->
                cache |> Cache.set (Key path) hash
            )

            debugMessage $"Write current cache [{cache |> Cache.length}] ..."
            let lines =
                cache
                |> Cache.items
                |> List.map (fun (Key path, (Hash hash)) ->
                    sprintf "%s:%s" path.Value hash
                )
                |> List.sort

            do! File.WriteAllLinesAsync(cachePath, lines) |> AsyncResult.ofEmptyTaskCatch (PrepareError.Exception >> List.singleton)

            return ()
        }

        let tryFind fullPath: Hash option =
            cache |> Cache.tryFind (Key fullPath)

[<RequireQualifiedAccess>]
module FileName =
    let value = function
        | Hashed (Hash hash, extension) -> sprintf "%s%s" hash (extension |> Extension.value)
        | Normal name -> name

    let tryParse = function
        | null | "" -> None
        | name ->
            match name |> Hash.tryParse with
            | Some name -> Some (Hashed name)
            | _ -> Some (Normal name)

[<RequireQualifiedAccess>]
module MetaAttribute =
    let [<Literal>] KeyCreatedAt = "Date/Time Original"
    let [<Literal>] KeyModel = "Model"
    let [<Literal>] KeyGpsLatitude = "GPS Latitude"
    let [<Literal>] KeyGpsLongitude = "GPS Longitude"
    let [<Literal>] KeyGpsAltitude = "GPS Altitude"
    let [<Literal>] KeyGpsIso6709 = "GPS ISO6709"
    let [<Literal>] KeyOther = "Other"

    let value = function
        | CreatedAt -> KeyCreatedAt
        | Model -> KeyModel
        | GpsLatitude -> KeyGpsLatitude
        | GpsLongitude -> KeyGpsLongitude
        | GpsAltitude -> KeyGpsAltitude
        | GpsIso6709 -> KeyGpsIso6709
        | Other -> KeyOther

[<RequireQualifiedAccess>]
module File =
    let name { Name = name } = name
    let path { FullPath = path } = path

    let hash = function
        | { Name = Hashed (hash, _) } -> Some hash
        | _ -> None

    let inline private (>!>) f1 f2 = f1 >> Async.map f2

    let private metaDataAsync output file = async {
        let! meta = file |> FileMetadata.loadAsync output

        return meta |> Result.toOption
    }

    let createdAtRawAsync output: File -> Async<string option> =
        metaDataAsync output >!> Option.bind (Map.tryFind CreatedAt)

    let createdAtDateTimeAsync output: File -> Async<DateTime option> =
        createdAtRawAsync output >!> Option.bind DateTime.parseExifDateTime

    let modelAsync output: File -> Async<string option> =
        metaDataAsync output >!> Option.bind (Map.tryFind Model)

[<RequireQualifiedAccess>]
type FFMpeg =
    | OnWindows of path: string
    | OnOther
    /// Not set from whatever reason
    | Empty

[<RequireQualifiedAccess>]
module FFMpeg =
    open System
    open System.IO

    let private runOnWindows () =
        [ PlatformID.Win32NT; PlatformID.Win32S; PlatformID.Win32Windows; PlatformID.WinCE ]
        |> List.contains Environment.OSVersion.Platform

    let empty = FFMpeg.Empty

    let init path =
        try
            if runOnWindows() then
                let cwd = Directory.GetCurrentDirectory()
                let (/) a b = Path.Combine(a, b)

                let fullPath =
                    match path with
                    | Some path -> cwd / path / "ffmpeg.exe"
                    | _ -> cwd / "ffmpeg.exe"
                    |> Path.GetFullPath

                if fullPath |> File.Exists |> not then
                    let atPath = path |> Option.map (sprintf "at %s ") |> Option.defaultValue ""
                    Error (PrepareError.ErrorMessage $"ffmpeg does not exists {atPath}in {cwd} (fullPath: {fullPath})")
                else
                    Ok (FFMpeg.OnWindows fullPath)
            else
                Ok FFMpeg.OnOther
        with e -> Error (PrepareError.Exception e)

//
// Command errors
//

type PrepareFilesError =
    | PrepareError of PrepareError
    | PrepareErrors of PrepareError list
    | RuntimeError of exn

[<RequireQualifiedAccess>]
module PrepareFilesError =
    let format = function
        | PrepareError e -> PrepareError.format e
        | PrepareErrors e ->
            e
            |> List.map PrepareError.format
            |> String.concat "\n"
        | RuntimeError e -> $"Preparing ends with runtime error {e}"
