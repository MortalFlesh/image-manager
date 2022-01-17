namespace MF.ImageManager

open System
open MF.Utils

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
    | Override
    | Exclude
    | DryRun

type TargetSubdir =
    | Flat
    | ByMonth
    | ByYear
    | ByYearAndMonth

type Prefix = Prefix of string

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

    let extensions = formats |> Set.map (String.toLower >> (+) ".")

[<RequireQualifiedAccess>]
module Image =
    /// https://en.wikipedia.org/wiki/Video_file_format
    let formats = Set [
        "gif"
        "jpg"; "jpeg"
        "png"
        "bmp"
    ]

    let extensions = formats |> Set.map (String.toLower >> (+) ".")

type FileType =
    | Image of string
    | Video of string

[<RequireQualifiedAccess>]
module FileType =
    open System.IO

    let determine (path: string) =
        let extension = path |> Path.GetExtension |> String.toLower

        match path, extension with
        | null, _ | "", _
        | _, null | _, "" -> None

        | video, extension when extension |> Video.extensions.Contains -> Some (Video video)
        | image, extension when extension |> Image.extensions.Contains -> Some (Image image)

        | _ -> None

type Hash = Hash of string

type File = {
    Type: FileType
    Name: string
    Hash: Hash option
    FullPath: string
    Metadata: Map<MetaAttribute, string>
}

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
    open MF.ErrorHandling
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
                | FileType.Image _ -> Some "i"
                | FileType.Video _ -> Some "v"

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
    let hash { Hash = hash } = hash

    let createdAtRaw { Metadata = metaData } = metaData.TryFind CreatedAt
    let createdAtDateTime = createdAtRaw >> Option.bind DateTime.parseExifDateTime
    let model { Metadata = metaData } = metaData.TryFind Model

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
        [ PlatformID.Win32NT; PlatformID.Win32S; PlatformID.Win32Windows; PlatformID.WinCE ] |> List.contains Environment.OSVersion.Platform

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
