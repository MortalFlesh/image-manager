namespace MF.ImageManager

open System
open MF.Utils

[<RequireQualifiedAccess>]
type PrepareError =
    | Exception of exn
    | ErrorMessage of string

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

type Image = {
    Name: string
    FullPath: string
    Metadata: Map<MetaAttribute, string>
}

[<RequireQualifiedAccess>]
module MetaAttribute =
    let [<Literal>] createdAt = "Date/Time Original"
    let [<Literal>] model = "Model"
    let [<Literal>] gpsLatitude = "GPS Latitude"
    let [<Literal>] gpsLongitude = "GPS Longitude"
    let [<Literal>] gpsAltitude = "GPS Altitude"

    let value = function
        | CreatedAt -> createdAt
        | Model -> model
        | GpsLatitude -> gpsLatitude
        | GpsLongitude -> gpsLongitude
        | GpsAltitude -> gpsAltitude

[<RequireQualifiedAccess>]
module Image =
    let name { Name = name } = name

    let createdAtRaw { Metadata = metaData } = metaData.TryFind CreatedAt
    let createdAtDateTime = createdAtRaw >> Option.map DateTime.Parse
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
