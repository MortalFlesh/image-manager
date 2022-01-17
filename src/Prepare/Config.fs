namespace MF.ImageManager.Prepare

open MF.ImageManager

type Config = {
    Source: string list
    Target: string
    TargetSubdirFallback: string option
    TargetDirMode: TargetDirMode
    TargetSubdir: TargetSubdir
    Exclude: string list option
    ExcludeList: string option
    Ffmpeg: FFMpeg
}

[<RequireQualifiedAccess>]
module Config =
    open FSharp.Data
    open MF.Utils
    open MF.ErrorHandling

    type private Config = JsonProvider<"src/Prepare/schema/config.json", SampleIsList = true>

    let parse targetDirMode config = result {
        let config = Config.Parse config
        let! ffmpeg = config.Ffmpeg |> FFMpeg.init

        return {
            Target = ""
            Source = config.Source |> Seq.toList
            TargetSubdirFallback = config.Fallback
            TargetDirMode = targetDirMode
            TargetSubdir =
                match config.TargetSubdir with
                | Some "year-month" -> ByYearAndMonth
                | Some "year" -> ByYear
                | Some "month" -> ByMonth
                | _ -> Flat
            Exclude =
                match config.Exclude |> Seq.toList with
                | [] -> None
                | exclude -> Some exclude
            ExcludeList = config.ExcludeList
            Ffmpeg = ffmpeg
        }
    }

    let combine config defaults =
        match defaults with
        | Some defaults ->
            { defaults with
                Target = config.Target
                Source = config.Source @ defaults.Source |> List.distinct
                TargetSubdirFallback =
                    match config.TargetSubdirFallback with
                    | Some targetSubdirFallback -> Some targetSubdirFallback
                    | _ -> defaults.TargetSubdirFallback
                Exclude =
                    match config.Exclude with
                    | None
                    | Some [] -> defaults.Exclude
                    | exclude -> exclude
                ExcludeList =
                    match config.ExcludeList with
                    | Some excludeList -> Some excludeList
                    | _ -> defaults.ExcludeList
            }
        | _ -> config
