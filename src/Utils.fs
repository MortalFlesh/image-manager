namespace MF.Utils

module FileSystem =
    open System.IO

    let rec getAllFiles = function
        | [] -> []
        | directories -> [
            yield! directories |> Seq.collect Directory.EnumerateFiles
            yield! directories |> Seq.collect Directory.EnumerateDirectories |> List.ofSeq |> getAllFiles
        ]

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
