namespace MF.ImageManager

[<RequireQualifiedAccess>]
module MetaData =
    open System
    open System.IO
    open System.Collections.Generic
    open ErrorHandling
    open MF.ConsoleApplication
    open MF.Utils

    [<RequireQualifiedAccess>]
    module private DateTimeOriginal =
        open MetadataExtractor

        let private tryFind (dir, tag) (attr: IReadOnlyList<Directory>) =
            attr
            |> Seq.tryFind (fun d -> d.Name = dir)
            |> Option.bind (fun dir ->
                dir.Tags
                |> Seq.tryFind (fun t -> t.HasName && t.Name = tag)
            )

        let forImage (output: Output) (file: string) =
            try
                file
                |> ImageMetadataReader.ReadMetadata
                |> tryFind ("Exif SubIFD", "Date/Time Original")
                |> Option.bind (fun t -> t.Description |> DateTime.parseExifDateTime)
            with e ->
                output.Error <| sprintf "[Warning] File %s could not be parsed due to %A." file e.Message
                if output.IsVerbose() then output.Error <| sprintf "Error:\n%A" e
                None

        open MediaToolkit.Services
        open MediaToolkit.Tasks

        let forVideo (output: Output) ffmpegPath path = async {
            try
                let cwd = Directory.GetCurrentDirectory()
                let (/) a b = Path.Combine(a, b)

                // win only
                let ffmpeg =
                    match ffmpegPath with
                    | Some path -> cwd / path / "ffmpeg.exe"
                    | _ -> cwd / "ffmpeg.exe"

                let ffmpegFullpath = ffmpeg |> Path.GetFullPath

                if output.IsVeryVerbose() then
                    output.Message <| sprintf "ffmpeg: %s (%s)" ffmpeg ffmpegFullpath

                if ffmpeg |> File.Exists |> not then failwithf "ffmpeg does not exists at %s" ffmpegFullpath

                let service = MediaToolkitService.CreateInstance(ffmpegFullpath)

                let! result =
                    service.ExecuteAsync(path |> FfTaskGetMetadata)
                    |> Async.AwaitTask

                let meta = result.Metadata

                match meta.Format.Tags.TryGetValue("creation_time") with
                | true, createdAt ->
                    return
                        match createdAt |> DateTime.TryParse with
                        | true, createdAtDateTime -> Some createdAtDateTime
                        | _ -> None

                | _ -> return None
            with e ->
                output.Error <| sprintf "[Warning] Video metadata for %s could not be get due to: %s" path e.Message
                if output.IsVerbose() then output.Error <| sprintf "%A" e
                return None
        }

    let dateTimeOriginal output ffmpegPath file: Async<DateTime option> = async {
        let dateTimeOriginal = file |> DateTimeOriginal.forImage output

        match dateTimeOriginal with
        | Some dateTimeOriginal -> return Some dateTimeOriginal
        | _ -> return! file |> DateTimeOriginal.forVideo output ffmpegPath
    }
