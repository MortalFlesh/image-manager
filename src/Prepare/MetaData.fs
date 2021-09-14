namespace MF.ImageManager.Prepare

[<RequireQualifiedAccess>]
module MetaData =
    open System
    open System.IO
    open System.Collections.Generic
    open MF.ErrorHandling
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

        let forImage output (file: string) =
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

        let forVideo output ffmpeg path = asyncResult {
            try
                match ffmpeg with
                | FFMpeg.OnOther | FFMpeg.Empty -> return None
                | FFMpeg.OnWindows ffmpeg ->
                    if output.IsVeryVerbose() then output.Message <| sprintf "ffmpeg: %s" ffmpeg

                    let service = MediaToolkitService.CreateInstance(ffmpeg)

                    let! result =
                        service.ExecuteAsync(path |> FfTaskGetMetadata)
                        |> AsyncResult.ofTaskCatch PrepareError.Exception

                    return
                        match result.Metadata with
                        | IsNull -> None
                        | meta ->
                        match meta.Format with
                        | IsNull -> None
                        | format ->
                        match format.Tags with
                        | IsNull -> None
                        | tags ->
                        match tags.TryGetValue("creation_time") with
                        | true, createdAt ->
                            match createdAt |> DateTime.TryParse with
                            | true, createdAtDateTime -> Some createdAtDateTime
                            | _ -> None

                        | _ -> None
            with e ->
                output.Error <| sprintf "[Warning] Video metadata for %s could not be get due to: %s" path e.Message
                if output.IsVerbose() then output.Error <| sprintf "%A\n" e
                return None
        }

    let dateTimeOriginal output ffmpeg file: AsyncResult<DateTime option, PrepareError> = asyncResult {
        let dateTimeOriginal = file |> DateTimeOriginal.forImage output

        match dateTimeOriginal with
        | Some dateTimeOriginal -> return Some dateTimeOriginal
        | _ -> return! file |> DateTimeOriginal.forVideo output ffmpeg
    }
