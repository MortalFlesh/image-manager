namespace MF.ImageManager

[<RequireQualifiedAccess>]
module MetaData =
    open System
    open System.IO
    open System.Collections.Generic
    open MF.ErrorHandling
    open MF.ConsoleApplication
    open MF.Utils

    [<RequireQualifiedAccess>]
    module private Meta =
        open MetadataExtractor

        let private tryFind (attr: IReadOnlyList<Directory>) (dir, tag) =
            attr
            |> Seq.tryFind (fun d -> d.Name = dir)
            |> Option.bind (fun dir ->
                dir.Tags
                |> Seq.tryFind (fun t -> t.HasName && t.Name = tag)
            )
            |> Option.map (fun value -> tag, value)

        let forImage output ignoreWarnings wanted (file: string) =
            try
                let meta = file |> ImageMetadataReader.ReadMetadata

                (* meta
                |> Seq.iter (fun dir ->
                    output.Message $"dir: {dir.Name}"
                    dir.Tags |> Seq.map (fun t -> if t.HasName then t.Name else "-") |> Seq.toList |> output.List |> output.NewLine
                ) *)

                wanted |> List.choose (tryFind meta)
            with e ->
                if not ignoreWarnings then
                    output.Error <| sprintf "[Warning] File %s could not be parsed due to %A." file e.Message
                if output.IsVerbose() then output.Error <| sprintf "Error:\n%A" e
                []

        open MediaToolkit.Services
        open MediaToolkit.Tasks

        let forVideo output ignoreWarnings ffmpeg path = asyncResult {
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
                if not ignoreWarnings then
                    output.Error <| sprintf "[Warning] Video metadata for %s could not be get due to: %s" path e.Message
                if output.IsVerbose() then output.Error <| sprintf "%A\n" e
                return None
        }

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

        let parse output file (tag: Tag) =
            try tag.Description |> DateTime.parseExifDateTime
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

    let find output ignoreWarnings ffmpeg file: AsyncResult<DateTime option * Map<string, string>, PrepareError> = asyncResult {
        let parsedMetadata =
            file
            |> Meta.forImage output ignoreWarnings [
                "Exif SubIFD", "Date/Time Original"
                "Exif IFD0", "Model"
            ]

        let dateTimeOriginal =
            parsedMetadata
            |> List.tryFind (fst >> (=) "Date/Time Original")
            |> Option.bind (snd >> DateTimeOriginal.parse output file)

        return
            dateTimeOriginal,
            parsedMetadata
            |> List.filter (fst >> (<>) "Date/Time Original")
            |> List.map (fun (k, t) -> k, t.Description)
            |> Map.ofList
    }
