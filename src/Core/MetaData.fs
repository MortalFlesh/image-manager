namespace MF.ImageManager

[<RequireQualifiedAccess>]
module MetaData =
    open System
    open System.Collections.Generic
    open Microsoft.Extensions.Logging
    open MF.ErrorHandling
    open MF.ConsoleApplication
    open MF.Utils

    [<RequireQualifiedAccess>]
    module private Meta =
        module private ImageMeta =
            open MetadataExtractor

            let private tryFind (attr: IReadOnlyList<Directory>) (dir, tag) =
                attr
                |> Seq.tryFind (fun d -> d.Name = dir)
                |> Option.bind (fun dir ->
                    dir.Tags
                    |> Seq.tryFind (fun t -> t.HasName && t.Name = tag)
                )
                |> Option.map (fun value -> tag, value)

            let forImage ((input, output): IO) (logger: ILogger) wanted (FullPath path) =
                try
                    let meta = path |> ImageMetadataReader.ReadMetadata

                    match input with
                    | Input.Option.Has CommonOptions.DebugMeta _ ->
                        meta
                        |> Seq.collect (fun dir -> [
                            [ "<c:gray>-----</c>"; "<c:gray>-----</c>" ]
                            [ "<c:dark-yellow|u>Dir</c>"; $"<c:dark-yellow|u>{dir.Name}</c>" ]

                            yield!
                                dir.Tags
                                |> Seq.map (fun t -> [
                                    (if t.HasName then $"<c:yellow>{t.Name}</c>" else "-")
                                    t.Description
                                ])
                        ])
                        |> List.ofSeq
                        |> output.Table [ "Meta"; "Value" ]
                    | _ -> ()

                    wanted |> List.choose (tryFind meta)
                with e ->
                    logger.LogWarning("File {file} could not be parsed due to {error}.", path, e)
                    []

        module private VideoMeta =
            open MediaToolkit.Services
            open MediaToolkit.Tasks

            let private tryFind (tags: Dictionary<string, string>) name =
                match tags.TryGetValue(name) with
                | true, String.IsEmpty -> None
                | true, value -> Some (name, value)
                | _ -> None

            let forVideo ((input, output): IO) (logger: ILogger) ffmpeg wanted (FullPath path) = asyncResult {
                try
                    match ffmpeg with
                    | FFMpeg.OnOther | FFMpeg.Empty -> return []
                    | FFMpeg.OnWindows ffmpeg ->
                        if output.IsVeryVerbose() then output.Message <| sprintf "ffmpeg: %s" ffmpeg

                        let service = MediaToolkitService.CreateInstance(ffmpeg)

                        let! (result: GetMetadataResult) =
                            service.ExecuteAsync(path |> FfTaskGetMetadata)
                            |> AsyncResult.ofTaskCatch PrepareError.Exception

                        return
                            match result.Metadata with
                            | IsNull -> []
                            | meta ->
                            match meta.Format with
                            | IsNull -> []
                            | format ->
                            match format.Tags with
                            | IsNull -> []
                            | tags ->
                                match input with
                                | Input.Option.Has CommonOptions.DebugMeta _ ->
                                    output.SubTitle ("Video: " + path)
                                    tags |> Seq.map (fun t -> [ t.Key; t.Value ]) |> List.ofSeq |> output.Table [ "Meta"; "Value" ]
                                | _ -> ()

                                wanted |> List.choose (tryFind tags)
                            | _ -> []
                with e ->
                    logger.LogWarning("Video metadata for {file} could not be get due to {error}.", path, e)
                    return []
            }

        let forImage = ImageMeta.forImage
        let forVideo = VideoMeta.forVideo

    [<RequireQualifiedAccess>]
    module private DateTimeOriginal =
        open MetadataExtractor

        let tryParse (logger: ILogger) file (tag: Tag) =
            try tag.Description |> DateTime.parseExifDateTime
            with e ->
                logger.LogWarning("File {file} could not be parsed due to {error}", file, e)
                None

    let private showParsed ((input, output): IO) name parsedMetadata =
        match input with
        | Input.Option.Has CommonOptions.DebugMeta _ ->
            output.Section("%s matadata", name)

            parsedMetadata
            |> List.sortBy fst
            |> List.map (fun (meta, value) -> [ $"{meta}"; value ])
            |> output.Table [ "Meta"; "Value" ]
        | _ -> ()

    let find ((_, output) as io: IO) (loggerFactory: ILoggerFactory) ffmpeg file: AsyncResult<Map<MetaAttribute, string>, PrepareError> = asyncResult {
        if output.IsDebug() then
            output.Message $"<c:dark-yellow>[Debug]</c> Fetching metadata for <c:cyan>{file}</c> ..."

        let! parsedMetadata =
            match file with
            | Image, path ->
                let logger = loggerFactory.CreateLogger("MetaData.Image")

                path
                |> Meta.forImage io logger [
                    "Exif SubIFD", "Date/Time Original"
                    "Exif IFD0", "Model"
                    "GPS", "GPS Latitude"
                    "GPS", "GPS Longitude"
                    "GPS", "GPS Altitude"
                ]
                |> List.choose (function
                    | MetaAttribute.KeyCreatedAt, value ->
                        value
                        |> DateTimeOriginal.tryParse logger file
                        |> Option.map (fun createdAt -> CreatedAt, string createdAt)
                    | MetaAttribute.KeyModel, value -> Some (Model, value.Description)
                    | MetaAttribute.KeyGpsLatitude, value -> Some (GpsLatitude, value.Description)
                    | MetaAttribute.KeyGpsLongitude, value -> Some (GpsLongitude, value.Description)
                    | MetaAttribute.KeyGpsAltitude, value -> Some (GpsAltitude, value.Description)
                    | _ -> None
                )
                |> AsyncResult.ofSuccess

            | Video, path ->
                asyncResult {
                    let! meta =
                        path
                        |> Meta.forVideo io (loggerFactory.CreateLogger("MetaData.Video")) ffmpeg [
                            "creation_time"; "com.apple.quicktime.creationdate"
                            "model"; "com.apple.quicktime.model"
                            "location"; "com.apple.quicktime.location.ISO6709"
                        ]

                    return
                        meta
                        |> List.sortBy (fst >> String.length) // move com.apple.... meta down in the list to prefer them
                        |> List.choose (function
                            | "creation_time", createdAt
                            | "com.apple.quicktime.creationdate", createdAt -> Some (CreatedAt, createdAt)

                            | "model", model
                            | "com.apple.quicktime.model", model -> Some (Model, model)

                            | "location", location
                            | "com.apple.quicktime.location.ISO6709", location -> Some (GpsIso6709, location)

                            | _ -> None
                        )
                }

        return
            parsedMetadata
            |> tee (showParsed io "Parsed")
            |> Map.ofList
    }
