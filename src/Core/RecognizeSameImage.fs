namespace MF.ImageManager

type SameImageGroup = string * Image list

type SameImageAdepts = {
    ByCompleteHash: SameImageGroup list
    ByDateTimeOriginal: SameImageGroup list
    ByGps: SameImageGroup list
    ByContent: SameImageGroup list
}

[<RequireQualifiedAccess>]
module RecognizeSameImage =
    open System
    open MF.ConsoleApplication
    open MF.Utils
    open MF.Utils.Progress
    open MF.ErrorHandling
    open MF.ErrorHandling.AsyncResult.Operators

    type private TableItem = {
        Image: Image
        CompleteHash: string
        DateTimeOriginal: string
        Gps: string
    }

    [<RequireQualifiedAccess>]
    module private TableItem =
        let create (image: Image) =
            {
                Image = image
                CompleteHash = image.Metadata |> Map.values |> String.concat "-"
                DateTimeOriginal = image |> Image.createdAtRaw |> Option.defaultValue ""
                Gps =
                    [
                        image.Metadata.TryFind GpsLatitude
                        image.Metadata.TryFind GpsLongitude
                        image.Metadata.TryFind GpsAltitude
                    ]
                    |> List.choose id
                    |> String.concat "-"
            }

        let image { Image = image } = image
        let completeHash { CompleteHash = hash } = hash
        let dateTimeOriginal { DateTimeOriginal = value } = value
        let gps { DateTimeOriginal = value } = value

    let private prepareTable output images =
        use progress = new Progress(output, "Prepare images table")
        images
        |> tee (List.length >> progress.Start)
        |> List.map (TableItem.create >> tee (fun _ -> progress.Advance()))

    let private findBy output name exclude (f: TableItem -> string) images =
        use progress = new Progress(output, $"Finding by {name}")
        images
        |> tee (List.length >> progress.Start)
        |> List.filter (exclude >> not)
        |> List.groupBy (f >> tee (fun _ -> progress.Advance()))
        |> List.filter (fun (group, items) -> group.Length > 0 && items.Length > 1)
        |> List.map (fun (group, items) -> group, items |> List.map TableItem.image )

    let private findImageByMetadata output images = asyncResult {
        let imagesTable = images |> prepareTable output

        let byCompleteHash =
            imagesTable
            |> findBy output "Complete hash"
                (fun _ -> false)
                TableItem.completeHash

        let excludeByCompleteHashSet =
            byCompleteHash
            |> List.map fst
            |> Set.ofList

        let byDateTimeOriginal =
            imagesTable
            |> findBy output "Date/Time original"
                (TableItem.completeHash >> excludeByCompleteHashSet.Contains)
                TableItem.dateTimeOriginal

        let excludeByDateTime =
            byDateTimeOriginal
            |> List.map fst
            |> Set.ofList

        let byGps =
            imagesTable
            |> findBy output "GPS"
                (fun i ->
                    (i |> TableItem.completeHash |> excludeByCompleteHashSet.Contains)
                    ||
                    (i |> TableItem.dateTimeOriginal |> excludeByDateTime.Contains)
                )
                TableItem.gps

        return byCompleteHash, byDateTimeOriginal, byGps
    }

    open MF.ImageManager.ImageComparator

    let private findImageByContent output cache (images: Image list) = asyncResult {
        let formatError (e: exn) =
            if output.IsVeryVerbose() then sprintf "%A" e
            else e.Message

        let loadFresh () =
            output.Message $"Loading images..."
            use progress = new Progress(output, "Find images content hash.")
            images
            |> tee (List.length >> progress.Start)
            |> List.map (ImageWithHash.fromImage output >> tee (fun _ -> progress.Advance()))
            |> AsyncResult.handleMultipleResults output id <@> List.map formatError

        let loadFromCache file =
            try
                output.Message $"Loading images from cache <c:dark-yellow>{file}</c>..."
                file
                |> ImagesWithHashList.deserialize
                |> Option.defaultValue []
                |> AsyncResult.ofSuccess
            with e ->
                AsyncResult.ofError e

        let! imagesWithHash =
            match cache with
            | NoCache -> loadFresh ()

            | FromFile file ->
                loadFromCache file
                >>- fun e ->
                    output.Error e.Message
                    output.Message "<c:yellow>! Skip cache and load fresh ...</c>"
                    loadFresh ()

            | FreshAndCacheResultToFile file ->
                loadFresh ()
                >>* ImagesWithHashList.serialize file

        if output.IsDebug() then
            imagesWithHash
            |> List.map (fun i -> [
                i.Image.Name
                $"{i.Width}x{i.Height}"
                (i.Hash |> ImageHash.format)
            ])
            |> output.Table [ "Name"; "Size"; "Hash" ]

        let similarTo90 =
            imagesWithHash
            |> ImageComparator.findSimilar 90

        if output.IsVeryVerbose() then
            similarTo90
            |> List.iter (fun (diff, group) ->
                [[
                    yield! group |> List.collect (fun i -> [$"{i.Image.Name} ({i.Image.FullPath})"; $"{i.Width}x{i.Height}"])
                    $"{diff} %%"
                ]]
                |> output.Table [ "Image 1"; "Size"; "Image 2"; "Size"; "Similarity" ]
            )

        return
            similarTo90
            |> List.sortByDescending fst
            |> List.map (fun (diff, group) ->
                $"{diff} %%", group |> List.map ImageWithHash.image
            )
    }

    let findSameImages output cache (images: Image list): AsyncResult<SameImageAdepts, string list> = asyncResult {
        let! byCompleteHash, byDateTimeOriginal, byGps = images |> findImageByMetadata output

        let! byContent =
            let exlude =
                [
                    byCompleteHash
                    byDateTimeOriginal
                    byGps
                ]
                |> List.concat
                |> List.collect snd
                |> List.map Image.path
                |> Set.ofList

            images
            |> List.filter (Image.path >> exlude.Contains >> not)
            |> findImageByContent output cache
            >>- fun errors ->
                if output.IsVerbose() then errors |> List.iter output.Error
                AsyncResult.ofSuccess []

        return {
            ByCompleteHash = byCompleteHash
            ByDateTimeOriginal = byDateTimeOriginal
            ByGps = byGps
            ByContent = byContent
        }
    }
