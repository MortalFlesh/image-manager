namespace MF.ImageManager

type SameImageGroup = string * File list

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
        Image: File
        CompleteHash: string
        DateTimeOriginal: string
        Gps: string
    }

    [<RequireQualifiedAccess>]
    module private TableItem =
        let create (image: File) =
            let metadata =
                image
                |> FileMetadata.load
                |> Result.orFail

            {
                Image = image
                CompleteHash = metadata |> Map.values |> String.concat "-"
                DateTimeOriginal = image |> File.createdAtRaw |> Option.defaultValue ""
                Gps =
                    [
                        metadata.TryFind GpsLatitude
                        metadata.TryFind GpsLongitude
                        metadata.TryFind GpsAltitude
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

    let private findImageByContent output (images: File list) = asyncResult {
        let formatError (e: exn) =
            if output.IsVeryVerbose() then sprintf "%A" e
            else e.Message

        let! imagesWithHash =
            output.Message $"Loading images..."
            use progress = new Progress(output, "Find images content hash.")
            images
            |> tee (List.length >> progress.Start)
            |> List.map (ImageWithHash.fromImage output >> tee (fun _ -> progress.Advance()))
            |> AsyncResult.handleMultipleResults output id <@> List.map formatError

        if output.IsDebug() then
            imagesWithHash
            |> List.map (fun i -> [
                (i.Image.Name |> FileName.value)
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
                    yield! group |> List.collect (fun i -> [$"{i.Image.Name |> FileName.value} ({i.Image.FullPath})"; $"{i.Width}x{i.Height}"])
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

    let findSameImages output (images: File list): AsyncResult<SameImageAdepts, string list> = asyncResult {
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
                |> List.map File.path
                |> Set.ofList

            images
            |> List.filter (File.path >> exlude.Contains >> not)
            |> findImageByContent output
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
