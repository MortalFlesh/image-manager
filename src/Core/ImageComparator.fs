namespace MF.ImageManager.ImageComparator

open System
open System.Drawing
open System.IO
open MF.ConsoleApplication
open MF.ErrorHandling
open MF.Utils
open MF.ImageManager

type private DomainImage = MF.ImageManager.File

type ImageHash = ImageHash of int array

[<Serializable>]
type ImageWithHash = {
    Image: DomainImage
    Hash: ImageHash
    Width: int
    Height: int
}

type ImageHashError =
    | FileIsNotImage
    | Runtime of exn

[<RequireQualifiedAccess>]
module ImageHash =
    // See https://stackoverflow.com/questions/35151067/algorithm-to-compare-two-images-in-c-sharp/35153895

    let generate (image: Image) =
        let tWidth = 32
        let tHeight = tWidth / image.Width * image.Height

        use thumbnail: Bitmap =
            new Bitmap(image.GetThumbnailImage(tWidth, tHeight, Image.GetThumbnailImageAbort(fun () -> false), IntPtr.Zero))

        seq {
            for h in 0 .. thumbnail.Height - 1 do
                for w in 0 .. thumbnail.Width - 1 do
                    // todo - could work with .GetBrightness() < 0.5f)
                    yield thumbnail.GetPixel(w, h).ToArgb()
        }
        |> Seq.toArray
        |> ImageHash

    let value (ImageHash hash) = hash
    let format = value >> Array.map string >> String.concat ""

    /// It will compare the hash of 2 images and calculate an average difference between all the positions
    let compare (ImageHash h1) (ImageHash h2): float =
        match h1, h2 with
        | h1, h2 when h1.Length <> h2.Length -> 0
        | h1, h2 ->
            seq {
                for i in 0 .. h1.Length - 1 do
                    match float h1[i], float h2[i] with
                    | 0., 0. -> 1.
                    | 0., _ | _, 0. -> 0.
                    | h1, h2 when h1 < h2 -> h1 / h2
                    | h1, h2 -> h2 / h1
            }
            |> Seq.average
            |> (*) 100.

[<RequireQualifiedAccess>]
module ImageWithHash =
    let fromImage (output: Output) (image: DomainImage) = asyncResult {
        if image.Type <> Image then
            return! AsyncResult.ofError FileIsNotImage

        if output.IsDebug() then output.Message $"Generating hash for <c:yellow>{image.Name |> FileName.value}</c>"

        try
            let (FullPath path) = image.FullPath
            // this works on Windows platform only
            use bitmap = Bitmap.FromFile(path, true)

            if output.IsDebug() then
                output.Message (
                    String.concat "\n" [
                        sprintf "HorizontalResolution: %A" bitmap.HorizontalResolution
                        sprintf "HorizontalResolution: %A" bitmap.Palette
                        sprintf "HorizontalResolution: %A" bitmap.PhysicalDimension
                    ]
                )

            return {
                Image = image
                Hash = ImageHash.generate bitmap
                Width = bitmap.Size.Width
                Height = bitmap.Size.Height
            }
        with e ->
            return! AsyncResult.ofError (Runtime e)
    }

    let image { Image = image } = image

[<RequireQualifiedAccess>]
module ImageComparator =
    let findSimilar (similarity: float) (images: ImageWithHash list) =
        seq {
            let similarity = max similarity 0.
            let similarity = min similarity 100.

            for a in 0 .. images.Length - 2 do
                for b in a + 1 .. images.Length - 1 do
                    let diff = ImageHash.compare images[a].Hash images[b].Hash
                    if diff >= similarity then
                        yield diff, [images[a]; images[b]]
        }
        |> Seq.toList
