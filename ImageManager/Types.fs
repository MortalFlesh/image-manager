module ImageManager.Types

type PrepareForSorting = {
    source: string
    target: string
    exclude: string list option
}
