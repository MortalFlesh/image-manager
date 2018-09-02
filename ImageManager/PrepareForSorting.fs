module ImageManager.PrepareForSorting

open ImageManager.Types

let prepareForSorting prepare =
    printfn "from %s to %s" prepare.source prepare.target
    ("Done", 0)
