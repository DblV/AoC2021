open Depth

[<EntryPoint>]
let main args =
    printfn "%i" (countIncreases (Lib.readLines args[0]))

    0 // return an integer exit code
