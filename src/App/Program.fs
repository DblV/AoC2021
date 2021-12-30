open Polymerisation

[<EntryPoint>]
let main args =
    printfn "%f" (getAnswer (Lib.readLines args[0]))

    0 // return an integer exit code
