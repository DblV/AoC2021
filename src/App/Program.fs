open Polymerisation

[<EntryPoint>]
let main args =
    printfn "%A" (getAnswer (Lib.readLines args[0]))

    0 // return an integer exit code
