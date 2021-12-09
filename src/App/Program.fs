open LavaTunnels

[<EntryPoint>]
let main args =
    printfn "%A" (findRiskScore (Lib.readLines args[0]))

    
    0 // return an integer exit code
