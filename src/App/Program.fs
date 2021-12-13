open Navigation

[<EntryPoint>]
let main args =
    printfn "%A" (calculateSyntaxErrorScore (Lib.readLines args[0]))

    0 // return an integer exit code
