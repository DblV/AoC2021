open SevenSegment

[<EntryPoint>]
let main args =
    printfn "%A" (untangleSevenSegments (Lib.readLines args[0]))

    
    0 // return an integer exit code
