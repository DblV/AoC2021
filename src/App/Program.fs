open Bingo
open Depth
open Diagnostic
open Drive
open Lanternfish
open Vents

[<EntryPoint>]
let main args =
    // printfn "%i" (countIncreases (Lib.readLines args[0]))
    // printfn "%i" (count3rdIncreases (Lib.readLines args[0]))
    // printfn "%i" (followInstructions (Lib.readLines args[0]))
    // printfn "%f" (analyseDiagnosticReport' (Lib.readLines args[0]))
    // printfn "%i" (playBingoToLose (Lib.readLines args[0]))
    // printfn "%i" (findHotspots (Lib.readLines args[0]))
    printfn "%A" (howManyFish (Lib.readLines args[0]))
    
    0 // return an integer exit code
