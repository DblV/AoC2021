
let getRate (zeroes:int) (ones:int) (func:int->int->bool) =
    match (zeroes, ones) with
    | x,y when func x y -> 1
    | _ -> 0

let generateRateValues (input:list<string>) (func:int->int->bool) (index:int) =
    let zeroes = 
        input
        |> List.map (fun x -> x.[index])
        |> List.filter (fun x -> x = '0')
        |> List.length

    let ones = 
        input
        |> List.map (fun x -> x.[index])
        |> List.filter (fun x -> x = '1')
        |> List.length

    printfn "zeroes: %i" zeroes
    printfn "ones: %i" ones

    getRate zeroes ones func

let convertBinaryToInt (bin:list<int>) =
    let inter = 
        bin 
        |> List.rev
    printfn "reversed: %A" inter

    let inter2 =
        inter
        |> List.mapi (fun i x -> if x = 0 then 0.0 else (2.0 ** (float i)))
    printfn "to binary: %A" inter2
    
    inter2 |> List.sum

let getPowerConsumption ints =
    0

let analyseDiagnosticReport (diagnosticReport:seq<string>) =
    let rptList = diagnosticReport |> List.ofSeq
    let passes = [0..(rptList.Head.Length)-1]
    
    printfn "Passes: %A" passes

    let gammaRate = 
        passes 
        |> List.map (generateRateValues rptList (<))
    printfn "gammaRate: %A" gammaRate

    let epsilonRate =
        passes 
        |> List.map (generateRateValues rptList (>))
    printfn "epsilonRate: %A" epsilonRate

    printfn "gamma: %f epsilon: %f" (convertBinaryToInt gammaRate) (convertBinaryToInt epsilonRate)
    (convertBinaryToInt gammaRate) * (convertBinaryToInt epsilonRate)

let inputs = [ "00001"; "10010"; "10100"; "01001"; "10000"; "11111" ]


analyseDiagnosticReport inputs