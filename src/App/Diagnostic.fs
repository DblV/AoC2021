module Diagnostic

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

    getRate zeroes ones func

let convertBinaryToDec (bin:list<int>) =
    bin 
        |> List.rev
        |> List.mapi (fun i x -> if x = 0 then 0.0 else (2.0 ** (float i)))
        |> List.sum

let analyseDiagnosticReport (diagnosticReport:seq<string>) =
    let rptList = diagnosticReport |> List.ofSeq
    let passes = [0..(rptList.Head.Length-1)]

    let gammaRate = 
        passes 
        |> List.map (generateRateValues rptList (<))

    let epsilonRate =
        passes 
        |> List.map (generateRateValues rptList (>))

    (convertBinaryToDec gammaRate) * (convertBinaryToDec epsilonRate)

