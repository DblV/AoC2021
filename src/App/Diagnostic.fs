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

let numberOfZeroesAtIndex (input:list<string>) index = 
    input
    |> List.map (fun x -> x.[index])
    |> List.filter (fun x -> x = '0')
    |> List.length

let rec getGasRating (input:list<string>) (func:float->float->bool) index =
    match input.Length with
    | 1 -> convertBinaryToDec (input.Head.ToCharArray() |> Array.map (System.Char.GetNumericValue >> int) |> List.ofArray)
    | 0 -> 0.0
    | _ -> 
        match numberOfZeroesAtIndex input index with
        | x when func (float x) (float (input.Length/2)) -> getGasRating (input |> List.filter (fun y -> y.[index] = '0')) func (index+1)
        | _ -> getGasRating (input |> List.filter (fun y -> y.[index] = '1')) func (index+1)

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

let analyseDiagnosticReport' (diagnosticReport:seq<string>) =
    let rptList = diagnosticReport |> List.ofSeq

    let oxygenRating = getGasRating rptList (>) 0
    let co2Rating = getGasRating rptList (<=) 0

    oxygenRating * co2Rating