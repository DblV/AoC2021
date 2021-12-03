let convertBinaryToDec (bin:list<int>) =
    printfn "converting %A" bin
    bin 
        |> List.rev
        |> List.mapi (fun i x -> if x = 0 then 0.0 else (2.0 ** (float i)))
        |> List.sum

let rec getGasRating (input:list<string>) (func:float->float->bool) index =
    let filterOutOnes = 
        input
        |> List.filter (fun y -> y.[index] = '0')
    
    let filterOutZeroes = 
        input
        |> List.filter (fun y -> y.[index] = '1')

    let numberOfZeroesAtIndex = 
        input
        |> List.map (fun x -> x.[index])
        |> List.filter (fun x -> x = '0')
        |> List.length

    printfn "list: %A, index: %i, numberofzeros: %i" input index numberOfZeroesAtIndex

    match input.Length with
    | 1 -> convertBinaryToDec (input.Head.ToCharArray() |> Array.map (System.Char.GetNumericValue >> int) |> List.ofArray)
    | 0 -> 0.0
    | _ -> 
        match numberOfZeroesAtIndex with
        | x when func (float x) (float (input.Length/2)) -> getGasRating filterOutOnes func (index+1)
        | _ -> getGasRating filterOutZeroes func (index+1)

let analyseDiagnosticReport' (diagnosticReport:seq<string>) =
    let rptList = diagnosticReport |> List.ofSeq

    let oxygenRating = getGasRating rptList (>) 0
    let co2Rating = getGasRating rptList (<=) 0

    printfn "oxy: %f, co2: %f" oxygenRating co2Rating

    oxygenRating * co2Rating

let inputs = [ "00001"; "10010"; "10100"; "01001"; "10000"; "11111" ]


analyseDiagnosticReport' inputs