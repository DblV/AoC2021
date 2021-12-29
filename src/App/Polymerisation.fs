module Polymerisation

let applyInstruction (instructions:list<char*char*char>) c1 c2 =
    let instruction = instructions |> List.filter (fun (i,j,_) -> i = c1 && j = c2)
    if instruction.Length > 0 then
        [c1; (instruction |> List.head |> (fun (_,_,k) -> k))]
    else
        [c1]

let rec polymerInsertion (instructions:list<char*char*char>) (newPolymer:list<char>) (workingPolymer:list<char>) =
    match workingPolymer with
    | f::s::t -> 
        polymerInsertion instructions (newPolymer@(applyInstruction instructions f s)) (s::t)
    | _ -> 
        newPolymer@workingPolymer

let rec polymerInsertions (roundsRemaining:int) (instructions:list<char*char*char>) (polymer:list<char>) = 
    match roundsRemaining with
    | 0 -> polymer
    | _ ->
        printfn "Polymer string is %A" polymer
        polymerInsertions (roundsRemaining-1) instructions (polymerInsertion instructions List.empty<char> polymer)
            
let rec getInstructions (instructions:list<char*char*char>) (input:list<string>) =
    match input with
    | h::t -> 
        getInstructions (instructions@[(h.[0],h.[1],h.[6])]) t
    | _ -> instructions

let getAnswer (input:seq<string>) =
    let polymer = input |> List.ofSeq |> List.head |> List.ofSeq

    let instructions = 
        input 
        |> List.ofSeq 
        |> List.tail 
        |> List.filter (fun s -> not (s = ""))
        |> getInstructions List.empty<char*char*char>

    // printfn "%A" polymer
    // printfn "%A" instructions

    let mutatedPolymer = polymerInsertions 10 instructions polymer

    mutatedPolymer
    |> List.countBy (fun l -> l)
    |> List.map (fun (_,i) -> i)
    |> (fun l -> (l |> List.max) - (l |> List.min))

    