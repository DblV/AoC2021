module Polymerisation

let rec updatePolymerPairCount (polymerPairCountUpdate:list<string*float>) (polymerPairCount:string*float) =
    match polymerPairCountUpdate with
    | h::t -> 
        if (fst polymerPairCount) = (fst h) then
            updatePolymerPairCount t (fst polymerPairCount, (snd polymerPairCount + snd h))
        else
            updatePolymerPairCount t polymerPairCount
    | _ -> polymerPairCount

let findAndApplyInstruction (instruction:string*string*string) (oldPolymer:list<string*float>) (newPolymer:list<string*float>) =
    let i,r1,r2 = instruction

    let matchingPolymerPairCount = 
        oldPolymer 
        |> List.filter (fun (p,_) -> p = i)
        |> List.head
        |> (fun (_,c) -> c)
    
    let polymerPairCountUpdate = 
        [i, ((float 0)-matchingPolymerPairCount); r1, matchingPolymerPairCount; r2, matchingPolymerPairCount]

    newPolymer |> List.map (updatePolymerPairCount polymerPairCountUpdate)

let rec polymerInsertion (instructions:list<string*string*string>) (oldPolymer:list<string*float>) (newPolymer:list<string*float>) =
    match instructions with
    | h::t -> 
        polymerInsertion t oldPolymer (findAndApplyInstruction h oldPolymer newPolymer)
    | _ -> 
        newPolymer

let rec polymerInsertions (roundsRemaining:int) (instructions:list<string*string*string>) (polymer:list<string*float>) = 
    match roundsRemaining with
    | 0 -> polymer
    | _ ->
        polymerInsertions (roundsRemaining-1) instructions (polymerInsertion instructions polymer polymer)
            
let rec getInstructions (instructions:list<string*string*string>) (allChars:list<char>) (input:list<string>) =
    match input with
    | h::t -> 
        getInstructions (instructions@[(new System.String([|h.[0];h.[1]|])),(new System.String([|h.[0];h.[6]|])),(new System.String([|h.[6];h.[1]|]))]) (allChars@[h.[0];h.[1];h.[6]]) t
    | _ -> instructions, allChars

let getAnswer (input:seq<string>) =
    let polymerChars = 
        input 
        |> List.ofSeq 
        |> List.head 
        |> List.ofSeq 
    let finalChar = polymerChars |> List.rev |> List.head

    let polymerInit =
        polymerChars
        |> List.pairwise 
        |> List.map (fun (a,b) -> new System.String([|a;b|]))
        |> List.countBy (fun l -> l)
        |> List.map (fun (s,i) -> (s,(float i)))

    let (instructions, allChars) = 
        input 
        |> List.ofSeq 
        |> List.tail 
        |> List.filter (fun s -> not (s = ""))
        |> getInstructions List.empty<string*string*string> List.empty<char>

    let allUniqueChars =
        allChars
        |> List.groupBy (fun c -> c)
        |> List.map (fun (c,_) -> new System.String([|c|]))
        
    let emptyPolymer = 
        allUniqueChars
        |> List.map (fun l1 -> allUniqueChars |> List.map (fun l2 -> l1+l2))
        |> List.reduce (List.append)
        |> List.countBy (fun l -> l)
        |> List.map (fun (p,_) -> p, (float 0))

    let polymer =
        emptyPolymer
        |> List.map (updatePolymerPairCount polymerInit)

    let mutatedPolymer = polymerInsertions 40 instructions polymer

    let finalCounts = 
        mutatedPolymer
        |> List.filter (fun (_,c) -> c > (float 0))
        |> List.map (fun (p,c) -> p.[0], c)
        |> List.groupBy (fun (p,_) -> p)
        |> List.map (fun (c,l) -> c, (l |> List.sumBy snd))
        |> List.map (fun (ch,co) -> if ch = finalChar then (ch,co+(float 1)) else (ch,co))

    printfn "%A" polymerChars
    printfn "%A" instructions
    printfn "%A" mutatedPolymer
    printfn "%A" finalCounts

    finalCounts
    |> List.map (fun (_,i) -> i)
    |> (fun l -> (l |> List.max) - (l |> List.min))    