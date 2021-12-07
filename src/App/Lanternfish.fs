module Lanternfish

let addFishToCount (fishState:int) (fishTotal:float) (counts:list<float>) =
    counts
    |> List.mapi (fun i c -> match (i = fishState) with | true -> c + fishTotal | _ -> c) 

let rec initialiseCounts (counts:list<float>) (fishStates:list<int>) =
    match fishStates with
    | h::t -> initialiseCounts (addFishToCount h 1 counts) t
    | _ -> counts
    
let rec makeTheFishDance (counts:list<float>) (days:int)=
    match days with
    | x when x > 0 -> 
        match counts with
        | h::t -> makeTheFishDance (t@[h] |> addFishToCount 6 ( float h)) (days - 1)
        | _ -> counts
    | _ -> counts

let howManyFish (input:seq<string>) =
    let fishStates = 
        input
        |> List.ofSeq
        |> (fun x -> x.Head.Split(','))
        |> List.ofArray
        |> List.map int
    
    let fishCounts = initialiseCounts [0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0] fishStates

    makeTheFishDance fishCounts 256
    |> List.sum
