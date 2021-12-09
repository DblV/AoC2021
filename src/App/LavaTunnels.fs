module LavaTunnels

let testLowSpot (floor:int[][]) (x:int) (y:int) =
    let valueAtSpot = floor.[y].[x]
    let valueToLeft = if (x-1) < 0 then 9 else floor.[y].[x-1]
    let valueToRight = if (x+1) >= floor.[y].Length then 9 else floor.[y].[x+1]
    let valueAbove = if (y-1) < 0 then 9 else floor[y-1].[x]
    let valueBelow = if (y+1) >= floor.Length then 9 else floor.[y+1].[x]

    if (valueAtSpot < valueAbove) && (valueAtSpot < valueBelow) && (valueAtSpot < valueToLeft) && (valueAtSpot < valueToRight) then
        [valueAtSpot]
    else
        List.empty<int>

let rec findLowSpots (x:int) (y:int) (lowSpots:list<int>) (floor:int[][]) =
    match y with
    | a when a >= floor.Length -> lowSpots
    | _ -> 
        match x with
        | b when b >= floor.[y].Length -> findLowSpots 0 (y+1) lowSpots floor
        | _ -> findLowSpots (x+1) y (lowSpots@(testLowSpot floor x y)) floor

let findRiskScore (input:seq<string>) = 
    input
    |> Seq.map (fun s -> s.ToCharArray() |> Array.map (System.Char.GetNumericValue >> int))
    |> Array.ofSeq 
    |> findLowSpots 0 0 List.empty<int>
    |> List.map (fun i -> i+1)
    |> List.sum

