let rec multiplyFish rounds (fish:list<int>)  =
    match rounds with
    | x when x > 0 -> multiplyFish 
                        (rounds-1) 
                        (fish 
                        |> List.map (fun f -> f-1) 
                        |> List.append [ for i in 1 .. (fish |> List.filter (fun f -> f = 0) |> List.length) -> 8 ]
                        |> List.map (fun f -> if f = -1 then 6 else f))
    | _ -> fish

let input = "3,4,3,1,2"

input
|> (fun x -> x.Split(','))
|> List.ofArray
|> List.map int
|> multiplyFish 80
|> List.length