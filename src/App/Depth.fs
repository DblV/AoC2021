module Depth

let compareInts t =
    match fst t, snd t with 
    | (i,j) when i < j -> 1
    | _ -> 0

let countIncreases (depths:seq<string>) =
    depths
    |> Seq.map int
    |> Seq.pairwise
    |> Seq.map compareInts
    |> Seq.sum
