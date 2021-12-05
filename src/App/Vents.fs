module Vents

let matchX (coords:((int*int)*(int*int))) =
    (coords |> fst |> fst) = (coords |> snd |> fst)

let matchY (coords:((int*int)*(int*int))) =
    (coords |> fst |> snd) = (coords |> snd |> snd)

let stringToArray (s:string) =
    s.Split("->")
    |> Array.map (fun s -> s.Split(','))
    |> Array.map (fun a -> a |> Array.map int)
    |> Array.map (fun a -> (a.[0],a[1]))
    |> (fun a -> (a.[0],a.[1]))

let min x y =
    if x < y then x 
    else y

let max x y = 
    if x > y then x
    else y

let getAllCoordsForNonDiagonalLines (points:((int*int)*(int*int))) =
    let startX, endX, startY, endY =
        (points |> fst |> fst),
        (points |> snd |> fst),
        (points |> fst |> snd),
        (points |> snd |> snd)

    match startX, endX, startY, endY with
    | a,b,c,d when a = b -> [(min c d)..(max c d)] |> List.map (fun l -> (a,l))
    | e,f,g,h when g = h -> [(min e f)..(max e f)] |> List.map (fun l -> (l,g))
    | _ -> list<int*int>.Empty

let findHotspots (data:seq<string>) = 
    data
    |> List.ofSeq
    |> List.map stringToArray
    |> List.map getAllCoordsForNonDiagonalLines
    |> List.reduce (@)
    |> List.countBy (fun l -> l)
    |> List.filter (fun c -> snd(c) > 1)
    |> List.length
