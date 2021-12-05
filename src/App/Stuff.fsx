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

let getAllCoordsForDiagonal (startX:int) (endX:int) (startY:int) (endY:int) =
    let xRange = 
        match startX, endX with
        | a,b when a < b -> [a..b]
        | _ -> [endX..startX] |> List.rev
    printfn "xrange %A" xRange
    let yRange = 
        match startY, endY with
        | a,b when a < b -> [a..b]
        | _ -> [endY..startY] |> List.rev
    printfn "yRange %A" yRange

    (xRange,yRange)
    ||> List.map2 (fun xs ys -> (xs,ys))

let getAllCoordsForLines (points:((int*int)*(int*int))) =
    let startX, endX, startY, endY =
        (points |> fst |> fst),
        (points |> snd |> fst),
        (points |> fst |> snd),
        (points |> snd |> snd)

    printfn "startX, endX, startY, endY : %i %i %i %i" startX endX startY endY

    match startX, endX, startY, endY with
    | a,b,c,d when a = b -> [(min c d)..(max c d)] |> List.map (fun l -> (a,l))
    | e,f,g,h when g = h -> [(min e f)..(max e f)] |> List.map (fun l -> (l,g))
    | _ -> getAllCoordsForDiagonal startX endX startY endY

let findHotspots (data:list<string>) = 
    data
    |> List.map stringToArray
    |> List.map getAllCoordsForLines
    |> List.reduce (@)
    |> List.countBy (fun l -> l)
    |> List.filter (fun c -> snd(c) > 1)
    |> List.length

let input = 
    ["0,9 -> 5,9";
    "8,0 -> 0,8";
    "9,4 -> 3,4";
    "2,2 -> 2,1";
    "7,0 -> 7,4";
    "6,4 -> 2,0";
    "0,9 -> 2,9";
    "3,4 -> 1,4";
    "0,0 -> 8,8";
    "5,5 -> 8,2";]

findHotspots input