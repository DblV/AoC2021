module ThermalImaging

let parseFold (s:string) =
    let bits = s.Split('=')
    let fold = bits.[0]
    fold.[(fold.Length-1)], (int bits.[1])

let parseCoords (s:string) =
    let coords = s.Split(',')
    (int coords.[0]), (int coords.[1])

let rec parseInput (input:list<string>) (coords:list<int*int>) (folds:list<char*int>) =
    match input with
    | h::t -> 
        match h.[0] with
        | 'f' -> parseInput t coords (folds@[(parseFold h)])
        | _ -> parseInput t (coords@[(parseCoords h)]) folds
    | _ -> coords, folds

let coordMatch (coords:list<int*int>) (x:int) (y:int) =
    (if (coords |> List.exists (fun c -> fst c = x && snd c = y)) then 1 else 0)

let rec populateGridFromCoords (coords:list<int*int>) (x:int) (y:int) (maxX:int) (maxY:int) (grid:list<list<int>>) (currentRow:list<int>) =
    match y with
    | a when a > maxY -> grid
    | _ -> 
        match x with
        | b when b > maxX ->
            populateGridFromCoords coords 0 (y+1) maxX maxY (grid@[currentRow]) List.empty<int>
        | _ -> 
            populateGridFromCoords coords (x+1) y maxX maxY grid (currentRow@[(coordMatch coords x y)])

let combineGrids (g1:list<list<int>>) (g2:list<list<int>>) =
    (g1,g2) ||> List.map2 (fun l1 l2 -> (l1,l2) ||> List.map2 (fun c1 c2 -> if c1 = 1 || c2 = 1 then 1 else 0))

let applyFold (grid:list<list<int>>) (fold:char*int) =
    match (fst fold) with
    | 'y' -> 
        let firstHalf = grid |> List.take (snd fold)
        let secondHalf = grid |> List.rev |> List.take (snd fold)
        combineGrids firstHalf secondHalf
    | _ -> 
        let firstHalf = grid |> List.map (fun l -> l |> List.take (snd fold))
        let secondHalf = grid |> List.map (fun l -> l |> List.rev |> List.take (snd fold))
        combineGrids firstHalf secondHalf

let rec applyFolds (grid:list<list<int>>) (folds:list<char*int>) =
    match folds with
    | h::t -> applyFolds (applyFold grid h) t
    | _ -> grid

let getAnswer (input:seq<string>) = 
    let coords, folds =
        parseInput (input |> List.ofSeq) List.empty<int*int> List.empty<char*int> 

    let maxX = 
        coords
        |> List.map (fun l -> fst l)
        |> List.max
    let maxY = 
        coords
        |> List.map (fun l -> snd l)
        |> List.max

    let grid = 
        populateGridFromCoords coords 0 0 maxX maxY [] List.empty<int>

    // let foldedGrid = applyFold grid folds.Head
    // foldedGrid |> List.map (fun l -> l |> List.sum) |> List.sum

    applyFolds grid folds