
type BingoCardNumber = {
    Value: int;
    Row: int;
    Col: int;
    Marked: bool
}

type BingoCard = {
    Numbers: list<BingoCardNumber>
}

let rec getBingoCardRow (input:list<int>) (currentRow:int) (currentCol:int) (output:list<BingoCardNumber>) =
    match input with
    | h::t -> getBingoCardRow t (currentRow+1) currentCol ({Value = h; Row = currentRow; Col = currentCol; Marked = false}::output)
    | _ -> output

let getIntListFromString (s:string) =
    s.Split(' ') |> Array.filter (fun x -> x <> "") |> Array.map int |> List.ofArray

let rec createBingoCards (input:list<string>) (currentBingoCard:list<BingoCardNumber>) (currentCol:int) (bingoCards:list<BingoCard>) =
    match input with 
    | h :: t -> 
        match h with 
        | "" -> createBingoCards t list<BingoCardNumber>.Empty 0 ({Numbers=currentBingoCard}::bingoCards)
        | _ -> createBingoCards t ((getBingoCardRow (getIntListFromString h) 0 currentCol (list<BingoCardNumber>.Empty) ) @ currentBingoCard) (currentCol+1) bingoCards
    | _ -> bingoCards


let isRowComplete (card:BingoCard) =
    let maxRowNumber = 
        card.Numbers
        |> List.maxBy (fun n -> n.Row)
        |> (fun n -> n.Row)

    let markedNumbers =
        card.Numbers
        |> List.filter (fun n -> n.Marked)
    
    printfn "marked numbers check: %A" markedNumbers

    let groupedCards =
        markedNumbers
        |> List.countBy (fun n -> n.Row)
        
    printfn "count of cards for row check %A" groupedCards

    let filteredCards =
        groupedCards
        |> List.filter (fun t -> snd(t) = maxRowNumber+1)
    
    filteredCards
    |> List.length > 0

let isColComplete (card:BingoCard) =
    let maxColNumber = 
        card.Numbers
        |> List.maxBy (fun n -> n.Col)
        |> (fun n -> n.Col)

    card.Numbers
    |> List.filter (fun n -> n.Marked)
    |> List.countBy (fun n -> n.Col)
    |> List.filter (fun t -> snd(t) = maxColNumber+1)
    |> List.length > 0

let markCard (num:int) (card:BingoCard) = 
    let markedNumberSet =
        card.Numbers
        |> List.map (fun n -> { Value = n.Value; Row = n.Row; Col = n.Col; Marked = n.Marked || (n.Value = num) })

    { Numbers = markedNumberSet }

let rec findWinningCard (numbers:list<int>) (cards:list<BingoCard>) =
    // Find each card that matches the number (filter)
    let nextNumber = numbers.Head

    printfn "number: %i" nextNumber

    let includesNumber, missingNumber = 
        cards
        |> List.partition (fun c -> (c.Numbers |> List.exists (fun n -> n.Value = nextNumber)))
    
    printfn "includesNumber: %i, missingNumber: %i" includesNumber.Length missingNumber.Length

    // Create a new card with that number Marked
    let markedCards = 
        includesNumber
        |> List.map (markCard nextNumber)

    printfn "markedCards %i" markedCards.Length
    
    let allCards = missingNumber@markedCards

    // Look for any cards with a completed row or column
    let completedCards =
        allCards 
        |> List.filter (fun c -> isRowComplete c || isColComplete c)
    
    printfn "completedCards: %A" completedCards

    // Return the first completed card and the last number called
    if completedCards.Length > 0 then
        completedCards.Head, nextNumber
    else
        findWinningCard numbers.Tail allCards

let calculateScore (card:BingoCard) (multiplier:int) =
    card.Numbers
    |> List.filter (fun n -> n.Marked = false)
    |> List.map (fun n -> n.Value)
    |> List.sum
    |> (fun x -> x * multiplier)

let inputList = [ 
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1";
    "";
    "22 13 17 11  0";
    " 8  2 23  4 24";
    "21  9 14 16  7";
    " 6 10  3 18  5";
    " 1 12 20 15 19";
    "";
    " 3 15  0  2 22";
    " 9 18 13 17  5";
    "19  8  7 25 23";
    "20 11 10 24  4";
    "14 21 16 12  6";
    "";
    "14 21 17 24  4";
    "10 16 15  9 19";
    "18  8 23 26 20";
    "22 11 13  6  5";
    " 2  0 12  3  7";
    "";]

let callerNumbers = inputList.Head.Split(',') |> List.ofSeq |> List.map int
let bingoCards = createBingoCards inputList.Tail.Tail list<BingoCardNumber>.Empty 0 list<BingoCard>.Empty

let winner, num = findWinningCard callerNumbers bingoCards
calculateScore winner num