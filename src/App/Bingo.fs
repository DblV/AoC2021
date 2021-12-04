module Bingo

type BingoCardNumber = {
    Value: int;
    Row: int;
    Col: int;
    Marked: bool
}

type BingoCard = {
    Numbers: list<BingoCardNumber>
}

let isRowComplete (card:BingoCard) =
    let maxRowNumber = 
        card.Numbers
        |> List.maxBy (fun n -> n.Row)
        |> (fun n -> n.Row)

    card.Numbers
    |> List.filter (fun n -> n.Marked)
    |> List.countBy (fun n -> n.Row)
    |> List.filter (fun t -> snd(t) = maxRowNumber+1)
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

let markCard (num:int) (card:BingoCard) = 
    let markedNumberSet =
        card.Numbers
        |> List.map (fun n -> { Value = n.Value; Row = n.Row; Col = n.Col; Marked = n.Marked || (n.Value = num) })

    { Numbers = markedNumberSet }

let rec findWinningCard (numbers:list<int>) (cards:list<BingoCard>) =
    // Find each card that matches the number (filter)
    let nextNumber = numbers.Head

    let includesNumber, missingNumber = 
        cards
        |> List.partition (fun c -> (c.Numbers |> List.exists (fun n -> n.Value = nextNumber)))

    // Create a new card with that number Marked
    let markedCards = 
        includesNumber
        |> List.map (markCard nextNumber)
    
    let allCards = missingNumber@markedCards

    // Look for any cards with a completed row or column
    let completedCards =
        allCards 
        |> List.filter (fun c -> isRowComplete c || isColComplete c)
    
    // Return the first completed card and the last number called
    if completedCards.Length > 0 then
        completedCards.Head, nextNumber
    else
        findWinningCard numbers.Tail allCards

let rec findLosingCard (numbers:list<int>) (cards:list<BingoCard>) =
    // Find each card that matches the number (filter)
    let nextNumber = numbers.Head

    let includesNumber, missingNumber = 
        cards
        |> List.partition (fun c -> (c.Numbers |> List.exists (fun n -> n.Value = nextNumber)))

    // Create a new card with that number Marked
    let markedCards = 
        includesNumber
        |> List.map (markCard nextNumber)
    
    //Rebuild the cards list including the newly marked numbers
    let allCards = missingNumber@markedCards

    if allCards.Length = 1 then
        // When we're down to one card remaining, if this is complete, we're done
        if allCards |> List.forall (fun c -> isRowComplete c || isColComplete c) then
            allCards.Head, nextNumber
        else
            // Keep going until the last card is complete
            findLosingCard numbers.Tail allCards
    else
        // Remove any completed cards
        let remainingCards =
            allCards 
            |> List.filter (fun c -> not (isRowComplete c) && not (isColComplete c))

        findLosingCard numbers.Tail remainingCards

let calculateScore (card:BingoCard) (multiplier:int) =
    card.Numbers
    |> List.filter (fun n -> n.Marked = false)
    |> List.map (fun n -> n.Value)
    |> List.sum
    |> (fun x -> x * multiplier)

let playBingo (input:seq<string>) =
    let inputList = input |> List.ofSeq

    let callerNumbers = inputList.Head.Split(',') |> List.ofSeq |> List.map int

    let bingoCards = createBingoCards inputList.Tail.Tail list<BingoCardNumber>.Empty 0 list<BingoCard>.Empty

    let winningCard, finalNumber = findWinningCard callerNumbers bingoCards

    calculateScore winningCard finalNumber

let playBingoToLose (input:seq<string>) =
    let inputList = input |> List.ofSeq

    let callerNumbers = inputList.Head.Split(',') |> List.ofSeq |> List.map int

    let bingoCards = createBingoCards inputList.Tail.Tail list<BingoCardNumber>.Empty 0 list<BingoCard>.Empty

    let losingCard, finalNumber = findLosingCard callerNumbers bingoCards

    calculateScore losingCard finalNumber

    