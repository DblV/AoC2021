module Navigation

type Outcome = 
    | Complete
    | Incomplete of required: string
    | Corrupt of error: char

let rec checkNextChar (seekingChars:list<char>) (chars:list<char>) =
    match chars with
    | h::t ->
            if seekingChars.IsEmpty then
                // printfn "Seeking new opening symbol"
                match h with
                | '(' -> checkNextChar [')'] t
                | '[' -> checkNextChar [']'] t
                | '<' -> checkNextChar ['>'] t
                | '{' -> checkNextChar ['}'] t
                | _ -> Corrupt(h)
            else
                // printfn "Seeking closing symbol %A" seekingChars
                match h with
                | '(' -> checkNextChar (')'::seekingChars) t 
                | '[' -> checkNextChar (']'::seekingChars) t
                | '<' -> checkNextChar ('>'::seekingChars) t
                | '{' -> checkNextChar ('}'::seekingChars) t
                | x when h = seekingChars.Head -> checkNextChar seekingChars.Tail t
                | _ -> Corrupt(h)
    | _ -> if not seekingChars.IsEmpty then Incomplete(new System.String(seekingChars |> Array.ofList)) else Complete

let rec calculateCorruptScore score outcome =
    match outcome with
    | h::t ->
        match h with
        | Corrupt(')') -> calculateCorruptScore (score+3) t
        | Corrupt(']') -> calculateCorruptScore (score+57) t
        | Corrupt('}') -> calculateCorruptScore (score+1197) t
        | Corrupt('>') -> calculateCorruptScore (score+25137) t
        | _ -> calculateCorruptScore score t
    | _ -> score

let rec calculateIncompleteScore score outcome =
    match outcome with
    | h::t -> 
        match h with
        | ')' -> calculateIncompleteScore ((score*5.0)+1.0) t
        | ']' -> calculateIncompleteScore ((score*5.0)+2.0) t
        | '}' -> calculateIncompleteScore ((score*5.0)+3.0) t
        | '>' -> calculateIncompleteScore ((score*5.0)+4.0) t
        | _ -> calculateIncompleteScore score t
    | _ -> score

let rec getIncompleteScores (filtered:list<string>) outcomes =
    match outcomes with
    | h::t ->
        match h with
        | Incomplete(x) -> getIncompleteScores (x::filtered) t
        | _ -> getIncompleteScores filtered t
    | _ -> filtered

let calculateSyntaxErrorScore (input:seq<string>) = 
    input
    |> List.ofSeq
    |> List.map (fun s -> checkNextChar List.empty<char> (s.ToCharArray() |> List.ofArray))
    |> getIncompleteScores List.empty<string>
    |> List.map (fun l -> (calculateIncompleteScore 0.0 (l.ToCharArray() |> List.ofArray)))
    |> List.sort
    |> (fun l -> l.[(int (l.Length/2))])