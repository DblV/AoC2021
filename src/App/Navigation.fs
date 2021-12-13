module Navigation

type Outcome = 
    | Complete
    | Incomplete
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
    | _ -> if not seekingChars.IsEmpty then Incomplete else Complete

let rec calculateScore score outcome =
    match outcome with
    | h::t ->
        match h with
        | Corrupt(')') -> calculateScore (score+3) t
        | Corrupt(']') -> calculateScore (score+57) t
        | Corrupt('}') -> calculateScore (score+1197) t
        | Corrupt('>') -> calculateScore (score+25137) t
        | _ -> calculateScore score t
    | _ -> score

let calculateSyntaxErrorScore (input:seq<string>) = 
    input
    |> List.ofSeq
    |> List.map (fun s -> checkNextChar List.empty<char> (s.ToCharArray() |> List.ofArray))
    |> (calculateScore 0)
